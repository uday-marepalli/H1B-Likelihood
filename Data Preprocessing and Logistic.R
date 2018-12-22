install.packages("dplyr")
library(dplyr)
install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)

data <- read.csv('FinalData.csv')
data <- data[,-1]

lca_certified <- data[(data$CASE_STATUS=="CERTIFIED"),]
lca_denied <- data[(data$CASE_STATUS=="DENIED"),]

certified_sample <- sample(1:nrow(lca_certified),2*nrow(lca_denied))
lca_certified_sample <- lca_certified[certified_sample,]

lca_combined <- rbind(lca_certified_sample,lca_denied)

#rate.x = acceptance per employer
#rate.y = acceptance per SOC

names(lca_combined) <- tolower(names(lca_combined))
lca_combined$factor_cs <-as.factor(ifelse(lca_combined$case_status %in% c("CERTIFIED"),1,0))
lca_combined$factor_ftp <-as.factor(ifelse(lca_combined$full_time_position == 'Y',1,0))
lca_combined$h1b_dependent <-as.factor(ifelse(lca_combined$h1b_dependent == 'Y',1,0))
lca_combined$willful_violator <- as.factor(ifelse(lca_combined$willful_violator == 'N',1,0))
lca_combined$worksite_state = as.factor(lca_combined$worksite_state)

lca_combined$prevailing_wage <- ifelse(lca_combined$pw_unit_of_pay== 'Hour', lca_combined$prevailing_wage*(1920), lca_combined$prevailing_wage)

lca_model = lca_combined[-c(1:7,9,13)]

install.packages('glmnet')
install.packages('ISLR')

library (glmnet)
library (ISLR)

train <- sample(1:nrow(lca_combined),0.75*nrow(lca_combined))
test<- -train

train_data <- lca_combined[train,]
test_data <- lca_combined[test,]

log_reg_train = glm( factor_cs ~ prevailing_wage + h1b_dependent + willful_violator +  rate.x + application_number + rate.y + application_number_soc + factor_cs + factor_ftp, data = train_data, family = binomial)
summary (log_reg_train)

y_predict = predict (log_reg_train, newdata = test_data, type = "response")

p_values <- seq (0,1,by =0.05)
vec <- vector("numeric", length(p_values))

install.packages("data.table")
library(data.table)

test <- p_values

for (i in 1:21)
{
  y_predict_ROC <- y_predict
  y_predict_ROC[y_predict_ROC < p_values[i]] <- 0
  y_predict_ROC[y_predict_ROC >= p_values[i]] <- 1
  
  classficationTable1 = table(truth= test_data$factor_cs, predict= y_predict_ROC)
  classficationTable1
  
  
  ####ROC Curve
  pred <- prediction(y_predict_ROC, test_data$factor_cs)
  perf <- performance(pred,"tpr","fpr")
  plot(perf)
  
  ###Area Under the Curve
  auc.tmp <- performance(pred,"auc");
  auc <- as.numeric(auc.tmp@y.values)
  vec[i] <- auc
}

#Printing classification table for best value of AUC

y_predict_ROC <- y_predict
y_predict_ROC[y_predict_ROC < 0.7] <- 0
y_predict_ROC[y_predict_ROC >= 0.7] <- 1

classficationTable1 = table(truth= test_data$factor_cs, predict= y_predict_ROC)
classficationTable1


####ROC Curve
pred <- prediction(y_predict_ROC, test_data$factor_cs)
perf <- performance(pred,"tpr","fpr")
plot(perf)















