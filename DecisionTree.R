library ("glmnet")
library ("ROCR")
install.packages("car")
library(car)
install.packages("partykit")
library(partykit) 
install.packages("party")
library(party)
library(caret)
install.packages("tree")
library(tree)
install.packages("caret",dependencies = TRUE)
install.packages('e1071', dependencies=TRUE)

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

lca_model = lca_combined[-c(1:7,9, 12, 13)]

set.seed(32388)

train <- sample(1:nrow(lca_model),0.75*nrow(lca_model))
test<- -train


#inTrain11 <- createDataPartition (y=lca_model$factor_cs, p=0.75, list = FALSE)
training1 <- lca_model[train,]
testing1 <- lca_model[test,]
ctree1 <- ctree(factor_cs ~. , data = training1)

prediction1 <- predict(ctree1, newdata = testing1)
confusionMatrix(prediction1,testing1$factor_cs)

plot(ctree1)

probabilities <- 1 - unlist(treeresponse(ctree1,newdata = testing1), use.names = F)[seq(1,nrow(testing1)*2,2)]

pred1 <- prediction(probabilities,testing1$factor_cs)
perf1 <- performance(pred1,"tpr","fpr")
plot(perf1)

auc.tmp1 <- performance(pred1,"auc")
auc1 <- as.numeric(auc.tmp1@y.values)
auc1

####DECISION TREE######
tree1 <- tree(factor_cs ~. , data = training1)
plot(tree1)
text(tree1,pretty=0)
summary(tree1)

prediction2 <- predict(tree1, newdata = testing1)
confusionMatrix(prediction1,testing1$factor_cs)


####RANDOM FOREST####
install.packages('randomForest')
library(randomForest)
fit <- randomForest(factor_cs ~ .,   data=lca_model, classwt = c(0.20, 0.80))
print(fit)
importance(fit)
plot(fit)
