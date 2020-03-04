claimants <- read.csv(file.choose()) # Choose the claimants Data set

#### To identify Missing values 
sum(is.na(claimants$CLMINSUR))
summary(claimants)
str(claimants)
claimants$ATTORNEY =  as.factor(claimants$ATTORNEY)

claimants <- na.omit(claimants)
model <- glm(ATTORNEY~.,data=claimants[,-1],family = "binomial")
?glm
summary(model)
# Odds Ratio
#exp(coef(model))

# Confusion matrix table 
prob <- predict(model,type=c("response"),claimants)
prob

confusion<-table(prob>0.6,claimants$ATTORNEY)
confusion
table(prob>0.6)
table(claimants$ATTORNEY)
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 70.62


##
pred_values <- NULL
yes_no <- NULL
for (i in 1:1096){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}


claimants[,"prob"] <- prob
claimants[,"pred_values"] <- pred_values
claimants[,"yes_no"] <- yes_no

View(claimants[,c(2,8,9,10)])

# Accuracy 
acc <- table(claimants$ATTORNEY,pred_values)
Accuracy<-sum(diag(acc)/sum(acc))
Accuracy # 70.62



# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-2,2.5))
?plot
# More area under the ROC Curve better is the logistic regression model obtained
str(rocrperf)
