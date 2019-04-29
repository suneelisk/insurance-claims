
#############################################################
#############################################################
##############RANDOM FOREST##################################
#############################################################
#############################################################

library(readr)
library(dplyr)
library(randomForest)
library(data.table)
#Importing train and test data
#-------------------------------

traindata=train_data
testdata=test_data

head(traindata)
head(testdata)



#Changine dependent variable name to "y"
#------------------------------------------
names(traindata)[ncol(traindata)]=names(testdata)[ncol(testdata)]="y"


library(randomForest)



importance((fit))
varImpPlot(fit)


result=function(traindata,testdata,m,n)
{
  
  #Converting all character variables to factor
  #-----------------------------------------------
  #traindata=traindata %>%mutate_if(is.character, factor)
  #testdata=testdata %>%mutate_if(is.character, factor)
  traindata$y=as.factor(traindata$y)
  testdata$y=as.factor(testdata$y)
  observed=traindata$y
  
  #Fiting random forest
  #---------------------
  fit=randomForest(y~.,traindata,mtry=m,ntry=n)
  
  #------------------------------
  #Fiting by votes
  #------------------------------
  train_vote_predict=predict(fit)
  test_vote_predict=predict(fit,newdata = testdata)
  
  train_vote_sensitivity=table(observed,train_vote_predict)[2,2]/rowSums(table(observed,train_vote_predict))[2]
  train_vote_specificity=table(observed,train_vote_predict)[1,1]/rowSums(table(observed,train_vote_predict))[1]
  train_vote_misclass_error=mean(observed!=train_vote_predict)
  
  test_vote_sensitivity=table(testdata$y,test_vote_predict)[2,2]/rowSums(table(testdata$y,test_vote_predict))[2]
  test_vote_specificity=table(testdata$y,test_vote_predict)[1,1]/rowSums(table(testdata$y,test_vote_predict))[1]
  test_vote_misclass_error=mean(testdata$y!=test_vote_predict)
  
  vote_model=rbind("---",round(data.frame(train_vote=c(train_vote_misclass_error,train_vote_sensitivity,train_vote_specificity),
                                       test_vote=c(test_vote_misclass_error,test_vote_sensitivity,test_vote_specificity)),4))
  row.names(vote_model)=c("AUC","Class.Error","Sensitivity","specificty")
  
  #------------------------------
  #Fiting by probabilities
  #------------------------------
  prob=predict(fit,type="prob")[,2]
  
  #--------------------------------------------------------
  #Creating space for sensititvity and specificity
  #--------------------------------------------------------
  traindata$y=ifelse(as.numeric(traindata$y)==2,1,0)
  testdata$y=ifelse(as.numeric(testdata$y)==2,1,0)
  observed=traindata$y
  
  cutpoints = seq(0,1,0.01)
  sensitivity = seq(1,101,1)
  specificity = seq(1,101,1)
  cutpoint_perf = cbind(cutpoints,sensitivity,specificity)
  x=data.frame(table(observed))
  n1=x[2,2]
  n2=x[1,2]
  
  for (i in 1:101)
  {
    pred = ifelse(prob < cutpoint_perf[i,1],0,1)
    sumed = pred + observed
    pred1_1 = ifelse(sumed==2,1,0)
    correct1_1 = sum(pred1_1)
    pred0_0 = ifelse(sumed==0,1,0)
    correct0_0 = sum(pred0_0)
    cutpoint_perf[i,2] = correct1_1/n1
    cutpoint_perf[i,3] = correct0_0/n2
  }
  
  cutvalue_table = data.frame(cutpoint_perf)
  cutvalue_table$diff = abs(cutvalue_table$sensitivity-cutvalue_table$specificity)
  cut_point=subset(cutvalue_table,cutvalue_table$diff==min(cutvalue_table$diff))
  optimum_cut=mean((cut_point$cutpoints))
  
  #Prediction for train data
  #---------------------------------
  train_predicted=ifelse(prob<optimum_cut,0,1)
  
  #Classification Result for train data
  #---------------------------------------
  train_sensitivity=table(observed,train_predicted)[2,2]/rowSums(table(observed,train_predicted))[2]
  train_specificity=table(observed,train_predicted)[1,1]/rowSums(table(observed,train_predicted))[1]
  train_misclass_error=mean(observed!=train_predicted)
  
  #---------------------------------------------
  #ROC curve & AUC value for train data
  #---------------------------------------------
  library(ROCR)
  par(mfrow=c(1,2))
  pr1 =prediction(prob, observed)
  roc1=performance(pr1, measure = "tpr", x.measure = "fpr")
  plot(roc1,col="red",xlab="1-Specificity",ylab="Sensitivity",main="ROC of train data")
  abline(a=0,b=1,lwd=1,lty=4,col="black")
  
  auc1 = performance(pr1, measure = "auc")
  train_auc = auc1@y.values[[1]]
  
  #############################################################
  
  #predicting prob & response for test data
  #--------------------------------------------
  
  testprob=predict(fit,newdata=testdata,type="prob")[,2]
  test_predicted=ifelse(testprob<optimum_cut,0,1)
  
  #Classification Result for test data
  #---------------------------------------
  test_sensitivity=table(testdata$y,test_predicted)[2,2]/rowSums(table(testdata$y,test_predicted))[2]
  test_specificity=table(testdata$y,test_predicted)[1,1]/rowSums(table(testdata$y,test_predicted))[1]
  test_misclass_error=mean(testdata$y!=test_predicted)
  
  #---------------------------------------------
  #ROC curve & AUC value for test data
  #---------------------------------------------
  
  pr2 =prediction(testprob, testdata$y)
  roc2=performance(pr2, measure = "tpr", x.measure = "fpr")
  plot(roc2,col="red",xlab="1-Specificity",ylab="Sensitivity",main="ROC of test data")
  abline(a=0,b=1,lwd=1,lty=4,col="black")
  
  auc2 = performance(pr2, measure = "auc")
  test_auc = auc2@y.values[[1]]
  
  
  prob_model=round(data.frame(train_prob=c(train_auc,train_misclass_error,train_sensitivity,train_specificity),
                              test_prob=c(test_auc,test_misclass_error,test_sensitivity,test_specificity)),4)
  
  h=list(optimum_cutvalue=optimum_cut,data.frame(prob_model,vote_model))
  print(h)
}

result(traindata=traindata,testdata=testdata,2,200)







