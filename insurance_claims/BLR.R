
#############################################################
#############################################################
##############BINARY LOGISTIC REGRESSION#####################
#############################################################
#############################################################

#--------------------------------
#R-CODING FOR TRAIN DATA
#--------------------------------
library(readr)
library(dplyr)

data=data.frame(read.csv("insurance_claims (1).csv"))
head(data)
colnames(data)
str(data)
dim(data)
lapply(data[sapply(data,is.factor)], levels)
set.seed(55)



train_data=read.csv("F:\\suneel\\suneel\\insurance claim\\insurance_claims\\train_data.csv")
test_data=read.csv("F:\\suneel\\suneel\\insurance claim\\insurance_claims\\test_data.csv")
head(train_data)
names(train_data)
str(train_data)
cols=c("policy_state","policy_csl ","insured_sex ","insured_education_level ","insured_occupation ","insured_hobbies ",
       "insured_relationship","incident_type","collision_type ","incident_severity ","authorities_contacted",
       "incident_state ","incident_city","property_damage ","police_report_available ","auto_make ","auto_model ")
for (i in cols) {
  train_data[,i]=as.factor(train_data[,i])
}

#train_data=data.frame(fread("C:\\Users\\System2\\Desktop\\suneel\\project1\\citibank\\development.csv"))

#test_data=data.frame(fread("C:\\Users\\System2\\Desktop\\suneel\\project1\\citibank\\validation.csv"))

#train_data=train_data[,-c(1:2)]
#test_data=test_data[,-c(1:2)]
#head(train_data)
#head(test_data)



#Converting dependent variable name to "y" in both train and test data
#-----------------------------------------------------------------------
colnames(train_data)[dim(train_data)[2]]=colnames(test_data)[dim(test_data)[2]]="y"
test_data$y=ifelse(test_data$y=="Y",1,0)
train_data$y=ifelse(train_data$y=="Y",1,0)
str(train_data)
str(test_data)

#Converting dependent variable to numeric
#-------------------------------------------
#train_data$y=ifelse(train_data$y=="Y",1,0)
#test_data$y=ifelse(test_data$y=="Y",1,0)

#Fiting the model and geting result
#--------------------------------------
fit = step(glm(y~.,family = binomial(),data=train_data),direction = c("both"),trace = 0)
save(fit,file = "modelblr.rda")
summary(fit)

f=function(myfit)
{
  #------------------------------
  #Finding probabilities
  #------------------------------
  
  prob=predict(fit,type="response")
  
  #--------------------------------------------------------
  #Creating space for sensititvity and specificity
  #--------------------------------------------------------
  
  observed=train_data$y
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
  
  testprob=predict(fit,newdata=test_data,type="response")
  test_predicted=ifelse(testprob<optimum_cut,0,1)
  
  #Classification Result for test data
  #---------------------------------------
  test_sensitivity=table(test_data$y,test_predicted)[2,2]/rowSums(table(test_data$y,test_predicted))[2]
  test_specificity=table(test_data$y,test_predicted)[1,1]/rowSums(table(test_data$y,test_predicted))[1]
  test_misclass_error=mean(test_data$y!=test_predicted)
  
  #---------------------------------------------
  #ROC curve & AUC value for test data
  #---------------------------------------------
  
  pr2 =prediction(testprob, test_data$y)
  roc2=performance(pr2, measure = "tpr", x.measure = "fpr")
  plot(roc2,col="red",xlab="1-Specificity",ylab="Sensitivity",main="ROC of test data")
  abline(a=0,b=1,lwd=1,lty=4,col="black")
  
  auc2 = performance(pr2, measure = "auc")
  test_auc = auc2@y.values[[1]]
  
  g=data.frame(train=c(train_auc,train_misclass_error,train_sensitivity,train_specificity),
               test=c(test_auc,test_misclass_error,test_sensitivity,test_specificity))
  g=round(g,4)
  row.names(g)=c("AUC","Class.Error","Sensitivity","specificty")
  h=list(optimum_cutvalue=optimum_cut,model_result=g)
  print(h)
}

result=f(fit)


exp( 1.011e-07)
exp(-1.705e-01)
exp(1.228e+00)
exp( -1.133e-01)
exp(-1.307e+00)
exp(3.713e+00)
exp( 3.583e+0)
exp(-9.368e-01 )
exp(-7.842e-02 )
exp( 2.266e-01)
exp(6.176e-01)
exp(-2.224e-01)
exp( -8.169e-01)
exp(-1.145e-01 )
exp(7.507e-01)
exp( 1.653e-01)
exp( 8.719e-01)
exp(-6.048e-01 )
exp( 1.002e+00 )
exp( 1.718e+00 )
exp(-3.467e+00 )
exp(-3.137e+00 )
exp(-4.406e+00)
exp(-3.419e-02 )
exp(-5.187e-05)
