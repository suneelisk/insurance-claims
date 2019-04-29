library(data.table)
library(MASS)
library(missForest)
library(ROCR)
library(rpart)
library(caTools)

data=data.frame(read.csv("insurance_claims (1).csv"))
head(data)
colnames(data)
dim(data)
lapply(data[sapply(data,is.factor)], levels)
data=data[,-c(4,5,11,19,26)]
str(data)
set.seed(55)
data=as.data.frame(sapply(data,sub,pattern='\\?',replacement=NA))
sum(is.na(data))
data$cust_id=as.numeric(data$cust_id)
data$months_as_customer=as.integer(data$months_as_customer)
data$age=as.integer(data$age)
data$policy_deductable=as.integer(data$policy_deductable)
data$policy_annual_premium=as.numeric(data$policy_annual_premium)
data$umbrella_limit=as.integer(data$umbrella_limit)
data$capital.gains=as.integer(data$capital.gains)
data$capital.loss=as.integer(data$capital.loss)
data$incident_hour_of_the_day=as.integer(data$incident_hour_of_the_day)
data$number_of_vehicles_involved=as.integer(data$number_of_vehicles_involved)
data$bodily_injuries=as.integer(data$bodily_injuries)
data$total_claim_amount=as.integer(data$total_claim_amount)
data$injury_claim=as.integer(data$witnesses)
data$property_claim=as.integer(data$property_claim)
data$vehicle_claim=as.integer(data$vehicle_claim)
data$auto_year=as.integer(data$auto_year)
head(data)

set.seed(1)
impr_data= missForest(data,mtry=3,ntree=5)
full_data=impr_data$ximp
names(full_data)
sum(is.na(full_data))


#================================
#Splitting data to Train and Test
set.seed(33)
insurance=sample.split(full_data,SplitRatio = .7)
train_data=subset(full_data,insurance==TRUE)
test_data=subset(full_data,insurance==FALSE)
train_data$cust_id=(train_data$cust_id)+10000
test_data$cust_id=(test_data$cust_id)+10000
write.csv(train_data,file="train_data.csv")
write.csv(test_data, file="test_data.csv")

              

