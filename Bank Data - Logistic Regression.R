#Load necessary libraries
library(ggplot2)
library(ROCR)
library(caret)
library(dummies)
library(reshape2)
library(unbalanced)

#Reading the file. Sep = ; was used as with , all variables were coming under a single column
bank_data <- read.csv("bank-additional-full.csv")
View(bank_data)

#finding missing values
sapply(bank_data, function(x) sum(is.na(x)))

#Binning Age numeric variable into age_binned which is a factor variable with 6 levels
bank_data$age_binned <- cut(bank_data$age, c(0,20,35,50,65,100))

#Checking relation between education and success
#Not much to differentiate between the groups in terms of success rate
spineplot(bank_data$education, bank_data$y)
basic <- c("basic.4y","basic.6y","basic.9y")
bank_data$education <- as.character(bank_data$education)
bank_data$education[bank_data$education %in% basic]<- 'basic'
bank_data$education<-as.factor(bank_data$education)

#Converting 999 in pdays to 50
bank_data$pdays[bank_data$pdays==999]<-50

#Creating the (log) transformed campaign variable

bank_data$log_campaign <- log(bank_data$campaign)
bank_data$log_pdays <- log(1+bank_data$pdays)

#binning month variable to quarters (financial)
#Q1 <- c("apr","may","jun")
#Q2 <- c("jul","aug","sep")
#Q3 <- c("oct","nov","dec")
#Q4 <- c("mar")
#bank_data$quarters <- ifelse(bank_data$month %in% Q1, 'Q1', ifelse(bank_data$month %in% Q2, 'Q2', ifelse(bank_data$month %in% Q3, 'Q3', 'Q4')))

#Converting yes to 1 and no to 0 in target variable y
bank_data$y <- ifelse(bank_data$y == "yes",1,0)


data_long = melt(bank_data[, sapply(bank_data, is.numeric)], id='y')
#head(data_long)
ggplot(data_long, aes(x = value, group=y, color=factor(y)) )+ 
  geom_density()+ facet_wrap(~variable, scales="free")


#summary(bank_data$pdays[bank_data$pdays!=999])
#bank_data$pdays_cat <- ifelse(bank_data$pdays<=3,'1',ifelse(bank_data$pdays<=6,'2',ifelse(bank_data$pdays<=7,'3',ifelse(bank_data$pdays<=27,'4',"NC"))))
#dropping columns which are not required
columns_to_be_dropped <- c("age","duration","campaign","pdays","default","housing","loan")
bank_data_processed <- bank_data[,!names(bank_data) %in% columns_to_be_dropped]

#bank_data_processed <- dummy.data.frame(bank_data_processed,names = c("job","education","marital","default","housing","loan","contact","age_binned","quarters"))

#Rows_with_1 <- bank_data_processed[bank_data_processed$y==1,]
#Rows_with_0 <- bank_data_processed[bank_data_processed$y==0,]

#undersample_0<-sample.int(nrow(Rows_with_0),size = floor(0.7*nrow(Rows_with_0)),replace = FALSE)
balanced <- ubSMOTE(bank_data_processed[,-which(names(bank_data_processed)=='y')],as.factor(bank_data_processed$y),perc.over = 200,k=5,perc.under = 200)
balanceddata<- cbind(balanced$X,Class=balanced$Y)


#Creating training and test sets
set.seed(123456)
sample_list <- sample.int(nrow(balanceddata), size = floor(0.7*nrow(balanceddata)), replace = FALSE)
train_data <- balanceddata[sample_list,]
test_data <- balanceddata[-sample_list,]

#implementing the model
bank_model <- glm(Class~., data = train_data, family = binomial("logit"))
summary(bank_model)


bank_model_1 <- glm(Class~job+marital+education+contact+month+day_of_week+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed+log_campaign+age_binned+log_pdays,data=train_data,family=binomial)
#bank_model_1 <- glm(y~job+contact+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+age_binned+log_campaign+quarters+pdays_cat,data=train_data_1,family=binomial)
summary(bank_model_1)


#Predicting on the training set and finding accuracy using confusion matrix
pred_train <- predict(bank_model, train_data, type = "response")
pred_train[pred_train<=0.5]=0
pred_train[pred_train>0.5]=1
confusionMatrix(pred_train,train_data$Class,positive = '1')

pred_train_1 <- predict(bank_model_1, train_data, type = "response")
pred_train_1[pred_train_1<=0.5]=0
pred_train_1[pred_train_1>0.5]=1
confusionMatrix(pred_train_1,train_data$Class,positive = '1')

#Predicting on the test set and finding accuracy using confusion matrix
pred_test <- predict(bank_model, test_data, type = "response")
pred_test[pred_test<=0.4]=0
pred_test[pred_test>0.4]=1
confusionMatrix(pred_test,test_data$Class,positive = '1')

pred_test_1 <- predict(bank_model_1, test_data, type = "response")
pred_test_1[pred_test_1<=0.4]=0
pred_test_1[pred_test_1>0.4]=1
confusionMatrix(pred_test_1,test_data$Class,positive = '1')

#Finding area under curve for test set prediction and plotting the curve too
pred_auc <- prediction(pred_test_1,test_data$Class)
perf <- performance(pred_auc, measure = "tpr", x.measure = "fpr")
plot(perf)

auc <- performance(pred_auc, measure = "auc")
auc_value <- auc@y.values
round(as.numeric(auc_value), digits = 4)




