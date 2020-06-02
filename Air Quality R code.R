#1- getting the data
data = read.csv("/Users/vijitchekkala/Desktop/AIR1.csv")
head(data)
tail(data)
#2- describing the attributes
str(data)
summary(data)
#3- checing for null values
any(is.na(data))
#the output is false because the value -200 is considered to be the null value
#replacing -200 with null values
data [data=="-200"] <- NA
#checking for null values
any(is.na(data))

#checking the number of null values present in the dataset
summary(is.na(data))
summary(data)

#here NMHC.GT. has 90% of missing values , so we drop the column
data <- data[,-5] #5 is the number of the colummn
summary(is.na(data))

#getting hour data from the time column
data$Time <- as.POSIXct(data$Time, format= "%H:%M:%S")
data$hour <- sapply(data$Time,function(x){format(x,"%H")})
head(data$hour)
summary(data)

#libraries to use for missing data
library(mice)
library(VIM)
#checking with missing values percentage
miss <- function(x) {sum(is.na(x)/length(x)*100)}
apply(data,2,miss)
#pattern of the missing data
library(dplyr)
library(ggplot2)
library(tidyr)
missing.values <- data %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels <-(missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('grey', 'skyblue'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Missing values", x =
         'Variable', y = " Percentage of missing values")

percentage.plot

#md.pairs(data)
#marginplot(data[,c('AH','RH')])
#marginplot(data[,c('T','RH')])
#converting hour from character to numeric
data$hour <- as.numeric(as.character(data$hour))
summary(data$hour)
#marginplot(data[,c('hour','RH')])
#imputing values
impute <- mice(data[,3:15],m=3)
print(impute)  #pmm= predictive mean matching is done here for filling the null values
head(impute$imp$T) #checking Temperature values (comparing mean with the imputed values)
data[702,]#checking the 702nd row for comaparing imputed values with the mean
data[525,]
data[527,]
data[704,]#comparing all these immputed values with the mean 

summary(data$T)

#complete with imputing values
data <- mice::complete(impute,2)
#data <- complete(impute,2)  #filling all the null values with the imputed values
any(is.na(data))
#here the values is imputed in the original dataset and first two columns are removed
tail(data$T)

#pattern of the data after umputing the values
#md.pattern(data)
#md.pairs(data)
#marginplot(data[,c('AH','RH')])
#marginplot(data[,c('T','RH')])
#marginplot(data[,c('T','AH')])
#marginplot(data[,c('hour','RH')])

#using packages for correlation
library(ggplot2)
library(dplyr)

#plotting AH based on all other input values
pl1 <- ggplot(filter(data),aes(T,RH))
pl1= pl1+geom_point(colour='skyblue')
print(pl1)


pl2 <- ggplot(filter(data),aes(hour,RH))
pl2= pl2+geom_point(colour='skyblue')
print(pl2)

pl3 <- ggplot(filter(data),aes(AH,RH))
pl3= pl3+geom_point(colour='skyblue')
print(pl3)
#define correlation of AH and RH
#feature scaling
colnames(data)
head(data)
data[,1:10]=scale(data[,1:10])
head(data)
#splitting data into training(70%) and testing(30%) 
library(caTools)

sample <- sample.split(data, SplitRatio = 0.70) #storing the train and test data in sample variable
traindata <- subset(data, sample==TRUE) #training data
testdata <-  subset(data,sample==FALSE) #testing data
head(traindata)
#finding correaltion 
model1 <- lm(RH ~.- hour,data=traindata)
model1
summary(model1)
#plotting cont
par(mfrow=c(2,2))
plot(model1)
#cont
correlation <- data[,1:11]
#install.packages(('GGally'))
library(GGally)
ggpairs(correlation)
#cont
#install.packages('corpcor')
library(corpcor)
cor2pcor(cov(correlation))
#cont
#install.packages('mctest')
library(mctest)
omcdiag(correlation,data$RH) # shows perfect fit
#cont
imcdiag(correlation,data$RH) #shows perfect fit

#cont
#install.packages('ppcor')
library(ppcor)
pcor(correlation, method = "pearson")

colnames(testdata)

library(car)
vif(model1)
#removing correlation using stepwise , ridge, lasso and elastic net

#backward regression (model 3)
step(lm(RH ~.- hour,data=traindata),direction = "backward")
modelbackward <- lm(formula = RH ~ CO.GT. + PT08.S1.CO. + C6H6.GT. + NOx.GT. + 
                      PT08.S3.NOx. + NO2.GT. + PT08.S4.NO2. + PT08.S5.O3. + T + 
                      AH, data = traindata)
modelbackward
summary(modelbackward)
vif(modelbackward)
#forard elimination
step(lm(RH ~.- hour,data=traindata),direction = "forward",scope =~CO.GT. + PT08.S1.CO. + C6H6.GT. + PT08.S2.NMHC. + NOx.GT. + 
       PT08.S3.NOx. + NO2.GT. + PT08.S4.NO2. + PT08.S5.O3. + T + 
       AH)

#starting with new form
library(ggplot2)
library(caret)
#install.packages("glmnet")
library(glmnet)
#install.packages('mlbench')
library(mlbench)
#install.packages('psych')
library(psych)
#custom control parameters
custom <- trainControl(method='repeatedcv',
                       number=10,
                       repeats=5,
                       verboseIter = T)
#linear model
lm <- train(RH~.,data=traindata,method='lm',trControl=custom)
#results
lm$results
lm
summary(lm)
plot(lm$finalModel)
#ridge regression
modelridge <- train(RH~. -hour,data=traindata,method='glmnet',
                    tuneGrid=expand.grid(alpha=0,
                                         lambda=seq(0.0001,1,length=5)),
                    trControl=custom)
#plot ridge
plot(modelridge)
modelridge
plot(modelridge$finalModel,xvar="lambda",label=T)
plot(modelridge$finalModel,xvar='dev',label=T)
plot(varImp(modelridge,scale=T))

#lasso
custom <- trainControl(method='repeatedcv',
                       number=10,
                       repeats=5,
                       verboseIter = T)
lasso <- train(RH~.  -hour,,data=traindata,method='glmnet',
               tuneGrid=expand.grid(alpha=1,
                                    lambda=seq(0.0001,1,length=5)),
               trControl=custom)
#plot lasso
plot(lasso)
plot(varImp(lasso,scale=T))

#removing vif more than 10 
model98 <- lm(formula = RH ~ CO.GT. + PT08.S1.CO. + NOx.GT. + PT08.S4.NO2.+
                PT08.S3.NOx. + NO2.GT. + PT08.S5.O3. + T + 
                AH, data = traindata)

model98
summary(model98)
vif(model98)

res <- residuals(model98) #getting the residuals
str(res)
summary(res)
res <- as.data.frame(res) #converting res to dataframe for ggplot
head(res)

#histogram of the residuals
# Histogram of residuals
ggplot(res,aes(res)) +  geom_histogram(fill='green',alpha=1,binwidth = 1.5) #it is normally distributed

#predicting the values

#results <- predict(results,data.frame(RH=0.50),interval='confidence')  #not working

#model 1
RH.predict<- predict(model98,testdata)

results <- cbind(RH.predict,testdata$RH) ## proposal discussion #predicting and actual values comparison

head(results)
colnames(results) <- c('predicted','actual')
str(results)
summary(results)
results <- as.data.frame(results)
tail(results)


#predicted values less than 0 should be 0
zero <- function(a){
  if  (a < 0){
    return(0)
  }else{
    return(a)
  }
}

results$predicted <- sapply(results$predicted,zero)
summary(results)
#Multiple linear regression
#mean square method
MSE <- mean((results$actual-results$predicted)^2)
print(MSE)
#root mean square method
RMSE<-MSE^0.5
RMSE
#root squared value

SSE = sum((results$predicted - results$actual)^2)
SST = sum( (mean(data$RH) - results$actual)^2)

R2 = 1 - SSE/SST
R2


#support vector regression

library(ggplot2)
library(e1071)

help(svm)


modelsvr1 <- svm(RH~.,data = traindata,kernel='linear')
modelsvr2 <- svm(RH~.,data = traindata,kernel='polynomial')
modelsvr3 <- svm(RH~.,data = traindata,kernel='radial')
modelsvr4 <- svm(RH~.,data = traindata,kernel='sigmoid')
summary(modelsvr1)
summary(modelsvr2)
summary(modelsvr3)
summary(modelsvr4)
pred.values1 <- predict(modelsvr1,testdata)
pred.values2 <- predict(modelsvr2,testdata)
pred.values3 <- predict(modelsvr3,testdata)#
pred.values4 <- predict(modelsvr4,testdata)


table(pred.values1,testdata[,12])
table(pred.values2,testdata[,12])
table(pred.values3,testdata[,12])#
table(pred.values4,testdata[,12])
#table(pred.values,traindata)
tail(pred.values1)
tail(pred.values2)
tail(pred.values3)#
tail(pred.values4)

tail(testdata$RH)
#rmse of svr

error1 <- testdata$RH - pred.values1
error2 <- testdata$RH - pred.values2
error3 <- testdata$RH - pred.values3#
error4 <- testdata$RH - pred.values4

#Support Vecotr Regression
#mean square method
MSE3 <- mean((testdata$RH-pred.values3)^2)#
print(MSE3)
MSE1 <- mean((testdata$RH-pred.values1)^2)
print(MSE1)
#
MSE2 <- mean((testdata$RH-pred.values2)^2)
print(MSE2)

#
MSE4 <- mean((testdata$RH-pred.values4)^2)
print(MSE4)

#root mean square method
MSE1^0.5
MSE2^0.5
RMSE3 <- MSE3^0.5
RMSE3
MSE4^0.5

#radial kernel is the best here

SSE = sum((pred.values3 - testdata$RH)^2)
SST = sum( (mean(data$RH) - testdata$RH)^2)

R21 = 1 - SSE/SST
R21

#compring R square values
R2


plotsvr <- points(testdata$RH, pred.values3, col = "red", pch=16)
plotsvr
plot(testdata$RH, pred.values3,col = "skyblue", pch=2)












