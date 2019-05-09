
set.seed(666)
x1 <- runif(500,min=0,max=1)
x2 <- rep(NA, 500)
odd <- seq(1,500,2)
x2[odd] <- 1
x2[-odd] <-0

X <- cbind(1, x1, x2)
Beta <- c(-1.1, 5, -0.4)

z <- X %*% Beta

p <- 1/(1+exp(-z))
y <- rbinom(500,1,p)

data <- data.frame(y, x1, x2)
hist(y)

## develop probit regression model##
prob <- glm(y~., family = binomial(link="probit"), data = data)
summary(prob)
hist(predict(prob,type="response"), main = "histogram of predicted response 
     y = -0.50 + 2.74*x1 -0.32*x2")
prob_y <- predict(prob,type="response")

## sensitivity VS specificity for probit regression##
library(ROCR)
pred1 <- prediction(prob_y,y)
perf1 <- performance(pred1, "tpr", "fpr")
plot1 <- plot(perf1, colorize = TRUE, main = "ROC for the probit regression")
prob_acu <- round(as.numeric(performance(pred1,"auc")@y.values),2)

## develop logistic regression model##
logit <- glm(y~., family = binomial(link = "logit"), data = data)
summary(logit)
hist(predict(logit, type = "response"), main = "histogram of predicted response
     y = -0.8841 +4.7537*x1 - 0.5098*x2")
logit_y <- predict(logit, type = "response")

##sensitivity VS specificity for logistic regression##
pred2 <- prediction (logit_y, y)
perf2 <- performance (pred2, "tpr", "fpr")
plot2 <- plot(perf2, colorize = TRUE, main = "ROC for the logistic regression")
logit_acu <- round(as.numeric(performance(pred2, "auc")@y.values), 2)



##problem 1.b
#simulation

set.seed(666)
x1 <- runif(500, 0, 1)
x2 <- rep(NA, 500)
odd <- seq(1, 500, 2)
x2[odd] <- 1
x2[-odd] <- 0

X <- cbind(1, x1, x2)


beta <- c(-1.1, 5, -0.4)

z <- X %*% beta
p<-pnorm(z)
y <- rbinom(500, 1, p)

data <- data.frame(y = y, x1 = x1, x2 = x2)

hist(y)
#logstic
glm.log <- glm(y ~ ., family = binomial(link = "logit"), data = data )
summary(glm.log)

hist(predict(glm.log, type = "response"))
prob.log <- predict(glm.log, type = "response")

library(ROCR)
pred <- prediction(prob.log, y)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC for logistic link")
as.numeric(performance(pred,"auc")@y.values) 

#probit
glm.prob <- glm(y ~., family = binomial(link = "probit"), data = data)
summary(glm.prob)

hist(predict(glm.prob, type = "response"))
prob.prob2 <- predict(glm.prob, type = "response")
library(ROCR)
pred2 <- prediction(prob.prob2, y)
perf2 <- performance(pred, "tpr", "fpr")

plot(perf2, colorize = TRUE, main = "ROC for probit link")
as.numeric(performance(pred2,"auc")@y.values)




##HW3 problem 2

##German Credit dataset
#import data
getwd()
setwd("E:/BANA Spring course/Data Mining I/Homework/HW3/HW3_3")
GermanCredit <- read.table("German_data.txt",header = F)
str(GermanCredit)
summary(GermanCredit)

##sampling 70% of data for training, the othe 30% for testing
set.seed(9)
sample <- sample(nrow(GermanCredit), nrow(GermanCredit)*0.7)
GermanTrain <- GermanCredit[sample,]
GermanTest <- GermanCredit[-sample,]

#EDA 
summary(GermanTrain)

# histogram of numeric varialbes 
par(mfrow = c(2,4))
for (i in c(2,5,8,11,13,16,18,21)) {
  n <- names(GermanTrain)
  hist(GermanTrain[,i], prob = TRUE, xlab = paste("The range of", n[i]), main = paste("Histogram of", n[i])) 
  lines(density(GermanTrain[,i], na.rm = T), col="blue") 
}

#boxplot 
par(mfrow = c(2,4))
for (i in c(2,5,8,11,13,16,18,21)) {
  n <- names(GermanTrain)
  boxplot(GermanTrain[,i], prob = TRUE, xlab = paste("The range of", n[i]), main = paste("Boxplot of", n[i])) 
}

# barchart of categorical variables
par()
par(mfrow=c(3,5))
for (i in c(1,2,4,6,7,9,10,12,14,15,17,19,20)){
  n <- names(GermanTrain)
  counts <- table(GermanTrain[,i])
  barplot(counts,  
          xlab=paste("Number of", n[i]))
}
#scatterplot matrix
par(mfrow = c(1,1))
# panel.smooth function is built in.
# panel.cor puts correlation in upper panels, size proportional to correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

col_vec=ifelse(GermanTrain$V21==1,"blue","red")
pairs(GermanTrain[,1:10], cex = 0.2, lwd = 0.2, col = col_vec,upper.panel=panel.cor, 
      pch=20, main="Scatterplot Matrix")

col_vec=ifelse(GermanTrain$V21==1,"blue","red")
pairs(GermanTrain[,11:21], cex = 0.2, lwd = 0.2, col = col_vec, 
      upper.panel=panel.cor, 
      pch=20, main="Scatterplot Matrix")


##logistic regression ###
#add a new variable V22 to present good or bad load base 
#on V21 value if V21== 1, V22==0 and V21 ==2 then V21==1 
library(tidyverse)
GermanTrain.1 <- GermanTrain %>% mutate(V22 = ifelse(V21==1,0,1))
GermanTest.1 <- GermanTest %>% mutate(V22 = ifelse(V21==1,0,1))
#full model in sample
German.glm <- glm(V22~.-V21,family = binomial, data= GermanTrain.1)
German.summary <- summary(German.glm)
German.summary   
BIC(German.glm)
MRD.glm <- German.glm$deviance/German.glm$df.residual
PseuRsq.glm <- (German.glm$null.deviance-German.glm$deviance)/German.glm$null.deviance
#0.2765

#full model out sample
German.glm.test <- glm(V22~.-V21,family = binomial, data= GermanTest.1)
German.summary <- summary(German.glm.test)
German.summary   
BIC(German.glm.test)
#mean residual deviance
MRD.glm.test <- German.glm.test$deviance/German.glm.test$df.residual
#Pseudo R-squre
PseuRsq.glm.test <- (German.glm.test$null.deviance-German.glm.test$deviance)/German.glm.test$null.deviance
#0.4410


#stepwise selection
#using full model, stepwise selection
German.glm.step <- step(German.glm) #stepwise selection "direction defalut = both"
#suing BIC for selection
German.glm1.step <-  step(German.glm, k=log(nrow(GermanTrain))) 


#model comparison
#full model, AIC model vs BIC model
#AIC model in sample
German.glmAIC <- glm(V22~V14+V15+V9+V2+V13+V5+V20+V8+V10+V6+V4+V3+V1, family = binomial, data=GermanTrain.1)
summary(German.glmAIC)
BIC(German.glmAIC) 
MRD.glmAIC <- German.glmAIC$deviance/German.glmAIC$df.residual
#MRD =0.9584
PseuRsq.glmAIC <- (German.glmAIC$null.deviance-German.glmAIC$deviance)/German.glmAIC$null.deviance
#0.2591

##AIC out of sample
German.glmAIC.test <- glm(V22~V14+V15+V9+V2+V13+V5+V20+V8+V10+V6+V4+V3+V1, family = binomial, data=GermanTest.1)
summary(German.glmAIC.test)
BIC(German.glmAIC.test) 
MRD.glmAIC.test <- German.glmAIC.test$deviance/German.glmAIC.test$df.residual
#MRD 0.8898
PseuRsq.glmAIC.test <- (German.glmAIC.test$null.deviance-German.glmAIC.test$deviance)/German.glmAIC.test$null.deviance
#BIC model in sample
German.glmBIC <- glm(V22~V3+V2+V1, family = binomial, data = GermanTrain.1)
summary(German.glmBIC)
BIC(German.glmBIC) 
MRD.glmBIC <- German.glmBIC$deviance/German.glmBIC$df.residual
#MRD =1.0389
PseuRsq.glmBIC <- (German.glmBIC$null.deviance-German.glmBIC$deviance)/German.glmBIC$null.deviance
#0.1655

##BIC out sample
German.glmBIC.test <- glm(V22~V3+V2+V1, family = binomial, data = GermanTest.1)
summary(German.glmBIC.test)
BIC(German.glmBIC.test) 
MRD.glmBIC.test <- German.glmBIC.test$deviance/German.glmBIC.test$df.residual
#0.9841
PseuRsq.glmBIC.test <- (German.glmBIC.test$null.deviance-German.glmBIC.test$deviance)/German.glmBIC.test$null.deviance
#0.2074

##draw ROC curve for three models and calculat AUC
## in sample  ***
#full model
prob.glm.insample <- predict(German.glm, type = "response") #gives prob of 1 for training data
#3 ROC curve evaluation method for logistic regression ###
#install.packages("verification")
library(verification)
par(mfrow=c(1,1))
roc.plot(GermanTrain.1$V22 == "1", prob.glm.insample) #put in probability in Y
#To get the area under the ROC curve:
roc.plot(GermanTrain.1$V22 == "1", prob.glm.insample)$roc.vol
roc.plot(x=GermanTrain.1$V22 == "1", pred = prob.glm.insample)$roc.vol$Area
# out sample
prob.glm.outsample <- predict(German.glm, newdata = GermanTest.1,type = "response")
roc.plot(GermanTest.1$V22 == "1", prob.glm.outsample) #put in probability in Y
#To get the area under the ROC curve:
roc.plot(GermanTest.1$V22 == "1", prob.glm.outsample)$roc.vol
roc.plot(x=GermanTest.1$V22 == "1", pred = prob.glm.outsample)$roc.vol$Area 



##AIC model
prob.glmA.insample <- predict(German.glmAIC, type = "response") #gives prob of 1 for training data
#3 ROC curve evaluation method for logistic regression ###
#install.packages("verification")
library(verification)
par(mfrow=c(1,1))
roc.plot(GermanTrain.1$V22 == "1", prob.glmA.insample) #put in probability in Y
#To get the area under the ROC curve:
roc.plot(GermanTrain.1$V22 == "1", prob.glmA.insample)$roc.vol
roc.plot(x=GermanTrain.1$V22 == "1", pred = prob.glmA.insample)$roc.vol$Area
# out sample
prob.glmA.outsample <- predict(German.glmAIC, newdata = GermanTest.1,type = "response")
roc.plot(GermanTest.1$V22 == "1", prob.glmA.outsample) #put in probability in Y
#To get the area under the ROC curve:
roc.plot(GermanTest.1$V22 == "1", prob.glmA.outsample)$roc.vol
roc.plot(x=GermanTest.1$V22 == "1", pred = prob.glmA.outsample)$roc.vol$Area 

##BIC model
prob.glmB.insample <- predict(German.glmBIC, type = "response") #gives prob of 1 for training data
#3 ROC curve evaluation method for logistic regression ###
#install.packages("verification")
library(verification)
par(mfrow=c(1,1))
roc.plot(GermanTrain.1$V22 == "1", prob.glmB.insample) #put in probability in Y
#To get the area under the ROC curve:
roc.plot(GermanTrain.1$V22 == "1", prob.glmB.insample)$roc.vol
roc.plot(x=GermanTrain.1$V22 == "1", pred = prob.glmB.insample)$roc.vol$Area
# out sample
prob.glmB.outsample <- predict(German.glmBIC, newdata = GermanTest.1,type = "response")
roc.plot(GermanTest.1$V22 == "1", prob.glmB.outsample) #put in probability in Y
#To get the area under the ROC curve:
roc.plot(GermanTest.1$V22 == "1", prob.glmB.outsample)$roc.vol
roc.plot(x=GermanTest.1$V22 == "1", pred = prob.glmB.outsample)$roc.vol$Area 

##plot ROC curve for three model
#in sample
roc.plot(x = GermanTrain.1$V22 == "1", pred = cbind(prob.glm.insample, prob.glmA.insample,prob.glmB.insample), 
         legend = TRUE, leg.text = c("Full Model", "AIC model", "BIC model"), main="ROC Curve of In Sample")$roc.vol

#out sample
roc.plot(x = GermanTest.1$V22 == "1", pred = cbind(prob.glm.outsample, prob.glmA.outsample,prob.glmB.outsample), 
         legend = TRUE, leg.text = c("Full Model", "AIC model", "BIC model"), main="ROC Curve of Out Sample")$roc.vol


#symmetric misclassification
# Symmetric cost
cost1 <- function(r, pi) {
  mean(((r == 0) & (pi > pcut)) | ((r == 1) & (pi < pcut)))
}

#insample
#confusion matrix
predicted.glmA.insample <- prob.glmA.insample>1/6
predicted.glmA.insample <- as.numeric(predicted.glmA.insample)
table(GermanTrain.1$V22, predicted.glmA.insample, dnn = c("Truth", "Predicted"))

#misclassification rate
mean(ifelse(GermanTrain.1$V22 != predicted.glmA.insample, 1, 0))

#outsample
#confusion matrix
predicted.glmA.outsample <- prob.glmA.outsample>1/6
predicted.glmA.outsample <- as.numeric(predicted.glmA.outsample)
table(GermanTest.1$V22, predicted.glmA.outsample, dnn = c("Truth", "Predicted"))

#misclassification rate
mean(ifelse(GermanTest.1$V22 != predicted.glmA.outsample, 1, 0))

pcut=1/6
# Asymmetric cost
cost2 <- function(r, pi) {
  weight1 = 5
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

#assymetric missclassification rate
#insample 
insample_ams <-cost2(GermanTrain.1$V22, predicted.glmA.insample) 
insample_ams
#outsample
outsample_ams <-cost2(GermanTest.1$V22, predicted.glmA.outsample) 
outsample_ams


#search for an optimal cut-off p-value

# define the searc grid from 0.01 to 0.99
searchgrid = seq(0.01, 0.99, 0.01)
# result is a 99x2 matrix, the 1st col stores the cut-off p, the 2nd column
# stores the cost
result = cbind(searchgrid, NA)
# in the cost function, both r and pi are vectors, r=truth, pi=predicted
# probability
cost1 <- function(r, pi) {
  weight1 = 5
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
German.glmAIC <- glm(V22~V14+V15+V9+V2+V13+V5+V20+V8+V10+V6+V4+V3+V1, family = binomial, data=GermanTrain.1)
for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  # assign the cost to the 2nd col
  result[i, 2] <- cost1(GermanTrain.1$V22, predict(German.glmAIC, type = "response"))
}
plot(result, ylab = "Cost in Training Set", main = "Search for the optimal cut-off P-value")

result[which.min(result[,2]),1] ##gives the optimal cut-off value 0.24
####misclassification rate


#compare assymetric misclasification value for pcut = 0.24
pcut=0.24
# Asymmetric cost
cost2 <- function(r, pi) {
  weight1 = 5
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

#assymetric missclassification rate
#insample 
insample_ams1 <-cost2(GermanTrain.1$V22, predicted.glmA.insample) 
insample_ams
#outsample
outsample_ams1 <-cost2(GermanTest.1$V22, predicted.glmA.outsample) 
outsample_ams


##HW3 problem3

#load the data
bankruptcy.data <- read.csv("bankruptcy.csv", header = T)
#View(bankruptcy.data)
head(bankruptcy.data)
dim(bankruptcy.data)
bankruptcy.data=unique(bankruptcy.data)
bankruptcy.data=bankruptcy.data[complete.cases(bankruptcy.data),]
dim(bankruptcy.data)

#create training and testing datasets
set.seed(2017)
bankruptcy.data= bankruptcy.data[-3]
subset <- sample(nrow(bankruptcy.data), nrow(bankruptcy.data) * 0.7)
bankruptcy.train = bankruptcy.data[subset, ]
bankruptcy.test = bankruptcy.data[-subset, ]
dim(bankruptcy.train)
dim(bankruptcy.test)
summary(bankruptcy.train)
#View(bankruptcy.train)

#data exploration of training dataset
# histogram of numeric varialbes 
par()
par(mfrow = c(3,4))
for (i in 1:12) {
  n <- names(bankruptcy.train)
  hist(bankruptcy.train[,i], prob = TRUE, xlab = paste("The range of", n[i]), main = paste("Histogram of", n[i])) 
  lines(density(bankruptcy.train[,i], na.rm = T), col="blue") 
}

#boxplot 
par(mfrow = c(3,4))
for (i in 1:12) {
  n <- names(bankruptcy.train)
  boxplot(bankruptcy.train[,i], prob = TRUE, xlab = paste("The range of", n[i]), main = paste("Boxplot of", n[i])) 
}

#scatterplot matrix
col_vec=ifelse(bankruptcy.train$DLRSN==1,"blue","red")
pairs(~.,data=bankruptcy.train)

#logistic regression
bankruptcy.glm=glm(DLRSN~.-FYEAR,family=binomial,data=bankruptcy.train)
bankruptcy.summary=summary(bankruptcy.glm)
bankruptcy.glm.step1 <- step(bankruptcy.glm, direction="backward")
bankruptcy.glm.step2 <- step(bankruptcy.glm, direction="backward",k=log(nrow(bankruptcy.train))) #use BIC
bankruptcy.glm1=glm(DLRSN~.-FYEAR-R5,family=binomial,data=bankruptcy.train)
bankruptcy.glm2=glm(DLRSN~.-FYEAR-R4-R5-R1,family=binomial,data=bankruptcy.train)
bankruptcy.summary1=summary(bankruptcy.glm1)
bankruptcy.summary2=summary(bankruptcy.glm2)
AIC(bankruptcy.glm)
AIC(bankruptcy.glm1)
AIC(bankruptcy.glm2)
BIC(bankruptcy.glm)
BIC(bankruptcy.glm1)
BIC(bankruptcy.glm2)

#in sample prediction
#full model
prob.glm.insample=predict(bankruptcy.glm,type="response")  #give prob of 1 for training data
predicted.glm.insample=prob.glm.insample> 1/16
predicted.glm.insample=as.numeric(predicted.glm.insample)  #give 0s and 1s
#model1
prob.glm1.insample=predict(bankruptcy.glm1,type="response")  #give prob of 1 for training data
predicted.glm1.insample=prob.glm1.insample> 1/16
predicted.glm1.insample=as.numeric(predicted.glm1.insample)  #give 0s and 1s
#confusion matrix
mytable1=table(bankruptcy.train$DLRSN,predicted.glm1.insample,dnn=c("True", "Predicted") )
mytable1
#evaluation of models: logical comparison
missclassifylist1=ifelse(bankruptcy.train$DLRSN != predicted.glm1.insample, 1,0)
#misclassification rate
mis_rate1=mean(missclassifylist1)
mis_rate1
#model2
prob.glm2.insample=predict(bankruptcy.glm2,type="response")  #give prob of 1 for training data
predicted.glm2.insample=prob.glm2.insample> 1/16
predicted.glm2.insample=as.numeric(predicted.glm2.insample)  #give 0s and 1s
#confusion matrix
mytable2=table(bankruptcy.train$DLRSN,predicted.glm2.insample,dnn=c("True", "Predicted") )
mytable2
#evaluation of models: logical comparison
missclassifylist2=ifelse(bankruptcy.train$DLRSN != predicted.glm2.insample, 1,0)
#misclassification rate
mis_rate2=mean(missclassifylist2)
mis_rate2

#out of sample prediction
#model1
prob.glm1.outsample <- predict(bankruptcy.glm1, bankruptcy.test, type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > 1/16
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
mytable_1=table(bankruptcy.test$DLRSN, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))
mytable_1
missclassifylist_1=ifelse(bankruptcy.test$DLRSN != predicted.glm1.outsample, 1,0)
#misclassification rate fro test data
mis_rate_1=mean(missclassifylist_1)
mis_rate_1
#model2
prob.glm2.outsample <- predict(bankruptcy.glm2, bankruptcy.test, type = "response")
predicted.glm2.outsample <- prob.glm2.outsample > 1/16
predicted.glm2.outsample <- as.numeric(predicted.glm2.outsample)
mytable_2=table(bankruptcy.test$DLRSN, predicted.glm2.outsample, dnn = c("Truth", "Predicted"))
mytable_2
missclassifylist_2=ifelse(bankruptcy.test$DLRSN != predicted.glm2.outsample, 1,0)
#misclassification rate fro test data
mis_rate_2=mean(missclassifylist_2)
mis_rate_2

#ROC curve evaluation method for logistic regression
#install.packages("verification")
library(verification)
#in sample
roc.plot(bankruptcy.train$DLRSN == "1", prob.glm.insample)
roc.plot(bankruptcy.train$DLRSN == "1", prob.glm.insample)$roc.vol$Area #--$Area
roc.plot(bankruptcy.train$DLRSN == "1", prob.glm1.insample)
roc.plot(bankruptcy.train$DLRSN == "1", prob.glm1.insample)$roc.vol$Area #--$Area
roc.plot(bankruptcy.train$DLRSN == "1", prob.glm2.insample)
roc.plot(bankruptcy.train$DLRSN == "1", prob.glm2.insample)$roc.vol$Area 
roc.plot(x = bankruptcy.train$DLRSN == "1", pred = cbind(prob.glm.insample, prob.glm1.insample, prob.glm2.insample), 
         legend = TRUE, leg.text = c("Full Model", "Model1", "Model2"))$roc.vol
#out of sample
roc.plot(bankruptcy.test$DLRSN == "1", prob.glm1.outsample)
roc.plot(bankruptcy.test$DLRSN == "1", prob.glm1.outsample)$roc.vol$Area  #--$Area
roc.plot(bankruptcy.test$DLRSN == "1", prob.glm2.outsample)
roc.plot(bankruptcy.test$DLRSN == "1", prob.glm2.outsample)$roc.vol$Area  #--$Area
roc.plot(x = bankruptcy.test$DLRSN == "1", pred = prob.glm1.outsample, 
         legend = TRUE, leg.text = "Model1")$roc.vol


cost <- function(r, pi) {
  weight1 = 15
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
pcut=1/16
weighted_cost_insample1=cost(bankruptcy.train$DLRSN, predicted.glm1.insample)
weighted_cost_insample1
weighted_cost_insample2=cost(bankruptcy.train$DLRSN, predicted.glm2.insample)
weighted_cost_insample2
weighted_cost_outsample1=cost(bankruptcy.test$DLRSN, predicted.glm1.outsample)
weighted_cost_outsample1
weighted_cost_outsample2=cost(bankruptcy.test$DLRSN, predicted.glm2.outsample)
weighted_cost_outsample2
