#______________________________________________________________________
#TESTS FOR STARTING
#______________________________________________________________________

library(readxl)

setwd("/Users/MDo/Desktop/AA_Project/svm")
#default of credit card clients
#df = read.table("default of credit card clients.xls", header = TRUE)
data <- read_excel("default_small.xls", skip=1)
df <- read_excel("default_small.xls", skip=1)

apply(data, 2, function (x) sum(is.na(x))) # no NA -> good
# ID is useless
names <- names(data)[!names(data) %in% c("ID")]
cleared_data <- data[names]y

# normalize input with min-max
maxs <- apply(cleared_data, 2, max) 
mins <- apply(cleared_data, 2, min)
normalized_data <- as.data.frame(scale(cleared_data, center = mins, scale = maxs - mins))



round0 <- function(val) {
  return(round(val, digits=0))
}

cnterr <- function(V1, V2) {
  cnt <- 0
  for(i in 1:length(V1)) {
    if ( (V1[i] == "0" && V2[i] == "2") ||
         (V1[i] == "1" && V2[i] == "1") ) {
      cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
      cnt <- cnt+1; }
  }
  return(cnt)
}



df$Y <- factor(df$Y)
index <- 1:nrow(df)

testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(df[testindex,])
traindata <- na.omit(df[-testindex,])

library("e1071")

## Train SVM with e1071:
#deal with spaces in column names?  -> (name = "Age group")
x <- subset(traindata, select = -Y)#list all except
y <- traindata$Y
model <- svm(x, y, type="C-classification", kernel="radial", cost=1000)
summary(model)
pred <- predict(model,x)

table(pred,y)


svm_model1 <- svm(x,y)
summary(svm_model1)
pred <- predict(svm_model1,x)
system.time(pred <- predict(svm_model1,x))

table(pred,y)

svm_tune1 <- tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune1)

svm_model1_after_tune <- svm(x,y, kernel="radial", cost=1, gamma=0.5)
summary(svm_model1_after_tune)

pred <- predict(svm_model1_after_tune,x)
system.time(predict(svm_model1_after_tune,x))

table(pred,y)


svm_model_after_tune <- svm(x, y, type="C-classification", kernel="radial", cost=1000, gamma=0.5)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))

table(pred,y)

#print(model)
#summary(model)
pred <- fitted(model)
predres <- factor(pred)  ## pred in (1,2)
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

nerr <- cnterr(cmpdata$actual,cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
## Test with test data
xte <- subset(testdata, select = -Y)
pred <- cbind(predict(model, xte), show.plot=TRUE)
## (same as:)
#pred <- fitted(svm_model_after_tune)
predres <- factor(pred)  ## pred in (1,2)
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

nerr <- cnterr(cmpdata$actual,cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
#______________________________________________________________________
#END OF TESTS FOR STARTING
#______________________________________________________________________


#______________________________________________________________________
#TESTS FOR FINAL VERSION
#______________________________________________________________________


# -- IMPORT DATA --
#svm library 
library("e1071") 
# read data from xls
library(readxl)

rm(list=ls())

# -- AUXILIARY FUNCS --
#func to return a random # of rows from dataset
#this will be used to tune the model Vars on a smaller set
randomRows = function(df,n){
  return(df[sample(nrow(df),n),])
}


round0 <- function(val) {
  return(round(val, digits=0))
}

cnterr <- function(V1, V2) {
  cnt <- 0
  for(i in 1:length(V1)) {
    if ( (V1[i] == "0" && V2[i] == "2") ||
         (V1[i] == "1" && V2[i] == "1") ) {
      cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
      cnt <- cnt+1; }
  }
  return(cnt)
}
# -- END OF AUXILIARY FUNCS --


#get data from file
df <- read_excel("/Users/MDo/Desktop/AA_Project/svm/project-default-credit-card-clients.xls", skip=1)# skip first collumn

#cleanning the data from file
#replace the spaces by _ in the name of last column, or just use Y, because of future had problems
colnames(df)[25] <- "Y"
names <- names(df)[!names(df) %in% c("ID")]
clean_data <- df[names]

#encode a vector as a categorical type
clean_data$Y <- factor(clean_data$Y)
#create 2 subsets one from training the model and another for testing
#training 2/3, testing 1/3
index <- 1:nrow(clean_data)
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(clean_data[testindex,])
traindata <- na.omit(clean_data[-testindex,])

#TUNNING VALUES FOR SVM IN A SMALLER SUBSET
#get a subset of 5000 random rows of the 30000
subset_ <- randomRows(clean_data, 1000)
index <- 1:nrow(subset_)
testindex_ <- sample(index, trunc(length(index)/3))
testdata_ <- na.omit(subset_[testindex,])
traindata_ <- na.omit(subset_[-testindex,])


#professor example
x <- subset(traindata_, select = -Y)#list all except
y <- traindata_$Y
model <- svm(x, y, type="C-classification", kernel="radial", cost=1, gamma=0.2  )
summary(model)
pred <- predict(model,x)
#check time for perdiction
system.time(predict(model,x))

table(pred,y)

## Test with test data
xte <- subset(testdata_, select = -Y)
pred <- cbind(predict(model, xte))
## (same as:)
##pred <- fitted(model)

predres <- factor(pred)  ## pred in (1,2)
cmpdata <- data.frame(actual=testdata_$Y, predicted=predres)

nerr <- cnterr(cmpdata$actual,cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
#RESULTS OF
#TEST 1
# model <- svm(x, y, type="C-classification", kernel="radial", cost=1000)
#------------------------------------------------------
# table(pred,y)
# y
# pred    0    1
# 0 2594  109
# 1   15  634
# (...)
# cat(sprintf("Percent errors: %f\n", errprct))
# Percent errors: 24.450000
#
#TEST 2
# model <- svm(x, y, type="C-classification", kernel="radial", cost=1, gamma=0.5  )
#------------------------------------------------------
# table(pred,y)
# y
# pred    0    1
# 0 2607  255
# 1    8  498
# (...)
# cat(sprintf("Percent errors: %f\n", errprct))
# Percent errors: 20.770000
#TEST 3
# model <- svm(x, y, type="C-classification", kernel="radial", cost=1, gamma=0.2  )
#------------------------------------------------------
# table(pred,y)
# y
# pred    0    1
# 0 2502  374
# 1   51  395
# (...)
# cat(sprintf("Percent errors: %f\n", errprct))
# Percent errors: 18.830000

svm_tune <- tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=10^(-1:5), gamma=c(.2,.5,.7,1,2)))
print(svm_tune)
summary(svm_tune)
#RESULTS OF
#TEST 1
# svm_tune1 <- tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=10^(-1:4), gamma=c(.5,1,2)))
# print(svm_tune1)
#------------------------------------------------------
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma
# 1   0.5
# 
# - best performance: 0.2139037 
#TEST 2
#svm_tune1 <- tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=10^c(-1,2,4), gamma=c(.5,.8,1,2)))
#print(svm_tune1)
#------------------------------------------------------
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma
# 0.1   0.2
# 
# - best performance: 0.2292707 
#TEST 3
# svm_tune1 <- tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=10^c(-1,0,2), gamma=c(.2,.5,.8,1)))
# print(svm_tune1)
#------------------------------------------------------
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma
# 1   0.2
# 
# - best performance: 0.1863795 
#TEST 4
# svm_tune <- tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=10^(-1:4), gamma=c(.2,.5,1,2)))
# print(svm_tune)
# ------------------------------------------------------
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma
# 1   0.2
# 
# - best performance: 0.1977363 
# ------------------------------------------------------
# summary(svm_tune)
# 
# Parameter tuning of 'svm':
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   cost gamma
# 1   0.2
# 
# - best performance: 0.1977363 
# 
# - Detailed performance results:
#   cost gamma     error dispersion
# 1  1e-01   0.2 0.2287536 0.02065736
# 2  1e+00   0.2 0.1977363 0.01906487
# 3  1e+01   0.2 0.2215947 0.02393054
# 4  1e+02   0.2 0.2600693 0.02113690
# 5  1e+03   0.2 0.2851262 0.02283827
# 6  1e+04   0.2 0.3018221 0.02117882
# 7  1e-01   0.5 0.2317333 0.01861376
# 8  1e+00   0.5 0.2177230 0.02022327
# 9  1e+01   0.5 0.2409861 0.02071382
# 10 1e+02   0.5 0.2663433 0.02161972
# 11 1e+03   0.5 0.2785732 0.02184256
# 12 1e+04   0.5 0.2794678 0.02095199
# 13 1e-01   1.0 0.2317333 0.01861376
# 14 1e+00   1.0 0.2260732 0.02181283
# 15 1e+01   1.0 0.2448685 0.02507328
# 16 1e+02   1.0 0.2573969 0.02735520
# 17 1e+03   1.0 0.2594856 0.02545044
# 18 1e+04   1.0 0.2591871 0.02475540
# 19 1e-01   2.0 0.2317333 0.01861376
# 20 1e+00   2.0 0.2299458 0.02009359
# 21 1e+01   2.0 0.2439694 0.01796678
# 22 1e+02   2.0 0.2508271 0.01624244
# 23 1e+03   2.0 0.2499334 0.01664070
# 24 1e+04   2.0 0.2499334 0.01664070

#______________________________________________________________________
#END OF TESTS FOR FINAL VERSION
#______________________________________________________________________