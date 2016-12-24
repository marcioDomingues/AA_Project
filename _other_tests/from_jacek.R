# -- IMPORT DATA --
# read data from xls
rm(list = ls())
library(readxl)
setwd("/Users/MDo/Desktop/AA_Project/svm")
data <- read_excel("default_small.xls", skip=1)


# check if data is missing
apply(data, 2, function (x) sum(is.na(x))) # no NA -> good
# ID is useless
names <- names(data)[!names(data) %in% c("ID")]
cleared_data <- data[names]
# normalize input with min-max
maxs <- apply(cleared_data, 2, max) 
mins <- apply(cleared_data, 2, min)
normalized_data <- as.data.frame(scale(cleared_data, center = mins, scale = maxs - mins))
# divide into train and test sets with 2/3 and 1/3 parts of the normalized set, respectively
set.seed(1234)
ind <- sample(2, nrow(normalized_data), replace=TRUE, prob=c(0.67, 0.33))
train <- normalized_data[ind==1,]
test <- normalized_data[ind==2,]

# train knn
library(class)
knn.train <- train[,1:23]
knn.test <- test[,1:23]
knn.trainLabels <- train[,24]
knn.testLabels <- test[,24]
knn.pred <- knn(train=knn.train, test=knn.test, cl=knn.trainLabels, k=4)
# train the net
library(neuralnet)
#f <- as.formula(paste("`default payment next month` ~", paste(names[!names %in% "default payment next month"], collapse = " + ")))
f <- as.formula(paste("Y ~", paste(names[!names %in% "default payment next month"], collapse = " + ")))
#nn <- neuralnet(f,data=train,hidden=c(15),linear.output=F)
nn <- neuralnet(f,data=train,hidden=c(5,2),linear.output=F)
#MY TESTES
#> nn <- neuralnet(f,data=train,hidden = 4, lifesign = "minimal", 
#                  +                 linear.output = FALSE, threshold = 0.1)
#hidden: 4    thresh: 0.1    rep: 1/1    steps:    8706	error: 1324.70855	time: 51.13 secs
#> plot(nn)

#nn <- neuralnet(f,data=train,hidden = 12, lifesign = "minimal", 
#                +                 linear.output = FALSE, threshold = 0.1)
#hidden: 12    thresh: 0.1    rep: 1/1    steps: 
#hidden: 12    thresh: 0.1    rep: 1/1    steps:    1897	error: 15.43319	time: 1.21 secs
#> plot(nn, rep = "best")creditnet.results <- compute(creditnet, temp_test)

#nn <- neuralnet(f,data=train,hidden = c(8, 5), lifesign = "minimal", 
#                linear.output = FALSE, threshold = 0.1)
#hidden: 8, 5    thresh: 0.1    rep: 1/1    steps:    1550	error: 22.36347	time: 1.11 secs


#plot(nn)