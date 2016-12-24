# -- IMPORT DATA --
#svm library 
library("e1071") 
# read data from xls
library(readxl)

rm(list=ls())

#------------------------------------------------------#
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
      #no need to print in the final version
      #cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
      cnt <- cnt+1; }
  }
  return(cnt)
}

# -- END OF AUXILIARY FUNCS --
#------------------------------------------------------#

#get data from file
df <- read_excel("/Users/MDo/Desktop/AA_Project/project-default-credit-card-clients.xls", skip=1)# skip first collumn

#cleanning the data from file
#replace the spaces by _ in the name of last column, or just use Y, because of future had problems
colnames(df)[25] <- "Y"
names <- names(df)[!names(df) %in% c("ID")]
clean_data <- df[names]

#encode a vector as a categorical type
clean_data$Y <- factor(clean_data$Y)


#------------------------------------------------------#
#TUNNING VALUES FOR SVM IN A SMALLER SUBSET
#------------------------------------------------------#
#get a subset of 1000 random rows of the 30000
subset_ <- randomRows(clean_data, 1000)

x_ <- subset(subset_, select = -Y)# - list all except
y_ <- subset_$Y

#tunning the cost and gamma parameters for the subset_ of 1000 rows 
#in order to get the best performing ones for the entire dataset 
#and its much farter to process more variations on the parameters
svm_tune <- tune(svm, train.x=x_, train.y=y_, kernel="radial", ranges=list(cost=10^(-1:5), gamma=c(.2,.5,.7,1,2)))
print(svm_tune)
plot(svm_tune)
summary(svm_tune)


#------------------------------------------------------#
#APPLYING TO DATASET
#------------------------------------------------------#
#create 2 subsets one from training the model and another for testing
#training 2/3, testing 1/3
index <- 1:nrow(clean_data)
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(clean_data[testindex,])
traindata <- na.omit(clean_data[-testindex,])

x <- subset(traindata, select = -Y)# - list all except
y <- traindata$Y

#Model construction using the best parameters for cost and gamma given above
model <- svm(x, y, type="C-classification", kernel="radial", cost=1, gamma=0.2  )
summary(model)
        #OUTPUT EXAMPLE
          # Call:
          #   svm.default(x = x, y = y, type = "C-classification", kernel = "radial", gamma = 0.2, cost = 1)
          #
          #
          # Parameters:
          #   SVM-Type:  C-classification
          # SVM-Kernel:  radial
          # cost:  1
          # gamma:  0.2
          #
          # Number of Support Vectors:  11251
          #
          # ( 3981 7270 )
          #
          #
          # Number of Classes:  2
          #
          # Levels:
          #   0 1
pred <- predict(model,x)
# #check time for perdiction
system.time(predict(model,x))
        #OUTPUT EXAMPLE
          # user  system elapsed
          # 12.593   0.061  12.669

table(pred,y)
        #OUTPUT EXAMPLE
          # y
          # pred     0     1
          # 0   15150  2508
          # 1   449  1893

#Test with test data
x_teste <- subset(testdata, select = -Y)
#Running the model against the testdata
pred <- cbind(predict(model, x_teste))

predres <- factor(pred)  ## pred in (1,2)
#Create dataframe with 2 columns for error calculation
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

table( cmpdata$actual, cmpdata$predicted )

#Use error calculation functions from class
nerr <- cnterr(cmpdata$actual,cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
#print error % from model
cat(sprintf("Percent errors: %f\n", errprct))
        #OUTPUT EXAMPLE
        #Percent errors: 18.360000

