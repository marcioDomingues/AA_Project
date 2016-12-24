library(e1071) 
library(readxl)

rm(list=ls())

round0 <- function(val) {
    return(round(val, digits=0))
}

cnterr <- function(V1, V2) {
    cnt <- 0
    for(i in 1:length(V1)) {
        if (V1[i] != V2[i])  {
            #cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
            cnt <- cnt+1; }
    }
    return(cnt)
}

df <- read_excel("/Users/MDo/Desktop/AA_Project/svm/project-default-credit-card-clients.xls", skip=1)# skip first collumn
#cleanning the data from file
#replace the spaces by _ in the name of last column, or just use Y, because of future had problems
colnames(df)[25] <- "Y"
names <- names(df)[!names(df) %in% c("ID")]
df <- df[names]

df$Y <- factor(df$Y)

index <- 1:nrow(df)
#split data into 33% test vs 66% train
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(df[testindex,])
traindata <- na.omit(df[-testindex,])

## Train naive bayes:
names <- names(traindata)
#fmt <- paste("Y ~", paste(names[!names %in% c("Y", "X")], collapse=" + "))

#library(class)
#pairs( df, main = "Data (red=1,green=0)",pch = 21, bg = c("red", "green3", "blue")[unclass(df$Y)])

## Train SVM with e1071:
x <- subset(traindata, select = -Y)
y <- traindata$Y

# creates a classifier using Gender and Dept as data, and Admittance as the observation class
model <- naiveBayes( x , y  )

print(model)
summary(model)

## Test with test data
xte <- subset(testdata, select = -Y)
pred <- predict(model, xte)

head(pred)
table(pred, testdata$Y, dnn=list('predicted','actual'))
model$apriori / sum(model$apriori)

predres <- factor(pred)  ## pred in (1,2)
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

nerr <- cnterr(cmpdata$actual,cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))

