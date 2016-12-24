library("rpart")
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

#df <- read.csv("/Users/MDo/Desktop/AA_Project/tictactoe_test/tree/tic-tac-toe.csv")

df <- read_excel("/Users/MDo/Desktop/AA_Project/svm/project-default-credit-card-clients.xls", skip=1)# skip first collumn
#cleanning the data from file
#replace the spaces by _ in the name of last column, or just use Y, because of future had problems
colnames(df)[25] <- "Y"
names <- names(df)[!names(df) %in% c("ID")]
df <- df[names]


index <- 1:nrow(df)
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(df[testindex,])
traindata <- na.omit(df[-testindex,])


## Train decision tree:
names <- names(traindata)
fmt <- paste("Y ~", paste(names[!names %in% c("Y", "X")], collapse=" + "))



model <- rpart(fmt, data = traindata)# ,control=rpart.control(minsplit=20,cp=0))


#plot(model, uniform=TRUE )#, compress=TRUE)
#text(model, use.n = TRUE)
library(rpart.plot)
rpart.plot(model, uniform=TRUE, compress=TRUE, branch=.2)




## Test decision tree:

#Test with test data
x_teste <- subset(testdata, select = -Y)

pred <- cbind(predict(model, x_teste ))

predres <-  apply(pred, MARGIN=2, FUN=round0)
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

nerr <- cnterr(cmpdata$actual, cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
