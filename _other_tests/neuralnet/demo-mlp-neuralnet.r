library("neuralnet")

rm(list=ls())

round0 <- function(val) {
    return(round(val, digits=0))
}

cnterr <- function(V) {
    cnt <- 0
    for(i in 1:length(V)) {
        if (V[i] != 0) { cnt <- cnt+1; }
    }
    return(cnt)
}


df <- read.csv("/Users/MDo/Desktop/AA_Project/tictactoe_test/svm/tic-tac-toe.csv")
index <- 1:nrow(df)
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(df[testindex,])
traindata <- na.omit(df[-testindex,])

## Train MLP with neuralnet:
names <- names(traindata)
nn <- neuralnet(paste("Y ~",
                      paste(names[!names %in% c("Y", "X")],
                            collapse=" + ")),
                traindata, hidden=c(8,4,3), act.fct="logistic",
                linear.output=FALSE, threshold=0.1)
print(nn)
plot(nn)

## Test MLP
cmpv<-data.frame(actual=traindata$Y,predicted=nn$net.result)
names(cmpv) <- sub("^structure.*", "predicted", names(cmpv))
#print(cmpv)

tstdata <- subset(testdata, select = !names(testdata) %in% c("Y"))
nn.pred <- compute(nn, tstdata)$net.result

#rounds the nn.pred values to 1 or 0 by a margin of 2 decimal points
predres <- apply(nn.pred, MARGIN=2, FUN=round0)
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

#TEST______________________________
cmpdata[100:115, ]

table( actual=testdata$Y, predicted=predres )

pr.nn_ <- nn.pred*(max(df$Y)-min(df$Y))+min(df$Y)
test.r <- (testdata$Y)*(max(df$Y)-min(df$Y))+min(df$Y)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(testdata)

plot(testdata$Y,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
#______________________________



nerr <- cnterr(cmpdata$actual-cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
