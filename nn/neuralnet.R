# -- IMPORT DATA --
#neuralnet library 
library("neuralnet")
# read data from xls
library(readxl)

rm(list=ls())

#------------------------------------------------------#
# -- AUXILIARY FUNCS --
#normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}


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

# -- END OF AUXILIARY FUNCS --
#------------------------------------------------------#



#get data from file
df <- read_excel("/Users/MDo/Desktop/AA_Project/project-default-credit-card-clients.xls", skip=1)# skip first collumn

# check if data is missing
apply(df, 2, function (x) sum(is.na(x))) # no NA -> good

#cleanning the data from file
#replace the spaces by _ in the name of last column, or just use Y, because of future had problems
colnames(df)[25] <- "Y"
names <- names(df)[!names(df) %in% c("ID")]
clean_data <- df[names]


# normalize input with min-max
maxs <- apply(clean_data, 2, max) 
mins <- apply(clean_data, 2, min)
normalized_data <- as.data.frame(scale(clean_data, center = mins, scale = maxs - mins))


#------------------------------------------------------#
#APPLYING TO THE WHOLE DATASET
#------------------------------------------------------#
#create 2 subsets one from training the model and another for testing
#training 2/3, testing 1/3
index <- 1:nrow(normalized_data)
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(normalized_data[testindex,])
traindata <- na.omit(normalized_data[-testindex,])

nn <- neuralnet(paste("Y ~", paste(names[!names %in% c("Y")],collapse=" + ")),
                traindata, 
                #i will go with the h6 set of hidden layers 
                #its the one with results fitting the other sets and with a lower time
                hidden=c(12), 
                act.fct="logistic",
                linear.output=FALSE, 
                threshold=0.1, 
                lifesign = "minimal")

#print(nn)
plot(nn)


## Test MLP
cmpv<-data.frame(actual=traindata$Y, predicted=nn$net.result)
names(cmpv) <- sub("^structure.*", "predicted", names(cmpv))
#print(cmpv[1100:1150, ])


tstdata <- subset(testdata, select = !names(testdata) %in% c("Y"))
nn.pred <- compute(nn, tstdata)$net.result

#rounds the nn.pred values to 1 or 0 by a margin of 2 decimal points
predres <- apply(nn.pred, MARGIN=2, FUN=round0)
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

print(cmpdata[1100:1150, ])

#confusion table
table( actual=testdata$Y, predicted=predres )

#ROC curve
ROCR_pred=prediction(nn.pred,testdata$Y)
ROCR_perf=performance(ROCR_pred,"tpr","fpr")
plot(ROCR_perf)


nerr <- cnterr(cmpdata$actual-cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))


