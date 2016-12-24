##BUILD TO TEST PARAMETERS FOR THE NEURALNET FUNCTION
#------------------------------------------------------#

# -- IMPORT DATA --
#neuralnet library 
library("neuralnet")
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



#Save to log
#------------------------------------------------------#
con <- file("/Users/MDo/Desktop/AA_Project/nn/test_NNparam.log")
sink(con, append=TRUE);
sink(con, append=TRUE, type="message")

## This will echo all input and not truncate 150+ character lines...
#source("/Users/MDo/Desktop/AA_Project/nn/Test_var_NN.R", echo=TRUE, max.deparse.length=10000)

#end of save to log
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
#TUNNING VALUES FOR NEURALNET IN A SMALLER SUBSET

#get a subset of 10000 random rows of the 30000
subset_ <- randomRows(normalized_data, 10000)
#create 2 subsets one from training the model and another for testing
#vary the percentage of trainning / test data
# 50% testing vs 50% trainning
# 33% testing vs 66% training
# 25% testing vs 75% 
# 20% testing vs 80%  
n_list <- list( 2, 3, 4, 5 )

for( n in n_list ){
  #log value of split
  cat(sprintf("\ntrunc(length(index_)/ %d\n\n", n))
  
  #split data into trainning and testing 
  index_ <- 1:nrow(subset_)
  testindex_ <- sample(index_, trunc(length(index_)/ n ))
  testdata_ <- na.omit(df[testindex_,])
  traindata_ <- na.omit(df[-testindex_,])
  
  ## Train MLP with neuralnet:
  names <- names(traindata_)
  #test multiple setting of hiddenlayers
  
  #several combination of hidden layers to test 
  h1=0
  h2=8
  h3=12
  h4=c(6,4)
  h5=c(12,4)
  h5=c(12,6)
  h6=c(16,10)
  h7=c(3,6,3)
  h8=c(7,4,3)
  h9=c(10,5,3)
  h10=c(12,6,2)
  h11=c(18,10,5)
  h12=c(8,16,6)
  h13=c(6,12,10,5)
  h14=c(16,10,4,8)
  
  #list of testing hiden layers combinations
  h_list = list(h1, h2, h3 ,h4 ,h5, h6, h7, h8, h9, h10, h11, h12, h13, h14)
  #cycle hiden layers variations
  for(h in h_list ){
    #list of testing thresholds
    t_list = list( 0.1, 0.25, 0.4, 0.6, 0.7, 0.9 ) 
    #cycle thresholds
    for(t in t_list ){
      nn <- neuralnet(paste("Y ~",
                            paste(names[!names %in% c("Y", "X")],
                                  collapse=" + ")),
                            traindata_, 
                            hidden= h, #parameter 'hidden' to test differente values
                            act.fct="logistic",
                            linear.output=FALSE, 
                            threshold= t, #parameter 'threshold' to test differente values
                            lifesign = "minimal")

      ## Test MLP
      cmpv<-data.frame(actual=traindata_$Y,predicted=nn$net.result)
      names(cmpv) <- sub("^structure.*", "predicted", names(cmpv))
      
      tstdata <- subset(testdata_, select = !names(testdata_) %in% c("Y"))
      nn.pred <- compute(nn, tstdata)$net.result
  
      predres <- apply(nn.pred, MARGIN=2, FUN=round0)
      cmpdata <- data.frame(actual=testdata_$Y, predicted=predres)
  
      nerr <- cnterr(cmpdata$actual-cmpdata$predicted)
      errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
      cat(sprintf("Percent errors: %f\n", errprct))
    }
  }
}