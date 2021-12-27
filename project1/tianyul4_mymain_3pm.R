library(Metrics)
library(glmnet)

setwd("/Users/mac/Desktop/大四/CS598/project1")
#############################################################################
#Part 1 dealing with the missing values preprocessing the training/test data

#In the function ProcessWinsorization, compute the upper 95% quantile of the train column, 
#denoted by M; then replace all values in the train and test that are bigger than M by M. 
#https://blogs.sas.com/content/iml/2017/02/08/winsorization-good-bad-and-ugly.html replace the extreme value
ProcessWinsorization <- function(train.col, test.col, upper){
  train <- as.numeric(train.col)
  test <- as.numeric(test.col)
  M <- quantile(train,probs=upper)
  index = which(train > M)
  id = which(test > M)
  for(i in index){
    train[i] = M
  }
  for(i in id){
    test[i] = M
  }
  return(list(train = as.matrix(train), test = as.matrix(test)))
}

#Refer this function from https://github.com/panzhang666/Statistical-Machine-learning
PreProcessingMatrixOutput <- function(train.data, test.data){
  # generate numerical matrix of the train/test
  # assume train.data, test.data have the same columns
  categorical.vars <- colnames(train.data)[which(sapply(train.data, 
                                                        function(x) is.character(x)))]
  train.matrix <- train.data[, !colnames(train.data) %in% categorical.vars, drop=FALSE]
  test.matrix <- test.data[, !colnames(train.data) %in% categorical.vars, drop=FALSE]
  n.train <- nrow(train.data)
  n.test <- nrow(test.data)
  for(var in categorical.vars){
    mylevels <- sort(unique(train.data[, var]))
    m <- length(mylevels)
    tmp.train <- matrix(0, n.train, m)
    tmp.test <- matrix(0, n.test, m)
    col.names <- NULL
    for(j in 1:m){
      tmp.train[train.data[, var]==mylevels[j], j] <- 1
      tmp.test[test.data[, var]==mylevels[j], j] <- 1
      col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(tmp.train) <- col.names
    colnames(tmp.test) <- col.names
    train.matrix <- cbind(train.matrix, tmp.train)
    test.matrix <- cbind(test.matrix, tmp.test)
  }
  return(list(train = as.matrix(train.matrix), test = as.matrix(test.matrix)))
}


#load the training and test data
data <- read.csv("Ames_data.csv", stringsAsFactors=FALSE)
testIDs <- read.table("project1_testIDs.dat")

#change the X matrix to a numerical matrix (no factors)
#loop start for 1 to 0.
for(j in 1:10)
{
test.dat <- data[testIDs[,j], ]
train.dat <- data[-testIDs[,j], ]
n.train = dim(train.dat)[1]
n.test = dim(test.dat)[1]

#Linear Regression preparation, remove pid and sale-price
train.y <- log(train.dat$Sale_Price)
names(train.y) <- "Sale_Price"
train.x <- subset(train.dat,select=-c(PID, Sale_Price))
test.y <- log(test.dat$Sale_Price)
test.PID <- test.dat$PID
test.x <- subset(test.dat,select=-c(PID, Sale_Price))

#Remove the following variables
#Refer from https://github.com/JingyiZ9/Housing-Prices-in-Ames-Forcasting/tree/c7d60523de120428c3693ed6cdefaf128c9b1c9b
train.x <- subset(train.dat,select= -c(Street, Utilities, Condition_2, Roof_Matl, Heating,
                                       Pool_QC, Misc_Feature, Low_Qual_Fin_SF, Pool_Area, 
                                       Longitude, Latitude, PID, Sale_Price))
test.x <- subset(test.dat,select= -c(Street, Utilities, Condition_2, Roof_Matl, Heating,
                                     Pool_QC, Misc_Feature, Low_Qual_Fin_SF, Pool_Area, 
                                     Longitude, Latitude, PID, Sale_Price))

#Set "Mo_Sold" and "Year_Sold" as categorical variables
train.x$YrSold <- as.character(train.x$Year_Sold)
train.x$MoSold <- as.character(train.x$Mo_Sold)
test.x$YrSold <- as.character(test.x$Year_Sold)
test.x$MoSold <- as.character(test.x$Mo_Sold)


# Apply winsorization on the following numerical variables
# choose by looking resources to find variables existing extreme value and check with hhttps://github.com/xinyis8/Statistical-Learning
winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", 
                 "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', 
                 "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", 
                 "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")


for(var in winsor.vars){
  r <- ProcessWinsorization(train.x[, var], test.x[, var], 0.95)
  train.x[, var] <- r$train
  test.x[, var] <- r$test
}

#Call PreProcessingMatrixOutput to create the design matrix
total <- PreProcessingMatrixOutput(train.x, test.x)
train.x <- total$train
test.x <- total$test

#dealing with the missing values  
id1 <- which(is.na(train.x[,'Garage_Yr_Blt']))
id2 <- which(is.na(test.x[,'Garage_Yr_Blt']))
train.x[id1, 'Garage_Yr_Blt'] <- 0
test.x[id2, 'Garage_Yr_Blt'] <- 0

#call glmnet
set.seed(1820)
cv.out <- cv.glmnet(train.x, train.y, alpha = 0.1) # change alpha to 0.1 by adjusting the parameters
pred <-predict(cv.out, s = cv.out$lambda.min, newx = test.x)
#write.csv(pred, "mysubmission2.txt", row.names = FALSE, quote = FALSE)
#pred <- read.csv("mysubmission1.txt")

#For display the error
print(j)
print(sqrt(mean((pred-test.y)^2)))
}

