library(caret)

#Loading the raw data
rawData <- read.csv(file='./data/pml-training.csv', na.strings=c(""," ","NA"))

#number of cases/observations and covariates
dim(rawData)

# Five different fashions (Unilateral Dubbell Biceps Curl)
table(rawData$classe)

# num of participants
table(rawData$user_name)


###########################DATA CLEANING#############################
#Find covariates with >= 80% NAs
#too many missing data -> no reason to impute and increase the noise
#better robust model can be built without them (prevent overfitting)
covNames <- names(rawData)

#covariates with too many missing ('NA') values
cov_na <- colSums(is.na(rawData)) >= ceiling(dim(rawData)[1] * 0.8)
rawData_adj <- rawData[, !cov_na]

#find covariates with near no variabilities  
nsv <- nearZeroVar(rawData_adj, saveMetrics=TRUE)
names(rawData_adj)[nsv$nzv==TRUE]
rawData_adj <- rawData_adj[,!nsv$nzv==TRUE]

#Also, other data that likely does not contribute to the class outcome
#X, user_name, cvtd_timestamp
cov_drop <- c("X", "user_name", "cvtd_timestamp")
rawData_cleaned <- rawData_adj[ , !(names(rawData_adj)) %in% cov_drop]

#for the TEST set, we'll keep all the names of the covariate that we kept
covariates_kept <- names(rawData_cleaned)

#scale and
#Look at cleaned data to see if covariate dimension can be further reduced
#Look at the paired correlation
preObj <- preProcess(rawData_cleaned[,-57], method=c("center", "scale"))
#use predict function to apply the obj holding the transformed data to the training dataset
rawData_cleaned <- predict(preObj, rawData_cleaned)


#do explorative data analysis
#show that they are correlated through plot
M <- abs(cor(as.data.frame(lapply(rawData_cleaned[,-57], as.numeric)))) #results in 57 x 57 matrix
diag(M) <- 0 #correlation with it own self is 1 so...
which(M > 0.9, arr.ind=T)
library(corrplot)
corrplot(M, method="circle")


#PCA to further reduce the dimensionality
set.seed(1111)
preProc <- preProcess(rawData_cleaned[,-57], method="pca", thresh=0.98)
preProc$numComp
rawData_cleaned <- predict(preProc, rawData_cleaned)

#######################################################################


##########################Training and Testing###############################
#split the training data into training and testing
inTrain <- createDataPartition(y=rawData_cleaned$classe,
                               p=0.75, list=F)
#data sets for training and testing
trainSet <- rawData_cleaned[inTrain,]
testSet <- rawData_cleaned[-inTrain,]

X<-subset(trainSet, select=-c(classe))
Y<-trainSet$classe
treebag <- bag(X,Y,B=20,
               bagControl=bagControl(
                 fit=ctreeBag$fit, 
                 predict=ctreeBag$pred, 
                 aggregate=ctreeBag$aggregate
               )
)
#predict and estimate the accuracy
predicted <- predict(treebag, subset(testSet, select=-c(classe)))
table(predicted == testSet$classe)

########################################################################

##################K-Fold Cross Validation###################
K=10
folds <- createFolds(testSet$classe, k=K, list=TRUE, returnTrain=FALSE)
miss_class <- rep(-1, K)

for (i in 1:K) {
  #obtain train and test set
  trSet <- testSet[-folds[[i]], ]
  ttSet <- testSet[folds[[i]], ]
  
  #fit a model
  x<-subset(trSet, select=-c(classe))
  y<-trSet$classe
  fitModel_bag <- bag(x,y,B=20,
                    bagControl=bagControl(
                    fit=ctreeBag$fit, 
                    predict=ctreeBag$pred, 
                    aggregate=ctreeBag$aggregate
                  )
  )
  
  #test the model
  true_output <- ttSet$classe
  ttSet <- subset(ttSet, select=-c(classe))
  modelPre <- predict(fitModel_bag, newdata=ttSet)
  
  #record misclassification rate
  ms <- sum((true_output == modelPre)*1)/length(modelPre)
  miss_class[i] <- ms
  
  as.character(ms)
}

sum(miss_class)/length(miss_class)
############################################################




#####################Applying on the Test Data set#########################

#Loading the raw data
testData_raw <- read.csv(file='./data/pml-testing.csv', na.strings=c(""," ","NA"))
dim(testData_raw)

testData_raw <- subset(testData_raw, select=covariates_kept[-56])
#scaled
testData_cleaned <- predict(preObj, testData_raw)
#PCA
testData_cleaned <- predict(preProc, testData_cleaned)

#Predict
predict(treebag, testData_cleaned)
########################################################################



