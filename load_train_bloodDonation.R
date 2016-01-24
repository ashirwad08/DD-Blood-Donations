library(caret)
library(devtools)
library(e1071)
library(randomForest)
library(rpart)
library(kernlab)
library(RCurl)

# if getting from drivendata.org
competition.train.path <- "http://drivendata.s3.amazonaws.com/data/2/public/9db113a1-cdbe-4b1c-98c2-11590f124dd8.csv"

# locally downloaded 
#path <- '/Users/ash/Downloads/data/'

train <- read.csv(competition.train.path, header=T)

        # Convert dependent variable to factor, friendly levels, friendly columns
        colnames(train)[c(2, 4, 5, 6)] <- c("DonorLast","VolumeDonated","DonorStart","isMarch07Donor")
        train$isMarch07Donor <- as.factor(train$isMarch07Donor)
        levels(train$isMarch07Donor) <- c("NotMarch07Donor","March07Donor")
        
        train <- train[,-c(1,3)]
        

        #NEW FEATURES
        ## Donor Coefficient, ratio of most recent donation date to first active span
        ## Helps give an idea of activity since beginning (when combined with DOnorFreq)
        #train$DonorCoeff <- with(train, DonorLast/DonorStart)
        #pred.file$DonorCoeff <- with(pred.file, DonorLast/DonorStart)
        
        ## Donor Frequency is avg. months between donations during activity period
       # train$DonorFreq <- with(train, (DonorStart-DonorLast)/Number.of.Donations)
        #test$DonorFreq <- with(test, (DonorStart-DonorLast)/Number.of.Donations)
        #pred.file$DonorFreq <- with(pred.file, (DonorStart-DonorLast)/Number.of.Donations)
        
        #Looks like the best statistically significant reductions in residual deviation
        #are from model: isMarch07Donor~DonorFreq+DonorCoeff+Total.Volume
        #rm(train.log)
        #train <- train[,c('DonorFreq','DonorCoeff','Total.Volume.Donated..c.c..','isMarch07Donor')]
        #test.final <- test[,-c(1,3)]

        #library(doMC)
        #registerDoMC(cores=detectCores())
        
        # Going to run each algorithm on two models: 
        # original feature set and train.final
        # pre-processing: Center, Scale all numeric features
        # pre-processing: No dummies, no NZV
        # 
        # 
        # =====================================================================
        # 1. GLM
        # =====================================================================
        fit.glm.final <- train(isMarch07Donor~., data=train,
                               preProcess=c('center','scale','BoxCox'), method="glm",
                               metric='logLoss',
                               trControl=trainControl(method='repeatedcv',
                                                      number = 3,
                                                      repeats = 3,
                                                      summaryFunction = mnLogLoss,
                                                      classProbs = T))

        fit.rpart.final <- train(isMarch07Donor~., data=train,
                               preProcess=c('center','scale','BoxCox'), method="rpart",
                               metric='logLoss',
                               trControl=trainControl(method='repeatedcv',
                                                      number = 3,
                                                      repeats = 3,
                                                      summaryFunction = mnLogLoss,
                                                      classProbs = T))
        fit.svm.final <- train(isMarch07Donor~., data=train,
                              preProcess=c('center','scale','BoxCox'), method="svmRadial",
                              metric='logLoss', tuneLength=5,
                              trControl=trainControl(method='repeatedcv',
                                                     number = 3,
                                                     repeats = 3,
                                                     summaryFunction = mnLogLoss,
                                                     classProbs = T))
        fit.rf.final <- train(isMarch07Donor~., data=train,
                               preProcess=c('center','scale','BoxCox'), method="rf",
                               metric='logLoss',
                               trControl=trainControl(method='repeatedcv',
                                                      number = 3,
                                                      repeats = 3,
                                                      summaryFunction = mnLogLoss,
                                                      classProbs = T,
                                                      allowParallel = T))