library(caret)
library(ggplot2)

# if getting from drivendata.org
competition.train.path <- "https://drivendata.s3.amazonaws.com/data/2/public/9db113a1-cdbe-4b1c-98c2-11590f124dd8.csv"
competition.test.path <- "https://drivendata.s3.amazonaws.com/data/2/public/5c9fa979-5a84-45d6-93b9-543d1a0efc41.csv"
competition.format.path <- "https://drivendata.s3.amazonaws.com/data/2/public/BloodDonationSubmissionFormat.csv"

# locally downloaded 
path <- '/Users/ash/Downloads/data/'
pred.file <- read.csv(paste0(path,"driven data blood donation test.csv"), header=T)
submit.format <- read.csv(paste0(path, "BloodDonationSubmissionFormat.csv"), header=T, check.names = F)



getTestTrain <- function(path){
        raw <- read.csv(paste0(path,"driven data blood train.csv"), header=T)
        summary(raw)
        
        # No missing values
        # No NZV features
       
        # Convert dependent variable to factor, friendly levels, friendly columns
        colnames(raw)[c(2, 5, 6)] <- c("DonorLast","DonorStart","isMarch07Donor")
        colnames(pred.file)[c(2, 5)] <- c("DonorLast","DonorStart")
        raw$isMarch07Donor <- as.factor(raw$isMarch07Donor)
        levels(raw$isMarch07Donor) <- c("NotMarch07Donor","March07Donor")
        
        
        # seed and split into test/train: 70/30
        set.seed(3142857)
        inTrain <- createDataPartition(raw$isMarch07Donor, p=0.7, list=F)
        train <- raw[inTrain,]
        test <- raw[-inTrain,]
        
        #featureEng(train,test)
        
}

featureEng <- function(train,test){
        #barplot(table(raw$isMarch07Donation)) 
        #outcome is skewed toward "no donations" by about 68%
        #
        #NEW FEATURES
        ## Donor Coefficient, ratio of most recent donation date to first active span
        ## Helps give an idea of activity since beginning (when combined with DOnorFreq)
        train$DonorCoeff <- with(train, DonorLast/DonorStart)
        test$DonorCoeff <- with(test, DonorLast/DonorStart)
        #pred.file$DonorCoeff <- with(pred.file, DonorLast/DonorStart)
        
        ## Donor Frequency is avg. months between donations during activity period
        train$DonorFreq <- with(train, (DonorStart-DonorLast)/Number.of.Donations)
        test$DonorFreq <- with(test, (DonorStart-DonorLast)/Number.of.Donations)
        #pred.file$DonorFreq <- with(pred.file, (DonorStart-DonorLast)/Number.of.Donations)
        
        #
        #OUTLIERS:
        #Data points where DonorLast > 70 are one time donations. Removing this
        #reduces the correlation between DonorLast and DonorStart somewhat, so I'll
        #remove them.
        train <- train[train$DonorLast<70,]
        #
        #MULTICOLLINEARITY
        #Number of Donations is exactly correlated to Total Vol. DOnated!
        #Remove one of them. 
        train <- train[,-c(1,3)]
        test <- test[,-c(1,3)]
        #vif on the model shows multicollinearity is acceptable
        sqrt(vif(glm(isMarch07Donor~.,data=train,family='binomial')))
        
        
        
        #OVERSMAPLING:
        #tried over-sampling using SMOTE method but it reduced accuracy
        #Consider trying it limited in the future
        #
        #
        #
        
        #featurePlot(x=train[,c(2:5)], y=train[,6], plot='pairs',
        #            auto.key=list(columns=3))
        # # of donations and volume donated are exactly correlated!!
     

        featurePlot(x=scale(train[,c(1,2,3,5,6)], center=TRUE), 
                    y=train[,4], plot='density',
                    scales = list(x = list(relation="free"),
                                  y = list(relation="free")),
                    adjust = 1.5,
                    pch = "|")
        
        featurePlot(x=log(train[,c(1,2,3,5,6)]+5), 
                    y=train[,4], plot='density',
                    scales = list(x = list(relation="free"),
                                  y = list(relation="free")),
                    adjust = 1.5,
                    pch = "|")
        
        #okay! let's use a log transform then (a little more normal looking predictors)
        train.log <- data.frame(log(train[,c(1,2,3,5,6)]+5),isMarch07Donor=train$isMarch07Donor)
        
        #t.tests on each cotninuous predictor against outcome 
        #show DonorStart isn't significantly contributing to mean variance
        #between 2 target outcomes;  DonorFreq is borderline
        #remove from analysis?? build a competing model with these.
        apply(train[,-4], 2, function(x){
                t.test(x~isMarch07Donor,data=train.log)$p.value
                })
        
        #Finally ANOVA test between these competing models
        #
        anova(glm(isMarch07Donor~.,data=train.log[,c(6:1)],family='binomial'), test = 'Chisq')
        #anova(glm(isMarch07Donor~.,data=train.log[,-c(1,3)],family='binomial'), test='Chisq')
        
        #Looks like the best statistically significant reductions in residual deviation
        #are from model: isMarch07Donor~DonorFreq+DonorCoeff+Total.Volume
        rm(train.log)
        train.final <- train[,-c(1,3)]
        test.final <- test[,-c(1,3)]
        
}

trainMods <- function(train){
        library(doMC)
        registerDoMC(cores=detectCores())
        
        # Going to run each algorithm on two models: 
        # original feature set and train.final
        # pre-processing: Center, Scale all numeric features
        # pre-processing: No dummies, no NZV
        # 
        # 
        # =====================================================================
        # 1. GLM
        # =====================================================================
        fit.glm.base <- train(isMarch07Donor~., data=train[,-c(5,6)],
                         preProcess=c('center','scale'), method="glm",
                         metric='logLoss',
                         trControl=trainControl(method='repeatedcv',
                                                number = 20,
                                                repeats = 20,
                                                summaryFunction = mnLogLoss,
                                                classProbs = T,
                                                allowParallel = T))
        #logLoss ~ 0.48 
        fit.glm.final <- train(isMarch07Donor~., data=train.final,
                              preProcess=c('center','scale','BoxCox'), method="glm",
                              metric='logLoss',
                              trControl=trainControl(method='repeatedcv',
                                                     number = 20,
                                                     repeats = 20,
                                                     summaryFunction = mnLogLoss,
                                                     classProbs = T,
                                                     allowParallel = T))
        #logLoss ~ 0.50
        
        
        pred.glm.base <- predict(fit.glm.base, newdata=test) 
        pred.glm.base.probs <- predict(fit.glm.base, newdata=test, type='prob')
        #acc. 76.74%; kappa=0.1632; logLoss = 0.4753314
        
        pred.glm.final <- predict(fit.glm.final, newdata=test.final) 
        pred.glm.final.probs <- predict(fit.glm.final, newdata=test.final, type='prob')
        #acc. 76.16%; kappa=0.0486; logLoss = 0.4816045
        #
        dat <- data.frame(obs = test$isMarch07Donor, pred = pred.glm.base, NotMarch07Donor = pred.glm.base.probs[1], March07Donor = pred.glm.base.probs[2])
        mnLogLoss(dat, lev=levels(test$isMarch07Donor))
        # =====================================================================

        # =====================================================================
        # 2. Random Forest
        # =====================================================================
        fit.rf.base <- train(isMarch07Donor~., data=train[,-c(5,6)],
                              preProcess=c('center','scale','BoxCox'), method="rf",
                              metric='logLoss',
                              trControl=trainControl(method='repeatedcv',
                                                     number = 20,
                                                     repeats = 20,
                                                     summaryFunction = mnLogLoss,
                                                     classProbs = T,
                                                     allowParallel = T))
        #logLoss ~ 1.14537
        fit.rf.final <- train(isMarch07Donor~., data=train.final,
                               preProcess=c('center','scale','BoxCox'), method="rf",
                               metric='logLoss',
                               trControl=trainControl(method='repeatedcv',
                                                      number = 20,
                                                      repeats = 20,
                                                      summaryFunction = mnLogLoss,
                                                      classProbs = T,
                                                      allowParallel = T))
        #logLoss ~ 1.2682
        pred.rf.base <- predict(fit.rf.base, newdata=test) 
        pred.rf.base.probs <- predict(fit.rf.base, newdata=test, type='prob')
        dat <- data.frame(obs = test$isMarch07Donor, pred = pred.rf.base, NotMarch07Donor = pred.rf.base.probs[1], March07Donor = pred.rf.base.probs[2])
        mnLogLoss(dat, lev=levels(test$isMarch07Donor))
        #acc = 74.42%, kappa=0.2019, logLoss = 1.35522
        # =====================================================================
        
        # =====================================================================
        # 3. SVM
        # =====================================================================
        fit.svm.base <- train(isMarch07Donor~., data=train[,-c(5,6)],
                              preProcess=c('center','scale','BoxCox'), method="svmRadial",
                              metric='logLoss', tuneLength=10,
                              trControl=trainControl(method='repeatedcv',
                                                     number = 20,
                                                     repeats = 20,
                                                     summaryFunction = mnLogLoss,
                                                     classProbs = T,
                                                     allowParallel = T))
        #logLoss ~ 0.48088 
        fit.svm.final <- train(isMarch07Donor~., data=train.final,
                               preProcess=c('center','scale','BoxCox'), method="svmRadial",
                               metric='logLoss', tuneLength=10,
                               trControl=trainControl(method='repeatedcv',
                                                      number = 20,
                                                      repeats = 20,
                                                      summaryFunction = mnLogLoss,
                                                      classProbs = T,
                                                      allowParallel = T))
        #logLoss ~ 0.48082
        
        
        pred.svm.base <- predict(fit.svm.base, newdata=test) 
        pred.svm.base.probs <- predict(fit.svm.base, newdata=test, type='prob')
        #acc. 79.07%; kappa=0.2469; logLoss = 0.50085
        
        pred.svm.final <- predict(fit.svm.final, newdata=test.final) 
        pred.svm.final.probs <- predict(fit.svm.final, newdata=test.final, type='prob')
        #acc. 77.33%; kappa=0.2573; logLoss = 0.51501
        #
        dat <- data.frame(obs = test$isMarch07Donor, pred = pred.svm.final, NotMarch07Donor = pred.svm.final.probs[1], March07Donor = pred.svm.final.probs[2])
        mnLogLoss(dat, lev=levels(test.final$isMarch07Donor))
        # =====================================================================
        
        # =====================================================================
        # 4. GBM
        # =====================================================================
        fit.gbm.base <- train(isMarch07Donor~., data=train[,-c(5,6)],
                              preProcess=c('center','scale','BoxCox'), method="gbm",
                              metric='logLoss',
                              trControl=trainControl(method='repeatedcv',
                                                     number = 20,
                                                     repeats = 20,
                                                     summaryFunction = mnLogLoss,
                                                     classProbs = T, verboseIter = F,
                                                     allowParallel = T),
                              tuneGrid=expand.grid(.interaction.depth=c(1,3,5,9),
                                                   .n.trees=(1:50)*50,
                                                   .shrinkage=.01:.09,
                                                   .n.minobsinnode=50))
        #logLoss ~ 0.4885 
        fit.gbm.final <- train(isMarch07Donor~., data=train.final,
                               preProcess=c('center','scale','BoxCox'), method="gbm",
                               metric='logLoss', verbose=FALSE,
                               trControl=trainControl(method='repeatedcv',
                                                      number = 20,
                                                      repeats = 20,
                                                      summaryFunction = mnLogLoss,
                                                      classProbs = T, verboseIter = F,
                                                      allowParallel = T))
        #logLoss ~ 0.49037
        
        
        pred.gbm.base <- predict(fit.gbm.base, newdata=test) 
        pred.gbm.base.probs <- predict(fit.gbm.base, newdata=test, type='prob')
        #acc. 76.16%; kappa=0.0926; logLoss = 0.4847
        
        pred.gbm.final <- predict(fit.gbm.final, newdata=test.final) 
        pred.gbm.final.probs <- predict(fit.gbm.final, newdata=test.final, type='prob')
        #acc. 80.23%; kappa=0.2728; logLoss = 0.49808
        #
        dat <- data.frame(obs = test.final$isMarch07Donor, pred = pred.gbm.final, NotMarch07Donor = pred.gbm.final.probs[1], March07Donor = pred.gbm.final.probs[2])
        mnLogLoss(dat, lev=levels(test.final$isMarch07Donor))
        # =====================================================================
}

pred.submit <- function(model){
        pred.final <- predict(glm.mod.1, newdata=pred.file, type="prob")
        pred.submit <- data.frame(pred.file$X, pred.final[2])
        colnames(pred.submit) <- colnames(submit.format)
        write.csv(pred.submit, "ash_submit_01062015.csv",row.names = F)
}