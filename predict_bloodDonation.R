predictDonation <- function(first, last, volume, fit.glm, fit.rpart, fit.svm, fit.rf){
        
        #transform to new variables:
        #DCoeff <- last/first
        #DFreq <- (first-last)/(round(volume/250))
        #
        nd<-data.frame(DonorStart=first, 
                   DonorLast=last, 
                   VolumeDonated=volume)
        
        pred.glm.probs <- predict(fit.glm, 
                                  newdata=nd,
                                  type='prob')
        
        pred.df <- data.frame(Algorithm="Logistic Regression ",
                              Prediction=attr(which.max(pred.glm.probs),"names"),
                              Probability=paste0(round(max(pred.glm.probs),5)*100,"%"),
                              LogLoss=(mean(fit.glm$resample[[1]])))
        
        pred.rpart.probs <- predict(fit.rpart, 
                                  newdata=nd,
                                  type='prob')
        pred.df <- rbind(pred.df, data.frame(Algorithm="Classification Tree ",
                                             Prediction=attr(which.max(pred.rpart.probs),"names"),
                                             Probability=paste0(round(max(pred.rpart.probs),5)*100,"%"),
                                             LogLoss=(mean(fit.rpart$resample[[1]])))
                         )
        
        pred.svm.probs <- predict(fit.svm, 
                                    newdata=nd,
                                    type='prob')
        pred.df <- rbind(pred.df, data.frame(Algorithm="Support Vector Machine ",
                                             Prediction=attr(which.max(pred.svm.probs),"names"),
                                             Probability=paste0(round(max(pred.svm.probs),5)*100,"%"),
                                             LogLoss=(mean(fit.svm$resample[[1]])))
        )
        
        pred.rf.probs <- predict(fit.rf, 
                                  newdata=nd,
                                  type='prob')
        
        pred.df <- rbind(pred.df, data.frame(Algorithm="Random Forest ",
                                             Prediction=attr(which.max(pred.rf.probs),"names"),
                                             Probability=paste0(round(max(pred.rf.probs),5)*100,"%"),
                                             LogLoss=(mean(fit.rf$resample[[1]])))
        )
        pred.df <- pred.df[order(pred.df$LogLoss),]
        row.names(pred.df) <- NULL
        pred.df$LogLoss <- as.character(pred.df$LogLoss)
        pred.df
}