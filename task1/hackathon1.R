library(stringi)
library(rpart)
library(ggplot2)
library(caret)
library(corrplot)
library(gbm)
library(kernlab)
library(Metrics)
library(randomForest)
library(glmnet)


training_data <- read.csv("auto-insurance-check.csv", header=T, na.strings = c("NA", "","#DIV/0!"))
training_data <-training_data[rowSums(is.na(training_data)) == 0,]
training_data <-training_data[!duplicated(training_data[,c('НомерАвто', 'МаркаАвто', 'Штрафы')]),]

training_data$Region<-stri_extract_last_regex(training_data$НомерАвто, "[0-9]{2}RUS")
training_data$Region<-substr(training_data$Region,1,nchar(training_data$Region)-3)
training_data$Region<-as.numeric(as.character(training_data$Region))
training_data$CarType<-as.numeric(training_data$МаркаАвто)
training_data$Moscow <- 0
training_data$Moscow[training_data$Region == 97|training_data$Region == 77|training_data$Region==99|training_data$Region==50] <- 1
training_data$Oblast <- 0
training_data$Oblast[training_data$Region==50] <- 1
training_data$Spb <- 0
training_data$Spb[training_data$Region==78] <- 1
training_data$Nnovg <- 0
training_data$Nnovg[training_data$Region==52] <- 1
training_data$Novosib <- 0
training_data$Novosib[training_data$Region==54] <- 1
training_data$Rostov <- 0
training_data$Rostov[training_data$Region==61] <- 1
#training_data$Moscow[training_data$Region != 97&training_data$Region != 77&training_data$Region != 50&training_data$Region==99] <- 0
colnames(training_data)[4]<-'Fines'
colnames(training_data)[3]<-'Repayed'
#training_data$Fines<-scale(training_data$Fines)
training_data$Fines<-log(training_data$Fines)
training_data$НомерАвто<-NULL
training_data$МаркаАвто<-NULL
training_data$isFordFocus<-0
training_data$isFordFocus[training_data$CarType == 3]<-1
#training_data$Region<-scale(training_data$Region)
#training_data$CarType<-scale(training_data$CarType)
write.csv(training_data,"t.csv",row.names =FALSE)

CorMx<-abs(cor(training_data[,-13]))
corrplot(CorMx, method = "color", tl.cex = 0.8)
CorMx

#training_data$Repayed<-factor(training_data$Repayed)
training_data$Repayed[training_data$Repayed==1]<-0
training_data$Repayed[training_data$Repayed==2]<-1

inTrain<-createDataPartition(training_data$Repayed, p=0.75, list=FALSE)
training<-training_data[inTrain,]
validating<-training_data[-inTrain,]

tc <- trainControl(method = "repeatedcv", number = 5, 
                   repeats = 5, verboseIter = FALSE, 
                   returnResamp = "all")

mod1 <- train(as.factor(Repayed) ~ Fines + as.factor(Moscow),method="gbm",trControl = tc,data=training)


mod2 <- train(as.factor(Repayed) ~ Fines + as.factor(Moscow)+as.factor(Novosib)+as.factor(Oblast)+as.factor(Rostov)+as.factor(Nnovg)+as.factor(Spb),method="rf",data=training)

svmGrid <- data.frame(.C = c(.25, .5, 1),.sigma = .01)
mod3 <- train(as.factor(Repayed) ~  Fines + as.factor(Moscow)+as.factor(Novosib)+as.factor(Rostov)+as.factor(Nnovg)+as.factor(Spb),method="svmRadial",tuneGrid = svmGrid,trControl = tc,data=training)

mod4 <- train(as.factor(Repayed) ~  Fines + as.factor(Moscow)+as.factor(Novosib)+as.factor(Rostov)+as.factor(Nnovg)+as.factor(Spb),method="rpart",trControl = tc,data=training)

mod5 <- train(as.factor(Repayed) ~  Fines + as.factor(Moscow),method="glm",family=binomial(logit),trControl = tc,data=training)


C1 <- confusionMatrix(validating$Repayed, factor(predict(mod1, validating)))
print('gbm')
print(C1)
C2 <- confusionMatrix(validating$Repayed, factor(predict(mod2, validating)))
print('rf')
print(C2)
C3 <- confusionMatrix(validating$Repayed, factor(predict(mod3, validating)))
print('svm')
print(C3)
C4 <- confusionMatrix(validating$Repayed, factor(predict(mod4, validating)))
print('rpart')
print(C4)
C5 <- confusionMatrix(validating$Repayed, factor(predict(mod5, validating)))
print('glm(binomial(logit)) ~ logreg')
print(C5)



