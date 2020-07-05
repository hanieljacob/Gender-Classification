library(caret)
library(ggplot2)
library(randomForest)
library(nnet)
library(e1071)
library(rpart)
library(dplyr)

Data<- read.csv("C:/Users/HP/Desktop/Haniel/GenderClassification.csv",stringsAsFactors = TRUE)
set.seed(10)
Data$Favorite.Color <- as.numeric(Data$Favorite.Color)
Data$Favorite.Music.Genre <- as.numeric(Data$Favorite.Music.Genre)
Data$Favorite.Beverage <- as.numeric(Data$Favorite.Beverage)
Data$Favorite.Soft.Drink <- as.numeric(Data$Favorite.Soft.Drink)
TrainingSize <- createDataPartition(Data$Gender, p=0.8, list=FALSE)
TrainingData <- Data[TrainingSize,]
TestingData <- Data[-TrainingSize,]

#using dplyr
Data %>% 
  filter( Gender == "M")

# Using ggplot2
ggplot(Data, aes(Favorite.Color)) +
  geom_bar(fill = "#0073C2FF") 

#using randomForest
model <- randomForest(formula = Gender ~ ., data = Data)
print(model)

#using nnet
model <- nnet(formula = Gender ~ ., data = Data, size = 30)
print(model)

#using e1071
model <- svm(formula = Gender ~ ., data = Data)
print(model)

#using rpart
partition <- rpart(formula = Gender~., data=Data)
plot(partition)


#using caret
model <- train(Gender ~ ., data= TrainingData,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale","center"),
               trControl = trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)
model.cv <- train(Gender ~ ., data= TrainingData,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale","center"),
               trControl = trainControl(method="cv", number=6),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)
print(model)
print(model.cv)

model.train <- predict(model,TrainingData)
model.test <- predict(model,TestingData)
model.cv <- predict(model.cv,TrainingData)

model.train.confusion <- confusionMatrix(model.train, TrainingData$Gender)
model.test.confusion <- confusionMatrix(model.test, TestingData$Gender)
model.cv.confusion <- confusionMatrix(model.cv, TrainingData$Gender)

print(model.train.confusion)
print(model.test.confusion)
print(model.cv.confusion)

importance <- varImp(model)
plot(importance)
plot(importance, col = "red")