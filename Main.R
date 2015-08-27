
rm(list = ls())

library(caret);  library(readr); 
dataset <- read_csv("covtype.data", col_names = TRUE)


inTrain <- createDataPartition(y = dataset[,55], p = .75,list = FALSE);
training <- dataset[ inTrain,];
testing <- dataset[-inTrain,];

labels_training <- as.factor(training[,55])
training <- training[,-55]

labels_testing <- as.factor(testing[,55])
testing  <- testing[,-55]

#Muestrea 10000 instancias del entrenamiento
indices <- sample( 1:nrow( training ), 10000 )
training2 <- training[ indices, ]
labels_training2 <- labels_training[ indices ]


#train
#Usa sólo las 10 primeras variables
svmLinearmodel <- train(x=training2[,1:10],y=labels_training2, method = "svmLinear", tuneLength = 2);
#test local
classesPredictionSVMLin <- predict(svmLinearmodel, newdata = testing[,1:10])
confusionMatrix(data = classesPredictionSVMLin, labels_testing)



#ctrl <- trainControl(method = "repeatedcv", repeats = 2);
#svmLinearmodel <- train(x=training2[,1:10],y=labels_training2, method = "svmLinear", tuneLength = 1, trControl = ctrl);


