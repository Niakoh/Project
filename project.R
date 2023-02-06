library(readr)
library(caret)
library(randomForest)
library(e1071)
library(ggplot2)
library(dplyr)
library(lattice)
library(corrplot)


##Import data
pml_training <- read_csv("C:/Users/niako/Downloads/pml-training.csv")
pml_training <- read.csv("C:/Users/niako/Downloads/pml-training.csv", sep = ",", header = TRUE, na.strings = c("", "NA", "#DIV/0!"))
pml_testing <- read.csv("C:/Users/niako/Downloads/pml-testing.csv",sep = ",", header = TRUE, na.strings = c("", "NA", "#DIV/0!"))
View(pml_testing)  
pml_training%>% head(5) 


##Remove feature with near zero variance()
near.zero.variance  = nearZeroVar( pml_training, saveMetrics=TRUE)
nzv.cols = which(near.zero.variance$nzv==TRUE)
pml_training.clean = pml_training[,-nzv.cols]
pml_training.clean %>% head(5)



###Also we need to remove all the columns with NAs values
NAs = apply(pml_training.clean, 2, function(x) { sum(is.na(x)) })
cols.with.data = names(NAs[NAs == 0])
predictors = grep("(belt|arm|dumbbell)", cols.with.data, value=T)
pml_training.clean = cbind(pml_training.clean[,c(predictors, "classe")])


###Features(predictors) relationships with classes using featurePlot from caret:
featurePlot(pml_training.clean[,1:52], pml_training.clean$classe, "strip").clean[,1:52], pml_training.clean$classe, "strip")

###Correlations matrix between features itself:

corrplot.mixed(cor(pml_training.clean[,1:52]), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")

###After that we can train the model. Splitting the training set in to 70/40:
inTrain  = createDataPartition(pml_training.clean$classe, p = 0.7, list = FALSE)
train.final = pml_training.clean[inTrain,]
test.final = pml_training.clean[-inTrain,]


##Model Fit
trc = trainControl(method = "cv", number = 4, allowParallel=T)
random.Forest.fit = train(classe ~ ., data = train.final, method = "rf", trControl = trc)



###Model Validation and Confusion Matrix
random.Forest.prediction = predict(random.Forest.fit, test.final)
test.final$classe=as.factor(test.final$classe)
prediction.results = confusionMatrix(random.Forest.prediction, test.final$classe)
prediction.results



###Prediction of the answers
pml_testing.clean = pml_testing[, names(train.final)[-53]]
project.answers = predict(random.Forest.fit, pml_testing.clean)
pml_testing.clean$classe <- project.answers
pml_testing.clean$classe %>% head(5)




###final output submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
final=pml_write_files(project.answers)
final
