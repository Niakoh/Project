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


##Remove feature with near zero variance()
nearzerovariance  = nearZeroVar( pml_training, saveMetrics=TRUE)
nzvcols = which(nearzerovariance$nzv==TRUE)
pml_training.clean = pml_training[,-nzvcols]
pml_training.clean %>% head(5)



###Also we need to remove all the columns with NAs values
NAs = apply(pml_training.clean, 2, function(x) { sum(is.na(x)) })
cols.with.data = names(NAs[NAs == 0])
predictors = grep("(belt|arm|dumbbell)", cols.with.data, value=T)
pml_training.clean = cbind(pml_training.clean[,c(predictors, "classe")])

###Correlations matrix between features itself:
corrplot.mixed(cor(pml_training.clean[,1:52]), lower="circle", upper="color", 
               tl.pos="lt", diag="n", order="hclust", hclust.method="complete")

###After that we can train the model. Splitting the training set in to 70/40:
pmlsplit  = createDataPartition(pml_training.clean$classe, p = 0.7, list = FALSE)
train.final = pml_training.clean[pmlsplit,]
test.final = pml_training.clean[-pmlsplit,]

##Model Fit
random.Forest.fit = train(classe ~ ., data = train.final, method = "rf", trControl = trainControl(method = "cv", number = 4, allowParallel=T))

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

####Projected anmswers
project.answers
 
