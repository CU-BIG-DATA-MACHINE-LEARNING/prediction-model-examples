install.packages("caret")
install.packages("Rcpp")
install.packages("glue")
install.packages("rlang")
install.packages("kernlab")
library(rlang)

# Importing libraries
library(datasets) # Contains the Iris data set
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression Training

# Importing the Iris data set
data(iris)
View(iris)
# Check to see if there are missing data?
sum(is.na(iris))

# To achieve reproducible model; set the random seed number
set.seed(100)

# Performs stratified random split of the data set  - have a dataset to use to train the model
TrainingIndex <- caret::createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

# Compare scatter plot of the 80 and 20 data subsets




###############################
# SVM model (polynomial kernel) - support vector machine model

# Build Training model - uses training set to build the model (80% of dataset) - this will help us predict class label of testing set
#train (class label,...)
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),  #scales data by computing mean value
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model - cross validation model - separates and analyzes data
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)


# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set - apply the training model to predict the class of the testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)   #shows predictions based off of model
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model) #shows which characteristics were most important for the learning/training model
plot(Importance)
plot(Importance, col = "red")