setwd("F:/University/4th year/2ed Semester/Distributed Computing/Assg/Lab Assignemnt/Titanic Disaster")
test_data <- read.table("test.csv", sep=",", header=TRUE)
train_data <- read.table("train.csv", sep=",", header=TRUE) 
real_test_labes <- read.table("submission.csv", sep=",", header=TRUE)
##############################################################
# Some Data Exploratin and visulization
#############################################################

# Count the number of passengers by survival status
survival_count <- table(train_data$Survived)
# Create a bar plot of passenger survival
barplot(survival_count, main="Passenger Survival",
        xlab="Survived", ylab="Count", col=c("red", "green"), 
        legend=c("No", "Yes"))


# Create a histogram of passenger age
hist(train_data$Age, main="Passenger Age", xlab="Age", ylab="Count", col="lightblue")

# Create a density plot of passenger fare
plot(density(train_data$Fare), main="Passenger Fare", xlab="Fare", ylab="Density", col="blue")


# Create a scatter plot of passenger age and fare
plot(train_data$Age, train_data$Fare, main="Age vs. Fare",
     xlab="Age", ylab="Fare", col=c("red", "green")[train_data$Survived+1])
legend("topright", legend=c("No", "Yes"), col=c("red", "green"), pch=1)


# Create a table of passenger class and survival
class_survival_count <- table(train_data$Pclass, train_data$Survived)

# Create a stacked bar plot of passenger class and survival
barplot(class_survival_count, main="Passenger Class and Survival",
        xlab="Class", ylab="Count", col=c("red", "green"), legend=c("No", "Yes"), beside=TRUE)



##############################################################
# preProcssing based on the data Exploration and visualizatin
#############################################################
preprocessing <- function(data) {
  
  is_train_data <- FALSE
  df <- data
  
  # Drop the some columns with very unique values like:
  # - Name
  # - PassengerId
  # - Ticket
  # Other columns with categorical values and a lot of missing data that cannot be resolved like:
  # - Cabin
  # - take-off
  df <- df[ , !(names(df) %in% c("PassengerId", "Name", "Ticket", "Cabin", "take-off"))]
  
  X <- df
  y <- df
  
  
  # Applying one-hot encoding in the Sex column
  dummies <- model.matrix(~ Sex + 0, data = X)
  colnames(dummies) <- gsub("Sex", "", colnames(dummies))
  X <- cbind(X, dummies)
  
  # Drop the categorical data column for the full data
  X <- X[ , !(names(X) %in% c("Sex"))]
  
  # Look for the null values in the data
  print(colSums(is.na(X)))
  
  # Fill the null values in the 'Age' column by the "mean" value
  X$Age[is.na(X$Age)] <- mean(X$Age, na.rm = TRUE)
  X$Fare[is.na(X$Fare)] <- mean(X$Fare, na.rm = TRUE)
  
  print("After Null Handle")
  print(colSums(is.na(X)))
  
  return (X)
}

##############################################################
# Apply the preporcessing on the data and split it to train and test
#############################################################
X <- preprocessing(train_data)
set.seed(728)

train_index <- sample(nrow(X), floor(0.7 * nrow(X)), replace = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]

##############################################################
# Create The Naive Bayes Model and fit in the data
#############################################################
# Creating Naive Bayes Model
# install.packages("e1071")
library(e1071)
model_Naive_Bayes <- naiveBayes(Survived ~ ., data = X_train)

# Evaluate the model on the train set
train_predictions <- predict(model_Naive_Bayes, newdata = X_train) 
train_accuracy <- mean(train_predictions == X_train$Survived)
print(paste("Train accuracy for NaiveBayes:", round(train_accuracy, 3)))

test_predictions <- predict(model_Naive_Bayes, newdata = X_test)
test_accuracy <- mean(test_predictions == X_test$Survived)
print(paste("Test accuracy for NaiveBayes :", round(test_accuracy, 3)))

##############################################################
# Evaluating The Naive Bayes Model
#############################################################
test_data_for_predection <- preprocessing(test_data)
# Predicting the Data of Passengers
test_prediction <- predict(model_Naive_Bayes, test_data_for_predection)
# Extracting the Data as csv file after prediction
test_data$Survived <- test_prediction
submation <- test_data[ , !(names(test_data) %in% c("Pclass","Sex","Age","SibSp",
                                                    "Parch","Fare","take.off",
                                                    "female","male","Name", 
                                                    "Ticket", "Cabin",
                                                    "take-off"))]
write.csv(submation,"submation.csv",row.names = FALSE) # get 81.7% in kaggel compitaion

##############################################################
# Create The logistic regression Model and fit in the data
#############################################################
model_Log_reg <- glm(Survived ~ ., data = X_train, family = binomial)

# Evaluate the model on the train set
train_predictions <- ifelse(predict(model_Log_reg, type = "response", newdata = X_train) > 0.5, 1, 0)
train_accuracy <- mean(train_predictions == X_train$Survived)
print(paste("Train accuracy logistic regression:", round(train_accuracy, 3)))

# Evaluate the model on the test set
test_predictions <- ifelse(predict(model_Log_reg, type = "response", newdata = X_test) > 0.5, 1, 0)
test_accuracy <- mean(test_predictions == X_test$Survived)
print(paste("Test accuracy logistic regression:", round(test_accuracy, 3)))

##############################################################
# Evaluating The logistic regression Model
#############################################################
test_data_for_predection <- preprocessing(test_data)
# Predicting the Data of Passengers
test_prediction <-ifelse(predict(model_Log_reg, type = "response", newdata = test_data_for_predection) > 0.9, 1, 0)
# Extracting the Data as csv file after prediction
#res <- mean(test_prediction == real_test_labes$Survived)
#res
test_data$Survived <- test_prediction
submation <- test_data[ , !(names(test_data) %in% c("Pclass","Sex","Age","SibSp",
                                                    "Parch","Fare","take.off",
                                                    "female","male","Name", 
                                                    "Ticket", "Cabin",
                                                    "take-off"))]
write.csv(submation,"submation.csv",row.names = FALSE) # get 70.3% in kaggel compitaion
