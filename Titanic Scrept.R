setwd("C:/Users/Hady Ahmed (Main)/Documents/GitHub/Predicting-Survival-on-the-Titanic")
saveing_dir = "C:/Users/Hady Ahmed (Main)/Documents/GitHub/Predicting-Survival-on-the-Titanic/Addetional files/"
test_data <- read.table("Data/test.csv", sep=",", header=TRUE)
train_data <- read.table("Data/train.csv", sep=",", header=TRUE) 
real_test_labes <- read.table("Addetional files/100 supmation.csv", sep=",", header=TRUE)
##############################################################
# Some Data Exploratin and visulization
#############################################################

# Count the number of passengers by survival status
survival_count <- table(train_data$Survived)
# Create a bar plot of passenger survival
barplot(survival_count, main="Passenger Survival",
        xlab="Survived", ylab="Count", col=c("#FF5733", "#5F9EA0"), 
        legend=c("No", "Yes"))

# Create a histogram of passenger age
hist(train_data$Age, main="Passenger Age", xlab="Age", ylab="Count", col="lightblue")

# Create a density plot of passenger fare
plot(density(train_data$Fare), main="Passenger Fare", xlab="Fare", ylab="Density", col="blue")


# Create a scatter plot of passenger age and fare
plot(train_data$Age, train_data$Fare, main="Age vs. Fare",
     xlab="Age", ylab="Fare", col=c("#FF5733", "#5F9EA0")[train_data$Survived+1])
legend("topright", legend=c("No", "Yes"), col=c("#FF5733", "#5F9EA0"), pch=1)


# Create a table of passenger class and survival
class_survival_count <- table(train_data$Pclass, train_data$Survived)

# Create a stacked bar plot of passenger class and survival
barplot(class_survival_count, main="Passenger Class and Survival",
        xlab="Class", ylab="Count", col=c("#FF5733", "#5F9EA0"), legend=c("No", "Yes"), beside=TRUE)

#install.packages("ggplot2")
# install.packages("colorspace")
library(colorspace)
library(ggplot2)
# Bar plot for categorical variables: For variables like "Sex" and "Embarked", you can create a bar plot to see the proportion of survivors for each category. Here's an example code for the "Sex" variable
ggplot(data = train_data, aes(x = Sex, fill = factor(Survived))) + 
  geom_bar(position = "dodge") + 
  scale_fill_manual(values = c("#FF5733", "#5F9EA0"), 
                    name = "Survived", 
                    labels = c("No", "Yes")) + 
  labs(x = "Sex", y = "Count", title = "Survival by Sex")

# Histogram for continuous variables: For variables like "Age" and "Fare", you can create a histogram to see the distribution of survivors and non-survivors. Here's an example code for the "Age" variable:
ggplot(data = train_data, aes(x = Age, fill = factor(Survived))) + 
  geom_histogram(position = "dodge", bins = 30, alpha = 0.5) + 
  scale_fill_manual(values = c("#FF5733", "#5F9EA0"), 
                    name = "Survived", 
                    labels = c("No", "Yes")) + 
  labs(x = "Age", y = "Count", title = "Survival by Age")

# Box plot for continuous variables and categorical variables: For variables like "Pclass" and "Embarked", you can create a box plot to see the distribution of survivors and non-survivors for each category. Here's an example code for the "Pclass" variable
ggplot(data = train_data, aes(x = factor(Pclass), y = Age, fill = factor(Survived))) + 
  geom_boxplot(alpha = 0.5) + 
  scale_fill_manual(values = c("#FF5733", "#5F9EA0"), 
                    name = "Survived", 
                    labels = c("No", "Yes")) + 
  labs(x = "Pclass", y = "Age", title = "Survival by Pclass and Age")

ggplot(data = train_data, aes(x = Fare, fill = factor(Survived))) + 
  geom_histogram(position = "dodge", bins = 30, alpha = 0.5) + 
  scale_fill_manual(values = c("#FF5733", "#5F9EA0"), 
                    name = "Survived", 
                    labels = c("No", "Yes")) + 
  labs(x = "Fare", y = "Count", title = "Survival by Fare")

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
  #print(colSums(is.na(X)))
  
  # Fill the null values in the 'Age' column by the "mean" value
  X$Age[is.na(X$Age)] <- mean(X$Age, na.rm = TRUE)
  X$Fare[is.na(X$Fare)] <- mean(X$Fare, na.rm = TRUE)
  
  #print("After Null Handle")
  #print(colSums(is.na(X)))
  
  return (X)
}
##############################################################
# Apply the preporcessing on the data and split it to train and test
#############################################################
evaluation<- function(model, model_name){
  # apply the same preporccsing in the test data
  test_data_for_predection <- preprocessing(test_data) 
  # Predicting the Data of Passengers
  if (model_name =="logistic regression"||model_name =="Random Forest") {
    train_predictions <- ifelse(predict(model, type = "response", newdata = X_train) > 0.5, 1, 0)
    test_predictions<-ifelse(predict(model, type = "response", newdata = X_test) > 0.5, 1, 0)
    test_prediction <- ifelse(predict(model, type = "response", newdata = test_data_for_predection) > 0.5, 1, 0)
  }else{
    train_predictions <- predict(model, newdata = X_train) 
    test_predictions<-predict(model, newdata = X_test) 
    test_prediction <-predict(model, newdata = test_data_for_predection) 
  }
  
  # Comparing the The model results with the true lables 
  train_accuracy <- mean(train_predictions == X_train$Survived)
  test_accuracy <- mean(test_predictions == X_test$Survived)
  kaggel_test_accurcy <-mean(test_prediction == real_test_labes$Survived)
  
  # Extracting the Data as csv file after prediction
  test_data$Survived <- test_prediction
  submation <- test_data[ , !(names(test_data) %in% c("Pclass","Sex","Age","SibSp",
                                                      "Parch","Fare","take.off",
                                                      "female","male","Name", 
                                                      "Ticket", "Cabin",
                                                      "take-off"))]
  write.csv(submation,paste("Addetional files/Sub_Model_",model_name,".csv"),row.names = FALSE)
  # Printing
  print(paste("Train accuracy for ",model_name,":", round(train_accuracy, 3)))
  print(paste("Test accuracy for ",model_name,":", round(test_accuracy, 3)))
  print(paste("The kaggel Test accuracy is : ",kaggel_test_accurcy))
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
# Evaluate the model
evaluation(model = model_Naive_Bayes, "Naive Bayes")

##############################################################
# Create The logistic regression Model and fit in the data
#############################################################
model_Log_reg <- glm(Survived ~ ., data = X_train, family = binomial)
# Evaluate the model
evaluation(model = model_Log_reg, "logistic regression")
##############################################################
# Creating Random Forest Model
##############################################################
library(randomForest)
model_randomForest <- randomForest(Survived ~ ., data = X_train)
evaluation(model = model_randomForest,"Random Forest")
