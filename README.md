# Predicting Survival on the Titanic

This project aims to build a predictive model that answers the question: "what sorts of people were more likely to survive?" using passenger data, such as age, gender, and ticket class.

## Data Preprocessing

The following preprocessing steps were performed on the dataset:

- Dropped unnecessary columns like "PassengerId", "Name", "Ticket", "Cabin", and "take-off".
- Applied one-hot encoding for the "Sex" column.
- Changed some columns into factors like "tack. off" and "Pclass".
- Filled the null values in both the "Age" and "Fear" columns with the mean.

## Models Used

The following machine learning models were used to predict the survival of passengers:

- Random Forest Model
- Logistic Regression
- Naive Bayes Model

## Results

The Random Forest Model achieved the highest accuracy with a score of 78%, followed by Logistic Regression with a score of 76%, and Naive Bayes Model with a score of 74%.

## Usage

To run the project, you can follow these steps:

1. Clone the repository.
2. Install the required packages (listed in the `requirements.txt` file).
3. Run the scripts in the `scripts/` folder to preprocess the data and build the models.
4. Check the `results/` folder for the performance of the models and the `models/` folder for the saved models.


## Acknowledgments

- [Kaggle Titanic Competition](https://www.kaggle.com/c/titanic)
