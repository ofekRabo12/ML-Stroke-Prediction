Stroke Prediction Project
Project Overview
This project aims to predict whether an individual is likely to suffer a stroke based on several health-related parameters. The solution involves cleaning the provided dataset and applying classification techniques to generate predictions. The dataset includes a variety of features such as demographic information and health-related metrics.

Dataset
The dataset consists of several features that are used to predict stroke risk. Key columns include:

gender: Gender of the individual
age: Age of the individual
hypertension: Whether the individual has hypertension (0: No, 1: Yes)
heart_disease: Whether the individual has heart disease (0: No, 1: Yes)
ever_married: Marital status of the individual
work_type: Type of employment
Residence_type: Whether the individual lives in an urban or rural area
avg_glucose_level: Average glucose level in blood
bmi: Body Mass Index (BMI)
smoking_status: Smoking habits of the individual
stroke: Target variable indicating whether the individual has had a stroke (1: Yes, 0: No)
Project Workflow
Data Cleaning:

Handling missing or erroneous data, such as missing BMI values.
Encoding categorical variables to ensure compatibility with machine learning algorithms.
Normalizing numerical features to optimize model performance.
Feature Engineering:

Selecting the most relevant features for predicting stroke.
Creating new features or transforming existing ones for better accuracy.
Classification Model:

The project uses classification algorithms (e.g., Logistic Regression, Decision Trees, Random Forest) to predict whether a person will experience a stroke.
The models are evaluated based on performance metrics such as accuracy, precision, recall, and F1-score.
Model Selection & Tuning:

Hyperparameter tuning is performed to optimize the model's performance.
The final model is selected based on cross-validation scores and its ability to generalize on unseen data.

Contact
For any questions, feel free to reach out to ofek.rabotnicoff@gmail.om