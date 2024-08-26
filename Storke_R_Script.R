#Set time zone
Sys.setenv(TZ="UTC")
Sys.setlocale("LC_TIME","English")

#Install packages
#install.packages("lubridate")
#install.packages("ggplot2")
# Install the latest version of 'tibble'
#install.packages("tibble")
# install.packages("dplyr")
#install.packages("smotefamily")
#install.packages("ROSE")
#install.packages("caret")
#install.packages("recipes")
#install.packages("cli")
#install.packages("caTools")
#install.packages("rpart.plot")
#install.packages("recipes")
#install.packages("randomForest")
#install.packages("pROC")
#install.packages("ROCR")
install.packages("gridExtra")



# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(smotefamily)
library(ROSE)
library(caret)
library(pROC)
library(caTools)
library(rpart.plot)
library(randomForest)
library(class)
library(pROC)
library(ROCR)
library(gridExtra)



setwd("C:/Users/user/Desktop/ML Storke Prediction")
# loading data
raw_data <- read.csv('C:/Users/user/Desktop/ML Storke Prediction/healthcare-dataset-stroke-data.csv', sep = ',', encoding = 'UTF-8')
head((raw_data), 3)

stroke_date_prepered <- raw_data

# displaying information about the dataset
str(stroke_date_prepered)

# 2. Replacing "N/A" values in BMI column with median
stroke_date_prepered$bmi <- as.numeric(stroke_date_prepered$bmi)

# checking for missing values
missing_values <- colSums(is.na(stroke_date_prepered))

# transposing the result
t(missing_values)

#-----------------------Data Preparation---------------------

#Replacing N/A values in BMI column with median
median_bmi <- median(stroke_date_prepered$bmi, na.rm = TRUE)
stroke_date_prepered$bmi[is.na(stroke_date_prepered$bmi)] <- median_bmi

# selecting only non-character columns
numeric_data <- stroke_date_prepered[sapply(stroke_date_prepered, function(x) !is.character(x))]

# calculating summary statistics
summary_stats <- summary(numeric_data)
t(summary_stats)

#-------Handling categorical data------  
#----Gender Col----
unique(stroke_date_prepered$gender) # "Male"   "Female" "Other"
table(stroke_date_prepered$gender)
replace_gender <- function(x) {
  if (x == "Other") {
    return("Female")
  } else {
    return(x)
  }
}
#Replace Other gender to Female gender
stroke_date_prepered$gender <- sapply(stroke_date_prepered$gender, replace_gender)
stroke_date_prepered$gender <- as.factor(stroke_date_prepered$gender)

#---------ever_married Col-------------
unique(stroke_date_prepered$ever_married) # "Yes" "No" 
tab <- table(stroke_date_prepered$ever_married)
proportions(tab)
stroke_date_prepered$ever_married <- as.factor(stroke_date_prepered$ever_married)

#---------work_type Col-------------
# Display unique values in the work_type column
unique(stroke_date_prepered$work_type) #  "Private","Self-employed" "Govt_job", "children", "Never_worked" 
# Display the distribution of values in the work_type column
table(stroke_date_prepered$work_type)
# Data is relatively clean, except for 22 observations that never worked.
# Move these observations to the largest group, "Private".

# Function to replace "Never_worked" with "Private"
replace_work_type <- function(x) {
  if (x == "Never_worked") {
    return("Private")
  } else {
    return(x)
  }
}
# Apply the function to the work_type column
stroke_date_prepered$work_type <- sapply(stroke_date_prepered$work_type, replace_work_type)
stroke_date_prepered$work_type <- as.factor(stroke_date_prepered$work_type)


#---------Residence_type Col-------------
# Display unique values in the Residence_type column
unique(stroke_date_prepered$Residence_type) # "Urban" "Rural"
# Display the distribution of values in the work_type column
table(stroke_date_prepered$Residence_type)
stroke_date_prepered$Residence_type <- as.factor(stroke_date_prepered$Residence_type)

#---------smoking_status Col-------------
# Display unique values in the smoking_status column
unique(stroke_date_prepered$smoking_status) # "formerly smoked" "never smoked" "smokes" "Unknown" 
# Display the distribution of values in the smoking_status column
table(stroke_date_prepered$smoking_status)
stroke_date_prepered$smoking_status <- as.factor(stroke_date_prepered$smoking_status)   

# Remove the id column
stroke_date_prepered <- stroke_date_prepered[, !names(stroke_date_prepered) %in% c("id")]

#-------Checking for binary data------  
table(stroke_date_prepered$hypertension)
table(stroke_date_prepered$heart_disease)
table(stroke_date_prepered$stroke)

#Checking numeric data
summary(stroke_date_prepered$age)
summary(stroke_date_prepered$avg_glucose_level)
summary(stroke_date_prepered$bmi)

#Convert stroke col to factor 
stroke_date_prepered$stroke <- as.factor(stroke_date_prepered$stroke)


#----------EDA-----------
#As part of the preparation of the data and data types, we performed part of the data arrangement.
#Now we would like to see more clearly which columns affect in order to avoid a situation of incoming garbage - outgoing garbage.
#and continue with correcting the data in order to refine the algorithm
#----------gender-----------
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=gender,fill=stroke))
#there are different between total male and female so we need to check the proprotion from the gender of stroke
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=gender,fill=stroke),position = 'fill') #gender dont impact stroke

#----------hypertension-----------
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=hypertension,fill=stroke))
#there are different between total hypertension status so we need to check the proprotion 
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=hypertension,fill=stroke),position = 'fill')

#----------ever_married-----------
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=ever_married,fill=stroke))
#there are different between  ever_married status so we need to check the proprotion 
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=ever_married,fill=stroke),position = 'fill')

#----------work_type-----------
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=work_type,fill=stroke))
#there are different between  work_type status so we need to check the proprotion 
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=work_type,fill=stroke),position = 'fill')

#----------Residence_type-----------
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=Residence_type,fill=stroke))
#there are different between Residence_type status so we need to check the proprotion 
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=Residence_type,fill=stroke),position = 'fill')

#----------smoking_status-----------
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=smoking_status,fill=stroke))
#there are different between smoking_status status so we need to check the proprotion 
ggplot(stroke_date_prepered) + geom_bar(mapping = aes(x=smoking_status,fill=stroke),position = 'fill')

baseGluc
#-----------Age-------------
baseAge <- ggplot(stroke_date_prepered,aes(age,fill=stroke))
# baseAge + geom_histogram(binwidth =10)
baseAge + geom_histogram(binwidth =10,position = "fill")

#------------avg_glucose_level-------------
baseGluc <- ggplot(stroke_date_prepered,aes(avg_glucose_level,fill=stroke))
baseGluc + geom_histogram(binwidth =50)
baseGluc + geom_histogram(binwidth =50, position = "fill")


#------------bmi---------
baseBmi <- ggplot(stroke_date_prepered,aes(bmi,fill=stroke))
baseBmi + geom_histogram(binwidth =10)
baseBmi + geom_histogram(binwidth =10, position = "fill")

# Function to manipulate the bmi column
adjust_bmi <- function(df) {
  df$bmi <- ifelse(df$bmi < 25, 25, df$bmi)
  df$bmi <- ifelse(df$bmi > 50, 50, df$bmi)
  return(df)
}

# Apply the function to the DataFrame
stroke_date_prepered <- adjust_bmi(stroke_date_prepered)



#Check for the impact of 2 combined feutures
#--------------gender and ever married---------------
gender_married <- stroke_date_prepered %>% group_by(gender,ever_married)
for_bubble <- gender_married  %>%  summarise(amount=n(),freq=sum(stroke==1)/n())
ggplot(for_bubble,aes(x=gender,y=ever_married,color=freq,size=amount)) + geom_point(alpha =0.7)+scale_size(range=c(1,40))

#--------------gender ans smoaking status---------------
 
gender_smoaking <- stroke_date_prepered %>% group_by(gender,smoking_status)
gender_smoaking_for_bubble <- gender_smoaking  %>%  summarise(amount=n(),freq=sum(stroke==1)/n())
ggplot(gender_smoaking_for_bubble,aes(x=gender,y=smoking_status,color=freq,size=amount)) + geom_point(alpha =0.7)+scale_size(range=c(1,40))

#--------------gender_work_type ---------------
gender_work_type <- stroke_date_prepered %>% group_by(gender,work_type)
gender_work_type_for_bubble <- gender_work_type  %>%  summarise(amount=n(),freq=sum(stroke==1)/n())
ggplot(gender_work_type_for_bubble,aes(x=gender,y=work_type,color=freq,size=amount)) + geom_point(alpha =0.7)+scale_size(range=c(1,40))

#--------------gender_Residence_type   ---------------
gender_Residence_type <- stroke_date_prepered %>% group_by(gender,Residence_type)
gender_Residence_type_for_bubble <- gender_Residence_type  %>%  summarise(amount=n(),freq=sum(stroke==1)/n())
ggplot(gender_Residence_type_for_bubble,aes(x=gender,y=Residence_type,color=freq,size=amount)) + geom_point(alpha =0.7)+scale_size(range=c(1,40))

#--------------smoking_status_Residence_type   ---------------
smoking_status_Residence_type <- stroke_date_prepered %>% group_by(Residence_type,smoking_status)
smoking_status_Residence_type_for_bubble <- smoking_status_Residence_type  %>%  summarise(amount=n(),freq=sum(stroke==1)/n())
ggplot(smoking_status_Residence_type_for_bubble,aes(x=Residence_type,y=smoking_status,color=freq,size=amount)) + geom_point(alpha =0.7)+scale_size(range=c(1,40))

#--------------smoking_status_work_type ---------------
smoking_status_work_type <- stroke_date_prepered %>% group_by(work_type,smoking_status)
smoking_status_work_type_for_bubble <- smoking_status_work_type  %>%  summarise(amount=n(),freq=sum(stroke==1)/n())
ggplot(smoking_status_work_type_for_bubble,aes(x=work_type,y=smoking_status,color=freq,size=amount)) + geom_point(alpha =0.7)+scale_size(range=c(1,40))

#--------------hypertension_heart_disease ---------------
hypertension_heart_disease <- stroke_date_prepered %>% group_by(hypertension,heart_disease)
hypertension_heart_disease_for_bubble <- hypertension_heart_disease  %>%  summarise(amount=n(),freq=sum(stroke==1)/n())
ggplot(hypertension_heart_disease_for_bubble,aes(x=hypertension,y=heart_disease,color=freq,size=amount)) + geom_point(alpha =0.7)+scale_size(range=c(1,40))

#--------------ever_married_heart_disease ---------------
ever_married_heart_disease <- stroke_date_prepered %>% group_by(ever_married,heart_disease)
ever_married_heart_disease_for_bubble <- ever_married_heart_disease  %>%  summarise(amount=n(),freq=sum(stroke==1)/n())
ggplot(ever_married_heart_disease_for_bubble,aes(x=ever_married,y=heart_disease,color=freq,size=amount)) + geom_point(alpha =0.7)+scale_size(range=c(1,40))

str(stroke_date_prepered)

#handle with imbalance data with overSampleing
stroke_date_preperedOver <- ovun.sample(stroke~., data = stroke_date_prepered, method = "over",N=7000)$data

#Before over sampleing
ggplot(stroke_date_prepered, aes(x = factor(stroke))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Before over sampleing",
       x = "Stroke",
       y = "מספר תצפיות") +
  theme_minimal()

#After over smpleing
ggplot(stroke_date_preperedOver, aes(x = factor(stroke))) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = " After over smpleing ",
       x = "Stroke",
       y = "מספר תצפיות") +
  theme_minimal()



# גרף לפני אובר סאמפלינג
p1 <- ggplot(stroke_date_prepered, aes(x = avg_glucose_level, y = age, color = factor(stroke))) +
  geom_point(alpha = 0.6) +
  labs(title = "Original Data",
       subtitle = paste("Original Data contain", nrow(stroke_date_prepered), "number of datapoints,\nand targets distribution as {0:", sum(stroke_date_prepered$stroke == 0), ", 1:", sum(stroke_date_prepered$stroke == 1), "}."),
       color = "Stroke") +
  scale_color_manual(values = c("purple", "red"), labels = c("No stroke", "Stroke")) +
  theme_minimal()

# גרף אחרי אובר סאמפלינג
p2 <- ggplot(stroke_date_preperedOver, aes(x = avg_glucose_level, y = age, color = factor(stroke))) +
  geom_point(alpha = 0.6) +
  labs(title = "Over Sampling",
       subtitle = paste("Over Sampling contains", nrow(stroke_date_preperedOver), "number of datapoints,\nand targets distribution as {0:", sum(stroke_date_preperedOver$stroke == 0), ", 1:", sum(stroke_date_preperedOver$stroke == 1), "}."),
       color = "Stroke") +
  scale_color_manual(values = c("purple", "red"), labels = c("No stroke", "Stroke")) +
  theme_minimal()

# הצגת הגרפים זה לצד זה
grid.arrange(p1, p2, ncol = 2)

#---------------------------models---------------------------------


splitData <- sample.split(stroke_date_preperedOver$stroke, SplitRatio = 0.7)
storkeTrain <- subset(stroke_date_preperedOver, splitData == T)
storkeTest <- subset(stroke_date_preperedOver, splitData == F)

#Checking if the split was good
dim(stroke_date_preperedOver)
dim(storkeTrain)
dim(storkeTest)
#-------model 1 logistic regression with all feutures--------
#ביצוע מודל
#model with all Features
stroke_model <- glm(stroke~.,family = binomial(link = "logit"),data = storkeTrain)
summary(stroke_model)

#test the model
predict_test <- predict(stroke_model,storkeTest, type = "response")
predict_test_prob <- predict(stroke_model, storkeTest, type = "response")
ct <- table(storkeTest$stroke, predict_test > 0.5)

cost_function <- function(cuttoff, cost1,cost2){
  ct1 <- table(storkeTest$stroke, predict_test > cuttoff)
  #עלות של חיזוי לבן אדם שאין לו שבץ ואמרת לו שיש לו + עלות לבן אדם שאמרת לו שאין לו שבץ אבל יש לו
  result <- ct1[1,2]*cost1+ ct1[2,1]*cost2
  return(result)
  
}
cuttoff_vec <- seq(0.1,0.9,length = 9 )
res <- sapply(cuttoff_vec,cost_function,cost1=500,cost2=3000)
plot(res)
min_ct <- table(storkeTest$stroke, predict_test > 0.2)
#מדדים עבור גבול 0.2
# ערכים מהטבלה
TP <- min_ct[2, 2]
TN <- min_ct[1, 1]
FP <- min_ct[2, 1]
FN <- min_ct[1, 2]
# חישוב המדדים
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * (precision * recall) / (precision + recall)

# הצגת התוצאות
cat("Logistic Regression", "\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1, "\n")

# חישוב ROC ו-AUC עבור רגרסיה לוגיסטית
roc_logit <- roc(storkeTest$stroke, predict_test_prob)
auc_logit <- auc(roc_logit)
cat("AUC - Logistic Regression:", auc_logit, "\n")

##-------model 2 Decision tree with all feutures--------
#ביצוע מודל
model_dt <- rpart(stroke~.,storkeTrain)
# הצגת עץ ההחלטות
rpart.plot(model_dt,box.palette = "RdBu",shadow.col = "grey")

# ביצוע תחזיות על קבוצת הבדיקה
predictions <- predict(model_dt, storkeTest, type = "class")
#  - התסברויות - ביצוע תחזיות על קבוצת הבדיקה
predictions_dt_prob <- predict(model_dt, storkeTest, type = "prob")[,2]

# יצירת מטריצת התוצאות (confusion matrix)
conf_matrix <- table(predictions, storkeTest$stroke)
#מדדים
# חישוב המדדים
TP <- conf_matrix[2, 2]
TN <- conf_matrix[1, 1]
FP <- conf_matrix[2, 1]
FN <- conf_matrix[1, 2]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * (precision * recall) / (precision + recall)

# הצגת המטריצה והמדדים
print(conf_matrix)
cat("Decision tree", "\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1, "\n")
# חישוב ROC ו-AUC עבור Decision Tree
roc_dt <- roc(storkeTest$stroke, predictions_dt_prob)
auc_dt <- auc(roc_dt)
cat("AUC - Decision Tree:", auc_dt, "\n")

##-------model 3 Random forest with all feutures--------
#ביצוע מודל
model_rf <- randomForest(stroke~.,storkeTrain,importance = T)
varImpPlot(model_rf)
# ביצוע תחזיות על קבוצת הבדיקה
predictions_rf <- predict(model_rf,storkeTest)
predictions_rf_prob  <- predict(model_rf, newdata =  storkeTest,type = "prob")[,2]

# יצירת מטריצת התוצאות (confusion matrix)
conf_matrix_rf <- table(predictions_rf, storkeTest$stroke)

# חישוב המדדים
TP_rf <- conf_matrix_rf[2, 2]
TN_rf <- conf_matrix_rf[1, 1]
FP_rf <- conf_matrix_rf[2, 1]
FN_rf <- conf_matrix_rf[1, 2]

accuracy_rf <- (TP_rf + TN_rf) / (TP_rf + TN_rf + FP_rf + FN_rf)
precision_rf <- TP_rf / (TP_rf + FP_rf)
recall_rf <- TP_rf / (TP_rf + FN_rf)
f1_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

# הצגת המטריצה והמדדים
print(conf_matrix_rf)
cat("Random forest", "\n")
cat("Accuracy:", accuracy_rf, "\n")
cat("Precision:", precision_rf, "\n")
cat("Recall:", recall_rf, "\n")
cat("F1 Score:", f1_rf, "\n")
# חישוב ROC ו-AUC עבור Random Forest
roc_rf <- roc(storkeTest$stroke, predictions_rf_prob)
auc_rf <- auc(roc_rf)
cat("AUC - Random Forest:", auc_rf, "\n")

##-------model 4 K-neighbors  tree with all feutures--------
# קביעת מספר השכנים (k) - לדוגמה k=5
k <- 5
# הכנת הנתונים - המרת משתנים קטגוריים למשתנים דיגיטליים (one-hot encoding)
train_data <- model.matrix(~ . - stroke - 1, data = storkeTrain)
test_data <- model.matrix(~ . - stroke - 1, data = storkeTest)
# ביצוע תחזיות עם KNN
predictions_knn <- knn(train = train_data,
                       test = test_data,
                       cl = storkeTrain$stroke,
                       k = k)
predictions_knn_prob <- as.numeric(predictions_knn) - 1 # המרת התשובות ל-0 ו-1

# יצירת מטריצת התוצאות (confusion matrix)
conf_matrix_knn <- table(predictions_knn, storkeTest$stroke)
# חישוב המדדים
TP_knn <- conf_matrix_knn["1", "1"]
TN_knn <- conf_matrix_knn["0", "0"]
FP_knn <- conf_matrix_knn["1", "0"]
FN_knn <- conf_matrix_knn["0", "1"]

accuracy_knn <- (TP_knn + TN_knn) / (TP_knn + TN_knn + FP_knn + FN_knn)
precision_knn <- TP_knn / (TP_knn + FP_knn)
recall_knn <- TP_knn / (TP_knn + FN_knn)
f1_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)

# הצגת המטריצה והמדדים
print(conf_matrix_knn)
cat("KNN", "\n")
cat("Accuracy:", accuracy_knn, "\n")
cat("Precision:", precision_knn, "\n")
cat("Recall:", recall_knn, "\n")
cat("F1 Score:", f1_knn, "\n")

# חישוב ROC ו-AUC עבור KNN
roc_knn <- roc(storkeTest$stroke, predictions_knn_prob)
auc_knn <- auc(roc_knn)
cat("AUC - KNN:", auc_knn, "\n")

#----------model 5 ----------ביצוע מודל עם תכונות שנראו חשובות
# יצירת העתק של הנתונים
stroke_data_focused <- stroke_date_preperedOver

# הסרת התכונות Residence_type ו- work_type
stroke_data_focused <- stroke_data_focused[, !colnames(stroke_data_focused) %in% c("Residence_type", "work_type")]
# פיצול הנתונים מחדש לסטי אימון ובדיקה
stroke_data_focused_split <- sample.split(stroke_data_focused$stroke, SplitRatio = 0.7)
storkeTrainFocus <- subset(stroke_data_focused, splitData == T)
storkeTestFocus <- subset(stroke_data_focused, splitData == F)

train_dataFocus <- model.matrix(~ . - stroke - 1, data = storkeTrainFocus)
test_dataFocus <- model.matrix(~ . - stroke - 1, data = storkeTestFocus)
# הגדרת k
k <- 5

# ביצוע תחזיות עם KNN
predictions_knnFocus <- knn(train = train_dataFocus,
                       test = test_dataFocus,
                       cl = storkeTrainFocus$stroke,
                       k = k)
predictions_knn_probFocus <- as.numeric(predictions_knnFocus) - 1 # המרת התשובות ל-0 ו-1

# יצירת מטריצת התוצאות (confusion matrix)
conf_matrix_knnFocus <- table(predictions_knn, storkeTest$stroke)
# חישוב המדדים
TP_knnFocus <- conf_matrix_knnFocus["1", "1"]
TN_knnFocus <- conf_matrix_knnFocus["0", "0"]
FP_knnFocus <- conf_matrix_knnFocus["1", "0"]
FN_knnFocus <- conf_matrix_knnFocus["0", "1"]

accuracy_knnFocus <- (TP_knnFocus + TN_knnFocus) / (TP_knnFocus + TN_knnFocus + FP_knnFocus + FN_knnFocus)
precision_knnFocus <- TP_knnFocus / (TP_knnFocus + FP_knnFocus)
recall_knnFocus <- TP_knnFocus / (TP_knnFocus + FN_knnFocus)
f1_knnFocus <- 2 * (precision_knnFocus * recall_knnFocus) / (precision_knnFocus + recall_knnFocus)

# הצגת המטריצה והמדדים
print(conf_matrix_knnFocus)
cat("Accuracy:", accuracy_knnFocus, "\n")
cat("Precision:", precision_knnFocus, "\n")
cat("Recall:", recall_knnFocus, "\n")
cat("F1 Score:", f1_knnFocus, "\n")

# חישוב ROC ו-AUC עבור KNN
roc_knn <- roc(storkeTest$stroke, predictions_knn_prob)
auc_knn <- auc(roc_knn)
cat("AUC - KNN:", auc_knn, "\n")
#לא שיפר את המודל.

#---------------model 6 אימון מודל Random Forest עם הנתונים המעודכנים
model_rf_focused <- randomForest(stroke ~ ., data = storkeTrainFocus, importance = TRUE)

# ביצוע תחזיות על קבוצת הבדיקה
predictions_rf_focused <- predict(model_rf_focused, storkeTestFocus)
predictions_rf_prob_focused <- predict(model_rf_focused, storkeTestFocus, type = "prob")[, 2]

# חישוב מדדי ביצוע עבור Random Forest
conf_matrix_rf_focused <- table(storkeTestFocus$stroke, predictions_rf_focused)
TP_rf_focused <- conf_matrix_rf_focused[2, 2]
TN_rf_focused <- conf_matrix_rf_focused[1, 1]
FP_rf_focused <- conf_matrix_rf_focused[1, 2]
FN_rf_focused <- conf_matrix_rf_focused[2, 1]

accuracy_rf_focused <- (TP_rf_focused + TN_rf_focused) / sum(conf_matrix_rf_focused)
precision_rf_focused <- TP_rf_focused / (TP_rf_focused + FP_rf_focused)
recall_rf_focused <- TP_rf_focused / (TP_rf_focused + FN_rf_focused)
f1_rf_focused <- 2 * (precision_rf_focused * recall_rf_focused) / (precision_rf_focused + recall_rf_focused)

# הצגת המדדים
cat("Random Forest - Feature-Focused Model\n")
cat("Accuracy:", accuracy_rf_focused, "\n")
cat("Precision:", precision_rf_focused, "\n")
cat("Recall:", recall_rf_focused, "\n")
cat("F1 Score:", f1_rf_focused, "\n")

# הצגת חשיבות המשתנים
varImpPlot(model_rf_focused)

# חישוב עקומת ה-ROC וה-AUC
roc_rf_focused <- roc(storkeTestFocus$stroke, predictions_rf_prob_focused)
auc_rf_focused <- auc(roc_rf_focused)



# הצגת ה-AUC
cat("AUC:", auc_rf_focused, "\n")

#מסקנות ובחירת מודל
# roChart
plot(roc_dt, col = "blue", main = "ROC Curves for All Models")
lines(roc_rf, col = "green")
lines(roc_knn, col = "red")
lines(roc_logit, col = "purple")
lines(roc_rf_focused, col = "orange")

abline(a = 0, b = 1, col = "black", lty = 2)

# הוספת אגדה (legend)
legend("bottomright", legend = c("Decision Tree", "Random Forest", "KNN", "Logistic Regression","Random Forest - Feature-Focused Model"),
       col = c("blue", "green", "red", "purple","orange"), lwd = 2)

# הדפסת ערכי ה-AUC
cat("AUC for Decision Tree:", auc_dt, "\n")
cat("AUC for Random Forest:", auc_rf, "\n")
cat("AUC for K-Nearest Neighbors:", auc_knn, "\n")
cat("AUC for Logistic Regression:", auc_logit, "\n")
cat("AUC for Random Forest - Feature-Focused Model:", auc_rf_focused, "\n")

# פונקציה לחישוב ROI עבור cutoff נתון
roi_function <- function(cutoff, revenue_tp, cost_fp, cost_fn, revenue_tn) {
  predictions <- ifelse(predictions_rf_prob > cutoff, 1, 0)
  conf_matrix <- table(storkeTestFocus$stroke, predictions)
  
  TP <- conf_matrix[2, 2]
  TN <- conf_matrix[1, 1]
  FP <- conf_matrix[1, 2]
  FN <- conf_matrix[2, 1]
  
  # חישוב ROI
  revenue <- TP * revenue_tp + TN * revenue_tn
  cost <- FP * cost_fp + FN * cost_fn
  
  total_roi <- revenue - cost
  return(total_roi)
}

# קביעת פרמטרים
revenue_tp <- 1000
cost_fp <- 200
cost_fn <- 2000
revenue_tn <- 10

# טווח של cutoff
cutoff_vec <- seq(0.1, 0.9, length = 9)

# חישוב ROI עבור כל cutoff
roi_values <- sapply(cutoff_vec, roi_function, revenue_tp = revenue_tp, cost_fp = cost_fp, cost_fn = cost_fn, revenue_tn = revenue_tn)

# מציאת ה-cutoff עם ה-ROI המקסימלי
max_roi <- max(roi_values)
best_cutoff <- cutoff_vec[which.max(roi_values)]

# הצגת התוצאות
plot(cutoff_vec, roi_values, type = "b", col = "blue", xlab = "Cutoff", ylab = "ROI", main = "ROI for Different Cutoffs")
abline(v = best_cutoff, col = "red", lty = 2)
cat("Maximum ROI:", max_roi, "at Cutoff:", best_cutoff, "\n")

