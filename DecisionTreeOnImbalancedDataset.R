#libraries
library(plyr)
library(corrplot)
library(ggplot2)
library(ROSE)
library(C50)
library(rpart)
library(caret)



#given train_dataset for analysis
df = read.csv('churn_train_data.csv')
str(df)
summary(df)
df$phone.number <- NULL

#given test_dataset for analysis
df_t = read.csv('churn_test_data.csv')
df_t$phone.number <- NULL




#EXPLORATORY DATA ANALYSIS#
#missing value analysis
sapply(df,function(x)sum(is.na(x)))
write(capture.output(sapply(df,function(x)sum(is.na(x)))), "missing_values.txt" )



#Target Class Distribution
barplot(prop.table(table(df$Churn)),
        col = rainbow(2),
        ylim = c(0,1),
        main = 'Target Class Distribution')


#Train and test splits on the data
set.seed(121)
samples <- sample(2,nrow(df),replace = T, prob = c(0.7,0.3))
df_train <- df[samples == 1,]
df_test <- df[samples == 2,]
prop.table(table(df_train$Churn))


#Predictive Model using Decision Tree
dt_model = C5.0(Churn ~ ., data = df,rules = TRUE)
summary(dt_model)


#Predictions with the Training Data
pred_test = predict(dt_model, df_t, type = "class")

#Evaluating Model Performance using Confusion Matrix
cnf = table(pred_test, df_t$Churn)
write(capture.output(confusionMatrix(cnf)),'Model on Imbalanced Dataset')



# The model accuracy is turned out to be = 0.946
# Accuracy  = TP + TN / TP + TN + FP + FN
#           = 1428+149 / 1428+149+75+15 = 94.6%

# Sensitivity = TP / TP + FN  
#             = 1428/1428+15 =  98.96%

# Specificity = TN / TN + FP
#             = 149 / 75 + 149 = 66.52%

