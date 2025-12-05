install.packages("ggplot2")
install.packages("car") 
install.packages("mlogit")
install.packages("MASS")
install.packages("pROC")
install.packages("caret")
install.packages("tidyverse")

library(ggplot2)
library(car)
library(mlogit)
library(MASS)
library(pROC)
library(caret)
library(tidyverse)



bar <- ggplot(stroke_prediction, aes(x = factor (Smoking_Status), y = (Diagnosis))) +
  stat_summary(fun = mean, geom = "bar", fill = "pink", colour = "red") +
  labs(x = "Smoking Status", y = "Proportion Diagnosed with Stroke") +
  ggtitle("Stroke by Smoking Status") +
  theme_minimal()
print(bar)
ggsave("graph11.png")


bar <- ggplot(stroke_prediction, aes(x = (Heart_Disease), fill = Diagnosis)) +
  geom_bar(position = "fill") +
  labs(ggtitle = "Heart Disease and Stroke Risk", x = "Heart Disease", y = "Proportion") +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme_minimal()
print(bar)
ggsave("graph22.png")


bar <- ggplot(stroke_prediction, aes(x = Diagnosis)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Stroke vs No Stroke Cases", x = "Diagnosis", y = "Count") +
  theme_minimal()
print(bar)
ggsave("graph33.png")


line <- ggplot(stroke_prediction, aes(x = Age, y = `Average_Glucose_Level`)) +
  geom_line(color = "blue", alpha = 0.5) +
  labs(title = "Average Glucose Level vs Age", x = "Age", y = "Average Glucose Level") +
  theme_minimal()
print(line)
ggsave("graph44.png")


stroke_prediction_Histogram <- ggplot(stroke_prediction, aes(x = Age, fill = Diagnosis)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Age Distribution by Stroke Outcome", x = "Age", y = "Count") +
  theme_minimal()
print(stroke_prediction_Histogram)
ggsave("graph55.png")


stroke_prediction_Boxplot <- ggplot(stroke_prediction, aes(y = `Body_Mass_Index (BMI)`, x = Diagnosis)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Diagnosis", y = "Body Mass Index (BMI)") +
  ggtitle("BMI by Stroke Outcome") +
  theme_minimal()
print(stroke_prediction_Boxplot)
ggsave("graph66.png")


stroke_prediction_Histogram <- ggplot(stroke_prediction, aes(x = `Average_Glucose_Level`, fill = Diagnosis)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Glucose Level Distribution by Stroke Outcome", x = "Average Glucose Level", y = "Count") +
  theme_minimal()
print(stroke_prediction_Histogram)
ggsave("graph77.png") 


stroke_prediction_Histogram <- ggplot(stroke_prediction, aes(x = `Blood_Pressure_Levels (Systolic)`, fill = Diagnosis)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Systolic Blood Pressure by Stroke Outcome", x = "Systolic BP", y = "Count") +
  theme_minimal()
print(stroke_prediction_Histogram)
ggsave("graph88.png") 


scatter <- ggplot (stroke_prediction, aes(Alcohol_Intake,Physical_Activity,colour= Work_Type))
scatter + geom_point() + ggtitle("Alcohol_Intake vs Physical_Activity")
ggsave("graph99.png")

bar <- ggplot(stroke_prediction, aes(Gender, fill = factor(Diagnosis))) + 
  geom_bar(position = "fill") + 
  ggtitle("Proportion of Stroke by Gender") +
  labs(y = "Proportion", x = "Gender", fill = "Diagnosis") +
  theme_minimal()
print(bar)
ggsave("graph100.png")



# CENTRAL TENDENCIES AND DISPERSION MEASURES
# AGE
mean_Age <- mean(stroke_prediction$Age, na.rm = TRUE)
print(mean_Age)

mode_Age <- mode(stroke_prediction$Age)
print(mode_Age)

variance_Age<- var(stroke_prediction$Age)
print(variance_Age)

std_dev_Age <- sd(stroke_prediction$Age)
print(std_dev_Age)

# AVERAGE GLUCOSE LEVEL
mean_Average_Glucose_Level <- mean(stroke_prediction$Average_Glucose_Level, na.rm = TRUE)
print(mean_Average_Glucose_Level)

mode_Average_Glucose_Level <- mode(stroke_prediction$Average_Glucose_Level)
print(mode_Average_Glucose_Level)

variance_Average_Glucose_Level <- var(stroke_prediction$Average_Glucose_Level)
print(variance_Average_Glucose_Level)

std_dev_Average_Glucose_Level <- sd(stroke_prediction$Average_Glucose_Level)
print(std_dev_Average_Glucose_Level)


# STRESS LEVEL
mean_Stress_Levels <- mean(stroke_prediction$Stress_Levels, na.rm = TRUE)
print(mean_Stress_Levels)

mode_Stress_Levels <- mode(stroke_prediction$Stress_Levels)
print(mode_Stress_Levels)

variance_Stress_Levels<- var(stroke_prediction$Stress_Levels)
print(variance_Stress_Levels)

std_dev_Stress_Levels <- sd(stroke_prediction$Stress_Levels)
print(std_dev_Stress_Levels)

# BLOOD PRESSURE LEVELS (SYSTOLIC)
mean_Blood_Pressure_Levels.Systolic. <- mean(stroke_prediction$`Blood_Pressure_Levels (Systolic)`, na.rm = TRUE)
print(mean_Blood_Pressure_Levels.Systolic.)

mode_Blood_Pressure_Levels.Systolic. <- mode(stroke_prediction$`Blood_Pressure_Levels (Systolic)`)
print(mode_Blood_Pressure_Levels.Systolic.)

variance_Blood_Pressure_Levels.Systolic. <- var(stroke_prediction$`Blood_Pressure_Levels (Systolic)`)
print(variance_Blood_Pressure_Levels.Systolic.)

std_dev_Blood_Pressure_Levels.Systolic. <- sd(stroke_prediction$`Blood_Pressure_Levels (Systolic)`)
print(std_dev_Blood_Pressure_Levels.Systolic.)


# BLOOD PRESSURE LEVELS (Diastolic)
mean_Blood_Pressure_Levels.Diastolic. <- mean(stroke_prediction$`Blood_Pressure_Levels (Diastolic )`, na.rm = TRUE)
print(mean_Blood_Pressure_Levels.Diastolic.)

mode_Blood_Pressure_Levels.Diastolic. <- mode(stroke_prediction$`Blood_Pressure_Levels (Diastolic )`)
print(mode_Blood_Pressure_Levels.Diastolic.)

variance_Blood_Pressure_Levels.Diastolic. <- var(stroke_prediction$`Blood_Pressure_Levels (Diastolic )`)
print(variance_Blood_Pressure_Levels.Diastolic.)

std_dev_Blood_Pressure_Levels.Diastolic. <- sd(stroke_prediction$`Blood_Pressure_Levels (Diastolic )`)
print(std_dev_Blood_Pressure_Levels.Diastolic.)


# CHOLESTOROL LEVELS (HDL)
mean_Cholestorol_Levels.HDL. <- mean(stroke_prediction$`Cholestorol_Levels (HDL)`, na.rm = TRUE)
print(mean_Cholestorol_Levels.HDL.)

mode_Cholestorol_Levels.HDL. <- mode(stroke_prediction$`Cholestorol_Levels (HDL)`)
print(mode_Cholestorol_Levels.HDL.)

variance_Cholestorol_Levels.HDL.<- var(stroke_prediction$`Cholestorol_Levels (HDL)`)
print(variance_Cholestorol_Levels.HDL.)

std_dev_Cholestorol_Levels.HDL. <- sd(stroke_prediction$`Cholestorol_Levels (HDL)`)
print(std_dev_Cholestorol_Levels.HDL.)


# CHOLESTOROL LEVELS (LDL)
mean_Cholestorol_Levels.LDL. <- mean(stroke_prediction$`Cholestorol_Levels (LDL)`, na.rm = TRUE)
print(mean_Cholestorol_Levels.LDL.)

mode_Cholestorol_Levels.LDL. <- mode(stroke_prediction$`Cholestorol_Levels (LDL)`)
print(mode_Cholestorol_Levels.LDL.)

variance_Cholestorol_Levels.LDL.<- var(stroke_prediction$`Cholestorol_Levels (LDL)`)
print(variance_Cholestorol_Levels.LDL.)

std_dev_Cholestorol_Levels.LDL. <- sd(stroke_prediction$`Cholestorol_Levels (LDL)`)
print(std_dev_Cholestorol_Levels.LDL.)


# Convert Columns to factor
stroke_prediction$Stroke_History <- as.factor(stroke_prediction$Stroke_History)
stroke_prediction$Family_History_of_Stroke <- as.factor(stroke_prediction$Family_History_of_Stroke)
stroke_prediction$Gender <- ifelse(stroke_prediction$Gender == "Male", 0, 1)
stroke_prediction$Marital_Status <- as.factor(stroke_prediction$Marital_Status)
stroke_prediction$Work_Type <- as.factor(stroke_prediction$Work_Type)
stroke_prediction$Residence_Type <- ifelse(stroke_prediction$Residence_Type == "Rural", 0, 1)
stroke_prediction$Smoking_Status <- as.factor(stroke_prediction$Smoking_Status)
stroke_prediction$Alcohol_Intake <- as.factor(stroke_prediction$Alcohol_Intake)
stroke_prediction$Physical_Activity <- as.factor(stroke_prediction$Physical_Activity)
stroke_prediction$Diagnosis <- as.factor(stroke_prediction$Diagnosis)
# Check the updated structure of the data
str(stroke_prediction)

# MODEL MAKING
fullModel <- glm(Diagnosis ~ Age + Gender + Hypertension + Heart_Disease +
                   Marital_Status + Work_Type + Residence_Type + Average_Glucose_Level + 
                   `Body_Mass_Index (BMI)` + Smoking_Status + Alcohol_Intake + Physical_Activity + 
                   Stroke_History + Family_History_of_Stroke + Stress_Levels +
                   `Blood_Pressure_Levels (Systolic)` + `Blood_Pressure_Levels (Diastolic )` +
                   `Cholesterol_Levels (HDL)` + `Cholesterol_Levels (LDL)`,
                 data = stroke_prediction, 
                 family = binomial())
summary(fullModel)


step(fullModel, direction = "both")


### MODIFIED MODEL
Model <- glm(formula = Diagnosis ~ Hypertension + Age + Average_Glucose_Level + 
      Stress_Levels + `Cholesterol_Levels (HDL)`, family = binomial(), 
    data = stroke_prediction)

summary(Model)

# Predict on test set
test_stroke_prediction$Predicted_Probability <- predict(Model, data = test_stroke_prediction, type = "response")
test_stroke_prediction$Predicted_Class <- ifelse(test_stroke_prediction$Predicted_Probability > 0.5, "Stroke", "No Stroke")

# View predicted classes
test_stroke_prediction[c("Patient ID", "Predicted_Probability", "Predicted_Class")]

Hypertension <- 0
Age <- 60
Stroke_History <- 0
Average_Glucose_Level <- 154.32
Cholesterol_Level.HDL. <- 78
Diagnosis = 0.1176097 -0.0539509 * Hypertension + -0.0001465 * Age +0.0007851 *Average_Glucose_Level -0.0088649 * Cholesterol_Level.HDL. -0.0029306
print(Diagnosis)


actual_labels <- stroke_prediction$Diagnosis
stroke_prediction$Diagnosis <- as.factor(stroke_prediction$Diagnosis)
levels(stroke_prediction$Diagnosis) # Should be "Stroke" and "No Stroke"

conf_matrix <- confusionMatrix(factor(predicted_class), factor(actual_labels)) ########
print(conf_matrix)

# Print full confusion matrix
print(conf_matrix$table)

##################################
# Extract TP, TN, FP, FN
TN <- conf_matrix$table[1,1]  # True Negatives
FP <- conf_matrix$table[2,1]  # False Positives
FN <- conf_matrix$table[1,2]  # False Negatives
TP <- conf_matrix$table[2,2]  # True Positives
#### INDICATORS EXPlain later
senstivity <- TP / (TP + FN)
print(senstivity)
specificity <- TN / (TN + FP)
print(specificity)
accuracy <- (TP + TN) / (TP + TN + FP + FN)
print(accuracy)
precision <- TP / (TP + FP)
print(precision)
f1_score <- 2 * (precision * senstivity) / (precision + senstivity)
print(f1_score)
roc <- ( 0.4827078 + 0.5551862)/2

## thresholds
conf_matrix_0_25 <- get_conf_matrix(predicted_prob, actual_labels, 0.25)
print(conf_matrix_0_25)

conf_matrix_0_5 <- get_conf_matrix(predicted_prob, actual_labels, 0.5)
print(conf_matrix_0_5)


conf_matrix_0_75 <- get_conf_matrix(predicted_prob, actual_labels, 0.75)
print(conf_matrix_0_75)


#chi square
modelChi <- Model$null.deviance - Model$deviance 
modelChi

chidf <- Model$df.null - Model$df.residual
chidf

chisq.prob <-  1 - pchisq(modelChi, chidf)
chisq.prob

R2.mcf <- 1- (Model$deviance /Model$null.deviance)
R2.mcf

plot(Model)
lm.beta(Model)
confint(Model)
resid(Model)
# Calculate AIC
aic_value <- AIC(Model)
print(aic_value)
vif(Model)
cooks.distance(Model)

actual_binary <- ifelse(actual_labels == "Stroke", 1, 0)
roc_obj <- roc(actual_binary, predicted_prob)
plot(roc_obj, main = "ROC Curve - Stroke Prediction Model",col = "green", lwd = 2)
grid()
auc_value <- auc(roc_obj)
print(auc_value)

