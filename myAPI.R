library(plumber)
library(tidyverse)
library(dplyr)
library(caret)

# Read-In data
diabetes_data_clean <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv") %>%
  select(-HighChol) %>%  # Remove the HighChol predictor
  mutate_at(vars(Diabetes_binary, HighBP, CholCheck, Smoker, Stroke, HeartDiseaseorAttack, PhysActivity, Fruits, Veggies, HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, DiffWalk, Sex), as.factor) %>%
  filter(BMI < 50) #Removal of outliers from EDA.

# LOGISTIC REGRESSION
# Best Model = Model with all main effects except HighChol
Best_Model_LogReg <- glm(Diabetes_binary ~ ., data = diabetes_data_clean, family = binomial)

# Mean of Non-Factors
mean_GenHlth = sum(diabetes_data_clean$GenHlth, na.rm = TRUE) / sum(!is.na(diabetes_data_clean$GenHlth))
mean_Income = sum(diabetes_data_clean$Income, na.rm = TRUE) / sum(!is.na(diabetes_data_clean$Income))
mean_PhysHlth = sum(diabetes_data_clean$PhysHlth, na.rm = TRUE) / sum(!is.na(diabetes_data_clean$PhysHlth))
mean_MentHlth = sum(diabetes_data_clean$MentHlth, na.rm = TRUE) / sum(!is.na(diabetes_data_clean$MentHlth))
mean_Age = sum(diabetes_data_clean$Age, na.rm = TRUE) / sum(!is.na(diabetes_data_clean$Age))
mean_Education = sum(diabetes_data_clean$Education, na.rm = TRUE) / sum(!is.na(diabetes_data_clean$Education))
mean_BMI = sum(diabetes_data_clean$BMI, na.rm = TRUE) / sum(!is.na(diabetes_data_clean$BMI))

# Function to calculate mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Mode of Factors
mode_HighBP = get_mode(as.numeric(as.character(diabetes_data_clean$HighBP)))
mode_CholCheck = get_mode(as.numeric(as.character(diabetes_data_clean$CholCheck)))
mode_Smoker = get_mode(as.numeric(as.character(diabetes_data_clean$Smoker)))
mode_Stroke = get_mode(as.numeric(as.character(diabetes_data_clean$Stroke)))
mode_HeartDiseaseorAttack = get_mode(as.numeric(as.character(diabetes_data_clean$HeartDiseaseorAttack)))
mode_PhysActivity = get_mode(as.numeric(as.character(diabetes_data_clean$PhysActivity)))
mode_Fruits = get_mode(as.numeric(as.character(diabetes_data_clean$Fruits)))
mode_Veggies = get_mode(as.numeric(as.character(diabetes_data_clean$Veggies)))
mode_HvyAlcoholConsump = get_mode(as.numeric(as.character(diabetes_data_clean$HvyAlcoholConsump)))
mode_AnyHealthcare = get_mode(as.numeric(as.character(diabetes_data_clean$AnyHealthcare)))
mode_NoDocbcCost = get_mode(as.numeric(as.character(diabetes_data_clean$NoDocbcCost)))
mode_DiffWalk = get_mode(as.numeric(as.character(diabetes_data_clean$DiffWalk)))
mode_Sex = get_mode(as.numeric(as.character(diabetes_data_clean$Sex)))
mode_Diabetes_binary = get_mode(as.numeric(as.character(diabetes_data_clean$Diabetes_binary)))


#* Predict diabetes
#* @param HighBP High Blood Pressure (default: mode value; 0 or 1)
#* @param CholCheck Cholesterol Check (default: mode value; 0 or 1)
#* @param BMI Body Mass Index (default: mean value)
#* @param Smoker Smoker (default: mode value; 0 or 1)
#* @param Stroke Stroke (default: mode value; 0 or 1)
#* @param HeartDiseaseorAttack Heart Disease or Attack (default: mode value; 0 or 1)
#* @param PhysActivity Physical Activity (default: mode value; 0 or 1)
#* @param Fruits Fruits (default: mode value; 0 or 1)
#* @param Veggies Vegetables (default: mode value; 0 or 1)
#* @param HvyAlcoholConsump Heavy Alcohol Consumption (default: mode value; 0 or 1)
#* @param AnyHealthcare Any Healthcare (default: mode value; 0 or 1)
#* @param NoDocbcCost No Doctor because of Cost (default: mode value; 0 or 1)
#* @param GenHlth General Health (default: mean value)
#* @param MentHlth Mental Health (default: mean value)
#* @param PhysHlth Physical Health (default: mean value)
#* @param DiffWalk Difficulty Walking (default: mode value; 0 or 1)
#* @param Sex Sex (default: mode value; 0 or 1)
#* @param Age Age (default: mean value)
#* @param Education Education (default: mean value)
#* @param Income Income (default: mean value)
#* @get /pred
pred <- function(
    GenHlth = mean_GenHlth,
    Income = mean_Income,
    PhysHlth = mean_PhysHlth,
    MentHlth = mean_MentHlth,
    Age = mean_Age,
    Education = mean_Education,
    BMI = mean_BMI,
    HighBP = mode_HighBP,  # Use mode for binary predictors
    CholCheck = mode_CholCheck,
    Smoker = mode_Smoker,
    Stroke = mode_Stroke,
    HeartDiseaseorAttack = mode_HeartDiseaseorAttack,
    PhysActivity = mode_PhysActivity,
    Fruits = mode_Fruits,
    Veggies = mode_Veggies,
    HvyAlcoholConsump = mode_HvyAlcoholConsump,
    AnyHealthcare = mode_AnyHealthcare,
    NoDocbcCost = mode_NoDocbcCost,
    DiffWalk = mode_DiffWalk,
    Sex = mode_Sex
) {
  # Create a new data frame for the input values
  new_data <- data.frame(
    GenHlth = as.numeric(GenHlth),  # Ensure these are numeric
    Income = as.numeric(Income),
    PhysHlth = as.numeric(PhysHlth),
    MentHlth = as.numeric(MentHlth),
    Age = as.numeric(Age),
    Education = as.numeric(Education),
    BMI = as.numeric(BMI),
    HighBP = as.factor(HighBP),
    CholCheck = as.factor(CholCheck),
    Smoker = as.factor(Smoker),
    Stroke = as.factor(Stroke),
    HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack),
    PhysActivity = as.factor(PhysActivity),
    Fruits = as.factor(Fruits),
    Veggies = as.factor(Veggies),
    HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
    AnyHealthcare = as.factor(AnyHealthcare),
    NoDocbcCost = as.factor(NoDocbcCost),
    DiffWalk = as.factor(DiffWalk),
    Sex = as.factor(Sex)
  )
  
  # Make predictions using the logistic regression model
  prediction_prob <- predict(Best_Model_LogReg, newdata = new_data, type = "response")
  
  # Determine the predicted class (0 or 1) based on a threshold (e.g., 0.5)
  predicted_class <- ifelse(prediction_prob > 0.5, 1, 0)
  
  # Return the results as a list
  list(
    predicted_probability = prediction_prob,
    predicted_class = predicted_class
  )
}


# Example function calls
#* @plumber
function(pr) {
  pr$handle("GET", "/example1", function() {
    pred()
  })
  pr$handle("GET", "/example2", function() {
    pred(HighBP = 1, BMI = 25)
  })
  pr$handle("GET", "/example3", function() {
    pred(Smoker = 0, Age = 50, Income = 4)
  })
}

# `info` Endpoint
# No input, simply returns first name and last name, and github repo URL. 
#* @get /info
info <- function() {
  list(
    name = "Brock Akerman",
    github_url = "https://github.com/Bakerman1982/ST558_FinalProj"
  )
}
