############ Step 1: Set Up Your Environment #######################
if(!require("tidyverse"))install.packages("tidyverse",dependencies = T)
if(!require("caret"))install.packages("caret",dependencies = T)
if(!require("MASS"))install.packages("MASS", dependencies = T)

################ Step 2: Load and Explore the Data #####################
# Loading dataset
heart_data <- read.csv(choose.files())

# To view the first rows of our dataset
head(heart_data)

# To view the Summary of our dataset
summary(heart_data)

# To check if there is any missing values in our dataset
sum(is.na(heart_data))


################ Step 3: Data Cleaning and Preprocessing ####################

# Convert categorical variables to factors
heart_data$Sex <- factor(heart_data$Sex, levels = c(0, 1), labels = c("Female", "Male"))
class(heart_data$Sex)


heart_data$Chest_Pain_Type <- factor(heart_data$Chest_Pain_Type, levels = c(0, 1, 2, 3), 
                             labels = c("Typical Angina", "Atypical Angina", 
                                        "Non-anginal Pain", "Asymptomatic"))
class(heart_data$Chest_Pain_Type)


heart_data$Fasting_Blood_Sugar <- factor(heart_data$Fasting_Blood_Sugar, levels = c(0, 1), 
                                 labels = c("<=120", ">120"))


heart_data$Resting_ECG <- factor(heart_data$Resting_ECG, levels = c(0, 1, 2), 
                         labels = c("Normal", "Abnormality", "Hypertrophy"))

heart_data$Exercise_Induced_Angina <- factor(heart_data$Exercise_Induced_Angina, levels = c(0, 1), 
                                     labels = c("No", "Yes"))

heart_data$Slope <- factor(heart_data$Slope, levels = c(0, 1, 2), 
                   labels = c("Upsloping", "Flat", "Downsloping"))


heart_data$Thalassemia <- factor(heart_data$Thalassemia, levels = c(0, 1, 2), 
                         labels = c("Normal", "Fixed Defect", "Reversible Defect"))

heart_data$Risk_Level <- factor(heart_data$Risk_Level, levels = c(0, 1, 2), 
                        labels = c("Low", "Medium", "High"), ordered = TRUE)


############### Step 4: Exploratory Data Analysis (EDA) ###############

# To check number of rows
ncol(heart_data)

# To check number of columns
nrow(heart_data)

# To check the dimension of dataset
dim(heart_data)

# To check the column names
colnames(heart_data)

############### VIzs for cont var ###########
# Histogram for Age
p1 <- ggplot(heart_data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "magenta", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")
p1

# Histogram for Resting Blood Pressure
p2 <- ggplot(heart_data, aes(x = Resting_BP)) + 
  geom_histogram(binwidth = 10, fill = "#A9FDAC", color = "black") +
  labs(title = "Distribution of Resting Blood Pressure", x = "Resting Blood Pressure", y = "Frequency")
p2

############### VIzs for categorical var ###########

# Bar plot for Sex
p3 <- ggplot(heart_data,aes(x=Sex))+
  geom_bar(aes(fill= heart_data$Sex), color= "black")+
  labs(title = "Gender Distribution", x = "Gender", y = "Count")
p3
# Bar plot of Risk Level by Gender
p4 <- ggplot(heart_data, aes(x = Risk_Level)) +
  geom_bar(aes(fill= heart_data$Sex), color= "black",position = "dodge")+
  labs(title = "Risk Level by Gender", x = "Risk Level", y = "Count", fill = "Gender")
p4
# Bar plot for Chest Pain Type
p5 <- ggplot(heart_data, aes(x = Chest_Pain_Type)) + 
  geom_bar(aes(fill = Chest_Pain_Type) , color = "#FF5964") +
  labs(title = "Chest Pain Type Distribution", x = "Chest Pain Type", y = "Count")
p5

# Boxplot for Age by Risk Level
ggplot(heart_data, aes(x = Risk_Level, y = Age, fill = Risk_Level)) + 
  geom_boxplot() +
  labs(title = "Age Distribution by Risk Level", x = "Risk Level", y = "Age")


#########################################################################

# Set a random seed for reproducibility
set.seed(123)

# Split data in train and test  (80% training, 20% testing)
train_indices <- sample(1:nrow(heart_data), 0.8 * nrow(heart_data)) #0.8 = 80%
train_data <- heart_data[train_indices, ]
test_data <- heart_data[-train_indices, ]

# To check dimensions for training and testing sets
dim(train_data)
dim(test_data)


# To fit ordered logistic regression model on training data
olr_model <- polr(Risk_Level ~ Age + Sex + Chest_Pain_Type + Resting_BP + 
                Cholesterol + Fasting_Blood_Sugar + Resting_ECG + 
                Max_Heart_Rate + Exercise_Induced_Angina + Oldpeak + 
                Slope + Num_Vessels_Fluoroscopy + Thalassemia, 
              data = train_data, Hess = TRUE) # Hess = Hessian matrix (used to calculate standard errors)

#  To check model summary
summary(olr_model)

#Step 8: Calculate Odds Ratios
# Calculate and print odds ratios (exponentiate the coefficients)
exp(coef(olr_model))

#Step 9: Make Predictions on the Test Set

# Make predictions on the test set
predictions <- predict(olr_model, test_data, type = "class")

# Check the predicted vs actual Risk_Level
table(predictions, test_data$Risk_Level)































