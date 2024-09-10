# Installing and loading the required packages
library("car")
library("ggcorrplot")
library("MASS")
library("performance")
library("readxl")
library("see")

# Importing data and renaming columns for easier use
rev_data <- read_excel("rev_data.xlsx")
names(rev_data) = gsub(" ", "_", names(rev_data))
head(rev_data)

# Pair plot
rev_data_reduced <- subset(rev_data, select = -No)
pairs(rev_data_reduced)

# Forming the model
rev_model = lm(Y_house_price_of_unit_area ~ X1_transaction_date + X2_house_age + 
            X3_distance_to_the_nearest_MRT_station + X4_number_of_convenience_stores + 
            X5_latitude + X6_longitude, rev_data)
summary(rev_model)

# Checking for potential problems
par(mfrow=c(2,3))
plot(rev_model, 1:6)
  
# Checking for collinearity
rev_model_reduced <- subset(rev_data, select = c(-Y_house_price_of_unit_area, -No))
corr_matrix = round(cor(rev_model_reduced), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)
  
# Checking for multicollinearity
vif(rev_model)

# Stepwise Regression
stepforward <- stepAIC(rev_model, direction="forward")
stepforward$anova 
stepback <- stepAIC(rev_model, direction="back")
stepback$anova 

# Forming the model best selection of predictors
rev_model_best = lm(Y_house_price_of_unit_area ~ X1_transaction_date + X2_house_age + 
                 X3_distance_to_the_nearest_MRT_station + X4_number_of_convenience_stores + 
                 X5_latitude, rev_data)
summary(rev_model_best)

# Checking for potential problems for best selection of predictors
par(mfrow=c(2,3))
plot(rev_model_best, 1:6)

# Checking for collinearity for best selection of predictors
rev_model_best_reduced <- subset(rev_data, select = c(-Y_house_price_of_unit_area, -No, -X6_longitude))
corr_matrix_best = round(cor(rev_model_best_reduced), 2)
ggcorrplot(corr_matrix_best, hc.order = TRUE, type = "lower", lab = TRUE)

# Checking for multicollinearity for best selection of predictors
vif(rev_model_best)