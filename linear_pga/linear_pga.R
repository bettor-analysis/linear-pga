# PGA Tour Score / Strokes Gained Linear Regression Model
# Adam Wickwire 2024

# Pre Model Building Steps -------------------------------------------------------------#

# Load the Required Libraries
# The tidyverse is a collection of R packages designed for data science. It simplifies 
# many common data manipulation, analysis, and visualization tasks. Loading the tidyverse 
# ensures access to a suite of tools that work well together to tackle most data analysis tasks.
library(tidyverse)

# Load the Data
# The read_csv function from the readr package (part of the tidyverse) is used here to import the dataset. 
# read_csv is optimized for speed and automatically handles different data types, making it a preferred choice 
# over base R's read.csv function for large datasets and straightforward data import tasks.
# The dataset contains information on PGA Tour Scores and Strokes Gained, crucial for our analysis.
# Data set is from PGA Tour site. It contains the season average score and strokes gained for each player.
# THe data set contains the last 9 season from 2023 to 2014.
pga_data <- read_csv("score_stroke_data.csv") # Replace with the actual file path

# After loading, it's a good practice to briefly examine the dataset to ensure it has been read correctly.
# This can be done using functions like head(pga_data), summary(pga_data), or glimpse(pga_data), providing
# a quick overview of the data structure, first few rows, or a summary of each column, respectively.
glimpse(pga_data)
# Results:
# Rows: 1,344
# Columns: 3
# $ PLAYER  <chr> "Scottie Scheffler", "Jon Rahm", "Patrick Cantlay", "Viktor Hovland", "Rory McIlroy", "Colli…
# $ SCORE   <dbl> 68.26, 68.82, 68.83, 68.92, 68.92, 69.07, 69.17, 69.20, 69.23, 69.26, 69.…
# $ STGAIND <dbl> 2.314, 1.679, 1.869, 1.648, 2.102, 1.869, 0.949, 1.638, 1.438, 1.026, 1.0…

# Step 1: Exploratory Data Analysis (EDA) ----------------------------------------------#

# Summary Statistics
summary(pga_data)
# Results: 
#    PLAYER              SCORE          STGAIND       
# Length:1344        Min.   :68.06   Min.   :-3.4540  
# Class :character   1st Qu.:70.09   1st Qu.:-0.2755  
# Mode  :character   Median :70.49   Median : 0.1355  
#                    Mean   :70.51   Mean   : 0.1432  
#                    3rd Qu.:70.90   3rd Qu.: 0.5833  
#                    Max.   :74.26   Max.   : 2.5510
#
# # The `summary()` function provides a statistical summary for each variable in the dataset.
# - PLAYER: This is a character variable listing the names of the golf players. 
#           It is not used in the regression model but can be used for labeling or grouping data.
# - SCORE: This numeric variable represents the average scores of the players. 
#          The summary indicates a range from a minimum of 68.06 to a maximum of 74.26, 
#          with a median score slightly higher than the first quartile, which suggests a 
#          left-skewed distribution.
# - STGAIND: This numeric variable signifies the strokes gained by the players. 
#            It ranges from a large negative value (indicating strokes lost) to a 
#            positive maximum, with a median that is slightly above zero, pointing to a 
#            relatively symmetrical distribution around the mean.
# 
# The results inform us about the central tendency and dispersion of the scores and strokes gained.
# It's important to note any potential skewness or outliers in the data as they may 
# affect the model's assumptions and its predictive performance.


# Correlation Analysis
cor(pga_data$STGAIND, pga_data$SCORE)
# Result: -0.8571231
#
# The `cor()` function computes the Pearson correlation coefficient between two variables.
# This coefficient measures the strength and direction of the linear relationship between them.
# - A coefficient close to 1 implies a strong positive linear relationship.
# - A coefficient close to -1 implies a strong negative linear relationship.
# - A coefficient close to 0 implies no linear relationship.
#
# The result is approximately -0.857, indicating a strong negative linear relationship.
# This suggests that as strokes gained increase (becoming less negative or more positive), 
# the scores tend to decrease, which in the context of golf means better performance. 
# This strong correlation is a good indication that 'STGAIND' could be a significant 
# predictor in a linear regression model for 'SCORE'.
#
# The correlation coefficient is crucial for understanding the degree to which 'STGAIND' is related to 'SCORE'
# and supports the decision to investigate this relationship further using linear regression.


# Data Visualization

# Visualization: Scatter Plot of Score vs. Strokes Gained
#
# Create the scatter plot with a regression line
ggplot(pga_data, aes(x = STGAIND, y = SCORE)) +
  geom_point(alpha = 0.6, size = 2) +  # Points with adjusted transparency and size for clarity
  geom_smooth(method = "lm", se = FALSE, col = "blue", size = 1, linetype = "solid") +  # Regression line in blue
  labs(title = "PGA Tour Score vs. Strokes Gained",  # Chart title
       x = "Strokes Gained",  # X-axis label
       y = "Score") +  # Y-axis label
  theme_minimal() +  # Minimal theme for a clean look
  theme(plot.title = element_text(size = 14, face = "bold"),  # Bold title with larger font
        axis.title = element_text(size = 10),  # Axis titles with adjusted size
        axis.text = element_text(size = 10))  # Axis text with adjusted size

# This scatter plot is designed to visually assess the relationship between 'SCORE' and 'STGAIND'.
# - The x-axis represents 'STGAIND' (strokes gained), a key metric in golf that measures a player's performance relative to the field.
# - The y-axis represents 'SCORE', the average score of the players.
#
# The `geom_point()` function adds a layer of points to the plot, with the alpha setting for transparency to mitigate overplotting,
# and size adjusted for better visibility.
# The `geom_smooth()` function adds a regression line to the plot without the confidence interval (se = FALSE),
# in a solid blue color and standard thickness, indicating the trend of the data and providing a visual indication of the linear relationship.#
#
# This plot is crucial for the exploratory data analysis phase, providing a visual cue on the potential predictive power
# of 'STGAIND' on 'SCORE' and helping to validate the choice of linear regression for further analysis.
# The negative slope of the regression line suggests an inverse relationship, as expected from the negative correlation coefficient.


# Visualization: Histogram of PGA Tour Scores
#
# Create a histogram to visualize the distribution of scores
ggplot(pga_data, aes(x = SCORE)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", col = "black") +  # Histogram with specified bin width and color
  labs(title = "Distribution of PGA Tour Scores",  # Chart title
       x = "Score",  # X-axis label
       y = "Frequency") +  # Y-axis label
  theme_minimal() +  # Minimal theme for a clean look
  theme(plot.title = element_text(size = 14, face = "bold"),  # Bold title with larger font
        axis.title = element_text(size = 10),  # Axis titles with adjusted size
        axis.text = element_text(size = 10))  # Axis text with adjusted size

# The histogram provides a graphical representation of the distribution of the SCORE variable.
# - The x-axis represents the range of scores recorded.
# - The y-axis represents the frequency of each score range, giving an indication of the distribution's shape.
# - The `binwidth` is set to 0.2, chosen to balance detail and clarity, and can be adjusted based on the data's variability.
#
# This histogram is key for identifying the distribution characteristics of scores, such as skewness, modality, and the presence of outliers.
# A well-chosen binwidth helps in identifying any patterns or anomalies in the data distribution.


# Visualization: Histogram of Strokes Gained
#
# Create a histogram to visualize the distribution of strokes gained
ggplot(pga_data, aes(x = STGAIND)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", col = "black") +  # Histogram with specified bin width and color
  labs(title = "Distribution of Strokes Gained",  # Chart title
       x = "Strokes Gained",  # X-axis label
       y = "Frequency") +  # Y-axis label
  theme_minimal() +  # Minimal theme for a clean look
  theme(plot.title = element_text(size = 14, face = "bold"),  # Bold title with larger font
        axis.title = element_text(size = 10),  # Axis titles with adjusted size
        axis.text = element_text(size = 10))  # Axis text with adjusted size

# This histogram shows the distribution of the STGAIND variable, which is central to understanding the performance of players.
# - A symmetric distribution around zero would suggest that on average, players neither gain nor lose strokes,
#   while a skew could suggest a tendency toward gaining or losing strokes in the dataset.
#
# Analyzing the distribution of strokes gained is essential for understanding the variability in player performance and
# for further statistical modeling. The histogram can also reveal any potential biases in the data collection process.




# Step 2: Model Building ---------------------------------------------------------------#

# Splitting the Data into Training and Testing Sets
# To evaluate the performance of the linear regression model, the dataset is divided into two parts:
# - Training set: Used to train or fit the linear regression model.
# - Testing set: Used to evaluate the model's predictive performance on unseen data.

# Set a seed for reproducibility to ensure that the random partitioning of the data can be replicated in future runs.
set.seed(123)

# Randomly sample 80% of the data indices for the training set. The choice of 80% for 
# training and 20% for testing is common in machine learning and statistical modeling,
# balancing the need for enough training data with the need to validate the model on an independent dataset.
train_index <- sample(1:nrow(pga_data), 0.8 * nrow(pga_data))

# Create the training dataset based on the sampled indices.
train_data <- pga_data[train_index, ]

# Create the testing dataset from the remaining data not included in the training sample.
test_data <- pga_data[-train_index, ]

# Fitting the Linear Regression Model
# With the data split, a linear regression model is fitted using the training data. 
# This model aims to predict the SCORE variable as a function of STGAIND,
# reflecting the hypothesis that strokes gained can be used to predict a player's score.

# Use the lm() function to fit the linear model, specifying SCORE as the dependent 
# variable and STGAIND as the independent variable.
lm_model <- lm(SCORE ~ STGAIND, data = train_data)

# The resulting lm_model object contains the fitted model, including coefficients 
# that quantify the relationship between strokes gained and score,
# as well as statistics that can be used to assess the model's fit and predictive power.

# This step is critical for building the foundation of our analysis, allowing us to 
# understand the strength and nature of the relationship between
# strokes gained and score, and to predict scores based on new or unseen strokes gained data.




# Step 3: Model Summary ----------------------------------------------------------------#

# Model Summary and Interpretation

# The `summary()` function provides a comprehensive summary of the linear regression 
# model fitted, including details on the model's coefficients, statistical significance, 
# and diagnostic measures.
summary(lm_model)
# Results:
#
# Call:
#   lm(formula = SCORE ~ STGAIND, data = train_data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.29554 -0.23818 -0.02254  0.19146  1.40926 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 70.62784    0.01063 6646.52   <2e-16 ***
#   STGAIND     -0.79668    0.01426  -55.87   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3415 on 1073 degrees of freedom
# Multiple R-squared:  0.7442,	Adjusted R-squared:  0.7439 
# F-statistic:  3121 on 1 and 1073 DF,  p-value: < 2.2e-16
#
# The output includes:
# - The model formula: SCORE ~ STGAIND, indicating SCORE is modeled as a function of STGAIND.
# - Residuals: Summary statistics of the residuals (differences between observed and 
#              predicted values) indicate how well the model fits the data. The closer the 1Q 
#              (first quartile) and 3Q (third quartile) residuals are to zero, and the smaller the 
#              range from Min to Max, the better the model fits.
# - Coefficients: 
# - (Intercept) indicates the expected value of SCORE when STGAIND is zero.
# - STGAIND coefficient shows the expected change in SCORE for a one-unit increase in 
#   STGAIND. A negative coefficient suggests that as STGAIND increases (players perform 
#   better than average), the SCORE decreases (better scores in golf).
# - Statistical significance of coefficients is indicated by p-values (Pr(>|t|)). 
#   Values below 0.05 typically indicate statistical significance, suggesting a strong 
#   relationship between the predictor and the outcome.
# - Residual standard error gives a measure of the typical size of the residuals. 
#   Smaller values indicate a better fit.
# - Degrees of freedom (1073 here) relate to the sample size and the number of predictors used.
# - Multiple R-squared and Adjusted R-squared provide measures of how well the model 
#   explains the variability in the response variable. Values closer to 1 indicate a better fit.
# - F-statistic and its p-value test the overall significance of the model. A very 
#   small p-value (< 2.2e-16) suggests the model is statistically significant.
#
# This summary is crucial for understanding the effectiveness of the model in predicting 
# SCORE based on STGAIND. The significant p-values for the coefficients indicate a strong 
# relationship between the variables. The R-squared values suggest that the model 
# explains a substantial portion of the variability in SCORE, making it a potentially 
# useful tool for prediction and analysis.

# It's important to review these diagnostics carefully to ensure that the model meets 
# the assumptions of linear regression and to interpret the coefficients within the 
# context of the data and the research questions at hand.





# Step 4: Model Prediction -------------------------------------------------------------#

# Making Predictions with the Linear Regression Model
# With the model fitted using the training data, the next step is to assess its 
# predictive performance on unseen data. This is done by making predictions on the test 
# set, which was not used during the model training process.

# The `predict()` function is used to generate predictions:
# - `lm_model` is the linear model object created earlier.
# - `newdata = test_data` specifies that predictions should be made for the observations in the test dataset.

# Generate predictions for the test set
predictions <- predict(lm_model, newdata = test_data)

# The resulting 'predictions' object contains the predicted SCORE values for each 
# observation in the test set based on the STGAIND values. These predictions can now 
# be compared against the actual SCORE values in the test set to evaluate the model's accuracy.
# 
# This step is crucial for understanding how well the model generalizes to new data, 
# which is an essential aspect of model evaluation.
# The accuracy of these predictions will be assessed next, using metrics such as 
# Mean Absolute Error (MAE) and Root Mean Square Error (RMSE).
#
# It's important to note that while high accuracy on the test set is desirable, 
# the ultimate goal is to build a model that generalizes well to new, unseen data,
# reflecting its practical utility in predicting outcomes in real-world scenarios.





# Step 5: Model Evaluation

# Model Evaluation: Assessing Predictive Performance
# After generating predictions for the test set, it's crucial to evaluate the model's accuracy and reliability.
# This is done by comparing the predicted scores against the actual scores from the test data.

# Create a comparison data frame
# This data frame contains two columns: 'Actual' scores from the test data and 'Predicted' scores from the model.
comparison <- data.frame(Actual = test_data$SCORE, Predicted = predictions)

# Mean Absolute Error (MAE)

# MAE measures the average magnitude of the errors in a set of predictions, without considering their direction.
# It's the average over the test sample of the absolute differences between prediction 
# and actual observation where all individual differences have equal weight.
MAE <- mean(abs(comparison$Actual - comparison$Predicted))
print(paste("Mean Absolute Error:", MAE))
# Results: [1] "Mean Absolute Error: 0.28832673679144"
#
# A lower MAE indicates better model performance. The MAE here suggests how much, on 
# average, our predictions deviate from the actual scores.


# Root Mean Square Error (RMSE)

# RMSE is a quadratic scoring rule that measures the average magnitude of the error. 
# It's the square root of the average of squared differences between prediction and actual observation.
RMSE <- sqrt(mean((comparison$Actual - comparison$Predicted)^2))
print(paste("Root Mean Square Error:", RMSE))
# Results: [1] "Root Mean Square Error: 0.361524214205285"
#
# RMSE gives a relatively high weight to large errors, meaning it's useful when large 
# errors are particularly undesirable. A lower RMSE is better.

# R-squared (Coefficient of Determination)

# R-squared measures the proportion of the variance in the dependent variable that is predictable from the independent variable(s).
R_squared <- 1 - sum((comparison$Actual - comparison$Predicted)^2) / sum((comparison$Actual - mean(comparison$Actual))^2)
print(paste("R-squared:", R_squared))
# Results: [1] "R-squared: 0.693830767281267"
#
# An R-squared closer to 1 indicates that a large proportion of the variability in the 
# outcome has been explained by the model.
# Here, the R-squared value suggests how well the independent variable(s) are explaining 
# the variability in the dependent variable.

# These metrics collectively provide a comprehensive view of the model's accuracy and 
# efficacy in predicting new data.
# The MAE and RMSE offer insights into the average error magnitude, while R-squared 
# provides a measure of how well the model's predictions fit the actual data.
# Evaluating these metrics helps in understanding the model's practical utility and in 
# making informed decisions about model selection and improvement.




# Step 6: Model Visualization -----------------------------------------------------------#


# Visualize the Residuals vs Fitted Values
#
# This plot helps in assessing the assumption of linearity and homoscedasticity (equal variance) of residuals.
# - A well-fitted model should show residuals randomly dispersed around the horizontal axis (zero).
# - Patterns or systematic structures in the plot may indicate potential problems with the model fit.
ggplot(lm_model, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)) # Remove legend


# Visualize the Residuals vs Covariates (Strokes Gained)
#
# Plotting residuals against a predictor variable can help identify non-linear 
# relationships not captured by the model.
# - If the residuals display a systematic pattern when plotted against strokes gained, 
#   it might suggest the need for transformation or the addition of higher-order terms.
ggplot(lm_model, aes(x = train_data$STGAIND, y = .resid)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Residuals vs Covariates (Strokes Gained)",
       x = "Strokes Gained",
       y = "Residuals") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)) # Remove legend


# Visualize the Residuals vs Predicted Values
#
# This plot is similar to the Residuals vs Fitted Values plot and serves to check the 
# residuals' distribution against the predicted scores.
# - The absence of patterns or trends suggests that the model's predictions are consistent across all values.
ggplot(lm_model, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Residuals vs Predicted Values",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)) # Remove legend


# Visualize the Q-Q Plot of Standardized Residuals
# 
# A Q-Q (quantile-quantile) plot compares the distribution of standardized residuals to a normal distribution.
# - Points following the line closely indicate that residuals are normally distributed, 
#   a key assumption of linear regression.
# - Deviations from the line suggest departures from normality, which may affect 
#   confidence intervals and hypothesis tests.
ggplot(lm_model, aes(sample = .stdresid)) +
  stat_qq() +
  labs(title = "Q-Q Plot of Standardized Residuals",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)) # Remove legend




# Step 7: Real World Application --------------------------------------------------------#

# The linear regression model can be used to predict a golfer's score based on their 
# strokes gained. This information can be used by golf coaches and players to identify 
# areas for improvement and track progress over time. Additionally, the model can be used 
# to compare the performance of different golfers and identify potential areas for 
# improvement in their game.

# The model can also be used to identify the impact of strokes gained on a golfer's score,
# allowing coaches and players to focus on specific aspects of their game that have the
# greatest impact on their overall performance. This information can be used to develop
# targeted training programs and practice routines to improve specific areas of a golfer's
# game.

# Overall, the linear regression model provides valuable insights into the relationship
# between strokes gained and a golfer's score, and can be used to inform coaching and
# training decisions to improve performance on the golf course.

# Example prediction using a hypothetical strokes gained value
strokes_gained <- 1.24
predicted_score <- predict(lm_model, newdata = data.frame(STGAIND = strokes_gained))
print(paste("Predicted Score for Strokes Gained of", strokes_gained, ":", predicted_score))
# Results: [1] "Predicted Score for Strokes Gained of 1.24 : 69.6399535427069"

# The model predicts that a golfer with a strokes gained value of 1.24 would have a
# predicted score of approximately 69.64. This information can be used to set performance
# targets and track progress over time.
