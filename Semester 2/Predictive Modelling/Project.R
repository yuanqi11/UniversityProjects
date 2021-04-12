
# Description -------------------------------------------------------------

# Author: Gabriel Berardi
# ID: 2585814B
# Submission: 26 March 2021

# Imports and Loading of the Data -----------------------------------------
library(ggplot2)
library(GGally)
library(car)
library(olsrr)
library(gridExtra)
library(dplyr)
library(magrittr)

data <- read.csv('housing.csv')

# Exploratory Data Analysis -----------------------------------------------

## Get summary of data
summary(data)
    # From the summary, it looks like there are no missing values
    # We have a mixture of numeric and categorical variables
    # There are two faulty observation with impossible values for the 'precip'
    # column of -110 and 0

## Remove faulty observations

data <- data[data$precip > 0,]

## Plot a pairsplot to get an overview of the different variables
ggpairs(data, lower=list(continuous = wrap(linef, method="lm")),
        diag = list(continuous = wrap("barDiag", fill = "#3366ff"))) +
    theme_bw()
    # From the pairsplot we can see that most variables seem to be normally distributed
    # However, for some variables there are some extreme values
    # We can see some clear correlation between the 'price' and other variables
    # Moreover, we also see some correlation between predictors:
    # Strong correlation between 'sqft' and 'bath'
    # Strong correlation between 'dist_am1' and 'dist_am3'

## Plot separate boxplots for numeric variables
par(mfrow=c(1,8))
boxplot(data$elevation, main = 'Elevation')
boxplot(data$dist_am1, main = 'Dist Amenity 1')
boxplot(data$dist_am2, main = 'Dist Amenity 2')
boxplot(data$dist_am3, main = 'Dist Amenity 3')
boxplot(data$bath, main = 'Bathrooms')
boxplot(data$sqft, main = 'Square Feet')
boxplot(data$precip, main = 'Precipation')
boxplot(data$price, main = 'Price')
    # As we can clearly see, the columns 'bath', 'sqft' and 'price' have
    # exactly one extreme outlier, possibly even the same observation

## Finding the outlier
par(mfrow = c(1,1))
plot(lm(price ~ ., data = data), c(4))
outlierTest(lm(price ~ ., data = data))  # Outlier detected at row 348
data[346,]                               # Observation 346 (348-2) is a high leverage observation
                                         # It seems like a office building and
                                         # can therefore be removed
data <- data[-346, ]                     # Remove the high leverage observation
plot(lm(price ~ ., data = data), c(4))   # No more outliers detected

## Check pairsplot after removing the outlier
linef <- function(data, mapping, pts = list(), smt = list(), ...){
    ggplot(data = data, mapping = mapping, ...) +
        do.call(geom_point, pts) +
        do.call(geom_smooth, smt)
}
ggpairs(data, lower=list(continuous = wrap(linef, method="lm")),
        diag = list(continuous = wrap("barDiag", fill = "#3366ff"))) +
    theme_bw()
    # Now we can see that most numerical variables are fairly normally distributed
    # Moreover, in terms of the correlation we notice the following:
        # The correlation between 'price' and 'sqft' essentially disappeared
        # The correlation between 'price' and 'bath is still very strong
        # The correlation between 'bath' and 'sqft' essentially disappeared
        # The correlation between 'dist_am3' and 'dist_am1'/'dist_am2' is strong

# Model/Variable Selection ------------------------------------------------

# Create model with all variables
model <- lm(price ~ ., data = data)

# Get all combinations of variables for the model and values of different criteria
model.selection <- ols_step_all_possible(model)
View(model.selection)
    # Looking at the output of the ols_step_all_possible() function, we can see:
    # Model with predictors 'bath', 'sqft', and 'parking' has the highest adjr
    # Model with predictors 'bath' and 'sqft' has the lowest aic
    # Model with only 'bath' as a predictor performs almost as good as the other two

# Compare result with backward selection using p-value
ols_step_backward_p(model)
    # Using backward selection with p-value, we end up with the model using
    # 'bath', 'sqft' and 'parking'

# Let's look at the ANOVA table for these three models

model.bath <- lm(price ~ bath, data = data)
model.bath.sqft <- lm(price ~ bath + sqft, data = data)
model.bath.sqft.parking <- lm(price ~ bath + sqft + parking, data = data)

anova(model.bath)
anova(model.bath.sqft)
anova(model.bath.sqft.parking)
# As we can see, the 'sqft' and 'parking' variables seem to be irrelevant

# Check for Interactions --------------------------------------------------

ggplot(data, aes(x = sqft, y = price, color = factor(bath), shape = factor(bath))) +
    geom_point() +
    geom_smooth(method = 'lm', fill = NA, fullrange = TRUE)

ggplot(data, aes(x = sqft, y = price, color = parking, shape = parking)) +
    geom_point() +
    geom_smooth(method = 'lm', fill = NA, fullrange = TRUE)

ggplot(data, aes(x = bath, y = price, color = parking, shape = parking)) +
    geom_point() +
    geom_smooth(method = 'lm', fill = NA, fullrange = TRUE)

# Validating Assumptions --------------------------------------------------

# Check: Assumption of Normality

par(mfrow = c(1,1))
plot(model.bath, c(2))
    # As we can see from the Normal-Q-Q plot, the assumption of normally distributed
    # residuals seems to be a bit dubious, since the tails do not seem normal
    # Let's therefore try a log-transformation on our data 
model.bath.log <- lm(log(price) ~ log(bath), data = data)
plot(model.bath.log, c(2))
    # This looks better, now let's see what this means for the quality of this model
summary(model.bath.log)
    # The R squared value is only 0.0217 lower than before, which is acceptable

# Check for outlier after log-transformation
outlierTest(model.bath.log)
plot(model.bath.log, c(4))

# Remove the outlier
data[32,]
data <- data[-32,]

# Re-fit the model and assess normality again
model.bath.log <- lm(log(price) ~ log(bath), data = data)
plot(model.bath.log, c(2))
hist(resid(model.bath.log))
    # The histogram seems reasonably normal
    # However, now that we've transformed the data, we should check whether
    # 'bath' is still the best predictor
    model <- lm(log(price) ~ log(elevation) + log(dist_am1) + log(dist_am2) +
                             log(dist_am3) + log(bath) + log(sqft) + log(precip),
                data = data)

    # Get all combinations of variables for the model and values of different criteria
    model.selection <- ols_step_all_possible(model)
    View(model.selection)
    # As we can see, model.bath.log is still the best model

summary(model.bath.log)

# Check: Assumption of Constant Variance

plot(model.bath.log, c(1))
    # The Variance seems to be a bit different different values of 'bath'

# Check: Assumption of Linearity

plot(model.bath.log, c(1))
    # The assumption of linearity seems to be fulfilled

# Interpreting the Model --------------------------------------------------

# Look at the model again
summary(model.bath.log)
    # We would expect that the log(price) would increase by 0.83 for each
    # unit increase in log(bath)
    # We can explain 84% of the variability in log(price) with log(bath)

# Get confidence intervals for parameters
confint(model.bath.log)
    # It is highly likely that the parameter for log(bath) lies
    # between 0.80 and 0.86
