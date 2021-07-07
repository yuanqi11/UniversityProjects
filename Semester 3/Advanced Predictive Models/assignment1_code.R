
# Imports -----------------------------------------------------------------

library(ggplot2)
library(ggfortify)
library(GGally)
library(tidyr)
library(magrittr)
library(dplyr)
library(reshape2)
library(wesanderson)
library(olsrr)
library(data.table)
library(car)
library(MASS)
library(gridExtra)

# Load Data ---------------------------------------------------------------

oldat <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"))

summary(oldat) # NAs seem to be encoded as #N/A
str(oldat)

# Reload the data specifying how NAs are encoded

oldat <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"),
                      na.strings = "#N/A")

# Turn categorical variables into factors 
oldat$country <- as.factor(oldat$country)
oldat$country.code <- as.factor(oldat$country.code)
oldat$soviet <- as.factor(oldat$soviet)
oldat$comm <- as.factor(oldat$comm)
oldat$muslim <- as.factor(oldat$muslim)
oldat$oneparty <- as.factor(oldat$oneparty)
oldat$host <- as.factor(oldat$host)

# Validity Check ----------------------------------------------------------

# Check which columns contain NAs
colSums(is.na(oldat))  # gdp00: 1 missing value, gdp16: 2 missing value

# Check if any values seem odd and validate those (e.g. negative altitude)
summary(oldat)

# Check if totgoldYY/totmedalsYY is the sum of all medals won in that year

sum(oldat$gold00) == oldat$totgold00[1] # TRUE
sum(oldat$gold04) == oldat$totgold04[1] # TRUE
sum(oldat$gold08) == oldat$totgold08[1] # TRUE
sum(oldat$gold12) == oldat$totgold12[1] # TRUE
sum(oldat$gold16) == oldat$totgold16[1] # FALSE

sum(oldat$tot00) == oldat$totmedals00[1] # TRUE
sum(oldat$tot04) == oldat$totmedals04[1] # TRUE
sum(oldat$tot08) == oldat$totmedals08[1] # TRUE
sum(oldat$tot12) == oldat$totmedals12[1] # TRUE
sum(oldat$tot16) == oldat$totmedals16[1] # FALSE

# Fix Data Problems -------------------------------------------------------

# For the three NAs, I decided to find the value for the missing GDP data

# For Afghanistan's GDP in 2016 there is available data from https://countryeconomy.com/gdp/
oldat$gdp00[1] <- 3532
oldat$gdp00 <- as.integer(oldat$gdp00)

# For Syria's GDP in 2016 there is available data from https://countryeconomy.com/gdp/
oldat$gdp16[92] <- 12377

# For Cuba's GDP in 2016 there is available data from https://data.worldbank.org
oldat$gdp16[21] <- 91370
oldat$gdp16 <- as.integer(oldat$gdp16)

# For the wrong sums of gold and total medals in 2016, I will replace the
# original data with the sum of gold16 and tot16
oldat$totgold16 <- sum(oldat$gold16)
oldat$totmedals16 <- sum(oldat$tot16)

# Arrange Data ------------------------------------------------------------

# Turn wide data into long data format
oldat <- melt(setDT(oldat), measure.vars=list(c(3,4,5,6,7),
                                              c(8,9,10,11,12),
                                              c(17,18,19,20,21),
                                              c(22,23,24,25,26),
                                              c(27,28,29,30,31),
                                              c(32,33,34,35,36),
                                              c(38,39,40,41,42)), 
              variable.name='year', value.name=c('gdp',
                                                 'pop',
                                                 'gold_medals_won',
                                                 'total_medals_won',
                                                 'total_golds',
                                                 'total_medals',
                                                 'athletes'))[,year:= paste0('f',year)][order(country)]

oldat$year[oldat$year == 'f1'] <- '2000'
oldat$year[oldat$year == 'f2'] <- '2004'
oldat$year[oldat$year == 'f3'] <- '2008'
oldat$year[oldat$year == 'f4'] <- '2012'
oldat$year[oldat$year == 'f5'] <- '2016'

# Split data into training and test set -----------------------------------

# We should only use the most recent year, as the assumption of
# independence is otherwise ignored
train.data <- oldat[oldat$year == '2012']
test.data <- oldat[oldat$year == '2016']

# Drop columns that we don't need
train.data <- subset(train.data, select = -c(year, total_golds, total_medals))
test.data <- subset(test.data, select = -c(year))

# We want to predict the total number of medals only, so we can remove
# the number of gold medals won
train.data <- train.data %>% 
  dplyr::select(-c(gold_medals_won))

test.data <- test.data %>% 
  dplyr::select(-c(gold_medals_won))

# Exploratory Data Analysis -----------------------------------------------

### Pairs Plot ###

oldat %>% 
  dplyr::select(-c(country, country.code, year, soviet, comm, muslim, oneparty, host,
            total_golds, total_medals)) %>% 
  ggpairs(upper=list(continuous = wrap("points", alpha = 0.4, color = "#d73027")),
          lower = "blank", axisLabels = "none") +
  ggtitle('Pairsplot of Numeric Variables')

### Plot the log(pop) and log(gdp) against log(total_medals_won + 1) ###

p1 <- ggplot(data = oldat) + 
  geom_point(aes(x = log(pop),
                 y = log(total_medals_won + 1),
                 colour = year)) +
  xlab('log(pop)') +
  ylab('log(total_medals_won + 1)') +
  ggtitle('Log of Total Medals vs. Log of Population') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

p2 <- ggplot(data = oldat) + 
  geom_point(aes(x = log(gdp),
                 y = log(total_medals_won + 1),
                 colour = year)) +
  xlab('log(gdp)') +
  ylab('log(total_medals_won + 1)') +
  ggtitle('Log of Total Medals vs. Log of GDP') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

grid.arrange(p1, p2, nrow = 1)

### Plot athletes against total_medals_won ###

ggplot(data = oldat) + 
  geom_point(aes(x = athletes,
                 y = total_medals_won,
                 colour = year)) +
  xlab('athletes') +
  ylab('total_medals_won') +
  ggtitle('Total Medals vs. Athletes') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

### Display correlation between numerical features ###

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

plotdata <- train.data %>% 
  dplyr::select(where(is.numeric)) %>% 
  cor() %>% 
  round(2) %>% 
  get_upper_tri() %>% 
  melt()

pal <- wes_palette("Zissou1", 100, type = "continuous")

ggplot(data = plotdata, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradientn(colours = pal) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  coord_equal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("") +
  ylab("") +
  ggtitle("Correlation Between Variables for Data in 2012")

# Construct New Features --------------------------------------------------

train.data$gdpcapita <- train.data$gdp / train.data$pop
test.data$gdpcapita <- test.data$gdp / test.data$pop

# Build Models ------------------------------------------------------------

#### Linear Regression ####

linear.regression <- lm(total_medals_won ~ . -country -country.code,
                        data = train.data)
summary(linear.regression)

# Check for outliers
outlierTest(linear.regression)

# Remove outliers
train.data <- train.data[-c(80,18), ]  

# Find variables to keep
model.selection <- ols_step_forward_p(linear.regression)
model.selection

# Plot the decrease in RMSE
ggplot() +
  geom_bar(aes(x = factor(seq(1, length(model.selection$predictors))),
               y = model.selection$rmse),
           stat = 'identity',
           fill = "#f2544b") +
  scale_x_discrete(labels = as.vector(model.selection$predictors)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("") +
  ylab("RMSE") +
  ggtitle("Decrease in RMSE per Added Variable")

# Re-fit the linear regression with variables that decrease RMSE only
linear.regression <- lm(total_medals_won ~ athletes + gdp + comm + oneparty + pop,
                        data = train.data)

summary(linear.regression)  # oneparty doesn't seem to be significant

# Re-fit the linear regression without oneparty
linear.regression <- lm(total_medals_won ~ athletes + gdp + comm + pop,
                        data = train.data)

summary(linear.regression)

# Re-fit the linear regression without comm
linear.regression <- lm(total_medals_won ~ athletes + gdp + pop,
                        data = train.data)

summary(linear.regression)

# Re-fit the linear regression without pop
linear.regression <- lm(total_medals_won ~ athletes + gdp,
                        data = train.data)

summary(linear.regression)  # All variables are significant and the 
                            # adjusted R-Squared is ~91%

# Check assumptions
cor(train.data %>% dplyr::select(c(total_medals_won,
                                   athletes,
                                   gdp)))  #  Some corr. between variables

ggplot() +
  geom_density(aes(linear.regression$residuals)) +
  geom_vline(xintercept = mean(linear.regression$residuals), color = 'red') +
  ggtitle('Distribution of the Residuals') +
  xlab('') +
  ylab('')   #  Assumption of normally distributed residuals seems a bit dubious

# Check residual plot
plot(linear.regression)  #  There seem to be more outliers

# Detect and remove two more outliers based on Cook's distance
ols_plot_cooksd_bar(linear.regression)
cooksd <- cooks.distance(linear.regression)
influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:2]))
train.data <- train.data[-influential, ]

# Re-fit the linear regression
linear.regression <- lm(total_medals_won ~ athletes + gdp,
                        data = train.data)

summary(linear.regression)  # All variables are significant and the 
                            # adjusted R-Squared is ~ 81 %

# Calculate RMSE

predicted.values.lr <- predict(linear.regression, newdata = train.data)
actual.values <- train.data$total_medals_won

RMSE.lr = sqrt(mean((predicted.values.lr-actual.values)^2))

#### Poisson Regression ####

# Reset training data
train.data <- oldat[oldat$year == '2012']
train.data <- subset(train.data, select = -c(year, total_golds, total_medals, gold_medals_won))
train.data$gdpcapita <- train.data$gdp / train.data$pop

# Remove outliers based on the 0.99 quantile of the total medals won
train.data <- train.data %>% 
  filter(total_medals_won < quantile(train.data$total_medals_won, 0.99))

# Fit a Poisson regression with all variables
poisson.regression <- glm(total_medals_won ~ . -country -country.code,
                          family = poisson(link = "sqrt"), data = train.data)

summary(poisson.regression)  # Many variables are not significant

# Re-fit the model without soviet
poisson.regression <- glm(total_medals_won ~ comm + muslim + altitude + host + 
                          gdp + pop + athletes + gdpcapita + oneparty,
                          family = poisson(link = "sqrt"), data = train.data)

summary(poisson.regression)

# Re-fit the model without pop
poisson.regression <- glm(total_medals_won ~ comm + muslim + host + 
                          gdp + altitude + athletes + gdpcapita + oneparty,
                          family = poisson(link = "sqrt"), data = train.data)

summary(poisson.regression) 

# Re-fit the model without oneparty
poisson.regression <- glm(total_medals_won ~ comm + muslim + host + 
                          gdp + altitude + athletes + gdpcapita,
                          family = poisson(link = "sqrt"), data = train.data)

summary(poisson.regression)

# Re-fit the model without gdpcapita
poisson.regression <- glm(total_medals_won ~ comm + muslim + host + 
                          gdp + altitude + athletes,
                          family = poisson(link = "sqrt"), data = train.data)

summary(poisson.regression)

# Re-fit the model without host
poisson.regression <- glm(total_medals_won ~ comm + muslim + gdp + altitude +
                          athletes, family = poisson(link = "sqrt"), data = train.data)

summary(poisson.regression)

# Re-fit the model without muslim
poisson.regression <- glm(total_medals_won ~ comm + gdp + altitude +
                            athletes, family = poisson(link = "sqrt"), data = train.data)

summary(poisson.regression)  #  All remaining variables seem significant
                             #  but we have a quite large deviance

# Calculate dispersion parameter
X2 <- sum(resid(poisson.regression, type = "pearson")^2)
dp <- X2/poisson.regression$df.res

# Calculate RMSE
actual.values <- train.data$total_medals_won
predicted.values.pr <- predict(poisson.regression, newdata = train.data, type = "response")

RMSE.pr = sqrt(mean((predicted.values.pr-actual.values)^2))

#### Quasi-Poisson model ####

# Fit a Quasi-Poisson model with all variables
quasipoi.regression <- glm(total_medals_won ~ .  -country -country.code,
                           family = quasipoisson(link = "log"), data = train.data)

summary(quasipoi.regression)

drop1(quasipoi.regression, test = "F")

# Re-fit model without soviet
quasipoi.regression <- glm(total_medals_won ~ comm + muslim + host + gdpcapita + 
                           gdp + oneparty + athletes + pop + altitude,
                           family = quasipoisson(link = "log"), data = train.data)

drop1(quasipoi.regression, test = "F")

# Re-fit model without  pop
quasipoi.regression <- glm(total_medals_won ~ comm + muslim + host + gdpcapita + 
                           gdp + oneparty + athletes + altitude,
                           family = quasipoisson(link = "log"), data = train.data)

drop1(quasipoi.regression, test = "F")

# Re-fit model without  altitude
quasipoi.regression <- glm(total_medals_won ~ comm + muslim + host + gdpcapita + 
                           gdp + oneparty + athletes, family = quasipoisson(link = "log"), data = train.data)

drop1(quasipoi.regression, test = "F")

# Re-fit model without gdpcapita
quasipoi.regression <- glm(total_medals_won ~ comm + muslim + host +
                           gdp + oneparty + athletes, family = quasipoisson(link = "log"), data = train.data)

drop1(quasipoi.regression, test = "F")

# Re-fit model without  oneparty
quasipoi.regression <- glm(total_medals_won ~ comm + muslim + host +
                           gdp + athletes, family = quasipoisson(link = "log"), data = train.data)

drop1(quasipoi.regression, test = "F")

# Re-fit model without muslim
quasipoi.regression <- glm(total_medals_won ~ comm + host +
                           gdp + athletes, family = quasipoisson(link = "log"), data = train.data)

drop1(quasipoi.regression, test = "F")

# Re-fit model without gdp
quasipoi.regression <- glm(total_medals_won ~ comm + host +
                           athletes, family = quasipoisson(link = "log"), data = train.data)

drop1(quasipoi.regression, test = "F")

summary(quasipoi.regression)

# Calculate RMSE

predicted.values.qpr <- predict(quasipoi.regression, newdata = train.data, type = "response")
RMSE.qpr = sqrt(mean((predicted.values.qpr-actual.values)^2))

#### Negative Binomial Regression ####

# Fit a Negative-Binomial model with all variables
glm.nb(total_medals_won ~ . -country -country.code,
                        data = train.data, link = log)

nb.regression <- glm.nb(total_medals_won ~ . -country -country.code,
                        data = train.data, link = sqrt)

summary(nb.regression)  # Many variables are not significant

# Re-fit the model without soviet
nb.regression <- glm.nb(total_medals_won ~ comm + muslim + gdp + host + pop +
                        oneparty + athletes + gdpcapita + altitude, data = train.data,
                        link = sqrt)

summary(nb.regression)

# Re-fit the model without oneparty
nb.regression <- glm.nb(total_medals_won ~ comm + muslim + gdp + host + pop +
                        athletes + gdpcapita + altitude, data = train.data,
                        link = sqrt)
summary(nb.regression)

# Re-fit the model without host
nb.regression <- glm.nb(total_medals_won ~ comm + muslim + gdp + pop +
                        athletes + gdpcapita + altitude, data = train.data,
                        link = sqrt)

summary(nb.regression) 

# Re-fit the model without pop
nb.regression <- glm.nb(total_medals_won ~ comm + muslim + gdp +
                        athletes + gdpcapita + altitude, data = train.data,
                        link = sqrt)

summary(nb.regression) 

# Re-fit the model without gdpcapita
nb.regression <- glm.nb(total_medals_won ~ comm + muslim + gdp +
                        athletes + altitude, data = train.data,
                        link = sqrt)

summary(nb.regression) 

# Re-fit the model without muslim
nb.regression <- glm.nb(total_medals_won ~ comm + gdp + athletes + altitude,
                        data = train.data, link = sqrt)

summary(nb.regression) 

# Re-fit the model without gdp
nb.regression <- glm.nb(total_medals_won ~ comm + athletes + altitude,
                        data = train.data, link = sqrt)
summary(nb.regression) 

# Re-fit the model without altitude
nb.regression <- glm.nb(total_medals_won ~ comm + athletes,
                        data = train.data, link = sqrt)

summary(nb.regression) 

# Re-fit the model without comm
nb.regression <- glm.nb(total_medals_won ~ athletes,
                        data = train.data, link = sqrt)

summary(nb.regression) 

# Compare the Poisson Regression and Negative Binomial Regression

c(poisson.regression$deviance, poisson.regression$aic)
c(nb.regression$deviance, nb.regression$aic)

# Calculate RMSE

predicted.values.nb <- predict(nb.regression, newdata = train.data, type = "response")
RMSE.nb = sqrt(mean((predicted.values.nb-actual.values)^2))

# Evaluate Models ---------------------------------------------------------

# Select the first 50 values of the training data and each model's prediction
actual.values <- actual.values[1:50]
predicted.values.lr <- predicted.values.lr[1:50]
predicted.values.pr <- predicted.values.pr[1:50]
predicted.values.qpr <- predicted.values.qpr[1:50]
predicted.values.nb <- predicted.values.nb[1:50]

# Plot the actual values and each model's prediction
ggplot() +
  geom_point(aes(x = seq(1, length(actual.values)),
                y = actual.values,
                color = 'black'),
            alpha = 0.8,
            shape = 17) +
  geom_line(aes(x = seq(1, length(actual.values)),
                y = actual.values,
                color = 'black'),
            alpha = 0.6) +
  geom_point(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.nb,
                 color = 'red'),
            alpha = 0.25) +
  geom_path(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.nb,
                color = 'red'),
             alpha = 0.25) +
  geom_point(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.pr,
                 color = 'blue'),
             alpha = 0.25) +
  geom_line(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.pr,
                color = 'blue'),
             alpha = 0.25) +
  geom_point(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.qpr,
                 color = 'violet'),
             alpha = 0.25) +
  geom_line(aes(x = seq(1, length(actual.values)),
                y = predicted.values.qpr,
                color = 'violet'),
            alpha = 0.25) +
  geom_point(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.lr,
                 color = 'green'),
             alpha = 0.25) +
  geom_line(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.lr,
                color = 'green'),
             alpha = 0.25) +
  labs(x = "", 
       y = "",
       title = "Actual Values and Predictions for the first 50 Data Points") +
  scale_color_identity(name = "Model fit",
                       breaks = c("black", "red", "blue", "violet", "green"),
                       labels = c("Actual Values",
                                  "NeBi Regression",
                                  "Poisson Regression",
                                  "Quasi-Poisson Regression",
                                  "Linear Regression"),
                       guide = "legend") +
  theme(legend.position = 'bottom')

# From the plot above, it seems that all models are doing pretty good,
# but the NeBi model is quite off in some cases

RMSE.lr
RMSE.pr
RMSE.qpr
RMSE.nb

# Test Performance of Quasi-Poisson Regression Mode -----------------------

actual.values <- test.data$total_medals_won

# Make predictions on the test set using the linear regression model
predicted.values.lr <- predict(linear.regression, newdata = test.data,
                               type = "response")

# Make predictions on the test set using the Quasi-Poisson model
predicted.values.qpr <- predict(quasipoi.regression, newdata = test.data,
                                type = "response")

# Calculate the RMSE of both models on the test set 
RMSE.test.lr <- sqrt(mean((predicted.values.lr-actual.values)^2))
RMSE.test.qpr <- sqrt(mean((predicted.values.qpr-actual.values)^2))

# Compare each model's RMSE on the training and the test set
RMSE.lr
RMSE.test.lr
RMSE.qpr
RMSE.test.qpr

# Plot the actual and the predicted total medals won for each of the 108 countries
ggplot() +
  geom_point(aes(x = seq(1, length(actual.values)),
                y = actual.values,
                color = 'black')) +
  geom_line(aes(x = seq(1, length(actual.values)),
                 y = actual.values,
                 color = 'black')) +
  geom_point(aes(x = seq(1, length(actual.values)),
                y = predicted.values.lr,
                color = 'green')) +
  geom_line(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.lr,
                 color = 'green')) +
  geom_point(aes(x = seq(1, length(actual.values)),
                 y = predicted.values.qpr,
                 color = 'violet')) +
  geom_line(aes(x = seq(1, length(actual.values)),
                y = predicted.values.qpr,
                color = 'violet')) +
  labs(x = "", 
       y = "",
       title = "Actual Values and Predictions for the Test Data") +
  scale_color_identity(name = "Model fit",
                       breaks = c("black", "green", "violet"),
                       labels = c("Actual Values",
                                  "Linear Regression",
                                  "Quasi-Poisson Regression"),
                       guide = "legend") +
  theme(legend.position = 'bottom')

# Take a look at the predicted and actual values for each country
predact <- data.frame(country = test.data$country,
                      actual = round(actual.values),      
                      predicted_lr = round(predicted.values.lr),
                      predicted_qpr = round(predicted.values.qpr))

# Calculate in how many cases the model has a good, moderate or bad prediction
predact <- predact %>% mutate(eval.lr = case_when(
  abs(predact$actual - predact$predicted_lr) <= 2 ~ "Good",
  abs(predact$actual - predact$predicted_lr) > 2 & abs(predact$actual - predact$predicted_lr) <= 5 ~ "Moderate",
  abs(predact$actual - predact$predicted_lr) > 5 ~ "Bad"
))

predact <- predact %>% mutate(eval.qpr = case_when(
  abs(predact$actual - predact$predicted_qpr) <= 2 ~ "Good",
  abs(predact$actual - predact$predicted_qpr) > 2 & abs(predact$actual - predact$predicted_qpr) <= 5 ~ "Moderate",
  abs(predact$actual - predact$predicted_qpr) > 5 ~ "Bad"
))

predact

predtable <- cbind('Linear Regression' = table(predact$eval.lr),
                   'Quasi-Poisson Regression' = table(predact$eval.qpr))
predtable[c(2,3,1),]

