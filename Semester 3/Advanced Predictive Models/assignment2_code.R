
# Install/load packages ---------------------------------------------------
list_of_packages <- c("ggplot2", "dplyr", "magrittr", "gridExtra", "GGally",
                      "ggrepel", "forecast")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(list_of_packages, require, character.only = TRUE)
rm(list_of_packages, new_packages)

# Load the data -----------------------------------------------------------

original_df <- read.csv('data_2021-Jun-28.csv')

# Validity check ----------------------------------------------------------

# Look at the overall structure of the data
str(original_df)

# Check for missing values
original_df[rowSums(is.na(original_df)) != 0,]

# Prepare data ------------------------------------------------------------

# Remove unnecessary columns
df <- original_df %>% select(-c("areaType", "areaName", "areaCode"))

# Format 'date' as a date object
df$date <- as.Date(df$date)
str(df)
summary(df)

# Splitting data into training and test set -------------------------------

# Specify the number of days that should be in the test set
no_days = 14
# Make the split into a training and a test set
test_df <- df %>% top_n(no_days) %>% arrange(date)
train_df <- df %>% top_n(-(nrow(df) - no_days)) %>% arrange(date)

rm(original_df, no_days)

# EDA ---------------------------------------------------------------------

summary(train_df)

#### Plot the daily new admissions ####
ggplot(data = train_df,
       aes(x = date,
           y = newAdmissions)) +
  # Add all relevant vertical lines
  geom_vline(xintercept = c(as.Date('2020-03-23'),
                            as.Date('2020-05-28'),
                            as.Date('2020-12-02'),
                            as.Date('2020-12-30'),
                            as.Date('2021-02-18'),
                            as.Date('2021-04-22')),
             linetype = 2, color = 'grey') +
  
  # Annotate the vertical lines
  annotate(x = as.Date('2020-03-23'), y = 5000, label = "First National Lockdown",
           vjust = 2, geom = "text", angle = 90, size = 3.5, col = '#616161') +
  annotate(x = as.Date('2020-05-28'), y = 5100, label = "NHS Test and Trace",
           vjust = 2, geom = "text", angle = 90, size = 3.5, col = '#616161') +
  annotate(x = as.Date('2020-12-02'), y = 5250, label = "Pfi/Bio Vaccine",
           vjust = 2, geom = "text", angle = 90, size = 3.5, col = '#616161') +
  annotate(x = as.Date('2020-12-30'), y = 5250, label = "Oxf/Ast Vaccine",
           vjust = 2, geom = "text", angle = 90, size = 3.5, col = '#616161') +
  annotate(x = as.Date('2021-02-18'), y = 4900, label = "25% received 1 vacc. dose",
           vjust = 2, geom = "text", angle = 90, size = 3.5, col = '#616161') +
  annotate(x = as.Date('2021-04-22'), y = 4900, label = "50% received 1 vacc. dose",
           vjust = 2, geom = "text", angle = 90, size = 3.5, col = '#616161') +
  
  # Add the bars and a line
  geom_bar(stat = 'identity', aes(col = newAdmissions, fill = newAdmissions)) +
  geom_line(col = 'black', lwd = 0.5) +
  
  # Add some aesthetics
  scale_fill_gradient2(low = "#035c09",
                        mid = '#d2c900',
                        high = "#8A0A03",
                        midpoint = 1500) +
  scale_color_gradient2(low = "#035c09",
                       mid = '#d2c900',
                       high = "#8A0A03",
                       midpoint = 1500) +
  ggtitle('Daily Hospital Admissions of COVID-19 Patients') +
  coord_cartesian( ylim=c(0, 6000), expand = FALSE ) +
  xlab('') +
  ylab('New Admissions') +
  theme_bw() +
  theme(legend.position = 'none')

#### Plot the daily cumulative admissions ####
ggplot(data = train_df,
       aes(x = date,
           y = cumAdmissions)) +
  
  geom_bar(stat = 'identity', aes(col = cumAdmissions, fill = cumAdmissions)) +
  geom_line(col = 'black', lwd = 0.75) +
  
  scale_fill_gradient2(low = "#035c09",
                       mid = '#d2c900',
                       high = "#8A0A03",
                       midpoint = 150000) +
  scale_color_gradient2(low = "#035c09",
                        mid = '#d2c900',
                        high = "#8A0A03",
                        midpoint = 150000) +
  ggtitle('Cumulative Hospital Admissions of COVID-19 Patients') +
  coord_cartesian( ylim=c(0, 600000), expand = FALSE ) +
  xlab('') +
  ylab('Cumulative Admissions') +
  theme_bw()

# Remove the cumulative admissions as we don't need it anymore
train_df <- train_df %>% select(-cumAdmissions)
test_df <- test_df %>% select(-cumAdmissions)

#### ACF/PACF ####

# Plot the ACF for new submissions
acf(train_df$newAdmissions, main = 'ACF Plot for New Admissions')

# The ACF plot suggests that the data is non-stationary, therefore we should
# try a first-order differencing technique
train_df_diff1 <- data.frame(diff = diff(train_df$newAdmissions,
                                        lag = 1,
                                        differences = 1),
                             ind = train_df$date[-1])

# Plot the ACF after first-order differencing
acf(train_df_diff1$diff, main = 'ACF Plot for New Admissions after first-order differencing')

# Plot the PACF after first-order differencing
pacf(train_df_diff1$diff, main = 'PACF Plot for New Admissions after first-order differencing')

# Model Building ----------------------------------------------------------

#### ARIMA ####

# Automatically find the best ARIMA model
arima_model <- auto.arima(train_df$newAdmissions,
                    ic = 'aicc')

# The best ARIMA model seems to be ARIMA(2,1,2)
summary(arima_model)

# Plot the forecast with a 95% confidence interval
par(mfrow = c(1,2))
plot(forecast(arima_model, h = 14),
     main = 'ARIMA(2,1,2) Forecast',
     ylab = '# Admissions',
     ylim = c(0, 5000),
     cex.main = 0.95)
plot(forecast(arima_model, h = 14),
     xlim = c(400,451),
     ylim = c(-750,1000),
     main = 'ARIMA(2,1,2) Forecast, Zoomed In',
     ylab = '# Admissions',
     cex.main = 0.95)

# Plot the forecast and actual data from the test set
colors <- c('Actual Train' = 'black', 'Predicted Train' = 'orange',
            'Predicted Test' = 'red', 'Actual Test' = 'blue')
forecast_plot1 <- ggplot() +
  geom_line(aes(x = train_df$date,
                y = train_df$newAdmissions,
                colour = 'Actual Train')) +
  geom_line(aes(x = train_df$date,
                y = arima_model$fitted,
                colour = 'Predicted Train'),
            alpha = 0.5) +
  geom_line(aes(x = test_df$date,
                y = forecast(arima_model, h = 14)$mean,
                colour = 'Predicted Test')) +
  geom_line(aes(x = test_df$date,
                y = test_df$newAdmissions,
                colour = 'Actual Test')) +
  labs(title = 'Predicted and Actual Number of New Covid-19 Admissions in the UK',
       x = '',
       y = '# New Admissions',
       color = 'Legend') +
  scale_color_manual(values = colors) +
  theme_bw()

# Zoom in to better see the forecast
forecast_plot2 <- forecast_plot +
  xlim(c(as.Date('2021-03-01'), as.Date('2021-07-01'))) +
  ylim(c(0, 1000))

grid.arrange(forecast_plot1, forecast_plot2)

# Calculate the RMSE for the forecast
arima_rmse <- sqrt(mean((forecast(arima_model, h = 14)$mean - test_df$newAdmissions)^2))

# Calculate the Coverage for the forecast
low95 <- forecast(arima_model, h = 14)$lower[,2]
upp95 <- forecast(arima_model, h = 14)$upper[,2]
arima_coverage <- sum(test_df$newAdmissions <= upp95 &
                      test_df$newAdmissions >= low95)

#### Triple Exponential Model ####

# Automatically find the best TEM model
tem_model <- ets(ts(train_df$newAdmissions))

# The best TEM model seems to be ETS(M,Ad,N)
summary(tem_model)

# Plot the forecast with a 95% confidence interval
plot(forecast(tem_model, h = 14),
     main = 'ETS(M, Ad, N) Forecast',
     ylab = '# Admissions',
     cex.main = 0.95)
plot(forecast(tem_model, h = 14),
     main = 'ETS(M, Ad, N) Forecast, Zoomed In',
     ylab = '# Admissions',
     xlim = c(400,451),
     ylim = c(0,350),
     cex.main = 0.95)

# Plot the forecast and actual data from the test set
colors <- c('Actual Train' = 'black', 'Predicted Train' = 'orange',
            'Predicted Test' = 'red', 'Actual Test' = 'blue')
forecast_plot1 <- ggplot() +
  geom_line(aes(x = train_df$date,
                y = train_df$newAdmissions,
                colour = 'Actual Train')) +
  geom_line(aes(x = train_df$date,
                y = tem_model$fitted,
                colour = 'Predicted Train'),
            alpha = 0.5) +
  geom_line(aes(x = test_df$date,
                y = forecast(tem_model, h = 14)$mean,
                colour = 'Predicted Test')) +
  geom_line(aes(x = test_df$date,
                y = test_df$newAdmissions,
                colour = 'Actual Test')) +
  labs(title = 'Predicted and Actual Number of New Covid-19 Admissions in the UK',
       x = '',
       y = '# New Admissions',
       color = 'Legend') +
  scale_color_manual(values = colors) +
  theme_bw()

# Zoom in to better see the forecast
forecast_plot2 <- forecast_plot1 +
  xlim(c(as.Date('2021-03-01'), as.Date('2021-07-01'))) +
  ylim(c(0, 1000))

grid.arrange(forecast_plot1, forecast_plot2)

# Calculate the RMSE for the forecast
tem_rmse <- sqrt(mean((forecast(tem_model, h = 14)$mean - test_df$newAdmissions)^2))

# Calculate the Coverage for the forecast
low95 <- forecast(tem_model, h = 14)$lower[,2]
upp95 <- forecast(tem_model, h = 14)$upper[,2]
tem_coverage <- sum(test_df$newAdmissions <= upp95 &
                        test_df$newAdmissions >= low95)

# Review ------------------------------------------------------------------
colors <- c('ARIMA' = '#00c3e3', 'TEM' = '#ff4554')
ggplot() +
  geom_point(aes(x = test_df$date,
                 y = forecast(arima_model, h = 14)$mean,
                 colour = 'ARIMA'),
             alpha = 0.5) +
  geom_line(aes(x = test_df$date,
                y = forecast(arima_model, h = 14)$mean,
                colour = 'ARIMA'),
            alpha = 0.5) +
  geom_point(aes(x = test_df$date,
                 y = forecast(tem_model, h = 14)$mean,
                 colour = 'TEM'),
             alpha = 0.5) +
  geom_line(aes(x = test_df$date,
                y = forecast(tem_model, h = 14)$mean,
                colour = 'TEM'),
            alpha = 0.5) +
  labs(title = 'Predicted Values of ARIMA and TEM',
       x = '',
       y = '# New Admissions',
       color = 'Legend') +
  scale_color_manual(values = colors) +
  theme_bw()
