library(dplyr)
library(stringr)
library(imputeTS)

# Read in all CSV files into separate dataframes --------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('../Original Data Files/')
temp = list.files(pattern = '*.csv')
for (i in 1:length(temp))
  assign(temp[i], read.csv(temp[i]))
rm(temp)
rm(Sites.csv)

setwd('../Prepared Data Files For App/Inidividual Sites/')


# Impute missing values for Site_1090 -------------------------------------

summary(Site_1090.csv)

# Imputing NA's of 'wind speed' as mean of closest data point in each direction
Site_1090.csv$wind_speed <- na_ma(Site_1090.csv$wind_speed, k = 1)

# Since we do not have any data for 'visibility', I see no reasonable way to 
# impute the NA's

# Save as CSV
write.csv(Site_1090.csv, file = '1090.csv')

# Impute missing values for Site_1135 -------------------------------------

summary(Site_1135.csv)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_1135.csv$visibility <- na_ma(Site_1135.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_1135.csv, file = '1135.csv')

# Impute missing values for Site_1144 -------------------------------------

summary(Site_1144.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_1144.csv$wind_speed <- na_ma(Site_1144.csv$wind_speed, k = 1)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_1144.csv$air_temperature <- na_ma(Site_1144.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_1144.csv$rltv_hum <- na_ma(Site_1144.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_1144.csv$visibility <- na_ma(Site_1144.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_1144.csv, file = '1144.csv')

# Impute missing values for Site_1226 -------------------------------------

summary(Site_1226.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_1226.csv$wind_speed <- na_ma(Site_1226.csv$wind_speed, k = 1)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_1226.csv$air_temperature <- na_ma(Site_1226.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_1226.csv$rltv_hum <- na_ma(Site_1226.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_1226.csv$visibility <- na_ma(Site_1226.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_1226.csv, file = '1226.csv')

# Impute missing values for Site_1302 -------------------------------------

summary(Site_1302.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_1302.csv$wind_speed <- na_ma(Site_1302.csv$wind_speed, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_1302.csv$rltv_hum <- na_ma(Site_1302.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_1302.csv$visibility <- na_ma(Site_1302.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_1302.csv, file = '1302.csv')

# Impute missing values for Site_1450 -------------------------------------

summary(Site_1450.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_1450.csv$wind_speed <- na_ma(Site_1450.csv$wind_speed, k = 1)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_1450.csv$air_temperature <- na_ma(Site_1450.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_1450.csv$rltv_hum <- na_ma(Site_1450.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_1450.csv$visibility <- na_ma(Site_1450.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_1450.csv, file = '1450.csv')

# Impute missing values for Site_161 --------------------------------------

summary(Site_161.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_161.csv$wind_speed <- na_ma(Site_161.csv$wind_speed, k = 1)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_161.csv$air_temperature <- na_ma(Site_161.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_161.csv$rltv_hum <- na_ma(Site_161.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_161.csv$visibility <- na_ma(Site_161.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_161.csv, file = '161.csv')

# Impute missing values for Site_235 --------------------------------------

summary(Site_235.csv)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_235.csv$air_temperature <- na_ma(Site_235.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_235.csv$rltv_hum <- na_ma(Site_235.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_235.csv$visibility <- na_ma(Site_235.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_235.csv, file = '235.csv')

# Impute missing values for Site_315 --------------------------------------

summary(Site_315.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_315.csv$wind_speed <- na_ma(Site_315.csv$wind_speed, k = 1)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_315.csv$air_temperature <- na_ma(Site_315.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_315.csv$rltv_hum <- na_ma(Site_315.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_315.csv$visibility <- na_ma(Site_315.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_315.csv, file = '315.csv')

# Impute missing values for Site_384 --------------------------------------

summary(Site_384.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_384.csv$wind_speed <- na_ma(Site_384.csv$wind_speed, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_384.csv$visibility <- na_ma(Site_384.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_384.csv, file = '384.csv')

# Impute missing values for Site_4 ----------------------------------------

summary(Site_4.csv)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_4.csv$visibility <- na_ma(Site_4.csv$visibility, k = 1)

# Change date format of site 4
Site_4.csv$ob_time <- lubridate::as_datetime(Site_4.csv$ob_time,format="%d/%m/%Y %H:%M")

# Save as CSV
write.csv(Site_4.csv, file = '4.csv')

# Impute missing values for Site_409 --------------------------------------

summary(Site_409.csv)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_409.csv$visibility <- na_ma(Site_409.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_409.csv, file = '409.csv')

# Impute missing values for Site_613 --------------------------------------

summary(Site_613.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_613.csv$wind_speed <- na_ma(Site_613.csv$wind_speed, k = 1)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_613.csv$air_temperature <- na_ma(Site_613.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_613.csv$rltv_hum <- na_ma(Site_613.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_613.csv$visibility <- na_ma(Site_613.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_613.csv, file = '613.csv')

# Impute missing values for Site_643 --------------------------------------

summary(Site_643.csv)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_643.csv$visibility <- na_ma(Site_643.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_643.csv, file = '643.csv')

# Impute missing values for Site_708 --------------------------------------

summary(Site_708.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_708.csv$wind_speed <- na_ma(Site_708.csv$wind_speed, k = 1)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_708.csv$air_temperature <- na_ma(Site_708.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_708.csv$rltv_hum <- na_ma(Site_708.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_708.csv$visibility <- na_ma(Site_708.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_708.csv, file = '708.csv')

# Impute missing values for Site_709 --------------------------------------

summary(Site_709.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_709.csv$wind_speed <- na_ma(Site_709.csv$wind_speed, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_709.csv$rltv_hum <- na_ma(Site_709.csv$rltv_hum, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_709.csv$visibility <- na_ma(Site_709.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_709.csv, file = '709.csv')

# Impute missing values for Site_79 ---------------------------------------

summary(Site_79.csv)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_79.csv$visibility <- na_ma(Site_79.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_79.csv, file = '79.csv')

# Impute missing values for Site_842 ---------------------------------------

summary(Site_842.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_842.csv$wind_speed <- na_ma(Site_842.csv$wind_speed, k = 1)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_842.csv$visibility <- na_ma(Site_842.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_842.csv, file = '842.csv')

# Impute missing values for Site_908 ---------------------------------------

summary(Site_908.csv)

# Imputing NA's of 'wind_speed' as mean of closest data point in each direction
Site_908.csv$wind_speed <- na_ma(Site_908.csv$wind_speed, k = 1)

# Imputing NA's of 'air_temperature' as mean of closest data point in each direction
Site_908.csv$air_temperature <- na_ma(Site_908.csv$air_temperature, k = 1)

# Imputing NA's of 'rltv_hum' as mean of closest data point in each direction
Site_908.csv$rltv_hum <- na_ma(Site_908.csv$rltv_hum, k = 1)

# Save as CSV
write.csv(Site_908.csv, file = '908.csv')

# Impute missing values for Site_971 ---------------------------------------

summary(Site_971.csv)

# Imputing NA's of 'visibility' as mean of closest data point in each direction
Site_971.csv$visibility <- na_ma(Site_971.csv$visibility, k = 1)

# Save as CSV
write.csv(Site_971.csv, file = '971.csv')