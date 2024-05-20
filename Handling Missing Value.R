library(tidyr)
library(dplyr)
library(tidyverse)
install.packages("Hmisc")


library(Hmisc)

#identifikasi missing value
data = read.csv("C:/Users/Natas/Downloads/CarRentalData.csv")
summary(data)
str(data)

#handling missing value
data$rating <- with(data, impute(rating,median))
summary(data$rating)

#identifikasi missing value pada data kategorik
data[data == '']<- NA
sum(is.na(data$fuelType))
sum(is.na(data$location.city))
sum(is.na(data$vehicle.make))
sum(is.na(data$vehicle.model))
sum(is.na(data$vehicle.type))


#mendapatkan modus dari data kategorik
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate((match(v, uniqv))))]
}

#mendapatkan modus dari variabel fueltype
getmode(data$fuelType)

#handling missing value
library(dplyr)
data %>% 
  mutate(imputed_fuelType = if_else(is.na(fuelType),
                                    getmode(fuelType),
                                    fuelType))

#identifikasi outlier pada data rating
summary(data$rating)
Q1 = quantile(data$rating, probs=0.25) 
Q3 = quantile(data$rating, probs = 0.75)
IQR = Q3-Q1
IQR(data$rating)

#threshold/batas bawah dan batas atas
Tmin = Q1 - (1.5*IQR)
Tmax = Q3 + (1.5*IQR)
data$rating[which(data$rating < Tmin | data$rating> Tmax)]

#handling outlier
log <- log10(data$rating)
hist(data$rating, col='steelblue', main='Original')
hist(log, col='coral2', main='Log Transformed')

#Removing outliers pada data rating
p75 = quantile(data$rating,0.75, na.rm = TRUE)
p25 = quantile(data$rating,0.25, na.rm = TRUE)
# print(p75)
# print(p25)
iqr = p75-p25
# print(head(is.na(data$rating)))
data$rating = ifelse(data$rating<p25-1.5*iqr,ave(data$rating, FUN = function(x) p25-1.5*iqr),data$rating)
data$rating = ifelse(data$rating>p75+1.5*iqr,ave(data$rating, FUN = function(x) p75+1.5*iqr),data$rating)
boxplot(data$rating, col ="lightblue", main="Without Outliers") # Attribute without outliers









