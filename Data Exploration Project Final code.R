#Load libraries
library(tidyverse)
library(lubridate)
library(fixest)

#Load the data
walmart <- read_csv("Walmart_Sales.csv")

#Convert dates and create month variable
walmart <- walmart %>% mutate(Date = dmy(Date), Month = month(Date))

#Look at summary statistics
summary(walmart)

#Create a scatter plot for Temperature vs Sales
ggplot(walmart, aes(x = Temperature, y = Weekly_Sales)) + geom_point(alpha = 0.1) + geom_smooth(method = "lm") +
  labs(title = "Temperature vs Sales", x = "Temperature (F)", y = "Weekly Sales ($)")

#Create a scatter plot of temperature vs sales by month
ggplot(walmart, aes(x = Temperature, y = Weekly_Sales)) + geom_point(alpha = 0.05) + geom_smooth(method = "lm") + facet_wrap(~Month)

#Run a linear regression model using fixed effects for store and month
m1 <- feols(Weekly_Sales ~ Temperature + Holiday_Flag + Unemployment + Fuel_Price + CPI | Store + Month, cluster = ~Store, data = walmart)

#Look at results of the regreesion
summary(m1)

#Check to see if the relationship between temperature and sales is non-linear using a quadratic
walmart <- walmart %>% mutate(Temperature_sq = Temperature^2)

#Run model with temperature and temperature^2
m2 <- feols(Weekly_Sales ~ Temperature + Temperature_sq + Holiday_Flag + Unemployment + Fuel_Price + CPI | Store + Month, cluster = ~Store, data = walmart)

#Look at results of the second regression
summary(m2)

#Compare linear and quadratic models
etable(m1, m2)

#Calculate where sales peak by calculating: Maximum of Y = b1*X + b2*X^2 occurs at X = -b1/(2*b2)
b1 <- coef(m2)["Temperature"]
b2 <- coef(m2)["Temperature_sq"]
peak <- -b1 / (2 * b2)
cat("Sales peak at", round(peak, 1), "degrees F\n")

#Plot the quadratic relationship
ggplot(walmart, aes(x = Temperature, y = Weekly_Sales)) + geom_point(alpha = 0.05) + stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  labs(title = "Quadratic Relationship", x = "Temperature (F)", y = "Weekly Sales ($)")

#Check model fit by comparing R-squared values to see how much variance each model explains
fitstat(m1, 'r2')
fitstat(m2, 'r2')