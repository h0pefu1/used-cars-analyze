
library(openintro)
data(bdims)
plot(bdims$hgt, bdims$wgt, xlab = "Height", ylab = "Weight", main = "Scatterplot of Weight vs. Height")
abline(lm(wgt ~ hgt, data = bdims), col = "red")





library(MASS)
linear_model <- lm(wgt ~ hgt, data = bdims)
summary(linear_model)
linear_model <- lm(slg ~ obp, data = mlbbat10)
summary(linear_model)
data(mammals)
str(mammals)
mammals$log_body <- log(mammals$body)
mammals$log_brain <- log(mammals$brain)
linear_model <- lm(log_body ~ log_brain, data = mammals)
summary(linear_model)












library(openintro)

library(dplyr)
data(bdims)
hgt_wgt_mod <- lm(wgt ~ hgt, data = bdims)
coefficients_hgt_wgt <- coef(hgt_wgt_mod)
print(coefficients_hgt_wgt)
summary_hgt_wgt <- summary(hgt_wgt_mod)
print(summary_hgt_wgt)

library(broom)
hgt_wgt_tidy <- augment(hgt_wgt_mod)
glimpse(hgt_wgt_tidy)




library(dplyr)
library(broom)
library(tidyverse)

used_cars <- read_csv("C:\\Study\\DataSets\\used_cars.csv")


library(dplyr)
library(broom)




used_cars$price <- as.numeric(gsub("[\\$,]", "", used_cars$price))
used_cars$milage <- as.numeric(gsub("[\\,, mi\\.]", "", used_cars$milage))

price_milage_mod <- lm(price ~ milage, data = used_cars)

coefficients_price_milage <- coef(price_milage_mod)
print(coefficients_price_milage)


summary_price_milage <- summary(price_milage_mod)
print(summary_price_milage)


price_milage_tidy <- augment(price_milage_mod)
glimpse(price_milage_tidy)