library(tidyverse)
library(openintro)
library(dplyr)
library(gapminder)



#Exercise 1
data(ncbirths)

ggplot(data = ncbirths, aes(x = weight, y = weeks)) +
  geom_point()
used_cars <- used_cars %>%
  mutate(price_numeric = as.numeric(str_replace_all(price, "[^0-9.]", "")))
used_cars <- used_cars %>%
  mutate(milage_numeric = as.numeric(str_replace_all(milage, "[^0-9.]", "")))

ggplot(data=used_cars,aes(x=model_year,y=milage_numeric,color=fuel_type))+ geom_point()

#Exercise 2
ncbirths$weeks_cut <- cut(ncbirths$weeks, breaks = 4)


ggplot(data = ncbirths, aes(x = weeks_cut, y = weight)) +
  geom_boxplot()


used_cars$price_cut <- cut(used_cars$price_numeric, breaks = 4)


ggplot(data = used_cars, aes(x = price_cut, y = milage_numeric,color=fuel_type)) +
  geom_boxplot() +
  labs(x = "Price Category", y = "Mileage (Numeric)")
#Exercise 3

data(mammals)
data(mlbbat10)
data(bdims)
data(smoking)


ggplot(data = mammals, aes(x = body_wt, y = brain_wt)) +
  geom_point()


ggplot(data = mlbbat10, aes(x = obp, y = slg)) +
  geom_point()


bdims$sex <- as.factor(bdims$sex)
ggplot(data = bdims, aes(x = hgt, y = wgt, color = sex)) +
  geom_point()

ggplot(data = smoking, aes(x = age, y = amt_weekdays)) +
  geom_point()


ggplot(data = used_cars, aes(x = model_year, y = milage,color=fuel_type)) +
  geom_point() +
  labs(x = "Model Year", y = "Mileage")

ggplot(data = used_cars, aes(x = model_year, y = price_numeric,color = fuel_type)) +
  geom_point() +
  labs(x = "Model Year", y = "Price (Numeric)")
#Exercise 4
ab_gt_200 <- mlbbat10 %>%
  filter(at_bat >= 200)


ggplot(data = ab_gt_200, aes(x = obp, y = slg)) +
  geom_point()


outlier_player <- ab_gt_200 %>%
  filter(obp < 0.200)


outlier_player





library(ggplot2)
used_cars <- read_csv("C:\\Study\\DataSets\\used_cars.csv")
