library(openintro)
library(tidyverse)
library(dplyr)
mydata <- read.csv("used_cars.csv")
head(mydata)

?glimpse(mydata)

hybryd <- mydata %>% filter(fuel_type=="Hybrid")
glimpse(mydata)
glimpse(hybryd)
#1.4
med_num_char <- median(mydata$model_year)

mydata_updated <- mydata %>%
  mutate(price_char = if_else(model_year < med_num_char, "below median", "at or above median"))

counts <- mydata_updated %>%
  count(price_char)

print(counts)

#1.5
mydata_updated <- mydata %>%
  mutate(clean_title_counts = if_else(clean_title %in% c("Yes"), "yes", "no"))

ggplot(mydata_updated, aes(x = clean_title_counts)) +
  geom_bar() +
  labs(title = "Distribution of clean_title Variable", x = "Contains Number", y = "Count")

#1.6
price_range <- c(16500, 17000)
mileage_range <- c(2000, 100000)

data_cleaned <- mydata %>%
   
  mutate(
    price = as.numeric(gsub("[\\$,]", "", price)),
    milage = as.numeric(gsub("[\\$, mi.]", "", milage)) 
  ) %>%
  filter(price >= price_range[1] & price <= price_range[2] & milage >= mileage_range[1] & milage <= mileage_range[2])
glimpse(data_cleaned)
ggplot(data_cleaned, aes(x = price, y = milage, color = factor(fuel_type))) +
  geom_point() +
  labs(
    title = "Scatter Plot of Price and Milage",  
    x = "Price",
    y = "Milage",  
    color = "Fuel Type"
  ) +
  facet_wrap(~fuel_type, scales = "free")
#1.7


glimpse(mydata)

summary(mydata)


#1.8
evals <- mydata %>%
  mutate(year_cluster = case_when(
    price <= 2010 ~ "old",
    price >= 2015 & price <= 2020 ~ "medium",
    price >= 2020 ~ "new"
  ))

glimpse(evals)