library(dplyr)
library(openintro)
library(tidyverse)

comics <- read_csv("comic_characters.csv")

head(comics)

levels(comics$align)
levels(comics$sex)

contingency_table <- comics %>%
  count(sex, align) %>%
  pivot_wider(names_from = align, values_from = n)

print(contingency_table)

comics_filtered <- comics_filtered %>%
  filter(align != "Neutral")

comics_filtered$align <- factor(comics_filtered$align)
comics_filtered$sex <- factor(comics_filtered$sex)

levels(comics_filtered$align)
levels(comics_filtered$sex)

library(ggplot2)

barchart1 <- ggplot(comics_filtered, aes(x = align, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "Alignment", y = "Count", fill = "Gender") +
  ggtitle("Character Alignment by Gender")

barchart2 <- ggplot(comics_filtered, aes(x = sex, fill = align)) +
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", fill = "Alignment") +
  ggtitle("Gender Distribution by Alignment")

print(barchart1)
print(barchart2)

head(comics)
levels(comics$align)
levels(comics$sex)

contingency_table <- comics %>%
  count(sex, align) %>%
  pivot_wider(names_from = align, values_from = n)

filtered_data <- comics %>%
  filter(sex %in% c("Male Characters", "Female Characters")) %>%
  filter(align %in% c("Good Characters", "Neutral Characters", "Bad Characters"))

filtered_data$sex = factor(filtered_data$sex)
filtered_data$sex <- droplevels(filtered_data$sex)

print(filtered_data)

library(openintro)
data("cars93")

min_price <- min(cars93$price)
max_price <- max(cars93$price)

hist(cars93$price, breaks = seq(min_price, max_price, by = 1000), 
     main = "Histogram of Price",
     xlab = "Price",
     ylab = "Frequency",
     col = "blue")

hist(cars93$price, breaks = seq(min_price, max_price, by = 3000),
     main = "Histogram of Price (Binwidth = 30)",
     xlab = "Price",
     ylab = "Frequency",
     col = "green")

hist(cars93$price, breaks = seq(min_price, max_price, by = 6000),
     main = "Histogram of Price (Binwidth = 60)",
     xlab = "Price",
     ylab = "Frequency",
     col = "red")

boxplot(cars93$price,
        main = "Boxplot of Price",
        ylab = "Price")

cars_no_out <- subset(cars93, price < 40000)

boxplot(cars_no_out$price,
        main = "Boxplot of Price (Excluding Outliers)",
        ylab = "Price")

ggplot(data = cars93, aes(x = mpg_city)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  facet_wrap(~ type, ncol = 2) +
  labs(title = "Distribution of Fuel Efficiency by Vehicle Type",
       x = "Miles Per Gallon (City)",
       y = "Frequency")

filtered_data <- cars93 %>%
  filter(price < 30000)
plot <- filtered_data %>%
  ggplot(aes(x = drive_train)) +
  geom_bar() +
  labs(title = "Bar Chart of Car Count by Drive Train",
       x = "Drive Train",
       y = "Count")

print(plot)

used <- read_csv("C:\\Study\\DataSets\\used_cars.csv")
head(used$milage)
summary(used$milage)
summary(used$price)
used$milage <- as.numeric(gsub("[, mi]", "", used$milage))
used$price <- as.numeric(gsub("[^0-9]", "", used$price))


ggplot(used, aes(x = milage, fill = fuel_type)) +
  geom_histogram(binwidth = 5000) +
  labs(title = "Distribution of Mileage by Fuel Type",
       x = "Mileage",
       y = "Count") +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 300000, by = 50000)) +
  scale_fill_discrete(name = "Fuel Type") +
  theme_minimal()


ggplot(used, aes(x = price)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black") +
  labs(title = "Distribution of Price",
       x = "Price",
       y = "Frequency")


used$color <- ifelse(used$fuel_type == "Gasoline", "red",
                     ifelse(used$fuel_type == "Diesel", "blue", "green"))

ggplot(used, aes(x = fuel_type, fill = fuel_type)) +
  geom_bar() +
  scale_fill_discrete(name = "Fuel Type") +
  labs(title = "Count of Vehicles by Fuel Type",
       x = "Fuel Type",
       y = "Count")

ggplot(used, aes(x = fuel_type, fill = color)) +
  geom_bar() +
  labs(title = "Count of Vehicles by Fuel Type",
       x = "Fuel Type",
       y = "Count")

ggplot(used, aes(x = fuel_type)) +
  geom_bar(fill = "purple") +
  labs(title = "Count of Vehicles by Fuel Type",
       x = "Fuel Type",
       y = "Count")

ggplot(used, aes(y = price)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Boxplot of Price",
       y = "Price")

ggplot(used, aes(x = clean_title, fill = clean_title)) +
  geom_bar() +
  labs(title = "Distribution by Clean Title",
       x = "Clean Title",
       y = "Count") +
  theme_minimal()



head(used)
