library(tidyverse)
library(openintro)
library(dplyr)
library(gapminder)
data("gapminder")
gap2007 <- gapminder %>% filter(year == 2007)
continent_life_summary <- gap2007 %>%
  group_by(continent) %>%
  summarize(mean_life_expectancy = mean(lifeExp), median_life_expectancy = median(lifeExp))

print(continent_life_summary)

gap2007 %>%
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(title = "Life Expectancy by Continent in 2007", x = "Continent", y = "Life Expectancy")

spread_summary <- gap2007 %>%
  group_by(continent) %>%
  summarize(sd_life_expectancy = sd(lifeExp), IQR_life_expectancy = IQR(lifeExp), count = n())

print(spread_summary)

gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Life Expectancy by Continent in 2007", x = "Life Expectancy")

americas_summary <- gap2007 %>%
  filter(continent == "Americas") %>%
  summarize(mean_life_expectancy = mean(lifeExp), sd_life_expectancy = sd(lifeExp))

print(americas_summary)

gap2007 %>%
  ggplot(aes(x = pop)) +
  geom_density() +
  labs(title = "Density Plot of Population (Before Transformation)", x = "Population")

gap2007 <- gap2007 %>%
  mutate(log_pop = log(pop))

gap2007 %>%
  ggplot(aes(x = log_pop)) +
  geom_density() +
  labs(title = "Density Plot of Log-Transformed Population", x = "Log Population")

gap2007 %>%
  filter(continent == "Asia") %>%
  mutate(is_outlier = if_else(lifeExp < 50, TRUE, FALSE)) %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = "", y = lifeExp)) +
  geom_boxplot() +
  labs(title = "Life Expectancy in Non-Outlier Asian Countries", x = "", y = "Life Expectancy")

library(tidyverse)

used_cars <- read_csv("C:\\Study\\DataSets\\used_cars.csv")

head(used_cars)

used_cars <- used_cars %>%
  mutate(milage_numeric = as.numeric(gsub("[^0-9]", "", milage)))

used_cars <- used_cars %>%
  filter(fuel_type != "" & !is.na(fuel_type))

used_cars <- used_cars %>%
  mutate(price_cleaned = as.numeric(gsub("[^0-9.]", "", price)))

used_cars <- used_cars %>%
  filter(!is.na(price_cleaned))

price_summary <- used_cars %>%
  group_by(fuel_type) %>%
  summarize(mean_price = mean(price_cleaned), median_price = median(price_cleaned, na.rm = TRUE))

print(price_summary)
fuel_colors <- price_summary %>%
  distinct(fuel_type) %>%
  mutate(color = scales::brewer_pal(palette = "Set1")(n())) %>%
  select(fuel_type, color)

# Merge fuel colors with price_summary
price_summary <- left_join(price_summary, fuel_colors, by = "fuel_type")

# Plot Mean Price by Fuel Type with color mapping
ggplot(price_summary, aes(x = fuel_type, y = mean_price, fill = fuel_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by Fuel Type", x = "Fuel Type", y = "Mean Price") +
  scale_fill_manual(values = price_summary$color)  # Use the color palette

# Plot Median Price by Fuel Type with color mapping
ggplot(price_summary, aes(x = fuel_type, y = median_price, fill = fuel_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Median Price by Fuel Type", x = "Fuel Type", y = "Median Price") +
  scale_fill_manual(values = price_summary$color) 
count_by_model_year <- used_cars %>%
  group_by(model_year) %>%
  summarize(count = n())

print(count_by_model_year)

ggplot(count_by_model_year, aes(x = model_year, y = count, fill = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Vehicles by Model Year", x = "Model Year", y = "Count") +
  scale_fill_gradient(low = "lightblue", high = "purple") +  # Gradient fill colors
  theme(
    text = element_text(size = 14),  # Adjust text size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )
used_cars <- used_cars %>%
  mutate(log_price = log(price_cleaned))

log_price_summary <- used_cars %>%
  group_by(accident) %>%
  summarize(mean_log_price = mean(log_price))

print(log_price_summary)

ggplot(log_price_summary, aes(x = accident, y = mean_log_price, fill = accident)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Log Price by Accident Status", x = "Accident Status", y = "Mean Log Price") +
  scale_fill_manual(values = c("blue", "green", "red"))



donut_chart_accident <- ggplot(accident_summary, aes(x = 1, y = count, fill = accident)) +
  geom_bar(stat = "identity", width = 0.4) +
  coord_polar("y") +
  labs(title = "Donut Chart by Accident Type") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round((count/sum(count)) * 100), "%")),
            position = position_stack(vjust = 0.5))

print(donut_chart_accident)

filtered_data <- grouped_data %>%
  filter(milage >= 0, milage <= 100000)
grouped_test <- filtered_data  %>% filter(accident == "At least 1 accident or damage reported")
glimpse(grouped_test)
# Постройте график для отфильтрованных данных
ggplot(data = filtered_data, aes(x = model_year, y = milage, color = fuel_type)) +
  geom_line() 



ggplot(used_cars, aes(x = brand, y = milage_numeric, label = milage_numeric)) +
  geom_bar(stat = 'identity', aes(fill = accident), width = 0.5) +
  
  scale_fill_manual(name = "Mileage",
                    labels = c("None reported", "At least 1 accident","Nothing"),
                    values = c("None reported" = "#f8766d", "At least 1 accident or damage reported" = "#00ba38")) +
  
  coord_flip()


popular_colors <- filtered_data %>%
  group_by(ext_col) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:5)


popular_int_colors <- filtered_data %>%
  group_by(int_col) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:5)

ggplot(popular_int_colors, aes(x = int_col, y = count, fill = int_col)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    subtitle = "Grouped Bar Chart by Car Interior Color (Top 5 Colors)",
    title = "Car Color Distribution (Top 5 Colors)"
  ) +
  theme(legend.position = "none") +
  coord_flip()

# Создайте столбчатый график с ограниченными цветами
ggplot(popular_colors, aes(x = ext_col, y = count, fill = ext_col)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    subtitle = "Grouped Bar Chart by Car Exterior Color (Top 5 Colors)",
    title = "Car Color Distribution (Top 5 Colors)"
  ) +
  theme(legend.position = "none") +
  coord_flip()
ggplot(popular_colors, aes(x = ext_col, y = count, fill = ext_col)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    subtitle = "Grouped Bar Chart by Car Exterior Color (Top 5 Colors)",
    title = "Car Color Distribution (Top 5 Colors)"
  ) +
  theme(legend.position = "none") +
  coord_flip()

ggplot(popular_colors, aes(x = ext_col, y = count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(aes(color = ext_col), size = 2) +
  theme_minimal() +
  labs(
    subtitle = "Line Chart by Car Exterior Color (Top 5 Colors)",
    title = "Car Color Distribution (Top 5 Colors)"
  ) +
  theme(legend.position = "none") +
  coord_flip()



