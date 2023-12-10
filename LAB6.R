library(dplyr)
library(ggplot2)
library(openintro)
# modeling packages
library(caret)
library(rsample)

# model interpretability packages
library(vip)


used_cars = read.csv("C:\\Study\\DataSets\\used_cars.csv")


head(used_cars)

used_cars$price <- as.numeric(gsub("[^0-9.]", "", used_cars$price))
used_cars$milage <- as.numeric(gsub("[^0-9.]", "", used_cars$milage))



split <- initial_split(used_cars, prop = 0.5,
                       strata = 'price')
used_train <- training(split)
used_test <- training(split)
summary(used_train)
str(used_train)

set.seed(123)

model <- lm(price ~ milage, data = used_train)


summary(model)
sigma(model) #RMSE
sigma(model) #MSE


(model2 <- update(model, . ~ . + model_year))

model3 <- lm(price ~ ., data = used_train)


broom::tidy(model3)





set.seed(123)
(cv_model1 <- train(
  form = price ~ milage,
  data = used_train,
  method =  'lm',
  trControl = trainControl(method = 'cv', number = 10)
))


#model 2
set.seed(123)
cv_model2 <- train(
  price ~ milage + model_year,
  data = used_train,
  method = 'lm',
  trControl = trainControl(method = 'cv', number = 10)
)

#model 3 CV

#cv_model3 <- train(
 # price ~ .,
  #data = used_train,
  #method = 'lm',
  #trControl = trainControl(method = 'cv', number = 10)
#)

# extract out of sample performamce measures
summary(resamples(list(
  model1 = cv_model1,
  model2 = cv_model2
  )))


p1 <- ggplot(used_train, aes(model_year, price)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(se = F) +
  scale_y_continuous('Price', labels = scales::dollar,
                     breaks = seq(15000, 120000, by = 30000),
                     limits = c(15000, 120000)) +
  xlab('Model Year') +
  ggtitle(paste('Non-transformed variables with a\n','non-linear relationshop'))
print(p1)


p2 <- ggplot(used_train, aes(ext_col, price)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F) +
  scale_y_continuous('Price', labels = scales::dollar,
                     breaks = seq(15000, 120000, by = 30000),
                     limits = c(15000, 120000)) +
  xlab('Model Year') +
  ggtitle(paste('Transforming variables can provide a\n',
                'near-linear relationship'))

p2
color_counts <- table(used_train$ext_col)

# Sort colors by frequency in descending order
sorted_colors <- sort(color_counts, decreasing = TRUE)

# Extract top 10 colors
top_10_colors <- names(sorted_colors)[1:10]

# Subset the data for the top 10 colors
used_train_top_colors <- subset(used_train, ext_col %in% top_10_colors)

# Plot the data for the top 10 colors
p3 <- ggplot(used_train_top_colors, aes(ext_col, price)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F) +
  scale_y_continuous('Price', labels = scales::dollar,
                     breaks = seq(15000, 120000, by = 30000),
                     limits = c(15000, 120000)) +
  xlab('Exterior Color') +
  ggtitle('Top 10 Exterior Colors by Frequency')

p3
