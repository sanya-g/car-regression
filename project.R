library(tidyverse)
library(moderndive)
library(GGally)
library(progress)

#cars = read_csv("cars.csv")
summary(cars)

cars$drivetrain = ifelse(cars$drivetrain == "FWD", "Front-wheel Drive", cars$drivetrain)
cars$drivetrain = ifelse(cars$drivetrain == "AWD", "All-wheel Drive", cars$drivetrain)
cars$drivetrain = ifelse(cars$drivetrain == "Four-wheel Drive", "All-wheel Drive", cars$drivetrain)
cars$drivetrain = ifelse(cars$drivetrain == "4WD", "All-wheel Drive", cars$drivetrain)
cars$drivetrain = ifelse(cars$drivetrain == "RWD", "Rear-wheel Drive", cars$drivetrain)



write.csv(cars, file = "cars2.csv", row.names=FALSE)
cars = read_csv("cars2.csv")

percentiles <- quantile(cars$price, c(0.005, 0.995))

upper_threshold = percentiles[2]  # 99th percentile
# Trim the dataset by removing data points outside the threshold range
cars = cars %>% filter(price >= 2 & price <= upper_threshold)
cars$percent_drop <- (cars$price - cars$price_drop) / cars$price * 100
cars <- cars[cars$percent_drop >= -100, ]
cars$percent_drop = ifelse(is.na(cars$percent_drop), 0, cars$percent_drop)

# Scatterplot: Price vs. Mileage
# Expectation: We expect to see a negative relationship between price and mileage, 
# indicating that cars with lower mileage tend to have higher prices. 
# This suggests that mileage is an important factor in determining car prices, 
# which can be useful for predicting prices and understanding market dynamics.
ggplot(cars, aes(x = mileage, y = price)) +
  geom_point() +
  labs(title = "Price vs. Mileage",
       x = "Mileage",
       y = "Price")

# Scatterplot: Price vs. MPG (city and highway)
# Expectation: We expect to see a positive relationship between price and MPG, 
# indicating that cars with higher fuel efficiency tend to have higher prices. 
# This suggests that fuel efficiency is an important factor in determining car prices, 
# which can be useful for predicting prices and understanding consumer preferences.
ggplot(cars, aes(x = mpg_c, y = price)) +
  geom_point() +
  labs(title = "Price vs. MPG (City)",
       x = "MPG (City)",
       y = "Price")

# Filter out cars with 0 MPG for highway (assuming this is a data cleaning step)
cars_nohighwayremoved <- filter(cars, mpg_h != 0)
ggplot(cars_nohighwayremoved, aes(x = mpg_h, y = price)) +
  geom_point() +
  labs(title = "Price vs. MPG (Highway)",
       x = "MPG (Highway)",
       y = "Price")

# Boxplot: Price by Drivetrain
# Expectation: We expect to see differences in car prices across different types of drivetrains. 
# This can provide insights into the relative value of each drivetrain type in the market, 
# which can be useful for investors and buyers.
# We see All-wheel Drives as the most valuable, followed closely by rear-wheel Drive. 
# Each has many outliers
ggplot(cars, aes(x = drivetrain, y = price)) +
  geom_boxplot() +
  labs(title = "Price by Drivetrain",
       x = "Drivetrain",
       y = "Price")

# Boxplot: Price by Fuel Type
# Expectation: We expect to see differences in car prices based on the type of fuel the car consumes. 
# This can provide insights into consumer preferences and the market demand for different fuel types.
# We see Diesel cars being more expensive by median, followed by hybrid then gas. Finally Flex fuels
# come in last.
ggplot(cars, aes(x = fuel_type, y = price)) +
  geom_boxplot() +
  labs(title = "Price by Fuel Type",
       x = "Fuel Type",
       y = "Price")

# Histogram: Distribution of Car Prices
# Expectation: We expect to see a right-skewed distribution of car prices, 
# with a peak at lower prices and a tail extending towards higher prices. 
# This provides insights into the typical price range in the market and the distribution of prices.
ggplot(cars, aes(x = price)) +
  geom_histogram(binwidth = 1000) +  # Adjust binwidth as needed
  labs(title = "Distribution of Car Prices",
       x = "Price",
       y = "Frequency")

# Histogram: Distribution of Mileage
# Expectation: We expect to see a right-skewed distribution of mileage values, 
# with a peak at lower mileage and a tail extending towards higher mileage. 
# This provides insights into the typical mileage range of cars in the market.
ggplot(cars, aes(x = mileage)) +
  geom_histogram(binwidth = 5000) +  # Adjust binwidth as needed
  labs(title = "Distribution of Mileage",
       x = "Mileage",
       y = "Frequency")

# Bar plot showing average price of cars by manufacturer
manufacturer_price <- cars %>%
  group_by(manufacturer) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))

ggplot(manufacturer_price, aes(x = reorder(manufacturer, -avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Price of Cars by Manufacturer",
       x = "Manufacturer",
       y = "Average Price")

# Scatter plot: Price by Seller & Driver Rating
# Expectation: We expect to see a right-skewed dataset, with higher-priced
# cars receiving higher driver and seller ratings. Correlation seems to be weaker.
ggplot(cars, aes(seller_rating, price, driver_rating)) +
  geom_point(0.5) +
  geom_smooth(method = "lm", se = FALSE)


# Boxplot: Price by Car Age
# Expectation: We expect to see a right-skewed distribution of age values, 
# with a peak at lower ages, similar to current market trends. We note an
# upward trend at higher ages, representing the market for older high-value cars.
cars$car_age <- as.factor(2024 - cars$year)
ggplot(cars, aes(car_age, price)) +
  geom_boxplot() 

intervals <- list(
  c(1990, 1995),
  c(1995, 2000),
  c(2005, 2010),
  c(2010, 2015),
  c(2015, 2020)
)

# Create separate plots for each interval
for (interval in intervals) {
  # Filter data for the current interval
  filtered_cars <- cars[cars$year >= interval[1] & cars$year <= interval[2], ]
  
  # Create scatter plot for the current interval
  plot_title <- paste("Trend of Car Prices", interval[1], "-", interval[2])
  plot(x = filtered_cars$year, y = filtered_cars$price, 
       main = plot_title, xlab = "Year", ylab = "Price", 
       pch = 16, col = "blue")
  abline(lm(price ~ year, data = filtered_cars), col = "red")
}

# Scatter plot showing relationship between engine size and car prices
# Observation: Greater engine sizes seem to be correlated with higher price
ggplot(cars, aes(x = engine, y = price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Engine Size vs. Price",
       x = "Engine Size",
       y = "Price") + 
  geom_smooth(method="lm", se = FALSE)

# Box plot showing distribution of car prices by transmission type
# Automatic cars seem to be more expensive than manual cars for the most part
ggplot(cars, aes(x = transmission, y = price)) +
  geom_boxplot() +
  labs(title = "Distribution of Car Prices by Transmission Type",
       x = "Transmission Type",
       y = "Price")

# Box plot comparing car prices with and without accidents or damage
# Naturally, no accidents is better for the car price. Even when it is indeterminate, 
# it seems to still be favored to accidentsyes, which makes sense
ggplot(cars, aes(x = as.factor(accidents_or_damage), y = price)) +
  geom_boxplot() +
  labs(title = "Comparison of Car Prices with and without Accidents or Damage",
       x = "Accidents or Damage",
       y = "Price")

# Scatter plot showing relationship between seller rating and car prices
# Better seller rating correlates with higher price... shocker
ggplot(cars, aes(x = seller_rating, y = price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Seller Rating vs. Price",
       x = "Seller Rating",
       y = "Price") + 
  geom_smooth(method="lm", se = FALSE)

# Scatter plot showing relationship between driver rating and car prices
# naturally, when a driver says the car is better, the price is higher.
ggplot(cars, aes(x = driver_rating, y = price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Driver Rating vs. Price",
       x = "Driver Rating",
       y = "Price") +
  geom_smooth(method="lm", se = FALSE)

# Scatter plot showing relationship between price drop and car prices
# Larger price drops are often correlated with a higher price. Will look into proportion
ggplot(cars, aes(x = price_drop, y = price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Price Drop vs. Price",
       x = "Price Drop",
       y = "Price") +
  geom_smooth(method="lm", se = FALSE)



# Scatter plot showing relationship between percentage drop and car prices
ggplot(cars, aes(x = percent_drop, y = price)) +
  geom_point(alpha = 0.5) +
  labs(title = "Percentage Drop vs. Price",
       x = "Percentage Drop",
       y = "Price")



# Define the list of predictor variables
predictor_vars <- c("manufacturer", "year", "mileage", "engine", "transmission", 
                    "drivetrain", "fuel_type", "mpg_c", "mpg_h", "accidents_or_damage", 
                    "driver_rating", "percent_drop", "one_owner", "personal_use_only", "seller_rating")

# Generate all possible combinations of predictor variables
predictor_combinations <- lapply(1:length(predictor_vars), function(x) combn(predictor_vars, x, simplify = FALSE))

# Flatten the list of combinations
all_combinations <- unlist(predictor_combinations, recursive = FALSE)

# Initialize variables to store the best model and its adjusted R-squared
best_model <- NULL
best_adj_r_squared <- -Inf
best_predictors <- NULL

# Create a progress bar
pb <- progress_bar$new(total = length(all_combinations))

# Fit models for all combinations of predictor variables
for (i in 1:length(all_combinations)) {
  combination <- all_combinations[[i]]
  formula <- paste("price ~", paste(combination, collapse = " + "))
  lm_model <- lm(formula, data = cars)
  adj_r_squared <- summary(lm_model)$adj.r.squared
  
  if (adj_r_squared > best_adj_r_squared) {
    best_model <- lm_model
    best_adj_r_squared <- adj_r_squared
    best_predictors <- combination
  }
  
  # Update progress bar
  pb$tick()
}

# Print the best model and its adjusted R-squared
print("Best Model:")
print(best_model)
print(paste("Adjusted R-squared:", best_adj_r_squared))
print(paste("Predictors:", paste(best_predictors, collapse = ", ")))

# Initialize variables to store the 10 best models and their adjusted R-squared per predictor
best_models <- vector("list", 10)
best_adj_r_squared_per_predictor <- rep(-Inf, 10)
best_predictors <- vector("list", 10)
pb <- progress_bar$new(total = length(all_combinations))
# Fit models for all combinations of predictor variables
for (i in 1:length(all_combinations)) {
  combination <- all_combinations[[i]]
  formula <- paste("price ~", paste(combination, collapse = " + "))
  lm_model <- lm(formula, data = cars)
  adj_r_squared <- summary(lm_model)$adj.r.squared
  
  # Calculate adjusted R-squared per predictor
  adj_r_squared_per_predictor <- adj_r_squared / length(combination)
  
  # Check if current model is among the 10 best models
  if (adj_r_squared_per_predictor > min(best_adj_r_squared_per_predictor)) {
    # Find the index of the model with the lowest adjusted R-squared per predictor
    idx <- which.min(best_adj_r_squared_per_predictor)
    # Replace the model at that index with the current model
    best_models[[idx]] <- lm_model
    best_adj_r_squared_per_predictor[idx] <- adj_r_squared_per_predictor
    best_predictors[[idx]] <- combination
  }
  
  # Update progress bar
  pb$tick()
}

# Print the 10 best models and their adjusted R-squared per predictor
print("Top 10 Models:")
for (i in 1:10) {
  print(paste("Model", i, ":"))
  print(best_models[[i]])
  print(paste("Adjusted R-squared per Predictor:", best_adj_r_squared_per_predictor[i]))
  print(paste("Predictors:", paste(best_predictors[[i]], collapse = ", ")))
}
#Takeaways: Mileage > Manufacturer > Year > Engine > MPG_H > MPG_C
summary(lm(price~mileage+manufacturer+year+engine+mpg_h+mpg_c, cars))

saveRDS(best_models, file = "best_models.rds")
best_models <- readRDS("best_models.rds")

##########
# Define the list of predictors
predictor_vars <- c("mileage", "manufacturer", "year", "engine", "mpg_h", "mpg_c")

# Generate all possible combinations of interaction terms (excluding manufacturer)
interaction_combinations_size_2 <- combn(predictor_vars, 2, FUN = function(x) paste(x, collapse = ":"))

# Generate all possible combinations of interaction terms of size 3
interaction_combinations_size_3 <- combn(predictor_vars, 3, FUN = function(x) paste(x, collapse = ":"))

interaction_combinations_size_4 <- combn(predictor_vars, 4, FUN = function(x) paste(x, collapse = ":"))

# Combine the interaction combinations of size 2 and size 3
interaction_combinations <- c(interaction_combinations_size_2, interaction_combinations_size_3, interaction_combinations_size_4)

# Create the formula for the model with main effects
main_formula <- as.formula(paste("price ~", paste(predictor_vars, collapse = " + ")))

# Initialize variables to store the best models
best_models <- list()
best_adj_r_squared <- rep(-Inf, 10)  # Initialize with negative infinity
best_model_names <- rep("", 10)

# Create a progress bar
pb <- progress_bar$new(total = length(interaction_combinations))

# Fit models with interaction terms
for (i in seq_along(interaction_combinations)) {
  interaction_formula <- as.formula(paste(main_formula, "+", interaction_combinations[i]))
  model <- lm(interaction_formula, data = cars)
  adj_r_squared <- summary(model)$adj.r.squared
  
  # Check if the current model has a higher adjusted R-squared than the lowest value in the list
  if (adj_r_squared > min(best_adj_r_squared)) {
    # Find the index of the lowest adjusted R-squared value
    idx <- which.min(best_adj_r_squared)
    # Replace the lowest value with the new adjusted R-squared
    best_adj_r_squared[idx] <- adj_r_squared
    # Store the new model in the corresponding position
    best_models[[idx]] <- model
    # Store the formula of the new model
    best_model_names[idx] <- as.character(interaction_formula)
  }
  
  # Update progress bar
  pb$tick()
}

print("Top 10 Models:")
for (i in 1:10) {
  print(paste("Model", i, ":"))
  print(best_models[[i]])
  print(paste("Adjusted R-squared per Predictor:", best_adj_r_squared[i]))
#  print(paste("Predictors:", best_model_names[i]))
}

tuples <- list(
  c("normal", 0.7149),
  c("mpg_h:mpg_c", 0.7196),
  c("engine:mpg_c", 0.7160),
  c("engine:mpg_h", 0.7160),
  c("year:mpg_c", 0.7233),
  c("year:mpg_h", 0.7273),
  c("year:engine", 0.7255),
  c("mileage:mpg_c", 0.7277),
  c("mileage:mpg_h", 0.7334),
  c("mileage:engine", 0.7319),
  c("mileage:year", 0.7253),
  c("manufacturer:mpg_c", 0.731),
  c("manufacturer:mpg_h", 0.7335),
  c("manufacturer:engine", 0.7302),
  c("manufacturer:year", 0.7466),
  c("mileage:manufacturer:year", 0.7591),
  c("manufacturer:year:engine", 0.7416),
  c("mileage:manufacturer:mpg_c", 0.7583),
  c("mileage:manufacturer:mpg_h", 0.7622),
  c("manufacturer:year:mpg_c", 0.7399),
  c("manufacturer:year:mpg_h", 0.7475),
  c("mileage:manufacturer:engine", 0.7529),
  c("mileage:manufacturer", 0.7506),
  c("mileage:manufacturer:year:mpg_c", 0.7582),
  c("mileage:manufacturer:year:engine", 0.7531),
  c("mileage:manufacturer:mpg_h:mpg_c", 0.7567),
  c("mileage:manufacturer:year:mpg_h", 0.7619),
  c("mileage:manufacturer:engine:mpg_c", 0.7533)
)

# Sort the list by the second value in each tuple
sorted_tuples <- tuples[order(sapply(tuples, "[", 2), decreasing = TRUE)]

# Print the sorted list
print(sorted_tuples)

summary(lm(price ~ mileage + manufacturer + year + engine + mpg_h + mpg_c + 
          mileage:manufacturer:mpg_h:mpg_c + 
          mileage:manufacturer:year:mpg_h + 
          mileage:manufacturer:year:mpg_c + 
          mileage:manufacturer:mpg_c + 
          mileage:manufacturer:mpg_h + 
          mileage:manufacturer + 
          manufacturer:year:mpg_c + 
          mileage:year:mpg_h + 
          mileage:manufacturer:year:engine, cars)) 
# R^2 of 0.8207
# Best interaction terms
best_interactions <- c(
  "mileage:manufacturer:mpg_h",
  "mileage:manufacturer:year:mpg_h",
  "mileage:manufacturer:car_age",
  "mileage:manufacturer:mpg_c",
  "mileage:manufacturer:year:mpg_c",
  "mileage:manufacturer:mpg_h:mpg_c",
  "mileage:manufacturer:engine:mpg_c",
  "mileage:manufacturer:year:engine",
  "mileage:manufacturer:engine",
  "mileage:manufacturer",
  "manufacturer:year:mpg_h",
  "manufacturer:year",
  "manufacturer:year:engine"
)

# Create the linear model formula
formula <- paste("price ~ ", paste(best_interactions, collapse = " + "), sep = "")

summary(lm(formula, cars))
#R^2 = 0.8341

example_car <- data.frame(
  mileage = 25000,
  manufacturer = "Audi",
  year = 2019,
  engine = "3.0L",
  mpg_h = 25,
  mpg_c = 19
) 


lm_model <- lm(price ~ mileage + manufacturer + year + engine + mpg_h + mpg_c, data = cars) #Basic model
lm_model <- lm(formula, cars)

predicted_price <- predict(lm_model, example_car)

print("Predicted Price: ")
predicted_price
