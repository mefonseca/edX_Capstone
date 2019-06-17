# Choose Your Own Project: Interstate Traffic Volume
# edX HarvardX: PH125.9x - Data Science: Capstone
# Author: Maria Eugenia Fonseca
# Date: June 2019

# Setting local
Sys.setlocale("LC_TIME", "English")

# Loading required packages
requiredPackages <- c("tidyverse", "lubridate", "caret", "R.utils", "knitr", "kableExtra", "tictoc")
lapply(requiredPackages, library, character.only = TRUE)

# Data Engineering

# Importing the data ------------------------------------------------------

temp <- tempfile()

download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00492/Metro_Interstate_Traffic_Volume.csv.gz", temp, mode = "wb")
try(gunzip(temp, "Metro_Interstate_Traffic_Volume.csv"))
metro <- read.csv("Metro_Interstate_Traffic_Volume.csv")
rm(temp)

glimpse(metro)

# Creating new features and fixing problems in the original dataset -------

metro2 <- metro %>%
  unique() %>%
  mutate(
    # Fixing the data. 2016-12-26 is not Christmas Day, 2016-12-25 is
    holiday = ifelse(date_time == "2016-12-26 00:00:00", "None",
                     ifelse(date_time == "2016-12-25 00:00:00", "Christmas Day", as.character(holiday))
    ),
    date = date(date_time),
    hour = factor(hour(date_time)),
    month = factor(month(date_time)),
    year = factor(year(date_time)),
    weekday = factor(weekdays(date),
                     levels = c(
                       "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
                     )
    ),
    weather_main = fct_recode(weather_main, "Other" = "Smoke", "Other" = "Squall")
  ) %>%
  group_by(date) %>%
  # If any hour of the day has the classification of a holiday, apply it to the rest of the day
  mutate(
    holiday = ifelse(any(!holiday == "No"), holiday[which(metro$holiday != "No")], "No"),
    is_holiday = ifelse(holiday == "None", "No", "Yes")
  ) %>%
  ungroup() %>%
  filter(
    # Removing strange observations
    temp != 0,               # 0 degrees in Kelvin is not a possible value 
    rain_1h != 9831.3) %>%   # 9831.3 mm of rain is not a possible value. The record is 305 mm/hour
  group_by(date_time) %>%
  # Fixing observations with more than 1 temperature
  mutate(temp = mean(temp),
         rain_1h = mean(rain_1h),
         clouds_all = mean(clouds_all)) %>%
  ungroup() %>%
  mutate(holiday_pre = factor(ifelse(! is_holiday[match(date+1, date)] %in% c("No", NA), "pre holiday", "No")),
         holiday_pos = factor(ifelse(! is_holiday[match(date-1, date)] %in% c("No", NA), "pos holiday", "No"))) %>%
  dplyr::select(- weather_main, -  weather_description) %>%
  unique()

# Exploratory Data Analysis -----------------------------------------------

# Histogram of traffic volume
metro2 %>%
  ggplot(aes(traffic_volume)) +
  geom_histogram(bins = 35, fill = "steelblue") +
  scale_x_continuous(breaks = seq(0, 7300, by = 1000)) +
  labs(title = "Histogram of traffic volume",
       x = "Traffic volume", y = "Count", fill = element_blank()) +
  theme_classic()

# Boxplot of traffic volume per weekday
metro2 %>%
  ggplot(aes(x = weekday, y = traffic_volume)) +
  geom_boxplot(fill = "steelblue", varwidth = T) + 
  labs(
    title = "Boxplot of traffic volume per weekday",
    x = "Weekday", y = "Traffic volume", fill = element_blank()
  ) +
  theme_classic()

# Boxplot of traffic volume if a day is a holiday or not
metro2 %>%
  ggplot(aes(y = traffic_volume, x = is_holiday)) +
  geom_boxplot(fill = "steelblue", varwidth = T) + 
  labs(
    title = "Boxplot of traffic volume",
    x = "Holiday", y = "Traffic volume", fill = element_blank()
  ) +
  theme_classic()

# Traffic volume per hour
metro2 %>%
  ggplot(aes(x = hour, y = traffic_volume)) +
  stat_summary(fun.y = mean, colour="steelblue", geom = "line", aes(group = 1), size = 1.5) + 
  labs(
    title = "Traffic volume per hour",
    x = "Hour", y = "Average hourly traffic volume", fill = element_blank()
  ) +
  theme_classic()


# Creating the train and test datasets ------------------------------------

metro_ml <- metro2 %>%
  select(holiday, temp, rain_1h, clouds_all,
         traffic_volume, hour, weekday, year, holiday_pre, holiday_pos)

metro_train <- metro_ml %>% filter(year != 2018) %>% select(-year) %>% droplevels()
metro_test  <- metro_ml %>% filter(year == 2018) %>% select(-year) %>% droplevels()

# Modeling Approaches -----------------------------------------------------

# Creating fit control for the models. A cross-validation with 3 folds
fitControl <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3 # with n folds
)

# Creating function to predict and mesure the model on the train and test data
predict_and_measure <- function(model, model_name, train_data, test_data, tm) {
  
  train_x <- train_data %>% select(- traffic_volume)
  train_y <- train_data %>% select(traffic_volume)  
  
  test_x <- test_data %>% select(- traffic_volume)
  test_y <- test_data %>% select(traffic_volume)  
  
  pred_train <- predict(model, train_x) 
  RMSE_train <- RMSE(obs = train_y , pred = pred_train)
  
  pred_test <- predict(model , test_x) 
  RMSE_test <- RMSE(obs = test_y , pred = pred_test)
  
  perf_grid = data.frame(Predictor = c(model_name),
                         "RMSE (train)" = c(round(RMSE_train, 2)),
                         "RMSE (test)" = c(round(RMSE_test, 2)),
                         "R squared (train)" = round(model$results$Rsquared[as.numeric(rownames(model$bestTune))], 2),
                         "Time(secs)" = round(tm, 2))
  
  perf_grid
}

# Linear model ------------------------------------------------------------
ptm <- proc.time()  
linearReg <- train(traffic_volume ~ .,
                   data = metro_train, 
                   method = "lm",
                   preProcess = c('center', 'scale'),
                   trControl = fitControl)   
tm <- proc.time() - ptm  
grid <- predict_and_measure(linearReg, 'Linear model', metro_train, metro_test, tm[[3]])


# xgbTree - default hyperparameters ---------------------------------------
ptm <- proc.time()
xgbTree_default <- train(traffic_volume ~.,
                         data = metro_train,
                         method = "xgbTree",
                         trControl = fitControl)
tm <- proc.time() - ptm
grid <- rbind(grid, predict_and_measure(xgbTree_default, 'xgbTree - Default', metro_train, metro_test, tm[[3]]))


# xgbTree - Step 1 --------------------------------------------------------
# Step 1: Number of Iterations and the Learning Rate
# Creating the grid for search
tune_grid <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = c(0.1, 0.2, 0.3, 0.4),
  max_depth = c(2, 3, 4, 5),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

ptm <- proc.time()
xgbTree_step1 <- train(traffic_volume ~.,
                       data = metro_train,
                       method = "xgbTree",
                       trControl = fitControl,
                       tuneGrid = tune_grid)

tm <- proc.time() - ptm
grid <- rbind(grid, predict_and_measure(xgbTree_step1, 'xgbTree - Step 1', metro_train, metro_test, tm[[3]]))

# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgbTree_step1)

# xgbTree - Step 2 --------------------------------------------------------
# Step 2: Maximum Depth and Minimum Child Weight
tune_grid2 <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = xgbTree_step1$bestTune$eta,
  max_depth = c(xgbTree_step1$bestTune$max_depth - 1, xgbTree_step1$bestTune$max_depth, xgbTree_step1$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(0.1, 0.25, 0.5),
  subsample = 1
)

ptm <- proc.time()
xgbTree_step2 <- train(traffic_volume ~.,
                       data = metro_train,
                       method = "xgbTree",
                       trControl = fitControl,
                       tuneGrid = tune_grid2)

tm <- proc.time() - ptm
grid <- rbind(grid, predict_and_measure(xgbTree_step2, 'xgbTree - Step 2', metro_train, metro_test, tm[[3]]))

tuneplot(xgbTree_step2)

# xgbTree - Step 3 --------------------------------------------------------
# Step 3: Subsample ratio of columns and subsample percentage

tune_grid3 <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = xgbTree_step1$bestTune$eta,
  max_depth = xgbTree_step2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = xgbTree_step2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

# xgbTree
ptm <- proc.time()
xgbTree_step3 <- train(traffic_volume ~.,
                       data = metro_train,
                       method = "xgbTree",
                       trControl = fitControl,
                       tuneGrid = tune_grid3)

tm <- proc.time() - ptm
grid <- rbind(grid, predict_and_measure(xgbTree_step3, 'xgbTree - Step 3', metro_train, metro_test, tm[[3]]))

tuneplot(xgbTree_step3)

# xgbTree - Step 4 --------------------------------------------------------
# Step 4: Gamma
tune_grid4 <- expand.grid(
  nrounds = seq(from = 100, to = 1000, by = 50),
  eta = xgbTree_step1$bestTune$eta,
  max_depth = xgbTree_step2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgbTree_step3$bestTune$colsample_bytree,
  min_child_weight = xgbTree_step2$bestTune$min_child_weight,
  subsample = xgbTree_step3$bestTune$subsample
)

ptm <- proc.time()
xgbTree_step4 <- train(traffic_volume ~.,
                       data = metro_train,
                       method = "xgbTree",
                       trControl = fitControl,
                       tuneGrid = tune_grid4)

tm <- proc.time() - ptm
grid <- rbind(grid, predict_and_measure(xgbTree_step4, 'xgbTree - Step 4', metro_train, metro_test, tm[[3]]))

# xgbTree - Step 5 --------------------------------------------------------
# Step 5: Reducing the Learning Rate
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgbTree_step3$bestTune$max_depth,
  gamma = xgbTree_step3$bestTune$gamma,
  colsample_bytree = xgbTree_step3$bestTune$colsample_bytree,
  min_child_weight = xgbTree_step3$bestTune$min_child_weight,
  subsample = xgbTree_step3$bestTune$subsample
)

ptm <- proc.time()
xgbTree_step5 <- train(traffic_volume ~.,
                       data = metro_train,
                       method = "xgbTree",
                       trControl = fitControl,
                       tuneGrid = tune_grid5)

tm <- proc.time() - ptm
grid <- rbind(grid, predict_and_measure(xgbTree_step5, 'xgbTree - Step 5', metro_train, metro_test, tm[[3]]))

tuneplot(xgbTree_step5)

# xgbTree - Final model ---------------------------------------------------
tune_grid_final <- expand.grid(
  nrounds = xgbTree_step5$bestTune$nrounds,
  eta = xgbTree_step5$bestTune$eta,
  max_depth = xgbTree_step5$bestTune$max_depth,
  gamma = xgbTree_step5$bestTune$gamma,
  colsample_bytree = xgbTree_step5$bestTune$colsample_bytree,
  min_child_weight = xgbTree_step5$bestTune$min_child_weight,
  subsample = xgbTree_step5$bestTune$subsample
)

tune_grid_final %>%
  kable() %>%
  kable_styling(latex_options="scale_down", bootstrap_options = "striped", full_width = F)  

# xgbTree - Final model
ptm <- proc.time()
(xgbTree_final <- train(traffic_volume ~.,
                        data = metro_train,
                        method = "xgbTree",
                        trControl = fitControl,
                        tuneGrid = tune_grid_final))

tm <- proc.time() - ptm
grid <- rbind(grid, predict_and_measure(xgbTree_final, 'xgbTree - Final model', metro_train, metro_test, tm[[3]]))

# Variable importance -----------------------------------------------------
importance <- varImp(xgbTree_final)
plot(importance, top = 20)
