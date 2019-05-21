# Choose Your Own Project
# edX HarvardX: PH125.9x - Data Science: Capstone
# Author: Maria Eugenia Fonseca
# Date: May 2019

# Loading required packages
requiredPackages <- c("tidyverse", "lubridate", "caret", "R.utils")
lapply(requiredPackages, library, character.only = TRUE)

# 
Sys.setlocale("LC_TIME", "English")


# Importing data ----------------------------------------------------------

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00492/Metro_Interstate_Traffic_Volume.csv.gz", temp, mode = "wb")
gunzip(temp, "Metro_Interstate_Traffic_Volume.csv")
metro <- read.csv("Metro_Interstate_Traffic_Volume.csv")

rm(temp)


glimpse(metro)
summary(metro)

metro <- metro %>%
  mutate(date = date(date_time),
         hour = factor(hour(date_time)),
         month = factor(month(date_time)),
         year = factor(year(date_time)),
         weekday = factor(weekdays(as_date(date_time)), 
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         is_holiday = ifelse(holiday == "None", "No", "Yes")) %>%
  select(- date_time)

# Descriptive analysis ----------------------------------------------------

hist(metro$traffic_volume)

metro %>%
  group_by(weekday) %>%
  summarise(mean(traffic_volume))

metro2 %>%
  ggplot(aes(x = weekday, y = traffic_volume)) +
  geom_boxplot(fill = "steelblue", varwidth = T) + 
  labs(
    title = "Boxplot of traffic volume per weekday",
    x = "Weekday", y = "Traffic volume", fill = element_blank()
  ) +
  theme_classic()


# Algumas datas tem poucos dados. O feriado só acontece 1 vez ao ano. Agregar feriados?
table(metro2$holiday)

metro2 %>%
  ggplot(aes(y = traffic_volume, x = is_holiday)) +
  geom_boxplot(fill = "steelblue", varwidth = T) + 
  labs(
    title = "Boxplot of traffic volume",
    x = "Holiday", y = "Traffic volume", fill = element_blank()
  ) +
  theme_classic()


metro2 %>%
  ggplot(aes(x = year, y = traffic_volume)) +
  geom_boxplot(fill = "steelblue", varwidth = T) + 
  labs(
    title = "Boxplot of traffic volume per year",
    x = "Year", y = "Traffic volume", fill = element_blank()
  ) +
  theme_classic()


metro3 <- metro2 %>%
  group_by(date) %>%
  summarise(a = mean(traffic_volume))

metro1 %>%
  ggplot(aes(x = date, y = a)) +
  geom_line(color = "#00AFBB", size = 1) + 
  stat_smooth(
    color = "#FC4E07", fill = "#FC4E07",
    method = "loess"
  )


# Time series effect? Traffic on time t is related to the traffic on time t - 1?

# Analysis section --------------------------------------------------------
metro_ml <- metro %>%
  select(temp, rain_1h, snow_1h, clouds_all, weather_main, weather_description, 
         traffic_volume, hour, weekday, is_holiday)

mat <- lapply(c('rf'),
              function (met) {
                train(traffic_volume~., method=met, data=metro_ml)
              })

# Changing characters variables to numeric factors. It saves memory 
metro_ml2 <- metro_ml %>%
  mutate(weather_main = as.factor(as.numeric(weather_main)),
         weather_description = as.factor(as.numeric(weather_description)),
         weekday = as.factor(as.numeric(weekday)),
         is_holiday = as.factor(ifelse(is_holiday == "No", 0, 1)))



library(randomForest)

# Create a Random Forest model with default parameters
model1 <- randomForest(traffic_volume ~ ., data = metro_ml2[1:2000,], importance = TRUE)
model1

model2 <- lm(traffic_volume ~ ., data = metro_ml2)
summary(model2)

cor(metro$traffic_volume[1:48203], metro$traffic_volume[2:48204])


# trend:
  # Yearly (per month)
  # Weekly
  # Daily (per hour)




# Results section ---------------------------------------------------------
