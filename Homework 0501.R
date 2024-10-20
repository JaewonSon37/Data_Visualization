# Q1
perception_experiment <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 5\\Data File\\PerceptionExperiment.csv")
perception_experiment$Error <- perception_experiment$Response - perception_experiment$TrueValue
head(perception_experiment)

library(dplyr)

summary_perception_experiment <- perception_experiment %>%
  group_by(Test, Display, TestNumber) %>% 
  summarise(num_rows = n()) %>% 
  arrange(TestNumber)
summary_perception_experiment

## Q1.b
library(ggplot2)

perception_experiment$AbsoluteError <- abs(perception_experiment$Error)

ggplot(perception_experiment,aes(x = Test, y = AbsoluteError, color = Test)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Distribution of Absolute Error by Test",
       x = "Test",
       y = "Absolute Error",
       color = "Test") +
  theme_minimal()

ggplot(perception_experiment, aes(x = Test, y = AbsoluteError, fill = Test)) +
  geom_violin(trim = FALSE) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +
  labs(title = "Distribution of Absolute Errors by Test",
       x = "Test",
       y = "Absolute Error",
       fill = "Test") +
  theme_minimal()

## Q1.c
subject_from_56_to_73 <- subset(perception_experiment,
                                Subject >= 56 & Subject <= 73)

ggplot(subject_from_56_to_73,
       aes(x = factor(Display), y = AbsoluteError, fill = factor(Display))) +
  geom_boxplot() +
  labs(title = "The Response Patterns Between Display 1 and 2",
       x = "Display",
       y = "Absolute Error",
       fill = "Display") +
  theme_minimal()

library(ggbeeswarm)

ggplot(subject_from_56_to_73,
       aes(x = Test, y = AbsoluteError, color = factor(Display))) +
  geom_beeswarm(cex = 0.3) +
  labs(title = "The Response Patterns Between Display 1 and 2",
       x = "Test",
       y = "Absolute Error",
       color = "Display") +
  theme_minimal()

## Q1.d
subject_with_erroneous_stimulus <- subset(perception_experiment,
                                          Test == "Vertical Distance, Non-Aligned")
subject_from_59_to_64 <- subset(subject_with_erroneous_stimulus,
                                Subject >= 59 & Subject <= 64 & Display == 1)

ggplot(subject_with_erroneous_stimulus, aes(x = Trial, y = Response)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.3), alpha = 0.5) +
  geom_point(data = subject_from_59_to_64,
             aes(x = Trial, y = Response),
             color = "red",
             position = position_dodge(width = 0.1)) +
  theme_minimal()

# Q2
messier_data <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 5\\Data File\\MessierData.csv")
messier_data <- na.omit(messier_data)
messier_data$Distance <- log10(messier_data$Distance)
head(messier_data)

## Q2.a
par(mfrow = c(3, 2))

plot(messier_data$Messier, messier_data$Distance, 
     xlab = "Messier Number", ylab = "Distance (LY)")

plot(messier_data$Messier, messier_data$Distance, 
     type = "l", xlab = "Messier Number", ylab = "Distance (LY)")

plot(messier_data$Messier, messier_data$Size, 
     xlab = "Messier Number", ylab = "Angular Size (Minutes of Arc)")

plot(messier_data$Messier, messier_data$Size, 
     type = "l", xlab = "Messier Number", ylab = "Angular Size (Minutes of Arc)")

plot(messier_data$Messier, messier_data$Apparent.Magnitude, 
     xlab = "Messier Number", ylab = "Apparent Magnitude")

plot(messier_data$Messier, messier_data$Apparent.Magnitude, 
     type = "l", xlab = "Messier Number", ylab = "Apparent Magnitude")

## Q2.b
kind_and_distance <- messier_data %>%
  group_by(Kind) %>%
  summarise(mean_distance = mean(Distance)) %>%
  arrange(mean_distance)

messier_data$Kind <- factor(messier_data$Kind, levels = kind_and_distance$Kind)

ggplot(messier_data, aes(x = Kind, y = Distance, fill = Kind)) +
  geom_boxplot() +
  labs(title = "Distances by Kind",
       x = "Kind",
       y = "Distance (LY)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Q2.c
ggplot(messier_data, aes(x = Distance, y = Apparent.Magnitude)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "Dark Goldenrod") + 
  scale_y_reverse() +
  labs(title = "Apparent Magnitude by Distance",
       x = "Distance (LY)",
       y = "Apparent Magnitude\n(fainter <-> brighter)") +
  theme_minimal()

## Q2.d
ggplot(messier_data, aes(x = Distance, y = Apparent.Magnitude, size = Size)) +
  geom_point() +
  scale_y_reverse() + 
  labs(title = "Apparent Magnitude by Distance with Augmented Points Size",
       x = "Distance (LY)",
       y = "Apparent Magnitude\n(fainter <-> brighter)",
       size = "Angular Size") +
  theme_minimal()


# Q3
montana_population_data <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 5\\Data File\\MontanaPopulationData.csv")
head(montana_population_data)

## Q3.a
library(scales)

ggplot(montana_population_data, aes(x = Year, y = Population)) +
  geom_line() + 
  geom_hline(yintercept = montana_population_data$Population[1] * 2^1,
             linetype = "dashed",
             color = "red") +
  geom_hline(yintercept = montana_population_data$Population[1] * 2^2,
             linetype = "dashed",
             color = "red") +
  scale_y_continuous(trans = "log2",
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  labs(title = "Population Change in Montana",
       x = "Year",
       y = "Population (Log2)") +
  theme_minimal()

## Q3.b
montana_population_data$ChangeofPercentageRate <- c(NA, diff(montana_population_data$Population) / head(montana_population_data$Population, -1) * 100)

ggplot(montana_population_data, aes(x = Year, y = ChangeofPercentageRate)) +
  geom_line(size = 1) + 
  geom_smooth(method = "lm", se = FALSE, color = "Tomato", linetype = "dashed") + 
  labs(title = "Population Change Rate in Montana",
       x = "Year",
       y = "Change Rate (%)") +
  theme_minimal()

greatest_increase <- max(montana_population_data$ChangeofPercentageRate, na.rm = TRUE)
greatest_increase

## Q3.c
increase_greater_than_15 <- subset(montana_population_data, ChangeofPercentageRate >= 15)

ggplot(montana_population_data, aes(x = Year, y = ChangeofPercentageRate)) +
  geom_line(size = 1) + 
  geom_point(data = increase_greater_than_15,
             aes(x = Year, y = ChangeofPercentageRate),
             color = "Dodger Blue", size = 3) + 
  geom_text(data = increase_greater_than_15,
            aes(x = Year, y = ChangeofPercentageRate,
                label = round(ChangeofPercentageRate, 2)), 
            hjust = - 0.5, color = "Dodger Blue") +
  geom_smooth(method = "lm", se = FALSE, color = "Tomato", linetype = "dashed") + 
  labs(title = "Population Change Rate in Montana",
       x = "Year",
       y = "Change Rate (%)") +
  theme_minimal()  
  

# Q4
air_quality <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 5\\Data File\\AirQuality.csv")
head(air_quality)

## Q4.a
ggplot(air_quality, aes(x = Wind, y = Solar.R)) +
  geom_point() +
  geom_smooth(method = "lm", color = "Tomato", linetype = "dashed") +
  labs(title = "Relationship Between Wind Speed and Solar Radiation",
       x = "Wind Speed",
       y = "Solar Radiation") +
  theme_minimal()

## Q4.b
library(tidyr)

air_quality2 <- air_quality %>%
  gather(key = "Variable", value = "Value", Wind, Solar.R)

ggplot(air_quality2, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap( ~ Variable, scales = "free") +
  labs(title = "Distributions of Wind Speed and Solar Radiation",
       x = "Value",
       y = "Density") +
  theme_minimal()

## Q4.c
air_quality3 <- air_quality %>%
  pivot_longer(-c(Month, Day),
               names_to = "Measurement",
               values_to = "Value")

ggplot(air_quality3, aes(x = Value, fill = Measurement)) +
  geom_density(alpha = 0.5) +
  facet_wrap(. ~ Measurement, scales = "free") +
  labs(title = "Comparison of Multiple Distributions",
       x = "Value",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = rainbow(length(unique(air_quality3$Measurement))))

## Q4.d
library(gridExtra)

qq_wind <- ggplot(air_quality, aes(sample = Wind)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "QQ Plot for Wind",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

qq_solar <- ggplot(air_quality, aes(sample = Solar.R)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "QQ Plot for Solar Radiation",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

grid.arrange(qq_wind, qq_solar, ncol = 2)
