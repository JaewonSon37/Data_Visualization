# Q1
earthquake <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Final Project\\Data File\\earthquake_1995-2023.csv")
earthquake$date_time2 <- as.POSIXct(earthquake$date_time, format = "%d-%m-%Y %H:%M")
earthquake$year <- format(earthquake$date_time2, "%Y")
head(earthquake)

library(dplyr)

earthquake_continent <- earthquake %>%
  filter(!is.na(continent) & continent != "") %>%
  select(year, continent)
head(earthquake_continent)

earthquake_contingency <- table(earthquake_continent$year, earthquake_continent$continent)
mosaicplot(earthquake_contingency, 
           main = "The Number of Earthquakes per Year by Continent", 
           las = 1, 
           cex = 0.6, 
           color = c('#2F4858', '#33658A', '#86BBD8', '#F9F871', '#F6AE2D','#F26419'))


# Q2
library(ggplot2)

plt <- ggplot(earthquake, aes(x = magnitude, y = sig)) +
  labs(title = "Contours Plot of Earthquake Magnitude and Significance",
       x = 'Magnitude',
       y = 'Significance')

plt +
  stat_density2d(aes(colour = ..level..))