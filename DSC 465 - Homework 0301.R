# Q1
food.srvc.by.county <- read.table("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 3\\Data File\\FoodSrvcByCounty.txt", header = TRUE, sep = "\t", quote = "")
food.srvc.by.county <- food.srvc.by.county[-1, ]
head(food.srvc.by.county)

## Q1.a
# extract data for states
states <- food.srvc.by.county[grep("^[A-Z ]+$", food.srvc.by.county$County), ]
states$FoodServiceAverage <- (states$FoodServices.97 + states$FoodServices.2002 + states$FoodServices.2007) / 3
states$State <- NULL # remove unnecessary column
colnames(states)[c(1)] <- c('State') # rename column
head(states)

library(dplyr)

# transform state names to lowercase for consistency
state.service <- states %>%
  mutate(state = tolower(State)) %>%
  data.frame()
head(state.service)

library(ggplot2)
library(mapproj)

# merge the dataset with map data
states_map <- map_data("state")
state_service_map <- left_join(states_map, state.service, by = c("region" = "state") )
head(state_service_map)

# plot average food service by state
ggplot(state_service_map, aes(x = long, y = lat, group = group, fill = FoodServiceAverage)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic") +
  ggtitle("Food Service Average by State") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Total")

## Q1.b
# extract data for counties
counties <- food.srvc.by.county[!grepl("^[A-Z ]+$", food.srvc.by.county$County), ]
counties$FoodServiceAverage <- (counties$FoodServices.97 + counties$FoodServices.2002 + counties$FoodServices.2007) / 3
head(counties)

# transform county names to lowercase for consistency
county.service <- counties %>%
  mutate(county = tolower(County)) %>%
  data.frame()
head(county.service)

# merge the dataset with map data
counties_map <- map_data("county")
county_service_map <- left_join(counties_map, county.service, by = c("subregion" = "county") )
head(county_service_map)

# plot average food service by county
ggplot(county_service_map, aes(x = long, y = lat, group = group, fill = FoodServiceAverage)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic") +
  ggtitle("Food Service Average by County") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Total")


# Q2
chicago.crashes <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 3\\Data File\\chicago_crashes.csv")
head(chicago.crashes)

## Q2.b
# visualize based on day of the week and hour of the day
ggplot(chicago.crashes, aes(x = factor(CRASH_DAY_OF_WEEK), y = factor(CRASH_HOUR)), na.rm = TRUE) +
  geom_bin2d() +
  scale_fill_gradient(low = "Orange red 1", high = "Orange red 4") +
  ggtitle("The Crashes in Chicago based on the Time of Day") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Day of Week") +
  ylab("Hour of Day")
  

# Q3
portland.water.level <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 3\\Data File\\PortlandWaterLevel2003.csv")
portland.water.level$Date <- as.Date(portland.water.level$Date, format = "%m/%d/%Y") # convert Date column to Date format
portland.water.level$Time <- format(as.POSIXct(portland.water.level$Time, format="%H:%M"), "%H:%M") # convert Time column to HH:MM format
head(portland.water.level)

## Q3.a
library(TTR)
library(dplyr)

portland.water.level$Number <- 1:nrow(portland.water.level)

# calculate moving averages for water level data
portland.water.level <- portland.water.level %>%
  group_by(Date) %>%
  mutate(
    MOV.Avg2 = runMean(WL, 2),
    MOV.Avg3 = runMean(WL, 3),
    MOV.Avg4 = runMean(WL, 4),
    MOV.Avg6 = runMean(WL, 6),
    MOV.Avg8 = runMean(WL, 8),
    MOV.Avg12 = runMean(WL, 12)
  )

# plot water level changes without moving average
ggplot(portland.water.level, aes(x = Number, y = WL)) + 
  geom_line(color = "Steel Blue") +
  ggtitle("Water Level Changes by 1 hour") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab(" ") +
  ylab("Water Level")

# plot water level changes with 2-hour moving average
ggplot(portland.water.level, aes(x = Number, y = MOV.Avg2)) + 
  geom_line(color = "Steel Blue") +
  ggtitle("Water Level Changes by 2 hours") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab(" ") +
  ylab("Water Level")

# plot water level changes with 3-hour moving average
ggplot(portland.water.level, aes(x = Number, y = MOV.Avg3)) + 
  geom_line(color = "Steel Blue") +
  ggtitle("Water Level Changes by 3 hours") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab(" ") +
  ylab("Water Level")

# plot water level changes with 4-hour moving average
ggplot(portland.water.level, aes(x = Number, y = MOV.Avg4)) + 
  geom_line(color = "Steel Blue") +
  ggtitle("Water Level Changes by 4 hours") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab(" ") +
  ylab("Water Level")

# plot water level changes with 6-hour moving average
ggplot(portland.water.level, aes(x = Number, y = MOV.Avg6)) + 
  geom_line(color = "Steel Blue") +
  ggtitle("Water Level Changes by 6 hours") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab(" ") +
  ylab("Water Level")

# plot water level changes with 8-hour moving average
ggplot(portland.water.level, aes(x = Number, y = MOV.Avg8)) + 
  geom_line(color = "Steel Blue") +
  ggtitle("Water Level Changes by 8 hours") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab(" ") +
  ylab("Water Level")

# plot water level changes with 12-hour moving average
ggplot(portland.water.level, aes(x = Number, y = MOV.Avg12)) + 
  geom_line(color = "Steel Blue") +
  ggtitle("Water Level Changes by 12 hours") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab(" ") +
  ylab("Water Level")


## Q4
# calculate average
average <- mean(portland.water.level$MOV.Avg12, na.rm =  TRUE)

# Define colors using HSV space
low_color <- hsv(0.15, 0.8, 0.9)
average_color <- hsv(0.5, 0.8, 0.9)
high_color <- hsv(0.9, 0.8, 0.9)

library(scales)

# plot water level changes with 12-hour moving average with custom divergent color palette
ggplot(portland.water.level, aes(x = Number, y = MOV.Avg12, color = MOV.Avg12)) +
  geom_line() +
  ggtitle("Water Level Changes by 12 hour") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab(" ") +
  ylab("Water Level") +
  scale_color_gradientn(colours = c(low_color, average_color, high_color),
                        values = rescale(c(min(portland.water.level$MOV.Avg12, na.rm = TRUE), average, max(portland.water.level$MOV.Avg12, na.rm = TRUE))),
                        name = "Water Level",
                        breaks = c(min(portland.water.level$MOV.Avg12, na.rm = TRUE), average, max(portland.water.level$MOV.Avg12, na.rm = TRUE)),
                        labels = c("Low", "Average", "High"))