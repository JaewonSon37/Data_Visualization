# Color
## Specifying Individual Colors
library(gcookbook)
library(ggplot2)

ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point(size = 3, color = hsv(0.8, 0.8, 0.7))

## Continuous Scales
hwplot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, color = weightLb)) +
  geom_point(size = 3)
hwplot + scale_color_gradient()

### Built-Ins
hwplot + 
  scale_colour_distiller(palette = "BuPu")

hwplot + 
  scale_color_viridis_c()

### Custom Gradient Colors
hwplot + 
  scale_color_gradient(low = "black", high = hsv(0.25, 0.75, 0.9))

### Custom Divergent Palette
hwplot +
  scale_colour_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 100.5,
    space= "Lab",
    na.value = "grey50"
  )

library(scales)

hwplot +
  scale_colour_gradient2(
    low = muted("red"),
    mid = "white",
    high = muted("blue"),
    midpoint = 100.5,
    space = "Lab",
    na.value = "grey50" 
  )

## Discrete Color Palettes
uspopage_plot <- ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area()
uspopage_plot

uspopage_plot +
  scale_fill_viridis_d()

uspopage_plot +
  scale_fill_brewer(palette = "Pastel1")

uspopage_plot +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  theme_dark()

### Manual Discrete Color Palettes
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
  geom_point(size = 3)
pal <- c("8" = "red", "4" = "blue", "6" = "darkgreen")
p + scale_colour_manual(values = pal)


# Geographic Data
## Glyph Maps
statesmap = map_data('state')
head(statesmap)

ggplot(statesmap, aes(long, lat, group = group)) + 
  geom_polygon(colour = 'black', fill = NA)

library(crimedata)
library(tidyverse)

crimes <- get_crime_data(years = c(2018), type = "sample", output = "tbl")
crimes <- crimes %>%
  select(-c("uid", "offense_code", "census_block", "date_start", "date_end"))
head(crimes)

ggplot() +
  geom_polygon(data = statesmap,
               aes(x = long, y = lat, group = group),
               colour = 'black',
               fill = NA) +
  geom_point(data = crimes,
             aes(longitude, latitude, color = offense_against),
             size = 3)

## Choropleth
crimes <- USArrests %>%
  rownames_to_column() %>%
  mutate(state = tolower(rowname)) %>%
  select(-c(rowname)) %>%
  data.frame()
head(crimes)

states_map <- map_data("state")
crime_map <- left_join(states_map, crimes,
                       by = c("region" = "state") )
head(crime_map)

library(mapproj)

ggplot(crime_map, 
       aes(x = long, y = lat, group = group, fill = Assault)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic")

ggplot(crimes, aes(map_id = state, fill = Assault)) +
  geom_map(map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic")


# Time series
## Dense Time series Heat maps
crashes <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 4\\Data File\\chicago_crashes.csv")

ggplot(crashes, aes(x = CRASH_DAY_OF_WEEK, y = CRASH_HOUR, fill = INJURIES_TOTAL),
       na.rm = TRUE) +
  geom_tile()

ggplot(crashes, aes(INJURIES_TOTAL)) + 
  geom_histogram(bins = 10)

summary(crashes$INJURIES_TOTAL)

ggplot(crashes, aes(x = factor(CRASH_DAY_OF_WEEK), y = factor(CRASH_HOUR)),
       na.rm = TRUE) +
  stat_summary_2d(fun = "mean", aes(z = INJURIES_TOTAL), geom = "tile")

ggplot(crashes, aes(x = factor(CRASH_DAY_OF_WEEK), y = factor(CRASH_HOUR)),
       na.rm = TRUE) + 
  geom_bin2d()

## Inflation Adjustments
CPITable <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 4\\Data File\\CPITable.csv")
head(CPITable)

cpiLookup <- function(y) { 
  return(CPITable %>% 
           filter(Year %in% y) %>%
           select(Value) %>% 
           .$Value ) }

cpiLookup(2010)

cpiLookup(c(2010, 2015))

typeof(cpiLookup(c(2010, 2015)))

moneyOverTime = tibble(y = c(2015, 2016, 2017, 2018), x = c(100, 100, 100, 100))
head(moneyOverTime)

moneyOverTime <- moneyOverTime %>% 
  mutate(adjustedX = x * cpiLookup(max(.$y)) / cpiLookup(y))
moneyOverTime

## Dealing with Dates
priceIndex <- read.csv("C:\\Users\\wodnj\\OneDrive\\바탕 화면\\Data Visualization\\DSC 465 - Week 4\\Data File\\ConsumerPriceIndex.csv")
head(priceIndex)

cIndex <- priceIndex %>%
  select(-c("Avg.", "Dec.1")) %>%
  pivot_longer(-c(Year, Avg), names_to = "Month", values_to = "value")
head(cIndex)

cIndexDateStr <- cIndex %>%
  mutate(date_string = paste(Year, Month, "01", sep = "-"))
head(cIndexDateStr)

altCIdx <- cIndex %>%
  unite("date_string", c(Year, Month), sep = "-")
head(altCIdx)

library(lubridate)

cIndex <- cIndexDateStr %>%
  mutate(Date = as_date(date_string, "%Y-%b-%d"))
head(cIndex)

timeplot <- ggplot(cIndex, aes(Date, value)) + 
  geom_line() +
  theme_bw()
timeplot

cIdxWD <- cIndex %>%
  mutate(weekday = wday(Date))
head(cIdxWD)

cIdxWD %>% 
  filter(Date > ymd(20060601), Date < ymd("2008-12-31")) %>% 
  ggplot(aes(x = year(Date) %>% factor(),
             y = month(Date) %>% factor()),
         na.rm = TRUE) +
  stat_summary_2d(fun = "median", aes(z = value), geom = "tile")

cIdxWD %>%
  filter(month(Date) <= 9, month(Date) >= 3) %>% 
  filter(year(Date) >= 2006, year(Date) <= 2008) %>%
  ggplot(aes(x = month(Date) %>% factor(),
             y = value),
         na.rm = TRUE) +
  facet_wrap( ~ year(Date)) +
  geom_col()

## Polar Coordinates
timeplot + 
  coord_polar()