# Data Frames
mc <- as.data.frame(mtcars)
head(mc)

summary(mc)

names(mc)

names(mc)[1]

names(mc)[1] <- "Miles.Per.Gallon"

names(mc)

mc$cyl

mc$cyl < 5

mc <- mc[mc$cyl == 4 & mc$Miles.Per.Gallon > 18, c("cyl", "Miles.Per.Gallon", "hp", "wt")]
head(mc)

mc$mpgPerWeight <- mc$Miles.Per.Gallon / mc$wt
head(mc)


# Tidyverse
library(tidyverse)

mycars <- as.data.frame(mtcars)

mycars <- filter(mycars, cyl == 4 & mpg > 18)
mycars <- select(mycars, cyl, mpg, hp, wt)
mycars <- mutate(mycars, mgpPerWeight = mpg / wt)
head(mycars)

## Pipes
mycars2 <- mtcars %>%
  filter(cyl == 4 & mpg >18) %>%
  select(cyl, mpg, hp, wt) %>%
  mutate(mgpPerWeight = mpg /wt)
head(mycars2)

## Discretizing
mycars %>%
  mutate(hpfactor = cut(hp,
                        breaks = c(-Inf, 120, 200, Inf),
                        labels = c("low", "medium", "high"))) %>%
  head()

## Group and Summarize
library(hflights)

data("hflights")
flights = hflights

flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm = TRUE))

flights %>%
  group_by(Dest) %>%
  select(Dest, Cancelled) %>%
  table() %>%
  head()

## Pivoting
EuStock <- as.data.frame(EuStockMarkets) %>%
  mutate(day = 1:n()) %>%
  pivot_longer(-day, names_to = "Index", values_to = "value")
head(EuStock)


# Basic Graph Types
## Line Graph
p <- ggplot(EuStock, aes(day, value))
p + geom_line()

ggplot(filter(EuStock, Index == "DAX"), aes(day, value)) +
  geom_line()

p + geom_line(aes(colour = Index))

p + geom_line() + facet_wrap( ~ Index)

p + geom_line(aes(colour = Index, size = day))

## Bar Chart
### Factors
mycars3 <- mtcars %>%
  mutate(gear = factor(gear,
                       levels = c(3, 4, 5),
                       labels = c("3gear", "4gear", "5gear"))) %>%
  mutate(cyl = factor(cyl,
                      levels = c(4, 6, 8),
                      labels = c("4cyl", "6cyl", "8cyl")))
head(mycars3)

### Dodging
ggplot(diamonds, aes(x = clarity)) +
  geom_bar()

ggplot(diamonds, aes(x = clarity, fill = cut)) +
  geom_bar()

p <- ggplot(diamonds, aes(x = clarity, fill = cut))
p + geom_bar(position = "stack")

p + geom_bar(position = "dodge")

p + geom_bar(position = "fill")

p + geom_bar(position = "identity")

ggplot(diamonds, aes(x = clarity, y = price)) +
  geom_bar(stat = "identity")

ggplot(diamonds, aes(x = clarity, y = price)) + 
  geom_col()

p <- ggplot(diamonds, aes(x = clarity, y = price))
p + stat_summary(fun.y = "sum", geom = "bar")

p + stat_summary(fun.y = "median", geom = "bar")

ggplot(diamonds, aes(x = fct_reorder(clarity, desc(price)), y = price)) +
  stat_summary(fun.y = "median", geom = "bar")

diamonds %>%
  mutate(clarity = fct_reorder(clarity, desc(price))) %>%
  ggplot(aes(clarity, price)) +
  stat_summary(fun.y = "median", geom = "bar")

## Contingency Plot
ggplot(mtcars, aes(x = cyl, y = gear, fill = wt)) +
  geom_tile()

ggplot(mtcars, aes(x = factor(cyl), y = factor(gear))) +
  geom_bin2d()

## Treemap
library(ggplot2)
library(treemapify)
library(treemap)
library(dplyr)

data("GNI2014")
head(GNI2014)

ggplot(GNI2014, aes(area = GNI, fill = population, label = country, subgroup = continent)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.9,
                             colour = "White", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "Red", place = "topleft", reflow = T)