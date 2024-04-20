# Labeling the Components
## Title
library(gcookbook)
library(ggplot2)

hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn)) +
  geom_point()

hw_plot + 
  labs(title = "Age and Height of Schoolchildren")

hw_plot +
  ggtitle("Age and Height of Schoolchildren", "11.5 to 17.5 years old")

hw_plot +
  ggtitle("Age and Height\nof Schoolchildren")

## Axes
hw_plot +
  labs(x = "Age in years", y = "Height in inches")

hw_plot +
  xlab("Age in years") +
  ylab("Height in inches")

hw_plot +
  scale_x_continuous(name = "Age in years")

## Title of Legend
pg_plot <- ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  stat_summary(fun.y = mean, geom = "bar")
pg_plot

pg_plot + labs(x = "Age in Years",
               y = "Height in Inches",
               title = "Weight per Group",
               fill = "Condition")

## Axis Ranges
pg_plot +
  ylim(0, 10)

pg_plot + 
  scale_y_continuous(limits = c(0, 10))

pg_plot +
  expand_limits(y = 0)

pg_plot +
  scale_x_discrete(limits = c("ctrl", "trt1"))

## Date Axes
library(scales) 
library(tidyverse)

econ_mod <- economics %>%
  filter(date >= as.Date("1992-05-01") & date < as.Date("1993-06-01"))

datebreaks <- seq(as.Date("1992-06-01"), as.Date("1993-06-01"), by = "2 month")

econ_plot <- ggplot(econ_mod, aes(x = date, y = psavert)) +
  geom_line()
econ_plot

econ_plot +
  scale_x_date(breaks = datebreaks, labels = date_format("%Y %b")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Themes
## Built-in Themes
hw_plot + theme_bw()

hw_plot + theme_minimal()

hw_plot + theme_classic()

hw_plot +
  theme_grey(base_size = 16)

## Adjustments with Themes
hw_plot +
  theme(axis.title.x = element_text(
    size = 16, lineheight = .9,
    face = "bold.italic", colour = "red"
  ))

hw_plot +
  ggtitle("Age and Height\nof Schoolchildren") +
  theme(plot.title = element_text(
    size = rel(1.5), lineheight = .9,
    face = "bold.italic", colour = "red"
  ))

hw_plot +
  theme(
    panel.grid.major = element_line(colour = "red"),
    panel.grid.minor = element_line(colour = "red", linetype = "dashed", size = 0.2),
    panel.background = element_rect(fill = "lightblue"),
    panel.border = element_rect(colour = "blue", fill = NA, size = 2)
  )

hw_plot +
  theme(
    legend.background = element_rect(fill = "grey85", colour = "red", size = 1),
    legend.title = element_text(colour = "blue", face = "bold", size = 14),
    legend.text = element_text(colour = "red"),
    legend.key = element_rect(colour = "blue", size = 0.25)
  )

hw_plot +
  ggtitle("Plot title here") +
  theme(
    axis.title.x = element_text(colour = "red", size = 14),
    axis.text.x = element_text(colour = "blue"),
    axis.title.y = element_text(colour = "red", size = 14, angle = 90),
    axis.text.y = element_text(colour = "blue"),
    plot.title = element_text(colour = "red", size = 20, face = "bold")
  )

hw_plot +
  facet_grid(sex ~ .) +
  theme(
    strip.background = element_rect(fill = "pink"),
    strip.text.y = element_text(size = 14, angle = -90, face = "bold")
  )


# Grids and Tick Marks
hw_plot +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

hw_plot +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

hw_plot +
  scale_x_continuous(breaks = c(12, 14, 15, 16, 18))

hw_plot +
  scale_x_continuous(breaks = seq(12, 18, by = .5))

pg_plot +
  scale_x_discrete(labels = c("Control", "T1", "T2"),
                   position = "top") 


# Legends
## Removing the Legend
pg_plot +
  guides(fill = FALSE)

pg_plot +
  scale_fill_discrete(guide = FALSE)

## Legend Position
pg_plot +
  theme(legend.position = "top")

## Order in Legend
pg_plot +
  scale_fill_discrete(limits = c("trt1", "trt2", "ctrl"))