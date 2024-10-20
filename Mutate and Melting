# qplot
library(dplyr)
library(mosaic)
library(lubridate)
library(ggplot2)

Births <- mutate(Births78,
                 date = ymd(date),
                 wd = wday(date),
                 wday = wday(date, label = TRUE, abbr = TRUE))
head(Births, 5)

qplot(substance, data = HELPrct)

qplot(i2, data = HELPrct)

qplot(date, births, data = Births)

qplot(sex, substance, data = HELPrct)

ggplot(data = Births, aes(x = date, y = births, color = wday))

ggplot(data = Births, aes(x = date, y = births, color = wday)) +
  geom_point()

ggplot(data = Births, aes(x = date, y = births, color = wday)) +
  geom_line()

ggplot(data = HELPrct, aes(x = age, color = substance)) +
  geom_density()

ggplot(data = HELPrct, aes(x = age, color = substance)) +
  stat_density(alpha = 0.5)

ggplot(data = HELPrct, aes(x = age, fill = substance)) +
  stat_density(alpha = 0.5)

ggplot(data = HELPrct, aes(x = age, fill = substance)) +
  stat_density(geom = "density", alpha = 0.5)

ggplot(dat = HELPrct, aes(x = sex, y = age)) +
  geom_boxplot(outlier.size = 0) +
  geom_jitter(alpha = 0.5) +
  coord_flip()


# Melting Data
library(ggplot2)
library(reshape2)

head(EuStockMarkets)

ds = as.data.frame(EuStockMarkets)
ds$day = seq(nrow(ds))
head(ds)

EuStock = melt(ds, id = c("day"))
names(EuStock)[2] = "Index"
head(EuStock)

ggplot(EuStock, aes(day, value, color = Index)) +
  geom_line()

ggplot(EuStock, aes(day, value, color = Index)) +
  geom_line(size = 3)

ggplot(EuStock, aes(day, value, color = Index)) +
  geom_line(size = 1.5, alpha = 0.5)
