# Uncertainty
## Quantile Dot Plot
library(ggplot2)
library(dplyr)

mu <- 1.02
sd <- 0.9
binwidth <- 0.31

df_q <- data.frame(x = qnorm(ppoints(50), mu, sd)) %>%
  mutate(type = ifelse(x <= 0, "A", "B"))

x_vals <- seq(min(df_q$x), max(df_q$x), length.out = 100)
y_vals <- dnorm(x_vals, mean = mu, sd = sd)
df_norm <- data.frame(x = x_vals, y = y_vals)

ggplot(df_q, aes(x, fill = type)) +
  geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
  geom_line(data = df_norm, aes(x, 1.92 * y), inherit.aes = FALSE) +
  geom_dotplot(binwidth = binwidth) +
  scale_fill_manual(values = c(A = "#f8f1a9", B = "#b1daf4"), guide = "none")

binwidth <- 0.31 * 2.1

df_q <- data.frame(x = qnorm(ppoints(10), mu, sd)) %>%
  mutate(type = ifelse(x <= 0, "A", "B"))

ggplot(df_q, aes(x, fill = type)) +
  geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
  geom_line(data = df_norm, aes(x, 1.92 * y), inherit.aes = FALSE) +
  geom_dotplot(binwidth = binwidth) +
  scale_fill_manual(values = c(A = "#f8f1a9", B = "#b1daf4"), guide = "none")

## Curve Fit
plt <- ggplot(ToothGrowth, aes(dose, len)) + 
  geom_point()

plt + 
  geom_smooth()

plt +
  geom_smooth(method = lm)

plt +
  geom_smooth(method = lm, level = 0.99)

plt +
  geom_smooth(method = lm, formula = y ~ poly(x, 2))

## Error Bars
library(Rmisc)

tgc <- summarySE(ToothGrowth, measurevar = "len", groupvars = c("supp", "dose"))
head(tgc)

plt <- ggplot(tgc, aes(x = dose, y = len, colour = supp))
plt +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = len - se, ymax = len + se), width = .1)

pd <- position_dodge(0.1)
plt + geom_errorbar(aes(ymin = len - se, ymax = len + se), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd)

plt + 
  geom_errorbar(aes(ymin = len-ci, ymax = len+ci), width = .1, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd)


# Contours
head(faithful)

ggplot(faithful, aes(eruptions)) + 
  geom_density()

ggplot(faithful, aes(waiting)) +
  geom_density()

ggplot(faithful, aes(waiting)) + 
  geom_density() +
  geom_density(aes(eruptions))

plt <- ggplot(faithful, aes(x = eruptions, y = waiting))
plt + 
  stat_density2d()

plt +
  stat_density2d(aes(colour = ..level..)) +
  geom_point()

plt +
  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)

plt + 
  geom_point() +
  stat_density2d(aes(alpha = ..density..), geom = "tile", contour = FALSE)

plt +
  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE, h = c(.5, 5))

plt + 
  stat_bin2d(aes(fill = ..density..))


# Advanced Color Mapping
pal = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")

d = data.frame(v = rexp(1000))
quarts = quantile(mtcars$hp, probs = seq(0, 1, .25), names = FALSE)

p <- ggplot(d, aes(v)) + 
  geom_histogram(aes(fill = ..count..), bins = 30)
p + 
  scale_fill_distiller(palette = "GnBu", direction = 1)

library(scales)

p + 
  scale_fill_gradientn(colors = pal, values = rescale(quarts, to = c(0, 1)))


# Some Extras
## Multiple Plots
library(gridExtra)

p1 <- ggplot(diamonds, aes(factor(color), price)) + 
  geom_boxplot()
p2 <- ggplot(diamonds, aes(depth, price)) + 
  geom_density_2d()
p3 <- ggplot(diamonds, aes(factor(clarity), price)) +
  geom_violin()

grid.arrange(p1, p2, p3, ncol = 3)

grid.arrange(p1, p2, p3, nrow = 3)

grid.arrange(p1, p2, p3, nrow = 2, ncol = 2)

## Animation
library(gganimate)
library(gifski)
library(gapminder)

p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour = country)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p

p + 
  transition_time(year) +
  labs(title = "Year: {frame_time}")