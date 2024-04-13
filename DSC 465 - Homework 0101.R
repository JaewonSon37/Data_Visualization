# Q5
cellPlans = data.frame(c("ATT", "Sprint", "Verizon", "ATT", "Sprint",
                         "Verizon", "ATT", "Sprint", "Verizon", "ATT",
                         "Verizon", "Sprint", "Verizon", "ATT", "Verizon", 
                         "Sprint", "ATT", "ATT", "Sprint"),
                       c(1, 1, 2, 3, 3, 4, 6, 6, 8, 10, 12, 
                         12, 16, 16, 24, 24, 25, 30, 40),
                       c(30, 20, 35, 40, 30, 50, 60, 45, 70, 80, 
                         80, 60, 90, 90, 110, 80, 110, 135, 100)) 
names(cellPlans) = c("Company", "DataGB", "Price")
head(cellPlans)

## Q5.a
library(ggplot2)

ggplot(cellPlans, aes(x = DataGB, y = Price, color = Company)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  labs(x = "Data (GB)", y = "Price ($)", color = "Company") +
  ggtitle("Trend of Mobile Plans by Each Company") +
  scale_color_manual(values = c("Light Sky Blue", "Goldenrod", "Saddle Brown")) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

## Q5.b
library(ggrepel)

ggplot(cellPlans, aes(x = DataGB, y = Price)) +
  geom_point(aes(color = Company), size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "Sea Green") +
  geom_text_repel(aes(label = Company), size = 3) +
  labs(title = "Comprehensive Trend of Mobile Plans") +
  scale_color_manual(values = c("Light Sky Blue", "Goldenrod", "Saddle Brown")) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))