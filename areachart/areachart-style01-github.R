#Source: DOI: 10.1016/S1473-3099(24)00492-4
#Date: 2024-11-20
#Code: Qilan Wen

library(ggplot2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
theme_set(theme_bw())

data_aggregated <- read.csv('D:/areachart-style01-github.csv')

data_aggregated_1 <- subset(data_aggregated, week_cont.f >= 1 & week_cont.f <= 25)
data_aggregated_1$week_cont.f <- factor(data_aggregated_1$week_cont.f)
data_aggregated_1$area <- factor(data_aggregated_1$area, levels = c("Control", "Intervention", "Buffer"))
data_aggregated_1$ratio <- (data_aggregated_1$sum_dengue / data_aggregated_1$sum_people)*10000
g2_1 <- ggplot(data = data_aggregated_1) +
  geom_area(mapping=aes(x = week_cont.f, y = ratio, group = area,fill = area), alpha = 0.6) +
  scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25"), labels = c("1", "5", "10", "15", "20", "25")) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("Control" = "firebrick1", "Buffer" = "deepskyblue3", "Intervention" = "green3")) +
  labs(x = "Week (1-25: 2016 epidemic)", y = "Dengue incidence (confirmed cases per 10000)")+
  scale_y_continuous(limits = c(0, 150))

plot(g2_1)
