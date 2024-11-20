#Source: DOI: 10.1016/S1473-3099(24)00492-4
#Date: 2024-11-19
#Code: Qilan Wen

library(ggpubr)
library(ggplot2)
library(ggnewscale)

means <- read.table("D:/boxplot-style02-github.txt",header=TRUE)
means$area <- factor(means$area)
means$period <- factor(means$period)
means$positive <- factor(means$positive)
summary(means$positive[means$period==1])
#Citywide
weekly_means_plot_posneg_log <- ggplot(data = means, aes(x = period, y = log10(mean), group = period)) +
  geom_boxplot(data = means, aes(x = period, y = log10(mean), group = period), fill = NA, outlier.shape = NA, alpha = 0.9, col = "black") + #box
  geom_point(data = means, aes(x = period, y = log10(mean), col = positive, alpha = positive, group = id), size = 2, position = position_dodge(0.3)) + #points
  geom_line(data = means, aes(x = period, y = log10(mean), group = id, col = positive, alpha = positive), position = position_dodge(0.3)) +#lines
  scale_color_manual(values=c('grey','grey20')) +
  scale_alpha_manual(values=c(0.2,0.5)) +
  theme_bw() +
  ylim(-1,2) +
  stat_summary(fun=mean, geom="point", shape=23, size=2, color="blue", fill="blue") + #mean in blue
  scale_x_discrete(breaks = c(0, 1), labels = c("Before", "After")) +
  theme(panel.border = element_blank(), axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Time Period", y = "Average weekly dengue incidence per 10000 (log10-transformed)")
#Control area
means_control<-subset(means,area=="Control")
weekly_means_plot_control_posneg_log <- ggplot(data = means_control, aes(x = period, y = log10(mean), group = period)) +
  geom_boxplot(data = means_control, aes(x = period, y = log10(mean), group = period), fill = NA, outlier.shape = NA, alpha = 0.9, col = "black") +
  geom_point(data = means_control, aes(x = period, y = log10(mean), col = positive, alpha = positive, group = id), size = 2, position = position_dodge(0.3)) +
  geom_line(data = means_control, aes(x = period, y = log10(mean), group = id, col = positive, alpha = positive), position = position_dodge(0.3)) +
  scale_color_manual(values=c('olivedrab','orange')) +
  scale_alpha_manual(values=c(0.2,1)) +
  theme_bw() +
  ylim(-1,2) +
  stat_summary(fun=mean, geom="point", shape=23, size=2, color="blue", fill="blue") +
  scale_x_discrete(breaks = c(0, 1), labels = c("Before", "After")) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Time Period", y = "")
#Intervention area
means_intervention<-subset(means,area=="Intervention")
weekly_means_plot_intervention_posneg_log <- ggplot(data = means_intervention, aes(x = period, y = log10(mean), group = period)) +
  geom_boxplot(data = means_intervention, aes(x = period, y = log10(mean), group = period), fill = NA, outlier.shape = NA, alpha = 0.9, col = "black") +
  geom_point(data = means_intervention, aes(x = period, y = log10(mean), col = positive, group = id), size = 2, position = position_dodge(0.3), alpha = 1) +
  geom_line(data = means_intervention, aes(x = period, y = log10(mean), group = id, col = positive), position = position_dodge(0.3), alpha = 1) +
  scale_color_manual(values=c('olivedrab','orange')) +
  theme_bw() +
  ylim(-1,2) +
  stat_summary(fun=mean, geom="point", shape=23, size=2, color="blue", fill="blue") +
  scale_x_discrete(breaks = c(0, 1), labels = c("Before", "After")) +
  theme( panel.border = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Time Period", y = "")
#Buffer area
means_buffer<-subset(means,area=="Buffer")
weekly_means_plot_buffer_posneg_log <- ggplot(data = means_buffer, aes(x = period, y = log10(mean), group = period)) +
  geom_boxplot(data = means_buffer, aes(x = period, y = log10(mean), group = period), fill = NA, outlier.shape = NA, alpha = 0.9, col = "black") + 
  new_scale_color() +
  geom_point(data = means_buffer, aes(x = period, y = log10(mean), col = positive, group = id), size = 2, position = position_dodge(0.3), alpha = 1) +
  geom_line(data = means_buffer, aes(x = period, y = log10(mean), group = id, col = positive), position = position_dodge(0.3), alpha = 1) +
  scale_color_manual(values=c('olivedrab','orange')) +
  theme_bw() +
  ylim(-1,2) +
  stat_summary(fun=mean, geom="point", shape=23, size=2, color="blue", fill="blue") +
  scale_x_discrete(breaks = c(0, 1), labels = c("Before", "After")) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Time Period", y = "")
#Combine panels
out <- ggarrange(weekly_means_plot_posneg_log, weekly_means_plot_control_posneg_log, weekly_means_plot_intervention_posneg_log, weekly_means_plot_buffer_posneg_log,
          labels = c("A", "B", "C", "D"),
          ncol = 4, nrow = 1)
plot(out)
