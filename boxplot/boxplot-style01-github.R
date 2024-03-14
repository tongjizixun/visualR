# Source: DOI: 10.1002/ejhf.172
# Date: 2023-12-05
# Using the built-in dataset 'pbc' in R

library(survival)
library(ggplot2)
data(pbc, package = "survival")

# Convert 'edema' to a factor with custom labels for plotting
pbc$edema_factor <- factor(pbc$edema, levels = c(1, 0.5, 0),
                           labels = c("Edema", "Uncertain", "No Edema"))

# Create a boxplot using ggplot2
ggplot(pbc, aes(x = edema_factor, y = albumin)) +
  stat_boxplot(geom = 'errorbar', width = 0.5,
               colour = rgb(112/255, 127/255, 140/255)) +
  geom_boxplot(fill = rgb(145/255, 163/255, 182/255),
               colour = rgb(112/255, 127/255, 140/255),
               outlier.size = 2.5,
               outlier.color = rgb(39/255, 75/255, 119/255)) +

  # Add labels for x-axis, y-axis, and the plot title
  labs(x = "Edema Status",
       y = "Albumin Level",
       title = "Albumin Levels by Edema Status") +

  # Customize the theme for a cleaner look and bold title and axis labels
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 13, color = "black"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(color = rgb(238/255, 240/255, 238/255)),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white", color = NA)) +
  scale_y_continuous(breaks = pretty(pbc$albumin, n = 8))


