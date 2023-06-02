# Source: Xie Y, Bowe B, Maddukuri G, Al-Aly Z. Comparative evaluation of clinical manifestations and risk of death in patients admitted to hospital with covid-19 and seasonal influenza: cohort study BMJ 2020; 371 :m4677 doi:10.1136/bmj.m4677 
# Date:2023-06-02

# Import packages
library(ggplot2)
library(scales)

# Create data frame
data <- data.frame(
  row = c("overall", "cancer", "chronic lung disease", "cardiovascular disease",
          "hypertension", "diabetes mellitus", "obesity", "cerebrovascular disease",
          "peripheral artery disease", "chronic kidney disease", "dementia"),
  `overall` = c(20.0, 18.2, 18.7, 19.2, 19.4, 20.0, 21.2, 22.0, 22.2, 24.9, 26.2),
  `Less than 65` = c(9.3, 12.3, 10.9, 12.3, 10.6, 13.3, 10.2, 9.1, 7.3, 15.5, 9.2),
  `from 65 to 75` = c(19.4, 19.3, 17.4, 20.0, 15.9, 22.3, 20.7, 20.4, 25.8, 25.9, 27.7),
  `over 75` = c(27.8, 21.6, 24.4, 24.3, 25.9, 28.6, 28.2, 31.3, 27.3, 36.5, 38.5),
  White = c(17.7, 15.5, 16.9, 16.8, 17.1, 22.8, 17.5, 19.5, 21.6, 21.6, 23.8),
  Black = c(28.1, 26.0, 25.9, 26.9, 27.2, 30.9, 35.1, 28.5, 26.1, 35.2, 28.3)
)

# Convert data to a long format
data_long <- tidyr::pivot_longer(data, -row, names_to = "Age Group / Race", values_to = "Percentage")

# Modify fill color
colors <- c("yellow", "orange", "orangered", "purple", "darkviolet", "black")
breaks <- c(10, 15, 20, 25, 30, 35)

# Adjust the order of elements on the horizontal and vertical axes
ggplot(data_long, aes(x = factor(`Age Group / Race`, levels = unique(`Age Group / Race`)), y = factor(row, levels = rev(unique(row))), fill = Percentage, label = Percentage)) +
  geom_tile() +
# Show different colors for different values
  geom_text(size = 4, color = ifelse(data_long$Percentage < 20, "black", "white")) +
  scale_fill_gradientn(colors = colors, values = rescale(breaks, to = c(0, 1)), limits = c(7, 39), breaks = breaks) +
# Modify the title and axis label  
  labs(x = NULL, y = NULL, title = "Deaths", fill = "Death per 100 patients") +
#Place the fill color label below  
  theme(legend.position = "bottom")

