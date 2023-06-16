#Source:Hettiarachchi, Ayesh et al. Heart disease complicating pregnancy as a leading cause of maternal deaths in LMIC settings: the Sri Lankan experience; The Lancet Regional Health - Southeast Asia, Volume 15, 100223
#Date:2023-06-16

# Import packages
library(ggplot2)

# Create data frame
data <- data.frame(
  year = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
  "Heart disease" = c(23, 26, 25, 27, 21, 17, 13, 37, 21, 29, 11, 19, 15),
  "No Heart disease" = c(77, 74, 72, 108, 114, 63, 70, 38, 64, 53, 64, 63, 69),
  "unconfirmed maternal deaths" = c(105, 98, 91, 90, 72, 102, 57, 71, 90, 76, 85, 98, 89)
)

# Convert data to a long format
data_long <- tidyr::gather(data, key = "disease", value = "value", -year)

# Set color
colors <- c("#8B4513", "#FFA07A", "#FFDAB9")

# Draw the bar chart and set the font
ggplot(data_long, aes(x = factor(year), y = value, fill = disease)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Number of deaths", fill = "") +
  scale_fill_manual(values = colors) +
  ggtitle("Bar Chart of Diseases by Year") +
  geom_text(aes(label = value), color = "white", position = position_stack(vjust = 0.5))
