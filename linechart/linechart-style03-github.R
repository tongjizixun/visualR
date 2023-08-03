#source DOI: 10.1016/S0140-6736(21)02249-2
#date：2023-07-26
library(readxl)
library(ggplot2)
df <- read_excel("D:/linechart-style03-data-github.xlsx")

# Converting the date column to date type
df$date <- as.Date(df$date)

# Set the language environment to English.
Sys.setlocale("LC_TIME", "en_US.UTF-8")

#Create a  plot.
plot <- ggplot(df, aes(x = date)) +
  geom_col(aes(y = case/80), fill = "gray80", color = "white", alpha = 0.7, width = 0.98, na.rm = TRUE)+#Plot a bar chart.
  geom_line(aes(y = `30-39`, color = "30-39"), linewidth = 0.6) +#Plot a line chart.
  geom_line(aes(y = `40-49`, color = "40-49"), linewidth = 0.6) +
  geom_line(aes(y = `50-59`, color = "50-59"), linewidth = 0.6) +
  geom_line(aes(y = `≥60`, color = "≥60"), linewidth = 0.6) +
  labs(x = "Date", y = "Daily incidence proportion per 100000 population (%)", color = "Age group(years)") +
  scale_color_manual(values = c("30-39" = "red", "40-49" = "green", "50-59" = "blue", "≥60" = "purple"), breaks = c("30-39", "40-49", "50-59", "≥60")) +#Manually set color mapping
  theme_bw()+
  theme(
    legend.position = c(0.15, 0.80),
    panel.grid = element_blank(),#Remove the gridlines.
    axis.ticks.length = unit(0.3, "cm"),# Adjust the length of the tick marks.
    axis.text.x = element_text(size = 10, hjust = 0.5),  # Font size of the x-axis tick labels.
    axis.text.y = element_text(size = 10),# Font size of the y-axis tick labels.
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)#Setting the plot margins
  )+
  scale_x_date(
    breaks = seq(as.Date("2023-07-05"), as.Date("2023-09-20"), by = "1 week"),# One tick mark per week.
    date_labels = "%b %d",  # Display the dates as abbreviated English.
    limits = as.Date(c("2023-07-05", "2023-09-21")), 
    expand = c(0, 0)  
  )+#Setting the x-axis range
  scale_y_continuous(
    name = "Daily incidence proportion per 100000 population (%)",
    limits = c(0, 125),
    expand = c(0, 0),
    sec.axis = sec_axis(~ .* (max(df$case) / 125),  # Scale to be proportional to the range of the primary axis.
      name = "Daily number of new cases",
      breaks = c(0, 2500, 5000, 7500, 10000)
    )
  )+#Setting the y-axis range
geom_vline(xintercept = as.numeric(as.Date(c("2023-07-30", "2023-08-12", "2023-08-19", "2023-08-24"))),
           linetype = "dashed",
           color = c("purple", "blue", "green", "red"))#Add dashed lines.

print(plot)

##the caption from the source paper
# Daily incidence proportions of SARS-CoV-2 infection (ie, positive PCR-test) 
# among the at-risk population by age group around the time the third dose 
# vaccination was initiated (left Y axis). For each age group, the vertical 
# dashed line with the same colour is the day that age group became eligible 
# for the third dose. The epidemic curve (daily incidence counts) is shown 
# shaded in the background (right Y axis). All curves were smoothed by using a 
# moving 7-day mean, assigning for each day the value of the mean of the 7 days 
# ending on that day.