#Source DOI: 10.1016/S2214-109X(21)00359-4.
#Date:2023-07-25

# Import packages
library(ggplot2)
library(gridExtra)
library(tidyr)
library(patchwork)

#Read files and modify titles
data1<-read.csv("D:/barchart-style03-data1-github.csv", encoding = "UTF-8")
data2<-read.csv("D:/barchart-style03-data2-github.csv", encoding = "UTF-8")
colnames(data1)[which(colnames(data1) == "NA.")] <- "NA"
colnames(data2)[which(colnames(data2) == "NA.")] <- "NA"

# Convert data to a long format
data_long <- gather(data1, key = variation, value = number, -Category)
data_long2 <- gather(data2, key = variation, value = number, -Category)

#Adjust variable properties and set colors
data_long$variation<-factor(data_long$variation,ordered = T, levels=c("NA","None","Partial","Full"))
data_long2$variation<-factor(data_long2$variation,ordered = T, levels=c("NA","None","Partial","Full"))
colors <- c("Full" = "#006400", "Partial" = "#00B300", "None" = "#FFD1BF", "NA" = "#2F4F4F")

#Draw a stacked bar chart
p1<-ggplot(data_long, aes(x = Category, y = number, fill = variation)) +
  geom_bar(position="stack", stat = "identity",width = 0.4) +#Set width
  scale_x_discrete(limits=data1$Category)+#Set the X-axis grouping order
  scale_y_continuous(breaks = c(0, 50,100,150), expand = c(0, 0))+#Set the Y-axis scale and boundary
  theme_bw() +#Change theme color
  labs(x = "", y = "Number of countries", fill="") +#Change axis titles
  scale_fill_manual(values = colors, limits = c("Full","Partial","None","NA" )) +#Set variable color change
  coord_flip()+#Axis inversion
  theme(aspect.ratio = 1.5,#Adjust the axis ratio
        legend.justification = "left",legend.position = "top",#Sets the properties of the legend
        legend.key.size = unit(0.4,"cm"),legend.key.height = unit(0.4,"cm"),
        panel.grid.major = element_blank(),#Remove border
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line(color = "black"),  #Set the axis line color
        axis.ticks = element_line(color = "black")) +
        guides(fill = guide_legend(override.aes = list(color = "black")))#Set the legend border

#Draw another stacked bar chart,Ibid.
p2<-ggplot(data_long2, aes(x = Category, y = number, fill = variation)) +
  geom_bar(position="stack", stat = "identity",width = 0.3) +
  scale_x_discrete(limits=data2$Category,expand = c(0.1, 0.1))+
  scale_y_continuous(breaks = c(0,25,50,75,100),expand = c(0, 0))+
  theme_bw() +
  labs(x = "", y = "%", fill="") +
  scale_fill_manual(values = colors, limits = c("Full","Partial","None","NA" )) +
  coord_flip()+
  theme(aspect.ratio = 0.07,
        legend.justification = "left",legend.position = "top",
        legend.key.height = unit(0.4,"cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line(color = "black"),  
        axis.ticks = element_line(color = "black")) +
        guides(fill = "none")

#Stacked bar charts merge and display
p12 <- p1 / p2
p12


