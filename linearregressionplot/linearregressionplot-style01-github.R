#Reference：Local government funding and life expectancy in England: a longitudinal ecological study. Lancet Public Health, 2021, 6(9): e641-e647
#Date：20230602
library(readxl) 
library(lme4)
library(splines)
library(dplyr)
library(lubridate)
library(car)
library(blme)
library(stringr)
library(ggplot2)
library(patchwork)

#import data
mydata<- read.csv("D:/linearregressionplot-style01-data-github.csv",header=T, sep=",")

#【descriptive statistics】
#Calculate variation value
allcauses<- subset(mydata, Disease == "All causes", select = c(2,3,6,7))#Only choose All causes for outcome
allcauses_2000 <- subset(allcauses, year == "2000")
allcauses_2019 <- subset(allcauses, year == "2019")
Name_vars <- names(allcauses_2000)
names(allcauses_2000)[3] <- paste0(Name_vars[3], "_2000")
names(allcauses_2019)[3] <- paste0(Name_vars[3], "_2019")#Add the corresponding year after the names of all variables
names(allcauses_2000)[4] <- paste0(Name_vars[4], "_2000")
names(allcauses_2019)[4] <- paste0(Name_vars[4], "_2019")
DATA_19Y <- merge(allcauses_2000[ ,-2], allcauses_2019[ ,-2], by=c("location_name"))#Merge data

DATA_19Y[ ,6] <- (DATA_19Y[ ,4]-DATA_19Y[ ,2])#Calculate the difference between the selected variables
DATA_19Y[ ,7] <- (DATA_19Y[ ,5]-DATA_19Y[ ,3])
DATA_19Y_change <- DATA_19Y[ ,c(1,6,7)]

allcauses2<- subset(mydata, Disease == "All causes", select = c(2,3,4,5))
#Classify GDP and population
mydata_2010 <- subset(allcauses2, year == "2010")
Educationq <- quantile(mydata_2010$Education, c(0.2,0.4,0.6,0.8,1.0))
mydata_2010$Education_q <- cut(mydata_2010$Education, breaks = c(0,Educationq[1],Educationq[2],Educationq[3],Educationq[4],Educationq[5]),
                               labels = c("Q1 (lowest Education)", "Q2", "Q3", "Q4", "Q5 (highest Education)"))
summary(mydata$population)
mydata_2010$pop_q <- cut(mydata_2010$population, breaks = c(0,10000000,100000000,10000000000),
                    labels = c("< 10 million", "< 100 million", "< 1000 million"), right=FALSE, include.lowest=FALSE)

#merge data
mydata_p<-NULL
mydata_p <- merge(DATA_19Y_change, mydata_2010[ ,c(1,5,6)], by = "location_name")
names(mydata_p)[4] <- "Education quintile (2010)"
names(mydata_p)[5] <- "Population (2010)"
names(mydata_p)[3] <- "Outcome"
names(mydata_p)[2] <- "GDP"
mydata_p$GDP<-mydata_p$GDP*0.01

#plot
mp_1 <- ggplot(data = mydata_p, aes(x = GDP, y = Outcome))+       
  coord_cartesian() +  
  geom_smooth(method = "lm",formula = y~x, size= 0.1, colour= "#4D6A75", alpha = .2, linetype = "solid")+#Add linear regression line
  geom_hline(yintercept = 0, color = "#4D6A75",size= 0.5,linetype="dashed")+#Add a dashed line to the point with a vertical coordinate of 0
  geom_point(data = mydata_p, aes(x = GDP, y = Outcome, bg=`Education quintile (2010)`, 
                                  color = `Education quintile (2010)`,size = `Population (2010)`), 
                                  alpha = 0.6, pch = 21) +#Add the data points you want to add (including other variables we want to present)
  ylim(-10000, 5000)+#Set the y-axis range
  xlab("Change in GDP 2000-2019") + ylab("Change in DALYs of NCDs 2000-2019 (/100 000)")+#Names of the x and y axes
  ggtitle("GDP")+#The name of the entire graph
  theme(axis.text = element_text(size=20, color="black", family = "sans"),
        legend.position = c(0.5,0.85),axis.title.y = element_text(vjust = 2.5))+#Set the size of coordinate axis dimensions
  theme(panel.background = element_rect(fill = "white"),#Set background to blank
        axis.line = element_line(size = 0.5, colour = "black"),#Set the thickness and color of the coordinate axis
        plot.margin = margin(t = 10, r = 10, b = 20, l = 10),  #Set the white space around the image
        legend.title = element_text(size = 15, family = "sans", color = "black", face = "bold"),
        legend.text =  element_text(size = 15, family = "sans", color = "black"),#Set the size of the legend
        strip.text = element_text(size = 15, color = "black", family = "sans"),
        axis.title = element_text(size=20, color="black", family = "sans"),
        axis.title.x = element_text(vjust = -1.5),#Set the distance between the title on the x-axis and the x-axis
        legend.direction = "vertical",#Set each legend to be vertical
        legend.box = "horizontal", #Set multiple legends to be placed horizontally
        legend.key = element_rect(fill = "white"),
        plot.title = element_text(color="black", size=20, family = "sans",
                                      hjust = 0.5, vjust = 0.5))#Set the position of the title for the graph
mp_1

# Warning messages:
# 1: Using size for a discrete variable is not advised. 
# 2: Removed 78 rows containing non-finite values (stat_smooth). 
# 3: Removed 78 rows containing missing values (geom_point). 

