#Source: Doi: 10.1016/S0140-6736(20)31965-6.
#Date: 2023-06-16
#Code: Yueqian Wu

library(ggplot2)
library(tidyverse)
#set up a database
set.seed(240)
n <- 213  # sample size
f <- function(x, n, ...) factor(sample(x, n, replace=T, ...), levels=x)  # random sampling
data <- data.frame(patientID = c(1:n),
                   Age = floor(runif(n,18,65)),# age of 18-65 years old
                   Sex = f(c("Male","Female"),n,prob=c(0.7,0.3)),# 7:3 female:male sampling
                   group = rep(c("Placebo", "Treated_1","Treated_2"),time = 71), # 1:1:1,repeat 71 times
                   Gout = f(c("No","Yes"),n,prob=c(0.7,0.3)),#patience 7:3
                   BL_UA=round(runif(n,min=600,max=700)))# create a data frame (baseline data)

data$M1st_UA[data$group=="Placebo"] <- round(rnorm(71,mean=600,sd=15))#Set the mean and standard deviation of follow-up outcomes
data$M1st_UA[data$group=="Treated_1"] <- round(rnorm(71,mean=550,sd=14))
data$M1st_UA[data$group=="Treated_2"] <- round(rnorm(71,mean=540,sd=15))

data$M2nd_UA[data$group=="Placebo"] <- round(rnorm(71,mean=590,sd=14))
data$M2nd_UA[data$group=="Treated_1"] <- round(rnorm(71,mean=500,sd=17))
data$M2nd_UA[data$group=="Treated_2"] <- round(rnorm(71,mean=430,sd=21))

data$M3rd_UA[data$group=="Placebo"] <- round(rnorm(71,mean=605,sd=16))
data$M3rd_UA[data$group=="Treated_1"] <- round(rnorm(71,mean=430,sd=20))
data$M3rd_UA[data$group=="Treated_2"] <- round(rnorm(71,mean=320,sd=24))

# Add some missing data to simulate real follow-up situation
data$M1st_UA[sample(n, 11)] <- NA
data$M2nd_UA[sample(n, 21)] <- NA
data$M2nd_UA[is.na(data$M1st_UA)] <- NA # Continuously missing on the basis of M1st missing
data$M3rd_UA[sample(n, 31)] <- NA
data$M3rd_UA[is.na(data$M2nd_UA)] <- NA # Continuously missing on the basis of missing M1st and M2nd

#so if data got from the real follow-up situation has  missing data, how can we draw the picture
data_analysed <- na.omit(data)
#Plot
df1 <- data_analysed %>% # Calculate mean and standard deviation
  group_by(group) %>%
  summarise(
    n=n(),
    m0=mean(BL_UA),s0=sd(BL_UA),
    m1=mean(M1st_UA),s1=sd(M1st_UA),
    m2=mean(M2nd_UA),s2=sd(M2nd_UA),
    m3=mean(M3rd_UA),s3=sd(M3rd_UA),
  )
df2 <- pivot_longer(df1,!c("group","n"),names_to = c(".value","time"),names_pattern = "(.)(.)") #Convert wide data to long data


ggplot(data=df2,aes(time,m,color=group,group=group,shape=group))+
       geom_line(position=position_dodge(0.3),linewidth=0.5)+ # fold line
       geom_point(position=position_dodge(0.3),size=2)+ # point
       scale_shape_manual(values = c(15,18,19),
                          labels=c("Placebo","Treated_1","Treated_2"))+ # the shape of point
       geom_errorbar(aes(time,ymin=m-s,ymax=m+s),
                     position=position_dodge(0.3),
                     width=0.3,size=0.5)+ # error bar
       labs(title = "Figure: Change of serum uric acid among visits",
            x = NULL,y="Serum uric acid (mmol/L)")+ # axis labels
       scale_x_discrete(breaks=c("0","1","2","3"),
                        labels=c("Baseline","1 month","2 months","3 months"))+
       scale_y_continuous(breaks = seq(0,900,100))+ #scale interval
       scale_color_manual(values = c("#FF4500","#27408B","#2F4F4F"),
                          labels=c("Placebo","Treated_1","Treated_2"))+ # color of lines
       theme(axis.line = element_line(colour = "black"), #Color of axis
             legend.position = c(0.9,0.2), # legend location
             legend.background = element_blank(),
             legend.key=element_blank(),
             panel.background = element_blank(), # set background to blank
             legend.title = element_blank(), # legend Title
             plot.title = element_text(hjust = 0.5), # title Position
             plot.margin = unit(c(1,3,1,1),"cm"))+ #drawing area edge size
      expand_limits(y=0)#set the start of Y-Axis

# Warning message:
# Ignoring unknown parameters: linewidth 
