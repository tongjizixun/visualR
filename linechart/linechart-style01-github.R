#Source: Doi:10.1016/S2542-5196(23)00089-X
#Date:2023-06-16
#Code: Hui Tang

library(gridExtra)
library(ggplot2)
library(openxlsx)
rm(list = ls())

df <- read.xlsx("D:/linechart-style01-data-github.xlsx",1)
df$factor <- as.factor(df$factor)
a <- df[df$factor==1,]
b <- df[df$factor==2,]
p1 <- ggplot(a,aes(x=year, y=HR))+
  geom_line(size=0.75,color="red") +   #draw line
  geom_point(size=3.5,color="red")+    #draw point
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.15,size=0.75,color="red")+    #draw error bar
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")+   #add horizontal dashed line
  geom_vline(aes(xintercept = 0), linetype = "dashed",color = "gray")+   #add vertical dashed line
  scale_y_continuous(breaks=-6:4*1) +        #set Y-axis range
  scale_x_continuous(breaks=-2:8*1)+         #set X-axis range
  labs(x="year",y="HR 95%CI",title="No housing stress before disaster")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),     #set drawing background to blank
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 15),   #set the title to be centered and adjust the font size
        axis.text.x = element_text(size = 14),   #adjust the font size of the X-axis label
        axis.title.x = element_text(size = 16),  #adjust the font size of the X-axis title
        axis.text.y = element_text(size = 14),   #adjust the font size of the Y-axis label
        axis.title.y = element_text(size = 16))  #adjust the font size of the Y-axis title

p2 <- ggplot(b,aes(x=year, y=HR))+
  geom_line(size=0.75,color="blue") +
  geom_point(size=3.5,color="blue")+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.15,size=0.75,color="blue")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")+
  geom_vline(aes(xintercept = 0), linetype = "dashed",color = "gray")+
  scale_x_continuous(breaks=-2:8*1)+
  scale_y_continuous(breaks=-6:4*1) + 
  labs(x="year",y=NULL,title="Housing stress before disaster")+
  theme_bw()+
  theme(panel.grid.major=element_line(colour=NA),    
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 15),   
        axis.text.x = element_text(size = 14),   
        axis.title.x = element_text(size = 16), 
        axis.text.y = element_text(size = 14),   
        axis.title.y = element_text(size = 16))  

P <- grid.arrange(p1, p2, nrow = 1)
ggsave("D:/linechart-style01-github.bmp",dpi = 350,width=10,height = 6,P)
