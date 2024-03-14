#source DOI: 10.1126/science.ade0805
#date：2024-03-14

Year=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
y1=c(0.38,0.52,0.68,0.38,0.36,0.4,0.28,0.46,0.37,0.36,0.27,0.47,0.28,0.38,0.35,0.33,0.34,0.33,0.39,0.37,0.45)
y2=c(9,25,28,15,14.9,15,12,26,13,17,12.8,18,10.9,17,14,11,13,13.2,13.8,10.8,17)
y3=c(2,4.2,4,4.7,3.2,3.4,2.8,3.3,2.7,3,4,6.4,7,6.2,4.7,6.2,5.9,5.7,5.4,8,9)
data1=data.frame(Year,y1)
sd1=sd(y1)
m1=mean(y1)
data2=data.frame(Year,y2)
sd2=sd(y2)
m2=mean(y2)
data3=data.frame(Year,y3)
sd3=sd(y3)
m3=mean(y3)
library(ggplot2)

p1=ggplot(data=data1,aes(x=Year,y=y1))+ 
  #the confidence band with different width
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=m1-sd1*2,ymax=m1+sd1*2),fill="#d5eaed",alpha=0.2)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=m1-sd1,ymax=m1+sd1),fill="#a2d4df",alpha=0.2)+
  #the points and line
  geom_point(color="#0F79AB",size=4)+
  geom_line(color="#0F79AB",lwd=1)+
  #the reference line
  geom_hline(yintercept=mean(y1),lty=2)+
  #set the backgroud being white
  theme(panel.background = element_rect(fill="white",color="grey"), 
        axis.title=element_text(size=20),
        #set the space around the chart clockwise from the top
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"),
        #set the picture border, fill to select "NA", otherwise automatically default to white
        panel.border = element_rect(color="black",fill="NA"),
        axis.text=element_text(size=17))+  
  labs(y="Active fire detection (million)")+
  #remove the white space on both sides of the y axis
  scale_y_continuous(limits=c(0,0.8),expand=c(0,0))+
  #change the font type
  annotate("text",x=2020,y=0.21,vjust=0,label="±2σ",size=6,color="#0F79AB",fontface="bold.italic")+
  annotate("text",x=2020,y=0.3,vjust=0,label="±1σ",size=6,color="#0F79AB",fontface="bold.italic")+
  annotate("text",x=2008,y=0.6,hjust=0,label="2000-2020 mean",fontface="bold.italic",size=6,color="#0F79AB")+
  annotate("segment",x=2014,xend=2015,y=0.57,yend=m1,size=0.5)+
  #make sure the axes don't expand to the left when set <2000, add them in annotate outside the diagram
  coord_cartesian(clip="off",xlim=c(2000,2020))+
  annotate("text",x=1996.5,y=0.8,label="A",size=8,fontface="bold")

p2=ggplot(data=data2,aes(x=Year,y=y2))+  
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=m2-sd2*2,ymax=m2+sd2*2),fill="#f4dbb2",alpha=0.2)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=m2-sd2,ymax=m2+sd2),fill="#d9b38f",alpha=0.2)+
  geom_point(color="#997031",size=4)+
  geom_line(color="#997031",lwd=1)+
  geom_hline(yintercept=mean(y2),lty=2)+
  theme(panel.background = element_rect(fill="white",color="grey"),
        axis.title=element_text(size=20),
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"),
        panel.border = element_rect(color="black",fill="NA"),
        axis.text=element_text(size=17))+
  labs(y="Burned area (Mha)")+
  scale_y_continuous(limits=c(0,40),expand=c(0,0))+
  annotate("text",x=2020,y=6,vjust=0,label="±2σ",size=6,color="#997031",fontface="bold.italic")+
  annotate("text",x=2017,y=10.5,vjust=0,label="±1σ",size=6,color="#997031",fontface="bold.italic")+
  annotate("text",x=2008,y=27,hjust=0,label="2000-2020 mean",size=6,color="#997031",fontface="bold.italic")+
  annotate("segment",x=2014,xend=2015,y=26,yend=m2,size=0.5)+
  coord_cartesian(clip="off",xlim=c(2000,2020))+  #保证坐标轴不会因为想设置<2000的坐标就向左扩展
  annotate("text",x=1996.5,y=40,label="B",size=8,fontface="bold")

p3=ggplot(data=data3,aes(x=Year,y=y3))+  
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=m3-sd3*2,ymax=m3+sd3*2),fill="#f3cbc3",alpha=0.2)+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=m3-sd3,ymax=m3+sd3),fill="#dfae9d",alpha=0.2)+
  geom_point(color="#cc2127",size=4)+
  geom_line(color="#cc2127",lwd=1)+
  geom_hline(yintercept=mean(y3),lty=2)+
  theme(panel.background = element_rect(fill="white",color="grey"),
        axis.title=element_text(size=20),
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"),
        panel.border = element_rect(color="black",fill="NA"),
        axis.text=element_text(size=17))+
  labs(y="Forest loss due to fire (Mha)")+
  scale_y_continuous(limits=c(0,12),expand=c(0,0))+
  annotate("text",x=2020,y=1.25,vjust=0,label="±2σ",size=6,color="#cc2127",fontface="bold.italic")+
  annotate("text",x=2020,y=3,vjust=0,label="±1σ",size=6,color="#cc2127",fontface="bold.italic")+
  annotate("text",x=2003,y=9,hjust=0,label="2000-2020 mean",size=6,color="#cc2127",fontface="bold.italic")+
  annotate("segment",x=2009,xend=2010,y=8.7,yend=m3,size=0.5)+  
  coord_cartesian(clip="off",xlim=c(2000,2020))+
  annotate("text",x=1996.5,y=12,label="C",size=8,fontface="bold")

library(gridExtra)
p4=grid.arrange(p1,p2,p3,ncol=3)
p4