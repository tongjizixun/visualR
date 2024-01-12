#source: DOI:10.1016/j.scitotenv.2023.165682

#load packages
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(tidyverse) # Easily Install and Load the 'Tidyverse'

#load data, the data is fictive
df <- read.table("D:/barchart-style04-data-github.txt", header = 1, check.names = F, sep = "\t")
df$group <- factor(df$group,levels = c("M","N","G","F"))

#compute the angle of the label
df2 <- as.data.frame(df[c(1:10,51:62,103:114,155:166,207:208),]) 
rownames(df2) <- 1:48
df2$group <- factor(df2$group,levels = c("M","N","G","F"))
df2$ID <- as.numeric(rownames(df2))
number_of_bar <- nrow(df2)
angle <-  90 - 360 * (df2$ID-0.5)/number_of_bar
df2$hjust<-ifelse(angle <- 90, 1, 0)
df2$angle<-ifelse(angle <- 90, angle+180, angle)

#determine the location of the significance sign(*)
result <- aggregate(value ~ group3, data = df, sum)

#create the label and location
df3 <- df2 %>% 
  group_by(group) %>% 
  summarize(start=min(ID), end=max(ID) - 4) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
df3$group <- factor(df3$group,levels = c("M","N","G","F"))

#plot
ggplot()+
  #subline
  geom_hline(yintercept = 0,lty="solid", color = "black",linewidth=0.6)+
  geom_hline(yintercept = 20,lty="solid", color = "grey80")+
  geom_hline(yintercept = 60,lty="solid", color = "grey80")+
  geom_hline(yintercept = 40,lty="solid", color = "grey80")+
  geom_hline(yintercept = 80,lty="solid", color = "grey80")+
  geom_hline(yintercept = 100,lty="solid", color = "grey80")+
  #barchart
  geom_col(df, mapping=aes(group3, value, fill = group), color = "grey20", linewidth = 0.5, width = 0.8)+
  #the limit of y axis
  scale_y_continuous(limits = c(-25,150))+
  #color
  scale_fill_manual(values = c("#4fbb98","#f46024","#dd6ab0","#7c8ebe"))+
  #theme
  theme_void()+
  theme(legend.position = 'none')+
  #add label
  geom_text(data=df2, aes(x=ID, y=103, label=c("Contral", "ADOM", "LHy1","LHA", "LFA","SHy1","SHA","SFA","RHy1","RHA","  "," ",
                                               "Contral", "ADOM", "LHy1","LHA", "LFA","SHy1","SHA","SFA","RHy1","RHA","  ","  ",
                                               "Contral", "ADOM", "LHy1","LHA", "LFA","SHy1","SHA","SFA","RHy1","RHA","  ","  ",
                                               "Contral", "ADOM", "LHy1","LHA", "LFA","SHy1","SHA","SFA","RHy1","RHA","  ","  "),
                          hjust=hjust,color=group), 
            fontface="bold", size=3, 
            angle= df2$angle, inherit.aes = F)+
  #add star *
  geom_text(data=df2, aes(x=ID, y=result$value+4, 
                          label=c("", "", "***","", "","","**","","","","","",
                                  "", "", "**","", "","","","***","","","","",
                                  "", "***", "***","***", "**","***","**","***","**","***","","",
                                  "", "***", "","", "","","","**","**","","",""),
                          color=group), 
            fontface="bold", size=4, 
            angle= df2$angle+90, inherit.aes = F)+ #标签
  #add text and title
  geom_text(data=df2,x=12,y=80, label="Biodegradation rate(%)",color="black",size=3.5)+
  geom_text(data=df2, x=-0.2,y=5,label="0",color="black",size=3)+
  geom_text(data=df2, x=-0.2,y=25,label="20",color="black",size=3)+
  geom_text(data=df2, x=-0.2,y=45,label="40",color="black",size=3)+
  geom_text(data=df2, x=-0.2,y=65,label="60",color="black",size=3)+
  geom_text(data=df2, x=-0.2,y=85,label="80",color="black",size=3)+
  geom_text(data=df2, x=-0.2,y=105,label="100",color="black",size=3)+
  #Polar transformation
  coord_polar(direction=1)+
  #group label
  geom_text(data=df3, aes(x = title, y = 140, label=group,color=group), 
            hjust=c(1,1,0,0), angle=c(335,250,135,60), size=5, 
            fontface="bold", inherit.aes = F)+
  #color
  scale_color_manual(values = c("#4fbb98","#f46024","#dd6ab0","#7c8ebe"))
