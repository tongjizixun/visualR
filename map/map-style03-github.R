#Source：Doi: 10.1016/j.jinf.2024.106250
#Date：2024-11-23
#Code: Yu Wang

library(ggmap)
library(maps)
library(dplyr)
library(readr)
EC <- read.csv("D:/map-style03-data-github.csv",header = T)  
APC_2021 <- subset(EC,EC$year=='1990-2021' & 
                     EC$metric== 'APC') 
APC_2021$val <- round(APC_2021$val,2) 
APC_2021$lower <- round(APC_2021$lower,2) 
APC_2021$upper <- round(APC_2021$upper,2) 
colnames(APC_2021)<-c("location","year","metric","val","lower","upper")
####  map for ASR
worldData <- map_data('world')
country_apc <- APC_2021
country_apc$location <- as.character(country_apc$location) 
###The purpose of the following code is to make the country name of the country_asr$location match the country name of the worldData
### Make your data mapped onto a map
country_apc$location[country_apc$location == 'United States of America'] = 'USA'
country_apc$location[country_apc$location == 'Russian Federation'] = 'Russia'
country_apc$location[country_apc$location == 'United Kingdom'] = 'UK'
country_apc$location[country_apc$location == 'Congo'] = 'Republic of Congo'
country_apc$location[country_apc$location == "Iran (Islamic Republic of)"] = 'Iran'
country_apc$location[country_apc$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_apc$location[country_apc$location == "Taiwan (Province of China)"] = 'Taiwan'
country_apc$location[country_apc$location == "Republic of Korea"] = 'South Korea'
country_apc$location[country_apc$location == "United Republic of Tanzania"] = 'Tanzania'
country_apc$location[country_apc$location == "C?te d'Ivoire"] = 'Saint Helena'
country_apc$location[country_apc$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_apc$location[country_apc$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_apc$location[country_apc$location == "Czechia"] = 'Czech Republic'
country_apc$location[country_apc$location == "Republic of Moldova"] = 'Moldova'
country_apc$location[country_apc$location == "Viet Nam"] = 'Vietnam'
country_apc$location[country_apc$location == "Lao People's Democratic Republic"] = 'Laos'
country_apc$location[country_apc$location == "Syrian Arab Republic"] = 'Syria'
country_apc$location[country_apc$location == "North Macedonia"] = 'Macedonia'
country_apc$location[country_apc$location == "Micronesia (Federated States of)"] = 'Micronesia'
country_apc$location[country_apc$location == "Macedonia"] = 'North Macedonia'
country_apc$location[country_apc$location == "Trinidad and Tobago"] = 'Trinidad'
country_apc <- rbind(country_apc,country_apc[country_apc$location == "Trinidad",])
country_apc$location[country_apc$location == "Trinidad"] = 'Tobago'
country_apc$location[country_apc$location == "Cabo Verde"] = 'Cape Verde'
country_apc$location[country_apc$location == "United States Virgin Islands"] = 'Virgin Islands'
country_apc$location[country_apc$location == "Antigua and Barbuda"] = 'Antigu'
country_apc <- rbind(country_apc,country_apc[country_apc$location == "Antigu",])
country_apc$location[country_apc$location == "Antigu"] = 'Barbuda'
country_apc$location[country_apc$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
country_apc <- rbind(country_apc,country_apc[country_apc$location == "Saint Kitts",])
country_apc$location[country_apc$location == "Saint Kitts"] = 'Nevis'
country_apc$location[country_apc$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_apc$location[country_apc$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
country_apc <- rbind(country_apc,country_apc[country_apc$location == "Saint Vincent",])
country_apc$location[country_apc$location == "Saint Vincent"] = 'Grenadines'
country_apc$location[country_apc$location == "Eswatini"] = 'Swaziland'
country_apc$location[country_apc$location == "Brunei Darussalam"] = 'Brunei'
country_apc$location[country_apc$location == "Coted'Ivoire"] = "Ivory Coast"


worldData <- map_data('world')
worldData<-worldData[-c(2325:6982),]
total <- full_join(worldData,country_apc,by = c('region'='location'))

p <- ggplot()
total <- total %>% mutate(val2 = cut(val, breaks = c(-1,-0.14,-0.09,-0.06,-0.03,0.01,2.05),
                                     labels = c("＜-0.14","-0.14 to ＜-0.09","-0.09 to ＜-0.06",
                                                "-0.06 to ＜-0.03","-0.03 to ＜0.01",
                                                "0.01 to ＜2.05"),  
                                     include.lowest = T,right = T))
p2 <- p + geom_polygon(data=total, 
                       aes(x=long, y=lat, group = group,fill=val2),
                       colour="black",linewidth = .2) + 
  scale_fill_manual(values=c("#1C54A0","#4685BB","#9BBDD8",
                             "#F3AB84","#C45F49","#910029"))+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='Net drift of incidence'))+
  theme( legend.position = "inside",
         legend.position.inside = c(.06,.25))
p2 <- p2 + ggtitle("B")+
  theme(plot.title = element_text(color = "black",
                                  size=15,
                                  family = "serif", 
                                  face = "bold",     
                                  hjust = 0.02,          
                                  vjust = 0,          
                                  angle = 0))         
#Caribbean and central america
p3 <- p2+
  coord_sf(xlim = c(-59,-92),ylim = c(27,7))+
  theme(legend.position = "none",
        panel.border = element_rect(color = "grey",linewidth = 1,fill=NA),
        plot.margin = margin(t = 0.5,  
                             r = 0.5,  
                             b = 0.5,  
                             l = 0.5,  
                             unit = "mm"))
p3 <- p3 + ggtitle("Caribbean and central america")+
  theme(plot.title = element_text(color = "black",
                                  size=10,
                                  family = "serif", 
                                  face = "bold",     
                                  hjust = 0,         
                                  vjust = 0.5,          
                                  angle = 0))         
#Persian gulf
p4 <- p2+
  coord_sf(xlim = c(48,56),ylim = c(30,22))+
  theme(legend.position = "none",
        panel.border = element_rect(color = "grey",linewidth = 1,fill=NA),
        plot.margin = margin(t = 0.5,  
                             r = 0.5,  
                             b = 0.5,  
                             l = 0.5,  
                             unit = "mm"))
p4 <- p4 + ggtitle("Persian gulf")+
  theme(plot.title = element_text(color = "black",
                                  size=10,
                                  family = "serif", 
                                  face = "bold",    
                                  hjust = 0,          
                                  vjust = 0.5,          
                                  angle = 0))         
#Balkan Peninsula
p5 <- p2+
  coord_sf(xlim = c(12,30),ylim = c(33,45))+
  theme(legend.position = "none",
        panel.border = element_rect(color = "grey",linewidth = 1,fill=NA),
        plot.margin = margin(t = 0.5,  
                             r = 0.5,  
                             b = 0.5,  
                             l = 0.5,  
                             unit = "mm"))
p5 <- p5 + ggtitle("Balkan Peninsula")+
  theme(plot.title = element_text(color = "black",
                                  size=10,
                                  family = "serif", 
                                  face = "bold",     
                                  hjust = 0,         
                                  vjust = 0.5,          
                                  angle = 0))        
#southeast Asia
p6 <- p2+
  coord_sf(xlim = c(92,130),ylim = c(10,-12))+
  theme(legend.position = "none",
        panel.border = element_rect(color = "grey",linewidth = 1,fill=NA),
        plot.margin = margin(t = 0.5, 
                             r = 0.5,  
                             b = 0.5, 
                             l = 0.5,  
                             unit = "mm"))
p6 <- p6 + ggtitle("Southeast Asia")+
  theme(plot.title = element_text(color = "black",
                                  size=10,
                                  family = "serif", 
                                  face = "bold",     
                                  hjust = 0,          
                                  vjust = 0.5,          
                                  angle = 0))         
#West Africa
p7 <- p2+
  coord_sf(xlim = c(-15,-5),ylim = c(4,14))+
  theme(legend.position = "none",
        panel.border = element_rect(color = "grey",linewidth = 1,fill=NA),
        plot.margin = margin(t = 0.5,  
                             r = 0.5,  
                             b = 0.5,  
                             l = 0.5,  
                             unit = "mm"))
p7 <- p7 + ggtitle("West Africa")+
  theme(plot.title = element_text(color = "black",
                                  size=10,
                                  family = "serif", 
                                  face = "bold",     
                                  hjust = 0,          
                                  vjust = 0.5,          
                                  angle = 0))         
#eastern Mediterranean
p8 <- p2+
  coord_sf(xlim = c(32,37),ylim = c(35,30))+
  theme(legend.position = "none",
        panel.border = element_rect(color = "grey",linewidth = 1,fill=NA),
        plot.margin = margin(t = 0.5,  
                             r = 0.5,  
                             b = 0.5,  
                             l = 0.5,  
                             unit = "mm"))
p8 <- p8 + ggtitle("Eastern \n Mediterranean")+
  theme(plot.title = element_text(color = "black",
                                  size=10,
                                  family = "serif", 
                                  face = "bold",     
                                  hjust = 0,         
                                  vjust = 0.5,         
                                  angle = 0))        
#northern Europe
p9 <- p2+
  coord_sf(xlim = c(5,35),ylim = c(60,48))+
  theme(legend.position = "none",
        panel.border = element_rect(color = "grey",linewidth = 1,fill=NA),
        plot.margin = margin(t = 0.5,  
                             r = 0.5,  
                             b = 0.5,  
                             l = 0.5,  
                             unit = "mm"))
p9 <- p9 + ggtitle("Northern Europe")+
  theme(plot.title = element_text(color = "black",
                                  size=10,
                                  family = "serif", 
                                  face = "bold",     
                                  hjust = 0.3,         
                                  vjust = 0.5,         
                                  angle = 0))         

library(patchwork)
p10 <- p7+p8-p9+plot_layout(ncol = 1,heights=c(15/12,1))
final_plot = p3+p4+p5+p6+plot_layout(ncol=4,widths = c(3/2,1,15/10,19/11))
layout <- "
AAAAAAAAAAAAAA
AAAAAAAAAAAAAA
AAAAAAAAAAAAAA
AAAAAAAAAAAAAA
AAAAAAAAAAAAAA
AAAAAAAAAAAAAA
BBBBBBBBBBBFFF
BBBBBBBBBBBFFF
"
pB <- p2 + final_plot + p10 + plot_layout(design = layout)
pB
