#######reference:DOI:https://doi.org/10.1016/S2542-5196(23)00127-4
#####date:2023-07-27

#world map
#install.packages("RColorBrewer")
#install.packages("tidyverse")
#install.packages("maps")
library(maps)#world map
library(ggplot2)
library(RColorBrewer)#palette
library(tidyverse)
library(openxlsx)
rs<-read.xlsx("D:/map-style02-data-github.xlsx")#The data is fictional
world_map <- map_data("world")
world_map$region<- gsub(" ",".",world_map$region)#Remove spaces from the name
#Unify the name of the data with the name of the World map
world_map$region[which(world_map$region=="Timor-Leste")]="Timor"
world_map$region[which(world_map$region=="Antigua")]="Antigua.and.Barbuda"
world_map$region[which(world_map$region=="Democratic.Republic.of.the.Congo")]="Democratic.Republic.of.Congo"
world_map$region[which(world_map$region=="Republic.of.Congo")]="Congo"
world_map$region[which(world_map$region=="UK")]="United.Kingdom"
world_map$region[which(world_map$region=="Swaziland")]="Eswatini"
world_map$region[which(world_map$region=="Micronesia")]="Micronesia.(country)"
world_map$region[which(world_map$region=="Kyrgyzstan")]="Kyrgyz.Republic"
world_map$region[which(world_map$region=="Saint.Kitts")]="Saint.Kitts.and.Nevis"
world_map$region[which(world_map$region=="North.Korea")]="Korea,.Dem..People's.Rep."
world_map$region[which(world_map$region=="South.Korea")]="Korea,.Rep."
world_map$region[which(world_map$region=="Sint.Maarten")]="Sint.Maarten.(Dutch.part)"
world_map$region[which(world_map$region=="Trinidad")]="Trinidad.and.Tobago"
world_map$region[which(world_map$region=="Tobago")]="Trinidad.and.Tobago"
world_map$region[which(world_map$region=="Turkey")]="Turkiye"
world_map$region[which(world_map$region=="USA")]="United.States"
world_map$region[which(world_map$region=="Saint.Vincent")]="Saint.Vincent.and.the.Grenadines"
world_map$region[which(world_map$region=="Slovakia")]="Slovak.Republic"
world_map$region[which(world_map$region=="Russia")]="Russian.Federation"

rs.world_map <-left_join(world_map,rs, by = "region")#Merge data by region
# Warning message:
#   In left_join(world_map, rs, by = "region") :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 98065 of `x` matches multiple rows in `y`.
# ℹ Row 13 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
rs.world_map<-rs.world_map[-c(2325:6982),]#Remove the Antarctic and Arctic


ggplot(rs.world_map, aes(long, lat, group = group))+#world map
  geom_polygon(aes(fill = Resistance.rate ), color = "black")+
  scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +#longtitude
  scale_y_continuous(breaks = seq(-90, 100, 30), labels = function(x){paste0(x, "°")}) +#latitude
  scale_fill_gradient2(low = "#1a9641",
                       mid = "#ffffbf",
                       high = "#d7191c", 
                       space = "Lab",
                       na.value = "white",
                       guide = "colourbar",
                       midpoint = 50,
                       limits = c(0, 100.1))+#Filling of continuous variable colors
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),#Clear the background
        legend.position = "bottom",
        legend.key.width = unit(2.5, "cm"),
        legend.key.height= unit(0.4, "cm"),#the parameters of legend
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 13))+#the size of legend title
  labs(subtitle = "D")+
  guides(fill = guide_colourbar(title="Resistance rate (%)",#the title of legend
                                title.position = "top"))#List the title of the legend above the color bar
