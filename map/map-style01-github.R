# Source: DOI: 10.1016/j.lanwpc.2023.100826
# Date: 2023-06-23

library(ggplot2)
library(maps)
library(mapdata)
library(maptools)
library(plyr)
library(rgdal)
library(mapproj)
library(sf)
library(stringr)

china_map <- readOGR("D:/map-style01-github.shp")#Read the map file
china_map <- fortify(china_map)#Convert map file to data frame
#When you click in ↑↑↑, you will see that each id represents a region, and when you use the aes function below, fill=id means to fill the region corresponding to each id with color
name_label <- read.csv("D:/map-style01-data-github.csv")#load text labels for different regions，need the latitude and longitude corresponding to each text label
color <- c("#cca087","#e4c9b8","#c096aa","#eb6c35","white","#956a57","#84a570","#569dc9","white")#Specified colors set for different regions
ggplot(china_map, aes(x=long, y=lat, group=group))+#long, lat, and group can be viewed in the data frame above
  geom_polygon(aes(fill=id), color="black") +#Fill according to different areas (id), adjust the boundary line to black
  geom_text(data=name_label, aes(x=jd, y=wd, label=label, fontface = "bold"), size=3.5)+#Text labels are mapped according to latitude and longitude, and can be resized, thickened, and styled using "family="
  scale_fill_manual(values = color)+#Fill each area (id) with the specified color
  coord_map("polyconic")+#Change the map perspective, let the map become vivid and three-dimensional
  theme(panel.grid=element_blank(),
        panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        legend.position = "none")#Make backgrounds, grid, titles, legends, etc. blank
#Finally output the image to a height of at least 800