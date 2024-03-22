#Source DOI: 10.1016/S2352-3026(23)00170-9
#Date: 2023-07-21
#Code: Junxi Li

library(ggplot2)
library(gridExtra)
PIH_2nd <- read.csv("forestplot-style02-data1-github.csv")#Read data for each graph
GDM_2nd <- read.csv("forestplot-style02-data2-github.csv")
PPROM_2nd <- read.csv("forestplot-style02-data3-github.csv")
A <- ggplot(PIH_2nd, aes(x=Hb.g.L., y=RR))+
     scale_x_continuous(limits = c(55, 185), breaks = c(55,85,110,135,160,185), expand = c(0, 0))+#Set the X-axis range, ticks and not letting the sides extend
     scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2), expand = c(0, 0))+#Similar to the x-axis setup
     geom_hline(yintercept = 1, color = "gray50", linewidth = 0.5, linetype = "dashed")+#Add gray dashed line at y=1
     geom_errorbar(aes(ymin=lower, ymax=upper), width = 2.2, size = 0.5, color="#c02050")+#Add error bar and set the appearance
     geom_point(size=1.5, color="#c02050")+#add scatter and set the color
     labs(y="RR for pregnancy-induced\nhypertension", title = "A")+#Name the title
     theme(panel.background = element_blank(),#Remove background, grid
           axis.line = element_line(size = 0.5, color = "#262425"),#Set the appearance of coordinate axes
           axis.ticks = element_line(size = 0.5, color = "#262425"),#the appearance of ticks
           axis.ticks.length = unit(0.2, "cm"),#the length of ticks
           axis.title.y = element_text(margin = margin(r = 13.5), color = "#262425", size = 10),#Set the distance of the y-axis title from the y-axis
           axis.text.x = element_blank(),#remove x-axis text
           axis.text.y = element_text(color = "#262425"),#set the color of y-axis text
           plot.title = element_text(face = "bold", hjust = -0.066, margin = margin(b = 12)))+#Setting the position and appearance of the plot title
     xlab(NULL)#remove the title of x-axis

#plot B has the same setup as plot A, but the title and the title of y-axis
B <- ggplot(GDM_2nd, aes(x=Hb.g.L., y=RR))+
     scale_x_continuous(limits = c(55, 185), breaks = c(55,85,110,135,160,185), expand = c(0, 0))+
     scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2), expand = c(0, 0))+
     geom_hline(yintercept = 1, color = "gray50", linewidth = 0.5, linetype = "dashed")+
     geom_errorbar(aes(ymin=lower, ymax=upper), width = 2.2, size = 0.5, color="#c02050")+
     geom_point(size=1.5, color="#c02050")+
     labs(y="RR for gestational diabetes mellitus", title = "B")+
     theme(panel.background = element_blank(),
           axis.line = element_line(size = 0.5, color = "#262425"),
           axis.ticks = element_line(size = 0.5, color = "#262425"),
           axis.ticks.length = unit(0.2, "cm"),
           axis.title.y = element_text(margin = margin(r = 24), color = "#262425", size = 10),
           axis.text.x = element_blank(),
           axis.text.y = element_text(color = "#262425"),
           plot.title = element_text(face = "bold", hjust = -0.066, margin = margin(b = 12)))+
    xlab(NULL)

#Plot C is also roughly the same, but with the addition of the x-axis text, title
C <- ggplot(PPROM_2nd, aes(x=Hb.g.L., y=RR))+
     scale_x_continuous(limits = c(55, 185), breaks = c(55,85,110,135,160,185), expand = c(0, 0))+
     scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5), expand = c(0, 0))+
     geom_hline(yintercept = 1, color = "gray50", linewidth = 0.5, linetype = "dashed")+
     geom_errorbar(aes(ymin=lower, ymax=upper), width = 2.2, size = 0.5, color="#c02050")+
     geom_point(size=1.5, color="#c02050")+
     labs(x="Haemoglobin(g/L)", y="RR for preterm-premature\nrupture of membranes", title = "C")+
     theme(panel.background = element_blank(),
           axis.line = element_line(size = 0.5, color = "#262425"),
           axis.ticks = element_line(size = 0.5, color = "#262425"),
           axis.ticks.length = unit(0.2, "cm"),
           axis.title.x = element_text(margin = margin(t = 10), color = "#262425", size = 10),
           axis.title.y = element_text(margin = margin(r = 13), color = "#262425", size = 10),
           axis.text.x = element_text(color = "#262425"),
           axis.text.y = element_text(color = "#262425"),
           plot.title = element_text(face = "bold", hjust = -0.066, margin = margin(b = 12)))

grid.arrange(A, B, C, nrow=3)#Combine the three diagrams in a column of three rows
#The final input at a scale of 3:5 gives the figure in the article

#You may have noticed that the points on either side are slightly different from the diagram in the original article,
#In ggplot2, landing points on the limit range of the x-axis will result in an incomplete display of these points,
#So here can not be 100% restore the original image, but to do a little adjustment,
#you can use other drawing software, or R package to try to restore :)