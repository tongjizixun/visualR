#Source：Stamm T A, Partheymüller J, Mosor E, et al. 2023. Determinants of COVID-19 vaccine fatigue. Nature Medicine [J], 29: 1164-1171.
#Date: 2023-06-02
#Code: Junxi Li

library(ggplot2)
Forest <- read.csv("D:/forestplot-style01-data-github.csv")#Data importing
Forest$names <- factor(Forest$names, levels = (rev(unique(Forest$names))))#The y-axis of the ggplot2 function is arranged from bottom to top, so the variables have to be reversed first in order to achieve variables in table order from top to bottom
Bold_names <- c(Forest$kind == "Attribute")[seq(from = 1, to = length(Forest$kind == "Attribute"), by = 2)]
ggplot(Forest, aes(x=Est., y=names, color=Sub.group))+
  geom_vline(xintercept = 0, color = "gray", size = 0.5)+ #Set the vertical line at x=0
  geom_point(size=1.5, position = position_dodge(width=0.7))+ #Set point sizes and stagger points in different subgroups
  geom_errorbar(aes(xmin=lower, xmax=upper), width = 0, size=0.5, position = position_dodge(width=0.7))+ #Cancellation of the plumb line on both sides of the error line as well as setting the size of the error line, staggered
  theme(axis.text.y = element_text(face = ifelse(rev(Bold_names), "bold", "plain")), #bold the target variables
        panel.background = element_blank(), #Set the axis background blank
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), #Set frame lines of axis
        plot.title = element_text(hjust = 0.6, vjust = 0), #Move the title
        legend.position = "bottom", #Place the legend at the bottom
        legend.key = element_rect(fill = "white"))+ #Legend icon background set to white/blank
  coord_cartesian(xlim = c(-0.25, 0.25))+ #Adjustment of the displayed x-axis range
  labs(x="Estimated AMCE", y="", title = "Evaluation of campaign (binary choice)", color = "GenderP")+ #Naming
  scale_color_manual(values = c("pink", "blue")) #Set the color of the subgroup

# Warning messages:
# 1: Vectorized input to `element_text()` is not officially supported.
# Results may be unexpected or may change in future versions of ggplot2. 
# 2: Removed 10 rows containing missing values (geom_point). 