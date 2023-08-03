#source DOI:10.1016/S0140-6736(23)01406-X
#date:2023-07-25
#using the built-in dataset 'lung' in R
library(survminer) 
library(survival)
library(ggplot2)

custom_theme <- function() {
  theme_survminer() %+replace%
    theme(
      plot.title=element_text(hjust=-0.08)
    )
}   #Set the title "number at risk" to left alignment.

fit <- survfit(Surv(time,status) ~ sex, data = lung) #Fit survival curve.
g <- ggsurvplot(fit, data = lung, ##Using the built-in lung dataset in R
           fun = "cumhaz",
           risk.table = TRUE,     #Generate a risk table
           risk.table.y.text.col=FALSE,
           risk.table.fontsize=4,
           risk.table.height=0.2,
           pval="Log-rank P=0.52",
           pval.size=4,
           pval.coord=c(450,3.1),
           linetype = "dashed",
           surv.plot.height = 0.7, surv.plot.width = 0.7,
           risk.table.position = "none",
           xlab = "Years from randomisation", 
           ylab ="Cumulative proportion with incident cognitive impairment",
           font.x = 12,
           font.y = 12,
           ggtheme=custom_theme(),
           legend = c(0.1,0.85), # Specify the legend position
           legend.title = " ", # Set legend title
           legend.labs = c("Control", "Intervent")) # Specify legend group labels
g$table$theme$axis.title.x <- element_blank()    #Remove the axes and lines of the risk table
g$table$theme$axis.line.x <- element_blank()
g$table$theme$axis.line.y <- element_blank()
g$table$theme$axis.text.x <- element_blank()
g$table$theme$axis.ticks.x <- element_blank()
g$table$theme$axis.ticks.y <- element_blank()

g$plot <- g$plot+ ggtitle("Secondary outcome Total(N=977)") +
  theme(plot.title = element_text(hjust = 0.5))  #Add plot title and center-align

print(g)

