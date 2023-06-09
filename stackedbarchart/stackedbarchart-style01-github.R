#source：Sophie Welsche, Emmanuel C Mrimi, Efficacy and safety of moxidectin and albendazole compared with ivermectin and albendazole coadministration in adolescents infected with Trichuris trichiura in Tanzania: an open-label, non-inferiority, randomised, controlled, phase 2/3 trial, The Lancet Infectious Diseases
#date：2023-06-08
# create a data frame
df <- data.frame(
  Variable = rep(c(
    "Headache","Abdominal pain","Itching","Dizziness","Musculoskeletal pain",
    "Muscle weakness","Rash","Nausea","Diarrhoea","Cough","Constipation",
    "Vomiting","Other*"
  ), each = 4), 
  Group = rep(c(
    "Ivermectin and albendazole group", "Ivermectin and albendazole group","Moxidectin and albendazole group","Moxidectin and albendazole group"
  ), times=13),
  Event = rep(rep(c(
    "Mild adverse events", "Moderate adverse events"
  ), times = 2), times=13)
)

# add a colume named "value"
df$value<- c(
  67, 7, 42, 11, 24, 6, 27, 4, 13, 3, 14, 4, 11,   
  1, 8, 2, 7, 1, 6, 4, 9, 0, 6, 1, 6, 1, 
  6, 2, 6, 0, 5, 0, 4, 1, 3, 0, 3, 4, 3, 
  1, 1, 0, 4, 1, 0, 0, 1, 0, 12, 8, 10, 6
)

print(df)

#convert the variable to an ordered factor
df$Variable <- factor(df$Variable, levels = rev(unique(df$Variable)))

#create a bar plot
library(ggplot2)
ggplot(df, aes(x = Variable, y = value, fill = Group, group = Group)) +
  geom_col(aes(fill = interaction(Group, Event)), color="black",position = position_dodge(width = 0.6), width = 0.6) +
  scale_fill_manual(values = c("#FF8888", "#88AAFF", "#FF3333", "#6699FF"), 
  labels = c("Ia Mild adverse events", "Ma Mild adverse events", "Ia Moderate adverse events","Ma Moderate adverse events")                  
      )+  
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Total number of reported adverse events") +
  theme_bw() +
  theme(legend.position = c(0.95, 0.20),  
        legend.justification = c(1, 0), 
        legend.key.size = unit(0.3, "cm"), 
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 80), breaks = seq(0, 80, 5))+
  scale_x_discrete(expand = c(-0.1, -0.1))+
  coord_flip()