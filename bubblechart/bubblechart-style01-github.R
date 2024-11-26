#Source: DOI:  10.1097/JS9.0000000000001071
#Date: 2024-11-23
#Code: Yu Wang

library(ggplot2)  
library(dplyr)  
library(ggpubr) 
library(readxl)
data<-read_excel("D:/bubblechart-style01-data-github.xlsx")
data1<-data[data$style =="prevalence", ]
data2<-data[data$style =="incidence", ]
data3<-data[data$style =="death", ]
data4<-data[data$style =="DALYs", ]

#Divide bubble sizes into categories
data1 <- data1 %>%  
  mutate(ASR_Category = cut(N_val, 
                            breaks = c(0, 532448, 1073424, 1889111, 8852278),   
                            labels = c("<532,446", "532,448~1,073,424", 
                                       "1,073,424~1,889,111", "1,889,111~8,852,278"),  
                            include.lowest = TRUE))

# Get the Pearson correlation coefficient and P-value
cor_test_result <- cor.test(data1$SDI, data1$R_val, method = "pearson")  
pearson_r1 <- cor_test_result$estimate  
p_value1 <- cor_test_result$p.value 
p_value1 = ifelse(p_value1<0.001, '<0.001', sprintf("%.3f", p_value1))

#cartography picture A
pA <- ggplot(data1, aes(x = SDI, y = R_val)) +  
  geom_point(aes(size = ASR_Category), color = "#7BADDE", alpha = 0.7) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, color = "black")+
  theme_minimal() +  
  labs(  
    title = "A",  
    x = "SDI",  
    y = "ASR(per 100,000 population) of prevalence",  
    size = "Prevalence cases in 2019"   
  ) +  
  scale_size_manual(values = c("<532,446"= 1, 
                               "532,448~1,073,424"= 2.5, 
                               "1,073,424~1,889,111"= 4,
                               "1,889,111~8,852,278"= 5.5)) +  # Customize the bubble size  
  theme(  
    legend.position = c(0.85, 0.75),   
    legend.key.size = unit(0.5, "cm"),   
    legend.text = element_text(size = 7),    
    legend.title = element_text(size = 8),   
    legend.background = element_rect(fill = "white", color = NA),   
    legend.title.align = 0.5,  # 图例标题对齐  
    panel.border = element_rect(colour = "black", fill = NA, size = 1)  
  ) +  
  annotate("text", x = 0.43, y = 950, label = paste("R =", round(pearson_r1, 3)), size = 4, color = "black") +  
  annotate("text", x = 0.50, y = 950, label = paste("/P =", p_value1), size = 4, color = "black")  

#picture B
data2 <- data2 %>%  
  mutate(ASR_Category = cut(N_val, 
                            breaks = c(0, 178408, 439992, 724490, 3095824),   
                            labels = c("<178,408", "178,408~439,992", 
                                       "439,992~724,490", "724,490~3,095,824"),  
                            include.lowest = TRUE))

cor_test_result <- cor.test(data2$SDI, data2$R_val, method = "pearson")  
pearson_r2 <- cor_test_result$estimate  
p_value2 <- cor_test_result$p.value 
p_value2= ifelse(p_value2<0.001, '<0.001', sprintf("%.3f", p_value2))

pB <- ggplot(data2, aes(x = SDI, y = R_val)) +  
  geom_point(aes(size = ASR_Category), color = "#466891", alpha = 0.7) +   
  geom_smooth(method = "loess", span = 1, se = TRUE, color = "black")+   
  theme_minimal() +    
  labs(  
    title = "B",  
    x = "SDI",  
    y = "ASR(per 100,000 population) of incidence",  
    size = "Incidence cases in 2019"    
  ) +  
  scale_size_manual(values = c("<178,408" =1, 
                               "178,408~439,992" =2.5, 
                               "439,992~724,490" =4, 
                               "724,490~3,095,824"=5.5)) +    
  theme(  
    legend.position = c(0.77, 0.25),    
    legend.key.size = unit(0.5, "cm"),   
    legend.text = element_text(size = 7),   
    legend.title = element_text(size = 8),   
    legend.background = element_rect(fill = "white", color = NA),    
    legend.title.align = 0.5,    
    panel.border = element_rect(colour = "black", fill = NA, size = 1)    
  ) +  
  annotate("text", x = 0.43, y = 300, label = paste("R =", round(pearson_r2, 3)), size = 4, color = "black") +  
  annotate("text", x = 0.50, y = 300, label = paste("/P =", p_value2), size = 4, color = "black")  


#picture C
data3 <- data3 %>%  
  mutate(ASR_Category = cut(N_val, 
                            breaks = c(0, 502, 1077, 2726, 12747),
                            labels = c("<502", "502~1,077", 
                                       "1,077~2,726", "2,726~12,747"),  
                            include.lowest = TRUE))

cor_test_result <- cor.test(data3$SDI, data3$R_val, method = "pearson")  
pearson_r3 <- cor_test_result$estimate  
p_value3 <- cor_test_result$p.value 
p_value3 = ifelse(p_value3<0.001, '<0.001', sprintf("%.3f", p_value3))

pC <- ggplot(data3, aes(x = SDI, y = R_val)) +  
  geom_point(aes(size = ASR_Category), color = "#259E90", alpha = 0.7) +   
  geom_smooth(method = "loess", span = 1, se = TRUE, color = "black")+ 
  theme_minimal() + 
  labs(  
    title = "C",  
    x = "SDI",  
    y = "ASR(per 100,000 population) of death",  
    size = "Deaths cases in 2019"    
  ) +  
  scale_size_manual(values = c("<502" =1, 
                               "502~1,077" =2.5, 
                               "1,077~2,726" =4, 
                               "2,726~12,747"=5.5)) +    
  theme(  
    legend.position = c(0.85, 0.75),    
    legend.key.size = unit(0.5, "cm"),  
    legend.text = element_text(size = 7),    
    legend.title = element_text(size = 8),   
    legend.background = element_rect(fill = "white", color = NA),    
    legend.title.align = 0.5,    
    panel.border = element_rect(colour = "black", fill = NA, size = 1)    
  ) +   
  annotate("text", x = 0.43, y = 2.2, label = paste("R =", round(pearson_r3, 3)), size = 4, color = "black") +  
  annotate("text", x = 0.50, y = 2.2, label = paste("/P =", p_value3), size = 4, color = "black")  

#picture D
data4 <- data4 %>%  
  mutate(ASR_Category = cut(N_val, 
                            breaks = c(0, 51209, 92203, 193550, 919587),
                            labels = c("<51,209", "51,209~92,203", 
                                       "92,203~193,550", "193,550~919,587"),  
                            include.lowest = TRUE))

cor_test_result <- cor.test(data4$SDI, data4$R_val, method = "pearson")  
pearson_r4 <- cor_test_result$estimate  
p_value4 <- cor_test_result$p.value 
p_value4 = ifelse(p_value4<0.001, '<0.001', sprintf("%.3f", p_value4))

pD <- ggplot(data4, aes(x = SDI, y = R_val)) +  
  geom_point(aes(size = ASR_Category), color = "#EF9737", alpha = 0.7) +    
  geom_smooth(method = "loess", span = 1, se = TRUE, color = "black")+ 
  theme_minimal() +   
  labs(  
    title = "D",  
    x = "SDI",  
    y = "ASR(per 100,000 population) of DALYs",  
    size = "DALYs cases in 2019"    
  ) +  
  scale_size_manual(values = c("<51,209" =1, 
                               "51,209~92,203" =2.5, 
                               "92,203~193,550" =4, 
                               "193,550~919,587"=5.5)) +   
  theme(  
    legend.position = c(0.85, 0.75),  
    legend.key.size = unit(0.5, "cm"),   
    legend.text = element_text(size = 7),    
    legend.title = element_text(size = 8),   
    legend.background = element_rect(fill = "white", color = NA),    
    legend.title.align = 0.5,    
    panel.border = element_rect(colour = "black", fill = NA, size = 1)    
  ) + 
  annotate("text", x = 0.43, y = 93, label = paste("R =", round(pearson_r4, 3)), size = 4, color = "black") +  
  annotate("text", x = 0.50, y = 93, label = paste("/P =", p_value4), size = 4, color = "black")  

print(pD)  

#Stitch pictures
library(gridExtra)
grid.arrange(pA, pB, pC, pD, ncol = 2, nrow= 2)
