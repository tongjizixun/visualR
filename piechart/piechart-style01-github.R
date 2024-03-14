#Source: DOI: 10.1016/S0140-6736(23)01701-4
#Date: 2024-30-14

library("ggplot2")
library("scales")
library("RColorBrewer")

mycolor=c(rgb(147,112,219,max=255),rgb(60,140,190,max=255),rgb(255,127,80,max=255),rgb(255,215,0,max=255),
          rgb(139,1,139,max=255),rgb(255,180,122,max=255),rgb(210,245,255,max=255),rgb(181,181,181,max=255))
data = data.frame(
  category = c("Breast", "Lung", "Cervix uteri", "Colorectum","Ovary","Liver","Stomach","Other cancers"),
  value = c(436298,291924,262728,159414,134351,133540,130600,777593))
value1=c(data$value/(sum(data$value)))
pie(data$value, paste(data$category,paste0(data$value,"(",percent(value1,2),")"),sep="\n"),
    main = "Premature cancer deaths among women aged 30–69 years", col = mycolor,radius = 1,
    clockwise = TRUE)
