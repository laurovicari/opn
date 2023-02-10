# Análises gráficas de idade

library (dplyr)
library (ggplot2)

ENEM_PN <- read.csv("C:/Users/Lauro Vicari/Desktop/R/Ponte Nova/ENEM_PN/ENEM_PN_2013_2019.csv", sep = ";", header = T, 
         stringsAsFactors = FALSE)

            data <- data.frame(X2013 = c(
             sum(ENEM_PN$NU_ANO == 2013 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE < 17),
             sum(ENEM_PN$NU_ANO == 2013 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(17,18)),
             sum(ENEM_PN$NU_ANO == 2013 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(19:25)),
             sum(ENEM_PN$NU_ANO == 2013 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(26:30)),
             sum(ENEM_PN$NU_ANO == 2013 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(31:41)),
             sum(ENEM_PN$NU_ANO == 2013 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE > 41)),
                            X2014 = c(
               sum(ENEM_PN$NU_ANO == 2014 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE < 17),
               sum(ENEM_PN$NU_ANO == 2014 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(17,18)),
               sum(ENEM_PN$NU_ANO == 2014 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(19:25)),
               sum(ENEM_PN$NU_ANO == 2014 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(26:30)),
               sum(ENEM_PN$NU_ANO == 2014 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(31:41)),
               sum(ENEM_PN$NU_ANO == 2014 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE > 41)),
                            X2015 = c(
               sum(ENEM_PN$NU_ANO == 2015 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE < 17),
               sum(ENEM_PN$NU_ANO == 2015 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(17,18)),
               sum(ENEM_PN$NU_ANO == 2015 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(19:25)),
               sum(ENEM_PN$NU_ANO == 2015 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(26:30)),
               sum(ENEM_PN$NU_ANO == 2015 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(31:41)),
               sum(ENEM_PN$NU_ANO == 2015 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE > 41)),
                            X2016 = c(
               sum(ENEM_PN$NU_ANO == 2016 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE < 17),
               sum(ENEM_PN$NU_ANO == 2016 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(17,18)),
               sum(ENEM_PN$NU_ANO == 2016 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(19:25)),
               sum(ENEM_PN$NU_ANO == 2016 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(26:30)),
               sum(ENEM_PN$NU_ANO == 2016 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(31:41)),
               sum(ENEM_PN$NU_ANO == 2016 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE > 41)),
                            X2017 = c(
               sum(ENEM_PN$NU_ANO == 2017 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE < 17),
               sum(ENEM_PN$NU_ANO == 2017 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(17,18)),
               sum(ENEM_PN$NU_ANO == 2017 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(19:25)),
               sum(ENEM_PN$NU_ANO == 2017 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(26:30)),
               sum(ENEM_PN$NU_ANO == 2017 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(31:41)),
               sum(ENEM_PN$NU_ANO == 2017 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE > 41)),
                            X2018 = c(
               sum(ENEM_PN$NU_ANO == 2018 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE < 17),
               sum(ENEM_PN$NU_ANO == 2018 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(17,18)),
               sum(ENEM_PN$NU_ANO == 2018 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(19:25)),
               sum(ENEM_PN$NU_ANO == 2018 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(26:30)),
               sum(ENEM_PN$NU_ANO == 2018 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(31:41)),
               sum(ENEM_PN$NU_ANO == 2018 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE > 41)),
                            X2019 = c(
               sum(ENEM_PN$NU_ANO == 2019 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE < 17),
               sum(ENEM_PN$NU_ANO == 2019 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(17,18)),
               sum(ENEM_PN$NU_ANO == 2019 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(19:25)),
               sum(ENEM_PN$NU_ANO == 2019 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(26:30)),
               sum(ENEM_PN$NU_ANO == 2019 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE %in% c(31:41)),
               sum(ENEM_PN$NU_ANO == 2019 & ENEM_PN$TP_PRESENCA_CH == 1 & ENEM_PN$TP_PRESENCA_CH ==1 & ENEM_PN$TP_PRESENCA_LC == 1 & ENEM_PN$TP_PRESENCA_MT == 1 & ENEM_PN$NU_IDADE > 41)))
             
             
rownames(data) <- c("< 17", "17 e 18", "19 a 25", "26 a 30", "31 a 41", "> 41")


data <- data %>% mutate (X2013 = X2013/sum(X2013))
data <- data %>% mutate (X2014 = X2014/sum(X2014))
data <- data %>% mutate (X2015 = X2015/sum(X2015))
data <- data %>% mutate (X2016 = X2016/sum(X2016))
data <- data %>% mutate (X2017 = X2017/sum(X2017))
data <- data %>% mutate (X2018 = X2018/sum(X2018))
data <- data %>% mutate (X2019 = X2019/sum(X2019))

colnames(data) <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019")

data <- round(data, digits = 4)

data <- as.matrix(data)

data <- data * 100
             


library(RColorBrewer)
coul <- brewer.pal(7, "Dark2") 

# Transform this data in %
data_percentage <- apply(data, 2, function(x){x*100/sum(x,na.rm=T)})

# Make a stacked barplot--> it will be in %!

par(oma = c(3,2,0,4.5))
par (mar = c(3,4,6,5)) 

graph <- barplot(data_percentage, col=coul , border="white", xlab="Ano")

legend("right", inset = c(-0.18, -0.15), pch = 15, pt.cex = 3, legend = c("< 17", "17 e 18", "19 a 25", "26 a 30", "31 a 41", "> 41"), xpd = TRUE, 
       horiz = FALSE, col = coul, bty = "n", border = "black")


text(x = 0.7, y = 4.5, labels = paste0(data[1,1], "%"), cex = 0.8)
text(x = 0.7, y = 21, labels = paste0(data[2,1], "%"), cex = 0.8)
text(x = 0.7, y = 58.5, labels = paste0(data[3,1], "%"), cex = 0.8)
text(x = 0.7, y = 84, labels = paste0(data[4,1], "%"), cex = 0.8)
text(x = 0.7, y = 92, labels = paste0(data[5,1], "%"), cex = 0.8)
text(x = 0.7, y = 98.9, labels = paste0(data[6,1], "%"), cex = 0.8)

text(x = 1.9, y = 4.5, labels = paste0(data[1,2], "%"), cex = 0.8)
text(x = 1.9, y = 20, labels = paste0(data[2,2], "%"), cex = 0.8)
text(x = 1.9, y = 55, labels = paste0(data[3,2], "%"), cex = 0.8)
text(x = 1.9, y = 81, labels = paste0(data[4,2], "%"), cex = 0.8)
text(x = 1.9, y = 90, labels = paste0(data[5,2], "%"), cex = 0.8)
text(x = 1.9, y = 98.4, labels = paste0(data[6,2], "%"), cex = 0.8)

text(x = 3.15, y = 4.5, labels = paste0(data[1,3], "%"), cex = 0.8)
text(x = 3.15, y = 20.5, labels = paste0(data[2,3], "%"), cex = 0.8)
text(x = 3.15, y = 55, labels = paste0(data[3,3], "%"), cex = 0.8)
text(x = 3.15, y = 81, labels = paste0(data[4,3], "%"), cex = 0.8)
text(x = 3.15, y = 91, labels = paste0(data[5,3], "%"), cex = 0.8)
text(x = 3.15, y = 98.4, labels = paste0(data[6,3], "%"), cex = 0.8)

text(x = 4.31, y = 4.5, labels = paste0(data[1,4], "%"), cex = 0.8)
text(x = 4.31, y = 21, labels = paste0(data[2,4], "%"), cex = 0.8)
text(x = 4.31, y = 60, labels = paste0(data[3,4], "%"), cex = 0.8)
text(x = 4.31, y = 86, labels = paste0(data[4,4], "%"), cex = 0.8)
text(x = 4.31, y = 93, labels = paste0(data[5,4], "%"), cex = 0.8)
text(x = 4.31, y = 98.6, labels = paste0(data[6,4], "%"), cex = 0.8)

text(x = 5.51, y = 4.4, labels = paste0(data[1,5], "%"), cex = 0.8)
text(x = 5.51, y = 20, labels = paste0(data[2,5], "%"), cex = 0.8)
text(x = 5.51, y = 60, labels = paste0(data[3,5], "%"), cex = 0.8)
text(x = 5.51, y = 87, labels = paste0(data[4,5], "%"), cex = 0.8)
text(x = 5.51, y = 93.2, labels = paste0(data[5,5], "%"), cex = 0.8)
text(x = 5.51, y = 98.6, labels = paste0(data[6,5], "%"), cex = 0.8)

text(x = 6.7, y = 4.4, labels = paste0(data[1,6], "%"), cex = 0.8)
text(x = 6.7, y = 22, labels = paste0(data[2,6], "%"), cex = 0.8)
text(x = 6.7, y = 61, labels = paste0(data[3,6], "%"), cex = 0.8)
text(x = 6.7, y = 87.9, labels = paste0(data[4,6], "%"), cex = 0.8)
text(x = 6.7, y = 93.4, labels = paste0(data[5,6], "%"), cex = 0.8)
text(x = 6.7, y = 98.9, labels = paste0(data[6,6], "%"), cex = 0.8)

text(x = 7.9, y = 4.4, labels = paste0(data[1,7], "%"), cex = 0.8)
text(x = 7.9, y = 23, labels = paste0(data[2,7], "%"), cex = 0.8)
text(x = 7.9, y = 63, labels = paste0(data[3,7], "%"), cex = 0.8)
text(x = 7.9, y = 88.1, labels = paste0(data[4,7], "%"), cex = 0.8)
text(x = 7.9, y = 93.4, labels = paste0(data[5,7], "%"), cex = 0.8)
text(x = 7.9, y = 98.9, labels = paste0(data[6,7], "%"), cex = 0.8)

mtext("Idade dos candidatos ao ENEM em Ponte Nova", 
      side=3, 
      line=3, 
      cex=1.8,
      adj = 0.0,
      col="black") 

mtext("Candidatos que compareceram aos dois dias de prova (2019)", 
      side=3, 
      line=2, 
      cex=1,
      adj = 0.0,
      col="black") 

mtext("Fonte: Microdados do Enem (Inep)", 
      side=1, 
      line=0, 
      adj=0.15, 
      cex=0.8, 
      col="black", 
      outer=TRUE)  
