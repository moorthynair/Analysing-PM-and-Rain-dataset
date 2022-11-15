library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(zoo)


patna <- read_excel("Patna_PM2.5.xlsx")
View(patna)

col1 = paste('data', seq(1,6), sep= '')
cols = c('Date', col1)

colnames(patna) = cols

patna$Date = dmy_hm(patna$Date)

patna = patna %>% mutate_if(is.character, as.numeric)


patna = patna %>% rowwise(Date) %>% summarise(Mean_PM2.5 = mean(c_across(everything()), na.rm=TRUE))

patna['Month'] = as.numeric(format(patna$Date, '%m'))
patna['Month'] = month.name[patna$Month]
patna['Year'] =  as.numeric(format(patna$Date, '%Y'))

FY_status1 = rep('2021-22', times=365)
FY_status2 = rep('2022-23', times=225)
patna$FY = c(FY_status1,FY_status2)

patna = patna %>% group_by(Month,Year,FY) %>% summarise_at(vars(Mean_PM2.5), mean ,na.rm=TRUE)
patna$Year = factor(patna$Year , levels = c('2021','2022'))
patna['Month'] = factor(patna$Month, levels=c('April', 'May', 'June', 'July', 'August', 'September','October', 'November','December', 'January','February','March'))


ggplot(patna, aes(x =Month, y = Mean_PM2.5, group =FY))+ geom_line(aes(color = FY),lwd=1.2)+
  geom_point(size = 3.5, color = 'black')+theme_bw()+ggtitle('Patna')+ labs(y = 'PM2.5')+
  theme(axis.text = element_text(face='bold', color='black', size=14),
        axis.title.y = element_text(face='bold', color='black', size=16),
        axis.title.x = element_blank(),
        legend.text = element_text(face='bold', color='black', size=14),
        legend.title = element_text(face='bold', color='black', size=16),
        axis.ticks = element_line(colour ='black', size=1.5),
        axis.ticks.length=unit(.25, "cm"),
        plot.title = element_text(face='bold', color='black', size=20, hjust = 0.5))+
  geom_text(aes(x=8.5, y =195, label = 'Data as on \n11.11.2022 \nis considered', fontface='bold'))+
  geom_segment(x=8.05, y =230, xend = 8.3, yend = 210,size=1.2,
               arrow = arrow(length = unit(.25, "cm")))