library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)
library(zoo)


patna <- read_excel("Patna_rain.xlsx")
View(patna)

col1 = paste('data', seq(1,6), sep= '')
cols = c('Date', col1)

colnames(patna) = cols

patna$Date = dmy_hm(patna$Date)

patna = patna %>% mutate_if(is.character, as.numeric)


patna = patna %>% rowwise(Date) %>% summarise(Mean_rainfall = mean(c_across(everything()), na.rm=TRUE))

patna['Month'] = as.numeric(format(patna$Date, '%m'))
patna['Month'] = month.name[patna$Month]
patna['Year'] =  as.numeric(format(patna$Date, '%Y'))
patna = patna[1:590, ]
patna_daily = patna


FY_status1 = rep('2021-22', times=365)
FY_status2 = rep('2022-23', times=225)
patna$FY = c(FY_status1,FY_status2)

patna = patna %>% group_by(Month,Year,FY) %>% summarise_at(vars(Mean_rainfall), mean ,na.rm=TRUE)
patna$Year = factor(patna$Year , levels = c('2021','2022'))
patna['Month'] = factor(patna$Month, levels=c('April', 'May', 'June', 'July', 'August', 'September','October', 'November','December', 'January','February','March'))

## plotting rainfall 
ggplot(patna, aes(x =Month, y = Mean_rainfall, group =FY))+ geom_line(aes(color = FY),lwd=1.2)+
  geom_point(size = 3.5, color = 'black')+theme_bw()+ggtitle('Patna')+ labs(y = 'Rainfall')+
  theme(axis.text = element_text(face='bold', color='black', size=14),
        axis.title.y = element_text(face='bold', color='black', size=16),
        axis.title.x = element_blank(),
        legend.text = element_text(face='bold', color='black', size=14),
        legend.title = element_text(face='bold', color='black', size=16),
        axis.ticks = element_line(colour ='black', size=1.5),
        axis.ticks.length=unit(.25, "cm"),
        plot.title = element_text(face='bold', color='black', size=20, hjust = 0.5))


## plotting rainfall day count 
rf_days = patna_daily %>% filter(Mean_rainfall>0) %>% group_by(Month,FY) %>% count()
rf_days['Month'] = factor(rf_days$Month, levels=c('April', 'May', 'June', 'July', 'August', 'September','October', 'November','December', 'January','February','March'))

ggplot(rf_days, aes(x = Month,y =n, group =FY, fill=FY))+
  geom_bar(stat ='identity', position = 'dodge', color='black')+theme_bw()+ggtitle('Patna')+ labs(y = 'Rainfall (days)')+
  theme(axis.text = element_text(face='bold', color='black', size=14),
        axis.title.y = element_text(face='bold', color='black', size=16),
        axis.title.x = element_blank(),
        legend.text = element_text(face='bold', color='black', size=14),
        legend.title = element_text(face='bold', color='black', size=16),
        axis.ticks = element_line(colour ='black', size=1.5),
        axis.ticks.length=unit(.25, "cm"),
        plot.title = element_text(face='bold', color='black', size=20, hjust = 0.5))


