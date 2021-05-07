library(tidyverse)
library(dslabs)
#data(us_accidents.csv)
dui <- read.csv(file = "C:/Users/Chad Stephens/Desktop/Excel Data Workbooks/DUI.csv",header = TRUE)
head(dui)
DUI_p <- as.data.frame(dui)
DUI_p <- DUI_p %>% mutate(fat_rate = Fatalities/10000) #fat_rate is fatalities 10000 people
head(DUI_p)
t.test(DUI_p$DUI,DUI_p$fat_rate)
#this shows that there is a significant relationships between DUI's and fatale car accidents

dui_g <- DUI_p %>% ggplot()
#data <- DUI %>% data.frame(Fatalities,DUI)

dui_g + geom_point(aes(DUI,fat_rate))+labs(y="Fatality Rate",title = "Correlation between Fatality Rate & DUI")

t.test(DUI_p$Population,DUI_p$fat_rate)
dui_g + geom_point(aes(Population,fat_rate))

samp1 <- rhyper(10000,251, 23150,50)
tenSamp <-table(samp1)
barplot(tenSamp)
