library(data.table)
library(tidyverse)

atna22 <- fread("./Inputs/Planting/AtnaBayMonitoring_927-976_17-22.csv", na.strings = "n/a")
str(atna22)
atna22[,`:=`(Vigour=as.factor(Vigour),Diam_cm=as.numeric(Diam_cm))]


ggplot(atna22[Vigour!="D"])+
  geom_point(aes(x=Year, y=Height_cm, group=Tree, colour = Tree),size=4)+
  geom_line(aes(x=Year, y=Height_cm, group=Tree, colour = Tree),linewidth=1.5)+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("seedling height (cm)")
ggsave("AtnaBay_heightGrowth.jpg")  

#average growth:
latna <- atna22[,.(Tree,Year,Height_cm)]
latna_h <- dcast(latna,Tree ~ Year, value.var = "Height_cm")
latna_h <- latna_h[!is.na(`2022`)]
latna_h[,`:=`(Y5=`2022`-`2017`,Y5r=(`2022`-`2017`)/5), by="Tree"]

mean(latna_h$Y5r) #9.9 cm/yr
range(latna_h$Y5r) #2.8 - 19cm/year 

#survival
nrow(atna22[Vigour=="D" & Year ==2022| is.na(Vigour) & Year==2022])
#9 out of 50 died or not found in 2022
(50-9)/50 #82% survival

#rust
nrow(atna22[!is.na(ABC)|!is.na(IABC)|!is.na(ASC)|!is.na(IASC)])/50
#64 % blister rust infection