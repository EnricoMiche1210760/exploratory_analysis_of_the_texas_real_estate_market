getwd()
setwd("/home/enrmic/exploratory_analysis_of_the_texas_real_estate_market")

dati<-read.csv(file = "realestate_texas.csv")
summary(dati)
head(dati)

library(ggplot2)
#library(mapdata)
library(dplyr)
#library(maps)

texas<-map_data("county")
ggplot(data=texas)+
  scale_y_continuous(limits = c(37,47))+
  scale_x_continuous(limits = c(7,18))+
  geom_map(map = texas,fill=dati$city,
           mapping = aes(map_id=region),
           #fill="green3", #prima questo
           col="black")
attach(dati)
#city
table(city)
#city sono equidistribuiti

gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.norm = gini/((J-1)/J)
  
  return(gini.norm)
}

table(city)/length(city) #!!!NOOO!!!!
gini.index(city) #indice di gini uguale a 1 esattamente come mi aspettavo

#year
table(year) #anche gli anni sono equidistribuiti !!!! NO !!!!

#vediamo un confronto città anni
sales_for_city <- dati %>%
  group_by(city, year) %>%
  summarize(sales_sum = sum(as.integer(sales)))

ggplot(data = sales_for_city,
    aes(x=sales_for_city$year, y=sales_for_city$sales_sum, group=sales_for_city$city))+
    geom_line(aes(colour=sales_for_city$city))+
    scale_color_discrete("Texas cities")+
    labs(x="year", y="sales ($)")+
    ggtitle("Sales Trend")+
    theme(plot.title = element_text(hjust = 0.5))
?labs
ggplot(data = sales_for_city,
       aes(x=sales_for_city$year, group=sales_for_city$sell))+
  geom_bar(aes(fill=sales_for_city$city))+
  scale_y_continuous(breaks=seq(0,2000,20))+
  

max(as.integer(sales_for_city$sales_sum))

detach(dati)


df %>% 
  group_by(Year, Month, Group, SubGroup) %>% 
  summarize(
    V1_sum = sum(V1),
    V2_sum = sum(V2)
  ) %>% 
  group_by(Year, Group, SubGroup) %>% 
  mutate(
    V1_cumsum = cumsum(V1_sum),
    V2_cumsum = cumsum(V2_sum)
  )




