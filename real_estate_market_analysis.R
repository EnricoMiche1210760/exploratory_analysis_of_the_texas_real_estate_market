getwd()
setwd("/home/enrmic/exploratory_analysis_of_the_texas_real_estate_market")

dati<-read.csv(file = "realestate_texas.csv")
summary(dati)
head(dati)

gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.norm = gini/((J-1)/J)
  
  return(gini.norm)
}


library(ggplot2)
library(dplyr)

attach(dati)

summary(sales)
summary(volume)
summary(listings)
summary(months_inventory)

# mode for city (as the city with the highest sales rate)
# Tyler
# anno 2014
# mese Giugno

#vediamo un confronto città anni mesi
sales_for_city <- dati %>%
  group_by(city, year) %>%
  summarize(sales_sum = sum(as.integer(sales)))

volume_sales <- dati %>%
  group_by(city, year) %>%
  summarize(volume_sum = sum((volume)))

sales_for_month <- dati %>%
  group_by(month) %>%
  summarize(sales_sum = sum(as.integer(sales)))

sales_for_month_for_year <- dati %>%
  group_by(year, month) %>%
  summarize(sales_sum = sum(as.integer(sales)))

dati_tyler<- filter(sales_for_city, city=="Tyler")
detach(dati)


#media delle vendite in un anno in una città:
summary(sales_for_city$sales_sum)
summary(dati_tyler$sales_sum)
summary(sales_for_month$sales_sum)
summary(sales_for_month_for_year$sales_sum)

sales_for_month_for_year[sales_for_month_for_year$sales_sum=="1177",1:3] #Giugno 2014
sales_for_month_for_year[sales_for_month_for_year$sales_sum=="421",1:3] #Gennaio 2010



ggplot(data = sales_for_city)+
    geom_line(aes(x=sales_for_city$year, 
                  y=sales_for_city$sales_sum, 
                  group=sales_for_city$city, 
                  colour=sales_for_city$city))+
    geom_point(aes(x=sales_for_city$year, 
                   y=sales_for_city$sales_sum, 
                   colour=sales_for_city$city), lwd=3)+
    geom_text(aes(x=sales_for_city$year,
                  y=sales_for_city$sales_sum+75,
                  label=sales_for_city$sales_sum))+
    scale_y_continuous(breaks = seq(0,4000,100))+
    scale_color_discrete("Texas Cities")+
    labs(x="year", y="sales")+
    ggtitle("Sales Trend")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
    

ggplot(data = volume_sales,
       aes(x=volume_sales$year, y=volume_sales$volume_sum, group=volume_sales$city))+
  geom_line(aes(colour=volume_sales$city))+
  scale_color_discrete("Texas Cities")+
  labs(x="year", y="volume sales (Million $)")+
  ggtitle("Volume Sales Trend")+
  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data = sales_for_city,
#       aes(x=sales_for_city$year, group=sales_for_city$sell))+
#  geom_bar(aes(fill=sales_for_city$city))+
#  scale_y_continuous(breaks=seq(0,2000,20))+
  






