getwd()
#setwd("/home/enrmic/exploratory_analysis_of_the_texas_real_estate_market")
setwd("C:/Users/miche/exploratory_analysis_of_the_texas_real_estate_market")

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
  summarize(volumes_sum = sum((volume)))

sales_for_month <- dati %>%
  group_by(month) %>%
  summarize(sales_sum = sum(as.integer(sales)))

sales_for_month_for_year <- dati %>%
  group_by(year, month) %>%
  summarize(sales_sum = sum(as.integer(sales)))

volume_sales_for_month <- dati %>%
  group_by(month) %>%
  summarize(volumes_sum = sum(volume))

volume_sales_for_month_for_year <- dati %>%
  group_by(year, month) %>%
  summarize(volumes_sum = sum(volume))

dati_tyler<- filter(sales_for_city, city=="Tyler")
detach(dati)


#media delle vendite in un anno in una città:
summary(sales_for_city$sales_sum)
summary(dati_tyler$sales_sum)
summary(sales_for_month$sales_sum)
summary(sales_for_month_for_year$sales_sum)
summary(volume_sales$volumes_sum)
summary(volume_sales_for_month$volumes_sum)
summary(volume_sales_for_month_for_year$volumes_sum)


sales_for_month_for_year[sales_for_month_for_year$sales_sum=="1177",1:3] #Giugno 2014
sales_for_month_for_year[sales_for_month_for_year$sales_sum=="421",1:3] #Gennaio 2010



ggplot(data = sales_for_city)+
    geom_line(aes(x=sales_for_city$year, 
                  y=sales_for_city$sales_sum, 
                  group=sales_for_city$city, 
                  colour=sales_for_city$city))+
    geom_point(aes(x=sales_for_city$year, 
                   y=sales_for_city$sales_sum, 
                   colour=sales_for_city$city), size=3)+
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
       aes(x=volume_sales$year, y=volume_sales$volumes_sum, group=volume_sales$city))+
  geom_line(aes(colour=volume_sales$city))+
  scale_color_discrete("Texas Cities")+
  labs(x="year", y="volume sales (Million $)")+
  ggtitle("Volume Sales Trend")+
  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data = sales_for_city,
#       aes(x=sales_for_city$year, group=sales_for_city$sell))+
#  geom_bar(aes(fill=sales_for_city$city))+
#  scale_y_continuous(breaks=seq(0,2000,20))+
  

#range, range interquartile, varianza, deviazione std, coeff di variabilità etc..
attach(dati)
var(sales); sd(sales)

CV <-function(x){
  return( sd(x)/mean(x) * 100 )
}

CV(sales)
IQR(sales)
range(sales)
gini.index(sales)

CV(volume)
IQR(volume)
range(volume)
gini.index(volume)


CV(listings)
IQR(listings)
range(listings)
gini.index(listings)

CV(months_inventory)
IQR(months_inventory)
range(months_inventory)
gini.index(months_inventory)

var(sales_for_city$sales_sum); sd(sales_for_city$sales_sum)
CV(sales_for_city$sales_sum)
IQR(sales_for_city$sales_sum)
range(sales_for_city$sales_sum)
gini.index(sales_for_city$sales_sum)

var(sales_for_month$sales_sum); sd(sales_for_month$sales_sum)
CV(sales_for_month$sales_sum)
IQR(sales_for_month$sales_sum)
range(sales_for_month$sales_sum)
gini.index(sales_for_month$sales_sum)

var(sales_for_month_for_year$sales_sum); sd(sales_for_month_for_year$sales_sum)
CV(sales_for_month_for_year$sales_sum)
IQR(sales_for_month_for_year$sales_sum)
range(sales_for_month_for_year$sales_sum)
gini.index(sales_for_month_for_year$sales_sum)


var(volume_sales$volumes_sum)
sd(volume_sales$volumes_sum)
CV(volume_sales$volumes_sum)
IQR(volume_sales$volumes_sum)
range(volume_sales$volumes_sum)
gini.index(volume_sales$volumes_sum)

var(volume_sales_for_month$volumes_sum)
sd(volume_sales_for_month$volumes_sum)
CV(volume_sales_for_month$volumes_sum)
IQR(volume_sales_for_month$volumes_sum)
range(volume_sales_for_month$volumes_sum)
gini.index(volume_sales_for_month$volumes_sum)


var(volume_sales_for_month_for_year$volumes_sum)
sd(volume_sales_for_month_for_year$volumes_sum)
CV(volume_sales_for_month_for_year$volumes_sum)
IQR(volume_sales_for_month_for_year$volumes_sum)
range(volume_sales_for_month_for_year$volumes_sum)
gini.index(volume_sales_for_month_for_year$volumes_sum)



#indici di forma (Asimmetria, curtosi )
library(moments)

skewness(sales)
kurtosis(sales)-3

detach(dati)



