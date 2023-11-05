getwd()
#setwd("/home/enrmic/exploratory_analysis_of_the_texas_real_estate_market")
setwd("C:/Users/miche/exploratory_analysis_of_the_texas_real_estate_market")

dati<-read.csv(file = "realestate_texas.csv")
summary(dati[4:8])
head(dati)

table(dati[1])
N <- dim(dati)[1]
city_distribution <- dati$city

ni<-table(city_distribution)
fi<-table(city_distribution)/N
Ni<-cumsum(ni)
Fi<-Ni/N

cbind(ni, fi, Ni, Fi)

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
library(xtable)

attach(dati)

print(xtable(t(summary(sales))), type = "latex", include.rownames=FALSE)

summary(volume)
print(xtable(t(summary(volume))), type = "latex", include.rownames=FALSE)
print(xtable(t(summary(median_price))), type = "latex", include.rownames=FALSE)
print(xtable(t(summary(listings))), type = "latex", include.rownames=FALSE)
print(xtable(t(summary(months_inventory))), type = "latex", include.rownames=FALSE)
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

sales_for_year <- dati %>%
  group_by(year) %>%
  summarize(sales_sum = sum(as.integer(sales)))

sales_for_month <- dati %>%
  group_by(month) %>%
  summarize(sales_sum = sum(as.integer(sales)))

print(xtable(sales_for_year))
print(xtable(sales_for_month))


volume_for_year <- dati %>%
  group_by(year) %>%
  summarize(volume_sum = sum(volume))


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


#MODA (https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(sales)



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

CV <-function(x){
  return( sd(x)/mean(x) * 100 )
}

stats_report<-function(x, type="quantitative"){
  variance<-var(x) 
  std_dev<-sd(x)
  variance_coeff<-CV(x)
  iqr<-IQR(x)
  distr_range<-range(x)[2]-range(x)[1]
  gini_coeff<-gini.index(x)
  if( type == "quantitative" )
  {
    return(data.frame(variance, std_dev, variance_coeff, iqr, distr_range))
  }
  return(data.frame(variance, std_dev, variance_coeff, iqr, distr_range, gini_coeff))
  
}
  

stats_report(sales)
print(xtable(stats_report(sales)), type = "latex", include.rownames = F)
print(xtable(stats_report(volume)), type = "latex", include.rownames = F)

stats_report(listings)
stats_report(months_inventory)

print(xtable(stats_report(median_price)), type = "latex", include.rownames = F)
print(xtable(stats_report(listings)), type = "latex", include.rownames = F)
print(xtable(stats_report(months_inventory)), type = "latex", include.rownames = F)


stats_report(sales_for_city$sales_sum)
stats_report(sales_for_month$sales_sum)
stats_report(sales_for_month_for_year$sales_sum)


stats_report(volume_sales$volumes_sum)
stats_report(volume_sales_for_month$volumes_sum)
stats_report(volume_sales_for_month_for_year$volumes_sum)



#indici di forma (Asimmetria, curtosi )
library(moments)
library(gghalves)
skewness(sales)
kurtosis(sales)-3
skewness(volume)
kurtosis(volume)-3
skewness(median_price)
kurtosis(median_price)-3
skewness(listings)
kurtosis(listings)-3
skewness(months_inventory)
kurtosis(months_inventory)-3


#*****
#VARIABILE CON LA MAGGIOR VARIABILITA' E DISPERSIONE DEI DATI!!!
#*****
#DISTRIBUZIONE DI FREQUENZA DA CREARE: ANNI, QUADRIMESTRI, CITTA, MESI (QUINDI UNIRE PER MESI, INVECE CHE PER QUADRIMESTRI)
#AD ESEMPIO: LA PERCENTUALE DI CASE VENDUTE NEL PRIMO MESE, RISPETTO AL TOTALE.
#PERCENTUALE DI VOLUMI RISPETTO AL TOTALE ETC..
#IN QUESTO MODO OTTENGO QUALCOSA DI CONCRETO...
round(max(months_inventory))
table(months_inventory)
min_month_inventory<-as.integer(min(months_inventory))
max_month_inventory<-round(max(months_inventory))

month_inventory_class <- cut(months_inventory, 
                             breaks = seq(min_month_inventory,
                                          max_month_inventory,
                                          (max_month_inventory-min_month_inventory)/4))

table(month_inventory_class)

ni<-table(month_inventory_class)
fi<-ni/N
Ni<-cumsum(ni)
Fi<-Ni/N

month_inventory_cl_distribution<-as.data.frame((cbind(ni,fi,Ni,Fi)))
gini.index(month_inventory_class)

c_size<-dim(month_inventory_cl_distribution)[1]
vec=c(0.1)
for ( i in 2:c_size ){
  vec<-c(vec, 0.1)
}

pdf("figures/myfile.pdf", height=6, width=6)

barplot(month_inventory_cl_distribution$ni,
        ylim = c(0,120),
        col = "aquamarine4",
        main = "Frequency distribution of \"Month inventory\" class",
        width = vec,
        names.arg = rownames(month_inventory_cl_distribution))

mtext(side=1, 
      text="Month inventory",
      line=2.5,
      cex = 1.25)
mtext(side=2,
      text="Absolute frequency",
      line=2.5,
      cex = 1.25)

dev.off()

mode(city)

attach(dati)
mean_price <- round((volume / sales) * 10^6)
ads_efficiency <- round(sales / listings, digits = 2)

table(ads_efficiency)

dati_with_mean_and_eff <- cbind(dati, mean_price)
dati_with_mean_and_eff <- dati_with_mean_and_eff %>% relocate(mean_price, .before = median_price)

dati_with_mean_and_eff <- cbind(dati_with_mean_and_eff, ads_efficiency)

if (!dir.exists("data"))
{
  dir.create("data")
}
write.csv(dati_with_mean_and_eff, "data/realestate_texas_with_mean_and_eff.csv")

summary(ads_efficiency)

ads_eff_distr<- dati_with_mean_and_eff %>%
                group_by(city, year) %>%
                summarise(average_eff= round(mean(ads_efficiency), digits = 2))

pdf("figures/sales_trend.pdf", height=6, width=6)
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
  labs(x="Year", y="Sales")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")
dev.off()

pdf("figures/ads_efficiency.pdf", height=6, width=6)

ggplot(data = ads_eff_distr)+
  geom_line(aes(x=ads_eff_distr$year, 
                y=ads_eff_distr$average_eff, 
                group=ads_eff_distr$city, 
                colour=ads_eff_distr$city))+
  geom_point(aes(x=ads_eff_distr$year, 
                 y=ads_eff_distr$average_eff, 
                 colour=ads_eff_distr$city), size=3)+
  geom_text(aes(x=ads_eff_distr$year,
                y=ads_eff_distr$average_eff+0.005,
                label=ads_eff_distr$average_eff))+
  scale_y_continuous(breaks = seq(0,1,0.01))+
  scale_color_discrete("Texas Cities")+
  labs(x="Year", y="Ads efficiency")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

dev.off()


#???????? anno e mese come factor???




detach(dati)



