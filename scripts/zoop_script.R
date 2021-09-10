#use this script for zoop analysis
#dark carbon wq and zoop data analysis
#1 create zoop graphs comparing biomass, abundance, and species compostion
#2 look at changes between specific parameters relating to water quality over time

#load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(vegan)
library(ggplot2)
library(scales)

#update July 2021 
zoop.dc2<-read.csv("darkcarbon_zoop_all.csv")

#format date into 
zoop.dc2$Date<-as.Date(zoop.dc2$Date,format="%m/%d/%y")

#check to see how many sites there are and change to have only five 
unique(zoop.dc2$Site)
unique(zoop.dc2$Group)

#convert flowmeter to numeric instead of character 
zoop.dc2$FlowMeterEnd  <- as.numeric(as.character(zoop.dc2$FlowMeterEnd))
zoop.dc2$FlowMeterBegin  <- as.numeric(as.character(zoop.dc2$FlowMeterBegin))
zoop.dc2$TotalVolume_ml  <- as.numeric(as.character(zoop.dc2$TotalVolume_ml))
zoop.dc2$Volumesubsampled_ml  <- as.numeric(as.character(zoop.dc2$Volumesubsampled_ml))
zoop.dc2$RingSize_cm  <- as.numeric(as.character(zoop.dc2$RingSize_cm))
zoop.dc2$abundance  <- as.numeric(as.character(zoop.dc2$abundance))
zoop.dc2$SplitFraction <- as.numeric(as.character(zoop.dc2$SplitFraction))
zoop.dc2$MeshSize_Microns <- as.numeric(as.character(zoop.dc2$MeshSize_Microns))

na.omit(zoop.dc2)

#abundance function 
abundance<-function(df) {
  df$distance<-(df$FlowMeterEnd-df$FlowMeterBegin)*26873/999999
  df$RingSize_m<-df$RingSize_cm/100
  df$hooparea<-(pi*(df$RingSize_m)^2)/4
  df$volumesampled<-df$distance*df$hooparea
  df$samplecount<-(df$TotalVolume_ml/df$Volumesubsampled_ml)*df$abundance
  df$ind_m3<-df$samplecount/df$volumesampled
  df
}

#use abundance function
drkcbnabundance2 <-abundance(zoop.dc2)

#Group by site, date, and order 
drkcbnzoop.sum2 <-drkcbnabundance2 %>% 
  group_by(Site, Date, Order, Subclass, Group, Species, LifeStage) %>% 
  summarise(Total=sum(ind_m3)) 


#format date into 
drkcbnzoop.sum2$Date<-as.Date(drkcbnzoop.sum2$Date,format="%m/%d/%y")

#check sites 
unique(drkcbnzoop.sum2$Site)

#do only january 2019 end of december 2020 
drkcbnzoop.sum3 <- drkcbnzoop.sum2 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>% 
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2020/12/31"))

#plot of  both years
ggplot(drkcbnzoop.sum3, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Yearly Abundance") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") +
  scale_y_continuous(label=comma) +
  theme_bw()

#subset 2019
zoop19<- drkcbnzoop.sum3 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2019/12/31"))

#subset 2020
zoop20<- drkcbnzoop.sum3 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>%
  filter(Date >= as.Date("2020/01/01") & Date <= as.Date("2020/12/31"))

#graph for 2019 

ggplot(zoop19, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Yearly Abundance 2019") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "3 months") +
  scale_y_continuous(label=comma) +
  theme_bw()

#graph for 2020 

ggplot(zoop20, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Yearly Abundance 2020") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "3 months") +
  scale_y_continuous(label=comma) +
  theme_bw()

#Export all the graph and ask for feedback 
#export 2019 + 2020 
png("Abundance_both.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(drkcbnzoop.sum3, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Yearly Abundance") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") +
  scale_y_continuous(label=comma) +
  theme_bw()

dev.off()

#export 2019 
png("Abundance_19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop19, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Yearly Abundance 2019") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "3 months") +
  scale_y_continuous(label=comma) +
  theme_bw()

dev.off()

#export 2020
png("Abundance_20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop20, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Yearly Abundance 2020") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "3 months") +
  scale_y_continuous(label=comma) +
  theme_bw()

dev.off()

#coming back to break cage dates down for 2019 and 2020
#subset 2019 field dates aka wet periods

zoop19s<- drkcbnzoop.sum3 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2019/04/30"))

#subset 2020 field dates 
zoop20s<- drkcbnzoop.sum3 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>%
  filter(Date >= as.Date("2020/01/01") & Date <= as.Date("2020/04/30"))

#graph for 2019 wet dates

ggplot(zoop19s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2019") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  scale_y_continuous(label=comma) +
  theme_bw()

#graph for 2020 

ggplot(zoop20s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2020") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  scale_y_continuous(label=comma) +
  theme_bw()

#Export all the graph and ask for feedback 

#export 2019 wet period
png("Abundance_Jan_April_19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop19s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2019") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  scale_y_continuous(label=comma) +
  theme_bw()

dev.off()

#export 2020 wet period
png("Abundance_Jan_April_20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop20s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2020") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  scale_y_continuous(label=comma) +
  theme_bw()

dev.off()

#logging abundance graphs - July 12th 
#using ggplot call y= log(abundance)

png("Abundance_Jan_April_20_log.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop20s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2020") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  scale_y_log10() + 
  theme_bw()

dev.off()
#another way
# y= log(abundance) 
#wet period 2020 
png("Abundance_Jan_April_20_log3.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop20s, aes(x=Date, y=log(Total), color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2020") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  theme_bw()

dev.off()

#another way
# y= log(abundance)
#wet period 2019
png("Abundance_Jan_April_19_log3.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop19s, aes(x=Date, y=log(Total), color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2019") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  theme_bw()

dev.off()

#redo with log y 
#export 2020 wet period
png("Abundance_20_free.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop20s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2020") +
  facet_grid(Site~. , scales = "free") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  scale_y_continuous(label=comma) +
  theme_bw()

dev.off()

#logging abundance graphs - July 12th 
#using ggplot call y= log(abundance)

png("Abundance_Field20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop20s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2020") +
  facet_grid(Site~.) +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  scale_y_log10() + 
  theme_bw()

dev.off()

#wet period 2020 

png("Abundance_Jan_April_20_log_new.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop20s, aes(x=Date, y=log(Total), color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2020") +
  scale_y_log10() +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  theme_bw()

dev.off()

#wet period 2019
png("Abundance_19_free.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop19s, aes(x=Date, y=log(Total), color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2019") +
  facet_grid(Site~. , scales = "free") +
  scale_y_log10() +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  theme_bw() 

dev.off()


# y= log(abundance)
#use options(scipen=10000) - ryan said will correct scientific notation
#adding free scale within facetgrid 

#wet period 2019
png("Abundance_19_free.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop19s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2019") +
  facet_grid(Site~. , scales = "free") +
  scale_y_log10() +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  theme_bw() 

dev.off()

#wet period 2020
png("Abundance_2020_free.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(zoop20s, aes(x=Date, y=Total, color=Group, fill=Group)) +
  geom_bar(stat="identity") +
  ggtitle("Abundance 2020") +
  facet_grid(Site~. , scales = "free") +
  scale_y_log10() +
  scale_x_date(date_labels = "%b-%y", breaks= "1 month") +
  theme_bw() 

dev.off()

#upload to git
