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

#zoop data collected and updated september 2020 
zoop.dc<-read.csv("2020_11_2_DC_Zoop_MRL.csv")

#format date into 
zoop.dc$Date<-as.Date(zoop.dc$Date,format="%m/%d/%y")

#check to see how many sites there are and change to have only five 
unique(zoop.dc$Site)


#convert flowmeter to numeric instead of character 
zoop.dc$FlowMeterEnd  <- as.numeric(as.character(zoop.dc$FlowMeterEnd))
zoop.dc$FlowMeterBegin  <- as.numeric(as.character(zoop.dc$FlowMeterBegin))
zoop.dc$TotalVolume_ml  <- as.numeric(as.character(zoop.dc$TotalVolume_ml))
zoop.dc$Volumesubsampled_ml  <- as.numeric(as.character(zoop.dc$Volumesubsampled_ml))
zoop.dc$RingSize_cm  <- as.numeric(as.character(zoop.dc$RingSize_cm))
zoop.dc$abundance  <- as.numeric(as.character(zoop.dc$abundance))
zoop.dc$SplitFraction <- as.numeric(as.character(zoop.dc$SplitFraction))
zoop.dc$MeshSize_Microns <- as.numeric(as.character(zoop.dc$MeshSize_Microns))

na.omit(zoop.dc)

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
drkcbnabundance<-abundance(zoop.dc)

#Group by site, date, and order 
drkcbnzoop.sum<-drkcbnabundance %>% 
  group_by(Site, Date, Order) %>% 
  summarise(Total=sum(ind_m3)) 

na.omit(drkcbnzoop.sum)

#color blind pallete colors 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Graph to see what it looks like right now for abundance 
ggplot(drkcbnzoop.sum, aes(x = Date, y = Total, fill=Order)) + geom_area() +
  scale_fill_manual(values=cbPalette) +
  ggtitle("Yearly Abundance") +
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") +
  scale_y_continuous(label=comma) +
  theme_classic()

#insert new group by data
zoopgroup <-read_csv("groupbysum.csv")

#insert new data for baby marsh
bmgroup <-read_csv("bmgroup.csv")

#format date into 
bmgroup$Date<-as.Date(bmgroup$Date,format="%m/%d/%y")

#graph abundance
ggplot(data = bmgroup, aes(x = Date, y = Total, fill = Order)) + 
  geom_bar(stat = "identity") +
  ggtitle("Baby Marsh Abundance") +
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") +
  labs(x= "Date", y= "Total Abundance (ind/m3)") +
  scale_y_continuous(label=comma) +
  theme_classic()

#export graph 
png("BM Abundance.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = bmgroup, aes(x = Date, y = Total, fill = Order)) + 
  geom_bar(stat = "identity") +
  ggtitle("Baby Marsh Abundance") +
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") +
  labs(x= "Date", y= "Total Abundance (ind/m3)") +
  scale_y_continuous(label=comma) +
  theme_classic()

dev.off()

#Group together large and small cladocera for in depth comparision  
