#use this script for WQ data
#LABWQ Data done by Marissa 
#redoing to omit all the other things I've done
##might need those later
load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(vegan)
library(ggplot2)
library(scales)

rm(list = ls())

#load csv 
darkcbn.wq <-read_csv("2020_01_labwq.csv")

#format date into 
darkcbn.wq$Date<-as.Date(darkcbn.wq$Date,format="%m/%d/%y")

#check to see how many sites there are and change to have only five 
unique(darkcbn.wq$Site)

#graphs
#new work with WQ 
#goal get all sites on one graph and distingush by colors

#pH
p <- ggplot(data = darkcbn.wq, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1)+
  labs(x = "Date ", y = "pH", title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export pH graph
png("pH_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1)+
  labs(x = "Date ", y = "pH", title = "pH")+
  ylim(7,8.5) +
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#chl
ggplot(data = darkcbn.wq, aes(x = Date, y = Chl), position="dodge") +
  geom_point(aes(col=Site), size=1)+ 
  ylim(0,12.5) +
  labs(x = "Date ", y = "Chlorophyll", title = "Chlorphyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export CHL 
png("chl_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = Chl), position="dodge") +
  geom_point(aes(col=Site), size=1)+ 
  ylim(0,12.5) +
  labs(x = "Date ", y = "Chlorophyll", title = "Chlorphyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dev.off()

#EC
ggplot(data = darkcbn.wq, aes(x = Date, y = EC)) +
  geom_point(aes(col=Site), size=1)+ 
  labs(x = "Date ", y = "Electrical Conductivity (µs/cm)", title = "Electrical Conductivity")+
  ylim(0,1000) +
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export EC
png("EC_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = EC)) +
  geom_point(aes(col=Site), size=1)+ 
  labs(x = "Date ", y = "Electrical Conductivity (µs/cm)", title = "Electrical Conductivity") +
  ylim(0,1000) +
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#Turbidity
ggplot(data = darkcbn.wq, aes(x = Date, y = Turbidity)) +
  geom_point(aes(col=Site), size=1) + 
  ylim(0,125) +
  labs(x = "Date ", y = "Turbidity (NTU)", title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export turbidity
png("turbidity_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = Turbidity)) +
  geom_point(aes(col=Site), size=1) + 
  ylim(0,125) +
  labs(x = "Date ", y = "Turbidity (NTU)", title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#DOC
ggplot(data = darkcbn.wq, aes(x = Date, y = DOC)) +
  geom_point(aes(col=Site), size=1) + 
  ylim(0,20) +
  labs(x = "Date ", y = "DOC (ppm)", title = "DOC")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export DOC
png("DOC_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = DOC)) +
  geom_point(aes(col=Site), size=1) + 
  ylim(0,20) +
  labs(x = "Date ", y = "DOC (ppm)", title = "DOC")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#PO4
ggplot(data = darkcbn.wq, aes(x = Date, y = PO4)) +
  geom_point(aes(col=Site), size=1) + 
  labs(x = "Date ", y = "PO4 (ppm)", title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export PO4
png("PO4_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = PO4)) +
  geom_point(aes(col=Site), size=1) + 
  labs(x = "Date ", y = "PO4 (ppm)", title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#TP
ggplot(data = darkcbn.wq, aes(x = Date, y = TP)) +
  geom_point(aes(col=Site), size=1) + 
  labs(x = "Date ", y = "TP (ppm)", title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export
png("TP_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = TP)) +
  geom_point(aes(col=Site), size=1) + 
  labs(x = "Date ", y = "TP (ppm)", title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#NO3
ggplot(data = darkcbn.wq, aes(x = Date, y = NO3)) +
  geom_point(aes(col=Site), size=1) + 
  labs(x = "Date ", y = "Nitrate (ppm)", title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export NO3
png("NO3_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = NO3)) +
  geom_point(aes(col=Site), size=1) + 
  labs(x = "Date ", y = "Nitrate (ppm)", title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#NH4
ggplot(data = darkcbn.wq, aes(x = Date, y = NH4)) +
  geom_point(aes(col=Site), size=1) + 
  labs(x = "Date ", y = "Ammonium (ppm)", title = "Ammonium")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export NH4
png("NH4_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = NH4)) +
  geom_point(aes(col=Site), size=1) + 
  labs(x = "Date ", y = "Ammonium (ppm)", title = "Ammonium")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()


#TN
ggplot(data = darkcbn.wq, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1)+
  labs(x = "Date ", y = "TN (ppm)", title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#export
png("TN_point.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = darkcbn.wq, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1)+
  labs(x = "Date ", y = "TN (ppm)", title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#now subsetting the water quality data for Jan 1, 2019-April 30, 2019
#labeling it as wet for 19 

wq19s <-darkcbn.wq%>% 
  select(Date, Site, pH, Chl, TN, NH4, NO3, TP, PO4, EC, DOC, Turbidity) %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2019/04/30"))

#now for individual graphs using the subsetted dates
#actual new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19s, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(7, 8.25) +
  theme_bw(base_size = 8)

#export final pH 
png("pH_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(7, 8.25) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
chl <- ggplot(data = wq19s, aes(x = Date, y = Chl), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Chlorophyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  ylim(0, 25) +
  theme_bw(base_size = 8)

#export final chl 
png("chl_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = Chl), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Chlorophyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 25) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
tn<- ggplot(data = wq19s, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1.5) +
  theme_bw(base_size = 8)

#export final TN 
png("TN_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1.5) +
  theme_bw(base_size = 8)

dev.off()


#START new code to put all the sites on one- july 7th 2021 
nh4<- ggplot(data = wq19s, aes(x = Date, y = NH4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Ammonium") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .5) +
  theme_bw(base_size = 8)

#export final NH4
png("NH4_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = NH4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Ammonium") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .5) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19s, aes(x = Date, y = NO3), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1) +
  theme_bw(base_size = 8)

#export final NH3 
png("NO3_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = NO3), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19s, aes(x = Date, y = TP), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 0.6) +
  theme_bw(base_size = 8)

#export final TP 
png("TP_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = TP), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 0.6) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19s, aes(x = Date, y = PO4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .4) +
  theme_bw(base_size = 8)

#export final PO4 
png("PO4_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = PO4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .4) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19s, aes(x = Date, y = DOC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Dissolved Organic Carbon")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 10) +
  theme_bw(base_size = 8)

#export final DOC 
png("DOC_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = DOC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Dissolved Organic Carbon")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 10) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19s, aes(x = Date, y = EC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Electrical conductivity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 875) +
  theme_bw(base_size = 8)

#export final EC 
png("EC_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19s, aes(x = Date, y = EC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Electrical conductivity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 875) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19s, aes(x = Date, y = Turbidity), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1000) +
  theme_bw(base_size = 8)

#export final Turb 
png("Turb_sum19_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19s, aes(x = Date, y = Turbidity), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1000) +
  theme_bw(base_size = 8)

dev.off()

#then subsetting the water quality data for Jan 1, 2020-April 30, 2020
wq20s <-darkcbn.wq%>% 
  select(Date, Site, pH, Chl, TN, NH4, NO3, TP, PO4, EC, DOC, Turbidity) %>%
  filter(Date >= as.Date("2020/01/01") & Date <= as.Date("2020/04/30"))

#now for individual graphs for subsetted field season
#actual new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20s, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(7, 8.25) +
  theme_bw(base_size = 8)

#export final pH 
png("pH_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(7, 8.25) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
chl <- ggplot(data = wq20s, aes(x = Date, y = Chl), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Chlorophyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  ylim(0, 25) +
  theme_bw(base_size = 8)

#export final chl 
png("chl_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = Chl), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Chlorophyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 25) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
tn<- ggplot(data = wq20s, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1.5) +
  theme_bw(base_size = 8)

#export final TN 
png("TN_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1.5) +
  theme_bw(base_size = 8)

dev.off()


#START new code to put all the sites on one- july 7th 2021 
nh4<- ggplot(data = wq20s, aes(x = Date, y = NH4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Ammonium") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .5) +
  theme_bw(base_size = 8)

#export final NH4
png("NH4_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = NH4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Ammonium") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .5) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20s, aes(x = Date, y = NO3), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1) +
  theme_bw(base_size = 8)

#export final NH3 
png("NO3_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = NO3), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20s, aes(x = Date, y = TP), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 0.6) +
  theme_bw(base_size = 8)

#export final TP 
png("TP_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = TP), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 0.6) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20s, aes(x = Date, y = PO4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .4) +
  theme_bw(base_size = 8)

#export final PO4 
png("PO4_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = PO4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .4) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20s, aes(x = Date, y = DOC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Dissolved Organic Carbon")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 10) +
  theme_bw(base_size = 8)

#export final DOC 
png("DOC_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = DOC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Dissolved Organic Carbon")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 10) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20s, aes(x = Date, y = EC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Electrical conductivity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 875) +
  theme_bw(base_size = 8)

#export final EC 
png("EC_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20s, aes(x = Date, y = EC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Electrical conductivity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 875) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20s, aes(x = Date, y = Turbidity), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1000) +
  theme_bw(base_size = 8)

#export final Turb 
png("Turb_sum20_wet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20s, aes(x = Date, y = Turbidity), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1000) +
  theme_bw(base_size = 8)

dev.off()

#then subsetting the water quality data for 2019
wq19 <-darkcbn.wq%>% 
  select(Date, Site, pH, Chl, TN, NH4, NO3, TP, PO4, EC, DOC, Turbidity) %>%
  filter(Date >= as.Date("2019/01/01") & Date <= as.Date("2019/12/31"))

#now for subsetted 2019 year
#actual new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(7, 8.25) +
  theme_bw(base_size = 8)

#export final pH 
png("pH_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(7, 8.25) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
chl <- ggplot(data = wq19, aes(x = Date, y = Chl), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Chlorophyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  ylim(0, 25) +
  theme_bw(base_size = 8)

#export final chl 
png("chl_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = Chl), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Chlorophyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 25) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
tn<- ggplot(data = wq19, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1.5) +
  theme_bw(base_size = 8)

#export final TN 
png("TN_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1.5) +
  theme_bw(base_size = 8)

dev.off()


#START new code to put all the sites on one- july 7th 2021 
nh4<- ggplot(data = wq19, aes(x = Date, y = NH4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Ammonium") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .5) +
  theme_bw(base_size = 8)

#export final NH4
png("NH4_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = NH4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Ammonium") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .5) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19, aes(x = Date, y = NO3), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1) +
  theme_bw(base_size = 8)

#export final NH3 
png("NO3_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = NO3), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19, aes(x = Date, y = TP), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 0.6) +
  theme_bw(base_size = 8)

#export final TP 
png("TP_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = TP), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 0.6) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19, aes(x = Date, y = PO4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .4) +
  theme_bw(base_size = 8)

#export final PO4 
png("PO4_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = PO4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .4) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19, aes(x = Date, y = DOC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Dissolved Organic Carbon")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 10) +
  theme_bw(base_size = 8)

#export final DOC 
png("DOC_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = DOC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Dissolved Organic Carbon")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 10) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19, aes(x = Date, y = EC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Electrical conductivity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 875) +
  theme_bw(base_size = 8)

#export final EC 
png("EC_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq19, aes(x = Date, y = EC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Electrical conductivity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 875) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19, aes(x = Date, y = Turbidity), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1000) +
  theme_bw(base_size = 8)

#export final Turb 
png("Turb_sum19.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq19, aes(x = Date, y = Turbidity), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1000) +
  theme_bw(base_size = 8)

dev.off()

#then subsetting the water quality data for 2020

wq20 <-darkcbn.wq%>% 
  select(Date, Site, pH, Chl, TN, NH4, NO3, TP, PO4, EC, DOC, Turbidity) %>%
  filter(Date >= as.Date("2020/01/01") & Date <= as.Date("2020/12/31"))

#now for subsetted year 2020 
ggplot(data = wq20, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(7, 8.25) +
  theme_bw(base_size = 8)

#export final pH 
png("pH_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "pH")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(7, 8.25) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
chl <- ggplot(data = wq20, aes(x = Date, y = Chl), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Chlorophyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  ylim(0, 25) +
  theme_bw(base_size = 8)

#export final chl 
png("chl_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = Chl), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Chlorophyll")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 25) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
tn<- ggplot(data = wq20, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1.5) +
  theme_bw(base_size = 8)

#export final TN 
png("TN_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = TN), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Nitrogen")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1.5) +
  theme_bw(base_size = 8)

dev.off()


#START new code to put all the sites on one- july 7th 2021 
nh4<- ggplot(data = wq20, aes(x = Date, y = NH4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Ammonium") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .5) +
  theme_bw(base_size = 8)

#export final NH4
png("NH4_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = NH4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Ammonium") +
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .5) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20, aes(x = Date, y = NO3), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1) +
  theme_bw(base_size = 8)

#export final NH3 
png("NO3_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = NO3), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Nitrate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20, aes(x = Date, y = TP), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 0.6) +
  theme_bw(base_size = 8)

#export final TP 
png("TP_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = TP), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Total Phosphorus")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 0.6) +
  theme_bw(base_size = 8)

dev.off()

#START new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20, aes(x = Date, y = PO4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .4) +
  theme_bw(base_size = 8)

#export final PO4 
png("PO4_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = PO4), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Phosphate")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, .4) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20, aes(x = Date, y = DOC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Dissolved Organic Carbon")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 10) +
  theme_bw(base_size = 8)

#export final DOC 
png("DOC_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = DOC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Dissolved Organic Carbon")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 10) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20, aes(x = Date, y = EC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Electrical conductivity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 875) +
  theme_bw(base_size = 8)

#export final EC 
png("EC_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(data = wq20, aes(x = Date, y = EC), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Electrical conductivity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 875) +
  theme_bw(base_size = 8)

dev.off()

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20, aes(x = Date, y = Turbidity), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1000) +
  theme_bw(base_size = 8)

#export final Turb 
png("Turb_sum20.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

#new code to put all the sites on one- july 7th 2021 
ggplot(data = wq20, aes(x = Date, y = Turbidity), position="dodge")+
  geom_point(aes(col=Site), size=1) +
  labs(title = "Turbidity")+
  scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
  ylim(0, 1000) +
  theme_bw(base_size = 8)

dev.off()


### Dark Carbon VSS data over time 

# Loading Libraries -------------------------------------------------------

library(tidyverse)
library(rio)
library(sf)
library(readxl)
library(lubridate)
library(dbplyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)

# Importing VSS data ------------------------------------------------------

#reading the csv in
vss_data = read.csv("data/2020_11_17_VSS_data_MRL.csv", 
                    sep = ",", header = TRUE, stringsAsFactors = FALSE)


# Changing VSS_mgL and POC_mgl to numeric values --------------------------

vss_data$VSS_mgL <- as.numeric(vss_data$VSS_mgL)

vss_data$POC_mgL <- as.numeric(vss_data$POC_mgL)


# Getting Date_collected to read as date ----------------------------------

vss_data$Date_collected <- ymd(vss_data$Date_collected)


# Filter by Site ----------------------------------------------------------

#trying to isolate BABYMARSH

baby_marsh_vss_data <- vss_data %>%
  filter(Site == "BABYMARSH")

#trying to isolate MOK-US-RR

mok_us_rr_vss_data <- vss_data %>%
  filter(Site == c("MOK-US-RR"))

#trying to isolate COS-BEACH

cos_beach_vss_data <- vss_data %>% 
  filter(Site == c("COS-BEACH"))

#trying to isolate SAC-FREMONT

sac_fremont_vss_data <- vss_data %>%
  filter(Site == c("SAC-FREMONT"))

#trying to isolate COS-TRI

cos_tri_vss_data <- vss_data %>%
  filter(Site == c("COS-TRI"))

#trying to isolate KNAGGS-F6

knaggs_f6_vss_data <- vss_data %>% 
  filter(Site == c("KNAGGS-F6"))

#trying to isolate KNAGGS-F3

knaggs_f3_vss_data <- vss_data %>% 
  filter(Site == c("KNAGGS-F3"))

#trying to isolate WENDELLS

wendells_vss_data <- vss_data %>%
  filter(Site == c("WENDELLS"))


# GRAPHING VSS_mg/L ----------------------------------------------------------------


#graphing VSS_mgl over time per site
#####graphing each site separately because for each 
#####sample date there are 3 repetition per site 

#BABYMARSH 

ggplot(data = baby_marsh_vss_data, aes(x = Date_collected, y = VSS_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "VSS (mg/L)", title = "Baby Marsh VSS(mg/L)")+
  theme_bw(base_size = 10)


#MOK-US-RR

ggplot(data = mok_us_rr_vss_data, aes(x = Date_collected, y = VSS_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "VSS (mg/L)", title = "Mok US RR VSS(mg/L)")+
  theme_bw(base_size = 10)


#COS-BEACH

ggplot(data = cos_beach_vss_data, aes(x = Date_collected, y = VSS_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "VSS (mg/L)", title = "COS BEACH VSS(mg/L)")+
  theme_bw(base_size = 10)


#SAC-FREMONT

ggplot(data = sac_fremont_vss_data, aes(x = Date_collected, y = VSS_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "VSS (mg/L)", title = "SAC-FREMONT VSS(mg/L)")+
  theme_bw(base_size = 10)


#COS-TRI

ggplot(data = cos_tri_vss_data, aes(x = Date_collected, y = VSS_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "VSS (mg/L)", title = "COS TRI VSS(mg/L)")+
  theme_bw(base_size = 10)


#KNAGGS-F6

ggplot(data = knaggs_f6_vss_data, aes(x = Date_collected, y = VSS_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "VSS (mg/L)", title = "KNAGGS F6 VSS(mg/L)")+
  theme_bw(base_size = 10)


#KNAGGS-F3

ggplot(data = knaggs_f3_vss_data, aes(x = Date_collected, y = VSS_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "VSS (mg/L)", title = "KNAGGS F3 VSS(mg/L)")+
  theme_bw(base_size = 10)


#Wendells

ggplot(data = wendells_vss_data, aes(x = Date_collected, y = VSS_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "VSS (mg/L)", title = "Wendels VSS(mg/L)")+
  theme_bw(base_size = 10)


# Graphing POC_mg/L -------------------------------------------------------

#graphing POC_mgl over time per site
#####graphing each site separately because for each 
#####sample date there are 3 repetition per site 


#BABYMARSH 

ggplot(data = baby_marsh_vss_data, aes(x = Date_collected, y = POC_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "POC (mg/L)", title = "Baby Marsh POC(mg/L)")+
  theme_bw(base_size = 10)


#MOK-US-RR

ggplot(data = mok_us_rr_vss_data, aes(x = Date_collected, y = POC_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "POC (mg/L)", title = "Mok US RR POC(mg/L)")+
  theme_bw(base_size = 10)

#COS-BEACH

ggplot(data = cos_beach_vss_data, aes(x = Date_collected, y = POC_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "POC (mg/L)", title = "COS BEACH POC(mg/L)")+
  theme_bw(base_size = 10)


#SAC-FREMONT

ggplot(data = sac_fremont_vss_data, aes(x = Date_collected, y = POC_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "POC (mg/L)", title = "SAC-FREMONT POC(mg/L)")+
  theme_bw(base_size = 10)


#COS-TRI

ggplot(data = cos_tri_vss_data, aes(x = Date_collected, y = POC_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "POC (mg/L)", title = "COS TRI POC(mg/L)")+
  theme_bw(base_size = 10)


#KNAGGS-F6

ggplot(data = knaggs_f6_vss_data, aes(x = Date_collected, y = POC_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "POC (mg/L)", title = "KNAGGS F6 POC(mg/L)")+
  theme_bw(base_size = 10)


#KNAGGS-F3

ggplot(data = knaggs_f3_vss_data, aes(x = Date_collected, y = POC_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "POC (mg/L)", title = "KNAGGS F3 POC(mg/L)")+
  theme_bw(base_size = 10)


#Wendells

ggplot(data = wendells_vss_data, aes(x = Date_collected, y = POC_mgL))+
  geom_point(aes(col=Site), size =1 )+
  labs(x = "Date Collected", y = "POC (mg/L)", title = "Wendels POC(mg/L)")+
  theme_bw(base_size = 10)





# DOC ---------------------------------------------------------------------


doc_data = read.csv("data/2020-12-03_BioAvail_SUVA254_NSC.csv", 
                    sep = ",", header = TRUE, stringsAsFactors = FALSE)




# Organizing DOC data -----------------------------------------------------

doc_data$A.254 <- as.numeric(doc_data$A.254)

doc_data$DOC.ppm <- as.numeric(doc_data$DOC.ppm)

doc_data$DOC_RERUN.ppm <- as.numeric(doc_data$SUVA.254_RERUN)

doc_data$SUVA.254 <- as.numeric(doc_data$SUVA.254)

doc_data$SUVA.254_RERUN <- as.numeric(doc_data$SUVA.254_RERUN)

doc_data$Date.Collected <- mdy(doc_data$Date.Collected)

doc_data$Date.Sampled <- mdy(doc_data$Date.Sampled)

#form a new column that is Location and Repitition 
site_rep_doc_data %>% 
  unite("Site_Rep", c(Location, Rep.ID), remove = FALSE)
#Help 



# Divide Samples into Quarters --------------------------------------------

#2019-01-04 COS-BEACH, MOK-US-RR DOC and DOC+N
#2019-01-08 KNAGGS-F6 DOC and DOC+N
#2019-01-12 DI
#2019-01-17 COS-TRI DOC and DOC+N
#2019-01-28 SAC-XS DOC and DOC+N 
#2019-01-29 KNAGGS-F6 DOC and DOC+N
#2019-02-28 BABYMARSH DOC and DOC+N

#2019-01-31 COSTRIDISCON DOC and DOC+N 
#2019-02-19 LUCO
#2019-03-14 314_COS DOC and DOC+N 


jan_2019_doc <- doc_data %>%
  filter(Date.Collected >= "2019-01-04", Date.Collected <= "2019-03-14")


#2019-04-08 SAC-XS DOC
#2019-04-09 BABYMARSH DOC
#2019-04-10 COS-TRI, WENNDELS, KNAGGS_F6 DOC
#2019-04-11 COS-BEACH, MOK-US-RR DOC
#2019-04-22 DI

april_2019_doc <- doc_data %>% 
  filter(Date.Collected >= "2019-04-08", Date.Collected <= "2019-04-22")

#2019-06-06 COS-TRI DOC
#2019-06-13 COS-TRI DOC
#2019-06-18 COS-TRI DOC

june_2019_doc <- doc_data %>% 
  filter(Date.Collected >= "2019-06-06", Date.Collected <= "2019-06-18")

#2019-09-10 BABYMARSH, MOK-US-RR, COS-BEACH, SAC-XS DOC 

sept_2019_doc <- doc_data %>% 
  filter(Date.Collected == "2019-09-10")

#2019-10-15 BABYMARSH, MOK-US-RR, COS-BEACH, SAC-XS DOC 
#2019-10-30 DI

oct_2019_doc <- doc_data %>% 
  filter(Date.Collected >= "2019-10-15", Date.Collected <= "2019-10-30")

#2019-12-09 WENDELS DOC
#2019-12-10 BABYMARSH, MOK-US-RR, COS-BEACH, SAC-XS DOC 
#2020-01-08 KNAGGS DOC

dec_2019_doc <- doc_data %>% 
  filter(Date.Collected >= "2019-12-09", Date.Collected <= "2020-01-08")

#2020-02-10 MOK-US-RR, COS-BEACH, KNAGGS DOC
#2020-02-11 Baby Marsh DOC
#2020-02-12 SAC-XS DOC
#2020-02-15 DI

feb_2020_doc <- doc_data %>% 
  filter(Date.Collected >= "2020-02-10", Date.Collected <= "2020-02-15")

#2020-03-22 BABYMARSH, MOK-US-RR, COS-BEACH DOC
#2020-03-23 SAC-XS, KNAGGS DOC 

march_2020 <- doc_data %>% 
  filter(Date.Collected >= "2020-03-22", Date.Collected <= "2020-03-23")

#2020-06-09 BABYMARSH, MOK-US-RR, COS-BEACH, SAC-XS DOC 
#2020-06-16 DI 

june_2020_doc <- doc_data %>% 
  filter(Date.Collected >= "2020-06-09", Date.Collected <= "2020-06-16")

#2020-10-07 BABYMARSH, MOK-US-RR, COS-BEACH, SAC-XS DOC and DI

oct_2020_doc <- doc_data %>% 
  filter(Date.Collected == "2020-10-07")

>>>>>>> 7b082d9b5668090966a6fb1b33383e502d0cc6ea

