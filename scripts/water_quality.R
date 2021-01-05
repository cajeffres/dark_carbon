#use this script for WQ data
#LABWQ Data done by Marissa 

#load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(vegan)
library(ggplot2)
library(scales)

#load csv 
darkcbn.wq <-read_csv("2020-10-01_labwq.csv")

#format date into 
darkcbn.wq$Date<-as.Date(darkcbn.wq$Date,format="%m/%d/%y")


#check to see how many sites there are and change to have only five 
unique(darkcbn.wq$Site)

#make individual date sets per site
mok<-darkcbn.wq%>% 
  filter(Site == "MOK-US-RR")

sac<-darkcbn.wq%>% 
  filter(Site == "XSSAC")

cb<-darkcbn.wq%>% 
  filter(Site == "COS-BEACH")

kngf6<-darkcbn.wq%>% 
  filter(Site == "KNAGGS-F6")

kngf3<-darkcbn.wq%>% 
  filter(Site == "KNAGGS-F3")

bm <-darkcbn.wq%>% 
  filter(Site == "BABYMARSH")

#make individual graphs for site per parameter 

#mok first
ggplot(mok, aes(x = Date, y = Sal_PSU))+ 
  geom_line() + 
  scale_fill_manual(values=cbPalette) +
  ggtitle("MokUSRR ") +
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") +
  theme_classic()

ggplot(mok, aes(x = Date, y = pH, data=pH))+ 
  geom_line() + 
  scale_fill_manual(values=cbPalette) +
  ggtitle("MokUSRR ") +
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") +
  theme_classic()

#export to get feedback from carson 
#Export graph- Cos
png("mok_pH.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(mok, aes(x = Date, y = pH, data=pH))+ 
  geom_line() + 
  scale_fill_manual(values=cbPalette) +
  ggtitle("MokUSRR ") +
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") +
  theme_classic()

dev.off()

#overlay multiple parameters on one graph 
#think about the connections between certain parameters, like temp & pH etc

#use lab wq data 
#read file 
bmwq <- read_csv('bmwqlab.csv')


#format date into 
bmwq$Date<-as.Date(bmwq$Date,format="%m/%d/%y")

#graph a parameter over time 
ggplot(bmwq, aes(x = Date, y = Chl))+ 
  geom_line() + 
  scale_fill_manual(values=cbPalette) +
  ggtitle("Baby Marsh Chl-a ") +
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  labs(x= "Date", y= "Chl Conc (ug/L)") +
  theme_classic()

#export graph
png("bm_chl.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(bmwq, aes(x = Date, y = Chl))+ 
  geom_line() + 
  geom_point() +
  scale_fill_manual(values=cbPalette) +
  ggtitle("Baby Marsh Chl-a ") +
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  labs(x= "Date", y= "Chl Conc (ug/L)") +
  theme_classic()

dev.off()

#bring in all lab wq data
labwq <-read.csv("labwq.csv")

#check for unique site names
unique(labwq$Site)

#format date into 
labwq$Date<-as.Date(labwq$Date,format="%m/%d/%y")

#parce out the indivudal sites
mokwq<-labwq%>% 
  filter(Site == "Mok-USRR")

sacwq<-labwq%>% 
  filter(Site == "XSSAC")

cbwq<-labwq%>% 
  filter(Site == "Cos Beach")

kngf6wq<-labwq%>% 
  filter(Site == "Knaggs F-6")

kngf3wq<-labwq%>% 
  filter(Site == "Knaggs-F3")

bmwq <-labwq%>% 
  filter(Site == "Baby Marsh")

#making a graph before facet graph
ggplot(bmwq, aes(x=Date, y=Chl))+
  geom_bar(stat='identity', fill="forest green")+
  ylab("Chl-a (ug/L)") +
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_classic()

#learn how to use facet wrap 
bmwqlong <- gather(bmwq, key="measure", value="value", c("Chl", "DOC.ppm", "TN.ppm", "TP.ppm"))

#facet wrap attempt 1 Baby Marsh 
ggplot(bmwqlong, aes(x=Date, y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure,  ncol=2) 

#Mok facet wrap 
mokwqlong <- gather(mokwq, key="measure", value="value", c("Chl", "DOC.ppm", "TN.ppm", "TP.ppm"))

#facet wrap attempt 2 mok
ggplot(mokwqlong, aes(x=Date, y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure,  ncol=2) 

#export bm facet graph
png("bm_facet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(bmwqlong, aes(x=Date, y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure,  ncol=2) 

dev.off()

#export mok 
png("mok_facet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(mokwqlong, aes(x=Date, y=value))+
  geom_bar(stat='identity', fill="forest green")+
  facet_wrap(~measure,  ncol=2) 

dev.off()

#New script
#putting all sites on the same grid 
#trying to facet grid one parameter(pH) for all sites
p <- ggplot(darkcbn.wq, aes(Date, pH)) + geom_point()

p + facet_grid(cols = vars(Site)) + 
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") + 
  coord_cartesian(ylim = c(2, 17))

#export to show and get feedback
#how should I compare with Knaggs vs other sites
#how clear can I make it 

png("pH_facet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

p + facet_grid(cols = vars(Site)) + 
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") + 
  coord_cartesian(ylim = c(2, 17))

dev.off()

#need to combine Knaggs together and maybe omit wendels?
#so take out BM, CB, Knaggs, Mok, XSSAC 
#might need to do knaggs seperately then wrap it in with its on y scale???
#y axis should be limited to 20 or even less 

#trying to facet grid one parameter(Chl) for all sites
c <- ggplot(darkcbn.wq, aes(Date, Chl)) + geom_point()

c + facet_grid(cols = vars(Site)) + 
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") 

#export to show and get feedback

png("chl_facet.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

c + facet_grid(cols = vars(Site)) + 
  scale_x_date(date_labels = "%b-%y", breaks= "6 months") 

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

