#use this script for WQ data



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


