#use this script for WQ data
#LABWQ Data done by Marissa (edits by RAP)

#redoing to omit all the other things I've done
##might need those later
library(lubridate)
library(tidyverse) # includes dplyr, ggplot, readr, etc
#library(reshape2) # would recommend using dplyr::pivot_longer/wider options
library(vegan)
library(scales)

# rm(list = ls()) # avoid this, try to change global setting to start with clean enviro
# see Global Options > uncheck Restore .RData into workspace at Startup

#load csv 
darkcbn.wq <-read_csv("data/2020_01_labwq.csv") # use relative path

#format date into 
darkcbn.wq$Date<-mdy(darkcbn.wq$Date) # lubridate is better at reporting errors

#check to see how many sites there are and change to have only five 
unique(darkcbn.wq$Site)



# Plot WQ -----------------------------------------------------------------

#new work with WQ 
#goal get all sites on one graph and distingush by colors

# add colors: this package good for diverging/distinct palettes
library(randomcoloR)
colPal <- randomcoloR::distinctColorPalette(length(unique(darkcbn.wq$Site)), 
                                            runTsne = TRUE)



# Plot PH -----------------------------------------------------------------

#pH
(pPH <- ggplot(data = darkcbn.wq, aes(x = Date, y = pH), position="dodge")+
  geom_point(aes(fill=Site), pch=21, size=3)+
  labs(x = "Date ", y = "pH", title = "pH") +
   scale_fill_manual(values=colPal) +
  scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# use ggsave for simplicity
ggsave("figs/wq_pH_point.png", width = 6.5, height = 4, units="in", dpi=300)

# box pH
(bxPH <- ggplot(data = darkcbn.wq, aes(x=Site, y = pH, group=Site))+
    # add scatter behind
    geom_jitter(data=darkcbn.wq, aes(x=Site, y=pH, color=Date), alpha=0.7) +
    geom_boxplot(aes(fill=Site), alpha=0.5) +
    labs(x = "Date ", y = "pH", title = "pH") +
    scale_fill_manual(values=colPal) +
    scale_color_date()+
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))
ggsave("figs/wq_pH_box.png", width = 6.5, height = 4, units="in", dpi=300)


# Chl-A -------------------------------------------------------------------


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

# EC ----------------------------------------------------------------------


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

# Turbidity ---------------------------------------------------------------


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

# DOC ---------------------------------------------------------------------

(pDOC <- ggplot(data = darkcbn.wq, aes(x = Date, y = DOC), position="dodge")+
   geom_point(aes(fill=Site), pch=21, size=3)+
   labs(x = "Date ", y = "DOC (ppm)", title = "DOC") +
   scale_fill_manual(values=colPal) +
   scale_x_date(date_labels = "%b-%y", breaks= "2 months") +
   theme_bw(base_size = 10) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# use ggsave for simplicity
ggsave("figs/wq_DOC_point.png", width = 6.5, height = 4, units="in", dpi=300)

# boxplot
(bxDOC <- ggplot(data = darkcbn.wq, aes(x=Site, y = DOC, group=Site))+
    # add scatter behind
    geom_jitter(data=darkcbn.wq, aes(x=Site, y=DOC, color=Date), alpha=0.7) +
    geom_boxplot(aes(fill=Site), alpha=0.5) +
    labs(x = "Date ", y = "DOC (ppm)", title = "DOC") +
    scale_fill_manual(values=colPal) +
    scale_color_date()+
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))
ggsave("figs/wq_DOC_box.png", width = 6.5, height = 4, units="in", dpi=300)


# P04 ---------------------------------------------------------------------


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
