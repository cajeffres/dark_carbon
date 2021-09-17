#use this script for zoop analysis
#dark carbon wq and zoop data analysis
#1 create zoop graphs comparing biomass, abundance, and species compostion
#2 look at changes between specific parameters relating to water quality over time

#load libraries
library(lubridate)
library(tidyverse)
library(reshape2)
library(vegan)
library(scales)

#update July 2021 
zoop.dc2<-read.csv("data/darkcarbon_zoop_all.csv")

#format date into 
zoop.dc2$Date<-mdy(zoop.dc2$Date)

#check to see how many sites there are and change to have only five 
unique(zoop.dc2$Site)
unique(zoop.dc2$Group)

# use 'across' to convert char to numeric
zoop.dc2 <- zoop.dc2 %>% 
  mutate(across(.cols = c(FlowMeterBegin, FlowMeterEnd, TotalVolume_ml,
                          Volumesubsampled_ml, RingSize_cm, abundance,
                          SplitFraction, MeshSize_Microns), as.numeric))
# warning about NAs, but that's ok
summary(zoop.dc2)
dim(zoop.dc2)

# if we want to drop NAs, we can do the following:
zoop.dc2.nona <- drop_na(zoop.dc2) # but this drops everything
# lets drop just date nas
zoop.dc2.nona <- zoop.dc2 %>% filter(!is.na(Date))
dim(zoop.dc2.nona)

#abundance function 
abundance<-function(df) {
  df$distance<-(df$FlowMeterEnd-df$FlowMeterBegin)*26873/999999
  df$RingSize_m<-df$RingSize_cm/100
  df$hooparea<-(pi*(df$RingSize_m)^2)/4
  df$volumesampled<-df$distance*df$hooparea
  df$samplecount<-(df$TotalVolume_ml/df$Volumesubsampled_ml)*df$abundance
  df$ind_m3<-df$samplecount/df$volumesampled
  return(df)
}

#use abundance function
drkcbnabundance2 <-abundance(zoop.dc2.nona)


# Data Subsetting ---------------------------------------------------------

#Group by site, date, and order 
drkcbnzoop.sum2 <-drkcbnabundance2 %>% 
  group_by(Site, Date, Order, Subclass, Group, Species, LifeStage) %>% 
  summarise(Total=sum(ind_m3)) 

#do only january 2019 end of december 2020 
drkcbnzoop.sum3 <- drkcbnzoop.sum2 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>% 
  filter(Date >= ymd("2019/01/01") & Date <= ymd("2020/12/31"))

#subset 2019
zoop19<- drkcbnzoop.sum3 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>%
  filter(Date >= ymd("2019/01/01") & Date <= ymd("2019/12/31"))

#subset 2020
zoop20<- drkcbnzoop.sum3 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>%
  filter(Date >= ymd("2020/01/01") & Date <= ymd("2020/12/31"))

#check sites 
unique(drkcbnzoop.sum2$Site)

# 2019-2020 Plot ---------------------------------------------------------------

# make max number of possible Groups for colors
mxColors <- length(unique(drkcbnzoop.sum2$Group))

# nice color packages:
# colorspace::hcl_palettes()
library(randomcoloR)
colPal <- randomcoloR::distinctColorPalette(mxColors, runTsne = TRUE)

#plot of  both years
(zpBoth <- ggplot(drkcbnzoop.sum3, aes(x=Date, y=Total, color=Group, fill=Group)) +
    geom_bar(stat="identity") +
    labs(title = "Yearly Abundance: 2019-2020") +
    facet_grid(Site~., scales = "free_y") +
    geom_vline(xintercept = ymd("2019-10-01"), color="maroon") +
    #scale_fill_manual(values = RColorBrewer::brewer.pal(mxColors, "BrBG")) +
    scale_color_manual(values=colPal) +
    scale_fill_manual(values=colPal) +
    scale_x_date(date_labels = "%b-%Y", breaks= "3 months") +
    scale_y_continuous(label=comma) +
    theme_bw())

ggsave(zpBoth, filename = "figs/zoop_abundance_2019-2020.png", width=9, height = 6.5, 
       units="in", dpi=300)

# 2019 Plot ---------------------------------------------------------------

# graph for 2019 
(zp2019 <- ggplot(zoop19, aes(x=Date, y=Total, color=Group, fill=Group)) +
   geom_bar(stat="identity") +
   labs(title = "Yearly Abundance: 2019") +
   facet_grid(Site~., scales = "free_y") +
   scale_color_manual(values=colPal) +
   scale_fill_manual(values=colPal) +
   scale_x_date(date_labels = "%b-%Y", breaks= "3 months") +
   scale_y_continuous(label=comma) +
   theme_bw())

ggsave(zp2019, filename = "figs/zoop_abundance_2019.png", width=9, height = 6.5, 
       units="in", dpi=300)


# 2020 Plot ---------------------------------------------------------------

#graph for 2020 
(zp2020 <- ggplot(zoop20, aes(x=Date, y=Total, color=Group, fill=Group)) +
   geom_bar(stat="identity") +
   labs(title = "Yearly Abundance: 2020") +
   facet_grid(Site~., scales = "free_y") + # turn off scales free y if needed
   scale_color_manual(values=colPal) +
   scale_fill_manual(values=colPal) +
   scale_x_date(date_labels = "%b-%y", breaks= "3 months") +
   scale_y_continuous(label=comma) +
   theme_bw())

ggsave(zp2020, filename = "figs/zoop_abundance_2020.png", width=9, height = 6.5, 
       units="in", dpi=300)


# Break Cage Dates Down ---------------------------------------------------

#coming back to break cage dates down for 2019 and 2020
#subset 2019 field dates aka wet periods

zoop19s<- drkcbnzoop.sum3 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>%
  filter(Date >= ymd("2019/01/01") & Date <= ymd("2019/04/30"))

#subset 2020 field dates 
zoop20s<- drkcbnzoop.sum3 %>%
  select(Site, Date, Order, Subclass, Total, Group, Species, LifeStage) %>%
  filter(Date >= ymd("2020/01/01") & Date <= ymd("2020/04/30"))


# Plot Wet Dates 2019 -----------------------------------------------------

#graph for 2019 wet dates
(wzp19 <- ggplot(zoop19s, aes(x=Date, y=Total, color=Group, fill=Group)) +
   geom_bar(stat="identity") +
   labs(title = "Abundance: 2019") +
   #facet_grid(Site~., scales = "free_y") + # turn off scales free y if needed
   facet_grid(Site~.) + # static y scale
   scale_color_manual(values=colPal) +
   scale_fill_manual(values=colPal) +
   scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
   scale_y_continuous(label=comma) +
   theme_bw())
ggsave(wzp19, filename = "figs/zoop_abundance_jan_apr_2019.png", width = 10, height = 8, 
       units="in", dpi=300)



#graph for 2020 
(wzp20 <- ggplot(zoop20s, aes(x=Date, y=Total, color=Group, fill=Group)) +
    geom_bar(stat="identity") +
    labs(title = "Abundance: 2020") +
    facet_grid(Site~., scales = "free_y") + # turn off scales free y if needed
    #facet_grid(Site~.) + # static y scale
    scale_color_manual(values=colPal) +
    scale_fill_manual(values=colPal) +
    scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
    scale_y_continuous(label=comma) +
    theme_bw())

# Export all the graph and ask for feedback 
ggsave(wzp20, filename = "figs/zoop_abundance_jan_apr_2020.png", width = 10, height = 8, 
       units="in", dpi=300)

# Log ---------------------------------------------------------------------

#logging abundance graphs - July 12th 
#using ggplot call y= log(abundance)

wzp20 + 
  labs(y="Total (logscale)")+
  scale_y_log10()
ggsave("figs/zoop_abundance_jan_apr_2020_logscale.png",
       width = 10, height = 8, 
       units="in", dpi=300)

# now do log of actual value
(wzp20log <- ggplot(zoop20s, aes(x=Date, y=log(Total), color=Group, fill=Group)) +
    geom_bar(stat="identity") +
    labs(title = "Abundance: 2020", y="Log(Total)") +
    facet_grid(Site~., scales = "free_y") + # turn off scales free y if needed
    #facet_grid(Site~.) + # static y scale
    scale_color_manual(values=colPal) +
    scale_fill_manual(values=colPal) +
    scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
    scale_y_continuous(label=comma) +
    theme_bw())
ggsave("figs/zoop_abundance_jan_apr_2020_log.png",
       width = 10, height = 8, 
       units="in", dpi=300)

(wzp19log <- ggplot(zoop19s, aes(x=Date, y=log(Total), color=Group, fill=Group)) +
    geom_bar(stat="identity") +
    labs(title = "Abundance: 2019", y="Log(Total)") +
    facet_grid(Site~., scales = "free_y") + # turn off scales free y if needed
    #facet_grid(Site~.) + # static y scale
    scale_color_manual(values=colPal) +
    scale_fill_manual(values=colPal) +
    scale_x_date(date_labels = "%b-%y", breaks= "1 months") +
    scale_y_continuous(label=comma) +
    theme_bw())
ggsave("figs/zoop_abundance_jan_apr_2019_log.png",
       width = 10, height = 8, 
       units="in", dpi=300)


