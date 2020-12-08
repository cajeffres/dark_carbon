#use this script for WQ data
#WQ data
#load csv 
darkcbn.wq <-read_csv("darkcarbonwq20.csv")

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

