#working on size distribution 
rm(list=ls())

#load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(vegan)
library(ggplot2)
library(scales)
library(see)
library(patchwork)
library(gghalves)

#zoop data collected and updated september 2020 
fishfood.dc<-read.csv("DCZoop_size.csv")

#format date into 
fishfood.dc$Date<-as.Date(fishfood.dc$Date,format="%m/%d/%y")

#check to see how many sites there are and change to have only five 
unique(fishfood.dc$Site)
unique(fishfood.dc$Date)

#paste life stage together 
fishfood.dc$splife <- paste(fishfood.dc$Species,fishfood.dc$Life.Stage)

#splife to a factor 
fishfood.dc$splife <- as.factor(fishfood.dc$splife)
fishfood.dc$Size <- as.factor(fishfood.dc$Size)

na.omit(fishfood.dc)
#filter out individual sampling dates and sites?? no 
size119 <- fishfood.dc %>%
  select(Date, Order, Size, Species, Size.with.Tail, splife) %>%
  filter(Date >= as.Date("2019/02/25") & Date <= as.Date("2019/02/28"))

#need to exclude Eucypris adult, Harpacticoida adult, Ilyocryptus adult, Mendotae adult, Eurycercus adult b/c there are not enough data points
#just did it myself bc I couldnt make code work 
final1<-read.csv("first round.csv")

ggplot(final1, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

size219<- fishfood.dc %>%
  select(Date, Order, Size, Species, Size.with.Tail, splife) %>%
  filter(Date >= as.Date("2019/03/04") & Date <= as.Date("2019/03/10"))

size319<-fishfood.dc%>% 
  select(Date, Order, Size, Species, Size.with.Tail, splife) %>%
  filter(Date >= as.Date("2019/03/11") & Date <= as.Date("2019/03/14"))

size419<-fishfood.dc%>% 
  select(Date, Order, Size, Species, Size.with.Tail, splife) %>%
  filter(Date >= as.Date("2019/03/18") & Date <= as.Date("2019/03/22"))

size519 <-fishfood.dc%>% 
  select(Date, Order, Size, Species, Size.with.Tail, splife) %>%
  filter(Date >= as.Date("2019/03/25") & Date <= as.Date("2019/03/27"))

size619 <-fishfood.dc%>% 
  select(Date, Order, Size, Species, Size.with.Tail, splife) %>%
  filter(Date >= as.Date("2019/04/01") & Date <= as.Date("2019/04/04"))

#break down by sampling date should be 6 
ggplot(size119, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(size219, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(size319, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(size419, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(size519, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(size619, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#half violins 
#Export graph 1
png("evencloserhalfviolin1.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(final1, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dev.off()

#Export graph 2
png("violin graph 2.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(size219, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dev.off()

#Export graph 3
png("violin graph 3.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(size319, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dev.off()

#Export graph 4
png("violin graph 4.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(size419, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)))

dev.off()

#Export graph 5
png("violin graph 5.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(size519, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dev.off()

#Export graph 6
png("violin graph 6.png", width = 6.5, height = 4, units = "in", res = 500, family = "sans")

ggplot(size619, aes(x = splife, y = Size, fill = splife)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

dev.off()
