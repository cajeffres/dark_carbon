#use this script for fish analysis
library(usethis)

use_git_config(user.name = "carson_jeffres", user.email= "cajeffres@ucdavis.edu")

#get a situation report
git_sitrep()

library(tidyverse)
library(lubridate)

fish_data <- read.csv("data/cagedata_NSC_06_29_20.csv", header = T)

fish_data$Date <- mdy(fish_data$Date)

fish_data <- fish_data %>% 
  mutate("year" = lubridate::year(Date))

fish_2019<- fish_data %>% 
  filter (year == "2019")

fish_2020 <- fish_data %>% 
  filter (year == "2020")

#make graph of 2019 fish growth data *** NEED TO ADD sAC DATA***

graph_2019 <- ggplot(fish_2019, aes(x=factor(Date), FL_mm))+
  geom_boxplot(aes(fill= Site), position = position_dodge (0.9))+
  theme_bw()
 

graph_2019

#make graph of 2020 data  ***NEED TO GET START SAC DATA AND CLEAN KNAGGS***
graph_2020 <- ggplot(fish_2020, aes(x=factor(Date), FL_mm))+
  geom_boxplot(aes(fill= Site), position = position_dodge (0.9))+
  theme_bw()


graph_2020
