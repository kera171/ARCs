title: "Fit GAMs to ARC Data.Rmd"
author: " A Mudahy"
date: "`r Sys.Date()`"
output: html_document
---
  
  
  #install.packages("remotes")
  #remotes::install_github("stefanocoretta/tidygam@v0.1.0")
  
  #library(remotes)
  #remotes::install_github("gavinsimpson/gratia")
  
  #install.packages('gratia')
  #install.packages('gridExtra')
  
library(tidyverse)
library(lubridate)
library(wesanderson)
library(marelac)
library(mgcv)
#library(tidygam)
library(gratia)
library(gridExtra)
#library(bayesbio)


pal <- wes_palette("Zissou1",5,type="discrete")
basic_theme <- theme(panel.grid=element_blank(),
                     axis.text =  element_text(size=14),
                     strip.text = element_text(size=14),
                     axis.title = element_text(size=14)
)

#source("<file containing the functions>")

# For Anna lisa:
setwd("C:/Users/amudahy/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Rdata/ARCs")
#load("~/Rdata/arc2021excr.Rdata")
load("~/Rdata/ARCs/R data/arc2020excr.Rdata")
load("~/Rdata/ARCs/R data/arc2021excr.2.Rdata")
load("~/Rdata/ARCs/R data/tide2021ARCS.Rdata")
names(arc2021EXCR.2) <- c ('Timestamp.Rates','Rates.ES','Rates.ED', 'surf.salinity',
                           'mid.salinity','deep.salinity','surf.temp','mid.temp','deep.temp',
                           'surf.o2','mid.o2','deep.o2','chl.ugl','surf.pressure','mid.pressure','deep.pressure',
                           'density.es','rho.dif.es','rho.delta.es'
)

names(arc2020EXCR) <- c ('Timestamp.Rates','Rates.ES', 'surf.salinity',
                           'mid.salinity','deep.salinity','surf.temp','mid.temp','deep.temp',
                           'surf.o2','mid.o2','deep.o2'
)



tide.2$TimeStamp.EST <- as_datetime(tide.2$TimeStamp.UTC,tz="Jamaica")

arc2021EXCR.2$tide.height.m <- approx(tide.2$TimeStamp.EST, tide.2$height.m,arc2021EXCR.2$Timestamp.Rates) %>%
  .[[2]]



# calculate predictor variables
arc2021EXCR.2<- arc2021EXCR.2%>% 
  mutate(
    month=month(Timestamp.Rates),
    hour=hour(Timestamp.Rates),   # hour of the day, 0-23
    doy=yday(Timestamp.Rates)+hour/24,  # decimal day of year
    year=year(Timestamp.Rates)+doy/365
  )

model.1 <- with(arc2021EXCR.2,gam(Rates.ES ~
                                    #s(hour,bs="cc",k=4) +
                                    s(surf.salinity)+
                                    mid.salinity+
                                    s(surf.temp)+
                                    s(mid.temp)+
                                    s(surf.o2)+
                                    s(mid.o2)+
                                    ti(surf.temp,surf.o2)+
                                    ti(surf.temp,mid.o2)+
                                    ti(mid.temp,mid.o2)+
                                    ti(surf.salinity,surf.o2)+
                                    ti(surf.salinity,mid.o2)+
                                    ti(mid.salinity,surf.o2)+
                                    ti(mid.salinity,mid.o2)
                                  
))

model.3<- with(arc2021EXCR.2,gam(Rates.ES ~ 
                                   s(surf.salinity)+
                                   s(mid.salinity,k=3)+
                                   s(mid.o2,k=3)+
                                   s(surf.o2)+
                                   ti(mid.salinity,mid.o2,k=3)+
                                   ti(surf.salinity,surf.o2,k=3)
))
arc2020EXCR$predicted.rates <- predict(model.1,arc2020EXCR)


 arc2020EXCR %>% 
  ggplot(aes(x=Timestamp.Rates))+
  geom_point(aes(y=predicted.rates),color=pal[4])+
  geom_point(aes(y=Rates.ES),color=pal[2])+
  basic_theme+
  labs(x="Date",y="Respiration Rates")

 ### pivot longer to transpose data
 ### github repository for ARCs data