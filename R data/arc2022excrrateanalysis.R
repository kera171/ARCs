title: "Fit GAMs to ARC 2022 Data.Rmd"
author: " A Mudahy"
date: "`r Sys.Date()`"
output: html_document
---
  
  
  
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


setwd("C:/Users/amudahy/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Rdata/ARCs")
load("~/Rdata/ARCs/R data/arc2022excr.Rdata")


names(arc2022EXCR) <- c ('timestamp.rates','rates.es','rates.ed', 'surf.salinity',
                         'mid.salinity','deep.salinity','surf.temp','mid.temp','deep.temp',
                         'surf.o2','mid.o2','deep.o2','chl.ugl','par','tide','windspd','winddirection','surf.pressure','mid.pressure','deep.pressure',
                         'density.es','rho.dif.es','rho.delta.es'
)



arc2022EXCR<- arc2022EXCR%>% 
  mutate(
    month=month(timestamp.rates),
    hour=hour(timestamp.rates),   # hour of the day, 0-23
    doy=yday(timestamp.rates)+hour/24,  # decimal day of year
    year=year(timestamp.rates)+doy/365
  )


model.1 <- with(arc2022EXCR,gam(rates.es ~
                                  #s(hour,bs="cc",k=4) +
                                  s(mid.salinity)+
                                  s(surf.o2)+
                                  s(windspd)+
                                  s(winddirection)+
                                  s(tide,bs="cc",k=6)+
                                  ti(chl.ugl,surf.o2)+
                                  #ti(chl.ugl,par,k=4)+
                                  ti(par,mid.o2)+
                                  ti(chl.ugl,surf.temp)+
                                  ti(windspd,winddirection)+
                                  ti(tide,surf.salinity)+
                                  ti(windspd,tide)+
                                  ti(windspd,surf.salinity)+
                                  ti(surf.temp,mid.o2)+
                                  ti(surf.salinity,surf.o2)+
                                  ti(surf.salinity,mid.o2)+
                                  ti(mid.salinity,mid.o2)
                                
))

summary(model.1)