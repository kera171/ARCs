title: "Predict dissolved O2 in WLIS"
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

# For Anna lisa:
setwd("C:/Users/amudahy/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Rdata/ARCs")
#load("~/Rdata/arc2021excr.Rdata")
load("~/Rdata/ARCs/R data/arc2020excr.Rdata")
load("~/Rdata/ARCs/R data/arc2021excr.Rdata")
load("~/Rdata/ARCs/R data/tide2021ARCS.Rdata")
load("~/Rdata/ARCs/R data/arc2022excr.Rdata")
load("~/Rdata/ARCs/R data/tide2020ARCS.Rdata")

#arc2020EXCR$
names(arc2021EXCR) <- c ('timestamp.rates','rates.es','rates.ed', 'surf.salinity',
                         'mid.salinity','deep.salinity','surf.temp','mid.temp','deep.temp',
                         'surf.o2','mid.o2','deep.o2','chl.ugl','surf.pressure','mid.pressure','deep.pressure',
                         'density.es','rho.dif.es','rho.delta.es','windspd','winddirection'
)

names(arc2020EXCR) <- c ('timestamp.rates','rates.es', 'surf.salinity',
                         'mid.salinity','deep.salinity','surf.temp','mid.temp','deep.temp',
                         'surf.o2','mid.o2','deep.o2','windspd','winddirection'
)
names(arc2022EXCR) <- c ('timestamp.rates','rates.es','rates.ed', 'surf.salinity',
                         'mid.salinity','deep.salinity','surf.temp','mid.temp','deep.temp',
                         'surf.o2','mid.o2','deep.o2','chl.ugl','par','tide','windspd','winddirection','surf.pressure','mid.pressure','deep.pressure',
                         'density.es','rho.dif.es','rho.delta.es'
)


tide.1$TimeStamp.EST <- as_datetime(tide.1$TimeStamp.UTC,tz="Jamaica")

tide.2$TimeStamp.EST <- as_datetime(tide.2$TimeStamp.UTC,tz="Jamaica")

arc2021EXCR$tide <- approx(tide.2$TimeStamp.EST, tide.2$height.m,arc2021EXCR$timestamp.rates) %>%
  .[[2]]

arc2020EXCR$tide <- approx(tide.1$TimeStamp.EST, tide.1$height.m,arc2020EXCR$timestamp.rates) %>%
  .[[2]]

arc2020EXCR$chl.ugl <- NA
arc2020EXCR$density.es <- NA
arc2020EXCR$rho.dif.es <- NA
arc2020EXCR$rho.delta.es <- NA
arc2020EXCR$surf.pressure <- NA
arc2020EXCR$mid.pressure <- NA
arc2020EXCR$deep.pressure <- NA
arc2020EXCR$rates.ed <- NA
arc2020EXCR$surf.pressure <- NA

alldata <- rbind(arc2020EXCR,arc2021EXCR,arc2022EXCR)

##create time variables

timevariables <- function(data) {
  data<- data%>% 
  mutate(
    month=month(timestamp.rates),
    hour=hour(timestamp.rates),   # hour of the day, 0-23
    doy=yday(timestamp.rates)+hour/24,  # decimal day of year
    year=year(timestamp.rates)+doy/365.25)
}


timevariables(arc2020EXCR)
timevariables(arc2021EXCR)
timevariables(arc2022EXCR)

arc2020EXCR<- arc2020EXCR%>% 
  mutate(
    month=month(timestamp.rates),
    hour=hour(timestamp.rates),   # hour of the day, 0-23
    doy=yday(timestamp.rates)+hour/24,  # decimal day of year
    year=year(timestamp.rates)+doy/365
  )


arc2021EXCR<- arc2021EXCR%>% 
  mutate(
    month=month(timestamp.rates),
    hour=hour(timestamp.rates),   # hour of the day, 0-23
    doy=yday(timestamp.rates)+hour/24,  # decimal day of year
    year=year(timestamp.rates)+doy/365
  )
arc2022EXCR<- arc2022EXCR%>% 
  mutate(
    month=month(timestamp.rates),
    hour=hour(timestamp.rates),   # hour of the day, 0-23
    doy=yday(timestamp.rates)+hour/24,  # decimal day of year
    year=year(timestamp.rates)+doy/365
  )



modelo2.1 <- with(arc2021EXCR,gam(deep.o2~
                                s(doy,bs="cc")+
                                s(hour,bs="cc")+
                                s(deep.temp)+
                                s(tide,bs="cc")+
                                ti(windspd,winddirection)
                              ))
summary(modelo2.1)


arc2022EXCR$predicted.o2 <- predict(modelo2.1,arc2022EXCR)

arc2022EXCR %>% 
  ggplot(aes(x=timestamp.rates))+
  geom_point(aes(y=predicted.o2),color=pal[4])+
  geom_point(aes(y=deep.o2),color=pal[2])+
  basic_theme+
  labs(x="Date",y="DO")

arc2020EXCR$predicted.o2 <- predict(modelo2.1,arc2020EXCR)

arc2020EXCR %>% 
  ggplot(aes(x=timestamp.rates))+
  geom_point(aes(y=predicted.o2),color=pal[4])+
  geom_point(aes(y=deep.o2),color=pal[2])+
  basic_theme+
  labs(x="Date",y="DO")

###----TEMP PREDICITIONS----

tempmodel <- with(arc2021EXCR,gam(deep.temp~
                                    s(doy,bs="cc")+
                                    s(hour,bs="cc")+
                                    s(tide,bs="cc")
                                    #ti(windspd,winddirection)
))
summary(tempmodel)

arc2020EXCR$predicted.temp <- predict(tempmodel,arc2020EXCR)

arc2022EXCR %>% 
  ggplot(aes(x=timestamp.rates))+
  geom_point(aes(y=predicted.temp),color=pal[4])+
  geom_point(aes(y=deep.temp),color=pal[2])+
  basic_theme+
  labs(x="Date",y="Temp")

save(tide.2,file="tide2021.csv")

####get 6 hour average of o2 after each rate super important