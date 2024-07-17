title: "Fit GAMs to ARC Data.Rmd"
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


## make function doy/month/year etc...

# calculate predictor variables
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



model.2021 <- with(arc2021EXCR,gam(rates.es ~
                                     #s(hour,bs="cc",k=4) +
                                     s(mid.salinity)+
                                     s(surf.o2)+
                                     s(windspd)+
                                     s(winddirection)+
                                     s(tide,bs="cc",k=6)+
                                     ti(chl.ugl,surf.o2)+
                                     #ti(chl.ugl,par,k=4)+
                                     #ti(par,mid.o2)+
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

summary(model.2021)




model.2020 <- with(arc2020EXCR,gam(rates.es ~
                                     #s(hour,bs="cc",k=4) +
                                     s(mid.salinity)+
                                     s(surf.o2)+
                                     s(windspd)+
                                     s(winddirection)+
                                     s(tide,bs="cc",k=6)+
                                     ti(windspd,winddirection)+
                                     ti(tide,surf.salinity)+
                                     ti(windspd,surf.salinity)+
                                     ti(surf.temp,mid.o2)+
                                     ti(surf.salinity,surf.o2)+
                                     ti(surf.salinity,mid.o2)+
                                     ti(mid.salinity,mid.o2)
                                   
                                
))

summary(model.2020)

#model surface rates at EXCR in 2022
model.2022 <- with(arc2022EXCR,gam(rates.es ~
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

summary(model.2022)

#model deep rates at EXCR in 2022
model.2022b <- with(arc2022EXCR,gam(rates.ed ~
                                     #s(hour,bs="cc",k=4) +
                                     s(deep.salinity)+
                                     s(deep.o2)+
                                     s(windspd)+
                                     s(winddirection)+
                                     s(tide,bs="cc",k=6)+
                                     ti(chl.ugl,deep.o2)+
                                     #ti(chl.ugl,par,k=4)+
                                     ti(par,deep.o2)+
                                     ti(chl.ugl,deep.salinity)+
                                     ti(windspd,winddirection)+
                                     ti(tide,deep.salinity)+
                                     ti(windspd,tide)+
                                     ti(windspd,deep.salinity)+
                                     ti(deep.temp,deep.salinity)+
                                     ti(deep.salinity,mid.o2)
                                   
))

summary(model.2022b)

plotGAMpredictions.1 <- function(inModel,inData) {
  
  # Plot the predicted values for 
  rate.predicted <- predict(inModel,inData)
  inData <- inData %>% mutate(residual=rates.es-rate.predicted)
  sefit <- predict(inModel,inData,se.fit = TRUE)
  plt.1c <- ggplot()+
    geom_point(data=inData,aes(x=timestamp.rates,y=rates.es),color="light grey",size=1,shape=1)+
    geom_line(data=inData,aes(x=timestamp.rates,y=rate.predicted,group=factor(year(timestamp.rates))),color=pal[1])+
    theme_classic()+
    scale_x_datetime(date_breaks="2 weeks",date_labels = "%m/%y")+
    ggtitle("Respiration Rates at EXCR Surface 2022")
  
  plt.2c <- ggplot()+
    geom_point(data=inData,aes(x=timestamp.rates,y=residual),color=pal[1],size=1.5,shape=1)+
    theme_classic()+
    scale_x_datetime( date_breaks="2 months",date_labels = "%m/%y")+
    ggtitle("Residual Respiration Rates at EXCR Surface 2022 ")
  
  return(list(plot1c=plt.1c,plot2c=plt.2c))
}
plotGAMpredictions.1b <- function(inModel,inData) {
  
  # Plot the predicted values for 
  rate.predicted <- predict(inModel,inData)
  inData <- inData %>% mutate(residual=rates.ed-rate.predicted)
  sefit <- predict(inModel,inData,se.fit = TRUE)
  plt.1c <- ggplot()+
    geom_point(data=inData,aes(x=timestamp.rates,y=rates.ed),color="light grey",size=1,shape=1)+
    geom_line(data=inData,aes(x=timestamp.rates,y=rate.predicted,group=factor(year(timestamp.rates))),color=pal[1])+
    theme_classic()+
    scale_x_datetime(date_breaks="2 weeks",date_labels = "%m/%y")+
    ggtitle("Respiration Rates at EXCR Deep 2022")
  
  plt.2c <- ggplot()+
    geom_point(data=inData,aes(x=timestamp.rates,y=residual),color=pal[1],size=1.5,shape=1)+
    theme_classic()+
    scale_x_datetime( date_breaks="2 months",date_labels = "%m/%y")+
    ggtitle("Residual Respiration Rates at EXCR Deep 2022 ")
  
  return(list(plot1c=plt.1c,plot2c=plt.2c))
}


output.1 <- plotGAMpredictions.1(model.2022,arc2022EXCR)
output.1b <- plotGAMpredictions.1b(model.2022b,arc2022EXCR)

output.1$plot1c
output.1b$plot1c



model.2022bed <- with(arc2022EXCR,gam(rates.ed ~
                                     #s(hour,bs="cc",k=4) +
                                     s(deep.salinity)+
                                    s(deep.o2)+
                                    s(surf.o2)+
                                    s(deep.temp)
                                     
))

summary(model.2022bed)

#predict 2020 rates

arc2022EXCR$predicted.rates <- predict(model.2021,arc2022EXCR)

arc2022EXCR %>% 
  ggplot(aes(x=timestamp.rates))+
  geom_point(aes(y=predicted.rates),color=pal[4])+
  geom_point(aes(y=rates.es),color=pal[2])+
  basic_theme+
  labs(x="Date",y="Respiration Rates")



### pivot longer to transpose data
### github repository for ARCs data