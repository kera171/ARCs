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


##-----rate vs tide

arc2021EXCR.2 %>% ggplot(aes(tide.height.m,Rates.ES))+
  geom_point(color="blue")
arc2021EXCR.2 %>% ggplot(aes(tide.height.m,Rates.ED))+
                  geom_point(color="blue")
### make plot for diel cycle, tidal cycle,in surface and deep

###------SURFACE
arc2021EXCR.2 %>% 
  filter(!is.na(month)) %>% 
  mutate(month=factor(month,labels=month.name[c(6,7,8,9,10)])) %>%  # set factor labls to month words
  ggplot(aes(x=tide.height.m,y=Rates.ED))+
  facet_wrap(~month)+
  geom_boxplot(fill=pal[2])+
  basic_theme+
  labs(x="Tide Height",y="Respioration Rate ED")

###------DEEP

arc2021EXCR.2 %>% 
  filter(!is.na(month)) %>% 
  mutate(month=factor(month,labels=month.name[c(6,7,8,9,10)])) %>%  # set factor labls to month words
  ggplot(aes(x=hour,y=Rates.ED,group=hour))+
  facet_wrap(~month)+
  geom_boxplot(fill=pal[2])+
  basic_theme+
  labs(x="Hour of Day",y="Respioration Rate Deep")

nrow(alldata %>% 
       filter(between(TimeStampEST,as.POSIXct("2022-02-01"),as.POSIXct("2023-02-01")))
)
###-------residuals
arc2021EXCR.2 %>% 
  filter(!is.na(month)) %>% 
  mutate(month=factor(month,labels=month.name[c(6,7,8,9,10)])) %>%  # set factor labls to month words
  ggplot(aes(x=hour,y=residuals,group=hour))+
  facet_wrap(~month)+
  geom_boxplot(fill=pal[2])+
  basic_theme+
  labs(x="Hour of Day",y="Respioration Residuals")


# fit the GAMs



# model 1  year, doy, hour, and tensor interaction
model.1 <- with(arc2021EXCR.2,gam(Rates.ES ~
                              #s(hour,bs="cc",k=4) +
                              s(surf.salinity)+
                              mid.salinity+
                              s(surf.temp)+
                              s(mid.temp)+
                              s(surf.o2)+
                              s(mid.o2)+
                              s(chl.ugl)+
                              ti(surf.temp,surf.o2)+
                              ti(surf.temp,mid.o2)+
                              ti(mid.temp,mid.o2)+
                              ti(chl.ugl,mid.o2)+
                              ti(chl.ugl,surf.o2)+
                              ti(surf.salinity,surf.o2)+
                              ti(surf.salinity,mid.o2)+
                              ti(mid.salinity,surf.o2)+
                              ti(mid.salinity,mid.o2)
                            
))

# model 2
model.2 <- with(arc2021EXCR.2,gam(Rates.ED ~ 
                                  s(hour,bs="cc") +
                                  s(surf.salinity)+
                                  s(mid.salinity)+
                                  s(deep.salinity)+
                                  s(mid.temp)+
                                  s(deep.temp)+
                                  s(surf.o2)+
                                  s(mid.o2)+
                                  s(deep.o2)+
                                  s(chl.ugl)+
                                  ti(mid.salinity,mid.o2)+
                                  ti(deep.salinity,deep.o2)+
                                  ti(deep.salinity,mid.o2)
                                
))


model.3<- with(arc2021EXCR.2,gam(Rates.ES ~ 
                                    s(surf.salinity)+
                                    s(mid.salinity,k=3)+
                                    s(mid.o2,k=3)+
                                    s(surf.o2)+
                                    ti(mid.salinity,mid.o2,k=3)+
                                    ti(surf.salinity,surf.o2,k=3)
))


summary(model.1)
summary(model.2)
summary(model.3)

plotgamsarcssurf <- function (inModel,inData) {
  rates.predicted <- predict(inModel,inData)
  inData <- inData %>% mutate(residuals = Rates.ES-rates.predicted)
    sefit <- predict(inModel,inData,se.fit=TRUE)
  
    plt.1 <- ggplot()+
    geom_point(data=inData,aes(x=Timestamp.Rates,y=Rates.ES),color="light grey",size=1,shape=1)+
    geom_line(data=inData,aes(x=Timestamp.Rates,y=rates.predicted,group=factor(year(Timestamp.Rates))),color=pal[1])+
    theme_classic()+
    scale_x_datetime(date_breaks="2 weeks",date_labels = "%m/%y")+
    ggtitle("Respiration Rates EXCR Surface (2022)")
    
    plt.2 <- ggplot()+
      geom_point(data=inData,aes(x=Timestamp.Rates,y=residuals),color=pal[1],size=1.5,shape=1)+
      theme_classic()+
      scale_x_datetime(date_breaks="2 weeks",date_labels = "%m/%y")+
      ggtitle("Respiration Rates EXCR Surface (2022)")
  
  return(list(plot1=plt.1,plot.2=plt.2))
  
}

plotgamsarcsdeep <- function (inModel,inData) {
  rates.predicted <- predict(inModel,inData)
  inData <- inData %>% mutate(residuals = Rates.ED-rates.predicted)
  sefit <- predict(inModel,inData,se.fit=TRUE)
  
  plt.1 <- ggplot()+
    geom_point(data=inData,aes(x=Timestamp.Rates,y=Rates.ED),color="light grey",size=1,shape=1)+
    geom_line(data=inData,aes(x=Timestamp.Rates,y=rates.predicted,group=factor(year(Timestamp.Rates))),color=pal[1])+
    theme_classic()+
    scale_x_datetime(date_breaks="2 weeks",date_labels = "%m/%y")+
    ggtitle("Respiration Rates EXCR Deep (2022)")
  
  plt.2 <- ggplot()+
    geom_point(data=inData,aes(x=Timestamp.Rates,y=residuals),color=pal[1],size=1.5,shape=1)+
    theme_classic()+
    scale_x_datetime(date_breaks="2 weeks",date_labels = "%m/%y")+
    ggtitle("Respiration Rates EXCR Deep (2022)")
  
  return(list(plot1=plt.1,plot.2=plt.2,residuals=rates.predicted))
  
}


output.es <- plotgamsarcssurf(model.1,arc2021EXCR.2)
output.ed <- plotgamsarcsdeep(model.2,arc2021EXCR.2)
output.sal <- plotgamsarcsdeep(model.3,arc2021EXCR.2)

output.es$plot1

output.ed$plot1
output.sal$plot1
arc2021EXCR.2$residuals <- output.sal$residuals


appraise(model.3)


draw(model.3,rug=TRUE)


####------------------------------------------------------
### NEXT STEPS
####----------------------------------


### Tease out frequency in tide signal
### gam with time since low tide/tide height and hour of the day
### gam with salinity and tide
#Load 2020 and 2022 data
### Try to use 2022 data to predict 2020 data#####
####Run model to predict O2 using parameters and respiration rates
####Run 2022 data against
###Plot hour using residuals from model 3 that removes salinity and oxygen effects
