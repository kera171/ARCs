library(lubridate)
library(tidyverse)
library(lunar)

load("~/Rdata/ARCs/R data/arc2022wlis.Rdata")
#Create the date intervals from 1991 to 2021. 
dates <- seq(as.Date("2022-06-01"),as.Date("2022-10-31"), by="day")

#Obtain the lunar phase for each date #gives the 8 lunar phases a EST which is the shift, using the dates chosen. The phase is given in radians, but you can add name to the function to get the name of the phase instead like full or quarter etc.
phase <- lunar.phase(dates, shift = -4) 
phase.name <- lunar.phase(dates, shift = -4, name =8) 
#Change the dates and phases to data frames to bind them together
dates <- data.frame(dates)
phase <- data.frame(phase)

#make column in the phase df that is the absolute value of the cosign of the phase. We will use the cosign so that we can get a cyclical variable that cycles between spring and neap tides to use in the model
phase$phase.abs.cos <- abs(cos(phase$phase))
phase$phase.cos <- cos(phase$phase)


#join dates and respective lunar phases into one data frame
lunartidalcycles <- cbind(dates,phase,phase.name)

#plot cosign of lunar phase to see what it looks like
ggplot(head(lunartidalcycles,100), aes(x=dates,y=phase.cos))+
  geom_line()

ggplot(head(lunartidalcycles,100), aes(x=dates,y=phase.abs.cos))+
  geom_line()

#get the lagged difference for the cosign values. This will split the data into negative difference values going into a neap cycle and positive 
lunartidalcycles$diff.abs.cos<- c(0, diff(lunartidalcycles$phase.abs.cos))

lunartidalcycles$diff.cos<- c(0, diff(lunartidalcycles$phase.cos))


# filter for phases that are spring and neap only
springneapcycles <- lunartidalcycles 

# Now I will make a column that labels each phase spring or neap using the abs cosign value of the phase and the phase name. 
springneapcycles$cycle <- NA
springneapcycles$time.st <- NA
for (i in 2:nrow(springneapcycles)) {
  if (springneapcycles$phase.name[i] %in% c("New","Full")& springneapcycles$diff.abs.cos[i] >= 0 ) {
    springneapcycles$cycle[i] <- "Spring"
  } else if (springneapcycles$phase.name[i] %in% c("First quarter","Last quarter")& springneapcycles$diff.abs.cos[i] < 0 ) {
    springneapcycles$cycle[i] <- "Neap"
  }
  
  if (springneapcycles$diff.abs.cos[i]<0 & springneapcycles$diff.abs.cos[i-1]>0) {
    springneapcycles$time.st[i] <- 1
    springneapcycles$time.st[i-1] <- 0
  } else {
    springneapcycles$time.st[i] <- springneapcycles$time.st[i-1]+1
  }
} 

#Time to add spring/neap data to profile data in order to calculate time since spring and neap tide. I make a date column in CTDEEP_profile to match date column in springneapcycles then join the two data frames. 

arc2022WLIS$dates <- as.Date.POSIXct(arc2022WLIS$Time_R,format("%Y-%m-%d"))


arc2022wlis.tides <- left_join(arc2022WLIS, springneapcycles, by ="dates")

arc2022wlis.tides <- rename(arc2022wlis.tides, "deep.rates" = "wlis deep rates")

arc2022wlis.tides <- rename(arc2022wlis.tides, "chla" = "chlorophyll _ugL")

arc2022wlis.tides <- rename(arc2022wlis.tides, "deep.salinity" = "deep salinity")

arc2022wlis.tides$deep.rates <- as.numeric(arc2022wlis.tides$deep.rates)

arc2022wlis.tides$deep.rates <- arc2022wlis.tides$deep.rates * -1


# plot spring neap phases and rates

arc2022wlis.tides %>% 
  ggplot()+
  geom_point(aes(x=dates,y=deep.rates))+
  geom_point(aes(x=dates,y=phase))

arc2022wlis.tides %>% 
  ggplot()+
  geom_point(aes(x=dates,y=deep.rates))+
  geom_point(aes(x=dates,y=time.st))



arc2022wlis.tides %>% 
  ggplot()+
  geom_point(aes(x=phase,y=deep.rates))


arc2022wlis.tides %>% 
  ggplot()+
  geom_point(aes(x=time.st,y=deep.rates))

arc2022wlis.tides %>% 
  ggplot()+
  geom_point(aes(x=chla,y=deep.rates))

arc2022wlis.tides %>% 
  ggplot()+
  geom_point(aes(x=dates,y=deep.rates*30))+
  geom_line(aes(x=dates,y=chla))

arc2022wlis.tides %>% 
  ggplot()+
  geom_point(aes(x=dates,y=deep.rates*30))+
  geom_line(aes(x=dates,y=chla))

arc2022EXCR.tides %>% 
  ggplot()+
  geom_point(aes(x=dates,y=wind.direction))


  #Added spring neap data to CTDEEP_complete and saved it to mae it permanent.
  #save(CTDEEP_complete,file = "data/CTDEEP_complete.Rdata")