

## Read tide data from NOAA

library(tidyverse)
library(lubridate)




getTideData <- function(beginDate,endDate,product) {

url <- paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=",product,"&application=NOS.COOPS.TAC.WL&begin_date=",beginDate,"&end_date=",endDate,"&datum=MLLW&station=8454049&time_zone=GMT&units=metric&format=csv")
tides.times <- read.csv(url,skip=1) %>% .[,1:3]
names(tides.times) <- c("TimeStamp.UTC","height.m","tide")
tides.times <- tides.times %>% 
  mutate(TimeStamp.UTC=as.POSIXct(TimeStamp.UTC,format="%Y-%m-%d %H:%M",tz="UTC"), 
         tide=trimws(tide))
return(tides.times)
}

tide.times <- rbind(
  getTideData(beginDate="20200101",endDate="20201231",product="high_low"),
  getTideData(beginDate="20210101",endDate="20211231",product="high_low")
) %>% mutate(NOAA=TRUE)

times <- data.frame(TimeStamp.UTC=seq(as.POSIXct("2020-01-01 00:00",format="%Y-%m-%d %H:%M",tz="UTC"),
             as.POSIXct("2021-12-31 23:45",format="%Y-%m-%d %H:%M",tz="UTC"),
             by=15*60),height.m=NA,tide=NA,NOAA=FALSE)

tide.times <- rbind(tide.times,times)
tide.times <- tide.times[order(tide.times$TimeStamp.UTC),]

lt.time <- NA
tide.times$T.lt <- NA
for (i in 1:nrow(tide.times)) {
  if (tide.times$tide[i] %in% c("L","LL")) {
    # you're at low tide
    tide.times$T.lt[i] <- 0
    lt.time <- as.numeric(tide.times$TimeStamp.UTC[i])
  } else {
    # it's not lot tide
    tide.times$T.lt[i] <- (as.numeric(tide.times$TimeStamp.UTC[i])-lt.time)/3600  
  }
  # end of the for loop
}

tide.times <- tide.times[!tide.times$NOAA,]

save(tide.times,file="data/tideTimes.Rdata")


  
# For later ... get tide heights for all these 1 month periods
beginDates <- seq(as.Date("2021-01-01"),as.Date("2023-01-01"),by=30)
endDates <- beginDates+29
