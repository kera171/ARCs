#ARC Processing Functions 10/7/2024

# assigning incubation numbers to data from Hobo-----------------------------------

assign.incubation <- function(mydata){
  df <- mydata
  incubation <- 0
  df$incubation.number <- NA
  for(i in 1:nrow(df)){
    if(df$status[i]=="flushing"){
      incubation <- incubation+1
      df$incubation.number[i] <- NA
    } else {
      df$incubation.number[i] <- incubation
    }
  }
  return(df) }

# calculate slopes from hobo data for each chamber----------------------------------------------

calculate_slope <- function(mydata) {
  
  df <- mydata %>% 
    filter(!is.na(incubation.number))
  slopes.o2.calc <- data.frame(
    incubation.number=unique(df$incubation.number))
  slopes.o2.calc$slope <- NA
  slopes.o2.calc$rsq <- NA
  slopes.o2.calc$n <- NA
  slopes.o2.calc$datetime <- as.POSIXct("1899-01-01 04:58:59",tz="America/New_York")
  slopes.o2.calc$incubation.duration <- NA
  slopes.o2.calc$slope.se <- NA
  for (i in 1:nrow(slopes.o2.calc)){
    df.segment <- df %>% 
      filter(incubation.number==slopes.o2.calc$incubation.number[i]) 
    df.segment <- df.segment %>%  
      mutate(elapsed.time=as.numeric(datetime-df.segment$datetime[1])/3600)
    slopes.o2.calc$incubation.duration[i] <- df.segment$elapsed.time[nrow(df.segment)]
    fit <- lm(do.mgl~elapsed.time, data = df.segment)
    tmp <- broom::tidy(fit)
    slopes.o2.calc$slope[i] <- fit$coefficients[2]
    slopes.o2.calc$slope.se[i] <- tmp$std.error[2]
    slopes.o2.calc$rsq[i] <- summary(fit)$r.squared
    slopes.o2.calc$n[i] <- nrow(df.segment)
    slopes.o2.calc$datetime[i] <- df.segment$datetime[1]
  }
  slopes.o2.calc <- slopes.o2.calc %>% 
    mutate(rate=slope*1000/32,
           rate.se=slope.se*1000/32,
           rate.cv=abs(100*rate.se/rate))
  
  return(slopes.o2.calc)
}

# graphing figure for rates--------------------------------------------
graph.figures.rates <-  function(mydata){
  
  incubations <- unique(mydata$incubation.number[!is.na(mydata$incubation.number)])
  
  pdf(file = paste0("Figures/Figures(EXCR-S Fouling Test 1 ", format(mydata$datetime[1],format = "%Y-%m-%d" ),").pdf"), 
      width = 6, 
      height = 6,
      paper = "letter")
  
  for ( i in 1:length(incubations)){
    df.segment <- mydata %>% 
      filter(incubation.number==incubations[i])
    plt <- ggplot( df.segment, aes(x=datetime,y=do.mgl))+
      geom_point()+
      geom_smooth(method = "lm")+
      theme_classic()+
      scale_x_datetime(date_breaks = "30 min",date_labels = "%H:%M")+
      ylab("Dissolved Oygen (mg/L)")+
      xlab("")+
      ggtitle(paste("Incubation Number",incubations[i],"--",
                    format(df.segment$datetime[1],format="%Y-%m-%d %H:%M")))
    print(plt)
  }
  
  dev.off()
}

