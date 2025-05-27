---
  title: ""
author: "Anna Mudahy"
date: "2024-10-06"
output: html_document
editor_options: 
  chunk_output_type: console
---
---- {packages}
library(tidyverse)
library(lubridate)
library(readxl)
library(broom)
library(here)

#source("ARC Processing Functions.R")

----- #{load files}
#put in path for file that need to be read
objectname <- read_excel(here("Raw data","file.xlsx"),sheet = "") %>% 
  mutate(datetime.2= as.POSIXct(paste(datetime),tz="America/New_York")) %>% 
  select(-datetime) %>% 
  rename(datetime=datetime.2)

----#load multiple excel files 
#define path for files needed that have similar pattern in name
replacewithnameyouwant <- dir(path = here("Raw data/"),pattern = "cleaned")
#function to read all file names selected
read.rates.files <- function(my.file){
  print(my.file)
  title <- my.file
  df <- read_xlsx(here("Raw data/",my.file),sheet = "cleaned", skip = 1) %>% 
    mutate(datetime.2 = as.POSIXct.default(datetime),
           deployment = gsub(" cleaned[.]xlsx","",title) ) %>% 
    select(-datetime) %>% 
    rename(datetime=datetime.2)
  
  return(df)
}

# gives rate files as separate files
for(i in 1:length(replacewithnameyouwant)){
  title <- replacewithnameyouwant[i]
  title.fixed <- gsub(" cleaned[.]xlsx","",title)
  df <-  read.rates.files(replacewithnameyouwant[i]) %>% 
    rename(status=start.point)
  assign(title.fixed,df)
  rm(df)
}

#gonna label all NA's as incubating in start point column and rename column to status

----#{r fixing start point col}
objectname<- objectname %>%
  replace_na(.,list(start.point="i")) %>% 
  mutate(status=ifelse(start.point=="s","flushing","incubating"))


----#{r functions to make incubation number and to calculate slope}

objectname <- assign.incubation(objectname)

objectname.rates <- calculate_slope(objectname)

----#{r function to graph incubations}


graph.figures.rates <-  function(mydata){

incubations <- unique(mydata$incubation.number[!is.na(mydata$incubation.number)])

pdf(file = here("R data/Figures", paste0("Figures(deployment/s title ", format(mydata$datetime[1],format = "%Y-%m-%d" ),").pdf")), 
    width = 7, 
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

graph.figures.rates(objectname)

----#{r save file with calculated rates }
filenameforsaving <- objectname.rates

#save(filenameforsaving, file = here("R data", "filenameforsaving.Rdata" ))

----#write files with rates calculated to excel
filename <- ls(pattern = "") #gets list of r objects that contain the rate calculations for each deployment
#saves each as xl file
write_xlsx(filenameforsaving, here("saved files", paste0(filename[7],".xlsx")), col_names = TRUE)
