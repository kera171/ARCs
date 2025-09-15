library(tidyverse)
library(readxl)
library(writexl)


excr.met.22<- read_excel("Raw data/met data 2022.xlsx")

excr.22.six_hour_avg <- excr.met.22 %>%
  mutate(period = floor_date(TIMESTAMP, "6 hours")) %>%  # Group into 6-hour periods
  group_by(period) %>%
  summarise(avg_sal.surf = mean(`surf psu`, na.rm = TRUE),
            avg_sal.mid = mean(`mid psu`, na.rm = TRUE),
            avg_sal.deep = mean(`deep psu`, na.rm = TRUE),
            avg_o2.surf = mean(`surf mg/L`, na.rm = TRUE),
            avg_02.mid = mean(`mid mg/L`, na.rm = TRUE),
            avg_02.deep = mean(`deep mg/L`, na.rm = TRUE),
            avg_temp.surf = mean(`surf degC`, na.rm = TRUE),
            avg_temp.mid = mean(`mid degC`, na.rm = TRUE),
            avg_temp.deep = mean(`deep degC`, na.rm = TRUE),
            avg_chl = mean(`chl_ug/L`, na.rm = TRUE))  # Calculate the average

write_xlsx(excr.22.six_hour_avg, path = "saved files/excr 2022 averaged met data.xlsx" )