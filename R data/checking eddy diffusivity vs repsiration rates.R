
library(lubridate)
library(tidyverse)
library(readxl)

eddy.data <- read_excel("~/Rdata/ARCs/Raw data/Respirationvs eddy.xlsx")
eddy.data <- rename(eddy.data,eddy = "6.04202520446895")
eddy.data <- rename(eddy.data,respiration = "0.113179074")


# Quantiles for all the data

quantile.eddy <- quantile(eddy.data$eddy,
                         probs = c(0.001,seq(0.01,0.99, by=0.01)))

quantile.respiration <- quantile(eddy.data$respiration,
                          probs = c(0.001,seq(0.01,0.99, by=0.01)))

# Put the quantiles together for graphing
df.rate <- rbind(
  data.frame(respiration=quantile.respiration,eddy=quantile.eddy,
             percentile=c(0.1,seq(1,99)))
)

qqplot.all <- ggplot()+
  geom_line(data=df.rate,aes(x=eddy,y=respiration),lwd = 0.75)+
  geom_point(data=df.rate %>% filter(percentile %in% c(0.1,1,seq(5,95,by=5),99)),
             aes(x=eddy,y=respiration),size=5,alpha=0.5)+
  geom_abline(slope = 1, intercept = 0, lwd=0.75, 
              linetype = 2)+
  coord_equal(xlim = c(0,7),
              ylim = c(0,7))+
  theme_classic()+
  xlab("Respiration Rates")+
  ylab("Eddy diffusivity")+
  theme(legend.position=c(0.7,0.2),
        legend.title=element_blank(),
        legend.text = element_text(size=14))

qqplot.all
