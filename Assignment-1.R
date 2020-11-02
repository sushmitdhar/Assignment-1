# Global Average Temperature
rm(list=ls())

library(tidyverse)
library(lubridate)
library(zoo)

generic_url <- function(url, location) {
  return(read_table2(url) %>%
           .[1:which(.$Year %in% "Year")-1, ] %>%
           .[ , 1:3] %>% 
           mutate(Date = ymd(paste(.$Year, .$Mo, 1, sep="-"))) %>% 
           select(Date, Globe) %>% 
           mutate_if(is.character, ~as.numeric(.)) %>%
           mutate(Globe=rollmean(Globe, 12, fill=NA, align="right"),
                  Location=paste0(location))) 
}


lotp <- generic_url("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", "Lower-Troposphere")
mitp <- generic_url("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt", "Mid-Troposphere")
trps <- generic_url("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt", "Tropopause")
lstp <- generic_url("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt", "Lower-Stratosphere")

head(lstp, 15)


Averagedf <- data.frame(Date=lotp$Date,Average=(lotp$Globe + mitp$Globe + trps$Globe + lstp$Globe)/4)
longdf <- bind_rows(lotp, mitp, trps, lstp)
longdf <- longdf %>% inner_join(Averagedf, by="Date")
head(longdf,20)
# Plot
longdf %>%
  mutate(Year=year(Date)) %>% 
  filter(Year >= 1980) %>%
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line(aes(group=Location, color=Location))+geom_line(aes(y=Average, color="Average"))+scale_color_discrete(name="Location and Their Average")+
  ylab(expression("Temperature ("*~degree*C*")")) +
  xlab("Year") +
  labs(title = "Global Temperature Deviation and their average",
       subtitle = "Temperature departure from 1980-2020 average",
       caption = "Series are 12-month moving averages, right aligned.") +
  theme_linedraw()

rm(list=ls())