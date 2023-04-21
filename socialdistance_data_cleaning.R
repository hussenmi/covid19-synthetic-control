# Using the following coding procedure, the data is accessible and can be cleaned and formatted from Johns Hopkins University Center for Systems Science and Engineering, or can be drictly loaded using the data file provided named "v1_apr13data.rds".

#** We follow preliminary lines of the R (open source language and environment) code posted by Tim Churches (Mar 05, 2010) in a blog (https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/) to extract the data from JHU and put it into a panel format

##Pulling and cleaning and formatting data

rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)
# library(splusTimeDate)
library(stringr)
library(doBy) 


web=paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", sep = "")

us_confirm=read_csv(web) %>% rename(province = "Province_State", country_region = "Country_Region") %>% 
  select(-c('UID','iso2','iso3','code3','Admin2','Combined_Key'))%>%
  pivot_longer(-c(province, country_region,FIPS, Lat, Long_), names_to = "date", values_to = "cases_cum")%>% 
  mutate(date=as.Date(date,"%m/%d/%y")) %>%
  filter(country_region == "US")%>% 
  arrange(province, date)%>% as.data.frame()


us_confirm=summaryBy(cases_cum~province+date, data = us_confirm, FUN=sum) 
us_confirm=us_confirm%>%mutate(Date =date - 1) %>%arrange(province, Date)%>%group_by(province)%>% mutate(cases_daily = c(0, diff(cases_cum.sum)))%>% 
  ungroup() %>% 
  filter(str_detect(province, "Grand Princess", negate = TRUE))




web2=paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv",sep = "")

us_death=read_csv(web2) %>% rename(province = "Province_State", country_region = "Country_Region") %>% 
  select(-c('UID','iso2','iso3','code3','Admin2','Combined_Key'))%>%
  pivot_longer(-c(province, country_region,Population,FIPS, Lat, Long_), names_to = "date", values_to = "death_cases_cum")%>% 
  mutate(date=as.Date(date,"%m/%d/%y")) %>%
  filter(country_region == "US")%>% 
  arrange(province, date)%>% as.data.frame()


us_death=summaryBy(death_cases_cum+Population~province+date, data = us_death, FUN=sum) 
us_death=us_death%>%mutate(Date =date - 1) %>%arrange(province, Date)%>%group_by(province)%>% mutate(death_cases_daily = c(0, diff(death_cases_cum.sum)))%>% 
  ungroup() %>% 
  filter(str_detect(province, "Grand Princess", negate = TRUE))


us_data=merge(us_confirm, us_death, by=c("province","Date"))

#creates a dataset with the first case date for each state
first_day_case=us_data %>% 
  group_by(province) %>% 
  # which(cases_daily>0)[1] returns the position of the first element of death_case_daily>0
  slice(which(cases_daily>0)[1]) %>%
  select(c('province','Date','cases_daily'))

#plots the dates 
library(ggplot2)
ggplot(first_day_case, aes(y= Date, x=province)) + 
  geom_point()+ 
  theme_bw()+
  scale_y_date(breaks = scales::pretty_breaks(n=20))+
  xlab(" ")


#first case day dummy for each state
us_data_filter=us_data%>% 
  merge(first_day_case, by=c("province","Date"),all.x = TRUE)%>%
  mutate(fd=ifelse(is.na(cases_daily.y),0,1))%>%
  select(c("province","Date","cases_cum.sum", "cases_daily.x","death_cases_cum.sum","Population.sum","death_cases_daily","fd"))


#creates a list of treatment and control states based on the map
states=c(unique(us_data_filter$province))
states=states[!(states=="American Samoa"|states=="Alaska")]

control=c("Idaho","Wyoming","Utah","Arizona","New Mexico","Oklahoma","Arkansas",
          "Alabama","Louisiana","Mississippi","South Carolina","Tennessee","Virginia",
          "West Virginia","Kentucky","Kansas","North Dakota","South Dakota","Nebraska",
          "Missouri","Iowa","Illinois","Wisconsin","Indiana","North Carolina",
          "Florida","Maryland","Pennsylvania","Vermont","Texas")

treat=states[!(states %in% control)]


us_data_filter=us_data_filter %>%
  mutate(
    inf_rate= (cases_daily.x/Population.sum)*10000,
    death_rate=(death_cases_daily/Population.sum)*10000,
    treat=ifelse(province %in% treat, 1,0),
    timedum=ifelse(Date>="2020-03-26",1,0))

us_data_filter=us_data_filter %>%
  group_by(province)%>%
  mutate(gr_case= (cases_daily.x-lag(cases_daily.x))/lag(cases_daily.x),
         gr_death=(death_cases_daily-lag(death_cases_daily))/lag(death_cases_daily))


us_data_filter$gr_case[is.nan(us_data_filter$gr_case)| is.infinite(us_data_filter$gr_case)]=0
us_data_filter$gr_death[is.nan(us_data_filter$gr_death)| is.infinite(us_data_filter$gr_death)]=0

us_data_filter$gr_case[is.na(us_data_filter$gr_case)]=0
us_data_filter$gr_death[is.na(us_data_filter$gr_death)]=0

us_data_filter$D=us_data_filter$treat*us_data_filter$timedum


us_data_filter=data.frame(us_data_filter)


data=subset(us_data_filter, Date <= "2020-04-13")

# Replace the dots with the actual file path
# saveRDS(data,'/path/to/v1_apr13data.rds')
saveRDS(data,'/social_distance_dataset.rds')

