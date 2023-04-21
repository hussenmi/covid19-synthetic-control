install.packages('readr')
install.packages('dplyr')
install.packages('tidyr')
# install.packages('splusTimeDate')
install.packages('stringr')
install.packages('doBy')
install.packages('bit64') # ADDED
install.packages('gsynth') # ADDED
install.packages('xtable') # ADDED
install.packages('panelView') # ADDED

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
saveRDS(data,'social_distance_dataset.rds') # CHANGE THIS


#Implements Generalized Synthetic Control by Xu (2017)
#The R implementation can be found here: https://yiqingxu.org/software/gsynth/gsynth_examples.html.
#We follow the code in the examples by Xu and Liu (Mar 6, 2020) to implement the model. 


rm(list = ls())
# data=readRDS('social_distance_dataset.rds')
data=readRDS('/Users/Hussen/OneDrive/Desktop/Year 3/Semester 2/CS130/Assignments/Final Project/v1_apr13data.rds')
data=subset(data,!province=='Alaska')
data=subset(data,!province=='American Samoa')


# imports libraries 
library(gsynth)
library(panelView)

#makes it R data frame
data=data.frame(data)

#visualizes treatment status
panelview(gr_case~D,data =data, index = c("province","Date"),ylab="State",pre.post = TRUE,
          theme.bw=TRUE,axis.adjust=TRUE) # CHANGED CAPITAL V TO LOWERCASE v IN FUNCTION NAME



#two way fixed effect + parametric
set.seed(123)
result1=gsynth(gr_case~D+fd, data=data,index= c("province","Date"), 
               force = "two-way",CV = TRUE,r=c(1, 5), se=TRUE, 
               inference = "parametric", nboots= 1000, parallel= FALSE)

result1$est.avg
result1

#plot
plot(result1, type = "counterfactual",axis.adjust=TRUE,
     raw = "none", main="", theme.bw=TRUE)

plot(result1, theme.bw=TRUE)


#unit fixed effect + parametric
set.seed(1234)
result2=gsynth(gr_case~D+fd, data=data,index= c("province","Date"), 
               force = "unit",CV = TRUE,r=c(1, 5), se=TRUE, 
               inference = "parametric", nboots= 1000, parallel= FALSE)

result2$est.avg 
result2

result2$Ntr
result2$Nco

#plot
plot(result2, type = "counterfactual",axis.adjust=TRUE,
     raw = "none", main="")


#time fixed effect + parametric
set.seed(1235)
result3=gsynth(gr_case~D+fd, data=data,index= c("province","Date"), 
               force = "time",CV = TRUE,r=c(1, 5), se=TRUE, 
               inference = "parametric", nboots= 1000, parallel= FALSE)

result3$est.avg
result3

#plot
plot(result3, type = "counterfactual",axis.adjust=TRUE,
     raw = "none", main="")


# Specify the path to the extracted Synth tar.gz file
path_to_package <- "Synth"
# Install the Synth package from the tar.gz file
install.packages("Synth", repos = NULL, type = "source")

install.packages("gsynth")
library(gsynth)

#-----------------------------------------SCM------------------------------------
treated_unit <- "Montana"

# alter treatment date to 16th March (when social distancing began in Montana)
data_filtered <- data %>%
  filter(province %in% c(treated_unit, control)) %>%
  mutate(treat = ifelse(province == treated_unit, 1, 0),
         timedum = ifelse(Date >= "2020-03-16", 1, 0),
         D = treat * timedum)

# run gsynth with single treated unit (Montana)
actual_treatment_effect <- gsynth(
  gr_case ~ D + fd, 
  data = data_filtered,
  index = c("province", "Date"),
  force = "two-way",
  CV = TRUE,
  r = c(1, 5),
  se = TRUE,
  inference = "parametric",
  nboots = 1000,
  parallel = FALSE
)

actual_treatment_effect$est.avg
actual_treatment_effect

plot(actual_treatment_effect, type = "counterfactual",axis.adjust=TRUE,
     raw = "none", main="", theme.bw=TRUE)

# jpeg(file="Montana ATT.jpeg")
plot(actual_treatment_effect, theme.bw=TRUE, main="ATT for Montana")
# dev.off()

plot(result, type = "counterfactual",axis.adjust=TRUE,
     raw = "none", main="Synthetic Control for Montana State", ylab="Growth rate of Covid-19 infection")



#---------------------------------------PLACEBO IN SPACE------------------------------

# create a function to run gsynth and return the treatment effect estimate
run_gsynth <- function(treated_unit, control, data) {
  data_filtered <- data %>%
    filter(province %in% c(treated_unit, control)) %>%
    mutate(treat = ifelse(province == treated_unit, 1, 0),
           timedum = ifelse(Date >= "2020-03-16", 1, 0),
           D = treat * timedum)
  
  result <- gsynth(
    gr_case ~ D + fd, 
    data = data_filtered,
    index = c("province", "Date"),
    force = "two-way",
    CV = TRUE,
    r = c(1, 5),
    se = TRUE,
    inference = "parametric",
    nboots = 1000,
    parallel = FALSE
  )
  
  return(result$est.avg)
}


# initialize a vector to store the placebo treatment effect estimates
placebo_treatment_effects <- c()

# perform placebo tests in space for each control state
for (placebo_treated_unit in control) {
  placebo_control_group <- control[control != placebo_treated_unit]
  placebo_treatment_effect <- run_gsynth(placebo_treated_unit, placebo_control_group, data)
  placebo_treatment_effects <- c(placebo_treatment_effects, placebo_treatment_effect)
  
  cat("Placebo treatment effect for", placebo_treated_unit, ":", placebo_treatment_effect, "\n")
}

# compare the actual treatment effect with the placebo treatment effects
cat("Actual treatment effect for Montana:", actual_treatment_effect, "\n")

# plot the treatment effects
plot_data <- data.frame(
  Unit = c(treated_unit, control),
  TreatmentEffect = c(actual_treatment_effect, placebo_treatment_effects)
)

ggplot(plot_data, aes(x = Unit, y = TreatmentEffect)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Unit", y = "Treatment Effect") +
  ggtitle("Placebo Treatment Effects in Space")

library(ggplot2)
# plot the average treatment effects (like a p-value)
jpeg(file="placebo_att3.jpeg")
ggplot(data = data.frame(RMSPE = placebo_space_att), aes(RMSPE)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept = -3.693, color = "red") +
  labs(title = "Distribution of RMSPE for Montana and control states")
dev.off()

# add a vertical line at x = 20
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram() +
  geom_vline(xintercept = 20)

# -------------------------------------PLACEBO IN TIME---------------------------------
# times to test for
post_treatment_dates <- seq(as.Date("2020-03-01"), as.Date("2020-03-16"), by = "day")
post_treatment_placebo_effects <- numeric()

for (placebo_date in post_treatment_dates) {
  data_filtered <- data %>%
    filter(province %in% c(treated_unit, control)) %>%
    mutate(treat = ifelse(province == treated_unit, 1, 0),
           timedum = ifelse(Date >= placebo_date, 1, 0),
           D = treat * timedum)
  
  result <- gsynth(
    gr_case ~ D + fd, 
    data = data_filtered,
    index = c("province", "Date"),
    force = "two-way",
    CV = TRUE,
    r = c(1, 5),
    se = TRUE,
    inference = "parametric",
    nboots = 1000,
    parallel = FALSE
  )
  
  post_treatment_placebo_effects <- c(post_treatment_placebo_effects, result$est.avg)
}

# create a data frame for the placebo test in time
placebo_time_data <- data.frame(
  Date = post_treatment_dates,
  PlaceboEffect = post_treatment_placebo_effects
)

# plot the placebo treatment effects in time
jpeg(file="placebo_att7.jpeg")
ggplot(placebo_time_data, aes(x = Date, y = PlaceboEffect)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Date", y = "Placebo Treatment Effect") +
  ggtitle("Placebo Treatment Effects in Time")
dev.off()

jpeg(file="placebo_att6.jpeg")
ggplot(data = data.frame(RMSPE = pre_treatment_placebo), aes(RMSPE)) +
  geom_histogram(bins=15) +
  geom_vline(xintercept = -3.693, color = "red") +
  labs(title = "Distribution of RMSPE for March 16 and earlier")
dev.off()
