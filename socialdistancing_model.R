#Implments Generalized Synthetic Control by Xu (2017)
#The R implementation can be found here: https://yiqingxu.org/software/gsynth/gsynth_examples.html.
#We follow the code in the examples by Xu and Liu (Mar 6, 2020) to implement the model. 


rm(list = ls())
data=readRDS('/Users/Hussen/OneDrive/Desktop/Year 3/Semester 2/CS130/Assignments/Final Project/v1_apr13data.rds')
data=subset(data,!province=='Alaska')
data=subset(data,!province=='American Samoa')


# imports libraries 
library(gsynth)
library(panelView)

# # I just added this to see if it fixes the 'panelView' problem
# library(lattice)

#makes it R data frame
data=data.frame(data)

library(dplyr)

# create a new variable for the id of a province in the panel data. if multiple rows have the represent the same province, they will have the same id

data <- data %>%
    # arrange the data in growing order for column you want to build sequential group ID from/for
    dplyr::arrange(province) %>%
    # build the groupings
    dplyr::group_by(province) %>%
    # add new column : sequenctial group id
    dplyr::mutate(id = dplyr::cur_group_id()) %>%
    # always ungroup to prevent unwanted behaviour down stream
    dplyr::ungroup()




head(data)

treated.ids <- data[data$treat == 1,]$id
control.ids <- data[data$treat == 0,]$id

# prepare the data for Synthetic control
dataprep.out <-
    dataprep(foo = data,
             predictors = c("cases_daily.x" , "death_cases_daily" , "death_rate" ,
                            "Population.sum") ,
             predictors.op = "mean" ,
             time.predictors.prior = as.Date("2020-01-21"):as.Date("2020-03-26"),
          #    special.predictors = list(
          #        list("co2_transport_capita" , 1989 , "mean"),
          #        list("co2_transport_capita" , 1980 , "mean"),
          #        list("co2_transport_capita" , 1970 , "mean")
          #    ),
             dependent = "gr_case",
             unit.variable = "id",
             unit.names.variable = "province",
             time.variable = "Date",
             treatment.identifier = 4, #data[data$province == 'California',]$id[1],
             controls.identifier = control.ids,
          #    time.optimize.ssr = 1960:1989,
             time.plot = min(data$Date):max(data$Date)
    )

#visualizes treatment status
panelview(gr_case~D,data =data, index = c("province","Date"),ylab="State",pre.post = TRUE,
          theme.bw=TRUE,axis.adjust=TRUE) 



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
