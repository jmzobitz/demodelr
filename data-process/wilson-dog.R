library(devtools)
### Here we will estimate the values of the logistic model for the following dataset:
# http://bscheng.com/2014/05/07/modeling-logistic-growth-data-in-r/
mass<-c(6.25,10,20,23,26,27.6,29.8,31.6,37.2,41.2,48.7,54,54,63,66,72,72.2,
        76,75) #Wilson’s mass in pounds
days<-c(31,62,93,99,107,113,121,127,148,161,180,214,221,307,
        452,482,923, 955,1308) #days since Wilson’s birth
wilson<-data.frame(days,mass) #create the data frame

use_data(wilson,overwrite = TRUE)
