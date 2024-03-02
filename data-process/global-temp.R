library(devtools)
library(tidyverse)
### Here we will incorporate average global temperature anomaly
# https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt
global_temperature <- read_csv('data-process/temperature.csv') %>%
  select(2,3) %>%
  rename(year_since_1880 = 1, temperature_anomaly=2)

use_data(global_temperature,overwrite = TRUE)
