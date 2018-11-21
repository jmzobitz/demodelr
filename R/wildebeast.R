#' Simulate wildebeast population dynamics
#'
#' \code{wildebeast} is modified from Logan and Wolesesnky "Mathematical Methods in Biology". Simluate a wildebeast population undergoing harvesting with stochastic environmental effects. due to rainfall. This corresponds to Exercise 8 on page 318
#'
#' @param harvestRate The harvesting rate of the wildebeast.
#' @param nYears Number of years we run the population
#' @param nSimulations  Number of simulations we try
#'
#'
#' @return A spaghetti plot and ensemble average plot of population dynamics
#' @examples
#'
#' wildebeast(.5,5,2)

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
#'
#'
wildebeast <- function(harvestRate,nYears,nSimulations) {

### Fixed parameters for the simulation
initialWildebeast=250000;	### Initial number of wildebeasts
r=1.1323;	### The growth rate
rainfall= c(100, 36, 100, 104, 167, 107, 165, 71, 91, 77, 134, 192, 235, 159, 211, 257, 204, 300, 187, 84, 99, 163, 97, 228, 208)  ### Rainfall rates for a 34 year period




### Set up the results
wildebeastResults=array(0,dim=c(nYears,nSimulations));
wildebeastResults[1,]=initialWildebeast;
for (j in 1:nSimulations) {
  for (i in 2:nYears) {
    currRainfall = sample(rainfall,1);
    carryingCapacity = 20748*currRainfall
    wildebeastResults[i,j]=((1-harvestRate)*r*carryingCapacity*wildebeastResults[i-1,j])/(carryingCapacity+(r-1)*wildebeastResults[i-1,j]);

  }
}

### Now loop through and make an ensemble plot of the results
quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize
# Now loop through everything
outCI=array(dim = c(length(quantVals),nYears));

for (i in 1:nYears) {
  outCI[,i] = quantile(wildebeastResults[i,],quantVals);

}


### Now make a plot
total_data=data.frame(steps=1:nYears,wildebeast =wildebeastResults) %>%
  gather(key=simulation,value=population,2:(nSimulations+1))

### Spaghetti plot
spaghettiPlot=ggplot(total_data,aes(x=steps,y=population,group=simulation)) +
  geom_line() + geom_abline(intercept = 150000,slope=0,color='blue',size=1.5) +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Years",y = "Population",title="Spaghetti Plot")


print(spaghettiPlot)


### Ensemble plot
data=data.frame(steps=1:nYears,
                F =outCI[2,],
                L =outCI[1,],
                U =outCI[3,])


ensemblePlot=ggplot(data,aes(x=steps,y=F)) +
  geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
  geom_line(size=1.5) + geom_abline(intercept = 150000,slope=0,color='blue',size=1.5) +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Years",y = "Population",title="Ensemble Plot")

print(ensemblePlot)

}







