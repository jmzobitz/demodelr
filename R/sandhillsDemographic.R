#' Simulate population dynamics for sandhills cranes
#'
#' \code{sandhillsDemographic} is modified from Logan and Wolesesnky "Mathematical Methods in Biology." Create a sandhill crane model of discrete dynamics x(t+1)=r*x(t), as detailed on page 311.  The parameter r = 1+b-d, where b is the birth rate, d is the death rate. b and d are drawn from normally distributed random variables.
#'
#' @param initialPopulation Initial population size
#' @param nYears the number of years we run our population
#' @param nSimulations  Number of simulations we try
#'
#' @return A spaghetti plot and ensemble average plot of population dynamics
#' @examples
#'
#' sandhillsDemographic(100,20,10)

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'

sandhillsDemographic <- function(initialPopulation,nYears,nSimulations) {

  ### Set up vector of results
  sandhills=array(0,dim=c(nYears,nSimulations));
  sandhills[1,]=initialPopulation;  # Set the first index equal to the initial amount

  ### Loop through the each years and simulation, varying the birth and death rate
  for (j in 1:nSimulations) {
    for (i in 2:nYears) {
      b=rnorm(1,mean=0.5,sd=0.03) ### Birth rate is a random variable of mean 0.5, st dev 0.03.
      d=rnorm(1,mean=0.1,sd=0.08)  ### Death rate is a random variable of mean 0.1, st dev 0.08.

      r = 1+b-d;  # The net growth rate
      sandhills[i,j]=r*sandhills[i-1,j];	### Update current year from last years population x[t]=r*x[t]

    }
  }

  ### Now loop through and do
  quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize
  # Now loop through everything
  outCI=array(dim = c(length(quantVals),nYears));

  for (i in 1:nYears) {
    outCI[,i] = quantile(sandhills[i,],quantVals);

  }


  #### Make a plot of the solution
  ### Spaghetti plot
  total_data=data.frame(steps=0:(nYears-1),sandhills =sandhills) %>%
    gather(key=simulation,value=population,2:(nSimulations+1))


  spaghettiPlot=ggplot(total_data,aes(x=steps,y=population,group=simulation))+
    geom_line()  +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Years",y = "Sandhill Crane Population",title="Spaghetti Plot")

  print(spaghettiPlot)

  # Plot your results

  data=data.frame(years=0:(nYears-1),
                  F =outCI[2,],
                  L =outCI[1,],
                  U =outCI[3,])
  ### Ensemble plot
  ensemblePlot=ggplot(data,aes(x=years,y=F)) +
    geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
    geom_line(size=1.5) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Years",y = "Sandhills")

  print(ensemblePlot)




}





