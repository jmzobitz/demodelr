#' Simulate population dynamics for sandhills cranes
#'
#' \code{sandhillsEnvironmental} is modified from Logan and Wolesesnky "Mathematical Methods in Biology." Create a sandhill crane model of discrete dynamics x(t+1)=r*x(t), as detailed on page 311.  The parameter r = 1+b-d, where b is the birth rate, d is the death rate. We modify r based on the stated flood rate potential. In normal years, the value of r is 1.4. In Catastrophic years the net growth rate is lowered so r=0.575.
#'
#' @param initialPopulation Initial population size
#' @param floodRate The frequency of flooding. (1/25 = one in 25 years)
#' @param nYears the number of years we run our population
#' @param nSimulations  Number of simulations we try
#'
#' @return A spaghetti plot and ensemble average plot of population dynamics
#' @examples
#'
#' sandhillsEnvironmental(100,1/25,20,10)

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'

sandhillsEnvironmental <- function(initialPopulation,floodRate,nYears,nSimulations) {

  ### Set up vector of results
  sandhills=array(0,dim=c(nYears,nSimulations));
  sandhills[1,]=initialPopulation;  # Set the first index equal to the initial amount

  ### Loop through the each years and simulation, varying the birth and death rate
  for (j in 1:nSimulations) {
    for (i in 2:nYears) {
      if (runif(1)< floodRate) {r=0.575}   # Catastrophic years the net growth rate is lowered
      else {r=1.4}   # Normal years the net growth rate is 1.4
      sandhills[i,j]=r*sandhills[i-1,j];  ### Update current year from last years population x[t]=r*x[t], r = 1-b-d


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
  # Plot your results


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


  data=data.frame(years=0:(nYears-1),
                  F =outCI[2,],
                  L =outCI[1,],
                  U =outCI[3,])

  ensemblePlot=ggplot(data,aes(x=years,y=F)) +
    geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
    geom_line(size=1.5)  +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Years",y = "Sandhill Crane Population",title="Ensemble Average Plot")

  print(ensemblePlot)


}





