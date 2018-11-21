#' Simulate a population dynamics that produce offspring
#'
#' \code{population} modified from Logan and Wolesesnky "Mathematical Methods in Biology". Simulates the dynamics of population that produces variable number of offspring generation to generation. This corresponds to Exercise 5 on page 318
#'
#' @param initialPopulation Initial population size
#' @param nGenerations the number of Generations
#' @param nSimulations  Number of simulations we try
#'
#'
#' @return A spaghetti plot and ensemble average plot of population dynamics
#' @examples
#'
#' population(10,200,10)

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'

population <- function(initialPopulation,nGenerations,nSimulations) {

  ### Set up vector of results
  population=array(0,dim=c(nGenerations,nSimulations));
  population[1,]=initialPopulation  # Set the first row of the initial population to p0.
  ### Loop through the each years and simulation, varying the birth and death rate
  for (j in 1:nSimulations) {
    for (i in 2:nGenerations) {
      r = runif(population[i-1,j])   ### Variable testing how many offspring are made
      newPopulation = array(0,dim=population[i-1,j])  ### Set up a vector of new populations, depending on

      newPopulation[which(r<0.25)]=2  # Two offspring are made if r < 0.25
      newPopulation[which((0.25 <= r & r < 0.75))]=1  # One offspring are made if 0.25 <= r < 0.75

      # else no more offspring are made (all other values of newPopulation are 0)

      population[i,j]=sum(newPopulation)   # Update the population

      if (population[i,j]==0) break   # If you go extinct, then exit the loop
    }
  }


  ### Now loop through and make an ensemble plot of the results
  quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize
  # Now loop through everything
  outCI=array(dim = c(length(quantVals),nGenerations));

  for (i in 1:nGenerations) {
    outCI[,i] = quantile(population[i,],quantVals);

  }


  ### Now make a plot
  total_data=data.frame(steps=1:nGenerations,population =population) %>%
    gather(key=simulation,value=population,2:(nSimulations+1))

  ### Spaghetti plot
  spaghettiPlot=ggplot(total_data,aes(x=steps,y=population,group=simulation)) +
    geom_line() +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Generation",y = "Population",title="Spaghetti Plot")

  print(spaghettiPlot)

  ### Ensemble plot
  data=data.frame(steps=1:nGenerations,
                  F =outCI[2,],
                  L =outCI[1,],
                  U =outCI[3,])


  ensemblePlot=ggplot(data,aes(x=steps,y=F)) +
    geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
    geom_line(size=1.5) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Years",y = "Population",title="Ensemble Plot")

  print(ensemblePlot)





}





