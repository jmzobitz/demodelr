#' Euler's method solution for a differential equation.
#'
#' \code{eulerStochastic} solves a multi-dimensional differential equation with Euler's method with stochastic elements.
#' See the vignette for detailed examples of usage.

#' @param deltaT Size of timesteps
#' @param timeSteps Number of timesteps we solve.  deltaT*timeSteps = total time
#' @param initialCondition Listing of initial conditions (we can only do one initial condition)
#' @param deterministicDynamics a function that we have for the deterministic part of the SDE
#' @param stochasticDynamics a function we have for the stochastic part of the SDE
#' @param parameters The values of the parameters we are using
#' @param nSimulations the number of simulations we are running
#' @param sigma scaling factor for of the stochastic part of the SDE
#' @return A spaghetti plot and ensemble average plot of your solution
#' @examples
#' eulerStochastic(deltaT,timeSteps,initialCondition,variableNames,deterministicDynamics,stochasticDynamics,parameters=parameters)
#' # Run the vignette that works through an example:
#' vignette("euler-stochastic")

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export

eulerStochastic <- function(deltaT=1,timeSteps=1,initialCondition,FUN=deterministicDynamics,FUN_2=stochasticDynamics,parameters=parameters,nSimulations=1,sigma=1) {


  # Make a list of things we are bringing back
  run_results <- vector("list", nSimulations)
  time = seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector
  # Loop through all the initial conditions.
  for(j in 1:nSimulations) {
    soln <- initialCondition
    newP <-soln


    for (i in 2:timeSteps) {
      oldP = newP
      newP <-  unlist(deterministicDynamics(time[i],oldP,parameters))*deltaT+ unlist(stochasticDynamics(time[i],oldP,parameters))*sigma*sqrt(deltaT)*rnorm(1)+oldP   # Your differential equation goes here.

      ## Add in a reality constraint that they can't be negative
      newP <- pmax(newP,0)

      soln=rbind(soln,newP)

    }

    run_results[[j]] <- data.frame(soln,row.names=NULL) %>%
      mutate(simulation=j,time=time) %>%
      gather(key=variables,value=value,-simulation,-time)


  }

  ### Now plot the solution

  spaghettiPlot <- run_results %>% bind_rows() %>%
    ggplot() +
    geom_line(aes(x=time,y=value,group=simulation)) +
    facet_grid(.~variables) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Time",y="",title="Spaghetti Plot")

  print(spaghettiPlot)

  ### plot the ensemble average plot
  ### Now loop through and make an ensemble plot of the results
  quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize


  # Stack exchange to the rescue!
  # https://stackoverflow.com/questions/37845915/how-to-use-quantile-with-dplyr-and-group-by


  ensemblePlot=run_results %>% bind_rows() %>%
    group_by(variables,time) %>%
    do(data.frame(t(quantile(.$value, probs = quantVals)))) %>%
    ggplot(aes(x=time,y=X50.)) +
    geom_ribbon(aes(ymin=X2.5.,ymax=X97.5.),alpha=0.2,colour='grey') +
    geom_line(size=1.5) +
    facet_grid(.~variables) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Time",y = "",title="Ensemble Plot")

  print(ensemblePlot)



}


