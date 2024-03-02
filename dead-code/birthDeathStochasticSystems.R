#' Euler Maruyama method solution for a stochastic system of differential equations.
#'
#' \code{birthDeathStochasticSystems} solves a multi-dimensional differential equation with the Euler Maruyama method with stochastic elements using a birth death process.


#' @param deltaT Size of timesteps
#' @param timeSteps Number of timesteps we solve.  deltaT*timeSteps = total time
#' @param initialCondition Listing of initial conditions (we can only do one initial condition)
#' @param birth a function that we have for the ``birth'' processes of the SDE
#' @param death a function we have for the ``death'' processes of the SDE
#' @param parameters The values of the parameters we are using
#' @param nSimulations the number of simulations we are running
#' @param sigma scaling factor for of the stochastic part of the SDE
#' @return A spaghetti plot and ensemble average plot of your solution
#' @examples


#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom expm sqrtm
#' @export

birthDeathStochasticSystems <- function(deltaT=1,timeSteps=1,initialCondition,FUN=birth,FUN_2=death,parameters=parameters,nSimulations=1,sigma=1) {


  # Compute the number of variables:
  nVariables = length(initialCondition)
  # Make a list of things we are bringing back
  run_results <- vector("list", nSimulations)
  time = seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector
  # Loop through all the initial conditions.
  for(j in 1:nSimulations) {
    soln <- initialCondition
    newP <-soln


    for (i in 2:timeSteps) {
      oldP = newP

      meanP <- (unlist(birth(time[i],oldP,parameters))-unlist(death(time[i],oldP,parameters)))
      stdP <- Re(sqrtm(meanP %*% t(meanP)) %*% as.vector(rnorm(nVariables)))

      newP <-  as.numeric(meanP*deltaT+ stdP*sigma*sqrt(deltaT)+oldP)   # Your differential equation goes here.
      newP <- setNames(newP,names(initialCondition))

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
