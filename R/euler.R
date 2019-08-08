#' Euler's method solution for a differential equation.
#'
#' \code{euler} solves a multi-dimensional differential equation with Euler's method.
#' See the vignette for detailed examples of usage.

#' @param deltaT Size of timesteps
#' @param timeSteps Number of timesteps we solve.  deltaT*timeSteps = total time
#' @param initialCondition Listing of initial conditions
#' @param dynamics a Function that we have for our dynamics
#' @param parameters The values of the parameters we are using
#' @return A plot of your Euler's method solution
#' @examples
#' euler(deltaT,timeSteps,initialCondition,variableNames,FUN=dynamics,parameters=parameters)
#' # Run the vignette that works through an example
#' vignette("eulers-method")

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export

euler <- function(deltaT=1,timeSteps=1,initialCondition,FUN=dynamics,parameters=parameters) {

  #  A quick check in case we have a one dimensional system
  if (is.null(dim(initialCondition))) {nSolns <- 1}
  else { nSolns <- dim(initialCondition)[1] }


  # Make a list of things we are bringing back
  run_results <- vector("list", nSolns)
  time = seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector
  # Loop through all the initial conditions.
  for(j in 1:nSolns) {

    if (nSolns ==1) {
      soln <- initialCondition
    } else {
      soln <- initialCondition[j,]
    }

    newP <- soln


    for (i in 2:timeSteps) {
      oldP = newP
      newP <-  unlist(dynamics(time[i],oldP,parameters))*deltaT+oldP   # Your differential equation goes here.
      soln=rbind(soln,newP)

    }

    run_results[[j]] <- data.frame(soln,row.names=NULL) %>%
      mutate(run=j,time=time) %>%
      gather(key=variables,value=value,-run,-time)


  }


  # Gather the solution in a format for plotting.
  outPlot <- run_results %>%
    bind_rows() %>% ggplot(aes(x=time, y=value,color=run,shape=variables,group=run)) +
    geom_line(size=2)+facet_grid(.~variables) +
    labs(title="Euler's Method Solution",x="Time",y="")+
    ### Expand the graph to make sure axes cross at (0,0)
    expand_limits(y=0) +
    theme(legend.position="none")

  return(outPlot)

}



