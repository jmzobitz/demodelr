#' Numerical solution for a differential equation.
#'
#' \code{systems} solves a multi-dimensional dimensional differential equation with numerical differential equation solvers.
#' See the vignette for detailed examples of usage.

#' @param deltaT Size of timesteps
#' @param timeSteps Number of timesteps we solve.  deltaT*timeSteps = total time
#' @param initialCondition Listing of initial conditions
#' @param dynamics a Function that we have for our dynamics
#' @param parameters The values of the parameters we are using
#' @param returnData A T/F if we want to return just the solution data than a plot.
#' @return A plot of your solved equation solution
#' @examples
#' systems(deltaT,timeSteps,initialCondition,FUN=dynamics,parameters=parameters)
#' # Run the vignette that works through an example
#' vignette("systems")

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import deSolve
#' @export

systems <- function(deltaT=1,timeSteps=1,initialCondition,FUN=dynamics,parameters=parameters,returnData=FALSE) {

  #  A quick check in case we have a one dimensional system
  if (is.null(dim(initialCondition))) {nSolns <- 1}
  else { nSolns <- dim(initialCondition)[1] }


  # Make a list of things we are bringing back
  run_results <- vector("list", nSolns)
  time = seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector
  # Loop through all the initial conditions.
  for(i in 1:nSolns) {

    if (nSolns ==1) {
      currInit <- initialCondition
    } else {
      currInit <- initialCondition[i,]
    }

    out <- ode(y = currInit, times = time, func = dynamics, parms = parameters) ## Integration with 'ode'

    ## Organize the solutions by run to make them plot easily.
    outSolutions = out %>%
      data.frame() %>%
      gather(key=variables,value=value,-1) # required by ggplot: data object must be a data frame
    outSolutions$run = i

    run_results[[i]] <- outSolutions
  }


  # Gather the solution in a format for plotting.
  outPlot <- run_results %>%
    bind_rows() %>%
    mutate(run=as.factor(run)) %>%
    ggplot(aes(x=time, y=value,color=as.factor(run),shape=variables,group=run)) +
    geom_line(size=2)+facet_grid(.~variables) +
    labs(title="Differential Equation Solution",x="Time",y="")+
    ### Expand the graph to make sure axes cross at (0,0)
    expand_limits(y=0) +
    theme(legend.position="none")


  if(returnData) {

    outData <- run_results %>% bind_rows() %>%
      mutate(run=as.factor(run))
    return(outData)
  } else { return(outPlot) }


}


