#' Numerical solution for a differential equation.
#'
#' \code{systems} solves a multi-dimensional dimensional differential equation with numerical differential equation solvers.
#' See the vignette for detailed examples of usage.

#' @param deltaT Size of timesteps
#' @param timeSteps Number of timesteps we solve.  deltaT*timeSteps = total time
#' @param initialCondition Listing of initial conditions
#' @param dynamics a Function that we have for our dynamics
#' @param parameters The values of the parameters we are using
#' @return A plot of your solved equation solution
#' @examples
#' systems(deltaT,timeSteps,initialCondition,FUN=dynamics,parameters=parameters)
#' # Run the vignette that works through an example
#' vignette("systems")

#' @import tidyverse
#' @import deSolve
#' @export

systems <- function(deltaT=1,timeSteps=1,initialCondition,FUN=dynamics,parameters=parameters) {

  #  A quick check in case we have a one dimensional system
  if (is.null(dim(initialCondition))) {initialCondition <- t(matrix(initialCondition))}
  nSolns <- dim(initialCondition)[1]  # Determine how many initial conditions we have

  # Make a list of things we are bringing back
  run_results <- vector("list", nSolns)

  # Loop through all the initial conditions.
  for(i in 1:nSolns) {
    out <- ode(y = initialCondition[i,], times = time, func = dynamics, parms = parameters) ## Integration with 'ode'

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
    ggplot(aes(x=time, y=value,color=run,shape=variables)) +
    geom_point(size=3)+facet_grid(run~variables) +
    labs(title="Differential Equation Solution",x="Time",y="")+
    ### Expand the graph to make sure axes cross at (0,0)
    expand_limits(y=0) +
    theme(legend.position="none")



  print(outPlot)

}


