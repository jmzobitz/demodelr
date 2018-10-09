#' Phase plane of differential equation.
#'
#' \code{mcmcEstimate} Computes and Markov Chain Monte Carlo parameter estimate for a given model
#'
#' @param n_points number of points we evaluate on the grid in both directions
#' @param x_window x axis limits.  Must be of the form c(minVal,maxVal)
#' @param y_window y axis limits.  Must be of the form c(minVal,maxVal)
#' @param x_label x axis label for plot
#' @param y_label y axis label for plot
#' @param initialCondition Listing of initial conditions
#' @param parameters an initial guess for our parameters
#' @param iterations the number of iterations we wish to run the MCMC for.
#' @param burn_percent the percentage of the iterations we discard due to "burn-in". This should be a number between 0 and 1
#' @param solveModel a function that solves the model we are interested in.
#' @return A phase plane diagram of system of differential equations
#' @examples
#' # Run the vignette that works through an example
#' vignette("mcmc")
#'
#' @import FME
#' @import GGally
#' @import tidyr
#' @import ggplot2
#' @export




mcmcEstimate <- function(parameters,iterations = 1500,lower_bound,upper_bound,burn_percent) {

  # cost function
  cost <- function(p){
    out = solveModel(p)
    modCost(out, input_data)
  }

  burninlength = floor(burn_percent*iterations)


  # do MCMC
  fit = modMCMC(f = cost, p = parameters, niter=1500, burninlength = 700, lower = lower_bound, upper = upper_bound, verbose = TRUE)

  # view results
  ### Can we save this to a file?  make a directory in the folder?
  summary(fit)

  print("The best parameter value:")
  print(fit$bestpar)

  # Make a correlation plot uses ggpairs

  ggpairs(data.frame(fit$pars), diag = list(continuous ="barDiag", discrete = "barDiag", na = "naDiag"))


  # Generate a summary of the model and the data - with confidence intervals
  sR <- sensRange(func = solveModel,parms=parameters,parInput=fit$pars) %>%
    summary() %>% rename(time=x)

  vars<-str_extract_all(row.names(sR),paste(names(initialCondition), collapse="|")) %>% unlist()

  plotData <- sR %>% mutate(vars)

  ### Now let's do the ribbon w/ the data - yay!
  measuredData <- input_data %>% gather(key=vars,value=measurement,-1)
  p1 <- ggplot(plotData)+
    geom_line(aes(x=time,y=q50)) +
    geom_ribbon(aes(x=time,ymin=q05,ymax=q95),alpha=0.3) +
    geom_point(data=measuredData,aes(x=time,y=measurement),color="red",size=2) +
    facet_grid(.~vars) + labs(y="")

  print(p1)





}

