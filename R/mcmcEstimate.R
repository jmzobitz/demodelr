#' Markov Chain parameter estimates
#'
#' \code{mcmcEstimate} Computes and Markov Chain Monte Carlo parameter estimate for a given model
#'

#' @param obs_data the data we need in order to solve optimize our cost function.
#' @param parameters an initial guess for our parameters
#' @param iterations the number of iterations we wish to run the MCMC for.
#' @param lower_bound the lower bound values for our parameters
#' @param upper_bound the upper bound values for our parameters
#' @param burn_percent the percentage of the iterations we discard due to "burn-in". This should be a number between 0 and 1
#' @return A output of the accepted parameter histograms, model output (with uncertainty) + data utilized, and a listing of the accepted parameters.
#' @examples
#' # Run the vignette that works through an example.
#' vignette("mcmc")
#'
#' @import FME
#' @import GGally
#' @import tidyr
#' @import stringr
#' @import deSolve
#' @import ggplot2
#' @export




mcmcEstimate <- function(obs_data,parameters,lower_bound,upper_bound,iterations = 1500,burn_percent=0.2) {

  # cost function
  cost <- function(p){
    out = solveModel(p)
    modCost(out, obs_data)
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
  ggplot(plotData)+
    geom_line(aes(x=time,y=q50)) +
    geom_ribbon(aes(x=time,ymin=q05,ymax=q95),alpha=0.3) +
    geom_point(data=measuredData,aes(x=time,y=measurement),color="red",size=2) +
    facet_grid(vars~.,scales="free") + labs(y="") %>% print()







}

