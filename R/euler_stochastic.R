#' Euler-Maruyama method solution for a stochastic differential equation.
#'
#' \code{euler_stochastic} solves a multi-dimensional differential equation with the Euler-Maruyama method with stochastic elements.


#' @param deterministic_rate The 1 or multi dimensional system of equations for the deterministic part of the differential equation, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param stochastic_rate The 1 or multi dimensional system of equations for the stochastic part of the differential equation, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param initial_condition (REQUIRED) Listing of initial conditions, as a vector
#' @param parameters The values of the parameters we are using
#' @param t_start The starting time point (defaults to t = 0)
#' @param deltaT The timestep length (defaults to 1)
#' @param n_steps The number of timesteps to compute solution (defaults to n_steps = 1)
#' @param D diffusion coefficient for the stochastic part of the SDE

#' @return A tidy of data frame the solutions

#' @examples

#' ### Simulate the stochastic differential equation dx = r*x*(1-x/K) dt + dW(t)
#' # Identify the deterministic and stochastic parts of the DE:
#' deterministic_logistic <- c(dx ~ r*x*(1-x/K))
#' stochastic_logistic <- c(dx ~ 1)
#'
#' # Identify the initial condition and any parameters
#' init_logistic <- c(x=3)
#'
#' logistic_parameters <- c(r=0.8, K=100) # parameters: a named vector
#'
#' # Identify how long we run the simulation
#' deltaT_logistic <- .05 # timestep length
#' timesteps_logistic <- 200 # must be a number greater than 1
#'
#' # Identify the standard deviation of the stochastic noise
#' D_logistic <- 1
#'
#' # Do one simulation of this differential equation
#' logistic_out <- euler_stochastic(
#' deterministic_rate = deterministic_logistic,
#' stochastic_rate = stochastic_logistic,
#' initial_condition = init_logistic,
#' parameters = logistic_parameters,
#' deltaT = deltaT_logistic,
#' n_steps = timesteps_logistic, D = D_logistic
#' )

#' ### Simulate a stochastic process for the tourism model presented in
#' ### Sinay, Laura, and Leon Sinay. 2006. “A Simple Mathematical
#' ### Model for the Effects of the Growth of Tourism on Environment.”
#' ### In International Tourism Conference. Alanya, Turkey.
#' ### where we have the following SDE:
#' ### dr = r*(1-r)-a*v dt, dv = b*v*(r-v) dt + v*(r-v) dW(t)
#'
#' # Identify the deterministic and stochastic parts of the DE:
#' deterministic_tourism<- c(dr ~ r*(1-r)-a*v, dv ~ b*v*(r-v))
#' stochastic_tourism <-  c(dr ~ 0, dv ~ v*(r-v))
#'
#' # Identify the initial condition and any parameters
#' init_tourism <- c(r = 0.995, v = 0.00167)
#' tourism_parameters <- c(a = 0.15, b = 0.3316)   #
#'
#' deltaT_tourism <- .5 # timestep length
#' timeSteps_tourism <- 200 # must be a number greater than 1
#'
#' # Identify the diffusion coefficient
#' D_tourism <- .05
#'
#' # Do one simulation of this differential equation
#' tourism_out <- euler_stochastic(
#'   deterministic_rate = deterministic_tourism,
#'  stochastic_rate = stochastic_tourism,
#'  initial_condition = init_tourism,
#'  parameters = tourism_parameters,
#'  deltaT = deltaT_tourism,
#'  n_steps = timeSteps_tourism,
#'  D = D_tourism
#' )


#' @import dplyr
#' @import tidyr
#' @import formula.tools
#' @importFrom stats rnorm
#' @import purrr
#' @export

euler_stochastic <- function(deterministic_rate,stochastic_rate,initial_condition,parameters=NULL,t_start=0,deltaT=1,n_steps=1,D=1) {

  # Add time to our condition vector, identify the names
  curr_vec <- c(initial_condition,t=t_start)

  vec_names <- names(curr_vec)
  n_vars <- length(vec_names)  # Number of variables

  time_eq <- c(dt ~ 1)  # This is an equation to keep track of the dt
  new_rate_eq <- c(deterministic_rate,time_eq) %>%
    formula.tools::rhs()

  time_eq_stoc <- c(dt~0)
  new_stochastic_rate <- c(stochastic_rate,time_eq_stoc) %>%
    formula.tools::rhs()


  # Start building the list
  out_list <- vector("list",length=n_steps)
  out_list[[1]] <- curr_vec

  for(i in 2:n_steps) {

    # Define the list of inputs to the rate equation
    in_list <- c(parameters,curr_vec) %>% as.list()

    curr_rate <-sapply(new_rate_eq,FUN=eval,envir=in_list) %>%
      purrr::set_names(nm =vec_names)

    curr_stoch_rate <-sapply(new_stochastic_rate,FUN=eval,envir=in_list) %>%
      purrr::set_names(nm =vec_names)

    # Now we add them together and update
    v3 <- c(curr_vec, curr_rate*deltaT,curr_stoch_rate*sqrt(2*D*deltaT)*rnorm(n_vars))
    curr_vec <-  tapply(v3, names(v3), sum)

    out_list[[i]] <- curr_vec

  }



  # Accumulate as we go and build up the data frame. This seems like magic.
  out_results <- out_list %>%
    dplyr::bind_rows() %>%
    dplyr::relocate(t)  # Put t at the start



  return(out_results)



}
