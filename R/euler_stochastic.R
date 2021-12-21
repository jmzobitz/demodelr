#' Euler-Maruyama method solution for a stochastic differential equation.
#'
#' \code{euler_stochastic} solves a multi-dimensional differential equation with the Euler-Maruyama method with stochastic elements.


#' @param deterministic_rate The 1 or multi dimensional system of equations for the deterministic part of the differential equation, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param stochastic_rate The 1 or multi dimensional system of equations for the stochastic part of the differential equation, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param init_cond Listing of initial conditions (we can only do one initial condition)
#' @param parameters The values of the parameters we are using
#' @param t_start The starting time point (defaults to t = 0)
#' @param deltaT The timestep length (defaults to 1)
#' @param n_steps The number of timesteps to compute solution (defaults to n_steps = 1)
#' @param D diffusion coefficient for the stochastic part of the SDE

#' @return A tidy of data frame the solutions

#' @examples

#' TBD


#' @import dplyr
#' @import tidyr
#' @export

euler_stochastic <- function(deterministic_rate,stochastic_rate,init_cond,parameters=NULL,t_start=0,deltaT=1,n_steps=1,D=1) {

  # Add time to our condition vector, identify the names
  curr_vec <- c(init_cond,t=t_start)

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
    bind_rows() %>%
    dplyr::relocate(t)  # Put t at the start



  return(out_results)



}
