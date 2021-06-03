#' Runge Kutta method solution for a differential equation.
#'
#' \code{rk4} solves a multi-dimensional differential equation with Runge-Kutta 4th order method.  The parameters listed as required are needed
#' See the vignette for detailed examples of usage.

#' @param system_eq (REQUIRED) The 1 or 2 dimensional system of equations, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param timeSteps Number of timesteps we solve.  deltaT*timeSteps = total time
#' @param initial_condition (REQUIRED) Listing of initial conditions, as a vector
#' @param parameters The values of the parameters we are using (optional)
#' @param t_start The starting time point (defaults to t = 0)
#' @param deltaT The timestep length (defaults to 1)
#' @param n_steps The number of timesteps to compute solution (defaults to n_steps = 1)
#' @return A tidy of data frame the solutions
#' @examples
#' @references See https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export

rk4 <- function(system_eq,initial_condition,parameters=NULL,t_start=0,deltaT=1,n_steps=1) {

  # Add time to our condition vector, identify the names
  curr_vec <- c(init_cond,t=t_start)

  vec_names <- names(curr_vec)

  time_eq <- c(dt ~ 1)  # This is an equation to keep track of the dt
  new_rate_eq <- c(system_eq,time_eq) %>%
    formula.tools::rhs()


  # Start building the list
  out_list <- vector("list",length=n_steps)
  out_list[[1]] <- curr_vec

  for(i in 2:n_steps) {

    # Define the list of inputs to the rate equation
    in_list <- c(parameters,curr_vec) %>% as.list()

    # This is our rate
    k1 <-sapply(new_rate_eq,FUN=eval,envir=in_list) %>%
      set_names(nm = vec_names)
    in_list_k1 <- c(parameters,curr_vec+0.5*deltaT*k1) %>% as.list()

    k2 <-sapply(new_rate_eq,FUN=eval,envir=in_list_k1) %>%
      set_names(nm = vec_names)
    in_list_k2 <- c(parameters,curr_vec+0.5*deltaT*k2) %>% as.list()

    k3 <-sapply(new_rate_eq,FUN=eval,envir=in_list_k2) %>%
      set_names(nm = vec_names)

    in_list_k3 <- c(parameters,curr_vec+deltaT*k3) %>% as.list()
    k4 <-sapply(new_rate_eq,FUN=eval,envir=in_list_k3) %>%
      set_names(nm = vec_names)

    curr_vec <- curr_vec + 1/6*deltaT*(k1+2*k2+2*k3+k4)

    out_list[[i]] <- curr_vec

  }

  # Accumulate as we go and build up the data frame. This seems like magic.
  out_results <- out_list %>%
    bind_rows() %>%
    relocate(t)  # Put t at the start



  return(out_results)



}


