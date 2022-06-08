#' Euler's method solution for a differential equation.
#'
#' \code{euler} solves a multi-dimensional differential equation with Euler's method.  The parameters listed as required are needed
#' See the vignette for detailed examples of usage.

#' @param system_eq (REQUIRED) The 1 or multi dimensional system of equations, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param initial_condition (REQUIRED) Listing of initial conditions, as a vector
#' @param parameters The values of the parameters we are using (optional)
#' @param t_start The starting time point (defaults to t = 0)
#' @param deltaT The timestep length (defaults to 1)
#' @param n_steps The number of timesteps to compute solution (defaults to n_steps = 1)
#' @return A tidy of data frame for the calculated solutions and the time

#' @seealso \code{\link{rk4}}
#' @examples
#' # Define the rate equation:
#' lynx_hare_eq <- c(
#'  dHdt ~ r * H - b * H * L,
#'  dLdt ~ e * b * H * L - d * L
#' )
#'
#' # Define the parameters (as a named vector):
#' lynx_hare_params <- c(r = 2, b = 0.5, e = 0.1, d = 1)
#'
#' # Define the initial condition (as a named vector):
#' lynx_hare_init <- c(H = 1, L = 3)
#'
#' # Define deltaT and the number of time steps:
#' deltaT <- 0.05
#' n_steps <- 200
#'
#' # Compute the solution via Euler's method:
#' out_solution <- euler(system_eq = lynx_hare_eq,
#'                      parameters = lynx_hare_params,
#'                      initial_condition = lynx_hare_init,
#'                      deltaT = deltaT,
#'                      n_steps = n_steps
#' )

#' @import dplyr
#' @import formula.tools
#' @import purrr
#' @import tidyr
#' @export

euler <- function(system_eq,initial_condition,parameters=NULL,t_start=0,deltaT=1,n_steps=1) {

    # Add time to our condition vector, identify the names
    curr_vec <- c(initial_condition,t=t_start)

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

      curr_rate <-sapply(new_rate_eq,FUN=eval,envir=in_list) %>%
        purrr::set_names(nm =vec_names)

      # Now we add them together and update
      v3 <- c(curr_vec, curr_rate*deltaT)
      curr_vec <-  tapply(v3, names(v3), sum)

      out_list[[i]] <- curr_vec

    }

    # Accumulate as we go and build up the data frame. This seems like magic.
    out_results <- out_list %>%
      dplyr::bind_rows() %>%
      dplyr::relocate(t)  # Put t at the start



    return(out_results)



  }


