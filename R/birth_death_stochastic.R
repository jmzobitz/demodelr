#' Euler-Maruyama method birth-death solution for a stochastic differential equation.
#'
#' \code{birth_death_stochastic} solves a multi-dimensional differential equation using a birth-death process, applying the Euler-Maruyama method. A reality constraint is applied so the variables can never be zero.


#' @param birth_rate The 1 or multi dimensional system of equations for the birth rate, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param death_rate The 1 or multi dimensional system of equations for the death rate of the differential equation, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param init_cond Listing of initial conditions (we can only do one initial condition)
#' @param parameters The values of the parameters we are using
#' @param t_start The starting time point (defaults to t = 0)
#' @param deltaT The timestep length (defaults to 1)
#' @param n_steps The number of timesteps to compute solution (defaults to n_steps = 1)
#' @param sigma scaling factor for of the stochastic part of the SDE

#' @return A tidy of data frame the solutions

#' @examples

#' TBD


#' @import dplyr
#' @import formula.tools
#' @import tidyr
#' @export

birth_death_stochastic <- function(birth_rate,death_rate,init_cond,parameters=NULL,t_start=0,deltaT=1,n_steps=1,sigma=1) {

  # Add time to our condition vector, identify the names
  curr_vec <- c(init_cond,t=t_start)

  vec_names <- names(curr_vec)
  n_vars <- length(vec_names)  # Number of variables

  # Make the mean and error equation (we do one less because we added t to curr_vec)
  mean_eq <- vector("list",length=n_vars-1)
  std_eq <- vector("list",length=n_vars-1)

  # Identify the mean and the variance
  for (i in seq_along(birth_rate)) {

    mean_eq[[i]] <- as.formula(
      paste0(
        as.character(formula.tools::lhs(birth_rate[i])),
        "~",
        as.character(formula.tools::rhs(birth_rate[i])),
        "-",
        as.character(formula.tools::rhs(death_rate[i])) )
    )

    std_eq[[i]] <- as.formula(
      paste0(
        as.character(formula.tools::lhs(birth_rate[i])),
        "~ ",
        as.character(formula.tools::rhs(birth_rate[i])),
        "+",
        as.character(formula.tools::rhs(death_rate[i])) )
    )

  }



  time_eq <- c(dt ~ 1)  # This is an equation to keep track of the dt
  new_mean_eq <- c(mean_eq,time_eq) %>%
    formula.tools::rhs()

  time_eq_stoc <- c(dt~0)
  new_std_eq <- c(std_eq,time_eq_stoc) %>%
    formula.tools::rhs()


  # Start building the list
  out_list <- vector("list",length=n_steps)
  out_list[[1]] <- curr_vec

  for(i in 2:n_steps) {

    # Define the list of inputs to the rate equation
    in_list <- c(parameters,curr_vec) %>% as.list()

    curr_mean <-sapply(new_mean_eq,FUN=eval,envir=in_list) %>%
      purrr::set_names(nm =vec_names)

    curr_std <-sapply(new_std_eq,FUN=eval,envir=in_list) %>%
      purrr::set_names(nm =vec_names)

    # For a system of equations we need to compute the matrix square root. This should work even for systems where we have one equation
    sqrt_matrix <- (Re(expm::sqrtm(curr_std[1:(n_vars-1)] %*% t(curr_std[1:(n_vars-1)])) ))

    # Pad a row of zeros for the time variable (it is NOT stochastic)
    s_rev <- rbind(cbind(sqrt_matrix,0),0)

    # This is our update equation
    out_compute <- (s_rev*sigma*sqrt(deltaT)) %*% rnorm(n_vars) %>%
      purrr::set_names(nm =vec_names)

    # Now we add them together and update
    v3 <- c(curr_vec, curr_mean*deltaT,out_compute)
    curr_vec <-  tapply(v3, names(v3), sum)
    curr_vec[curr_vec<0] <- 0  # Reality constraint forcing all variables to be positive

    out_list[[i]] <- curr_vec

  }



  # Accumulate as we go and build up the data frame. This seems like magic.
  out_results <- out_list %>%
    bind_rows() %>%
    dplyr::relocate(t)  # Put t at the start



  return(out_results)



}


