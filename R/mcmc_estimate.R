#' Markov Chain parameter estimates
#'
#' \code{mcmc_estimate} Computes and Markov Chain Monte Carlo parameter estimate for a given model
#'

#' @param model the model equations that we use to compute the result.
#' @param data the data used to assess the model
#' @param parameters a data frame that lists the names of the parameters along with upper and lower bounds
#' @param iterations the number of iterations we wish to run the MCMC for.
#' @param knob_flag determines if we tune the range that can be search (annealling)
#' @return A dataframe: the first column is the accept flag  of the mcmc run (TRUE/FALSE), the log likelihood, and the parameter values

#' @import dplyr
#' @export


mcmc_estimate <- function(model,data,parameters,iterations=1,knob_flag=FALSE) {

  param_info <- parameters %>%
    mutate(knob=1,
           range = upper_bound-lower_bound) %>%
    rowwise() %>%
    mutate(value = runif(1,min=lower_bound,max=upper_bound)) %>%
    relocate(name,value)

  # Have a vector that just gives out the current parameter values
  curr_param <- param_info %>%
    select(name,value) %>%
    pivot_wider()

  # The current likelihood for comparison
  curr_likelihood <- compute_likelihood(model,data,curr_param,logLikely = TRUE)$likelihood

  # Start building up the list for iterations
  out_iter <- vector("list",length=iterations)

  nParams <- dim(param_info)[1]

  # Identify which params we want to use and the tuning
  param_samples <- sample(param_info$name,size=iterations,replace=TRUE)
  tune_values <- runif(iterations)-0.5
  random_accept = rexp(iterations)  # A test to see if we want to keep a parameter that is slightly worse

  # Define some ctarget values
  A_STAR<-0.4  # target acceptance rate
  DEC<-0.99  # how much to decrease temp. by on rejection
  INC <- DEC^((A_STAR - 1)/A_STAR);
  # want INC^A_STAR * DEC^(1 - A_STAR) = 1

  # Now start to do the loop
  for (i in seq_along(out_iter)) {

    accept_flag <- TRUE
    curr_sample <- param_samples[[i]]
    curr_tune <- tune_values[[i]]


    # Sample one of the parameters
    sample_param <- param_info %>%
      mutate(old_value = value,
             value = if_else(name==curr_sample,
                             knob * range * curr_tune+value,value),
             in_bounds = between(value,lower_bound,upper_bound))


    if (sum(sample_param$in_bounds) == nParams) {  # If we are in the ranges, then go, otherwise ignore

      new_param <- sample_param %>%
        select(name,value) %>% pivot_wider()

      sample_likelihood <- compute_likelihood(model,data,new_param,logLikely = TRUE)$likelihood

      # OK: if the difference is positive, we might want to reject
      l_diff <- sample_likelihood$l_hood - curr_likelihood$l_hood


    } else {
      accept_flag = FALSE
    }



    # Since we have the log likelihood we want to minimize the log likelihood.  If this is positive, then we may want to keep it
    if (accept_flag & (l_diff < random_accept[[i]]  )   ) {

      # Update date the current parameters
      curr_param <- new_param
      curr_likelihood <- sample_likelihood

      # Adjust bounds if we are accepting: (knob tuning)

      if (knob_flag) {
        param_info <- sample_param %>%
          mutate(knob = if_else(name==curr_sample,
                                max(knob*INC,1e-8),knob) ) %>%
          select(-in_bounds)

      } else {
        param_info <- sample_param %>%
          select(-in_bounds)
      }


      accept_flag <- TRUE  # I think this is not necessary

    } else {

      accept_flag <- FALSE

      if (knob_flag) {
        # Adjust bounds if we are rejecting:
        param_info <- sample_param %>%
          mutate(value = old_value,
                 knob = if_else(name==curr_sample,
                                max(knob*DEC,1e-8),knob) ) %>%
          select(-old_value,-in_bounds)
      } else {
        # Adjust bounds if we are accepting:
        param_info <- sample_param %>%
          mutate(value = old_value) %>%
          select(-old_value,-in_bounds)
      }






    }

    # Update the list
    out_iter[[i]] <- list(likelihood = curr_likelihood,
                          acceptFlag = accept_flag)


  }


  out_results <- tibble(nested = out_iter) %>%
    hoist(nested,
          accept_flag = "acceptFlag",
          lhood = "likelihood") %>%
    unnest(cols=c(lhood)) %>%
    select(-log_lik) %>%
    relocate(accept_flag,l_hood)


  return(out_results)




}
