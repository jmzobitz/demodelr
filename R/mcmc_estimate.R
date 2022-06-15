#' Markov Chain parameter estimates
#'
#' \code{mcmc_estimate} Computes and Markov Chain Monte Carlo parameter estimate for a given model
#'

#' @param model the model equations that we use to compute the result.
#' @param data the data used to assess the model
#' @param parameters a data frame that lists the names of the parameters along with upper and lower bounds
#' @param iterations the number of iterations we wish to run the MCMC for.
#' @param knob_flag determines if we tune the range that can be search (annealing)
#' @param mode two choices: emp --> empirical (default) or de --> differential equations. The estimator works differently depending on which is used.
#' @param initial_condition The initial condition for the differential equation (DE mode only)
#' @param deltaT The length between timesteps (DE mode only)
#' @param n_steps The number of time steps we run the model (DE mode only)

#' @return A dataframe: the first column is the accept flag  of the mcmc run (TRUE/FALSE), the log likelihood, and the parameter values
#'
#' @seealso \code{\link{mcmc_analyze}}
#'
#' @examples
#'
#' \donttest{
#' ## Example with an empirical model:
#' ## Step 1: Define the model and parameters
#' phos_model <- daphnia ~ c * algae^(1 / theta)
#'
# ## Define the parameters that you will use with their bounds
#' phos_param <- tibble::tibble( name = c("c", "theta"),
#' lower_bound = c(0, 1),
#' upper_bound = c(2, 20))
#'
#' ## Step 2: Determine MCMC settings
#' # Define the number of iterations
#' phos_iter <- 1000
#'
#' ## Step 3: Compute MCMC estimate
#' phos_mcmc <- mcmc_estimate(model = phos_model,
#' data = phosphorous,
#' parameters = phos_param,
#' iterations = phos_iter)
#'
#' ## Example with a differential equation:
#' ## Step 1: Define the model, parameters, and data
#' ## Define the tourism model
#' tourism_model <- c(dRdt ~ resources * (1 - resources) - a * visitors,
#' dVdt ~ b * visitors * (resources - visitors))
#'
#' # Define the parameters that you will use with their bounds
#' tourism_param <- tibble::tibble( name = c("a", "b"),
#' lower_bound = c(10, 0),
#' upper_bound = c(30, 5))
#'
#' ## Step 2: Determine MCMC settings
#' # Define the initial conditions
#' tourism_init <- c(resources = 0.995, visitors = 0.00167)
#' deltaT <- .1 # timestep length
#' n_steps <- 15 # must be a number greater than 1
#' # Define the number of iterations
#' tourism_iter <- 1000
#'
#' ## Step 3: Compute MCMC estimate
#' tourism_out <- mcmc_estimate(
#'  model = tourism_model,
#'  data = parks,
#'   parameters = tourism_param,
#'  mode = "de",
#'   initial_condition = tourism_init, deltaT = deltaT,
#'  n_steps = n_steps,
#'   iterations = tourism_iter)
#' }
#'

#'
#' @importFrom rlang .data
#' @importFrom stats sd runif rexp
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @export


mcmc_estimate <- function(model,data,parameters,iterations=1,knob_flag=FALSE,mode="emp",initial_condition = NULL,deltaT = NULL,n_steps=NULL) {

  if (mode == "emp") {

    param_info <- parameters %>%
      dplyr::mutate(knob=1,
             range = .data$upper_bound-.data$lower_bound) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = runif(1,min=.data$lower_bound,max=.data$upper_bound)) %>%
      dplyr::relocate(.data$name,.data$value)

    # Have a vector that just gives out the current parameter values
    curr_param <- param_info %>%
      dplyr::select(.data$name,.data$value) %>%
      tidyr::pivot_wider()

    # The current likelihood for comparison
    curr_likelihood <- compute_likelihood(model,data,curr_param,logLikely = TRUE)$likelihood

    # Start building up the list for iterations
    out_iter <- vector("list",length=iterations)

    nParams <- dim(param_info)[1]

    # Identify which params we want to use and the tuning
    param_samples <- sample(param_info$name,size=iterations,replace=TRUE)
    tune_values <- stats::runif(iterations)-0.5
    random_accept = stats::rexp(iterations)  # A test to see if we want to keep a parameter that is slightly worse

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
        dplyr::mutate(old_value = .data$value,
               value = if_else(.data$name==curr_sample,
                               .data$knob * range * curr_tune+.data$value,.data$value),
               in_bounds = between(.data$value,.data$lower_bound,.data$upper_bound))


      if (sum(sample_param$in_bounds) == nParams) {  # If we are in the ranges, then go, otherwise ignore

        new_param <- sample_param %>%
          dplyr::select(.data$name,.data$value) %>%
          tidyr::pivot_wider()

        sample_likelihood <- compute_likelihood(model,data,new_param,logLikely = TRUE)$likelihood

        # OK: if the difference is positive, we might want to reject
        l_diff <- sample_likelihood$l_hood - curr_likelihood$l_hood


      } else {
        accept_flag = FALSE
        l_diff <- NA
      }



      # Since we have the log likelihood we want to minimize the log likelihood.  If this is positive, then we may want to keep it
      if (accept_flag & (l_diff < random_accept[[i]]  ) & !is.na(l_diff)   ) {

        # Update date the current parameters
        curr_param <- new_param
        curr_likelihood <- sample_likelihood

        # Adjust bounds if we are accepting: (knob tuning)

        if (knob_flag) {
          param_info <- sample_param %>%
            dplyr::mutate(knob = if_else(.data$name==curr_sample,
                                  max(.data$knob*INC,1e-8),.data$knob) ) %>%
            dplyr::select(-.data$in_bounds)

        } else {
          param_info <- sample_param %>%
            dplyr::select(-.data$in_bounds)
        }


        accept_flag <- TRUE  # I think this is not necessary

      } else {

        accept_flag <- FALSE
        l_diff <- NA

        if (knob_flag) {
          # Adjust bounds if we are rejecting:
          param_info <- sample_param %>%
            dplyr::mutate(value = .data$old_value,
                   knob = if_else(.data$name==curr_sample,
                                  max(.data$knob*DEC,1e-8),.data$knob) ) %>%
            dplyr::select(-.data$old_value,-.data$in_bounds)
        } else {
          # Adjust bounds if we are accepting:
          param_info <- sample_param %>%
            dplyr::mutate(value = .data$old_value) %>%
            dplyr::select(-.data$old_value,-.data$in_bounds)
        }






      }

      # Update the list
      out_iter[[i]] <- list(likelihood = curr_likelihood,
                            acceptFlag = accept_flag)


    }



  } else if (mode == "de") {  # Differential equation mode

    param_info <- parameters %>%
      dplyr::mutate(knob=1,
             range = .data$upper_bound-.data$lower_bound) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = runif(1,min=.data$lower_bound,max=.data$upper_bound)) %>%
      dplyr::relocate(.data$name,.data$value)

    # Have a vector that just gives out the current parameter values
    curr_param <- param_info %>%
      dplyr::select(.data$name,.data$value) %>%
      tidyr::pivot_wider()

    # Define this as a vector solution
    curr_param_vec <- param_info %>%
      dplyr::select(.data$name,.data$value) %>%
      tibble::deframe()

    # Mutate the data in a long format for comparison
    data_long <- data %>%
      tidyr::pivot_longer(cols=c(-1)) %>%
      dplyr::rename(t=1) %>%
      dplyr::mutate(type="data")

    # Compute the model values, pivot longer, nest
    out_solution <- rk4(model,
                        parameters = curr_param_vec,
                        initial_condition=initial_condition,
                        deltaT=deltaT,
                        n_steps = n_steps)

    # Make the solution long, add in the long data, nest, and pivot. This helps the linear approximation
    out_solution_long <- out_solution %>%
      tidyr::pivot_longer(cols=c(-"t")) %>%
      dplyr::mutate(type="model") %>%
      rbind(data_long) %>%
      dplyr::group_by(.data$name,.data$type) %>% nest() %>%
      tidyr::pivot_wider(names_from="type",values_from="data")

    # Now apply the map to get out the model results AT the timesteps the data are measured at
    out_solution_trim <- out_solution_long %>%
      dplyr::mutate(model_results = map2(.x=model, .y=data,.f=~(approx(.x$t, .x$value, xout = .y$t, method = "linear") %>% as_tibble()))) %>%
      dplyr::select(-model,-data) %>%
      tidyr::unnest(cols=c(.data$model_results)) %>%
      dplyr::rename(t=.data$x,value=.data$y) %>%
      dplyr::mutate(type="model") %>%
      dplyr::inner_join(data_long,by=c("t","name"))

    # Internal function to compute the likelihood
    likelihood <- function(y,mydata,logLikely){
      error = stats::sd(mydata-y)
      singlelikelihoods = dnorm(mydata, mean = y, sd = error, log = logLikely)

      if (logLikely) {
        return(-sum(singlelikelihoods))  # Here we make the log likelihood positive.
      } else {
        return(prod(singlelikelihoods))
      }


    }



    # The current likelihood for comparison
    curr_likelihood <- tibble(l_hood = likelihood(out_solution_trim$value.x,out_solution_trim$value.y,TRUE),
                              log_lik = TRUE) %>%
      cbind(curr_param)

    # Start building up the list for iterations
    out_iter <- vector("list",length=iterations)

    nParams <- dim(param_info)[1]

    # Identify which params we want to use and the tuning
    param_samples <- sample(param_info$name,size=iterations,replace=TRUE)
    tune_values <- stats::runif(iterations)-0.5
    random_accept = stats::rexp(iterations)  # A test to see if we want to keep a parameter that is slightly worse

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
        dplyr::mutate(old_value = .data$value,
               value = if_else(.data$name==curr_sample,
                               .data$knob * range * curr_tune+.data$value,.data$value),
               in_bounds = between(.data$value,.data$lower_bound,.data$upper_bound))


      if (sum(sample_param$in_bounds) == nParams) {  # If we are in the ranges, then go, otherwise ignore

        new_param <- sample_param %>%
          dplyr::select(.data$name,.data$value) %>%
          tidyr::pivot_wider()


        # Define this as a vector solution
        new_param_vec <- sample_param %>%
          dplyr::select(.data$name,.data$value) %>%
          tibble::deframe()


        # Compute the model values, pivot longer, nest
        out_solution <- rk4(model,
                            parameters = new_param_vec,
                            initial_condition=initial_condition,
                            deltaT=deltaT,
                            n_steps = n_steps)

        # Make the solution long, add in the long data, nest, and pivot. This helps the linear approximation
        out_solution_long <- out_solution %>%
          tidyr::pivot_longer(cols=c(-"t")) %>%
          dplyr::mutate(type="model") %>%
          rbind(data_long) %>%
          dplyr::group_by(.data$name,.data$type) %>% nest() %>%
          tidyr::pivot_wider(names_from="type",values_from="data")

        # Now apply the map to get out the model results AT the timesteps the data are measured at
        out_solution_trim <- out_solution_long %>%
          dplyr::mutate(model_results = map2(.x=model, .y=data,.f=~(approx(.x$t, .x$value, xout = .y$t, method = "linear") %>% as_tibble()))) %>%
          dplyr::select(-model,-data) %>%
          tidyr::unnest(cols=c(.data$model_results)) %>%
          dplyr::rename(t=.data$x,value=.data$y) %>%
          dplyr::mutate(type="model") %>%
          dplyr::inner_join(data_long,by=c("t","name"))

        # The current likelihood for comparison
        sample_likelihood <- tibble(l_hood = likelihood(out_solution_trim$value.x,out_solution_trim$value.y,TRUE),
                                    log_lik = TRUE) %>%
          cbind(new_param)


        ####
        # OK: if the difference is positive, we might want to reject
        l_diff <- sample_likelihood$l_hood - curr_likelihood$l_hood


      } else {
        accept_flag = FALSE
        l_diff <- NA
      }



      # Since we have the log likelihood we want to minimize the log likelihood.  If this is positive, then we may want to keep it
      if (accept_flag & (l_diff < random_accept[[i]]  ) & !is.na(l_diff)   ) {

        # Update date the current parameters
        curr_param <- new_param
        curr_likelihood <- sample_likelihood

        # Adjust bounds if we are accepting: (knob tuning)

        if (knob_flag) {
          param_info <- sample_param %>%
            dplyr::mutate(knob = if_else(.data$name==curr_sample,
                                  max(.data$knob*INC,1e-8),.data$knob) ) %>%
            dplyr::select(-.data$in_bounds)

        } else {
          param_info <- sample_param %>%
            dplyr::select(-.data$in_bounds)
        }


        accept_flag <- TRUE  # I think this is not necessary

      } else {

        accept_flag <- FALSE
        l_diff <- NA

        if (knob_flag) {
          # Adjust bounds if we are rejecting:
          param_info <- sample_param %>%
            dplyr::mutate(value = .data$old_value,
                   knob = if_else(.data$name==curr_sample,
                                  max(.data$knob*DEC,1e-8),.data$knob) ) %>%
            dplyr::select(-.data$old_value,-.data$in_bounds)
        } else {
          # Adjust bounds if we are accepting:
          param_info <- sample_param %>%
            dplyr::mutate(value = .data$old_value) %>%
            dplyr::select(-.data$old_value,-.data$in_bounds)
        }






      }

      # Update the list
      out_iter[[i]] <- list(likelihood = curr_likelihood,
                            acceptFlag = accept_flag)


    }



  }

  out_results <- tibble(nested = out_iter) %>%
    tidyr::hoist(.data$nested,
          accept_flag = "acceptFlag",
          lhood = "likelihood") %>%
    tidyr::unnest(cols=c(.data$lhood)) %>%
    dplyr::select(-.data$log_lik) %>%
    dplyr::relocate(accept_flag,.data$l_hood)


  return(out_results)




}
