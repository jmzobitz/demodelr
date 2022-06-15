#' Markov Chain parameter estimates
#'
#' \code{mcmc_analyze} Computes summary histograms and model-data comparisons from and Markov Chain Monte Carlo parameter estimate for a given model
#'
#' @param model the model equations that we use to compute the result.
#' @param data the data used to assess the model
#' @param mcmc_out A dataframe: the first column is the accept flag  of the mcmc run (TRUE/FALSE), the log likelihood, and the parameter values
#' @param mode two choices: emp --> empirical (default) or de --> differential equations. The estimator works differently depending on which is used.
#' @param initial_condition The initial condition for the differential equation (DE mode only)
#' @param deltaT The length between timesteps (DE mode only)
#' @param n_steps The number of time steps we run the model (DE mode only)
#' @param verbose TRUE / FALSE indicate if parameter estimates should be printed to console (option, defaults to TRUE)
#'
#' @seealso \code{\link{mcmc_estimate}}
#'
#' @return Two plots: (1) fitted model results compared to data, and (2) pairwise parameter histograms and scatterplots to test model equifinality.
#'
#' @examples
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
#' ## Step 4: Analyze results:
#' mcmc_analyze(model = phos_model,
#' data = phosphorous,
#' mcmc_out = phos_mcmc)
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
#'  parameters = tourism_param,
#'  mode = "de",
#'  initial_condition = tourism_init, deltaT = deltaT,
#'  n_steps = n_steps,
#'  iterations = tourism_iter)
#'
#' ## Step 4: Analyze results
#' mcmc_analyze(
#'  model = tourism_model,
#'  data = parks,
#'  mcmc_out = tourism_out,
#'  mode = "de",
#'  initial_condition = tourism_init, deltaT = deltaT,
#'  n_steps = n_steps
#' )
#'
#' }
#' @importFrom rlang .data
#' @importFrom utils capture.output
#' @importFrom utils head
#' @importFrom stats quantile
#' @import GGally
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @export




mcmc_analyze <- function(model, data, mcmc_out,mode="emp",initial_condition = NULL,deltaT = NULL,n_steps=NULL,verbose=TRUE) {

  # rename this to avoid confusion
  in_data <- data
  independent_var <- names(in_data)[1] # We will always have the independent variable first

  # Determine the summary results:
  if(verbose) {
    message("The parameter values at the optimized log likelihood:")
    print_params <- mcmc_out %>%
      dplyr::filter(.data$accept_flag) %>%
      dplyr::slice_min(.data$l_hood) %>%
      dplyr::select(-.data$accept_flag)

    out_message <- paste(utils::capture.output(utils::head(print_params, dim(print_params)[1])), collapse="\n")
     message(out_message)

    message("The 95% confidence intervals for the parameters:")
    print_probs <- mcmc_out %>%
      dplyr::filter(.data$accept_flag) %>%
      dplyr::select(-.data$accept_flag, -.data$l_hood) %>%
      dplyr::summarize(across(.fns = stats::quantile, probs = c(0.025, 0.50, 0.975))) %>%
      dplyr::mutate(probs = c("2.5%", "50%", "97.5%")) %>%
      dplyr::relocate(.data$probs)


    out_message <- paste(utils::capture.output(utils::head(print_probs, dim(print_probs)[1])), collapse="\n")
    message(out_message)


  }



  # Plot the estimates
  param_estimates <- mcmc_out %>%
    dplyr::filter(.data$accept_flag) %>%
    dplyr::select(-.data$accept_flag, -.data$l_hood)

  GGally::ggpairs(data = param_estimates, diag = list(continuous = "barDiag", discrete = "barDiag", na = "naDiag")) %>%
    print()

  if (mode == "emp") {
    # Get the right hand side of your equations
    new_eq <- model %>%
      formula.tools::rhs()

    # Internal function to compute the model
    compute_model_emp <- function(parameters, model_eq, mydata) {
      in_list <- c(parameters, mydata) %>% as.list()
      out_data <- eval(model_eq, envir = in_list)

      out_tibble <- mydata %>%
        dplyr::mutate(model = out_data)
      return(out_tibble)
    }

    out_model <- mcmc_out %>%
      dplyr::filter(.data$accept_flag) %>%
      dplyr::select(-.data$accept_flag, -.data$l_hood) %>%
      dplyr::mutate(id = 1:n()) %>%
      dplyr::group_by(id) %>%
      tidyr::nest() %>%
      dplyr::rename(in_params = data) %>%
      dplyr::mutate(m_data = lapply(X = .data$in_params, FUN = compute_model_emp, new_eq, in_data)) %>%
      tidyr::unnest(cols = c(.data$m_data)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-id, -.data$in_params)


    ggplot2::ggplot(out_model) +
      geom_boxplot(aes_string(x = independent_var, y = "model",group = independent_var),outlier.shape = NA) +
      geom_point(data = in_data, aes_string(x = independent_var, y = names(in_data)[2]), color = "red", size = 2) +
      labs(y = names(in_data)[2])


  } else if (mode == "de") {
    ### THIS IS MODE DEPENDENT (EMPIRICAL OR DIFFERENTIAL EQUATION)
    # Get the right hand side of your equations
    # new_eq <- my_model %>%
    #   formula.tools::rhs()
    #
    # # Internal function to compute the model
    compute_model_de <- function(parameters, model,initial_condition, deltaT,n_steps) {

      out_data <- rk4(model,
                      parameters = parameters,
                      initial_condition=initial_condition,
                      deltaT=deltaT,
                      n_steps = n_steps) %>%
        tidyr::pivot_longer(cols=c(-"t")) %>%
        dplyr::rename(model=.data$value)


      return(out_data)
    }


    out_model <- mcmc_out %>%
      dplyr::filter(.data$accept_flag) %>%
      dplyr::select(-.data$accept_flag, -.data$l_hood) %>%
      dplyr::mutate(id = 1:n()) %>%
      dplyr::group_by(id) %>%
      tidyr::nest() %>%
      dplyr::rename(in_params = data) %>%
      dplyr::mutate(m_data = lapply(X = .data$in_params, FUN = compute_model_de, model, initial_condition,deltaT,n_steps)) %>%
      tidyr::unnest(cols = c(.data$m_data)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-id, -.data$in_params) %>%
      dplyr::group_by(across(c(1,2))) %>%
      dplyr::summarize(across(.cols = c("model"), .fns = stats::quantile, probs = c(0.025, 0.50, 0.975))) %>%
      dplyr::mutate(probs = c("q025", "q50", "q975")) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = "probs", values_from = "model")


    # Mutate the data in a long format for comparison
    data_long <- data %>%
      tidyr::pivot_longer(cols=c(-1)) %>%
      dplyr::rename(t=1)

    ggplot2::ggplot(out_model) +
      geom_line(aes(x = t, y = .data$q50)) +
      geom_ribbon(aes(x = t, ymin = .data$q025, ymax = .data$q975), alpha = 0.3) +
      geom_point(data = data_long, aes(x = t, y = .data$value), color = "red", size = 2) +
      labs(y = "") + facet_grid(name~.,scales="free_y")

  }


}
