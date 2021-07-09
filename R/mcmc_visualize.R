#' Markov Chain parameter estimates
#'
#' \code{mcmc_visualize} Computes summary histograms and model-data comparisons from and Markov Chain Monte Carlo parameter estimate for a given model
#'
#' @param model the model equations that we use to compute the result.
#' @param data the data used to assess the model
#' @param mcmc_out A dataframe: the first column is the accept flag  of the mcmc run (TRUE/FALSE), the log likelihood, and the parameter values
#'
#' @import FME
#' @import GGally
#' @import dplyr
#' @import ggplot2
#' @export




mcmc_visualize <- function(model, data, mcmc_out) {

  # rename this to avoid confusion
  in_data <- data
  independent_var <- names(in_data)[1] # We will always have the independent variable first

  # Determine the summary results:
  print("The parameter values at the optimized log likelihood:")
  mcmc_out %>%
    filter(accept_flag) %>%
    slice_min(l_hood) %>%
    select(-accept_flag) %>%
    print()

  print("The 95% confidence intervals:")
  mcmc_out %>%
    filter(accept_flag) %>%
    select(-accept_flag, -l_hood) %>%
    summarize(across(.fns = quantile, probs = c(0.025, 0.50, 0.975))) %>%
    mutate(probs = c("2.5%", "50%", "97.5%")) %>%
    relocate(probs) %>%
    print()


  # Plot the estimates
  param_estimates <- mcmc_out %>%
    filter(accept_flag) %>%
    select(-accept_flag, -l_hood)

  GGally::ggpairs(data = param_estimates, diag = list(continuous = "barDiag", discrete = "barDiag", na = "naDiag")) %>%
    print()


  # Get the right hand side of your equations
  new_eq <- my_model %>%
    formula.tools::rhs()

  # Internal function to compute the model
  compute_model <- function(parameters, model_eq, mydata) {
    in_list <- c(parameters, mydata) %>% as.list()
    out_data <- eval(model_eq, envir = in_list)

    out_tibble <- mydata %>%
      mutate(model = out_data)
    return(out_tibble)
  }

  out_model <- mcmc_out %>%
    filter(accept_flag) %>%
    select(-accept_flag, -l_hood) %>%
    mutate(id = 1:n()) %>%
    group_by(id) %>%
    nest() %>%
    rename(in_params = data) %>%
    mutate(m_data = lapply(X = in_params, FUN = compute_model, new_eq, in_data)) %>%
    unnest(cols = c(m_data)) %>%
    ungroup() %>%
    select(-id, -in_params) %>%
    group_by(across(1)) %>%
    summarize(across(.cols = c("model"), .fns = quantile, probs = c(0.025, 0.50, 0.975))) %>%
    mutate(probs = c("q025", "q50", "q975")) %>%
    ungroup() %>%
    pivot_wider(names_from = "probs", values_from = "model")


  ggplot(out_model) +
    geom_line(aes_string(x = independent_var, y = "q50")) +
    geom_ribbon(aes_string(x = independent_var, ymin = "q025", ymax = "q975"), alpha = 0.3) +
    geom_point(data = in_data, aes_string(x = independent_var, y = names(in_data)[2]), color = "red", size = 2) +
    labs(y = "")


}
