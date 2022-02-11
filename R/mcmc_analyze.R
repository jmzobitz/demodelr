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
#'
#' @import GGally
#' @import dplyr
#' @import ggplot2
#' @export




mcmc_analyze <- function(model, data, mcmc_out,mode="emp",initial_condition = NULL,deltaT = NULL,n_steps=NULL) {

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

  print("The 95% confidence intervals for the parameters:")
  mcmc_out %>%
    filter(accept_flag) %>%
    select(-accept_flag, -l_hood) %>%
    summarize(across(.fns = quantile, probs = c(0.025, 0.50, 0.975))) %>%
    mutate(probs = c("2.5%", "50%", "97.5%")) %>%
    dplyr::relocate(probs) %>%
    print()


  # Plot the estimates
  param_estimates <- mcmc_out %>%
    filter(accept_flag) %>%
    select(-accept_flag, -l_hood)

  GGally::ggpairs(data = param_estimates, diag = list(continuous = "barDiag", discrete = "barDiag", na = "naDiag")) %>%
    print()

  if (mode == "emp") {
    # Get the right hand side of your equations
    new_eq <- model %>%
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
      select(-id, -in_params)

    glimpse(out_model)
    ggplot(out_model) +
      geom_boxplot(aes_string(x = independent_var, y = "model",group = independent_var)) +
      geom_point(data = in_data, aes_string(x = independent_var, y = names(in_data)[2]), color = "red", size = 2,shape = 15) +
      labs(y = names(in_data)[2])


  } else if (mode == "de") {
    ### THIS IS MODE DEPENDENT (EMPIRICAL OR DIFFERENTIAL EQUATION)
    # Get the right hand side of your equations
    # new_eq <- my_model %>%
    #   formula.tools::rhs()
    #
    # # Internal function to compute the model
    compute_model <- function(parameters, model,initial_condition, deltaT,n_steps) {

      out_data <- rk4(model,
                      parameters = parameters,
                      initial_condition=initial_condition,
                      deltaT=deltaT,
                      n_steps = n_steps) %>%
        pivot_longer(cols=c(-"t")) %>%
        rename(model=value)


      return(out_data)
    }


    out_model <- mcmc_out %>%
      filter(accept_flag) %>%
      select(-accept_flag, -l_hood) %>%
      mutate(id = 1:n()) %>%
      group_by(id) %>%
      nest() %>%
      rename(in_params = data) %>%
      mutate(m_data = lapply(X = in_params, FUN = compute_model, model, initial_condition,deltaT,n_steps)) %>%
      unnest(cols = c(m_data)) %>%
      ungroup() %>%
      select(-id, -in_params) %>%
      group_by(across(c(1,2))) %>%
      summarize(across(.cols = c("model"), .fns = quantile, probs = c(0.025, 0.50, 0.975))) %>%
      mutate(probs = c("q025", "q50", "q975")) %>%
      ungroup() %>%
      pivot_wider(names_from = "probs", values_from = "model")


    # Mutate the data in a long format for comparison
    data_long <- data %>%
      pivot_longer(cols=c(-1)) %>%
      rename(t=1)

    ggplot(out_model) +
      geom_line(aes(x = t, y = q50)) +
      geom_ribbon(aes(x = t, ymin = q025, ymax = q975), alpha = 0.3) +
      geom_point(data = data_long, aes(x = t, y = value), color = "red", size = 2) +
      labs(y = "") + facet_grid(name~.,scales="free_y")

  }


}
