#' Likelihood plot of a two parameter model
#'
#' \code{compute_likelihood} computes the likelihood for a model
#'
#' @param model a function or model of our situation, written with formula notation
#' @param data Data frame of data
#' First column is the independent variable, second column dependent variable.
#' Must be a data.frame
#' @param parameters The data frame matrix of values of the parameters we are using.  This will be made using expand.grid or equivalent
#' @param logLikely Do we compute the log likelihood function (default is FALSE).  NOTE: what gets returned is - logLikely - meaning that this will be a positive number to work with.
#' @return A list with two entries: (1) the likelihood values and (2) values of parameters that optimize the likelihood.

#' @examples
#' ### Contour plot of a logistic model for two parameters K and b
#' ### using data collected from growth of yeast population
#'
#' # Define the solution to the differential equation with
#' # parameters K and b Gause model equation
#' gause_model <- volume ~ K / (1 + exp(log(K / 0.45 - 1) - b * time))
#' # Identify the ranges of the parameters that we wish to investigate
#' kParam <- seq(5, 20, length.out = 100)
#' bParam <- seq(0, 1, length.out = 100)
#' # Allow for all the possible combinations of parameters
#' gause_parameters <- expand.grid(K = kParam, b = bParam)
#' # Now compute the likelihood
#' gause_likelihood <- compute_likelihood( model = gause_model,
#'                                        data = yeast,
#'                                        parameters = gause_parameters,
#'                                        logLikely = FALSE
#' )


#' @importFrom rlang .data
#' @importFrom stats sd dnorm
#' @importFrom utils head
#' @import formula.tools
#' @import dplyr
#' @import tidyr
#' @export



compute_likelihood <-function(model,data,parameters,logLikely=FALSE) {

  in_data <- data  # Rename so the function works

  # Get the right hand side of your equations
  new_eq <-model %>%
    formula.tools::rhs()

  # Internal function to compute the likelihood
  likelihood <- function(y,mydata,logLikely){
    error = stats::sd(mydata-y)
    singlelikelihoods = stats::dnorm(mydata, mean = y, sd = error, log = logLikely)

    if (logLikely) {
      return(-sum(singlelikelihoods))  # Here we make the log likelihood positive.
    } else {
      return(prod(singlelikelihoods))
    }


  }

  # Internal function to compute the model
  compute_model <- function(parameters,model_eq,mydata) {
    in_list <- c(parameters,mydata) %>% as.list()
    out_data <- eval(model_eq,envir=in_list)

    return(out_data)
  }


  # We will map along the data frame of parameters
  out_likelihood <- parameters %>%
    dplyr::mutate(id=1:n()) %>%
    dplyr::group_by(id) %>%
    tidyr::nest() %>%
    dplyr::rename(in_params=data) %>%
    dplyr::mutate(m_data = lapply(X=.data$in_params,FUN=compute_model, new_eq,in_data),
           l_hood = lapply(X=.data$m_data,FUN=likelihood,in_data[[2]],logLikely)

    ) %>%
    tidyr::unnest(cols=c(.data$in_params,.data$l_hood) )%>%
    dplyr::ungroup() %>%
    dplyr::select(-id,-.data$m_data) %>%
    dplyr::mutate(log_lik=logLikely)


  if (logLikely) {   # If we want the log likelihood, we minimize
    optValue <- out_likelihood %>%
      dplyr::arrange(.data$l_hood) %>%
      utils::head(n=1) %>%
      dplyr::mutate(log_lik=logLikely)
  }
  else {   # If not we maximize
    optValue <- out_likelihood %>%
      dplyr::arrange(desc(.data$l_hood)) %>%
      utils::head(n=1) %>%
      dplyr::mutate(log_lik=logLikely)
  }



  return(list(likelihood=out_likelihood,opt_value=optValue))
}




