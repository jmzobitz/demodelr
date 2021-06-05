#' Likelihood plot of a two parameter model
#'
#' \code{compute_likelihood} computes the likelihood for a model
#'
#' @param model a function or model of our situation, written with formula notation
#' @param data Data frame of data
#' First column is the independent variable, second column dependent variable.
#' Must be a data.frame
#' @param parameters The data frame matrix of values of the parameters we are using.  This will be made using expand.grid or equivalent
#' @param logLikely Do we compute the log likelihood function (default is FALSE)
#' @return A list with two entries: (1) the likelihood values and (2) values of parameters that optimize the likelihood.

#'
#' @import ggplot2
#' @import dplyr
#' @export



compute_likelihood <-function(model,data,parameters,logLikely=FALSE) {

  in_data <- data  # Rename so the function works

  # Get the right hand side of your equations
  new_eq <-model %>%
    formula.tools::rhs()

  # Internal function to compute the likelihood
  likelihood <- function(y,mydata,logLikely){
    error = sd(mydata-y)
    singlelikelihoods = dnorm(mydata, mean = y, sd = error, log = logLikely)

    if (logLikely) {
      return(-sum(singlelikelihoods))
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
    mutate(id=1:n()) %>%
    group_by(id) %>%
    nest() %>%
    rename(in_params=data) %>%
    mutate(m_data = lapply(X=in_params,FUN=compute_model, new_eq,in_data),
           l_hood = lapply(X=m_data,FUN=likelihood,in_data[[2]],logLikely)

    ) %>%
    unnest(cols=c(in_params,l_hood) )%>%
    ungroup() %>%
    select(-id,-m_data) %>%
    mutate(log_lik=logLikely)


  if (logLikely) {   # If we want the log likelihood, we minimize
    optValue <- out_likelihood %>%
      arrange(l_hood) %>% head(n=1) %>%
      mutate(log_lik=logLikely)
  }
  else {   # If not we maximize
    optValue <- out_likelihood %>%
      arrange(desc(l_hood)) %>% head(n=1) %>%
      mutate(log_lik=logLikely)
  }



  return(list(likelihood=out_likelihood,opt_value=optValue))
}




