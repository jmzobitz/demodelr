#' Likelihood plot of a two parameter model
#'
#' \code{plotLikelihood} makes a likelihood plot of a two parameter model.
#'
#' @param data two column of data to be plotted.
#' First column is independent variable. Second column dependent variable.
#' Must be a data.frame
#' @param model a function or model of our situation
#' @param parameters The values of the parameters we are using
#' @param logLikely Do we compute the log likelihood function (default is FALSE)
#' @return A plot of your likelihood function, with optimum value identified
#' @examples
#' # Run the vignette that works through an example
#' vignette("likelihood")
#'
#' @import ggplot2
#' @import dplyr
#' @export



plotLikelihood <-function(model,data,parameters,logLikely=FALSE) {

  likelihood <- function(y,indata,logLikely=FALSE){
    error = sd(indata-y)
    singlelikelihoods = dnorm(indata, mean = y, sd = error, log = logLikely)

    if (logLikely) {
      return(-sum(singlelikelihoods))
    } else {
      return(prod(singlelikelihoods))
    }


  }


  lValues <- parameters %>%
    apply(1,model, data[[1]]) %>%  # Apply the logistic function to each of the times (the rows of param)
    apply(2,likelihood,data[[2]],logLikely) %>%  # Apply the likelihood function to each of the columns
    data.frame(l_hood=.) %>%
    cbind(parameters)

  if (logLikely) {
    optValue <- lValues %>% arrange((l_hood)) %>% head(n=1)
  }
  else {
    optValue <- lValues %>% arrange(desc(l_hood)) %>% head(n=1)
  }

  print(optValue)
  lValues %>%
    ggplot(aes(x=parameters[[1]], y=parameters[[2]], z = l_hood))+
    geom_tile(aes(fill = l_hood)) + stat_contour()+
    geom_point(data=optValue,aes(x=optValue[[2]],y=optValue[[3]]),size=4,color="red") +
    labs(title="Contour Plot of Likelihood Function",
         x=names(parameters)[1],
         y=names(parameters)[2],
         fill="Likelihood")




}




