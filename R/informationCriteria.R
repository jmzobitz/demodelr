#'
#' \code{informationCriteria} fits a functional relationship to data, summarizes the results, makes a plot of the fitted formula, and reports values needed to compute the AIC and the BIC.


#' @param data two column of data to be plotted.
#' First column is independent variable. Second column dependent variable.
#' @param regression_formula function you use to fit the data
#' @param x_label value for the x label axis
#' @param y_label value for the y label axis

#' @examples
#' # Identify the name of the data set you wish to run a regression on and labels
#' for the axis.
#' my_data <- data.frame(age=Loblolly$age,height=Loblolly$height)

#'
#' #' Identify the basic formula of the regression and plotting formula. Make sure you have the same names of the columns in your data.
#' regression_formula = height ~ 1 + age+ I(age^2)
#'
#' informationCriteria(my_data,regression_formula,'Age','Height')

#' @import ggplot2
#' @export


informationCriteria <- function(data,regression_formula,x_label='x',y_label='y') {

  # Determine the linear fit according to your regression formula and print the summary
  fit=lm(regression_formula, data = data)
  print(summary(fit))

  nData = length(data[[2]])
  nParams = length(fit$coefficients)
  print(paste0('Number of parameters (P): ',nParams))
  print(paste0('Number of data points (N): ',nData))
  print(logLik(fit))


  smooth_data <- data.frame(x=data[[1]],y=predict(fit))

  p <-ggplot(data=data,aes(x=data[[1]],y=data[[2]])) +
    geom_point(color='red',size=2) +
    geom_line(data=smooth_data,aes(x=x,y=y),size=1.0)+
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    # stat_smooth(method = "lm", col = "black", formula = regression_formula) +
    labs(x = x_label,y = y_label)

  return(p)




}


