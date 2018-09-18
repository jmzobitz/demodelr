#'
#' \code{plotRegression_Data} fits a functional relationship to data, summarizes the results, and makes a plot of the fitted formula.


#' @param data two column of data to be plotted.
#' First column is independent variable. Second column dependent variable.
#' Must be a data.frame, labeled as x and y.
#' @param regression_formula function you use to fit the data
#' @param x_label value for the x label axis
#' @param y_label value for the y label axis

#' @examples
#' # Identify the name of the data set you wish to run a regression on and labels
#' for the axis.  You must label the columns as x and y.

#'
#' #' Identify the basic formula of the regression and plotting formula. Make sure you have the same names of the columns in your data.
#' regression_formula = height ~ 1 + age+ I(age^2)
#'
#' plotRegression_Data(Loblolly,regression_formula,'Height','Age')

#' @import ggplot2
#' @export


plotRegression_Data <- function(data,regression_formula,x_label='x',y_label='y') {

  # Determine the linear fit according to your regression formula and print the summary
  fit=lm(regression_formula, data = data)
  print(summary(fit))

  p <-ggplot(data=data,aes(x=data[[1]],y=data[[2]])) +
    geom_point(color='red',size=2) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    stat_smooth(method = "lm", col = "black", formula = regression_formula) +
    labs(x = x_label,y = y_label)

  return(p)

}


