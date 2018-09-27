#'
#' \code{bootstrap_model} computes a given number of bootstrap samples, computes a linear model, and then plots .


#' @param data two column of data to be plotted.
#' First column is independent variable. Second column dependent variable.
#' @param regression_formula function you use to fit the data
#' @param n number of bootstrap samples
#' @param x_label value for the x label axis
#' @param y_label value for the y label axis


#' @source Code is adapted from: \url{https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html}
#'
#' @examples
#'
#' # Identify the name of the data set you wish to run a regression on and labels
#' for the axis.
#' my_data <- data.frame(age=Loblolly$age,height=Loblolly$height)

#'
#' #' Identify the basic formula of the regression and plotting formula. Make sure you have the same names of the columns in your data.
#' regression_formula = height ~ 1 + age+ I(age^2)
#'
#' bootstrap_model(my_data,regression_formula,n=100,'Height','Age')

#' @import ggplot2
#' @import modelr
#' @import broom
#' @export


bootstrap_model <- function(data,regression_formula,n=100,x_label='x',y_label='y') {

  # Determine the linear fit according to your regression formula and print the summary
  fit=lm(regression_formula, data = data)
  print(summary(fit))

  # Get the smoothed prediction
  smooth_data <- data.frame(x=data[[1]],y=predict(fit))

  # Generate a bootstrap estimate
  boot_models <- modelr::bootstrap(data, n) %>%
    mutate(model=map(strap, ~ lm(regression_formula, data = .)),
           coef_info=map(model,tidy))

  # Get the coefficients for the bootstraps
  boot_coefs <- boot_models %>%
    unnest(coef_info)

  # Tidied up version of the models and their results
  boot_aug <- boot_models %>%
    mutate(augmented = map(model, augment)) %>%
    unnest(augmented)

  ### Print confidence intervals
  boot_coefs %>%
    group_by(term) %>%
    summarize("0.025%" = quantile(estimate, 0.025),
              "50%" = quantile(estimate, 0.5),
              "97.5%" = quantile(estimate, 0.975)) %>%
    print()


  ### Histogram of parameters
  histPlot <- ggplot(boot_coefs, aes(estimate)) +
    geom_histogram() +
    facet_grid(.~term, scales = "free")  +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(title="Bootstrap Estimate of Parameters")

  print(histPlot)


  # Rename for ease of use
  names(boot_aug)[names(boot_aug) %in% names(myData)[1]] <- "x"
  names(boot_aug)[names(boot_aug) %in% names(myData)[2]] <- "y"


  ### Plot of data with best fit line
  p<- ggplot(boot_aug, aes(x=x, y=y)) +
    geom_point(color='red',size=2) +
    labs(x=x_label,y=y_label) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    geom_line(aes(y = .fitted, group = .id), alpha=.2) +
    geom_line(data=smooth_data,aes(x=x,y=y),color='blue',size=1,linetype='dashed')

  print(p)

}


