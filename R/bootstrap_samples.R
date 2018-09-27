#'
#' \code{bootstrap_samples} computes a given number of bootstrap samples and computes the distribution of bootstrap results.  The resulting histogram of samples is shown, along with a sampling of the results


#' @param data one column of data to take bootstrap samples of.
#' @param n number of bootstrap samples
#' @param x_label value for the x label axis


#' @source Code is adapted from: \url{https://cran.r-project.org/web/packages/broom/vignettes/bootstrapping.html}
#'
#' @examples
#'
#' # Identify the name of the data set you wish to run a regression on and labels
#' for the axis.
#' my_data <- data.frame(precipitation$precip)

#' Compute the bootstrap samples
#'
#' bootstrap_samples(my_data,n=100,'Precipitation')

#' @import ggplot2
#' @import modelr
#' @import broom
#' @export


bootstrap_samples <- function(data,n=100,x_label='x') {


  # Generate a bootstrap estimate
  boot_models <- modelr::bootstrap(data, n)

  mean_calc <- boot_models$strap %>% as.data.frame() %>% map_df(mean,na.rm=TRUE) %>% t() %>% as.data.frame()
  print('Mean confidence interval:')
  print(quantile(mean_calc[[1]],probs=(c(0.025,0.5,0.975))))

  sd_calc <- boot_models$strap %>% as.data.frame() %>% map_df(sd,na.rm=TRUE) %>% t() %>% as.data.frame()
  print('Standard deviation confidence interval:')
  print(quantile(sd_calc[[1]],probs=(c(0.025,0.5,0.975))))


  p1 <- mean_calc %>% ggplot(aes(x=V1)) + geom_histogram() + xlab(x_label) + labs(title="Bootstrap estimate of mean")

  print(p1)

  p2 <- sd_calc %>% ggplot(aes(x=V1)) + geom_histogram() + xlab(x_label) + labs(title="Bootstrap estimate of standard deviation")

  print(p2)


}


