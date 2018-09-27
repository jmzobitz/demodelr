#' Plot a histogram of a dataset
#'
#' \code{plotHistogram} plots a histogram of a dataset

#' @param data two column of data to be plotted.
#' First column is independent variable. Second column dependent variable.
#' Must be a data.frame
#' @param bins number of bins you wish for the data
#' @param x_label value for the x label axis
#' @return A plot of your function
#' @examples
#' data <- data.frame(Loblolly$height)
#' plotHistogram(data,10,'height')

#' @import ggplot2
#' @export



plotHistogram <- function(data,bins=10,x_label='x') {

  names(data) <-c("x")  # Rename for ease of use

  ### Do a line plot

  p <- ggplot() +  geom_histogram(data=data,aes(x=x),bins=bins) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = x_label,title="Bootstrap Estimate")

return(p)







}
