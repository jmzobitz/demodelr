#' Plot a simple algebraic equation with data
#'
#' \code{plotData} plots an x-y relationship with data

#' @param data two column of data to be plotted.
#' First column is independent variable. Second column dependent variable.
#' Must be a data.frame
#' @param x_label value for the x label axis
#' @param y_label value for the y label axis
#' @return A plot of your function
#' @examples
#' data <- data.frame(Loblolly$age,Loblolly$height)
#' plotData(data,'age','height')

#' @import ggplot2
#' @export



plotData <- function(data,x_label='x',y_label='y') {



  ### Do a line plot

  p <- ggplot() +  geom_point(data=data,aes(x=data[,1],y=data[,2]),color='red',size=2) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = x_label,y = y_label)


  return(p)







}
