#' Plot a simple algebraic equation with data
#'
#' \code{plotFunction_Data} plots an x-y relationship with data

#' @param x The independent variable
#' @param y The dependent variable
#' @param data two column of data to be plotted.
#' First column is independent variable. Second column dependent variable.
#' Must be a data.frame
#' @param x_label value for the x label axis
#' @param y_label value for the y label axis

#' @return A plot of your function
#' @examples
#' x<- 0:25
#' y<- x^2
#' data <- data.frame(Loblolly$age,Loblolly$height)
#' plotFunction_Data(x,y,data,'x','y')

#' @import ggplot2
#' @export



plotFunction_Data <- function(x,y,data,x_label='x',y_label='y') {



  inputData=data.frame(x=x,y=y);
  ### The function "melt" puts the data from a matrix format to a vector for easy grouping


  ### Do a line plot

  p <-   ggplot(inputData,aes(x=x,y=y)) +
    geom_line(size=1.0) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = x_label,y = y_label) +
    geom_point(data=data,aes(x=data[,1],y=data[,2]),color='red',size=2)

  return(p)







}
