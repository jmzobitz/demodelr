#' Plot a simple algebraic equation
#'
#' \code{plotFunction} plots an x-y relationship of data
#' See the vignette for detailed examples of usage.

#' @param x The independent variable
#' @param y The dependent variable
#' @param x_label value for the x label axis
#' @param y_label value for the y label axis
#' @return A plot of your function
#' @examples
#' x<- 1:10
#' y<- x^2
#' plotFunction(x,y,'x','y')

#' @import ggplot2
#' @export



plotFunction <- function(x,y,x_label='x',y_label='y') {



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
      labs(x = x_label,y = y_label)

 return(p)







}
