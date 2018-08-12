### Code for plotting a simple function of x and y in R
### Author: JMZ
### Modified: 12/15/15

### Inputs:
### x: the independent variable
### y: the dependent variable
### xlabel: value for the x label axis
### ylabel: value for the y label axis

### Output: a plot to the console of the relationship, joined as a smooth line.
### Assumes that the equation has random noise in the transmissing

#' @import ggplot2
#' @export
plotFunction <- function(x,y,xLabel='x',yLabel='y') {



  inputData=data.frame(x=x,y=y);
  ### The function "melt" puts the data from a matrix format to a vector for easy grouping


  ### Do a line plot
  print (
    ggplot(inputData,aes(x=x,y=y)) +
      geom_line(size=1.0) +
      theme(plot.title = element_text(size=20),
            axis.title.x=element_text(size=20),
            axis.text.x=element_text(size=15),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=20)) +
      labs(x = xLabel,y = yLabel)
  )







}
