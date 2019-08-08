#' Phase plane of differential equation.
#'
#' \code{phaseplane} visualizes the vector field for a one or two dimensional differential equation.
#'
#' @param n_points number of points we evaluate on the grid in both directions
#' @param x_window x axis limits.  Must be of the form c(minVal,maxVal)
#' @param y_window y axis limits.  Must be of the form c(minVal,maxVal)
#' @param x_label x axis label for plot
#' @param y_label y axis label for plot
#' @param initialCondition Listing of initial conditions
#' @param dx a function for the dx/dt = f(x,y)
#' @param dy a function for the dy/dt = f(x,y)
#' @return A phase plane diagram of system of differential equations
#' @examples
#' # For a two variable system of differential equations we define the dx/dt and the dy/dt separately:
#' dx <- function(x,y) {
#' cos(x)
#' }
#'
#' dy <- function(x,y) {
#'  sin(y)
#' }
#'
#' phaseplane(20,c(-3,3),c(-3,3),'X','Y',dx,dy)
#'
#' # For a one dimensional system: dy/dt = f(t,y).  In this case the xWindow represents time.
#' # However, we still need to write dx as a function of x and y, but the plot can be labeled T
#'
#' dy <- function(x,y) {
#' -y
#' }
#'
#' phaseplane(20,c(-3,3),c(-3,3),'T','Y',1,dy)
#'
#' # Here is a second example for the differential equation dy/dt = -cos(t) y
#' # In this case, the x coordinate represents time.
#'
#' dy <- function(x,y) {
#' -cos(x)*y
#' }
#'
#' phaseplane(20,c(-3,3),c(-3,3),'T','Y',1,dy)
#'
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @export




phaseplane <- function(n_points,x_window,y_window,x_label,y_label,dx,dy) {

  # Define the grid for our solution
  in_grid <- expand.grid(x=seq(x_window[1],x_window[2],length.out=n_points),
                         y=seq(y_window[1],y_window[2],length.out=n_points))
  names(in_grid)<-c(x_label,y_label)
  p<- in_grid %>%
    mutate(u=pmap_dbl(in_grid,dx),v=pmap_dbl(in_grid,dy))

  out_plot <- p %>%
    ggplot(aes_string(x=colnames(p)[1], y=colnames(p)[2],u=colnames(p)[3],v=colnames(p)[4])) +
    geom_quiver() +
    xlab(x_label) +
    ylab(y_label)

  return(out_plot)

}

