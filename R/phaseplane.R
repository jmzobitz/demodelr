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
#' phaseplane(20,c(-3,3),c(-3,3),'x','y',dx,dy)
#'
#' # For a one dimensional system: dy/dx = f(x,y).  In this case the xWindow represents time.
#' # However, the first function looks a little different.
#'
#' dx <- function(x,y) {
#' 1
#' }
#'
#' dy <- function(x,y) {
#' -y
#' }
#'
#' phaseplane(20,c(-3,3),c(-3,3),'x','y',dx,dy)
#'
#' # Here is a second example for the differential equation dy/dx = -cos(x) y
#'
#'dx <- function(x,y) {
#' 1
#' }
#' dy <- function(x,y) {
#' -cos(x)*y
#' }
#'
#' phaseplane(20,c(-3,3),c(-3,3),'x','y',dx,dy)
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
    mutate(u=pmap_dbl(in_grid,dx),v=pmap_dbl(in_grid,dy),
           lens=sqrt(u^2+v^2),
           lens2 = lens/max(lens))

  maxx <- max(abs(p$u));
  maxy <- max(abs(p$v));
  dt <- min( abs(diff(x_window))/maxx, abs(diff(y_window))/maxy)/n_points;
  print(names(p))
  p <- p %>% mutate(
    xend = .[[1]]  + dt*u/((lens2)+.1)/2,
    yend = .[[2]]  + dt*v/((lens2)+.1)/2,
    x = .[[1]] - dt*u/((lens2)+.1)/2,
    y= .[[2]]  -dt*v/((lens2)+.1)/2
  )

  print(p)
  out_plot <- p %>%
    ggplot(aes_string(x=colnames(p)[1], y=colnames(p)[2],u=colnames(p)[3],v=colnames(p)[4])) +
    #geom_quiver(center=TRUE) +
    geom_segment(aes(xend = xend, yend = yend), arrow = arrow(length = unit(0.3,"cm")),lineend = 'butt')+
    xlab(x_label) +
    ylab(y_label)

  return(out_plot)

}
