#' Phase plane of differential equation.
#'
#' \code{phaseplane} visualizes the vector field for a one or two dimensional differential equation.
#' @param system_eq (REQUIRED) The 1 or 2 dimensional system of equations, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param x_var (REQUIRED) x axis variable (used in plot and formula)
#' @param y_var (REQUIRED) y axis variable (used in plot and formula)
#' @param x_window x axis limits.  Must be of the form c(minVal,maxVal).  Defaults to -4 to 4.
#' @param y_window y axis limits.  Must be of the form c(minVal,maxVal). Defaults to -4 to 4.
#' @param plot_points number of points we evaluate on the grid in both directions. Defaults to 10.
#' @param eq_soln TRUE / FALSE - lets you know if you want the code to estimate if there are any equilibrium solutions in the provided window
#' @return A phase plane diagram of system of differential equations
#' @examples
#' # For a two variable system of differential equations we use the formula notation for dx/dt and the dy/dt separately:
#' system_eq <- c(dx ~ cos(y),
#'               dy ~ sin(x))
#' phaseplane(system_eq,'x','y')
#'
#' # For a one dimensional system: dy/dt = f(t,y).  In this case the xWindow represents time.
#' # However, the code is structured a little differently.  Consider dy/dt = -y*(1-y):
#'
#' system_eq <- c(dt ~ 1,
#'                dy ~ -y*(1-y))
#'
#'  phaseplane(system_eq,"t","y")
#'
#' # Here is a second example for the differential equation dy/dt = -cos(t) y
#'
#' system_eq <- c(dt ~ 1,
#'                dy ~ -cos(t)*y
#'
#'  phaseplane(system_eq,"t","y")
#'
#' In this case there may be extraneous equilibrium solutions reported due to the time dependence in the differential equations - so be sure to verify your solutions!
#'
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @export




phaseplane <- function(system_eq,x_var,y_var,x_window=c(-4,4),y_window=c(-4,4),plot_points=10,eq_soln=FALSE)
{
  n_points = 2000 # Default number of grid points in each direction

  seq_by <- round(n_points/plot_points)
  # We will then figure out a regular sample of points to right size the window.
  # Define the grid for our solution
  in_grid <- expand.grid(x=seq(x_window[1],x_window[2],length.out=n_points),
                         y=seq(y_window[1],y_window[2],length.out=n_points)) %>%
    set_names(nm =c(x_var,y_var) )


  skip_vec = tibble(x=seq(x_window[1],x_window[2],length.out=n_points),
                    y=seq(y_window[1],y_window[2],length.out=n_points)) %>%
    slice(c(seq(1,n_points,by=seq_by),n_points)) %>%
    set_names(nm =c(x_var,y_var) )


  # Create a vector of arrows

  vec_field<- in_grid %>%
    map(.x=system_eq,.f=~eval(formula.tools::rhs(.x),envir = in_grid)) %>%
    set_names(nm =c("u","v") ) %>%
    bind_cols()


  p <- in_grid %>%
    cbind(vec_field) %>%
    mutate(lens=sqrt(u^2+v^2),  # Length of arrow
           lens2 = lens/max(lens)) # Scaling it by the maximum length


  maxx <- max(abs(p$u));  # The largest x and y vector.
  maxy <- max(abs(p$v));
  dt <- min( abs(diff(x_window))/maxx, abs(diff(y_window))/maxy)/plot_points;

  if(eq_soln){

    # Test if there are any equilibrium points.  If there are none in the range, it won't plot.
    if (formula.tools::rhs(system_eq)[[1]]==1) {
      eq_pts <- p %>% filter((between(v,-1e-3,1e-3))) %>%
        select(1,2) %>%  # Select the first two columns
        round(digits=1) %>%  # Round the digits for precision
        distinct()  # Check if they are unique


      eq_pts_print <- eq_pts %>%
        select(2) %>%  # Select the first two columns
        round(digits=1) %>%  # Round the digits for precision
        distinct()  # Check if they are unique

    } else {

      eq_pts <- p %>% filter(across(.cols=c("u","v"),.fns=~(between(.x,-1e-3,1e-3)))) %>%
        select(1,2) %>%  # Select the first two columns
        round(digits=1) %>%  # Round the digits for precision
        distinct()  # Check if they are unique

      eq_pts_print <- eq_pts
    }



    if(dim(eq_pts_print)[1]>0) {
      print("Possible equilbrium solutions at:")
      print(eq_pts_print)
      print("Be sure to confirm these equilibrium solutions in the differential equation.")

    } else {
      print("No equilibrium solutions were detected in the provided window. Please confirm with the differential equation")
    }


  }



  p_plot <- p %>% mutate(
    xend = .[[1]]  + dt*u/((lens2)+.1)/2,
    yend = .[[2]]  + dt*v/((lens2)+.1)/2,
    x_adj = .[[1]] - dt*u/((lens2)+.1)/2,
    y_adj= .[[2]]  -dt*v/((lens2)+.1)/2
  )



  # Now plot
  out_plot <- p_plot %>%
    filter(.[[1]] %in% skip_vec[[1]],
           .[[2]] %in% skip_vec[[2]]) %>%
    ggplot(aes(x=x_adj,y=y_adj)) +
    geom_segment(aes(xend = xend, yend = yend), arrow = arrow(length = unit(0.3,"cm")),lineend = 'butt')+
    xlab(x_var) +
    ylab(y_var)

  return(out_plot)

}
