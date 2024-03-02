#' Phase plane of differential equation.
#'
#' \code{phaseplane} visualizes the vector field for a one or two dimensional differential equation.
#' @param system_eq (required) The 1 or 2 dimensional system of equations, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))
#' @param x_var (required) x axis variable (used to create the plot and label axes)
#' @param y_var (required) y axis variable (used to create the plot and label axes)
#' @param parameters (optional) any parameters in the system of equations
#' @param x_window (optional) x axis limits.  Must be of the form c(minVal,maxVal).  Defaults to -4 to 4.
#' @param y_window (optional) y axis limits.  Must be of the form c(minVal,maxVal). Defaults to -4 to 4.
#' @param plot_points (optional) number of points we evaluate on the grid in both directions. Defaults to 10.
#' @param eq_soln (optional) TRUE / FALSE - lets you know if you want the code to estimate if there are any equilibrium solutions in the provided window. This will print out the equilibrium solutions to the console.
#'
#' @return A phase plane diagram of system of differential equations
#'
#' @examples
#' # For a two variable system of differential equations we use the
#' # formula notation for dx/dt and the dy/dt separately:
#' system_eq <- c(dx ~ cos(y),
#'               dy ~ sin(x))
#' phaseplane(system_eq,x_var='x',y_var='y')
#'
#' # For a one dimensional system: dy/dt = f(t,y).  In this case the
#' # xWindow represents time.
#' # However, the code is structured a little differently.
#' # Consider dy/dt = -y*(1-y):
#'
#' system_eq <- c(dt ~ 1,
#'                dy ~ -y*(1-y))
#'
#'  phaseplane(system_eq,x_var="t",y_var="y")
#' \donttest{
#' # Here is an example to find equilibrium solutions.
#'
#'  system_eq <- c(dx ~ y+x,
#'                dy ~ x-y)
#'
#'  phaseplane(system_eq,x_var='x',y_var='y',eq_soln=TRUE)
#'
#' # We would expect an equilibrium at the origin,
#' # but no equilibrium solution was found, but if we narrow the search range:
#'
#'  phaseplane(system_eq,x_var='x',y_var='y',x_window = c(-0.1,0.1),y_window=c(-0.1,0.1),eq_soln=TRUE)
#'
#' # Confirm any equilbrium solutions through direct evaluation of the differential equation.
#'
#' }
#'
#' @importFrom rlang .data
#' @importFrom utils capture.output
#' @importFrom utils head
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @export




phaseplane <- function(system_eq,x_var,y_var,parameters=NULL,x_window=c(-4,4),y_window=c(-4,4),plot_points=10,eq_soln=FALSE)
{
  n_points = 2000 # Default number of grid points in each direction

  seq_by <- round(n_points/plot_points)
  # We will then figure out a regular sample of points to right size the window.
  # Define the grid for our solution
  in_grid <- tidyr::expand_grid(x=seq(x_window[1],x_window[2],length.out=n_points),
                         y=seq(y_window[1],y_window[2],length.out=n_points)) %>%
    purrr::set_names(nm =c(x_var,y_var) )


  skip_vec = tibble(x=seq(x_window[1],x_window[2],length.out=n_points),
                    y=seq(y_window[1],y_window[2],length.out=n_points)) %>%
    slice(c(seq(1,n_points,by=seq_by),n_points)) %>%
    purrr::set_names(nm =c(x_var,y_var) )


  # Create a vector of arrows

  # Define the list of inputs to the rate equation
  in_list <- c(parameters,in_grid) %>% as.list()

  new_rate_eq <- system_eq %>%
    formula.tools::rhs()

  vec_field <-sapply(new_rate_eq,FUN=eval,envir=in_list) %>%
    as_tibble(.name_repair = make.names) %>%
    purrr::set_names(nm =c("u","v") ) %>%
    bind_cols()

  p <- in_grid %>%
    cbind(vec_field) %>%
    mutate(lens=sqrt(.data$u^2+.data$v^2),  # Length of arrow
           lens2 = .data$lens/max(.data$lens)) # Scaling it by the maximum length


  maxx <- max(abs(p$u));  # The largest x and y vector.
  maxy <- max(abs(p$v));
  dt <- min( abs(diff(x_window))/maxx, abs(diff(y_window))/maxy)/plot_points;

  if(eq_soln){

    # Test if there are any equilibrium points.  If there are none in the range, it won't plot.
    if (formula.tools::rhs(system_eq)[[1]]==1) {
      eq_pts <- p %>%
        dplyr::filter((between(.data$v,-1e-3,1e-3))) %>%
        dplyr::select(1,2) %>%  # Select the first two columns
        round(digits=1) %>%  # Round the digits for precision
        dplyr::distinct()  # Check if they are unique


      eq_pts_print <- eq_pts %>%
        dplyr::select(1,2) %>%  # Select the first two columns
        round(digits=1) %>%  # Round the digits for precision
        dplyr::distinct()  # Check if they are unique

    } else {

      eq_pts <- p %>%
        dplyr::filter(if_all(.cols=c("u","v"),.fns=~(between(.x,-1e-3,1e-3)))) %>%
        dplyr::select(1,2) %>%  # Select the first two columns
        round(digits=1) %>%  # Round the digits for precision
        dplyr::distinct()  # Check if they are unique

      eq_pts_print <- eq_pts

    }



    if(dim(eq_pts_print)[1]>0) {

      message("Possible equilibrium solutions at:")

      out_message <- paste(utils::capture.output(utils::head(eq_pts_print, dim(eq_pts_print)[1])), collapse="\n")

      message(out_message)

      message("Be sure to confirm these equilibrium solutions in the differential equation.")

    } else {
      message("No equilibrium solutions were detected in the provided window. Please confirm with the differential equation")
    }


  }

col_names <- names(p)

names(p) <- c("curr_vec_x","curr_vec_y",col_names[-(1:2)])

  p_plot <- p %>%
    mutate(
    xend = .data$curr_vec_x  + dt*.data$u/((.data$lens2)+.1)/2,
    yend = .data$curr_vec_y  + dt*.data$v/((.data$lens2)+.1)/2,
    x_adj = .data$curr_vec_x - dt*.data$u/((.data$lens2)+.1)/2,
    y_adj= .data$curr_vec_y  -dt*.data$v/((.data$lens2)+.1)/2
  )

   # Now plot
   out_plot <- p_plot %>%
     dplyr::filter(.data$curr_vec_x %in% skip_vec[[1]],
            .data$curr_vec_y %in% skip_vec[[2]]) %>%
     ggplot2::ggplot(aes(x=.data$x_adj,y=.data$y_adj)) +
     geom_segment(aes(xend = .data$xend, yend = .data$yend), arrow = arrow(length = unit(0.3,"cm")),lineend = 'butt')+
     xlab(x_var) +
     ylab(y_var)

  return(out_plot)

}
