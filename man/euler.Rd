% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/euler.R
\name{euler}
\alias{euler}
\title{Euler's method solution for a differential equation.}
\usage{
euler(
  system_eq,
  initial_condition,
  parameters = NULL,
  t_start = 0,
  deltaT = 1,
  n_steps = 1
)
}
\arguments{
\item{system_eq}{(REQUIRED) The 1 or multi dimensional system of equations, written in formula notation as a vector (i.e.  c(dx ~ f(x,y), dy ~ g(x,y)))}

\item{initial_condition}{(REQUIRED) Listing of initial conditions, as a vector}

\item{parameters}{The values of the parameters we are using (optional)}

\item{t_start}{The starting time point (defaults to t = 0)}

\item{deltaT}{The timestep length (defaults to 1)}

\item{n_steps}{The number of timesteps to compute solution (defaults to n_steps = 1)}
}
\value{
A tidy of data frame for the calculated solutions and the time
}
\description{
\code{euler} solves a multi-dimensional differential equation with Euler's method.  The parameters listed as required are needed
See the vignette for detailed examples of usage.
}
\examples{
# Define the rate equation:
lynx_hare_eq <- c(
 dHdt ~ r * H - b * H * L,
 dLdt ~ e * b * H * L - d * L
)

# Define the parameters (as a named vector):
lynx_hare_params <- c(r = 2, b = 0.5, e = 0.1, d = 1)

# Define the initial condition (as a named vector):
lynx_hare_init <- c(H = 1, L = 3)

# Define deltaT and the number of time steps:
deltaT <- 0.05
n_steps <- 200

# Compute the solution via Euler's method:
out_solution <- euler(system_eq = lynx_hare_eq,
                     parameters = lynx_hare_params,
                     initial_condition = lynx_hare_init,
                     deltaT = deltaT,
                     n_steps = n_steps
)
}
\seealso{
\code{\link{rk4}}
}
