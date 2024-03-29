% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_analyze.R
\name{mcmc_analyze}
\alias{mcmc_analyze}
\title{Markov Chain parameter estimates}
\usage{
mcmc_analyze(
  model,
  data,
  mcmc_out,
  mode = "emp",
  initial_condition = NULL,
  deltaT = NULL,
  n_steps = NULL,
  verbose = TRUE
)
}
\arguments{
\item{model}{the model equations that we use to compute the result.}

\item{data}{the data used to assess the model}

\item{mcmc_out}{A dataframe: the first column is the accept flag  of the mcmc run (TRUE/FALSE), the log likelihood, and the parameter values}

\item{mode}{two choices: emp --> empirical (default) or de --> differential equations. The estimator works differently depending on which is used.}

\item{initial_condition}{The initial condition for the differential equation (DE mode only)}

\item{deltaT}{The length between timesteps (DE mode only)}

\item{n_steps}{The number of time steps we run the model (DE mode only)}

\item{verbose}{TRUE / FALSE indicate if parameter estimates should be printed to console (option, defaults to TRUE)}
}
\value{
Two plots: (1) fitted model results compared to data, and (2) pairwise parameter histograms and scatterplots to test model equifinality.
}
\description{
\code{mcmc_analyze} Computes summary histograms and model-data comparisons from and Markov Chain Monte Carlo parameter estimate for a given model
}
\examples{
\donttest{
## Example with an empirical model:
## Step 1: Define the model and parameters
phos_model <- daphnia ~ c * algae^(1 / theta)

phos_param <- tibble::tibble( name = c("c", "theta"),
lower_bound = c(0, 1),
upper_bound = c(2, 20))

## Step 2: Determine MCMC settings
# Define the number of iterations
phos_iter <- 1000

## Step 3: Compute MCMC estimate
phos_mcmc <- mcmc_estimate(model = phos_model,
data = phosphorous,
parameters = phos_param,
iterations = phos_iter)

## Step 4: Analyze results:
mcmc_analyze(model = phos_model,
data = phosphorous,
mcmc_out = phos_mcmc)

## Example with a differential equation:
## Step 1: Define the model, parameters, and data
## Define the tourism model
tourism_model <- c(dRdt ~ resources * (1 - resources) - a * visitors,
dVdt ~ b * visitors * (resources - visitors))

# Define the parameters that you will use with their bounds
tourism_param <- tibble::tibble( name = c("a", "b"),
lower_bound = c(10, 0),
upper_bound = c(30, 5))

## Step 2: Determine MCMC settings
# Define the initial conditions
tourism_init <- c(resources = 0.995, visitors = 0.00167)
deltaT <- .1 # timestep length
n_steps <- 15 # must be a number greater than 1
# Define the number of iterations
tourism_iter <- 1000

## Step 3: Compute MCMC estimate
tourism_out <- mcmc_estimate(
 model = tourism_model,
 data = parks,
 parameters = tourism_param,
 mode = "de",
 initial_condition = tourism_init, deltaT = deltaT,
 n_steps = n_steps,
 iterations = tourism_iter)

## Step 4: Analyze results
mcmc_analyze(
 model = tourism_model,
 data = parks,
 mcmc_out = tourism_out,
 mode = "de",
 initial_condition = tourism_init, deltaT = deltaT,
 n_steps = n_steps
)

}
}
\seealso{
\code{\link{mcmc_estimate}}
}
