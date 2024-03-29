% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_likelihood.R
\name{compute_likelihood}
\alias{compute_likelihood}
\title{Likelihood plot of a two parameter model}
\usage{
compute_likelihood(model, data, parameters, logLikely = FALSE)
}
\arguments{
\item{model}{a function or model of our situation, written with formula notation}

\item{data}{Data frame of data
First column is the independent variable, second column dependent variable.
Must be a data.frame}

\item{parameters}{The data frame matrix of values of the parameters we are using.  This will be made using expand.grid or equivalent}

\item{logLikely}{Do we compute the log likelihood function (default is FALSE).  NOTE: what gets returned is - logLikely - meaning that this will be a positive number to work with.}
}
\value{
A list with two entries: (1) the likelihood values and (2) values of parameters that optimize the likelihood.
}
\description{
\code{compute_likelihood} computes the likelihood for a model
}
\examples{
### Contour plot of a logistic model for two parameters K and b
### using data collected from growth of yeast population

# Define the solution to the differential equation with
# parameters K and b Gause model equation
gause_model <- volume ~ K / (1 + exp(log(K / 0.45 - 1) - b * time))
# Identify the ranges of the parameters that we wish to investigate
kParam <- seq(5, 20, length.out = 100)
bParam <- seq(0, 1, length.out = 100)
# Allow for all the possible combinations of parameters
gause_parameters <- expand.grid(K = kParam, b = bParam)
# Now compute the likelihood
gause_likelihood <- compute_likelihood( model = gause_model,
                                       data = yeast,
                                       parameters = gause_parameters,
                                       logLikely = FALSE
)
}
