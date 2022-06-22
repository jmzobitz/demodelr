## Resubmission 21-06-22
This is a re-resubmission.
There were no ERRORs or WARNINGs or NOTEs

My apologies - I didn't realize that my comments file was entirely completed. Please ignore my previous submission.

I have revised the Description in the DESCRIPTION file to remove any extraneous spaces, to the best of my ability. The reference to the online text is now written in the form ".. "Exploring modeling with data and differential equations
     using R" by John Zobitz (2021) <https://jmzobitz.github.io/ModelingWithR/index.html>"
     
## Resubmission 17-06-22
This is a resubmission.
There were no ERRORs or WARNINGs or NOTEs

Thank you for providing more information on how to structure my DESCRIPTION file. Based on the previous review I have created a more extensive Description as written below:

  Designed to support the visualization, numerical computation, 
    qualitative analysis, model-data fusion, and stochastic simulation for 
    autonomous systems of differential equations. Euler and Runge-Kutta 
    methods are implemented, along with tools to visualize the two-dimensional
    phaseplane. Likelihood surfaces and a simple Markov Chain Monte Carlo 
    parameter estimator can be used for model-data fusion of differential 
    equations and empirical models. The Euler-Maruyama method is provided
    for simulation of stochastic differential equations. The package was 
    originally written for internal use to support teaching by Zobitz, and
    refined to support the text "Exploring modeling with data and 
    differential equations using R" by John Zobitz. See online text:
    <https://jmzobitz.github.io/ModelingWithR/index.html>.

## Resubmission 15-06-2022
This is a resubmission. 
There were no ERRORs or WARNINGs or NOTEs

Thank you for reviewing my previous submission. Based on the previous review I have changed the following:

- The DESCRIPTION file contains a reference to a forthcoming textbook that describes the methods in this package as shown below:

  Description: Functions to support "Exploring modeling with data and 
    differential equations using R"" by John Zobitz. See online text:
    <https://jmzobitz.github.io/ModelingWithR/index.html>.

- mcmc_analyze.Rd now has \value tag describing the exported method:

  Two plots: (1) fitted model results compared to data, and (2) pairwise parameter histograms and scatterplots to test model equifinality. 

- Examples in (1) phaseplane.Rd, (2) mcmc_analyze.Rd, and (3) mcmc_estimate.Rd now are wrapped in \donttest rather than commented out code lines.

- The files phaseplane.R and mcmc_analyze.R both had information messages provided to the console. These messages are first captured with utils::capture.output and a message is subsequently written to the console. The information messages can be supressed with the parameter eq_soln in phaseplane.R and verbose in mcmc_analyze.R


## Resubmission 08-06-2022
This is a resubmission. 
There were no ERRORs or WARNINGs or NOTEs

In this version I have changed the following:

* Changed the DESCRIPTION file to remove the Maintainer and used an Authors field that has similar information as given in the following NOTE:
Check: CRAN incoming feasibility, Result: NOTE
  Maintainer: 'John Zobitz <zobitz@augsburg.edu>'

* Changed the R code for phaseplane to remove the following NOTE:
Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64
Check: R code for possible problems, Result: NOTE
  phaseplane: no visible binding for global variable '.'
  Undefined global functions or variables:
  
* Modified the examples for mcmc_analyze and mcmc_estimate so that the CPU time does not exceed 5 seconds as given in the following NOTE:
  Check: examples, Result: NOTE
  Examples with CPU (user + system) or elapsed time > 10s
                  user system elapsed
  mcmc_analyze  138.19   3.12  141.32
  mcmc_estimate 136.17   3.52  139.69
  
## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

- checking R code for possible problems ... NOTE
  phaseplane: no visible binding for global variable ‘.’
  Undefined global functions or variables:
  
I am aware of this note - this was the easiest way to redefine a user-defined variable name in the code to produce a plot (lines 148-149 in phaseplane).
