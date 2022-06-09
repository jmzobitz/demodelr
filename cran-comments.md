## Resubmission 08-06-2022
This is a resubmission. 
There were no ERRORs or WARNINGs or NOTEs

In this version I have changed the following:

* Changed the DESCRIPTION file to remove the Maintainer and used an Authors field that has similar information.

Check: CRAN incoming feasibility, Result: NOTE
  Maintainer: 'John Zobitz <zobitz@augsburg.edu>'

* Changed the R code for phaseplane to remove the following NOTE:
Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64
Check: R code for possible problems, Result: NOTE
  phaseplane: no visible binding for global variable '.'
  Undefined global functions or variables:
  
* Modified the examples for mcmc_analyze and mcmc_estimate so that the CPU time does not exceed 5 seconds
  
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
