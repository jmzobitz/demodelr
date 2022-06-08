## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

- checking R code for possible problems ... NOTE
  phaseplane: no visible binding for global variable ‘.’
  Undefined global functions or variables:
  
I am aware of this note - this was the easiest way to redefine a user-defined variable name in the code to produce a plot (lines 148-149 in phaseplane).
