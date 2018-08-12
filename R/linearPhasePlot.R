### Function linearPhasePlot.R
### Modified: 9/15/15
### Purpose: Solve a two dimensional differential equation using some of the built-in functionality
### Adapted from phasePlane.R by Danny Kaplan
### Output:  
### phase plane diagram of linear system

### 1) Specify the coefficients for the linear system
### dx/dt = a*x+b*y
### dy/dt = c*x+d*y
### matrixEntries = c(a,b,c,d) # Read Across each rows

matrixEntries = c(4,6,-3,-2)  


### 2) Specify information about the plot
numArrows = 20    # number of arrows plotted on each row and column
xWindow = c(-2,2)   # x axis limits.  Must be of the form c(minVal,maxVal)
yWindow = c(-2,2)   # y axis limits.  Must be of the form c(minVal,maxVal)
xLabel = 'x'   # x axis label for plot
yLabel = 'y'   # x axis label for plot


############ NO ADDITONAL MODIFICATION IS NEEDED BELOW THIS LINE
############ everything else below the line just plots the solution

library(reshape2)

a=matrixEntries[1]
b=matrixEntries[2]
c=matrixEntries[3]
d=matrixEntries[4]


linear <- function(a=1,b=1,c=1,d=1){
  function(x,y=NULL){
    if (is.null(y)) {
      y<- x[2]; x <- x[1];
    }
    dx = a*x+b*y;
    dy = c*x+d*y;
    return( c(dx, dy) );
  }
}

phasearrows <- function(fun,xlims,ylims,resol=10, col='black', add=F) {
  if (add==F) {
    plot(1,xlim=xlims, ylim=ylims, type='n');
  }
  x <- matrix(seq(xlims[1],xlims[2], length=resol), byrow=T, resol,resol);
  y <- matrix(seq(ylims[1],ylims[2], length=resol),byrow=F, resol, resol);
  npts <- resol*resol;
  xspace <- abs(diff(xlims))/(resol*5);
  yspace <- abs(diff(ylims))/(resol*5);
  
  
  
  x <- x + matrix(runif(npts, -xspace, xspace),resol,resol);
  y <- y + matrix(runif(npts, -yspace, yspace),resol,resol);
  
  
  z <- fun(x,y);
  
  z1 <- matrix(z[1:npts], resol, resol);
  z2 <- matrix(z[(npts+1):(2*npts)], resol, resol);
  maxx <- max(abs(z1));
  maxy <- max(abs(z2));
  dt <- min( abs(diff(xlims))/maxx, abs(diff(ylims))/maxy)/resol;
  lens <- sqrt(z1^2 + z2^2);
  lens2 <- lens/max(lens); 
  delta_x = dt*z1/((lens2)+.1);
  delta_y = dt*z2/((lens2)+.1);
  
  xEndVals = x + delta_x;
  yEndVals = y + delta_y;
  
  dataValues = data.frame(xVal=melt(x)[,3], yVal=melt(y)[,3], xEndVal=melt(xEndVals)[,3],yEndVal=melt(yEndVals)[,3])
  
  library(grid) # needed for arrow function
  library(ggplot2)
  
  p = ggplot(dataValues, aes(x = xVal, y = yVal))+
    geom_segment(aes(xend = xEndVal, yend = yEndVal), arrow = arrow(length = unit(0.1,"cm")))+
    xlab(xLabel)+
    ylab(yLabel)
  
  
  print(p)
}


phasearrows(linear(a,b,c,d),xWindow,yWindow,numArrows)




