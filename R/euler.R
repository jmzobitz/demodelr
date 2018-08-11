#' Solve a multi-dimensional dimensional differential equation with Euler's method. See the vignette for detailed examples of usage.

#' @param deltaT Size of timesteps
#' @param timeSteps Number of timesteps we solve.  deltaT*timeSteps = total time
#' @param variableNames Name of variables (i.e. P,V) that we have in our differential equation.
#' @param dynamics a Function that we have for our dynamics
#' @param parameters The values of the parameters we are using
#' @return A plot of your Euler's method solution
#' @examples
#' euler(deltaT,timeSteps,variableNames,FUN=dynamics,parameters=parameters)


#' @import tidyverse
#' @export

euler <- function(deltaT,timeSteps,variableNames,FUN=dynamics,parameters=parameters) {


nSolns <- dim(initialCondition)[1]  # Determine how many initial conditions we have
nVars <- dim(initialCondition)[2]  # Determine how many variables we have
time <- seq(from=0,by=deltaT,length.out=timeSteps)  # the output time vector

outSolutions <- array(0,dim=c(1,4))  # Vector of solutions  time solution variableName Run

for (j in 1:nSolns) {

  newP <-  initialCondition[j,] # The updated value
  names(newP) <- variableNames;  # Assign the variable name to the

  outMatrix <- matrix(0,nrow=nVars,ncol=4);
  outMatrix[,1] <- time[1];
  outMatrix[,2] <- initialCondition[j,];
  outMatrix[,3] <- variableNames;
  outMatrix[,4] <-j;
  outSolutions=rbind(outSolutions,outMatrix)



  for (i in 2:timeSteps) {
    oldP = newP
    newP =  dynamics(t,oldP,parameters)*deltaT+oldP   # Your differential equation goes here.
    #outSolutions=rbind(outSolutions,c(time[i],newP,j))

    outMatrix = matrix(0,nrow=nVars,ncol=4);
    outMatrix[,1]=time[i];
    outMatrix[,2]=newP;
    outMatrix[,3]=variableNames;
    outMatrix[,4]=j;
    outSolutions=rbind(outSolutions,outMatrix)




  }

}

# Remove the first row of outSolutions
outSolutions=outSolutions[-1,];
outSolutions=unname(outSolutions)

outSolutions = data.frame(time=as.numeric(outSolutions[,1]), value=as.numeric(outSolutions[,2]),variableName=outSolutions[,3],run=as.numeric(outSolutions[,4]))
# ggplot2


#pdf(file = outPlotName)
# First plot
outPlot= ggplot(data.frame(outSolutions), aes(x=time, y=value,color=variableName)) +
  geom_point(size=3)+facet_wrap(~variableName) +
  labs(title="Euler's Method Solution",x="Time",y="")+
  ### Expand the graph to make sure axes cross at (0,0)
  expand_limits(y=0) +
  scale_color_discrete(guide=FALSE)

print(outPlot)

}

### Clean up workspace
#rm(list=ls(all=TRUE))
