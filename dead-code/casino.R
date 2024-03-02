#' Simulate a casino game
#'
#' \code{casino} visualizes the vector field for a one or two dimensional differential equation.
#'
#' @param houseWinProbability probability the house wins the game
#' @param initialCash cash you start out with
#' @param nTimes times you decide to play the game
#' @param nSimulations the number of times you want to simulate the results
#'
#' @return A spaghetti plot and ensemble average plot of your winnings
#' @examples
#'
#' casino(.52,100,200,100)

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export

casino <- function(houseWinProbability,initialCash,nTimes,nSimulations) {

 ### Set up vector of results
winnings=array(0,dim=c(nTimes,nSimulations));
winnings[1,]=initialCash  # Set the first row of all simulations to 100.
### Loop through the each years and simulation, varying the birth and death rate
for (j in 1:nSimulations) {
  for (i in 2:nTimes) {
    if (runif(1)< houseWinProbability) {winnings[i,j]=winnings[i-1,j]-1}   # LOSING :-(
    else {winnings[i,j]=winnings[i-1,j]+1}   # WINNING :-)

    if (winnings[i,j]==0) break   # If you go broke, then exit the loop
  }
}

### Now loop through and make an ensemble plot of the results
quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize
# Now loop through everything
outCI=array(dim = c(length(quantVals),nTimes));

for (i in 1:nTimes) {
  outCI[,i] = quantile(winnings[i,],quantVals);

}


#### Make a plot of the solution
# Plot your results


####
total_data=data.frame(steps=1:nTimes,winnings =winnings)
spaghettiPlot <- total_data %>% gather(key=simulation,value=money,2:(nSimulations+1)) %>%
  ggplot() +
  geom_line(aes(x=steps,y=money,group=simulation)) +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Games",y = "Winnings",title="Spaghetti Plot")

print(spaghettiPlot)





data=data.frame(steps=1:nTimes,
                F =outCI[2,],
                L =outCI[1,],
                U =outCI[3,])
### Ensemble plot
ensemblePlot=ggplot(data,aes(x=steps,y=F)) +
  geom_ribbon(aes(ymin=L,ymax=U),alpha=0.2,colour='grey') +
  geom_line(size=1.5) +
  theme(plot.title = element_text(size=20),
        axis.title.x=element_text(size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=20)) +
  labs(x = "Iteration",y = "Money",title="Ensemble Plot")

print(ensemblePlot)


}
