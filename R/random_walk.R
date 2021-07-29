#' Simulate a one dimensional random walk
#'
#' \code{random_walk} does a 1-dimensional random walk where you are equally likely to move to the left or the right.
#'
#' @param steps number of steps you run the walk
#' @param nSimulations the number of times you want to simulate the results
#'
#' @return A spaghetti plot and ensemble average plot of your winnings
#' @examples
#'
#' randomWalk(100,200)

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export

random_walk <- function(steps,nSimulations) {


  x <- array(0,dim=c(steps,nSimulations))
  prob <- apply(x,2,runif)
  x[prob>1/2] = 1
  x[prob<1/2] = -1
  x[1,]=0   # All conditions start at 0


  out<-apply(x, 2, cumsum)


  ### Now loop through and make an ensemble plot of the results
  quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize


  outCI = apply(out,1,quantile,quantVals);  # Apply it across the rows


  #### Make a plot of the solution
  # Plot your results


  ####
  total_data=data.frame(steps=1:steps,x =out)
  spaghettiPlot <- total_data %>% gather(key=simulation,value=out,2:(nSimulations+1)) %>%
    ggplot() +
    geom_line(aes(x=steps,y=out,group=simulation)) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Steps",y = "Position",title="Spaghetti Plot")

  print(spaghettiPlot)





  data=data.frame(steps=1:steps,
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
    labs(x = "Steps",y = "Position",title="Ensemble Plot")

  print(ensemblePlot)


}
