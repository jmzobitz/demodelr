#' Simulate a one dimensional random walk
#'
#' \code{summarizeDynamics} takes the result from a stochastic simulation and produces a spaghetti plot and ensemble average plot.
#'
#' @param simulations data frame from a stochastic simulation
#' @param labels the labes on the plot axes
#'
#' @return A spaghetti plot and ensemble average plot of simulation
#' @examples
#'
#' # Run the vignette that works through an example
#' vignette("stochstic-logistic")

#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export

summarizeDynamics <- function(simulations,labels) {



  #### Make a plot of the solution
  # Plot your results


  ####
  total_data=data.frame(steps=1:steps,x =out)
  spaghettiPlot <- total_data %>% gather(key=simulation,value=out,-time) %>%
    ggplot() +
    geom_line(aes(x=steps,y=out,group=simulation)) +
    theme(plot.title = element_text(size=20),
          axis.title.x=element_text(size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          axis.title.y=element_text(size=20)) +
    labs(x = "Steps",y = "Position",title="Spaghetti Plot")

  print(spaghettiPlot)


  ### Now loop through and make an ensemble plot of the results
  quantVals = c(0.025,0.5,0.975);  # The CI we want to utilize


  outCI = simulations %>%
    select(-time) %>%
    apply(1,quantile,quantVals);  # Apply it across the rows




  data=simulations %>%
    select(time)
  mutate(F =outCI[2,],L =outCI[1,],U =outCI[3,])
  ### Ensemble plot
  ensemblePlot=ggplot(data,aes(x=time,y=F)) +
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
