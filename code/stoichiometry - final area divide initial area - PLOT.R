#################################################
# Analysis of stoichiometry duckweed experiment #
# Conducted - Summer 2012                       #
#                                               #
# Final Area / Initial Area                     #    
#################################################

library(ggplot2)

# check out the data that you will use 
head(summary_data_area_final_divide_initial)


############
############
summary_data_area_final_divide_initial <- read.csv("area_final_divide_initial_posthoc.csv")
summary_data_area_final_divide_initial$nitrogen <- factor(summary_data_area_final_divide_initial$nitrogen , levels=c("lowN","medN","highN"))
summary_data_area_final_divide_initial$phosphorus <- factor(summary_data_area_final_divide_initial$phosphorus , levels=c("lowP","medP","highP"))

# labelling the facet variables
nitrogen_names <- list("lowN"="Low nitrogen","medN"="Medium nitrogen","highN"="High nitrogen")
phosphorus_names <- list("lowP"="Low phosphorus","medP"="Medium phosphorus","highP"="High phosphorus")

labeller_function <- function(variable,value){
  if (variable=="phosphorus") {
    return(phosphorus_names[value])
  } else {
    return(nitrogen_names[value])
  }
}

# making the plot 
mean_area_final_divide_initial_plot <- ggplot(summary_data_area_final_divide_initial, aes(x=species,y=final_divide_initial)) + geom_point() 
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_errorbar(aes(ymin=final_divide_initial-se, ymax=final_divide_initial+se), width=0.1)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + facet_grid(nitrogen ~ phosphorus, labeller=labeller_function)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + ylab("Final area / initial area ")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + xlab("Species")
#mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + ylim(0,0.4)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + theme_bw(base_size=18)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_text(data=summary_data_area_final_divide_initial,aes(x=species, y=final_divide_initial+se+1,label=label))
mean_area_final_divide_initial_plot 

# save it 
ggsave(filename = "mean_area_final_divide_initial_plot.jpg", mean_area_final_divide_initial_plot, height=11, width=11)

###########################
###########################
mean_area_final_divide_initial_plot <- ggplot(summary_data_area_final_divide_initial, aes(x=species, y=final_divide_initial)) 
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_errorbar(aes(ymin=final_divide_initial-se, ymax=final_divide_initial+se), width=0.1)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_point(size=3)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + facet_grid(nitrogen ~ phosphorus)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + ylab("final area / initial area")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + xlab("species")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + theme_gray(base_size=18)
mean_area_final_divide_initial_plot
