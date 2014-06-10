##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# maxRGR                                 #
# max within a replicate                 #
##########################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_rgr_by_day)
head(summary_data_maxRGR)

############
# maxRGR   #
# Average  #
############
summary_data_maxRGRII <- read.csv("maxRGR_posthoc.csv")
summary_data_maxRGRII$nitrogen <- factor(summary_data_maxRGRII$nitrogen , levels=c("lowN","medN","highN"))
summary_data_maxRGRII$phosphorus <- factor(summary_data_maxRGRII$phosphorus , levels=c("lowP","medP","highP"))

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
mean_maxRGR_plot <- ggplot(summary_data_maxRGRII, aes(x=species,y=maxRGR)) + geom_point() 
mean_maxRGR_plot <- mean_maxRGR_plot + geom_errorbar(aes(ymin=maxRGR-se, ymax=maxRGR+se), width=0.1)
mean_maxRGR_plot <- mean_maxRGR_plot + facet_grid(nitrogen ~ phosphorus, labeller=labeller_function)
mean_maxRGR_plot <- mean_maxRGR_plot + ylab(expression(paste("Maximum Relative Growth Rate (", mm^2,"/",mm^-2,"/",day^-1,")",sep="")))
mean_maxRGR_plot <- mean_maxRGR_plot + xlab("Species")
mean_maxRGR_plot <- mean_maxRGR_plot + ylim(0,0.4)
mean_maxRGR_plot <- mean_maxRGR_plot + theme_bw(base_size=18)
mean_maxRGR_plot <- mean_maxRGR_plot + geom_text(data=summary_data_maxRGRII,aes(x=species, y=maxRGR+se+0.025,label=label))
mean_maxRGR_plot 

# save it 
ggsave(filename = "mean_maxRGR_plot.jpg", mean_maxRGR_plot, height=11, width=11)

############
# maxRGR   #
# Average  #
############
# colour
mean_maxRGR_plot <- ggplot(summary_data_maxRGR, aes(x=species,y=maxRGR)) + geom_point() 
mean_maxRGR_plot <- mean_maxRGR_plot + geom_errorbar(aes(ymin=maxRGR-se, ymax=maxRGR+se), width=0.1)
mean_maxRGR_plot <- mean_maxRGR_plot + facet_grid(nitrogen ~ phosphorus)
mean_maxRGR_plot <- mean_maxRGR_plot + ylab("Maximum RGR")
mean_maxRGR_plot <- mean_maxRGR_plot + theme_gray(base_size=18)
mean_maxRGR_plot 



