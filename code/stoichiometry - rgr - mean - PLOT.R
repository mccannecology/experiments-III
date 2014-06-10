##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# avgRGR                                 #
# avg within a replicate                 #
##########################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_rgr_by_day)
head(summary_data_avgRGR)

############
# avgRGR   #
# Average  #
############
summary_data_avgRGRII <- read.csv("avgRGR_posthoc.csv")
summary_data_avgRGRII$nitrogen <- factor(summary_data_avgRGRII$nitrogen , levels=c("lowN","medN","highN"))
summary_data_avgRGRII$phosphorus <- factor(summary_data_avgRGRII$phosphorus , levels=c("lowP","medP","highP"))

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
mean_avgRGR_plot <- ggplot(summary_data_avgRGRII, aes(x=species,y=avgRGR)) + geom_point() 
mean_avgRGR_plot <- mean_avgRGR_plot + geom_errorbar(aes(ymin=avgRGR-se, ymax=avgRGR+se), width=0.1)
mean_avgRGR_plot <- mean_avgRGR_plot + facet_grid(nitrogen ~ phosphorus, labeller=labeller_function)
mean_avgRGR_plot <- mean_avgRGR_plot + ylab(expression(paste("Average Relative Growth Rate (", mm^2,"/",mm^-2,"/",day^-1,")",sep="")))
mean_avgRGR_plot <- mean_avgRGR_plot + xlab("Species")
#mean_avgRGR_plot <- mean_avgRGR_plot + ylim(0,0.4)
mean_avgRGR_plot <- mean_avgRGR_plot + theme_bw(base_size=18)
mean_avgRGR_plot <- mean_avgRGR_plot + geom_text(data=summary_data_avgRGRII,aes(x=species, y=avgRGR+se+0.015,label=label))
mean_avgRGR_plot 

# save it 
ggsave(filename = "mean_avgRGR_plot.jpg", mean_avgRGR_plot, height=11, width=11)

############
# avgRGR   #
# Average  #
############
# colour
mean_avgRGR_plot <- ggplot(summary_data_avgRGR, aes(x=species,y=avgRGR)) + geom_point() 
mean_avgRGR_plot <- mean_avgRGR_plot + geom_errorbar(aes(ymin=avgRGR-se, ymax=avgRGR+se), width=0.1)
mean_avgRGR_plot <- mean_avgRGR_plot + facet_grid(nitrogen ~ phosphorus)
mean_avgRGR_plot <- mean_avgRGR_plot + ylab("Maximum RGR")
mean_avgRGR_plot <- mean_avgRGR_plot + theme_gray(base_size=18)
mean_avgRGR_plot 



