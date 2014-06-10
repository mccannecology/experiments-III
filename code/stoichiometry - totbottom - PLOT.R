##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# totbottom                              #
##########################################
library(ggplot2)

# check out the data you will use
head(data_turions)
head(summary_data_turions)

###############
# totbottom   #
# Average     #
# all species #
###############
# excluding replicates that were replcated during the experiment 
mean_totbottom_plot <- ggplot(summary_data_turions, aes(x=species,y=totbottom)) + geom_point() 
mean_totbottom_plot <- mean_totbottom_plot + geom_errorbar(aes(ymin=totbottom-se, ymax=totbottom+se), width=0.1)
mean_totbottom_plot <- mean_totbottom_plot + facet_grid(nitrogen ~ phosphorus)
mean_totbottom_plot <- mean_totbottom_plot + ylab("Total turions produced per replicate")
mean_totbottom_plot <- mean_totbottom_plot + theme_gray(base_size=18)
mean_totbottom_plot 

###############
# totbottom   #
# Average     #
# all species # 
###############
summary_data_turions <- read.csv("summary_data_turions.csv")
summary_data_turions$nitrogen <- factor(summary_data_turions$nitrogen , levels=c("lowN","medN","highN"))
summary_data_turions$phosphorus <- factor(summary_data_turions$phosphorus , levels=c("lowP","medP","highP"))

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
mean_totbottom_plot <- ggplot(summary_data_turions, aes(x=species,y=totbottom)) + geom_point() 
mean_totbottom_plot <- mean_totbottom_plot + geom_errorbar(aes(ymin=totbottom-se, ymax=totbottom+se), width=0.1)
mean_totbottom_plot <- mean_totbottom_plot + facet_grid(nitrogen ~ phosphorus, labeller=labeller_function)
mean_totbottom_plot <- mean_totbottom_plot + ylab("Total turions produced per replicate")
mean_totbottom_plot <- mean_totbottom_plot + xlab("Species")
mean_totbottom_plot <- mean_totbottom_plot + theme_bw(base_size=18)
# mean_totbottom_plot <- mean_totbottom_plot + geom_text(data=summary_data_turions,aes(x=species, y=totbottom+se+0.015,label=label))
mean_totbottom_plot 

# save it 
ggsave(filename = "mean_totbottom_plot.jpg", mean_totbottom_plot, height=11, width=11)


###############
# totbottom   #
# Average     #
# wolffia     #
###############
# excluding replicates that were replcated during the experiment 
mean_totbottom_WB_plot <- ggplot(subset(summary_data_turions, summary_data_turions$species=="WB"), aes(x=species,y=totbottom)) 
mean_totbottom_WB_plot <- mean_totbottom_WB_plot + geom_point() 
mean_totbottom_WB_plot <- mean_totbottom_WB_plot + geom_errorbar(aes(ymin=totbottom-se, ymax=totbottom+se), width=0.1)
mean_totbottom_WB_plot <- mean_totbottom_WB_plot + facet_grid(nitrogen ~ phosphorus)
mean_totbottom_WB_plot <- mean_totbottom_WB_plot + ylab("Total turions produced per replicate")
mean_totbottom_WB_plot <- mean_totbottom_WB_plot + theme_bw(base_size=18)
mean_totbottom_WB_plot <- mean_totbottom_WB_plot + ggtitle("W. brasiliensis")
mean_totbottom_WB_plot 

ggsave(filename = "mean_totbottom_WB_plot.jpg", mean_totbottom_WB_plot, height=11, width=11)


###############
# totbottom   #
# Average     #
# spirodela   #
###############
# excluding replicates that were replcated during the experiment 
mean_totbottom_SP_plot <- ggplot(subset(summary_data_turions, summary_data_turions$species=="SP"), aes(x=species,y=totbottom)) 
mean_totbottom_SP_plot <- mean_totbottom_SP_plot + geom_point() 
mean_totbottom_SP_plot <- mean_totbottom_SP_plot + geom_errorbar(aes(ymin=totbottom-se, ymax=totbottom+se), width=0.1)
mean_totbottom_SP_plot <- mean_totbottom_SP_plot + facet_grid(nitrogen ~ phosphorus)
mean_totbottom_SP_plot <- mean_totbottom_SP_plot + ylab("Total turions produced per replicate")
mean_totbottom_SP_plot <- mean_totbottom_SP_plot + theme_bw(base_size=18)
mean_totbottom_SP_plot <- mean_totbottom_SP_plot + ggtitle("S. polyrhiza")
mean_totbottom_SP_plot 

ggsave(filename = "mean_totbottom_SP_plot.jpg", mean_totbottom_SP_plot, height=11, width=11)




###################
# turions_per_day #
# Average         #
# wolffia         #
###################
# excluding replicates that were replcated during the experiment 
mean_turions_per_day_WB_plot <- ggplot(subset(summary_data_turions_per_day, summary_data_turions_per_day$species=="WB"), aes(x=species,y=turions_per_day)) 
mean_turions_per_day_WB_plot <- mean_turions_per_day_WB_plot + geom_point() 
mean_turions_per_day_WB_plot <- mean_turions_per_day_WB_plot + geom_errorbar(aes(ymin=turions_per_day-se, ymax=turions_per_day+se), width=0.1)
mean_turions_per_day_WB_plot <- mean_turions_per_day_WB_plot + facet_grid(nitrogen ~ phosphorus)
mean_turions_per_day_WB_plot <- mean_turions_per_day_WB_plot + ylab("Turions per day")
mean_turions_per_day_WB_plot <- mean_turions_per_day_WB_plot + theme_bw(base_size=18)
mean_turions_per_day_WB_plot <- mean_turions_per_day_WB_plot + ggtitle("W. brasiliensis")
mean_turions_per_day_WB_plot 

ggsave(filename = "mean_turions_per_day_WB_plot.jpg", mean_turions_per_day_WB_plot, height=11, width=11)


###################
# turions_per_day #
# Average         #
# spirodela       #
###################
# excluding replicates that were replcated during the experiment 
mean_turions_per_day_SP_plot <- ggplot(subset(summary_data_turions_per_day, summary_data_turions_per_day$species=="SP"), aes(x=species,y=turions_per_day)) 
mean_turions_per_day_SP_plot <- mean_turions_per_day_SP_plot + geom_point() 
mean_turions_per_day_SP_plot <- mean_turions_per_day_SP_plot + geom_errorbar(aes(ymin=turions_per_day-se, ymax=turions_per_day+se), width=0.1)
mean_turions_per_day_SP_plot <- mean_turions_per_day_SP_plot + facet_grid(nitrogen ~ phosphorus)
mean_turions_per_day_SP_plot <- mean_turions_per_day_SP_plot + ylab("Turions per day")
mean_turions_per_day_SP_plot <- mean_turions_per_day_SP_plot + theme_bw(base_size=18)
mean_turions_per_day_SP_plot <- mean_turions_per_day_SP_plot + ggtitle("S. polyrhiza")
mean_turions_per_day_SP_plot 

ggsave(filename = "mean_turions_per_day_SP_plot.jpg", mean_turions_per_day_SP_plot, height=11, width=11)


