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
head(summary_data_turion_area_per_day)

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

######################
# turion production  #
# area per day       #
# Average            #
# all species        # 
######################
summary_data_turions <- read.csv("summary_data_turion_area_per_day_posthoc.csv")
summary_data_turions$nitrogen <- factor(summary_data_turions$nitrogen , levels=c("lowN","medN","highN"))
summary_data_turions$phosphorus <- factor(summary_data_turions$phosphorus , levels=c("lowP","medP","highP"))

# labelling the facet variables
nitrogen_names <- list("lowN"="0.5 mg N/L","medN"="5 mg N/L","highN"="10 mg N/L")
phosphorus_names <- list("lowP"="0.08 mg P/L","medP"="0.8 mg P/L","highP"="1.6 mg P/L")

labeller_function <- function(variable,value){
  if (variable=="phosphorus") {
    return(phosphorus_names[value])
  } else {
    return(nitrogen_names[value])
  }
}

# making the plot 
turion_production_plot <- ggplot(summary_data_turions, aes(x=species,y=turion_area_per_day)) + geom_point(size=3) 
turion_production_plot <- turion_production_plot + geom_errorbar(aes(ymin=turion_area_per_day-se, ymax=turion_area_per_day+se), width=0.1)
turion_production_plot <- turion_production_plot + facet_grid(nitrogen ~ phosphorus, labeller=labeller_function)
turion_production_plot <- turion_production_plot + ylab(expression(paste("Turion production (", mm^2,day^-1,")",sep="")))
turion_production_plot <- turion_production_plot + xlab("Species")
turion_production_plot <- turion_production_plot + theme_bw(base_size=18)
turion_production_plot <- turion_production_plot + geom_hline(aes(intercept=0),linetype="dashed")
#turion_production_plot <- turion_production_plot + geom_text(data=summary_data_turions,aes(x=species, y=turion_area_per_day+se+0.15,label=label))
turion_production_plot 

# save it 
ggsave(filename = "turion_production_plot.jpg", turion_production_plot, height=8)


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




#######################
# turion_area_per_day #
# Average             #
# All species         #
#######################
# excluding replicates that were replcated during the experiment 

summary_data_turion_area_per_day <- read.csv("summary_data_turion_area_per_day_posthoc.csv")
summary_data_turion_area_per_day$nitrogen <- factor(summary_data_turion_area_per_day$nitrogen , levels=c("lowN","medN","highN"))
summary_data_turion_area_per_day$phosphorus <- factor(summary_data_turion_area_per_day$phosphorus , levels=c("lowP","medP","highP"))

# labelling the facet variables
nitrogen_names <- list("lowN"="0.5 mg N/L","medN"="5 mg N/L","highN"="10 mg N/L")
phosphorus_names <- list("lowP"="0.08 mg P/L","medP"="0.8 mg P/L","highP"="1.6 mg P/L")

labeller_function <- function(variable,value){
  if (variable=="phosphorus") {
    return(phosphorus_names[value])
  } else {
    return(nitrogen_names[value])
  }
}

turion_area_per_day_plot <- ggplot(summary_data_turion_area_per_day, aes(x=species,y=turion_area_per_day)) 
turion_area_per_day_plot <- turion_area_per_day_plot + geom_point() 
turion_area_per_day_plot <- turion_area_per_day_plot + ylim(0,1.5)
turion_area_per_day_plot <- turion_area_per_day_plot + geom_errorbar(aes(ymin=turion_area_per_day-se, ymax=turion_area_per_day+se), width=0.1)
turion_area_per_day_plot <- turion_area_per_day_plot + facet_grid(nitrogen ~ phosphorus, labeller=labeller_function)
turion_area_per_day_plot <- turion_area_per_day_plot + ylab(expression(paste("Turion production (", mm^2,day^-1,") ",sep="")))
turion_area_per_day_plot <- turion_area_per_day_plot + geom_text(data=summary_data_turion_area_per_day,aes(x=species, y=turion_area_per_day+se+0.125,label=label))
turion_area_per_day_plot <- turion_area_per_day_plot + theme_bw(base_size=18)
turion_area_per_day_plot 

ggsave(filename = "turion_area_per_day_plot.jpg", turion_area_per_day_plot, height=11, width=11)


# re-making the plot for Hydrobiologia
turion_area_per_day_plot <- ggplot(summary_data_turion_area_per_day, aes(x=species,y=turion_area_per_day)) 
turion_area_per_day_plot <- turion_area_per_day_plot + geom_point() 
turion_area_per_day_plot <- turion_area_per_day_plot + ylim(0,1.5)
turion_area_per_day_plot <- turion_area_per_day_plot + geom_errorbar(aes(ymin=turion_area_per_day-se, ymax=turion_area_per_day+se), width=0.1)
turion_area_per_day_plot <- turion_area_per_day_plot + facet_grid(nitrogen ~ phosphorus, labeller=labeller_function)
turion_area_per_day_plot <- turion_area_per_day_plot + ylab(expression(paste("Turion production (", mm^2,day^-1,") ",sep="")))
turion_area_per_day_plot <- turion_area_per_day_plot + xlab("Species")
turion_area_per_day_plot <- turion_area_per_day_plot + geom_text(data=summary_data_turion_area_per_day,aes(x=species, y=turion_area_per_day+se+0.125,label=label))
turion_area_per_day_plot <- turion_area_per_day_plot + theme_bw(base_size=12)
turion_area_per_day_plot 

ggsave(filename = "Fig04-turion_area_per_day_plot.jpg", turion_area_per_day_plot, dpi=300, units="mm", width=174)
ggsave(filename = "Fig04-turion_area_per_day_plot.eps", turion_area_per_day_plot, dpi=300, units="mm", width=174)

