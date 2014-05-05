#################################################
# Analysis of stoichiometry duckweed experiment #
# Conducted - Summer 2012                       #
#                                               #
# Plotting data                                 #
# Area                                          #    
# Through time (day 0 to 12)                    #
#################################################
library(ggplot2)

# check out the data you will use
head(data_area)
head(summary_data_area)

############
# Raw data #
############
# colour
raw_area_0to17_plot <- ggplot(data_area, aes(x=day,y=area,group=id,colour=species)) + geom_line() + geom_point() 
raw_area_0to17_plot <- raw_area_0to17_plot + facet_grid(nitrogen ~ phosphorus)
raw_area_0to17_plot <- raw_area_0to17_plot + scale_x_discrete(breaks=c(0,3,7,10,14,17),labels=c(0,3,7,10,14,17))
raw_area_0to17_plot <- raw_area_0to17_plot + ylab("area (sq. mm)")
raw_area_0to17_plot 

############
# Average  #
############
# colour
mean_area_0to17_plot <- ggplot(summary_data_area, aes(x=day,y=area,colour=species)) + geom_line() + geom_point() 
mean_area_0to17_plot <- mean_area_0to17_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_area_0to17_plot <- mean_area_0to17_plot + facet_grid(nitrogen ~ phosphorus)
mean_area_0to17_plot <- mean_area_0to17_plot + scale_x_discrete(breaks=c(0,3,7,10,14,17),labels=c(0,3,7,10,14,17))
mean_area_0to17_plot <- mean_area_0to17_plot + ylab("area (sq. mm)")
mean_area_0to17_plot 

#####################
# Preliminary anova #
# Y = Area          #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
# day               #
#####################
area_anova <- aov(area ~ species*nitrogen*phosphorus*day, data=data_area)
summary(area_anova)
TukeyHSD(area_anova)

