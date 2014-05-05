##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# RGR                                    #
# average and max within a replicate     #
##########################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_rgr)

############
# rgr_avg  #
# Raw data #
############
# colour
raw_rgr_avg_plot <- ggplot(data_raw, aes(x=species,y=rgr_avg)) + geom_point() 
raw_rgr_avg_plot <- raw_rgr_avg_plot + facet_grid(Nutr ~ Temp)
<<<<<<< HEAD
raw_rgr_avg_plot <- raw_rgr_avg_plot + ylab("Mean RGR")
=======
raw_rgr_avg_plot <- raw_rgr_avg_plot + ylab("Max RGR")
>>>>>>> 3d13b452c75ce3f667fa3a78374141cbdbabc0d5
raw_rgr_avg_plot 

############
# rgr_avg  #
# Average  #
############
# colour
mean_rgr_avg_plot <- ggplot(summary_data_rgr_avg, aes(x=species,y=rgr_avg)) + geom_point() 
mean_rgr_avg_plot <- mean_rgr_avg_plot + geom_errorbar(aes(ymin=rgr_avg-se, ymax=rgr_avg+se), width=0.1)
mean_rgr_avg_plot <- mean_rgr_avg_plot + facet_grid(Nutr ~ Temp)
<<<<<<< HEAD
mean_rgr_avg_plot <- mean_rgr_avg_plot + ylab("Mean RGR")
=======
mean_rgr_avg_plot <- mean_rgr_avg_plot + ylab("Max RGR")
>>>>>>> 3d13b452c75ce3f667fa3a78374141cbdbabc0d5
mean_rgr_avg_plot 

#####################
# Preliminary anova #
# Y = rgr_avg       #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
#####################
rgr_avg_anova <- aov(rgr_avg ~ species*Temp*Nutr, data=data_raw)
summary(rgr_avg_anova)
TukeyHSD(rgr_avg_anova)





############
# rgr_max  #
# Raw data #
############
# colour
raw_rgr_max_plot <- ggplot(data_raw, aes(x=species,y=rgr_max)) + geom_point() 
raw_rgr_max_plot <- raw_rgr_max_plot + facet_grid(Nutr ~ Temp)
<<<<<<< HEAD
raw_rgr_max_plot <- raw_rgr_max_plot + ylab("Max RGR")
=======
raw_rgr_max_plot <- raw_rgr_max_plot + ylab("Mean RGR")
>>>>>>> 3d13b452c75ce3f667fa3a78374141cbdbabc0d5
raw_rgr_max_plot 

############
# rgr_max  #
# Average  #
############
# colour
mean_rgr_max_plot <- ggplot(summary_data_rgr_max, aes(x=species,y=rgr_max)) + geom_point() 
mean_rgr_max_plot <- mean_rgr_max_plot + geom_errorbar(aes(ymin=rgr_max-se, ymax=rgr_max+se), width=0.1)
mean_rgr_max_plot <- mean_rgr_max_plot + facet_grid(Nutr ~ Temp)
<<<<<<< HEAD
mean_rgr_max_plot <- mean_rgr_max_plot + ylab("Max RGR")
=======
mean_rgr_max_plot <- mean_rgr_max_plot + ylab("Mean RGR")
>>>>>>> 3d13b452c75ce3f667fa3a78374141cbdbabc0d5
mean_rgr_max_plot 

#####################
# Preliminary anova #
# Y = rgr_max       #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
#####################
rgr_max_anova <- aov(rgr_max ~ species*Temp*Nutr, data=data_raw)
summary(rgr_max_anova)
TukeyHSD(rgr_max_anova)
