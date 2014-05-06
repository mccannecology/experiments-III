#################################################
# Analysis of stoichiometry duckweed experiment #
# Conducted - Summer 2012                       #
#                                               #
# Plotting data                                 #
# Day 0 area                                    # 
#################################################
library(ggplot2)
library(gridExtra)

# check out the data that you will use 
head(summary_data_area)

# see the classes of the variables
class(summary_data_area$species)
class(summary_data_area$nitrogen)
class(summary_data_area$phosphorus)

###########################
# Mean area at day 0      #
# By each treatment combo #
###########################
mean_area_day0_plot <- ggplot(subset(summary_data_area, summary_data_area$day == 0), aes(x=species, y=area)) 
mean_area_day0_plot <- mean_area_day0_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_area_day0_plot <- mean_area_day0_plot + geom_point(size=3)
mean_area_day0_plot <- mean_area_day0_plot + facet_grid(nitrogen ~ phosphorus)
mean_area_day0_plot <- mean_area_day0_plot + ylab("initial area (sq. mm)")
mean_area_day0_plot <- mean_area_day0_plot + xlab("species")
mean_area_day0_plot <- mean_area_day0_plot + theme_gray(base_size=18)
mean_area_day0_plot

#####################
# Preliminary anova #
#####################
# Y = Area day 0
# Treatments: species, nitrogen, phosphorus       
area_day0_anova <- aov(area ~ species*nitrogen*phosphorus, data=subset(data_area, data_area$day == 0))
summary(area_day0_anova)
TukeyHSD(area_day0_anova)

#####################
# Examine residuals #
#####################
hist(resid(area_day0_anova)) # plot a histogram 

qqnorm(resid(area_day0_anova)) # QQ plot 
qqline(resid(area_day0_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_day0_anova)) # p-value = 0.09892 



######################################
# ANOVA @ each treatment combination #
######################################
###############
# Low N Low P #
###############
area_day0_LL_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="lowN" & data_area$phosphorus=="lowP"))
summary(area_day0_LL_anova)
TukeyHSD(area_day0_LL_anova)

###############
# Med N Low P #
###############
area_day0_ML_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="medN" & data_area$phosphorus=="lowP"))
summary(area_day0_ML_anova)
TukeyHSD(area_day0_ML_anova)

################
# High N Low P #
################
area_day0_HL_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="highN" & data_area$phosphorus=="lowP"))
summary(area_day0_HL_anova)
TukeyHSD(area_day0_HL_anova)

###############
# Low N Med P #
###############
area_day0_LM_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="lowN" & data_area$phosphorus=="medP"))
summary(area_day0_LM_anova)
TukeyHSD(area_day0_LM_anova)

################
# Low N High P #
################
area_day0_LH_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="lowN" & data_area$phosphorus=="highP"))
summary(area_day0_LH_anova)
TukeyHSD(area_day0_LH_anova)

###############
# Med N Med P #
###############
area_day0_MM_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="medN" & data_area$phosphorus=="medP"))
summary(area_day0_MM_anova)
TukeyHSD(area_day0_MM_anova)

#################
# High N High P #
#################
area_day0_HH_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="highN" & data_area$phosphorus=="highP"))
summary(area_day0_HH_anova)
TukeyHSD(area_day0_HH_anova)

################
# Med N High P #
################
area_day0_MH_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="medN" & data_area$phosphorus=="highP"))
summary(area_day0_MH_anova)
TukeyHSD(area_day0_MH_anova)

################
# High N Med P #
################
area_day0_HM_anova <- aov(area ~ species, data=subset(data_area, data_area$day == 0 & data_area$nitrogen=="highN" & data_area$phosphorus=="medP"))
summary(area_day0_HM_anova)
TukeyHSD(area_day0_HM_anova)

