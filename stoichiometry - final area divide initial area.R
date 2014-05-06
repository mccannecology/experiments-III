#################################################
# Analysis of stoichiometry duckweed experiment #
# Conducted - Summer 2012                       #
#                                               #
# Final Area / Initial Area                     #    
#################################################

library(ggplot2)

# check out the data that you will use 
head(summary_data_area_final_divide_initial)

###########################
# Mean area at day 0      #
# By each treatment combo #
###########################
mean_area_final_divide_initial_plot <- ggplot(summary_data_area_final_divide_initial, aes(x=species, y=final_divide_initial)) 
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_errorbar(aes(ymin=final_divide_initial-se, ymax=final_divide_initial+se), width=0.1)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + geom_point(size=3)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + facet_grid(nitrogen ~ phosphorus)
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + ylab("final area / initial area")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + xlab("species")
mean_area_final_divide_initial_plot <- mean_area_final_divide_initial_plot + theme_gray(base_size=18)
mean_area_final_divide_initial_plot

#####################
# Preliminary anova #
# Three-way         #
#####################
area_final_divide_initial_anova <- aov(final_divide_initial ~ species*nitrogen*phosphorus, data=data)
summary(area_final_divide_initial_anova)
TukeyHSD(area_final_divide_initial_anova)

#####################
# Examine residuals #
#####################
hist(resid(area_final_divide_initial_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initial_anova)) # QQ plot 
qqline(resid(area_final_divide_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initial_anova)) # p-value = 3.969e-09 




#################################
# transform and re-do the anova #
#################################



#############################
# try a sqrt transformation #
#############################
sqrt_area_final_divide_initial_anova <- aov(sqrt(final_divide_initial) ~ species*nitrogen*phosphorus, data=data)
summary(sqrt_area_final_divide_initial_anova)
posthoc_sqrt_area_final_divide_initial_anova<- TukeyHSD(sqrt_area_final_divide_initial_anova)
posthoc_sqrt_area_final_divide_initial_anova

#####################
# Examine residuals #
#####################
hist(resid(sqrt_area_final_divide_initial_anova)) # plot a histogram 

qqnorm(resid(sqrt_area_final_divide_initial_anova)) # QQ plot 
qqline(resid(sqrt_area_final_divide_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(sqrt_area_final_divide_initial_anova)) # p-value = 0.009532 



#################################
# transform and re-do the anova #
#################################



###############################
# try a logx+1 transformation #
###############################
logx1_area_final_divide_initial_anova <- aov(log(final_divide_initial+1) ~ species*nitrogen*phosphorus, data=data)
summary(logx1_area_final_divide_initial_anova)
posthoc_logx1_area_final_divide_initial_anova <- TukeyHSD(logx1_area_final_divide_initial_anova)
posthoc_logx1_area_final_divide_initial_anova[7]

#####################
# Examine residuals #
#####################
hist(resid(logx1_area_final_divide_initial_anova)) # plot a histogram 

qqnorm(resid(logx1_area_final_divide_initial_anova)) # QQ plot 
qqline(resid(logx1_area_final_divide_initial_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(logx1_area_final_divide_initial_anova)) # p-value = 0.3677 





######################################
# ANOVA @ each treatment combination #
######################################
###############
# Low N Low P #
###############
area_final_divide_initialLL_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="lowP"))
summary(area_final_divide_initialLL_anova)
TukeyHSD(area_final_divide_initialLL_anova)

hist(resid(area_final_divide_initialLL_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialLL_anova)) # QQ plot 
qqline(resid(area_final_divide_initialLL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialLL_anova)) # p-value =  0.7991

###############
# Med N Low P #
###############
area_final_divide_initialML_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))
summary(area_final_divide_initialML_anova)
TukeyHSD(area_final_divide_initialML_anova)

hist(resid(area_final_divide_initialML_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialML_anova)) # QQ plot 
qqline(resid(area_final_divide_initialML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialML_anova)) # p-value =  0.8382

################
# High N Low P #
################
area_final_divide_initialHL_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="lowP"))
summary(area_final_divide_initialHL_anova)
TukeyHSD(area_final_divide_initialHL_anova)

hist(resid(area_final_divide_initialHL_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialHL_anova)) # QQ plot 
qqline(resid(area_final_divide_initialHL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialHL_anova)) # p-value =  0.8382

###############
# Low N Med P #
###############
area_final_divide_initialLM_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="medP"))
summary(area_final_divide_initialLM_anova)
TukeyHSD(area_final_divide_initialLM_anova)

hist(resid(area_final_divide_initialLM_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialLM_anova)) # QQ plot 
qqline(resid(area_final_divide_initialLM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialLM_anova)) # p-value =  0.679

################
# Low N High P #
################
area_final_divide_initialLH_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP"))
summary(area_final_divide_initialLH_anova)
TukeyHSD(area_final_divide_initialLH_anova)

hist(resid(area_final_divide_initialLH_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialLH_anova)) # QQ plot 
qqline(resid(area_final_divide_initialLH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialLH_anova)) # p-value =  0.0005712

# log x + 1 transform 
logx1_area_final_divide_initialLH_anova <- aov(log(final_divide_initial+1)~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP"))
summary(logx1_area_final_divide_initialLH_anova)
TukeyHSD(logx1_area_final_divide_initialLH_anova)

hist(resid(logx1_area_final_divide_initialLH_anova)) # plot a histogram 

qqnorm(resid(logx1_area_final_divide_initialLH_anova)) # QQ plot 
qqline(resid(logx1_area_final_divide_initialLH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(logx1_area_final_divide_initialLH_anova)) # p-value = 0.07561

###############
# Med N Med P #
###############
area_final_divide_initialMM_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="medP"))
summary(area_final_divide_initialMM_anova)
TukeyHSD(area_final_divide_initialMM_anova)

hist(resid(area_final_divide_initialMM_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialMM_anova)) # QQ plot 
qqline(resid(area_final_divide_initialMM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialMM_anova)) # p-value =  0.4819

#################
# High N High P #
#################
area_final_divide_initialHH_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="highP"))
summary(area_final_divide_initialHH_anova)
TukeyHSD(area_final_divide_initialHH_anova)

hist(resid(area_final_divide_initialHH_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialHH_anova)) # QQ plot 
qqline(resid(area_final_divide_initialHH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialHH_anova)) # p-value =  0.3709

################
# Med N High P #
################
area_final_divide_initialMH_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="highP"))
summary(area_final_divide_initialMH_anova)
TukeyHSD(area_final_divide_initialMH_anova)

hist(resid(area_final_divide_initialMH_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialMH_anova)) # QQ plot 
qqline(resid(area_final_divide_initialMH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialMH_anova)) # p-value =  0.8046

################
# High N Med P #
################
area_final_divide_initialHM_anova <- aov(final_divide_initial~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="medP"))
summary(area_final_divide_initialHM_anova)
TukeyHSD(area_final_divide_initialHM_anova)

hist(resid(area_final_divide_initialHM_anova)) # plot a histogram 

qqnorm(resid(area_final_divide_initialHM_anova)) # QQ plot 
qqline(resid(area_final_divide_initialHM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(area_final_divide_initialHM_anova)) # p-value =  0.1918
