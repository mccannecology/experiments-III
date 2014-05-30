##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# avgRGR                                 #
# average within a replicate             #
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
# colour
mean_avgRGR_plot <- ggplot(summary_data_avgRGR, aes(x=species,y=avgRGR)) + geom_point() 
mean_avgRGR_plot <- mean_avgRGR_plot + geom_errorbar(aes(ymin=avgRGR-se, ymax=avgRGR+se), width=0.1)
mean_avgRGR_plot <- mean_avgRGR_plot + facet_grid(nitrogen ~ phosphorus)
mean_avgRGR_plot <- mean_avgRGR_plot + ylab("Average RGR")
mean_avgRGR_plot <- mean_avgRGR_plot + theme_gray(base_size=18)
mean_avgRGR_plot 

#####################
# Preliminary anova #
# Y = avgRGR        #
# Treatments:       #
# species,          #
# nitrogen,         #
# phosphorus,       #
#####################
avgRGR_anova <- aov(avgRGR ~ species*nitrogen*phosphorus, data=data)
summary(avgRGR_anova)
posthoc_avgRGR_anova <- TukeyHSD(avgRGR_anova)
posthoc_avgRGR_anova
threeway <- posthoc_avgRGR_anova[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(avgRGR_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(avgRGR_anova)) 
qqline(resid(avgRGR_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_anova)) # p-value = 0.01016 # Residuals are not noramlly distributed 


#################################
# transform and re-do the anova #
#################################
################################
# try a log + 1 transformation #
################################
avgRGR_anova_logx1 <- aov(log(avgRGR+1) ~ species*nitrogen*phosphorus, data=data)
summary(avgRGR_anova_logx1)
posthoc_avgRGR_anova_logx1 <- TukeyHSD(avgRGR_anova_logx1)
posthoc_avgRGR_anova_logx1
threeway <- posthoc_avgRGR_anova_logx1[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(avgRGR_anova_logx1))

# QQ plot 
# Does not look very normal 
qqnorm(resid(avgRGR_anova_logx1)) 
qqline(resid(avgRGR_anova_logx1)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_anova_logx1)) # p-value = 0.00218 



#################################
# transform and re-do the anova #
#################################
################################
# try a sqrt + 1 transformation #
################################
avgRGR_anova_sqrt1 <- aov(sqrt(avgRGR+1) ~ species*nitrogen*phosphorus, data=data)
summary(avgRGR_anova_sqrt1)
posthoc_avgRGR_anova_sqrt1 <- TukeyHSD(avgRGR_anova_sqrt1)
posthoc_avgRGR_anova_sqrt1
threeway <- posthoc_avgRGR_anova_sqrt1[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(avgRGR_anova_sqrt1))

# QQ plot 
# Does not look very normal 
qqnorm(resid(avgRGR_anova_sqrt1)) 
qqline(resid(avgRGR_anova_sqrt1)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_anova_sqrt1)) # p-value = 0.005152 


#################################
# transform and re-do the anova #
#################################
# DO NOT WORK: 
# arcsine sqrt 
# arcsine sqrt + 1 
# problems with negatives or #'s not in (0,1)




######################################
# ANOVA @ each treatment combination #
######################################
###############
# Low N Low P #
###############
avgRGR_LL_anova <- aov(avgRGR~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="lowP"))
summary(avgRGR_LL_anova)
TukeyHSD(avgRGR_LL_anova)

hist(resid(avgRGR_LL_anova)) # plot a histogram 

qqnorm(resid(avgRGR_LL_anova)) # QQ plot 
qqline(resid(avgRGR_LL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_LL_anova)) # p-value =  0.756

###############
# Med N Low P #
###############
avgRGR_ML_anova <- aov(avgRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))
summary(avgRGR_ML_anova)
TukeyHSD(avgRGR_ML_anova)

hist(resid(avgRGR_ML_anova)) # plot a histogram 

qqnorm(resid(avgRGR_ML_anova)) # QQ plot 
qqline(resid(avgRGR_ML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_ML_anova)) # p-value =  0.3127

################
# High N Low P #
################
avgRGR_HL_anova <- aov(avgRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="lowP"))
summary(avgRGR_HL_anova)
TukeyHSD(avgRGR_HL_anova)

hist(resid(avgRGR_HL_anova)) # plot a histogram 

qqnorm(resid(avgRGR_HL_anova)) # QQ plot 
qqline(resid(avgRGR_HL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_HL_anova)) # p-value =  0.6324

###############
# Low N Med P #
###############
avgRGR_LM_anova <- aov(avgRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="medP"))
summary(avgRGR_LM_anova)
TukeyHSD(avgRGR_LM_anova)

hist(resid(avgRGR_LM_anova)) # plot a histogram 

qqnorm(resid(avgRGR_LM_anova)) # QQ plot 
qqline(resid(avgRGR_LM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_LM_anova)) # p-value =  0.9212

################
# Low N High P #
################
avgRGR_LH_anova <- aov(avgRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP"))
summary(avgRGR_LH_anova)
TukeyHSD(avgRGR_LH_anova)

hist(resid(avgRGR_LH_anova)) # plot a histogram 

qqnorm(resid(avgRGR_LH_anova)) # QQ plot 
qqline(resid(avgRGR_LH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_LH_anova)) # p-value =  0.2664

###############
# Med N Med P #
###############
avgRGR_MM_anova <- aov(avgRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="medP"))
summary(avgRGR_MM_anova)
TukeyHSD(avgRGR_MM_anova)

hist(resid(avgRGR_MM_anova)) # plot a histogram 

qqnorm(resid(avgRGR_MM_anova)) # QQ plot 
qqline(resid(avgRGR_MM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_MM_anova)) # p-value =  0.01644

# log x+1 transformation
logx1_avgRGR_MM_anova <- aov(log(avgRGR+1) ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="medP"))
summary(logx1_avgRGR_MM_anova)
TukeyHSD(logx1_avgRGR_MM_anova)

hist(resid(logx1_avgRGR_MM_anova)) # plot a histogram 

qqnorm(resid(logx1_avgRGR_MM_anova)) # QQ plot 
qqline(resid(logx1_avgRGR_MM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(logx1_avgRGR_MM_anova)) # p-value =  0.01623


#################
# High N High P #
#################
avgRGR_HH_anova <- aov(avgRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="highP"))
summary(avgRGR_HH_anova)
TukeyHSD(avgRGR_HH_anova)

hist(resid(avgRGR_HH_anova)) # plot a histogram 

qqnorm(resid(avgRGR_HH_anova)) # QQ plot 
qqline(resid(avgRGR_HH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_HH_anova)) # p-value =  0.518

################
# Med N High P #
################
avgRGR_MH_anova <- aov(avgRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="highP"))
summary(avgRGR_MH_anova)
TukeyHSD(avgRGR_MH_anova)

hist(resid(avgRGR_MH_anova)) # plot a histogram 

qqnorm(resid(avgRGR_MH_anova)) # QQ plot 
qqline(resid(avgRGR_MH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_MH_anova)) # p-value =  0.05815

################
# High N Med P #
################
avgRGR_HM_anova <- aov(avgRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="medP"))
summary(avgRGR_HM_anova)
TukeyHSD(avgRGR_HM_anova)

hist(resid(avgRGR_HM_anova)) # plot a histogram 

qqnorm(resid(avgRGR_HM_anova)) # QQ plot 
qqline(resid(avgRGR_HM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(avgRGR_HM_anova)) # p-value =  0.9975


