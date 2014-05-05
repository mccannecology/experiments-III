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
mean_avgRGR_plot 

#####################
# Preliminary anova #
# Y = avgRGR        #
# Treatments:       #
# species,          #
# nitrogen,         #
# phosphorus,       #
#####################
avgRGR_anova <- aov(avgRGR ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
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
avgRGR_anova_logx1 <- aov(log(avgRGR+1) ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
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


