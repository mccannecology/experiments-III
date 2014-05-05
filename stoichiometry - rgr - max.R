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
# colour
mean_maxRGR_plot <- ggplot(summary_data_maxRGR, aes(x=species,y=maxRGR)) + geom_point() 
mean_maxRGR_plot <- mean_maxRGR_plot + geom_errorbar(aes(ymin=maxRGR-se, ymax=maxRGR+se), width=0.1)
mean_maxRGR_plot <- mean_maxRGR_plot + facet_grid(nitrogen ~ phosphorus)
mean_maxRGR_plot <- mean_maxRGR_plot + ylab("Average RGR")
mean_maxRGR_plot 

#####################
# Preliminary anova #
# Y = maxRGR        #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
#####################
maxRGR_anova <- aov(maxRGR ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
summary(maxRGR_anova)
posthoc_maxRGR_anova<- TukeyHSD(maxRGR_anova)
posthoc_maxRGR_anova
threeway <- posthoc_maxRGR_anova[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(maxRGR_anova))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova)) 
qqline(resid(maxRGR_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova)) # p-value = 0.0261 # Residuals are not noramlly distributed 



#################################
# transform and re-do the anova #
#################################
###############################
# try a sqrt+1 transformation #
###############################
maxRGR_anova_sqrt <- aov(sqrt(maxRGR+1) ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
summary(maxRGR_anova_sqrt)
posthoc_maxRGR_anova_sqrt <- TukeyHSD(maxRGR_anova_sqrt)
posthoc_maxRGR_anova_sqrt
threeway <- posthoc_maxRGR_anova_sqrt[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(maxRGR_anova_sqrt))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova_sqrt)) 
qqline(resid(maxRGR_anova_sqrt)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova_sqrt)) # p-value = 0.02419 



#################################
# transform and re-do the anova #
#################################
################################
# try a log + 1 transformation #
################################
maxRGR_anova_logx1 <- aov(log(maxRGR+1) ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
summary(maxRGR_anova_logx1)
posthoc_maxRGR_anova_logx1 <- TukeyHSD(maxRGR_anova_logx1)
posthoc_maxRGR_anova_logx1
threeway <- posthoc_maxRGR_anova_logx1[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(maxRGR_anova_logx1))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova_logx1)) 
qqline(resid(maxRGR_anova_logx1)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova_logx1)) # p-value = 0.01486 




#################################
# transform and re-do the anova #
#################################
####################################
# try a arcsinesqrt transformation #
####################################
maxRGR_anova_asinsqrt <- aov(asin(sqrt(maxRGR)) ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
summary(maxRGR_anova_asinsqrt)
posthoc_maxRGR_anova_asinsqrt <- TukeyHSD(maxRGR_anova_asinsqrt)
posthoc_maxRGR_anova_asinsqrt
threeway <- posthoc_maxRGR_anova_asinsqrt[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(maxRGR_anova_asinsqrt))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova_asinsqrt)) 
qqline(resid(maxRGR_anova_asinsqrt)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova_asinsqrt)) # p-value = 0.01486 



#################################
# transform and re-do the anova #
#################################
##############################
# try a power transformation #
##############################
# power transformation cannot handle (-) values
# convert (-) values to 0 
data$maxRGR 
data$maxRGR[data$maxRGR < 0] <- 0.01

# figure out the best power transformation 
library(car)
powerTransform(maxRGR ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
power <- 0.699505

# add the power transformation of area_stand
data$power_maxRGR <- ((data$maxRGR)^power - 1) / power 

maxRGR_anova_power <- aov(power_maxRGR ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
summary(maxRGR_anova_power)
posthoc_maxRGR_anova_power <- TukeyHSD(maxRGR_anova_power)
posthoc_maxRGR_anova_power
threeway <- posthoc_maxRGR_anova_logx1[[7]]
significant <- subset(threeway, threeway[,4]<=0.050) # significant comparisons 
nonsignif <- subset(threeway, threeway[,4]>0.050) # non-significant comparisons 

#####################
# Examine residuals #
#####################
# plot a histogram 
hist(resid(maxRGR_anova_power))

# QQ plot 
# Does not look very normal 
qqnorm(resid(maxRGR_anova_power)) 
qqline(resid(maxRGR_anova_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_anova_power)) # p-value = 0.01488 


