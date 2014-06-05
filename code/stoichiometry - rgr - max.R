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
mean_maxRGR_plot <- mean_maxRGR_plot + ylab("Maximum RGR")
mean_maxRGR_plot <- mean_maxRGR_plot + theme_gray(base_size=18)
mean_maxRGR_plot 

#####################
# Preliminary anova #
# Y = maxRGR        #
# Treatments:       #
# species,          #
# nutrients,        #
# temperature,      #
#####################
maxRGR_anova <- aov(maxRGR ~ species*nitrogen*phosphorus, data=data)
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

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data$maxRGR ~ data$species) # p-value = 0.002015



#################################
# transform and re-do the anova #
#################################
###############################
# try a sqrt+1 transformation #
###############################
maxRGR_anova_sqrt <- aov(sqrt(maxRGR+1) ~ species*nitrogen*phosphorus, data=data)
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
maxRGR_anova_logx1 <- aov(log(maxRGR+1) ~ species*nitrogen*phosphorus, data=data)
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
data$maxRGR[data$maxRGR <= 0] <- 0.001

# figure out the best power transformation 
library(car)
powerTransform(maxRGR ~ species*as.factor(nitrogen)*as.factor(phosphorus), data=data)
power <- 0.7332737

# add the power transformation of stand
data$power_maxRGR <- ((data$maxRGR)^power - 1) / power 

maxRGR_anova_power <- aov(power_maxRGR ~ species*nitrogen*phosphorus, data=data)
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
shapiro.test(resid(maxRGR_anova_power)) # p-value = 0.02066 






######################################
# ANOVA @ each treatment combination #
######################################
###############
# Low N Low P #
###############
maxRGR_LL_anova <- aov(maxRGR~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="lowP"))
summary(maxRGR_LL_anova)
TukeyHSD(maxRGR_LL_anova)

hist(resid(maxRGR_LL_anova)) # plot a histogram 

qqnorm(resid(maxRGR_LL_anova)) # QQ plot 
qqline(resid(maxRGR_LL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_LL_anova)) # p-value =  0.4188

# Bartlett's test of homogeneity of variances 
# null hypothesis: equal variance 
bartlett.test(maxRGR~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="lowP"))

###############
# Med N Low P #
###############
maxRGR_ML_anova <- aov(maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))
summary(maxRGR_ML_anova)
TukeyHSD(maxRGR_ML_anova)

hist(resid(maxRGR_ML_anova)) # plot a histogram 

qqnorm(resid(maxRGR_ML_anova)) # QQ plot 
qqline(resid(maxRGR_ML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_ML_anova)) # p-value =  0.03546

# logx+1 transformation 
logx1_maxRGR_ML_anova <- aov(log(maxRGR+1) ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))
summary(logx1_maxRGR_ML_anova)
TukeyHSD(logx1_maxRGR_ML_anova)

hist(resid(logx1_maxRGR_ML_anova)) # plot a histogram 

qqnorm(resid(logx1_maxRGR_ML_anova)) # QQ plot 
qqline(resid(logx1_maxRGR_ML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(logx1_maxRGR_ML_anova)) # p-value =  0.04691

# sqrt+1 transformation 
sqrt_maxRGR_ML_anova <- aov(sqrt(maxRGR+1) ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))
summary(sqrt_maxRGR_ML_anova)
TukeyHSD(sqrt_maxRGR_ML_anova)

hist(resid(sqrt_maxRGR_ML_anova)) # plot a histogram 

qqnorm(resid(sqrt_maxRGR_ML_anova)) # QQ plot 
qqline(resid(sqrt_maxRGR_ML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(sqrt_maxRGR_ML_anova)) # p-value =  0.03969

# arcsine sqrt transformation 
asinsqrt_maxRGR_ML_anova <- aov(asin(sqrt(maxRGR)) ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))
summary(asinsqrt_maxRGR_ML_anova)
TukeyHSD(asinsqrt_maxRGR_ML_anova)

hist(resid(asinsqrt_maxRGR_ML_anova)) # plot a histogram 

qqnorm(resid(asinsqrt_maxRGR_ML_anova)) # QQ plot 
qqline(resid(asinsqrt_maxRGR_ML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(asinsqrt_maxRGR_ML_anova)) # p-value =  0.03941

# Bartlett's test of homogeneity of variances 
# null hypothesis: equal variance 
bartlett.test(asin(sqrt(maxRGR)) ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))

# power transformation
library(car)
powerTransform(maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))
power <- 0.7094263 

# add the power transformation
data$power_maxRGR <- ((data$maxRGR)^power - 1) / power 

power_maxRGR_ML_anova <- aov(power_maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP"))
summary(power_maxRGR_ML_anova)
posthoc_power_maxRGR_ML_anova <- TukeyHSD(power_maxRGR_ML_anova)
posthoc_power_maxRGR_ML_anova

# Examine residuals 
hist(resid(power_maxRGR_ML_anova)) # plot a histogram 

qqnorm(resid(power_maxRGR_ML_anova)) # QQ plot 
qqline(resid(power_maxRGR_ML_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_maxRGR_ML_anova)) # p-value = 0.04574

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP")) # 0.01556

# levene's test: homogeneity of variance 
# null hypothesis: equal variance 
library(car)
leveneTest(power_maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="lowP")) # 0.2281

################
# High N Low P #
################
maxRGR_HL_anova <- aov(maxRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="lowP"))
summary(maxRGR_HL_anova)
TukeyHSD(maxRGR_HL_anova)

hist(resid(maxRGR_HL_anova)) # plot a histogram 

qqnorm(resid(maxRGR_HL_anova)) # QQ plot 
qqline(resid(maxRGR_HL_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_HL_anova)) # p-value =  0.426

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="lowP"))


###############
# Low N Med P #
###############
maxRGR_LM_anova <- aov(maxRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="medP"))
summary(maxRGR_LM_anova)
TukeyHSD(maxRGR_LM_anova)

hist(resid(maxRGR_LM_anova)) # plot a histogram 

qqnorm(resid(maxRGR_LM_anova)) # QQ plot 
qqline(resid(maxRGR_LM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_LM_anova)) # p-value =  0.3333

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="medP"))

################
# Low N High P #
################
maxRGR_LH_anova <- aov(maxRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP"))
summary(maxRGR_LH_anova)
TukeyHSD(maxRGR_LH_anova)

hist(resid(maxRGR_LH_anova)) # plot a histogram 

qqnorm(resid(maxRGR_LH_anova)) # QQ plot 
qqline(resid(maxRGR_LH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_LH_anova)) # p-value =  0.005253

# try a power transformation 
library(car)
powerTransform(maxRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP"))
power <- 0.2961235 

# add the power transformation
data$power_maxRGR <- ((data$maxRGR)^power - 1) / power 

power_maxRGR_LH_anova <- aov(power_maxRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP"))
summary(power_maxRGR_LH_anova)
posthoc_power_maxRGR_LH_anova <- TukeyHSD(power_maxRGR_LH_anova)
posthoc_power_maxRGR_LH_anova

# Examine residuals 
hist(resid(power_maxRGR_LH_anova)) # plot a histogram 

qqnorm(resid(power_maxRGR_LH_anova)) # QQ plot 
qqline(resid(power_maxRGR_LH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_maxRGR_LH_anova)) # p-value = 0.07402

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_maxRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP")) # p-value = 0.04142

# levene's test: homogeneity of variance 
# null hypothesis: equal variance 
library(car)
leveneTest(power_maxRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP")) # 0.2281

###############
# Med N Med P #
###############
maxRGR_MM_anova <- aov(maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="medP"))
summary(maxRGR_MM_anova)
TukeyHSD(maxRGR_MM_anova)

hist(resid(maxRGR_MM_anova)) # plot a histogram 

qqnorm(resid(maxRGR_MM_anova)) # QQ plot 
qqline(resid(maxRGR_MM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_MM_anova)) # p-value =  0.06162

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="medP")) # 0.4611

#################
# High N High P #
#################
maxRGR_HH_anova <- aov(maxRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="highP"))
summary(maxRGR_HH_anova)
TukeyHSD(maxRGR_HH_anova)

hist(resid(maxRGR_HH_anova)) # plot a histogram 

qqnorm(resid(maxRGR_HH_anova)) # QQ plot 
qqline(resid(maxRGR_HH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_HH_anova)) # p-value =  0.04204

# try a power transformation 
library(car)
powerTransform(maxRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="highP"))
power <- 1.924551 

# add the power transformation
data$power_maxRGR <- ((data$maxRGR)^power - 1) / power 

power_maxRGR_HH_anova <- aov(power_maxRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="highP"))
summary(power_maxRGR_HH_anova)
posthoc_power_maxRGR_HH_anova <- TukeyHSD(power_maxRGR_HH_anova)
posthoc_power_maxRGR_HH_anova

# Examine residuals 
hist(resid(power_maxRGR_HH_anova)) # plot a histogram 

qqnorm(resid(power_maxRGR_HH_anova)) # QQ plot 
qqline(resid(power_maxRGR_HH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(power_maxRGR_HH_anova)) # p-value = 0.2587

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_maxRGR ~ species, data=subset(data, data$nitrogen=="lowN" & data$phosphorus=="highP")) # p-value = 0.001893

################
# Med N High P #
################
maxRGR_MH_anova <- aov(maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="highP"))
summary(maxRGR_MH_anova)
TukeyHSD(maxRGR_MH_anova)

hist(resid(maxRGR_MH_anova)) # plot a histogram 

qqnorm(resid(maxRGR_MH_anova)) # QQ plot 
qqline(resid(maxRGR_MH_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_MH_anova)) # p-value =  0.7518

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="highP"))

# levene's test: homogeneity of variance 
# null hypothesis: equal variance 
library(car)
leveneTest(maxRGR ~ species, data=subset(data, data$nitrogen=="medN" & data$phosphorus=="highP"))

################
# High N Med P #
################
maxRGR_HM_anova <- aov(maxRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="medP"))
summary(maxRGR_HM_anova)
TukeyHSD(maxRGR_HM_anova)

hist(resid(maxRGR_HM_anova)) # plot a histogram 

qqnorm(resid(maxRGR_HM_anova)) # QQ plot 
qqline(resid(maxRGR_HM_anova)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(maxRGR_HM_anova)) # p-value =  0.5111

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(maxRGR ~ species, data=subset(data, data$nitrogen=="highN" & data$phosphorus=="medP"))
