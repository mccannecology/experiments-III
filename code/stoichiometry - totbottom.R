##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# totbottom                              #
##########################################
# check out the data you will use
head(data_turions)
head(summary_data_turions)

#####################
# anova             #
# WOLFFIA only      #
# Y = totbottom     #
# Treatments:       #
# nitrogen          #
# phosphorus        #
#####################
# Wolffia only    
# remove replicates that were replaced 
data_turions_WB <- subset(data_turions, data_turions$species=="WB")
data_turions_WB <- subset(data_turions_WB, data_turions_WB$replaced=="No")
data_turions_WB

totbottom_anova_WB <- aov(totbottom ~ nitrogen*phosphorus, data=data_turions_WB)
summary(totbottom_anova_WB)
posthoc_totbottom_anova_WB <- TukeyHSD(totbottom_anova_WB)
posthoc_totbottom_anova_WB

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(totbottom_anova_WB))

# QQ plot 
# Does not look very normal 
qqnorm(resid(totbottom_anova_WB)) 
qqline(resid(totbottom_anova_WB)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(totbottom_anova_WB)) # p-value = 0.0436 # Residuals are not noramlly distributed 

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data_turions_WB$totbottom ~ data_turions_WB$nitrogen * data_turions_WB$phosphorus) # p-value = 0.0003024

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(data_turions_WB$totbottom ~ data_turions_WB$nitrogen * data_turions_WB$phosphorus) # p-value = 0.03298

##############################
# try a power transformation #
##############################
data_turions_WB$totbottom <- data_turions_WB$totbottom+0.01

# figure out the best power transformation 
library(car)
powerTransform(totbottom ~ nitrogen*phosphorus, data=data_turions_WB)
power <- 0.4452075

# add the power transformation of stand
data_turions_WB$power_totbottom <- ((data_turions_WB$totbottom)^power - 1) / power 

totbottom_anova_power <- aov(power_totbottom ~ nitrogen*phosphorus, data=data_turions_WB)
summary(totbottom_anova_power)
posthoc_totbottom_anova_power <- TukeyHSD(totbottom_anova_power)
posthoc_totbottom_anova_power

# Examine residuals 
# plot a histogram 
hist(resid(totbottom_anova_power))

# QQ plot 
# Does not look very normal 
qqnorm(resid(totbottom_anova_power)) 
qqline(resid(totbottom_anova_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(totbottom_anova_power)) # p-value = 0.09592 

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data_turions_WB$power_totbottom ~ data_turions_WB$nitrogen * data_turions_WB$phosphorus) # p-value = 0.0006289

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(data_turions_WB$power_totbottom ~ data_turions_WB$nitrogen * data_turions_WB$phosphorus) # p-value = 0.07448



#####################
# anova             #
# SPIRODELA only    #
# Y = totbottom     #
# Treatments:       #
# nitrogen          #
# phosphorus        #
#####################
# An ANOVA is not appropriate
# the response looks like it is Poisson
# mostly 0, some 1s and 2s 
# a glm seems better 
# do the glm below 

# Spirodela only    
# remove replicates that were replaced 
data_turions_SP <- subset(data_turions, data_turions$species=="SP")
data_turions_SP <- subset(data_turions_SP, data_turions_SP$replaced=="No")
data_turions_SP

totbottom_anova_SP <- aov(totbottom ~ nitrogen*phosphorus, data=data_turions_SP)
summary(totbottom_anova_SP)
posthoc_totbottom_anova_SP <- TukeyHSD(totbottom_anova_SP)
posthoc_totbottom_anova_SP

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(totbottom_anova_SP))

# QQ plot 
# Does not look very normal 
qqnorm(resid(totbottom_anova_SP)) 
qqline(resid(totbottom_anova_SP)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(totbottom_anova_SP)) # p-value = 4.578e-06

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(data_turions_SP$totbottom ~ data_turions_SP$nitrogen * data_turions_SP$phosphorus) # p-value = 0.02115

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(data_turions_SP$totbottom ~ data_turions_SP$nitrogen * data_turions_SP$phosphorus) # p-value = 0.09071


##############
# SPIRODELA  #
# totbottom  #
# GLM        #
# Poisson    #
##############
# nitrogen * phosphorus 
glm_totbottomSP_1 <- glm(totbottom ~ nitrogen * phosphorus, family=poisson, data=data_turions_SP)
summary(glm_totbottomSP_1)
AIC(glm_totbottomSP_1)

# Check the significance of the residual deviance 
1-pchisq(24.509,36) # p = 0.9267574

# Output like an ANOVA table
anova(glm_totbottomSP_1, test="Chisq") 

# Tukey's HSD - comparison of treatment means 
library(multcomp)
# compare populations, temperatures  
summary(glht(glm_totbottomSP_1, mcp(nitrogen="Tukey",phosphorus="Tukey")))

# http://www.ats.ucla.edu/stat/r/faq/testing_contrasts.htm
# all pairwise comparsions
# creating a BIG group variable
data_turions_SP$interaction <- with(data_turions_SP, interaction(data_turions_SP$nitrogen, data_turions_SP$phosphorus, sep = "x"))
m2 <- glm(totbottom ~ interaction, family=poisson, data=data_turions_SP)
l2 <- glht(m2, linfct = mcp(interaction = "Tukey"))
summary(l2)




# nitrogen only
glm_totbottomSP_2 <- glm(totbottom ~ nitrogen, family=poisson, data=data_turions_SP)
summary(glm_totbottomSP_2)
AIC(glm_totbottomSP_2)

# phosphorus only
glm_totbottomSP_3 <- glm(totbottom ~ phosphorus, family=poisson, data=data_turions_SP)
summary(glm_totbottomSP_3)
AIC(glm_totbottomSP_3)
