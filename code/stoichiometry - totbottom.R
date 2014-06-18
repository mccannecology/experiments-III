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

# WB & SP only    
# remove replicates that were replaced 
data_turions_WBSP <- subset(data_turions, data_turions$species!="LM")
data_turions_WBSP <- subset(data_turions_WBSP, data_turions_WBSP$replaced=="No")
data_turions_WBSP


# I probably shouldn't use a SP and WB in the same ANOVA 
# Turion production for SP is 0 in 5 of 9 treatment levels 

###############################
# anova                       #
# Y = turion_area_per_day     #
# Treatments:                 #
# nitrogen                    #
# phosphorus                  #
# species                     #
# (NO LM, replaced removed)   #
###############################
anova_turion_area_per_day <- aov(turion_area_per_day ~ nitrogen*phosphorus*species, data=data_turions_WBSP)
summary(anova_turion_area_per_day)
posthoc_anova_turion_area_per_day <- TukeyHSD(anova_turion_area_per_day)
posthoc_anova_turion_area_per_day

# Examine residuals 

# plot a histogram 
hist(resid(anova_turion_area_per_day))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_turion_area_per_day)) 
qqline(resid(anova_turion_area_per_day)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_turion_area_per_day)) # p-value = 6.055e-08

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(turion_area_per_day ~ nitrogen*phosphorus*species,data=data_turions_WBSP) # p-value = 2.766e-06

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(turion_area_per_day ~ nitrogen*phosphorus*species,data=data_turions_WBSP) # p-value = 0.0001461

##############################
# try a power transformation #
##############################
data_turions_WBSP$turion_area_per_day <- data_turions_WBSP$turion_area_per_day+0.001

# figure out the best power transformation 
library(car)
powerTransform(turion_area_per_day ~ nitrogen*phosphorus*species, data=data_turions_WBSP)
power <- 0.1089096

# add the power transformation of stand
data_turions_WBSP$power_turion_area_per_day <- ((data_turions_WBSP$turion_area_per_day)^power - 1) / power 

anova_power_turion_area_per_day <- aov(power_turion_area_per_day ~ nitrogen*phosphorus*species, data=data_turions_WBSP)
summary(anova_power_turion_area_per_day)
posthoc_anova_power_turion_area_per_day <- TukeyHSD(anova_power_turion_area_per_day)
posthoc_anova_power_turion_area_per_day

# Examine residuals 
# plot a histogram 
hist(resid(anova_power_turion_area_per_day))

# QQ plot 
# Does not look very normal 
qqnorm(resid(anova_power_turion_area_per_day)) 
qqline(resid(anova_power_turion_area_per_day)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_power_turion_area_per_day)) # p-value = 7.73e-05

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_turion_area_per_day ~ nitrogen*phosphorus*species, data=data_turions_WBSP) # p-value = 0.2827

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(power_turion_area_per_day ~ nitrogen*phosphorus*species, data=data_turions_WBSP) # p-value = 0.003177



###############################
# anova                       #
# WOLFFIA only                #
# Y = turion_area_per_day     #
# Treatments:                 #
# nitrogen                    #
# phosphorus                  #
###############################
# Wolffia only    
# remove replicates that were replaced 
data_turions_WB <- subset(data_turions, data_turions$species=="WB")
data_turions_WB <- subset(data_turions_WB, data_turions_WB$replaced=="No")
data_turions_WB

turion_area_per_day_anova_WB <- aov(turion_area_per_day ~ nitrogen*phosphorus, data=data_turions_WB)
summary(turion_area_per_day_anova_WB)
posthoc_turion_area_per_day_anova_WB <- TukeyHSD(turion_area_per_day_anova_WB)
posthoc_turion_area_per_day_anova_WB

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(turion_area_per_day_anova_WB))

# QQ plot 
# Does not look very normal 
qqnorm(resid(turion_area_per_day_anova_WB)) 
qqline(resid(turion_area_per_day_anova_WB)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(turion_area_per_day_anova_WB)) # p-value 0.04363

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(turion_area_per_day ~ nitrogen * phosphorus,data=data_turions_WB) # p-value = 0.0003025

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(turion_area_per_day ~ nitrogen * phosphorus,data=data_turions_WB) # p-value = 0.03296

##############################
# try a power transformation #
##############################
data_turions_WB$turion_area_per_day <- data_turions_WB$turion_area_per_day+0.001

# figure out the best power transformation 
library(car)
powerTransform(turion_area_per_day ~ nitrogen*phosphorus, data=data_turions_WB)
power <- 0.4567131 

# add the power transformation of stand
data_turions_WB$power_turion_area_per_day <- ((data_turions_WB$turion_area_per_day)^power - 1) / power 

turion_area_per_day_anova_power <- aov(power_turion_area_per_day ~ nitrogen*phosphorus, data=data_turions_WB)
summary(turion_area_per_day_anova_power)
posthoc_turion_area_per_day_anova_power <- TukeyHSD(turion_area_per_day_anova_power)
posthoc_turion_area_per_day_anova_power

# Examine residuals 
# plot a histogram 
hist(resid(turion_area_per_day_anova_power))

# QQ plot 
# Does not look very normal 
qqnorm(resid(turion_area_per_day_anova_power)) 
qqline(resid(turion_area_per_day_anova_power)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(turion_area_per_day_anova_power)) # p-value = 0.2418

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(power_turion_area_per_day ~ nitrogen*phosphorus, data=data_turions_WB) # p-value = 0.0001708

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(power_turion_area_per_day ~ nitrogen*phosphorus, data=data_turions_WB) # p-value = 0.1365





###############################
# anova                       #
# SPIRODELA only              #
# Y = turion_area_per_day     #
# Treatments:                 #
# nitrogen                    #
# phosphorus                  #
###############################
# Spirodela only    
# remove replicates that were replaced 
data_turions_SP <- subset(data_turions, data_turions$species=="SP")
data_turions_SP <- subset(data_turions_SP, data_turions_SP$replaced=="No")
data_turions_SP

turion_area_per_day_anova_SP <- aov(turion_area_per_day ~ nitrogen*phosphorus, data=data_turions_SP)
summary(turion_area_per_day_anova_SP)
posthoc_turion_area_per_day_anova_SP <- TukeyHSD(turion_area_per_day_anova_SP)
posthoc_turion_area_per_day_anova_SP

#####################
# Examine residuals #
#####################
# plot a histogram 
# looks normal-ish
hist(resid(turion_area_per_day_anova_SP))

# QQ plot 
# Does not look very normal 
qqnorm(resid(turion_area_per_day_anova_SP)) 
qqline(resid(turion_area_per_day_anova_SP)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(turion_area_per_day_anova_SP)) # p-value 4.578e-06

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(turion_area_per_day ~ nitrogen * phosphorus,data=data_turions_SP) # p-value = 0.0003025

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(turion_area_per_day ~ nitrogen * phosphorus,data=data_turions_SP) # p-value = 0.03296







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




#######################
# Logistic regression #
# GLM                 #
#                     #
# Presence of turions #
#######################
data_turions$turion_presence[data_turions$totbottom>0] <- 1
data_turions$turion_presence[data_turions$totbottom==0] <- 0
data_turions$turion_presence

glm_turions <- glm(turion_presence ~ species * nitrogen * phosphorus, family=binomial, data=data_turions)
summary(glm_turions)
AIC(glm_turions)

# Check the significance of the residual deviance 
1-pchisq(49.684,135) # p = 1

# Output like an ANOVA table4
anova(glm_turions, test="Chisq") 

# Tukey's HSD - comparison of treatment means 
library(multcomp)
# compare populations, temperatures  
summary(glht(glm_turions, mcp(species="Tukey",nitrogen="Tukey",phosphorus="Tukey")))

# http://www.ats.ucla.edu/stat/r/faq/testing_contrasts.htm
# all pairwise comparsions
# creating a BIG group variable
data_turions$interaction <- with(data_turions, interaction(data_turions$species,data_turions$nitrogen, data_turions$phosphorus, sep = "x"))
# re-do the glm() with the "interaction" as the predictor 
m2 <- glm(tuion_presence ~ interaction, family=binomial, data=data_turions)
l2 <- glht(m2, linfct = mcp(interaction = "Tukey"))
summary(l2)


# nitrogen only
glm_turions_2 <- glm(turion_presence ~ nitrogen, family=binomial, data=data_turions)
summary(glm_turions_2)
AIC(glm_turions_2)

# phosphorus only
glm_turions_3 <- glm(turion_presence ~ phosphorus, family=binomial, data=data_turions)
summary(glm_turions_3)
AIC(glm_turions_3)

# species only
glm_turions_4 <- glm(turion_presence ~ species, family=binomial, data=data_turions)
summary(glm_turions_4)
AIC(glm_turions_4)

# Check the significance of the residual deviance 
1-pchisq( 81.792,159) # p = 0.9999999

# likelihood ratio test 
# http://www.ats.ucla.edu/stat/r/dae/logit.htm
# difference in deviance for the two models (i.e., the test statistic) 
with(glm_turions_4, null.deviance - deviance)
# df for difference between two models 
with(glm_turions_4, df.null - df.residual)
# p- value 
with(glm_turions_4, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# http://www.r-bloggers.com/veterinary-epidemiologic-research-glm-evaluating-logistic-regression-models-part-3/
sum(residuals(glm_turions_4,type="pearson")^2)
deviance(glm_turions_4)
1-pchisq(deviance(glm_turions_4),df.residual(glm_turions_4))

# Output like an ANOVA table4
anova(glm_turions_4, test="Chisq") 

# Tukey's HSD - comparison of treatment means 
library(multcomp)
# compare populations, temperatures  
summary(glht(glm_turions_4, mcp(species="Tukey")))


#######################
# Separate t-tests    #
# at each N*P level   #
# were SP&WB > 0      #
#                     #
# turion_area_per_day #
#######################

#########################
# Dunn-Sidak correction #
# adjusted critical p   #
#########################
# four comparions 
1-(1-0.05)^(1/4)
# 0.01274146

###############
# Low N Med P #
###############
anova_turions_LM <- aov(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="lowN" & data_turions_WBSP$phosphorus=="medP"))
summary(anova_turions_LM)

# examine the residuals 
hist(resid(anova_turions_LM))

# QQ plot of residuals
qqnorm(resid(anova_turions_LM)) 
qqline(resid(anova_turions_LM)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_turions_LM)) # p-value = 0.4951

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="lowN" & data_turions_WBSP$phosphorus=="medP")) #p=0.6724

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="lowN" & data_turions_WBSP$phosphorus=="medP")) #p=0.7542

# Alternatively, I can use a Wilcoxon, signed rank test 
wilcox.test(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="lowN" & data_turions_WBSP$phosphorus=="medP")) #p=0.007408

################
# Low N High P #
################
anova_turions_LH <- aov(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="lowN" & data_turions_WBSP$phosphorus=="highP"))
summary(anova_turions_LH)

# examine the residuals 
hist(resid(anova_turions_LH))

# QQ plot of residuals
qqnorm(resid(anova_turions_LH)) 
qqline(resid(anova_turions_LH)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_turions_LH)) # p-value = 0.06798

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="lowN" & data_turions_WBSP$phosphorus=="highP")) #p=0.6671

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="lowN" & data_turions_WBSP$phosphorus=="highP")) #p=0.4559

# Alternatively, I can use a Wilcoxon, signed rank test 
wilcox.test(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="lowN" & data_turions_WBSP$phosphorus=="highP")) #p=0.004267

###############
# Med N Low P #
###############
anova_turions_ML <- aov(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="medN" & data_turions_WBSP$phosphorus=="lowP"))
summary(anova_turions_ML)

# examine the residuals 
hist(resid(anova_turions_ML))

# QQ plot of residuals
qqnorm(resid(anova_turions_ML)) 
qqline(resid(anova_turions_ML)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_turions_ML)) # p-value = 0.148

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="medN" & data_turions_WBSP$phosphorus=="lowP")) # p=0.06186

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="medN" & data_turions_WBSP$phosphorus=="lowP")) #p=0.1851

# Alternatively, I can use a Wilcoxon, signed rank test 
wilcox.test(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="medN" & data_turions_WBSP$phosphorus=="lowP")) #p=0.004337

################
# High N Low P # 
################
anova_turions_HL <- aov(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="highN" & data_turions_WBSP$phosphorus=="lowP"))
summary(anova_turions_HL)

# examine the residuals 
hist(resid(anova_turions_HL))

# QQ plot of residuals
qqnorm(resid(anova_turions_HL)) 
qqline(resid(anova_turions_HL)) 

# null hypothesis = sample came from a normally distributed population 
shapiro.test(resid(anova_turions_HL)) # p-value = 0.3946

# Bartlett Test of Homogeneity of Variances
# null hypothesis = population variances are equal
bartlett.test(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="highN" & data_turions_WBSP$phosphorus=="lowP")) # p=0.06082

# Levene's Test of Homogeneity of Variances
# null hypothesis = population variances are equal
library(car)
leveneTest(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="highN" & data_turions_WBSP$phosphorus=="lowP")) #p=0.2326

# Alternatively, I can use a Wilcoxon, signed rank test 
wilcox.test(turion_area_per_day ~ species, data=subset(data_turions_WBSP, data_turions_WBSP$nitrogen=="highN" & data_turions_WBSP$phosphorus=="lowP")) #p=0.1081
