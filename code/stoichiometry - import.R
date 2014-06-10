#################################################
# Analysis of stoichiometry duckweed experiment #
# Conducted - Summer 2012                       #
#                                               #
# Importing data                                #
# Reads "stoichiometry.csv"                     # 
#################################################
library(plyr)

data <- read.csv("stoichiometry.csv") # import area data 
head(data)

# add a new variable that combines plate, row, and column to use as an ID 
data$id <- paste(data$plate,data$row,data$col,sep="")

# add new variables for difference in area 
data$final_minus_initial <- data$area17-data$area0
data$final_divide_initial <- data$area17/data$area0

class(data$nitrogen)
class(data$phosphorus)

# import turion data 
data_turions <- read.csv("stoichiometry_turions.csv")
head(data_turions)

# add turions per day 
data_turions$turions_per_day <- data_turions$totbottom / 17

############################# 
# reshape data              #
# area data                 #
# repeated measures (days)  #
# occur as separate rows    #
#############################
# new variable headings be day and area 
data_area <- reshape(data, 
                     idvar = "id",
                     varying = c("area0","area3","area7","area10","area14","area17"),
                     times = c(0,3,7,10,14,17),
                     timevar = "day",
                     v.names = "area",
                     direction = "long")

# clean-up
row.names(data_area) <- seq(nrow(data_area)) # Re-name the rows so they're not so ugly

# remove unwanted columns 
data_area$rgr1.5 <- NULL
data_area$rgr5 <- NULL
data_area$rgr8.5 <- NULL
data_area$rgr12 <- NULL
data_area$rgr15.5 <- NULL
data_area$minRGR <- NULL
data_area$maxRGR <- NULL
data_area$avgRGR <- NULL
data_area$nitrogen1 <- NULL
data_area$phosphorus1 <- NULL

# re-order my treatments so they go from low to high
data_area$nitrogen <- factor(data_area$nitrogen , levels=c("lowN","medN","highN"))
data_area$phosphorus <- factor(data_area$phosphorus , levels=c("lowP","medP","highP"))

# check it out 
head(data_area)

############################# 
# reshape data              #
# rgr data                  #
# repeated measures (days)  #
# occur as separate rows    #
#############################
# new variable headings be day and area 
data_rgr <- reshape(data, 
                    idvar="id",
                    varying = c("rgr1.5","rgr5","rgr8.5","rgr12","rgr15.5"),
                    times = c(1.5,5,8.5,12,15.5),
                    timevar = "day",
                    v.names = "rgr",
                    direction = "long")

# clean-up
row.names(data_rgr) <- seq(nrow(data_rgr)) # Re-name the rows so they're not so ugly

# remove unwanted columns 
data_rgr$area0 <- NULL
data_rgr$area3 <- NULL
data_rgr$area7 <- NULL
data_rgr$area10 <- NULL
data_rgr$area14 <- NULL
data_rgr$area17 <- NULL
data_rgr$maxRGR <- NULL
data_rgr$minRGR <- NULL
data_rgr$avgRGR <- NULL
data_rgr$nitrogen1 <- NULL
data_rgr$phosphorus1 <- NULL

# re-order my treatments so they go from low to high
data_rgr$nitrogen <- factor(data_rgr$nitrogen , levels=c("lowN","medN","highN"))
data_rgr$phosphorus <- factor(data_rgr$phosphorus , levels=c("lowP","medP","highP"))

# check it out 
head(data_rgr)

#######################
# Mean area           #
# through time        # 
# by treatment combo  #
# Use for plotting    #
#######################
# Area
summary_data_area <- ddply(data_area, c("species","nitrogen","phosphorus","day"), 
                           summarise, 
                           N = sum(!is.na(area)),
                           mean = mean(area,na.rm=T),
                           sd = sd(area,na.rm=T),
                           se = sd / sqrt(N) )
colnames(summary_data_area)[6] <- "area"
# re-order my treatments so they go from low to high
summary_data_area$nitrogen <- factor(summary_data_area$nitrogen , levels=c("lowN","medN","highN"))
summary_data_area$phosphorus <- factor(summary_data_area$phosphorus , levels=c("lowP","medP","highP"))
head(summary_data_area)


###########################
# Mean final-initial area #
# by treatment combo      #
# Use for plotting        #
###########################
# Area
summary_data_area_final_minus_initial <- ddply(data, 
                           c("species","nitrogen","phosphorus"), 
                           summarise, 
                           N = sum(!is.na(final_minus_initial)),
                           mean = mean(final_minus_initial,na.rm=T),
                           sd = sd(final_minus_initial,na.rm=T),
                           se = sd / sqrt(N) )
colnames(summary_data_area_final_minus_initial)[5] <- "final_minus_initial"
# re-order my treatments so they go from low to high
summary_data_area_final_minus_initial$nitrogen <- factor(summary_data_area_final_minus_initial$nitrogen , levels=c("lowN","medN","highN"))
summary_data_area_final_minus_initial$phosphorus <- factor(summary_data_area_final_minus_initial$phosphorus , levels=c("lowP","medP","highP"))
head(summary_data_area_final_minus_initial)


###########################
# Mean final/initial area #
# by treatment combo      #
# Use for plotting        #
###########################
# Area
summary_data_area_final_divide_initial <- ddply(data, 
                                               c("species","nitrogen","phosphorus"), 
                                               summarise, 
                                               N = sum(!is.na(final_divide_initial)),
                                               mean = mean(final_divide_initial,na.rm=T),
                                               sd = sd(final_divide_initial,na.rm=T),
                                               se = sd / sqrt(N) )
colnames(summary_data_area_final_divide_initial)[5] <- "final_divide_initial"
# re-order my treatments so they go from low to high
summary_data_area_final_divide_initial$nitrogen <- factor(summary_data_area_final_divide_initial$nitrogen , levels=c("lowN","medN","highN"))
summary_data_area_final_divide_initial$phosphorus <- factor(summary_data_area_final_divide_initial$phosphorus , levels=c("lowP","medP","highP"))
head(summary_data_area_final_divide_initial)


#######################
# Mean rgr by day     #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr
summary_data_rgr_by_day <- ddply(data_rgr, c("species","nitrogen","phosphorus","day"), 
                          summarise, 
                          N = sum(!is.na(rgr)),
                          mean = mean(rgr,na.rm=T),
                          sd = sd(rgr,na.rm=T),
                          se = sd / sqrt(N) )
colnames(summary_data_rgr_by_day)[6] <- "rgr"
head(summary_data_rgr_by_day)

#######################
# Mean maxRGR         #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr_max
summary_data_maxRGR <- ddply(data, c("species","nitrogen","phosphorus"), 
                              summarise, 
                              N = length(maxRGR),
                              mean = mean(maxRGR),
                              sd = sd(maxRGR),
                              se = sd / sqrt(N) )
colnames(summary_data_maxRGR)[5] <- "maxRGR"
# re-order my treatments so they go from low to high
summary_data_maxRGR$nitrogen <- factor(summary_data_maxRGR$nitrogen , levels=c("lowN","medN","highN"))
summary_data_maxRGR$phosphorus <- factor(summary_data_maxRGR$phosphorus , levels=c("lowP","medP","highP"))
head(summary_data_maxRGR)

#######################
# Mean avgRGR         #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr_max
summary_data_avgRGR <- ddply(data, c("species","nitrogen","phosphorus"), 
                             summarise, 
                             N = length(avgRGR),
                             mean = mean(avgRGR),
                             sd = sd(avgRGR),
                             se = sd / sqrt(N) )
colnames(summary_data_avgRGR)[5] <- "avgRGR"
# re-order my treatments so they go from low to high
summary_data_avgRGR$nitrogen <- factor(summary_data_avgRGR$nitrogen , levels=c("lowN","medN","highN"))
summary_data_avgRGR$phosphorus <- factor(summary_data_avgRGR$phosphorus , levels=c("lowP","medP","highP"))
head(summary_data_avgRGR)

#######################
# Mean totbottom      #
# by treatment combo  #
# Use for plotting    #
# excludes reps. that #
# were replaced       #
#######################
summary_data_turions <- ddply(subset(data_turions, data_turions$replaced=="No"), 
                              c("species","nitrogen","phosphorus"), 
                              summarise, 
                              N = length(totbottom),
                              mean = mean(totbottom),
                              sd = sd(totbottom),
                              se = sd / sqrt(N) )

colnames(summary_data_turions)[5] <- "totbottom"

# re-order my treatments so they go from low to high
summary_data_turions$nitrogen <- factor(summary_data_turions$nitrogen , levels=c("lowN","medN","highN"))
summary_data_turions$phosphorus <- factor(summary_data_turions$phosphorus , levels=c("lowP","medP","highP"))
head(summary_data_turions)

#######################
# Mean totbottom      #
# by treatment combo  #
# Use for plotting    #
# excludes reps. that #
# were replaced       #
#######################
summary_data_turions_per_day <- ddply(subset(data_turions, data_turions$replaced=="No"), 
                                      c("species","nitrogen","phosphorus"), 
                                      summarise, 
                                      N = length(turions_per_day),
                                      mean = mean(turions_per_day),
                                      sd = sd(turions_per_day),
                                      se = sd / sqrt(N) )

colnames(summary_data_turions_per_day)[5] <- "turions_per_day"

# re-order my treatments so they go from low to high
summary_data_turions_per_day$nitrogen <- factor(summary_data_turions_per_day$nitrogen , levels=c("lowN","medN","highN"))
summary_data_turions_per_day$phosphorus <- factor(summary_data_turions_per_day$phosphorus , levels=c("lowP","medP","highP"))
head(summary_data_turions)
summary_data_turions_per_day