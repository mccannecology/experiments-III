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
data$id <- paste(data$plate,data$roaw,data$col,sep="")

############################# 
# reshape data              #
# area data                 #
# repeated measures (days)  #
# occur as separate rows    #
#############################
# new variable headings be day and area 
data_area <- reshape(data, 
                     idvar="id",
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

# check it out 
head(data_rgr)

#######################
# Mean area           #
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
head(summary_data_area)

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
head(summary_data_avgRGR)

