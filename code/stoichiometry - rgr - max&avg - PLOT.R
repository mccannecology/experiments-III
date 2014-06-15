##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# maxRGR & avgRGR on same plot           #
# max within a replicate                 #
##########################################
library(ggplot2)

# check out the data you will use
head(data_rgr)
head(summary_data_rgr_by_day)

head(summary_data_maxRGR)
head(summary_data_avgRGR)

# re-format the data frames for plotting both on the same plot 
summary_data_maxRGR$RGR <- "maximum"
colnames(summary_data_maxRGR)[5] <- "RGRvalue"
summary_data_avgRGR$RGR <- "average"
colnames(summary_data_avgRGR)[5] <- "RGRvalue"

# combine into a single data frame 
summary_data_RGR <- rbind(summary_data_maxRGR,summary_data_avgRGR)


############
# RGR      #
# both max #
# and avg  #
############
summary_data_RGRII <- read.csv("comboRGR_posthoc.csv")
summary_data_RGRII$nitrogen <- factor(summary_data_RGRII$nitrogen , levels=c("lowN","medN","highN"))
summary_data_RGRII$phosphorus <- factor(summary_data_RGRII$phosphorus , levels=c("lowP","medP","highP"))

# labelling the facet variables
nitrogen_names <- list("lowN"="0.5 mg N/L","medN"="5 mg N/L","highN"="10 mg N/L")
phosphorus_names <- list("lowP"="0.08 mg P/L","medP"="0.8 mg P/L","highP"="1.6 mg P/L")

labeller_function <- function(variable,value){
  if (variable=="phosphorus") {
    return(phosphorus_names[value])
  } else {
    return(nitrogen_names[value])
  }
}

# making the plot 
combo_RGR_plot <- ggplot(summary_data_RGRII, aes(x=species,y=RGRvalue,group=RGR)) 
combo_RGR_plot <- combo_RGR_plot + geom_point(aes(shape=RGR),size=3) 
combo_RGR_plot <- combo_RGR_plot + scale_shape_manual(values=c(1,16))
combo_RGR_plot <- combo_RGR_plot + geom_errorbar(aes(ymin=RGRvalue-se, ymax=RGRvalue+se), width=0.1)
combo_RGR_plot <- combo_RGR_plot + facet_grid(nitrogen ~ phosphorus, labeller=labeller_function)
combo_RGR_plot <- combo_RGR_plot + xlab("Species")
combo_RGR_plot <- combo_RGR_plot + ylab(expression(paste("Relative Growth Rate (", mm^2,"/",mm^-2,"/",day^-1,")",sep="")))
combo_RGR_plot <- combo_RGR_plot + geom_text(data=summary_data_RGRII,aes(x=species, y=RGRvalue+se+0.025,label=label1))
combo_RGR_plot <- combo_RGR_plot + geom_text(data=summary_data_RGRII,aes(x=species, y=RGRvalue-se-0.025,label=label2))
combo_RGR_plot <- combo_RGR_plot + theme_bw(base_size=18)
combo_RGR_plot 

# save it 
ggsave(filename = "combo_RGR_plot.jpg", combo_RGR_plot, height=11, width=11)


