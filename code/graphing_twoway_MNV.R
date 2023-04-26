######################################
#
#GRAPHING TWO-WAY ANOVA WITH MNV THREE TIME POINTS   
#
# Created by KW JANUARY 2023 
# modified by K.Weisgerber on 4/26/23
#
############################################
#load libraries
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(emmeans)
library(rstatix)
library(performance)
library(car)
library(openxlsx)

setwd("/Users/kaitl/Desktop/LS_RBCs/raw_data/")

data<-read.csv("rawdata_total.csv")
three<-read.csv("threetimes.csv")
three1<-three %>% 
  convert_as_factor(Group, time, id)
##three1 is the data to use with proper data types 

#subset to remove extreme outliers 
Dthree<- three[c(1:18, 20:53, 55:130, 132:168, 171:271),]

#below reorder the time variables so now they are in order!!! 
Dthree$time<-factor(Dthree$time, levels = c("initial", "post", "final_"))


plot1<-ggplot(Dthree, aes(x = Group, y = MNV, fill = time, color = time))+
  geom_violin(linewidth = 0.75)+
  labs(fill = " Sample Time")+
  geom_point(position=position_jitterdodge(), na.rm = FALSE, shape = 16) +
  scale_color_manual(values = c("initial" = "#595959", "post" = "#6F6F6F", "final_" = "black")) +
  ylab(bquote('Average Modal Nuclei Volume'~ ~ (fL))) +
  xlab('Treatment Group')+ 
  theme(text = element_text(size = 16))+
  theme_classic()
plot1
plot2<- plot1 + scale_x_discrete("Treatment Group", labels = c("CNT" = "Hatchery", "A"= "A", "B"="B", "C"="C", "D"="D"), limits = c("CNT", "A", "B", "C", "D")) +
  scale_fill_manual(name = "Sample Time", labels = c("Initial", "Post-Acclimation", "Final"), values = c('#999999','#C3C3C3', 'white'))
plot2

##now need to add the Mean bar in the middle  
plot3<- plot2 +   stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,
                               geom = "crossbar", color = "black", 
                               width = 0.5,
                               position = position_dodge(width = .9))
plot3
#now they have the mean bar in each 

#need to add statistical significance 

p4<- plot3 +   annotate("text",
                               x = 4, y = 55,
                               label = "*",
                               size = 8)+
  annotate("text",
           x = 5, y = 59,
           label = "*",
           size = 8)
p4
##this ads that all three time levels are different than each other with *
p5<-p4+scale_y_continuous(n.breaks = 10)
p5
#this fixes the y axis so its actually informative

##now need to try to add alphabetical significant differences btwn final measure 

plot5<-p5+ annotate("text",
                     x = 1.3, y = 55,
                     label = "ab",
                     size = 5)+
  annotate("text",
           x = 2.3, y = 54.5,
           label = "ab",
           size = 5)+
  annotate("text",
           x = 3.3, y = 56.5,
           label = "a",
           size = 5)+
  annotate("text",
           x = 4.3, y = 54,
           label = "c",
           size = 5)+
  annotate("text",
           x = 5.3, y = 54,
           label = "bc",
           size = 5)
plot5
##perfect!! final touches if we can adjust the legend; added back above code to relabl legend 

plot6<-plot5+guides(col = FALSE)
plot6

ggsave(filename = "Fig1.tiff", plot = plot6, dpi = 300, height = 6, width = 7)
