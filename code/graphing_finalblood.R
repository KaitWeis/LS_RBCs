######################################
#
# GRAPHING EACH FINAL BLOOD MEASUREMENT    
#
# Created by KW JANUARY 2023 
# modified by K.Weisgerber on 5/1/23
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
library(moments)
library(stats)
library(pwr)
library(patchwork)
library(beeswarm)

setwd("/Users/kaitl/Desktop/LS_RBCs/raw_data/")

data<-read.csv("rawdata_total.csv")

#########################################
#hemoglobin - violin plot 
Hb<-ggplot(data, mapping = aes(x = Group, y= conc_HB_gmdL)) + 
  geom_violin(lwd = 0.75)+ 
  geom_jitter(width = 0.2)+
  stat_summary(fun = "mean",
               geom = "crossbar",
               linewidth = 0.5,
               color = "grey")+
  geom_point() +
  theme_classic() +
  ylab(bquote('[Hemoglobin]'~ ~ (g/dL))) +
  xlab('Treatment Group')+ 
  theme(axis.title.x = element_blank(),text = element_text(size = 15))
Hb
Hb1<-Hb + scale_x_discrete("Treatment Group", labels = c("CNT" = "Hatchery", "A"= "A", "B"="B", "C"="C", "D"="D"), limits = c("CNT", "A", "B", "C", "D"))
Hb1
##there is no significant differences in HB, this plot good 

############################################
Hct<-ggplot(data, mapping = aes(x = Group, y= PCV)) + 
  geom_violin(lwd = 0.75)+ 
  geom_jitter(width = 0.2)+
  stat_summary(fun = "mean",
               geom = "crossbar",
               linewidth = 0.5,
               color = "grey")+
  geom_point() +
  theme_classic() +
  ylab(bquote('% Packed Cell Volume')) +
  xlab('Treatment Group')+ 
  theme(axis.title.x = element_blank(),text = element_text(size = 15))
Hct
Hct1<-Hct + scale_x_discrete("Treatment Group", labels = c("CNT" = "Hatchery", "A"= "A", "B"="B", "C"="C", "D"="D"), limits = c("CNT", "A", "B", "C", "D"))
Hct1
##add significance from anova 
Hct2<- Hct1 +   annotate("text",
                        x = 1, y = 42,
                        label = "a",
                        size = 6)+
  annotate("text",
           x = 2, y = 33,
           label = "ab",
           size = 6)+
  annotate("text",
           x = 3, y = 48,
           label = "ab",
           size = 6)+
  annotate("text",
           x = 4, y = 32,
           label = "ab",
           size = 6)+
  annotate("text",
           x = 5, y = 41,
           label = "b",
           size = 6)
Hct2
###this graph looks good 

#################################################
##RBC concentration 

RBC<-ggplot(data, mapping = aes(x = Group, y= conc_cells_mm3)) + 
  geom_violin(lwd = 0.75)+ 
  geom_jitter(width = 0.2)+
  stat_summary(fun = "mean",
               geom = "crossbar",
               linewidth = 0.5,
               color = "grey")+
  geom_point() +
  theme_classic() +
  ylab(bquote('[RBCs]'~ ~ (mm^3))) +
  xlab('Treatment Group')+ 
  theme(text = element_text(size = 15))
RBC
RBC1<- RBC + scale_x_discrete("Treatment Group", labels = c("CNT" = "Hatchery", "A"= "A", "B"="B", "C"="C", "D"="D"), limits = c("CNT", "A", "B", "C", "D"))
RBC1
RBC2<- RBC1 + annotate("text",
                         x = 1, y = 770000,
                         label = "a",
                         size = 6)+
  annotate("text",
           x = 2, y = 1150000,
           label = "b",
           size = 6)+
  annotate("text",
           x = 3, y = 1100000,
           label = "b",
           size = 6)+
  annotate("text",
           x = 4, y = 1000000,
           label = "b",
           size = 6)+
  annotate("text",
           x = 5, y = 1100000,
           label = "b",
           size = 6)
RBC2
##now just change y axis ticks 
RBC3<- RBC2 +scale_y_continuous(n.breaks = 7)
RBC3
##looks good 

#############################################
MEH<-ggplot(data, mapping = aes(x = Group, y= MEH)) + 
  geom_violin(lwd = 0.75)+ 
  geom_jitter(width = 0.2)+
  stat_summary(fun = "mean",
               geom = "crossbar",
               linewidth = 0.5,
               color = "grey")+
  geom_point() +
  theme_classic() +
  ylab(bquote('MEH'~ ~ (gHb/cell))) +
  xlab('Treatment Group')+ 
  theme(axis.title.x = element_blank(),text = element_text(size = 15))
MEH
MEH1<- MEH + scale_x_discrete("Treatment Group", labels = c("CNT" = "Hatchery", "A"= "A", "B"="B", "C"="C", "D"="D"), limits = c("CNT", "A", "B", "C", "D"))
MEH1
MEH2<- MEH1 + annotate("text",
                       x = 1, y = 500,
                       label = "a",
                       size = 6)+
  annotate("text",
           x = 2, y = 260,
           label = "b",
           size = 6)+
  annotate("text",
           x = 3, y = 350,
           label = "b",
           size = 6)+
  annotate("text",
           x = 4, y = 338,
           label = "ab",
           size = 6)+
  annotate("text",
           x = 5, y = 310,
           label = "ab",
           size = 6)
MEH2
###looks good 

########################################
MEV<-ggplot(data, mapping = aes(x = Group, y= MEV)) + 
  geom_violin(lwd = 0.75)+ 
  geom_jitter(width = 0.2)+
  stat_summary(fun = "mean",
               geom = "crossbar",
               linewidth = 0.5,
               color = "grey")+
  geom_point() +
  theme_classic() +
  ylab(bquote('MEV'~ ~ (mm^3/cell))) +
  xlab('Treatment Group')+ 
  theme(axis.title.x = element_blank(),text = element_text(size = 15))
MEV
MEV1<- MEV + scale_x_discrete("Treatment Group", labels = c("CNT" = "Hatchery", "A"= "A", "B"="B", "C"="C", "D"="D"), limits = c("CNT", "A", "B", "C", "D"))
MEV1
MEV2<- MEV1 + annotate("text",
                       x = 1, y = 2300,
                       label = "a",
                       size = 6)+
  annotate("text",
           x = 2, y = 1000,
           label = "b",
           size = 6)+
  annotate("text",
           x = 3, y = 1100,
           label = "b",
           size = 6)+
  annotate("text",
           x = 4, y = 1700,
           label = "b",
           size = 6)+
  annotate("text",
           x = 5, y = 1300,
           label = "b",
           size = 6)
MEV2
##great 

#######################################################
MEHC<-ggplot(data, mapping = aes(x = Group, y= MEHC)) + 
  geom_violin(lwd = 0.75)+ 
  geom_jitter(width = 0.2)+
  stat_summary(fun = "mean",
               geom = "crossbar",
               linewidth = 0.5,
               color = "grey")+
  geom_point() +
  theme_classic() +
  ylab(bquote('MEHC'~ ~ (gHb/mm^3))) +
  xlab('Treatment Group')+ 
  theme(text = element_text(size = 15))
MEHC
MEHC1<- MEHC + scale_x_discrete("Treatment Group", labels = c("CNT" = "Hatchery", "A"= "A", "B"="B", "C"="C", "D"="D"), limits = c("CNT", "A", "B", "C", "D"))
MEHC1
MEHC2<- MEHC1 + annotate("text",
                       x = 1, y = 49,
                       label = "a",
                       size = 6)+
  annotate("text",
           x = 2, y = 58,
           label = "ab",
           size = 6)+
  annotate("text",
           x = 3, y = 48,
           label = "a",
           size = 6)+
  annotate("text",
           x = 4, y = 52,
           label = "ab",
           size = 6)+
  annotate("text",
           x = 5, y = 65,
           label = "b",
           size = 6)
MEHC2
#########################################
#build a patchwro kof these blood measrues all together 

AllBlood<-(Hb1 | Hct2) /
  (RBC3| MEH2) /
  (MEV2 | MEHC2)+
  plot_annotation(tag_levels = 'A')
AllBlood
ggsave(filename = "Allblood.tiff", plot = AllBlood, dpi = 300, height = 16, width = 14)

##try making font one smaller on last plot because it gets cut off 

##okay yea but now need to make font smaller on all 

##try saving this within their size guidelines for journal.. not sure if it will work
ggsave(filename = "Allblood1.tiff", plot = AllBlood, dpi = 600, height = 9, width = 7)
#yea that does not work... 

##lets try breaking these into 3 v. 3 graphs? 

F3<- (Hb1) /
  (Hct2) / 
  (RBC3)+ 
  plot_annotation(tag_levels = 'A')
F3
ggsave(filename = "HB_hct_RBC.tiff", plot = F3, dpi = 600, height = 9, width = 7)

##okay that works 

##other three 
N3<- (MEH2) /
  (MEV2) / 
  (MEHC2)+ 
  plot_annotation(tag_levels = 'A')
N3
ggsave(filename = "MEH_V_HC.tiff", plot = N3, dpi = 600, height = 9, width = 7)


##okay those look better split into 2 graphs of 3
##finally, make a version with the x axis labels removed from extras 

First3<- (Hb1) /
  (Hct2) / 
  (RBC3)+ 
  plot_annotation(tag_levels = 'A')
First3
ggsave(filename = "Fig2.tiff", plot = First3, dpi = 600, height = 9, width = 7)

Next3<- (MEH2) /
  (MEV2) / 
  (MEHC2)+ 
  plot_annotation(tag_levels = 'A')
Next3
ggsave(filename = "Fig3.tiff", plot = Next3, dpi = 600, height = 9, width = 7)

##okay these look good! 