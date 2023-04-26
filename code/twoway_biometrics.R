######################################################
#
#TWO-WAY ANOVA WITH BIOMETRICS AT THREE TIME POINTS   
#
# Created by KW JANUARY 2023 
# modified by K.Weisgerber on 4/26/23
#
#####################################################
#load libraries
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(emmeans)
library(moments)
library(performance)
library(rstatix)
library(stats)
library(pwr)

setwd("/Users/kaitl/Desktop/LS_RBCs/raw_data/")


expr<-read.csv("threetimes.csv")
mixbio<-expr %>% 
  convert_as_factor(Group, time, id)

#starting with length 

#summary stats 
mixbio %>% 
  group_by(time, Group) %>% 
  get_summary_stats(length_mm, type = "mean_sd")

bxp_mm<- ggboxplot(mixbio, x = "Group", y = "length_mm",
                   color = "time")
bxp_mm

mixlen_out<-mixbio %>% 
  group_by(time, Group) %>% 
  identify_outliers(length_mm)
#one that isnt extreme 

mixbio %>% 
  group_by(time, Group) %>% 
  shapiro_test(length_mm)
#these are all normal 

ggqqplot(mixbio, "length_mm", ggtheme = theme_bw()) +
  facet_grid(Group ~ time)
#these look great! 

mixbio %>% 
  group_by(time) %>% 
  levene_test(length_mm ~ Group)
#normal 
mixbio %>% 
  group_by(Group) %>% 
  levene_test(length_mm ~ time)
#also normal 


##now lets do for weight 
bxp_g<- ggboxplot(mixbio, x = "Group", y = "mass_g",
                  color = "time")
bxp_g

mixmass_out<-mixbio %>% 
  group_by(time, Group) %>% 
  identify_outliers(mass_g)
#two that arent extreme 

mixbio %>% 
  group_by(time, Group) %>% 
  shapiro_test(mass_g)
#control final mass may not be normal 

ggqqplot(mixbio, "mass_g", ggtheme = theme_bw()) +
  facet_grid(Group ~ time)
#these look great! 

mixbio %>% 
  group_by(time) %>% 
  levene_test(mass_g ~ Group)
#normal 
mixbio %>% 
  group_by(Group) %>% 
  levene_test(mass_g ~ time)
#all normal 


len_aov<-aov(length_mm ~ Group * time, data = mixbio)
Anova(len_aov, type = "III")
#Response: length_mm
#Sum Sq  Df   F value    Pr(>F)    
#(Intercept) 202021   1 1515.0650 < 2.2e-16 ***
#  Group          712   4    1.3355  0.257660    
#time          1067   1    7.9995  0.005109 ** 
#  Group:time     171   4    0.3209  0.863808    
#Residuals    29468 221 

emmeans(len_aov, pairwise ~ Group *time, adjust = "tukey")
#b final and b post different (0.001)
#control post and final (0.02)
#d final and d post (0.04) 

mass_aov<-aov(mass_g ~ Group * time, data = mixbio)
Anova(mass_aov, type = "III")
#Response: mass_g
#Sum Sq  Df  F value  Pr(>F)    
#(Intercept) 578.24   1 205.0962 < 2e-16 ***
#  Group        41.85   4   3.7109 0.00604 ** 
#  time         11.34   1   4.0235 0.04609 *  
#  Group:time   17.53   4   1.5544 0.18756    
#Residuals   623.08 221   
emmeans(mass_aov, pairwise ~ Group *time, adjust = "tukey")
##b final and b post (0.02)
#cnt post and final (0.02) 
