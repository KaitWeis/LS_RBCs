######################################
#
#TWO-WAY ANOVA WITH MNV THREE TIME POINTS   
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

##lets check anova models 
#first with additive
res.aov<-aov(MNV ~ Group + time, data = three1)
summary.aov(res.aov)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Group         4   32.1    8.03   1.076 0.36916   
#time          2   92.8   46.39   6.216 0.00238 **
#Residuals   213 1589.7    7.46    

#then with interactive an can check AIC 
res.aov1<-aov(MNV ~ Group * time, data = three1)
summary.aov(res.aov1)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Group         4   32.1    8.03   1.057 0.37917   
#time          2   92.8   46.39   6.102 0.00267 **
#Group:time    8   31.1    3.88   0.511 0.84759   
#Residuals   205 1558.6    7.60      

AIC(res.aov)
#1075.42
AIC(res.aov1)
#1087.078

##slightly better as an additive model,and there was no significant interaction between the two factors 

##so the levels of time are associated with significantly different MNV 

##now lets see where the significant differences are 
TukeyHSD(res.aov)
#this compares ALL possible values; even though we know group wasnt significant above 
#                     diff        lwr       upr     p adj
#initial-final_  1.7265936  0.5429929 2.9101943 0.0019943
#post-final_     0.6221247 -0.3784487 1.6226981 0.3086432
#post-initial   -1.1044689 -2.4002340 0.1912962 0.1118313

##so there was a significant difference btwn initial and final measures 

###need to check the normality/ assumptions of this data 

##homogentiy of vairance assumption 
plot(res.aov, 1)
##showing that there are a couple outliers 
#also use the levens test 
leveneTest(MNV ~ Group * time, data = three1)
#       Df F value Pr(>F)
#group  14  0.9531  0.503
#205   
#this is saying the assumption is met 

#check normality 
plot(res.aov, 2)
#not bad

#finally a shapiro-wilk on the residuals 
aov_resid<- residuals(object = res.aov)
shapiro.test(x = aov_resid)
#data:  aov_resid
#W = 0.57358, p-value < 2.2e-16

##this is saying the residuals are not normal, should consider removing the outliers in the analysis 

MNV_out<-three1 %>% 
  group_by(time, Group) %>% 
  identify_outliers(MNV)

#subset to remove these extreme outliers 
Dthree<- three[c(1:18, 20:53, 55:130, 132:168, 171:271),]

#rerun anova with additive 
res.aov3<-aov(MNV ~ Group + time, data = Dthree)
summary.aov(res.aov3)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
#Group         4   32.9    8.22   4.521   0.0016 ** 
#time          2  147.0   73.52  40.447 1.45e-15 ***
#Residuals   208  378.1    1.82   

#anova with interactive 
res.aov4<-aov(MNV ~ Group * time, data = Dthree)
summary.aov(res.aov4)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
#Group         4   32.9    8.22   4.928 0.000824 ***
#time          2  147.0   73.52  44.097  < 2e-16 ***
#Group:time    8   44.6    5.58   3.346 0.001280 ** 
#Residuals   200  333.4    1.67      

##now we are seeing a significant interaction! lets check AIC 

AIC(res.aov3)
#747.499

AIC(res.aov4)
#736.4897

##we want to use the interaction model, lets run post hoc comparisons 
aov.int<-TukeyHSD(res.aov4)

#now check assumptions on this data 
plot(res.aov4, 1)
leveneTest(MNV ~ Group * time, data = Dthree)
#       Df F value Pr(>F)
#group  14  1.1431 0.3225
#200
##homogenous variances 

#normality 
plot(res.aov4, 2)
aov.residuals<-residuals(object = res.aov4)
shapiro.test(x = aov.residuals)
#data:  aov.residuals
#W = 0.95851, p-value = 6.647e-06
#residuals are not normal according to this... 

##try for an unbalanced design with outliers removed - because there are different number of subjects in each group 

aov3<-aov(MNV ~ Group * time, data = Dthree)
Anova(aov3, type = "III")
#             Sum Sq  Df    F value    Pr(>F)    
#(Intercept) 31205.5   1 18717.4433 < 2.2e-16 ***
#Group          71.2   4    10.6720  7.37e-08 ***
#time           16.2   2     4.8438  0.008825 ** 
#Group:time     44.6   8     3.3464  0.001280 ** 
#Residuals     333.4 200      

new.aov<-emmeans(aov3, pairwise ~ Group *time, adjust = "tukey")
#saving the output of this comparison into processed data 
write.csv(new.aov, "C:\\Users\\kaitl\\Desktop\\LS_RBCs\\processed_data\\unbalancedtwowaypwc.csv")
#this file doesnt seem to have contrasts right.. will comment here 

#$contrasts
#contrast                 estimate    SE  df t.ratio p.value
#A final_ - B final_       -0.6043 0.435 200  -1.388  0.9875
#A final_ - C final_        1.9236 0.539 200   3.569  0.0330
#A final_ - CNT final_     -0.1219 0.471 200  -0.258  1.0000
#A final_ - D final_        0.8457 0.430 200   1.965  0.8162
#A final_ - A initial      -1.8267 0.589 200  -3.100  0.1271
#A final_ - B initial      -1.8267 0.589 200  -3.100  0.1271
#A final_ - C initial      -1.8267 0.589 200  -3.100  0.1271
#A final_ - CNT initial    -1.8267 0.589 200  -3.100  0.1271
#A final_ - D initial      -1.8267 0.589 200  -3.100  0.1271
#A final_ - A post         -0.8916 0.614 200  -1.452  0.9812
#A final_ - B post         -1.2088 0.487 200  -2.483  0.4574
#A final_ - C post         -1.2395 0.614 200  -2.019  0.7850
#A final_ - CNT post       -0.1725 0.500 200  -0.345  1.0000
#A final_ - D post         -0.9518 0.487 200  -1.955  0.8216
#B final_ - C final_        2.5278 0.450 200   5.623  <.0001
#B final_ - CNT final_      0.4824 0.366 200   1.318  0.9924
#B final_ - D final_        1.4500 0.311 200   4.660  0.0006
#B final_ - A initial      -1.2224 0.509 200  -2.402  0.5161
#B final_ - B initial      -1.2224 0.509 200  -2.402  0.5161
#B final_ - C initial      -1.2224 0.509 200  -2.402  0.5161
#B final_ - CNT initial    -1.2224 0.509 200  -2.402  0.5161
#B final_ - D initial      -1.2224 0.509 200  -2.402  0.5161
#B final_ - A post         -0.2873 0.537 200  -0.535  1.0000
#B final_ - B post         -0.6046 0.385 200  -1.568  0.9635
#B final_ - C post         -0.6353 0.537 200  -1.182  0.9975
#B final_ - CNT post        0.4318 0.402 200   1.074  0.9991
#B final_ - D post         -0.3475 0.385 200  -0.902  0.9999
#C final_ - CNT final_     -2.0454 0.485 200  -4.220  0.0033
#C final_ - D final_       -1.0779 0.445 200  -2.423  0.5009
#C final_ - A initial      -3.7503 0.600 200  -6.251  <.0001
#C final_ - B initial      -3.7503 0.600 200  -6.251  <.0001
#C final_ - C initial      -3.7503 0.600 200  -6.251  <.0001
#C final_ - CNT initial    -3.7503 0.600 200  -6.251  <.0001
#C final_ - D initial      -3.7503 0.600 200  -6.251  <.0001
#C final_ - A post         -2.8152 0.624 200  -4.509  0.0010
#C final_ - B post         -3.1324 0.500 200  -6.269  <.0001
#C final_ - C post         -3.1631 0.624 200  -5.067  0.0001
#C final_ - CNT post       -2.0961 0.513 200  -4.090  0.0054
#C final_ - D post         -2.8754 0.500 200  -5.755  <.0001
#CNT final_ - D final_      0.9676 0.360 200   2.687  0.3207
#CNT final_ - A initial    -1.7048 0.540 200  -3.156  0.1097
#CNT final_ - B initial    -1.7048 0.540 200  -3.156  0.1097
#CNT final_ - C initial    -1.7048 0.540 200  -3.156  0.1097
#CNT final_ - CNT initial  -1.7048 0.540 200  -3.156  0.1097
#CNT final_ - D initial    -1.7048 0.540 200  -3.156  0.1097
#CNT final_ - A post       -0.7697 0.567 200  -1.357  0.9899
#CNT final_ - B post       -1.0870 0.426 200  -2.552  0.4090
#CNT final_ - C post       -1.1177 0.567 200  -1.971  0.8127
#CNT final_ - CNT post     -0.0507 0.441 200  -0.115  1.0000
#CNT final_ - D post       -0.8300 0.426 200  -1.949  0.8252
#D final_ - A initial      -2.6724 0.505 200  -5.295  <.0001
#D final_ - B initial      -2.6724 0.505 200  -5.295  <.0001
#D final_ - C initial      -2.6724 0.505 200  -5.295  <.0001
#D final_ - CNT initial    -2.6724 0.505 200  -5.295  <.0001
#D final_ - D initial      -2.6724 0.505 200  -5.295  <.0001
#D final_ - A post         -1.7373 0.533 200  -3.257  0.0835
#D final_ - B post         -2.0545 0.380 200  -5.407  <.0001
#D final_ - C post         -2.0852 0.533 200  -3.910  0.0105
#D final_ - CNT post       -1.0182 0.397 200  -2.566  0.3993
#D final_ - D post         -1.7975 0.380 200  -4.731  0.0004
#A initial - B initial      0.0000 0.646 200   0.000  1.0000
#A initial - C initial      0.0000 0.646 200   0.000  1.0000
#A initial - CNT initial    0.0000 0.646 200   0.000  1.0000
#A initial - D initial      0.0000 0.646 200   0.000  1.0000
#A initial - A post         0.9351 0.668 200   1.399  0.9866
#A initial - B post         0.6179 0.554 200   1.116  0.9986
#A initial - C post         0.5872 0.668 200   0.879  0.9999
#A initial - CNT post       1.6542 0.565 200   2.926  0.1936
#A initial - D post         0.8749 0.554 200   1.580  0.9611
#B initial - C initial      0.0000 0.646 200   0.000  1.0000
#B initial - CNT initial    0.0000 0.646 200   0.000  1.0000
#B initial - D initial      0.0000 0.646 200   0.000  1.0000
#B initial - A post         0.9351 0.668 200   1.399  0.9866
#B initial - B post         0.6179 0.554 200   1.116  0.9986
#B initial - C post         0.5872 0.668 200   0.879  0.9999
#B initial - CNT post       1.6542 0.565 200   2.926  0.1936
#B initial - D post         0.8749 0.554 200   1.580  0.9611
#C initial - CNT initial    0.0000 0.646 200   0.000  1.0000
#C initial - D initial      0.0000 0.646 200   0.000  1.0000
#C initial - A post         0.9351 0.668 200   1.399  0.9866
#C initial - B post         0.6179 0.554 200   1.116  0.9986
#C initial - C post         0.5872 0.668 200   0.879  0.9999
#C initial - CNT post       1.6542 0.565 200   2.926  0.1936
#C initial - D post         0.8749 0.554 200   1.580  0.9611
#CNT initial - D initial    0.0000 0.646 200   0.000  1.0000
#CNT initial - A post       0.9351 0.668 200   1.399  0.9866
#CNT initial - B post       0.6179 0.554 200   1.116  0.9986
#CNT initial - C post       0.5872 0.668 200   0.879  0.9999
#CNT initial - CNT post     1.6542 0.565 200   2.926  0.1936
#CNT initial - D post       0.8749 0.554 200   1.580  0.9611
#D initial - A post         0.9351 0.668 200   1.399  0.9866
#D initial - B post         0.6179 0.554 200   1.116  0.9986
#D initial - C post         0.5872 0.668 200   0.879  0.9999
#D initial - CNT post       1.6542 0.565 200   2.926  0.1936
#D initial - D post         0.8749 0.554 200   1.580  0.9611
#A post - B post           -0.3172 0.580 200  -0.547  1.0000
#A post - C post           -0.3480 0.690 200  -0.504  1.0000
#A post - CNT post          0.7191 0.591 200   1.217  0.9966
#A post - D post           -0.0602 0.580 200  -0.104  1.0000
#B post - C post           -0.0307 0.580 200  -0.053  1.0000
#B post - CNT post          1.0363 0.457 200   2.266  0.6170
#B post - D post            0.2570 0.443 200   0.580  1.0000
#C post - CNT post          1.0670 0.591 200   1.805  0.8934
#C post - D post            0.2877 0.580 200   0.496  1.0000
#CNT post - D post         -0.7793 0.457 200  -1.704  0.9297

##Final measures in C and D were different than post and initial 
##A final different than C final 
##B final different than c and d final 
##C final different than control 

