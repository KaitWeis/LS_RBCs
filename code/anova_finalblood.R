######################################
#
# ANOVA OF EACH FINAL BLOOD MEASUREMENT    
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
library(moments)
library(performance)
library(effectsize)
library(mvnormalTest)
library(heplots)
library(corrplot)
library(rstatix)
library(stats)
library(pwr)

setwd("/Users/kaitl/Desktop/LS_RBCs/raw_data/")

expr<-read.csv("rawdata_total.csv")
#read in the final data sheet from the chapter 3 master sheet 

###########################################
#first is Hemoglobin 

#check for outliers 
HBout<-expr %>% 
  group_by(Group) %>% 
  identify_outliers(conc_HB_gmdL)
#B1-5, C1-3 and CNT-18 but they are not extreme 

#normality for each group of this measure 
expr %>% 
  group_by(Group) %>% 
  shapiro_test(conc_HB_gmdL)
## A tibble: 5 × 4
#Group variable     statistic      p
#<chr> <chr>            <dbl>  <dbl>
#  1 A     conc_HB_gmdL     0.933 0.573 
#2 B     conc_HB_gmdL     0.959 0.453 
#3 C     conc_HB_gmdL     0.968 0.875 
#4 CNT   conc_HB_gmdL     0.873 0.0842
#5 D     conc_HB_gmdL     0.946 0.339 
#these are all normal then 
ggqqplot(expr, "conc_HB_gmdL", facet.by = "Group")
#A looks a bit wide but otherwise fine 

#levens for homogentiy of variance 
expr %>% levene_test(conc_HB_gmdL ~ Group)
## A tibble: 1 × 4
#df1   df2 statistic       p
#<int> <int>     <dbl>   <dbl>
#  1     4    64      4.11 0.00502
##homogentiy of variance in question 
HBmod<- lm(conc_HB_gmdL ~ Group, data = expr)
HBmod
summary(HBmod)
#Coefficients:
#(Intercept)       GroupB       GroupC     GroupCNT       GroupD  
#7.89431     -0.23787      0.02912     -0.59863      0.55952  
#Residuals:
#Min      1Q  Median      3Q     Max 
#-3.7995 -0.8727  0.0417  0.7755  4.5456 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.89431    0.53588  14.732   <2e-16 ***
#  GroupB      -0.23787    0.61201  -0.389    0.699    
#GroupC       0.02912    0.71450   0.041    0.968    
#GroupCNT    -0.59863    0.68550  -0.873    0.386    
#GroupD       0.55952    0.62687   0.893    0.375    
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 1.418 on 64 degrees of freedom
#(46 observations deleted due to missingness)
#Multiple R-squared:  0.07995,	Adjusted R-squared:  0.02245 
#F-statistic:  1.39 on 4 and 64 DF,  p-value: 0.2473

ggqqplot(residuals(HBmod))
#not bad.. a bit wavy 

shapiro_test(residuals(HBmod))
## A tibble: 1 × 3
#variable         statistic p.value
#<chr>                <dbl>   <dbl>
#  1 residuals(HBmod)     0.962  0.0357
##this is saying the residuals are slightly non normal 

plot(HBmod, 1)
#looks good? 
check_model(HBmod, check = "all")
#homogenity of variance is def a bit funky here 

anova(HBmod)
#          Df Sum Sq Mean Sq F value Pr(>F)
#Group      4  11.18  2.7949  1.3904 0.2473
#Residuals 64 128.65  2.0101 
#also then saying no differences 
AIC(HBmod)
#250.7994
emmeans(HBmod, pairwise ~ Group, adjust = "tukey")
sjstats::anova_stats(car::Anova(HBmod, type = 3)) %>% dplyr::select(1:7)
#term      |   sumsq | meansq | df | statistic | p.value | etasq
#---------------------------------------------------------------
#  Group     |  11.180 |  2.795 |  4 |     1.390 |   0.247 | 0.080
#Residuals | 128.649 |  2.010 | 64 | 

#lets also try non parametric for Hb just in case 
kruskal.test(conc_HB_gmdL ~ Group, data = expr)
#data:  conc_HB_gmdL by Group
#Kruskal-Wallis chi-squared = 7.0385, df = 4, p-value = 0.1339
#non para was also not significant 

pairwise.wilcox.test(expr$conc_HB_gmdL, expr$Group, p.adjust.method = "BH")
#    A    B    C    CNT 
#   B   0.81 -    -    -   
#  C   0.72 0.72 -    -   
#  CNT 0.96 0.81 0.72 -   
#  D   0.68 0.15 0.54 0.31



##Next is hematocrit measures as % PCV

#check for outliers 
Hctout<-expr %>% 
  group_by(Group) %>% 
  identify_outliers(PCV)
#B1-6, D2-11 are extreme 

#normality for each group of this measure 
expr %>% 
  group_by(Group) %>% 
  shapiro_test(PCV)
#  Group variable statistic        p
#<chr> <chr>        <dbl>    <dbl>
#  1 A     PCV          0.910 0.216   
#2 B     PCV          0.865 0.000628
#3 C     PCV          0.941 0.536   
#4 CNT   PCV          0.904 0.0585  
#5 D     PCV          0.936 0.0513 
#this is saying B may not be normal \

ggqqplot(expr, "PCV", facet.by = "Group")
#b just has outlier, D actually looks the least normal 

#levens for homogentiy of variance 
expr %>% levene_test(PCV ~ Group)
#    df1   df2 statistic     p
#<int> <int>     <dbl> <dbl>
#  1     4   104     0.174 0.951
# variance is homogenoues 

Hctmod<- lm(PCV ~ Group, data = expr)
Hctmod
summary(Hctmod)
#Call:
#lm(formula = PCV ~ Group, data = expr)
#Coefficients:
#(Intercept)       GroupB       GroupC     GroupCNT       GroupD  
#23.9167       0.5245      -1.5530       1.7966      -2.1288  
#Residuals:
#Min       1Q   Median       3Q      Max 
#-10.7879  -2.9167  -0.4412   2.2868  20.5588 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  23.9167     1.4183  16.863   <2e-16 ***
#  GroupB        0.5245     1.6497   0.318    0.751    
#GroupC       -1.5530     2.0509  -0.757    0.451    
#GroupCNT      1.7966     1.8117   0.992    0.324    
#GroupD       -2.1288     1.6562  -1.285    0.202    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Residual standard error: 4.913 on 104 degrees of freedom
#(6 observations deleted due to missingness)
#Multiple R-squared:  0.08569,	Adjusted R-squared:  0.05052 
#F-statistic: 2.437 on 4 and 104 DF,  p-value: 0.05173
#overall not a difference.. 

ggqqplot(residuals(Hctmod))
#little tailing upward at end 

shapiro_test(residuals(Hctmod))
#variable          statistic  p.value
#<chr>                 <dbl>    <dbl>
#  1 residuals(Hctmod)     0.942 0.000139

plot(Hctmod, 1)
#normal 

check_model(Hctmod, check = "all")
#yea this looks normal! besides the little tail in residuals seems fine 

anova(Hctmod)
#           Df  Sum Sq Mean Sq F value  Pr(>F)  
#Group       4  235.28   58.82  2.4367 0.05173 .
#Residuals 104 2510.53   24.14  
AIC(Hctmod)
#663.251

emmeans(Hctmod, pairwise ~ Group, adjust = "tukey")
#$emmeans
#Group emmean    SE  df lower.CL upper.CL
#A       23.9 1.418 104     21.1     26.7
#B       24.4 0.843 104     22.8     26.1
#C       22.4 1.481 104     19.4     25.3
#CNT     25.7 1.127 104     23.5     27.9
#D       21.8 0.855 104     20.1     23.5
#Confidence level used: 0.95 
#$contrasts
#contrast estimate   SE  df t.ratio p.value
#A - B      -0.525 1.65 104  -0.318  0.9978
#A - C       1.553 2.05 104   0.757  0.9421
#A - CNT    -1.797 1.81 104  -0.992  0.8586
#A - D       2.129 1.66 104   1.285  0.7008
#B - C       2.078 1.70 104   1.219  0.7404
#B - CNT    -1.272 1.41 104  -0.904  0.8948
#B - D       2.653 1.20 104   2.210  0.1842
#C - CNT    -3.350 1.86 104  -1.799  0.3795
#C - D       0.576 1.71 104   0.337  0.9972
#CNT - D     3.925 1.41 104   2.774  0.0503
#no differences! last one is close but thats it 

sjstats::anova_stats(car::Anova(Hctmod, type = 3)) %>% dplyr::select(1:7)
#term      |    sumsq | meansq |  df | statistic | p.value | etasq
#-----------------------------------------------------------------
#  Group     |  235.282 | 58.820 |   4 |     2.437 |   0.052 | 0.086
#Residuals | 2510.534 | 24.140 | 104 | 

#next is concentration of RBC 

#check for outliers 
RBCout<-expr %>% 
  group_by(Group) %>% 
  identify_outliers(conc_cells_mm3)
#non are extreme 

#normality for each group of this measure 
expr %>% 
  group_by(Group) %>% 
  shapiro_test(conc_cells_mm3)
#  Group variable       statistic      p
#<chr> <chr>              <dbl>  <dbl>
#  1 A     conc_cells_mm3     0.907 0.222 
#2 B     conc_cells_mm3     0.962 0.295 
#3 C     conc_cells_mm3     0.931 0.423 
#4 CNT   conc_cells_mm3     0.860 0.0494
#5 D     conc_cells_mm3     0.934 0.0404
#CNT and D may not be normal 

ggqqplot(expr, "conc_cells_mm3", facet.by = "Group")
#okay..

#levens for homogentiy of variance 
expr %>% levene_test(conc_cells_mm3 ~ Group)
#    df1   df2 statistic     p
#<int> <int>     <dbl> <dbl>
#  1     4    96     0.950 0.439
#says homogenity of variance 

RBCmod<- lm(conc_cells_mm3 ~ Group, data = expr)
RBCmod
summary(RBCmod)
#Coefficients:
#(Intercept)       GroupB       GroupC     GroupCNT       GroupD  
#555000       -32576        -4545      -216667       -79412  
#Residuals:
#Min      1Q  Median      3Q     Max 
#-385455 -117424  -30000   91667  519412 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   555000      54048  10.269  < 2e-16 ***
#  GroupB        -32576      62409  -0.522  0.60289    
#GroupC         -4546      76435  -0.059  0.95270    
#GroupCNT     -216667      74826  -2.896  0.00469 ** 
#  GroupD        -79412      62179  -1.277  0.20463    
#Residual standard error: 179300 on 96 degrees of freedom
#(14 observations deleted due to missingness)
#Multiple R-squared:  0.1159,	Adjusted R-squared:  0.07903 
#F-statistic: 3.145 on 4 and 96 DF,  p-value: 0.01777
#showing significant difference possible 

ggqqplot(residuals(RBCmod))
#yea decent 

shapiro_test(residuals(RBCmod))
#  variable          statistic p.value
#<chr>                 <dbl>   <dbl>
#  1 residuals(RBCmod)     0.973  0.0363
#may be non normal residuals - should check the kruskal-wallace here 

plot(RBCmod, 1)
#okay yea this is weird 4 are close and one is far? 

check_model(RBCmod, check = "all")
#some variance but not bad 

anova(RBCmod)
#          Df     Sum Sq    Mean Sq F value  Pr(>F)  
#Group      4 4.0427e+11 1.0107e+11  3.1453 0.01777 *
#  Residuals 96 3.0847e+12 3.2133e+10   
#this does actually say theres a difference!! 

AIC(RBCmod)
#2737.005

emmeans(RBCmod, pairwise ~ Group, adjust = "tukey")
# Group emmean    SE df lower.CL upper.CL
#A     555000 54048 96   447716   662284
#B     522424 31204 96   460484   584365
#C     550455 54048 96   443171   657738
#CNT   338333 51747 96   235617   441050
#D     475588 30742 96   414566   536611
#Confidence level used: 0.95 
#$contrasts
#contrast estimate    SE df t.ratio p.value
#A - B       32576 62409 96   0.522  0.9849
#A - C        4545 76435 96   0.059  1.0000
#A - CNT    216667 74826 96   2.896  0.0369
#A - D       79412 62179 96   1.277  0.7058
#B - C      -28030 62409 96  -0.449  0.9915
#B - CNT    184091 60427 96   3.046  0.0244
#B - D       46836 43804 96   1.069  0.8218
#C - CNT    212121 74826 96   2.835  0.0434
#C - D       74866 62179 96   1.204  0.7490
#CNT - D   -137255 60190 96  -2.280  0.1603
sjstats::anova_stats(car::Anova(RBCmod, type = 3)) %>% dplyr::select(1:7)
#term      |     sumsq |    meansq | df | statistic | p.value | etasq
#--------------------------------------------------------------------
#  Group     | 4.043e+11 | 1.011e+11 |  4 |     3.145 |   0.018 | 0.116
#Residuals | 3.085e+12 | 3.213e+10 | 96 |           |         |    

#try the non para as well 
kruskal.test(conc_cells_mm3 ~ Group, data = expr)
#Kruskal-Wallis chi-squared = 12.533, df = 4, p-value = 0.0138
pairwise.wilcox.test(expr$conc_cells_mm3, expr$Group, p.adjust.method = "BH")
#    A     B     C     CNT  
#B   0.968 -     -     -    
#C   0.974 0.442 -     -    
#CNT 0.035 0.020 0.122 -    
#D   0.668 0.288 0.318 0.035


## MEH 

#check for outliers 
MEHout<-expr %>% 
  group_by(Group) %>% 
  identify_outliers(MEH)
#b2-12 and C1-3 are extreme 

#normality for each group of this measure 
expr %>% 
  group_by(Group) %>% 
  shapiro_test(MEH)
#  Group variable statistic      p
#<chr> <chr>        <dbl>  <dbl>
#  1 A     MEH          0.958 0.800 
#2 B     MEH          0.887 0.0164
#3 C     MEH          0.766 0.0124
#4 CNT   MEH          0.957 0.748 
#5 D     MEH          0.963 0.631 
#B and C may be not normal

ggqqplot(expr, "MEH", facet.by = "Group")
#pretty good, larger spread on CNT 

#levens for homogentiy of variance 
expr %>% levene_test(MEH ~ Group)
#    df1   df2 statistic      p
#<int> <int>     <dbl>  <dbl>
#  1     4    61      3.21 0.0186
#maybe not homo variant 

MEHmod<- lm(MEH ~ Group, data = expr)
MEHmod
summary(MEHmod)
#Coefficients:
#(Intercept)       GroupB       GroupC     GroupCNT       GroupD  
#128.10        23.49        28.40       108.28        47.11  
#Residuals:
#Min       1Q   Median       3Q      Max 
#-144.991  -32.075   -5.628   26.334  219.284 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   128.10      24.45   5.240 2.11e-06 ***
#  GroupB         23.49      28.07   0.837   0.4060    
#GroupC         28.40      33.48   0.848   0.3996    
#GroupCNT      108.28      31.88   3.397   0.0012 ** 
#  GroupD         47.11      28.60   1.647   0.1046    
#Residual standard error: 64.68 on 61 degrees of freedom
#3(49 observations deleted due to missingness)
#Multiple R-squared:  0.2049,	Adjusted R-squared:  0.1527 
#F-statistic: 3.929 on 4 and 61 DF,  p-value: 0.006665
#suggesting some differences! 

ggqqplot(residuals(MEHmod))
#some skewy tails here 

shapiro_test(residuals(MEHmod))
#  variable          statistic p.value
#<chr>                 <dbl>   <dbl>
#  1 residuals(MEHmod)     0.957  0.0238
#probably not normal data 

plot(MEHmod, 1)

check_model(MEHmod, check = "all")
#actually not terrible.. but chck kruskal too 

anova(MEHmod)
#Response: MEH
#Df Sum Sq Mean Sq F value   Pr(>F)   
#Group      4  65752 16438.0  3.9289 0.006665 **
#Residuals 61 255215  4183.8         
AIC(MEHmod)
#744.4735
emmeans(MEHmod, pairwise ~ Group, adjust = "tukey")
#$emmeans
#Group emmean   SE df lower.CL upper.CL
#A        128 24.4 61     79.2      177
#B        152 13.8 61    124.0      179
#C        156 22.9 61    110.8      202
#CNT      236 20.5 61    195.5      277
#D        175 14.8 61    145.5      205
#Confidence level used: 0.95 
#$contrasts
#contrast estimate   SE df t.ratio p.value
#A - B      -23.49 28.1 61  -0.837  0.9181
#A - C      -28.40 33.5 61  -0.848  0.9142
#A - CNT   -108.28 31.9 61  -3.397  0.0102
#A - D      -47.11 28.6 61  -1.647  0.4737
#B - C       -4.91 26.7 61  -0.184  0.9997
#B - CNT    -84.79 24.7 61  -3.437  0.0091
#B - D      -23.63 20.3 61  -1.166  0.7703
#C - CNT    -79.88 30.7 61  -2.603  0.0823
#C - D      -18.72 27.3 61  -0.687  0.9587
#CNT - D     61.16 25.3 61   2.420  0.1237
sjstats::anova_stats(car::Anova(MEHmod, type = 3)) %>% dplyr::select(1:7)
#term      |     sumsq |    meansq | df | statistic | p.value | etasq
#--------------------------------------------------------------------
#  Group     | 65752.150 | 16438.038 |  4 |     3.929 |   0.007 | 0.205
#Residuals | 2.552e+05 |  4183.848 | 61 |         

#try non paramet
MEHnon<-kruskal.test(MEH ~ Group, data = expr)
#data:  MEH by Group
#Kruskal-Wallis chi-squared = 8.848, df = 4, p-value = 0.06501

pairwise.wilcox.test(expr$MEH, expr$Group, p.adjust.method = "BH")
#    A    B    C    CNT 
#B   0.30 -    -    -   
#C   0.44 0.60 -    -   
#CNT 0.24 0.24 0.24 -   
#D   0.24 0.24 0.35 0.30



#MEV next 

#check for outliers 
MEVout<-expr %>% 
  group_by(Group) %>% 
  identify_outliers(MEV)
#C2-3 extreme 

#normality for each group of this measure 
expr %>% 
  group_by(Group) %>% 
  shapiro_test(MEV)
#  Group variable statistic          p
#<chr> <chr>        <dbl>      <dbl>
#  1 A     MEV          0.968 0.866     
#2 B     MEV          0.965 0.369     
#3 C     MEV          0.506 0.00000432
#4 CNT   MEV          0.917 0.261     
#5 D     MEV          0.848 0.000465  

ggqqplot(expr, "MEV", facet.by = "Group")
# tight except cnt a bit wide 

#levens for homogentiy of variance 
expr %>% levene_test(MEV ~ Group)
#    df1   df2 statistic      p
#<int> <int>     <dbl>  <dbl>
#  1     4    91      3.37 0.0128

MEVmod<- lm(MEV ~ Group, data = expr)
MEVmod
summary(MEVmod)
#Coefficients:
#(Intercept)       GroupB       GroupC     GroupCNT       GroupD  
#496.413       18.232      -22.484      442.352       -6.866  
#Residuals:
#Min      1Q  Median      3Q     Max 
#-663.40 -119.18  -41.46   77.18 1211.77 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  496.413     79.108   6.275 1.16e-08 ***
#  GroupB        18.232     91.702   0.199 0.842852    
#GroupC       -22.484    114.638  -0.196 0.844942    
#GroupCNT     442.352    109.519   4.039 0.000112 ***
#  GroupD        -6.866     92.079  -0.075 0.940720    
#Residual standard error: 262.4 on 91 degrees of freedom
#(19 observations deleted due to missingness)
#Multiple R-squared:  0.2471,	Adjusted R-squared:  0.214 
#F-statistic: 7.465 on 4 and 91 DF,  p-value: 3.024e-05
#suggesting differences 

ggqqplot(residuals(MEVmod))
#very skewed tails 

shapiro_test(residuals(MEVmod))
#A tibble: 1 × 3
#variable          statistic      p.value
#<chr>                 <dbl>        <dbl>
#  1 residuals(MEVmod)     0.851 0.0000000216
#residuals are very not normal 

plot(MEVmod, 1)
#weeeird 

check_model(MEVmod, check = "all")
#yea bad 

anova(MEVmod)
#Df  Sum Sq Mean Sq F value    Pr(>F)    
#Group      4 2055407  513852  7.4646 3.024e-05 ***
#  Residuals 91 6264261   68838    

AIC(MEVmod)
#1348.694

emmeans(MEVmod, pairwise ~ Group, adjust = "tukey")
# Group emmean   SE df lower.CL upper.CL
#A        496 79.1 91      339      654
#B        515 46.4 91      423      607
#C        474 83.0 91      309      639
#CNT      939 75.7 91      788     1089
#D        490 47.1 91      396      583
#Confidence level used: 0.95 
#$contrasts
#contrast estimate    SE df t.ratio p.value
#A - B      -18.23  91.7 91  -0.199  0.9996
#A - C       22.48 114.6 91   0.196  0.9997
#A - CNT   -442.35 109.5 91  -4.039  0.0010
#A - D        6.87  92.1 91   0.075  1.0000
#B - C       40.72  95.1 91   0.428  0.9929
#B - CNT   -424.12  88.8 91  -4.775  0.0001
#B - D       25.10  66.1 91   0.380  0.9955
#C - CNT   -464.84 112.3 91  -4.138  0.0007
#C - D      -15.62  95.4 91  -0.164  0.9998
#CNT - D    449.22  89.2 91   5.036  <.0001

sjstats::anova_stats(car::Anova(MEVmod, type = 3)) %>% dplyr::select(1:7)
#term      |     sumsq |    meansq | df | statistic | p.value | etasq
#--------------------------------------------------------------------
#  Group     | 2.055e+06 | 5.139e+05 |  4 |     7.465 |  < .001 | 0.247
#Residuals | 6.264e+06 | 68838.037 | 91 |           |         |    

kruskal.test(MEV ~ Group, data = expr)
#Kruskal-Wallis chi-squared = 14.862, df = 4, p-value = 0.004996

pairwise.wilcox.test(expr$MEV, expr$Group, p.adjust.method = "BH")
#    A     B     C     CNT  
#B   0.846 -     -     -    
#C   0.319 0.048 -     -    
#CNT 0.023 0.015 0.028 -    
#D   0.846 0.429 0.224 0.015

#in MEV B and C were different, and all different from control 

#finally (for blood) MEHV

#check for outliers 
MEHVout<-expr %>% 
  group_by(Group) %>% 
  identify_outliers(MEHC)
#none extreme 

#normality for each group of this measure 
expr %>% 
  group_by(Group) %>% 
  shapiro_test(MEHC)
## A tibble: 5 × 4
#Group variable statistic     p
#<chr> <chr>        <dbl> <dbl>
# 1 A     MEHC         0.914 0.424
#2 B     MEHC         0.963 0.528
#3 C     MEHC         0.908 0.341
#4 CNT   MEHC         0.964 0.818
#5 D     MEHC         0.958 0.559
#normal 

ggqqplot(expr, "MEHC", facet.by = "Group")
#okay 

#levens for homogentiy of variance 
expr %>% levene_test(MEHC ~ Group)
#    df1   df2 statistic     p
#<int> <int>     <dbl> <dbl>
#  1     4    62     0.524 0.719
#homogeneous 

MEHVmod<- lm(MEHC ~ Group, data = expr)
MEHVmod
summary(MEHVmod)
#Coefficients:
#(Intercept)       GroupB       GroupC     GroupCNT       GroupD  
#31.5359      -0.5421       6.2607      -2.4309       8.5228  
#Residuals:
#Min      1Q  Median      3Q     Max 
#-16.509  -4.488  -0.421   4.001  20.390 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  31.5359     2.9219  10.793  7.2e-16 ***
#  GroupB       -0.5421     3.3370  -0.162   0.8715    
#GroupC        6.2607     4.0009   1.565   0.1227    
#GroupCNT     -2.4309     3.7377  -0.650   0.5178    
#GroupD        8.5228     3.4435   2.475   0.0161 *  
#Residual standard error: 7.731 on 62 degrees of freedom
#(48 observations deleted due to missingness)
#Multiple R-squared:  0.2582,	Adjusted R-squared:  0.2104 
#F-statistic: 5.396 on 4 and 62 DF,  p-value: 0.0008563

ggqqplot(residuals(MEHVmod))
#yea pretty good 
shapiro_test(residuals(MEHVmod))
# variable           statistic p.value
#<chr>                  <dbl>   <dbl>
#1 residuals(MEHVmod)     0.981   0.386
#data is normal 

plot(MEHVmod, 1)

check_model(MEHVmod, check = "all")

anova(MEHVmod)
#          Df Sum Sq Mean Sq F value    Pr(>F)    
#Group      4 1290.0  322.49  5.3963 0.0008563 ***
#  Residuals 62 3705.2   59.76      
#some difference in MEHC!! 

AIC(MEHVmod)
#470.9953
emmeans(MEHVmod, pairwise ~ Group, adjust = "tukey")
#$emmeans
#Group emmean   SE df lower.CL upper.CL
#A       31.5 2.92 62     25.7     37.4
#B       31.0 1.61 62     27.8     34.2
#C       37.8 2.73 62     32.3     43.3
#CNT     29.1 2.33 62     24.4     33.8
#D       40.1 1.82 62     36.4     43.7
#Confidence level used: 0.95 
#$contrasts
#contrast estimate   SE df t.ratio p.value
#A - B       0.542 3.34 62   0.162  0.9998
#A - C      -6.261 4.00 62  -1.565  0.5252
#A - CNT     2.431 3.74 62   0.650  0.9659
#A - D      -8.523 3.44 62  -2.475  0.1097
#B - C      -6.803 3.17 62  -2.144  0.2151
#B - CNT     1.889 2.83 62   0.667  0.9628
#B - D      -9.065 2.43 62  -3.726  0.0037
#C - CNT     8.692 3.59 62   2.420  0.1237
#C - D      -2.262 3.28 62  -0.689  0.9582
#CNT - D   -10.954 2.96 62  -3.702  0.0040
# B and D and D and CNT are different**** 
sjstats::anova_stats(car::Anova(MEHVmod, type = 3)) %>% dplyr::select(1:7)
#term      |    sumsq |  meansq | df | statistic | p.value | etasq
#-----------------------------------------------------------------
#  Group     | 1289.964 | 322.491 |  4 |     5.396 |   0.001 | 0.258
#Residuals | 3705.197 |  59.761 | 62 |           |         |     

