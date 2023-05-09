######################################################
#
#ANCOVA AND INTERACTION EFFECTS    
#
# Created by KW JANUARY 2023 
# modified by K.Weisgerber on 5/9/23
#
#####################################################
#load libraries
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(emmeans)
library(moments)
library(performance)
library(rstatix)
library(pwr)
library(broom)
library(corrplot)

setwd("/Users/kaitl/Desktop/LS_RBCs/raw_data/")

data<-read.csv("rawdata_total.csv")

##lets looks at any correlations 
metrics<-expr[,c(3:4,11,16,18,21:24)]
metrics.cor<-cor(metrics, method = c("spearman"))
corrplot(metrics.cor)

##can also check individually 
cor.test(x = expr$`MEH`, y = expr$MEHC, method = "pearson")$estimate

###going into ANCOVA - looking at mass or length as covariates to RBC conc 

#create a scatterplot btwn covariate (weight) and outcome variable (RBC conc)

ggscatter(expr, x = "mass_g", y = "conc_cells_mm3", color = "Group", add = "reg.line")+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Group)
  )
#this is checking the linearity assumption.. and gives the R2 values for each 

#check homogentiy of regression slopes 
##tells us if there is an interaction btwn covriate and grouping variable 

expr %>% 
  anova_test(conc_cells_mm3 ~ Group*mass_g)
#        Effect DFn DFd      F        p p<.05   ges
#1        Group   4  91  6.836 7.40e-05     * 0.231
#2       mass_g   1  91 24.630 3.22e-06     * 0.213
#3 Group:mass_g   4  91  0.547 7.02e-01       0.023
#interaction term is not significantly different, therefore there is homogenity of regression slopes 

expr %>% 
  anova_test(conc_cells_mm3 ~ Group*length_mm)
#
#Effect DFn DFd      F        p p<.05   ges
#1           Group   4  91  5.564 4.72e-04     * 0.197
#2       length_mm   1  91 23.383 5.37e-06     * 0.204
#3 Group:length_mm   4  91  0.330 8.57e-01       0.014
#same for length 


#check for normality of residuals, but need to make the model 

model_g<- lm(conc_cells_mm3 ~ mass_g + Group, data = expr)
#inspect model diagnostic metrics 
model_g.metrics<- augment(model_g) 
head(model_g.metrics, 4)
AIC(model_g)
# 2715.318

#assess normality of residuals 
shapiro_test(model_g.metrics$.resid)
#  variable               statistic p.value
#<chr>                      <dbl>   <dbl>
#  1 model_g.metrics$.resid     0.982   0.178
#this means normality of residuals for mass

#try with length 
model_mm<- lm(conc_cells_mm3 ~ length_mm + Group, data = expr)
#inspect model diagnostic metrics 
model_mm.metrics<- augment(model_mm) 
head(model_mm.metrics, 4)
AIC(model_mm)
#2716.202

shapiro_test(model_mm.metrics$.resid)
## A tibble: 1 × 3
#variable                statistic p.value
#<chr>                       <dbl>   <dbl>
#  1 model_mm.metrics$.resid     0.982   0.174

#both weight and length have normal residuals 

#check homogentiy of variance 
model_g.metrics %>% 
  levene_test(.resid ~ Group)
#    df1   df2 statistic     p
#<int> <int>     <dbl> <dbl>
#  1     4    96      1.15 0.338
#assume homogenity of residual variance for weight 

model_mm.metrics %>% 
  levene_test(.resid ~ Group)
#    df1   df2 statistic     p
#<int> <int>     <dbl> <dbl>
#  1     4    96      1.40 0.241
#and for length too 

#finally check for outliers ## observations with >3 absolute value in standardized residual are possible outliers 

model_g.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()
#no outliers! 

model_mm.metrics %>% 
  filter(abs(.std.resid) > 3) %>% 
  as.data.frame()
#also none 

g.aov<-expr %>% anova_test(conc_cells_mm3 ~ mass_g + Group)
get_anova_table(g.aov)
#  Effect DFn DFd      F        p p<.05   ges
#1 mass_g   1  95 25.109 2.50e-06     * 0.209
#2  Group   4  95  6.969 5.79e-05     * 0.227

#after adjusting for mass of individuals there was a significant difference in [RBC] 

g.pwc<- expr %>% 
  emmeans_test(conc_cells_mm3 ~ Group, covariate = mass_g, 
               p.adjust.method = "bonferroni")
g.pwc
#run code to see 
#get the means of each group 
get_emmeans(g.pwc)
#  mass_g Group  emmean     se    df conf.low conf.high method      
#<dbl> <fct>   <dbl>  <dbl> <dbl>    <dbl>     <dbl> <chr>       
#  1   6.71 A     542489. 48384.    95  446434.   638544. Emmeans test
#2   6.71 B     521282. 27898.    95  465897.   576667. Emmeans test
#3   6.71 C     554570. 48327.    95  458629.   650511. Emmeans test
#4   6.71 CNT   250682. 49459.    95  152493.   348871. Emmeans test
#5   6.71 D     510349. 28346.    95  454075.   566623. Emmeans test


#accounting for length then 
mm.aov<-expr %>% anova_test(conc_cells_mm3 ~ length_mm + Group)
get_anova_table(mm.aov)
#     Effect DFn DFd      F        p p<.05   ges
#1 length_mm   1  95 24.062 3.85e-06     * 0.202
#2     Group   4  95  5.726 3.58e-04     * 0.194

mm.pwc<- expr %>% 
  emmeans_test(conc_cells_mm3 ~ Group, covariate = length_mm, 
               p.adjust.method = "bonferroni")
mm.pwc
get_emmeans(mm.pwc)

##using the mass as a covariate fit best and explained most variance 

