library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(car)
library(readxl)
library(DescTools)

#setwd
setwd("C:/Users/rawat/OneDrive/Documents/states")

##Problem 1##
#read in file
house <- read_xlsx("P02_35.xlsx", sheet = "Data", col_names = TRUE)

#A. re-code the location variables and
#filter out SW and NW
house <- house %>%
  mutate(Location = ifelse(Location == 1, "SW",
                           ifelse(Location == 2, "NW",
                                  ifelse(Location == 3, "NE", "SE")))) %>%
  filter(Location == "SW" | Location == "NW") 


#B. create a histogram for each sample to aid in describing the distribution
house_fi_nwsw_hist <- house %>%
  ggplot(aes(x = `First Income`)) +
  geom_histogram() +
  facet_grid(Location ~.)

house_fi_nwsw_hist 

#The images show histograms of the "First Income" variable for two groups: "SW" and "NW".
#Both groups have a right-skewed distribution, meaning there are fewer individuals with higher incomes.
#The median income for both groups appears to be around 60,000, but the mean income would be higher due to the right-skewness.
#The "SW" group seems to have a slightly larger spread of incomes compared to the "NW" group, and both groups have a few out liners, especially in the "SW" group.
#Overall, the "SW" and "NW" groups have similar distributions of first income, with both groups showing right-skewness and a few outliers.
#However, the "SW" group has a slightly larger spread of incomes.
#NW looks approximately normal, but both samples have a slight right skew; 
#SW more so than NW; 
#the average and median first income of SW is larger than NW
#both samples have outliers in the greater than direction

#C.test of normality and equality of variances
####first filter out each subset
house_fi_sw <- house %>%
  filter(Location == "SW") 

shapiro.test(house_fi_sw$`First Income`)
##Shapiro-Wilk normality test

##data:  house_fi_sw$`First Income`
##W = 0.94938, p-value = 0.0001083
##house_fi_nw <- house %>%
##  filter(Location == "NW") 

house_fi_nw <- house %>%
  filter(Location == "NW")
shapiro.test(house_fi_nw$`First Income`)
##Shapiro-Wilk normality test
##data:  house_fi_nw$`First Income`
##W = 0.9884, p-value = 0.3844
#the null hypothesis is normality. 
#the SW sample is not normal, the NW sample is normal

#a visual
house_fi_nwsw_qq <- house %>%
  ggplot(aes(sample = (`First Income`))) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Location ~.)

house_fi_nwsw_qq

#because SW is not normal, we will test for equal variance using Levene's test
house %>%
  leveneTest(data = ., `First Income` ~ as.factor(Location))
##Levene's Test for Homogeneity of Variance (center = median)
##Df F value Pr(>F)
##group   1  0.4002 0.5276
##250   
#the null hypothesis is equal variance.
#the 2 samples have equal variance

#D.  Wilcoxon Rank Sum Test
house_wmw <- wilcox.test(data = house,
                         `First Income` ~ as.factor(Location),
                         alternative = "two.sided",
                         mu = 0,
                         conf.int = TRUE, #why do we need this?
                         conf.level = 0.95, 
                         correct = TRUE)
house_wmw 
##Wilcoxon rank sum test with continuity correction

##data:  First Income by as.factor(Location)
##W = 11239, p-value = 1.102e-08
##alternative hypothesis: true location shift is not equal to 0
##95 percent confidence interval:
##  7420 14377
##sample estimates:
##  difference in location 
##10951 
house_wmw_tab <- map_df(list(house_wmw), tidy)

house_wmw_tab

#from HW3
house.test <- t.test(data = house,
                     `First Income` ~ as.factor(Location),
                     alternative = "two.sided",
                     mu = 0,
                     var.equal = TRUE,
                     conf.level = 0.95)

house.test
#Two Sample t-test

##data:  First Income by as.factor(Location)
##t = 5.8435, df = 250, p-value = 1.587e-08
##alternative hypothesis: true difference in means between group NW and group SW is not equal to 0
##95 percent confidence interval:
##  6879.942 13875.243
##sample estimates:
##  mean in group NW mean in group SW 
##58847.07         48469.48 


#The goal of this problem was to compare the first income levels of 
#randomly selected households in the NW and SW to see if they were different from each other. 
#We began assuming we could compare the differences using a t-test,
#and ran a Shapiro Wilk's test for normality to determine if the
#t-test would be a valid test. The results of the normality test
#revealed that the data for the SW were not normal, so we 
#conducted a Wilcoxon Rank Sum (WRS) Test, also know as a Mann-Whitney test,
#instead. The WRS test is a nonparametric tests, thus, it compares the
#median incomes of the two samples, not the means. The results indicate
#that the median incomes differ between the two regions (p-value = 0).
#As a point of comparison, the t-test was also run and a similar result
#was obtained (p-value = 0).


##Problem 2##
#read in file
golf <- read_xlsx("P18_29.xlsx", sheet = "Data", col_names = TRUE)

#make brand a factor
golf$Brand <- as.factor(golf$Brand)

#A. graph the data
golf_bp <- golf %>% 
  ggplot(aes(x = Brand, y = Distance, color = Brand)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = "cyan", geom = "point", 
               shape = 9, size = 2, show.legend=FALSE)

golf_bp
#The image shows a boxplot of the "Distance" variable for four brands (1, 2, 3, and 4). 
#Brand 2 has the lowest median distance and the smallest spread of distances. 
#Brand 4 has the largest spread of distances and the highest median distance. 
#Brand 1 and Brand 3 have skewed distributions and outliers.
#Brand 1 has the greatest mean distance followed by 4, 3, then 2. 
#the variances appear to be equal based upon the IQRs

#B.  view normality and test for equality of variances
#a visual
golf_qq <- golf %>%
  ggplot(aes(sample = (Distance))) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Brand ~.)

golf_qq

#each treatment sample appears normal, but due to the small sample size, we will test
#for equality of variance using Levene's Test

golf %>%
  leveneTest(data = ., Distance ~ Brand)
#Levene's Test for Homogeneity of Variance (center = median)
#Df F value Pr(>F)
#group  3   0.159 0.9235
#76   
#the null hypothesis is equal variance.
#the p-value is large enough to assume the 2 samples have equal variance

#C. perform the appropriate test:  Kruskal-Wallis Test
golf_kw <- kruskal.test(Distance ~ Brand, golf)

golf_kw
#Kruskal-Wallis rank sum test
#data:  Distance by Brand
#Kruskal-Wallis chi-squared = 51.018, df = 3, p-value = 4.848e-11

#multiple comparisons using Dunn's test
golf_kw_dun <- DunnTest(Distance ~ Brand, 
                        data = golf, 
                        method = "bonferroni")

golf_kw_dun
#Dunn's test of multiple comparisons using rank sums : bonferroni  

##mean.rank.diff    pval    
##2-1        -46.775 1.2e-09 ***
##  3-1        -24.700  0.0047 ** 
##  4-1         -4.425  1.0000    
##3-2         22.075  0.0160 *  
##  4-2         42.350 4.9e-08 ***
##  4-3         20.275  0.0348 *  
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#anova for comparison
golf_aov <- aov(Distance ~ Brand, golf)

summary(golf_aov)
##       Df Sum Sq Mean Sq F value Pr(>F)    
##Brand        3   7242  2414.1   49.81 <2e-16 ***
##  Residuals   76   3684    48.5                   
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#multiple comparisons using 95% Tukey CIs
golf_tuk <- TukeyHSD(golf_aov)

golf_tuk
##       Df Sum Sq Mean Sq F value Pr(>F)    
##Brand        3   7242  2414.1   49.81 <2e-16 ***
##  Residuals   76   3684    48.5                   
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#The PGA would like to determine how 4 golf balls differ by comparing how far a 
#robotic golfer can hit them. This is an analysis of more than 2 samples,
#so we will conduct an ANOVA to compare the mean distances or a Kruskal-Wallis test
#to compare the medians. After viewing the data, each sample has only 20 observations
#thus a Kruskal-Wallis test will be performed. 
#The results of the test indicate that there is a difference in how far each
#ball travels (p-value = 0). This result is confirmed by an ANOVA test (p-value = 0).
#A Dunn's test of differences was conducted, and the p-values from the 6 comparisons range
# from 0 to 1. The below differences are significant at the 1% level. The ball with the farthest
#distance is listed first.
#1-2, 3-1, and 4-2. If the expected Type I error is raised to 5%, the following 
#comparisons are also significant:  3-2 and 4-3. 
#Tukey CIs conclude that all comparisons listed above are significant at the 5% level.
#Thus, Dunn's test gives a more conservative result.




