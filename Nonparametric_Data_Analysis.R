install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("car")
install.packages("broom")
install.packages("purrr")
install.packages("readxl")

library(dplyr)
library(tidyr)
library(ggplot2)
library(car) #levene's test for equality of variance
library(broom) #make our output pretty
library(purrr) #make our output pretty part deux
library(readxl)

#ordering factors
#data$column <- ordered(data$column, levels = c("level1", "level2", "...", "levelk"))

diam <- diamonds
View(diam)
help(diamonds)

#rename the x,y,z variables based upon help
diam <- diam %>%
  rename(tot_depth_perc = depth,
         length_mm = x,
         width_mm = y,
         depth_mm = z)

####let's work with only C3 diamonds
diam_c3 <- diam %>%  
  mutate(car_group = if_else(carat <= 1, "C1", 
                             if_else(carat <=2, "C2",
                                     if_else(carat <=3, "C3",
                                             if_else(carat <=4, "C4", "C5"))))) %>%
  filter(car_group == "C3")

#distribution of the prices for C3 diamonds
plot_price_c3 <- diam_c3 %>%
  ggplot(aes(x = price)) +
  geom_histogram()

plot_price_c3

########SIGN TEST (analogous to a one-sample t-test)########
####one-sample; sample is price
#is the mean/median price different from $15000?

#normality test
shapiro.test(diam_c3$price)

#normality test
#h0:  the data ARE normal
#ha:  the date are not normal

#the pvalue is 0; it is below all reasonable LOSs.
#thus we reject the null; the data are not normal!

#sign test
c3_price_sign <- wilcox.test(diam_c3$price, 
                        alternative = "two.sided",
                        mu = 15000,
                        conf.int = TRUE, #why do we need this?
                        conf.level = 0.95, 
                        correct = TRUE)
c3_price_sign #can I expect to spend a median value different than 15000

c3_price_sign_tab <- map_df(list(c3_price_sign), tidy)

c3_price_sign_tab

#t-test for comparison!
c3_price_test <- t.test(diam_c3$price, 
                        alternative = "two.sided",
                        mu = 15000,
                        conf.level = 0.95)
c3_price_test #can I expect to spend different than 15000

c3_price_test_tab <- map_df(list(c3_price_test), tidy)

c3_price_test_tab

####let's discuss


########WILCOXON RANK SUM TEST (MANN-WHITNEY) (analogous to a two-sample independent sample test)########
####two-samples; samples are price broken down by 2 cuts (very good and ideal)
#is the mean/median price of a very good diamond less than the mean/median of an ideal diamond?

#subset the dataset
price_vg_id <- diam_c3%>%
  filter(cut == "Very Good" | cut == "Ideal") 

#let's look at the data
price_vg_id %>%
  ggplot(aes(price, cut)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = "black", geom = "point", 
               shape = 8, size = 2, show.legend=FALSE)

####we need to start by checking normality 
#qq-plot of each sample separately
price_vg_id %>%
  ggplot(aes(sample = price)) +
  stat_qq() + 
  stat_qq_line() + 
  facet_grid(. ~ cut)

#pivot wider to have ideal and very good in separate columns then run shapiro.test
price_vg_id_sw <- diam_c3 %>%
  filter(cut == "Very Good" | cut == "Ideal") %>%
  select(price, cut) %>%
  group_by(cut) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = cut, values_from = price) %>%
  select(-row) %>%
  apply(2, shapiro.test)

price_vg_id_sw

####because the data are not normal, let's use Levene's test to test for equal variance

price_vg_id %>%
  leveneTest(data = ., price ~ cut)

#h0:  variances are equal
#ha:  variances are not equal

#with a pvalue of 0.08, you reject at a LOS of 5% and fail to reject at LOS of 10%

####the data are not normal, and the variance is borderline
####let's perform a nonparametric test!

#Wilcoxon Rank Sum (Mann-Whitney) test
price_vg_id_wmw <- wilcox.test(data = price_vg_id,
                             price ~ cut,
                             alternative = "less",
                             mu = 0,
                             conf.int = TRUE, #why do we need this?
                             conf.level = 0.95, 
                             correct = TRUE)
price_vg_id_wmw  

price_vg_id_wmw_tab <- map_df(list(price_vg_id_wmw), tidy)

price_vg_id_wmw_tab

#t-test for comparison!
####what is the average price difference between the 2 cuts? 
#is the average price of a very good diamond less than the average price of an ideal cut 
price_vg_id_test <- t.test(data = price_vg_id,
                           price ~ cut,
                           alternative = "less",
                           mu = 0,
                           var.equal = TRUE,
                           conf.level = 0.95)
price_vg_id_test

price_vg_id_test_tab <- map_df(list(price_vg_id_test), tidy)

price_vg_id_test_tab



####KRUSKAL-WALLIS TEST (analogous to a One-Way ANOVA with independent samples)####
####k-samples; samples are price broken down by the k levels of the cut variable 
#k = 5 (fair, good, very good, ideal, premium)
#does the mean/median price of a diamond depend on its cut?

#visualize the distribution of price broken down by cut
diam_c3 %>% 
  ggplot(aes(x = cut, y = price, color = cut)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = "black", geom = "point", 
               shape = 8, size = 2, show.legend=FALSE)

####we need to start by checking normality 
#qq-plot of each sample separately
diam_c3 %>%
  ggplot(aes(sample = price)) +
  stat_qq() + 
  stat_qq_line() + 
  facet_grid(. ~ cut)

#test for equal variance among cuts
diam_c3 %>%
  leveneTest(data = ., price ~ cut)

####the data are not normal and they do not have equal variance 

#Kruskal-Wallis Test 
diam_price_cut_kw <- kruskal.test(price ~ cut, diam_c3)

diam_price_cut_kw

#how do we interpret this p-value?

#ANOVA for comparison!
diam_price_cut_aov <- aov(price ~ cut, diam_c3)

summary(diam_price_cut_aov)

#h0:  all means are equal
#ha:  at least one mean is not equal

#the p-value is ~0, so the at least one mean is not equal

#Dunn's Test for Multiple Comparisons
install.packages("DescTools")
library(DescTools)

diam_price_cut_kw_dun <- DunnTest(price ~ cut, 
                                  data = diam_c3, 
                                  method = "bonferroni")

diam_price_cut_kw_dun


#Tukey comparison!
diam_price_cut_tuk <- TukeyHSD(diam_price_cut_aov)

diam_price_cut_tuk


####WILCOXON SIGNED RANK TEST (analogous to the t-test for paired samples)####
#do mice have a different weight after a treatment effect

install.packages("datarium")
library(datarium)

mice <- mice2 

mice <- mice %>%
  pivot_longer(cols = before:after, names_to = "group", values_to = "weight") #pivot longer so that you can reference the groups from one column 

#let's visualize!
mice %>%
  ggplot(aes(x = group, y = weight)) +
  geom_boxplot()


#Wilcoxon signed rank test
mice_rank <- wilcox.test(data = mice,
                         weight ~ group,  
                         paired = TRUE)
mice_rank


#paired t-test for comparison!
mice_test <- t.test(data = mice,
                    weight ~ group,  
                    paired = TRUE)
mice_test














