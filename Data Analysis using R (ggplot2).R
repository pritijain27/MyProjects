install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("broom")
install.packages("purrr")
install.packages("car")
install.packages("readxl")

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)
library(car)
library(readxl)

#setwd
setwd("/Users/pritijain/Downloads/stats")

########Problem 1########
#read in file
house <- read_xlsx("P02_35.xlsx", sheet = "Data", col_names = TRUE)

#A. recode the location variables and
#filter out SW and NW
house <- house %>%
  mutate(Location = ifelse(Location == 1, "SW",
                           ifelse(Location == 2, "NW",
                                  ifelse(Location == 3, "NE", "SE")))) %>%
  filter(Location == "SW" | Location == "NW") 
#The Location variable has been recoded 


#B. create a histogram and/or boxplot for each sample to aid in describing the distribution
house_fi_nwsw_hist <- house %>%
  ggplot(aes(x = `First Income`)) +
  geom_histogram() +
  facet_grid(Location ~.)

house_fi_nwsw_hist 

house_fi_nwsw_bp <- house %>%
  ggplot(aes(x = `First Income`, y = Location)) +
  geom_boxplot() 

house_fi_nwsw_bp

#NW looks approximately normal, but both samples have a slight right skew; 
#SW more so than NW; 
#the average ($48,469.48) and median ($46,667) first income of SW is larger than NW
#both samples have outliers in the greater than direction
#SW maximum is ($90,488) NW maximum is ($98,881)

#C.  test of normality and equality of variances
####first filter out each subset
house_fi_sw <- house %>%
  filter(Location == "SW") 

shapiro.test(house_fi_sw$`First Income`)

house_fi_nw <- house %>%
  filter(Location == "NW") 

shapiro.test(house_fi_nw$`First Income`)

#the null hypothesis is normality. 
#the SW sample is not normal, the NW sample is normal
#W = 0.9884, p-value = 0.3844

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
#Levene's Test for Homogeneity of Variance (center = median)
#Df F value Pr(>F)
#group   1  0.4002 0.5276
#250

#the null hypothesis is equal variance.
#the 2 samples have equal variance

#can we confirm this result if we used var.test
var.test(data = house,
         `First Income` ~ as.factor(Location),
         alternative = "two.sided",
         ratio = 1,
         conf.level = 0.95)

#yes we can! data:  
#F = 1.0788, num df = 122, denom df = 128, p-value = 0.6712
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.7590199 1.5355753

#D. perform the appropriate test
house.test <- t.test(data = house,
                     `First Income` ~ as.factor(Location),
                     alternative = "two.sided",
                     mu = 0,
                     var.equal = TRUE,
                     conf.level = 0.95)

house.test
#NW is sample 1 and SW is sample 2
#h0:  the two means are equal
#ha:  the two means are NOT equal
#t = 5.8435, df = 250, p-value = 1.587e-08
#alternative hypothesis: true difference in means between group NW and group SW is not equal to 0
#95 percent confidence interval:
#  6879.942 13875.243
#sample estimates:
# mean in group NW mean in group SW 
#58847.07         48469.48 

house.test$p.value

#the p-value is ~0. we reject the null and conclude the two means are not equal
#the 2 locations have different First incomes

house.test$conf.int
# a 95% CI around the difference is ($6,879.94 to $13.875.24)
# we can assume NW First Income is stat. sig. greater by that amount with
# 95% confidence




########Problem 2########
#read in file
mawwage <- read_xlsx("P09_65.xlsx", sheet = "Data", col_names = TRUE)

#unstack the data
mawwage <- mawwage %>%
  pivot_longer(cols = Husband:Wife, names_to = "Partner", values_to = "Price")

#view the distributions of each sample
#can also view as the distribution of the difference of the 2 samples
mawwage %>%
  ggplot(aes(x = Partner, y = Price)) +
  geom_boxplot()

#there are no outliers. the median price for the wives is greater than the husbands

####is there any significant difference in the difference in price between husband and wife?
mawwage_test = t.test(mawwage$Price[mawwage$Partner == "Husband"],
                      mawwage$Price[mawwage$Partner == "Wife"],
                      paired = TRUE,
                      alternative = "greater")

mawwage_test
#t = -1.2978, df = 24, p-value = 0.8967
#alternative hypothesis: true mean difference is greater than 0
#5 percent confidence interval

#calculate each mean
mawwage %>%
  group_by(Partner) %>%
  summarise(Mean = mean(Price))

#mean price of husband is less, our mean difference is less, so husband is sample 1
#Husband 28268 Wife 29248

mawwage_test
#h0:  mean for husbands is <= wives
#ha: mean for husband is > wives
#the p-value indicates we should fail to reject the null, 
#and conclude that the mean for husbands is <= wives

## ALTERNATIVE CODE ##
# Load necessary library
library(readxl)

# Load the data from the Excel file
data <- read_excel("P09_65.xlsx")

# Extract the Husband and Wife columns
husband_data <- data$Husband
wife_data <- data$Wife

# Perform a paired t-test
t_test <- t.test(husband_data, wife_data, paired = TRUE)

# Print the results
print(t_test)

# Plot the distribution of each sample
par(mfrow = c(1, 2))

# Histogram for husbands
hist(husband_data, main = "Distribution of Amount Husbands are Willing to Spend on a New Car", xlab = "Amount", col = "blue", breaks = 10)
lines(density(husband_data), col = "red")

# Histogram for wives
hist(wife_data, main = "Distribution of Amount Wives are Willing to Spend on a New Car", xlab = "Amount", col = "green", breaks = 10)
lines(density(wife_data), col = "pink")

#A paired t-test was performed to determine if there is a significant difference in the amount husbands and wives are willing to spend on a new car.
#Paired T-Test Results:
#  Statistic: -1.298
#p-value: 0.207 (indicating no significant difference at the 5% significance level)
