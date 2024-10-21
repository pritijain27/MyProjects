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
golf <- read_xlsx("P18_29.xlsx", sheet = "Data", col_names = TRUE)

#make brand a factor
golf$Brand <- as.factor(golf$Brand)

#A. graph the data
golf_bp <- golf %>% 
  ggplot(aes(x = Brand, y = Distance, color = Brand)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = "pink", geom = "point", 
               shape = 8, size = 2, show.legend=FALSE)

golf_bp

#Brand 1 has the greatest mean distance followed by 4, 3, then 2. 
#the variances appear to be equal based upon the IQRs
#X-axis: Represents the different brands of golf balls.
#Y-axis: Represents the distance traveled by the golf balls.
#Boxplot: Shows the distribution of distances for each brand, including the median, quartiles, and potential outliers.
#Pink Points: Represent the mean distance for each brand, marked with a pink point in the shape of an asterisk.
#Observations from the Graph:
#  Central Tendency:
#  The median distances (represented by the line inside each box) vary across the brands.
#The mean distances (pink points) provide an additional measure of central tendency and are generally close to the medians.
#Spread:
#  The interquartile ranges (IQRs), represented by the height of the boxes, indicate the spread of the middle 50% of the data.
#Some brands may have wider boxes, indicating more variability in distances.
#By comparing the medians and means across brands, we can get an initial sense of which brands might perform better in terms of distance.
#Differences in the spread and presence of outliers can also provide insights into the consistency of each brand.

#B.  view normality and test for equality of variances
#a visual
golf_qq <- golf %>%
  ggplot(aes(sample = Distance, color = Brand)) +
  stat_qq() +
  stat_qq_line() +
  facet_grid(Brand ~ .)

golf_qq

#each treatment sample appears normal, but due to the small sample size, we will test
#for equality of variance using Levene's Test
#Normality Check:
#  The QQ-plots allow us to visually assess the normality of the distance data for each brand.
#If the points lie approximately along the reference line (the QQ-line), the data for that brand can be considered normally distributed.
#Brand 1:
#  The points for Brand 1 appear to follow the QQ-line closely, suggesting that the distance data for Brand 1 is approximately normally distributed.
#Brand 2:
#  The points for Brand 2 also follow the QQ-line fairly well, indicating normality in the distance data for Brand 2.
#Brand 3:
#  The points for Brand 3 show some deviations from the QQ-line, particularly at the tails, which may suggest slight deviations from normality.
#Brand 4:
#  The points for Brand 4 generally follow the QQ-line, but there may be some minor deviations at the tails, indicating that the data is mostly normal with some potential outliers.

golf %>%
  leveneTest(data = ., Distance ~ Brand)
#the null hypothesis is equal variance.
#the p-value is large enough to assume the 2 samples have equal variance
#P-value (0.9235):
#  Since the p-value is much greater than the common significance level (e.g., 0.05), we fail to reject the null hypothesis.
#This means there is no significant evidence to suggest that the variances of the distances are different across the four brands of golf balls.

#C. perform the appropriate test
golf_aov <- aov(Distance ~ Brand, golf)

summary(golf_aov)

#h0:  all means are equal
#ha:  at least one mean is not equal
#The ANOVA test is used to determine if there are statistically significant differences in the mean distances traveled by the four brands of golf balls
#Degrees of Freedom (Df):
#  Brand: 3 (since there are four brands, the degrees of freedom for the groups is 4 - 1 = 3)
#Residuals: 76 (total sample size minus the number of groups, 80 - 4 = 76)
#Sum of Squares (Sum Sq):
#  Brand: 7242 (variation due to the differences between the brands)
#Residuals: 3684 (variation within the brands)
#Mean Squares (Mean Sq):
#  Brand: 2414.1 (Sum Sq divided by its respective degrees of freedom)
#Residuals: 48.5 (Sum Sq divided by its respective degrees of freedom)
#F value: 49.81
#This is the test statistic for the ANOVA test. It compares the variance between the groups to the variance within the groups.
#Pr(>F): <2e-16
#Since the p-value is much smaller than the common significance level (e.g., 0.05), we reject the null hypothesis.
#This means there is strong evidence to suggest that there are significant differences in the mean distances traveled by the four brands of golf balls.

#the p-value is ~0, so the at least one mean is not equal

#D. multiple comparisons using 95% Tukey CIs
golf_tuk <- TukeyHSD(golf_aov)

golf_tuk

#h0:  mu1-mu2=0
#ha:  mu1-m2!=0
#Tukey’s Honest Significant Difference (HSD) test is used to identify which specific pairs of means are significantly different after finding a significant result in an ANOVA test
#Significant Differences:
#Brand 2 has significantly lower mean distances compared to Brands 1, 3, and 4.
#Brand 3 has significantly lower mean distances compared to Brands 1 and 4.
#Brand 4 has significantly higher mean distances compared to Brands 2 and 3.
#No Significant Difference:
#  There is no significant difference in the mean distances between Brand 4 and Brand 1.

# Plot of the Tukey intervals with color
plot(golf_tuk, las = 1, col = "blue")

#only 1 interval is not significant; Brand 4 is statistically equal to Brand 1
#all other combinations are significant
#Brand 1 is greater than 2 and 3, so can be assumed to be the best in terms of distance
#Brand 4 is next best and is greater than 2 and 3
#Brand 3 is greater than 2.

#PARAGRAPH ANSWER

#Recommendations
#For the Professional Golf Association:
#  Consider focusing on Brands 1 and 4 for tournaments or professional use, as they generally provide better performance in terms of distance.
#Further investigate the factors contributing to the lower performance of Brand 2 to understand if improvements can be made.

#Using the data provided we evaluated four different brands of golf balls to determine which ball hits the farthest.
#A sample size of 80 balls were randomly hit by a robot in order to record the distance.
#A boxplot of the four sample distances indicates that Brand 1 has the greatest mean distance followed by Brand 4, 3, and lastly 2.
#We visually determined that the data are normal, but due to the small sample size, 
#chose to analyze whether or not the variances are equal based upon Levene's Test. 
#Because the test indicated that the variances of the four samples were equal (p-value = 0.9235), 
#we performed a One-Way ANOVA which indicated that the mean distances for the brands differ (p-value = 0). 
#95% Tukey confidence intervals were formed for each brand-pair to determine the statistical ordering of the means.
#The results indicate that Brand 4 and Brand 1 do not differ in terms of mean distance,
#but Brand 1 hits farther than 3 and 2. 
#Brand 4 is next best and is greater than 2 and 3
#Brand 3 is greater than 2.

########Problem 2########
#A. Data format
#80 obs. 1 for each hit.
#var1 = Person, var2 = Brand, var3 = Distance

#B. Analysis
#Randomized block design
#Block = Person
#Brand = Treatment
#aov(Distance ~ Person + Brand)

#each person has their own swing that should be controlled for
#by blocking

To compare the mean distances traveled by four brands of golf balls using human golfers, the data format would be structured as follows:
  Data Collection
Participants: Recruit a sufficient number of human golfers.
Golf Balls: Use the same four brands of golf balls.
Experimental Design: Each golfer hits a subset of the 80 balls, ensuring each brand is represented equally. The order of hits is randomized.

Data Structure
Each row in the dataset represents a single hit of a golf ball by a golfer. The variables in each row include:
  Person: Identifier for each golfer (e.g., Golfer 1, Golfer 2, etc.).
Brand: The brand of the golf ball (categorical variable with four levels: Brand 1, Brand 2, Brand 3, Brand 4).
Distance: The distance traveled by the golf ball (continuous variable).

Number of Observations
Total Observations: 80 (one for each hit).
Observations per Golfer: Depends on the number of golfers. For example, if there are 10 golfers, each golfer would hit 8 balls (80 balls / 10 golfers).

Analysis
The appropriate analysis for this study is a Randomized Block Design. Here’s how it works:
  Block: The golfers (Person) are considered blocks. Each golfer has their own unique swing, which can introduce variability. By blocking, we control for this variability.
Treatment: The brands of the golf balls are the treatments we are comparing.

Explanation
Blocking: Each golfer’s swing is unique and can affect the distance the ball travels. By treating each golfer as a block, we control for this variability, ensuring that differences in distances are attributed to the brands rather than the golfers’ swings.
Treatment: The brands are the treatments we are interested in comparing. The goal is to determine if there are significant differences in the mean distances traveled by the balls from different brands.

####The model for the analysis would be:
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Example data frame
golf <- data.frame(
  Person = rep(1:10, each = 8),
  Brand = rep(rep(1:4, each = 2), 10),
  Distance = rnorm(80, mean = 250, sd = 15) # Example distances
)

# Perform the ANOVA
golf_aov <- aov(Distance ~ Person + Brand, data = golf)

# Summary of the ANOVA
summary(golf_aov)


########Problem 3########
#read in file
car <- read_xlsx("P18_20.xlsx", sheet = "Data", col_names = TRUE)

#make brand a factor
car$Age <- as.factor(car$Age )

#A. graph the data
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Summarize the data and create the plot
car_means <- car %>%
  group_by(Age, `Spouse Present`) %>%
  summarise(mean = mean(`Purchase Price`)) %>%
  ggplot(aes(x = Age, y = mean, color = `Spouse Present`)) +
  geom_point(size = 3, shape = 16) +  # Larger points with a different shape
  geom_line(aes(group = `Spouse Present`), size = 1.2) +  # Thicker lines
  scale_color_manual(values = c("blue", "red")) +  # Custom colors for the groups
  labs(
    title = "Mean Purchase Price by Age and Spouse Presence",
    x = "Age of Buyer",
    y = "Mean Purchase Price",
    color = "Spouse Present"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Print the plot
print(car_means)

#Interpretation of the Plot
#Age Trends: we can observe how the mean purchase price varies with the age of the buyer.We can see information such as whether younger or older buyers tend to spend more on new automobiles.
#Spouse Presence: The plot also allows us to compare the mean purchase prices between buyers who were accompanied by their spouse and those who were not. This can help identify if there is a significant difference in spending behavior based on spouse presence.

#there looks to be an interaction. 
#Age group 3 and a spouse is present results in the
#greatest mean

#can view the interaction the other way
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Summarize the data and create the plot
car_means_b <- car %>%
  group_by(Age, `Spouse Present`) %>%
  summarise(mean = mean(`Purchase Price`)) %>%
  ggplot(aes(x = `Spouse Present`, y = mean, color = Age)) +
  geom_point(size = 2) +
  geom_line(aes(group = Age)) 

car_means_b

#The plot uses different colors to represent different age groups, making it easy to compare the mean purchase prices across these groups.
#The x-axis represents whether the buyer was accompanied by their spouse (Yes or No).
#The y-axis shows the mean purchase price for each group.
#By examining the points and lines, we can observe how the mean purchase price varies depending on whether the buyer was accompanied by their spouse.
#The interaction between age and spouse presence can be observed by comparing the lines for different age groups.

#B.  Test for equality of variances
#a visual
car %>%
  leveneTest(data = ., `Purchase Price` ~ Age*`Spouse Present`)
#the null hypothesis is equal variance.
#the p-value is large enough to assume the 2 samples have equal variance
#Interpretation:
#  p-value: The p-value of 0.6927 is much higher than the common significance levels (0.05 or 0.10). This indicates that there is no significant evidence to reject the null hypothesis of equal variances. In other words, the variances across the different groups are not significantly different.
#F value: The F value of 0.6098 is relatively low, further supporting the conclusion that there is no significant difference in variances among the groups.
#Conclusion:
#  Equal Variance Assumption: Since the p-value is greater than 0.10, we fail to reject the null hypothesis. This means that the assumption of equal variances holds true for the data. We can proceed with the ANOVA analysis without concerns about violating the homogeneity of variance assumption.


#C. 2-Way ANOVA
car_2way <- aov(`Purchase Price` ~ Age*`Spouse Present`, car) #original anova
car_2way_III <- Anova(car_2way, type = "III") #use original as argument in Anova

summary(car_2way)
car_2way_III

#the p-value of the interaction is significant depending on our LOS. 
#at 10% the interaction is significant. 
#the main effects are more difficult to analyze
#based upon the p-values of main effects, spousal presence
#does not have an effect, but Age does.

#The intercept is highly significant, indicating that the overall mean purchase price is significantly different from zero.
#The age of the buyer has a significant effect on the purchase price at the 5% significance level. This suggests that the amount of money spent on a new automobile varies significantly with the age of the buyer.
#Whether the buyer is accompanied by their spouse has a marginally significant effect on the purchase price at the 10% significance level. This indicates a potential influence of spouse presence on the purchase price, but it is not strong enough to be considered significant at the 5% level.
#The interaction between age and spouse presence is marginally significant at the 10% significance level. This suggests that the effect of age on the purchase price may depend on whether the buyer is accompanied by their spouse, but this interaction is not strong enough to be considered significant at the 5% level.
#Analysis at Different Significance Levels:
 # At 10% Significance Level: Both the main effect of spouse presence and the interaction effect are considered marginally significant, suggesting some influence on the purchase price.
 # At 5% Significance Level: Only the main effect of age is considered significant, indicating that age is the primary factor affecting the purchase price.


#B. normality based upon residuals
#use the residuals of the test to test for normality
car_resid <- as.data.frame(residuals(car_2way))

car_resid <- car_resid %>%
  rename(resid = 1)

#plot the residuals
car_resid %>%
  ggplot(aes(sample = resid)) +
  stat_qq(color = "blue", size = 2) +  # Add color and adjust point size
  stat_qq_line(color = "red", linetype = "dashed", size = 1) +  # Add color, change line type and size
  labs(
    title = "Q-Q Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
#looks normal, and we have a large enough sample size
#Key Observations:
 # Alignment with the Line:
  #If the points closely follow the red dashed line, it indicates that the residuals are approximately normally distributed.
#Deviations from the line, especially at the tails, suggest departures from normality.
#Patterns and Deviations:
  #Systematic deviations (e.g., a curve) might indicate skewness in the residuals.
#Outliers or points far from the line suggest potential issues with normality.

#normality test of residuals
shapiro.test(car_resid$resid) 
#p-value = 0.2548:
#The p-value is much greater than 0.05, indicating that there is no significant evidence to reject the null hypothesis of normality.
#This means that the residuals from the model are approximately normally distributed.

#Since the p-value is greater than 0.05, we fail to reject the null hypothesis. This supports the assumption that the residuals are normally distributed.

#h0: data are normal
#ha: data are not normal

#the p-value is large enough to assume normality

#D. LOS of 5%
#the interaction is not significant.
#remove the interaction term and do a 2-Way ANOVA without the interaction.
#if either treatment is significant, do a multiple comparison of means.

#PARAGRAPH ANSWER

Statistical Assumptions: Both the equal variance and normality assumptions were met, supporting the validity of the ANOVA results.
Significance Levels: At the 10% significance level, both spouse presence and the interaction effect are considered marginally significant, indicating potential influences that warrant further investigation. 
At the 5% level, only age is significant, highlighting it as the primary factor affecting purchase prices.
Practical Implications: The findings suggest that automobile dealers should consider the age of buyers as a significant factor in purchase behavior. 
The presence of a spouse may also influence spending, though this effect is less pronounced.

#Using the data provided, we analyzed the effect of age and spousal presence
#on the purchase price of a car.
#A graph of the means of each age-spousal presence group indicates that the mean
#price of a car changes differently for each age group when a spouse is present.
#Two statistically analyze this relationship, we first ran a Levene's test to determine
#if the variances of each treatment combination are equal.
#The test indicated that the variances are equal (p-value = 0.6927); normality was
#tested after our ANOVA based upon the residuals of the model.
#A Two-Way ANOVA was evaluated, with age and spousal preference as main effects on the purchase price
#along with an interaction term of the two variables.
#The interaction of age and spousal preference was found to be significant (p-value = 0.0581)
#based upon a type I error of 10%.
#The main effects were also significant (p-value age = 0.0104; p-value spouse = 0.7447), but
#are harder to disambiguate due to the significance of the interaction term. 
#If a type I error of 5% is preferred, then the interaction is not significant, and the
#individual main effects could be analyzed with a new model without the interaction term.

#for fun!
car_2way_b <- aov(`Purchase Price` ~ Age + `Spouse Present`, car) #original anova
car_2way_III_b <- Anova(car_2way_b, type = "III") #use original as argument in Anova

summary(car_2way_b)
car_2way_III_b
#The ANOVA results indicate the following:
#Intercept: Highly significant (p < 2.2e-16), confirming a significant overall mean purchase price.
#Age: Highly significant (p = 9.089e-08), indicating that the age of the buyer significantly affects the purchase price.
#Spouse Present: Not significant (p = 0.837), suggesting that whether the buyer is accompanied by their spouse does not significantly impact the purchase price.
#Residuals: Represent the unexplained variation in purchase price.
#Summary:
#  Age is a significant factor in determining the purchase price of a new automobile, while the presence of a spouse does not have a significant effect.

