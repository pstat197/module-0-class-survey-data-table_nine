# Linear Transformation of Proficiency to be on the same scale as Comfort
# rescale prof (1-3) to 1-5
background <- background %>%
  mutate(prof_rescaled_math = (new.math.prof - 1) / (3 - 1) * (5 - 1) + 1,
         prof_rescaled_stat = (new.stat.prof - 1) / (3 - 1) * (5 - 1) + 1,
         prof_rescaled_prog = (new.prog.prof - 1) / (3 - 1) * (5 - 1) + 1)

# compute raw difference (rescaled proficiency minus comfort)
math_diff   <- background$prof_rescaled_math - background$math.comf
stat_diff   <- background$prof_rescaled_stat - background$stat.comf
prog_diff   <- background$prof_rescaled_prog - background$prog.comf

# build long data and run repeated-measures ANOVA
anova_data <- data.frame(
  participant = seq_len(nrow(background)),
  math = math_diff,
  stats = stat_diff,
  prog = prog_diff
)

library(tidyr)
library(dplyr)
long_data <- anova_data %>%
  pivot_longer(cols = c(math, stats, prog),
               names_to = "domain",
               values_to = "diff")

aov_result <- aov(diff ~ domain, data = long_data)
summary(aov_result)

# Interpretation of results with Tukey test

TukeyHSD(aov_result)

# Interpretation of finding which two domain means differ from one another
# Because we got a p-value of 0.179 from our ANOVA, we fail to reject our null
# hypothesis, and we conclude that there is no statistically significant evidence 
# that there is a difference among the mean differences across math, statistics, 
# and programming.

# In regard to the Tukey test, because all p-values were greater than 0.05, we
# cannot conclude that any pair of means were significant against each other.
# The confidence intervals all include 0, which shows us that none of the domain
# gaps differ signficantly from each other.



# Why We Assume Independence
# We assume independence in this case because each participant's score on the Likert scale of how 
# proficient/comfortable they are in each subject is independent of any other participant's score.


#ANOVA Assumption Checking

# Normality:

qqnorm(aov_result$residuals)
qqline(aov_result$residuals)

# Looking at the qqnorm() plot, we can see that the plotted residual points seem
# to fall far from the normal line towards the tails of the graph. Thus, we are 
# skeptical whether the assumption of normality is met.


shapiro.test(aov_result$residuals)

# A Shapiro–Wilk test on the ANOVA residuals indicated that the assumption of 
# normality was violated (W = 0.963, p < 0.001). This suggests that the residuals 
# deviate significantly from a normal distribution. However, since ANOVA is robust 
# to moderate non-normality with sufficiently large samples, the results can 
# still be interpreted with caution.





# Residuals vs. Fitted plot

plot(aov_result$residuals ~ aov_result$fitted.values)

# In this plot of residuals versus fitted values, we have vertical columns of 
# data points that correspond to the different treatment groups in this model. 
# Within each group, we can observe that the residuals are approximately scattered 
# around 0. This indicates that they have approximately equal variances, 
# satisfying the ANOVA assumption.
