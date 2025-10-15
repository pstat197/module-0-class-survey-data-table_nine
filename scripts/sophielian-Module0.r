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

# Interpretation of results

TukeyHSD(aov_result)

# Interpretation of finding which two domain means differ from one another






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




# Equal Variances:

plot(aov_result$residuals ~ aov_result$fitted.values)

# In this plot of residuals versus fitted values, we have vertical columns of 
# data points that correspond to the different treatment groups in this model. 
# Within each group, we can observe that the residuals are approximately scattered 
# around 0. This indicates that they have approximately equal variances, 
# satisfying the ANOVA assumption.



# Independence:

# The assumption of independence is reasonably met because each observations
# (student response) in the dataset is independent of others. The survey was 
# distributed to all students in the class, and each student completed it 
# individually. There is no indication that responses were influenced by or 
# dependent on other participants' responses.



