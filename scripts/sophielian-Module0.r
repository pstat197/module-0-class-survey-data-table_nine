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

TukeyHSD(aov_result)

#ANOVA Assumption Checking

# Normality
qqnorm()
qqline()

# QQ plot

# Residuals vs. Fitted plot

# interpret values
