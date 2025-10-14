# T-test:

background <- background %>%
  mutate(
    diff.math = std.math.comf - std.math.prof,
    diff.stat = std.stat.comf - std.stat.prof,
    diff.prog = std.prog.comf - std.prog.prof
  )


t_math <- t.test(background$diff.math, mu = 0)
t_stat <- t.test(background$diff.stat, mu = 0)
t_prog <- t.test(background$diff.prog, mu = 0)


#t-tests one-to-one for each mean
t_math_to_stat <- t.test(background$diff.math, background$diff.stat)
t_stat_to_prog <- t.test(background$diff.stat, background$diff.prog)
t_prog_to_math <- t.test(background$diff.prog, background$diff.math)

library(knitr)
kable()

library(knitr)
kable(t_results, digits = 3, caption = 
        "One-sample t-tests for comfort–proficiency differences by domain")

# need to make not round to 0 for mean_difference and t_statistic
# interpret values




# ANOVA Test:
diff_long <- background %>%
  select(diff.math, diff.stat, diff.prog) %>%
  pivot_longer(
    cols = everything(),
    names_to = "domain",
    values_to = "difference"
  )


anova_model <- aov(difference ~ domain, data = diff_long)
summary(anova_model)


TukeyHSD(anova_model)

# Standardizing Process
background <- background %>%
  mutate(new.prog.prof = case_when(
    prog.prof == "beg" ~ 1,
    prog.prof == "int" ~ 2,
    prog.prof == "adv" ~ 3
  ))

background <- background %>%
  mutate(new.stat.prof = case_when(
    stat.prof == "beg" ~ 1,
    stat.prof == "int" ~ 2,
    stat.prof == "adv" ~ 3
  ))

background <- background %>%
  mutate(new.math.prof = case_when(
    math.prof == "beg" ~ 1,
    math.prof == "int" ~ 2,
    math.prof == "adv" ~ 3
  ))
prog_diff <- background$new.prog.prof - background$prog.comf
math_diff <- background$new.math.prof - background$math.comf
stat_diff <- background$new.stat.prof - background$stat.comf

prog_diff_standardized <- scale(prog_diff)
math_diff_standardized <- scale(math_diff)
stat_diff_standardized <- scale(stat_diff)

prog_diff_standardized
math_diff_standardized
stat_diff_standardized

# Tidying Dataset
library(dplyr)
library(tidyr)

anova_data <- data.frame(
  participant = 1:nrow(background),  # create an ID for each participant
  math = as.numeric(math_diff_standardized),
  stats = as.numeric(stat_diff_standardized),
  prog = as.numeric(prog_diff_standardized)
)

long_data <- anova_data %>%
  pivot_longer(cols = c(math, stats, prog),
               names_to = "domain",
               values_to = "std_diff")

aggregate(std_diff ~ domain, data = long_data, mean)
aggregate(std_diff ~ domain, data = long_data, sd)

summary_aov <- summary(aov(std_diff ~ domain, data=long_data))
summary_aov

#ANOVA Assumption Checking

# Normality
qqnorm()
qqline()

# QQ plot

# Residuals vs. Fitted plot

# interpret values
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

library(tidyr); library(dplyr)
long_data <- anova_data %>%
  pivot_longer(cols = c(math, stats, prog),
               names_to = "domain",
               values_to = "diff")

# RM ANOVA
aov_result <- aov(diff ~ domain + Error(participant/domain), data = long_data)
summary(aov_result)
