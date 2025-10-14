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

# interpret values
