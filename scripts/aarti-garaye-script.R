# Required Packages
library(tidyverse)
library(dplyr)
library(ggplot2)

url <- 'https://raw.githubusercontent.com/pstat197/module-0-class-survey-data-table_nine/main/data/'

background <- paste(url, 'background-clean.csv', sep = '') %>%
  read_csv()

interest <- paste(url, 'interest-clean.csv', sep = '') %>%
  read_csv()

metadata <- paste(url, 'survey-metadata.csv', sep = '') %>%
  read_csv()

# Add new variables to change beg, int, adv to 1, 2, 3 ratings for proficiency 
background <- background %>%
  mutate(num.prog.prof = case_when(
    prog.prof == "beg" ~ 1,
    prog.prof == "int" ~ 2,
    prog.prof == "adv" ~ 3
  ))

background <- background %>%
  mutate(num.stat.prof = case_when(
    stat.prof == "beg" ~ 1,
    stat.prof == "int" ~ 2,
    stat.prof == "adv" ~ 3
  ))

background <- background %>%
  mutate(num.math.prof = case_when(
    math.prof == "beg" ~ 1,
    math.prof == "int" ~ 2,
    math.prof == "adv" ~ 3
  ))

# Standardizing variables and adding them as 6 new variables
# std.math.prof, std.stat.prof, std.prog.prof 
# std.math.comf, std.stat.comf, std.prog.comf

math.prof.mean <- mean(background$num.math.prof)
math.comf.mean <- mean(background$math.comf)
math.prof.sd <- sd(background$num.math.prof)
math.comf.sd <- sd(background$math.comf)

stat.prof.mean <- mean(background$num.stat.prof)
stat.comf.mean <- mean(background$stat.comf)
stat.prof.sd <- sd(background$num.stat.prof)
stat.comf.sd <- sd(background$stat.comf)

prog.prof.mean <- mean(background$num.prog.prof)
prog.comf.mean <- mean(background$prog.comf)
prog.prof.sd <- sd(background$num.prog.prof)
prog.comf.sd <- sd(background$prog.comf)

background <- background %>%
  mutate(
    std.math.prof = (num.math.prof - math.prof.mean) / math.prof.sd,
    std.stat.prof = (num.stat.prof - stat.prof.mean) / stat.prof.sd,
    std.prog.prof = (num.prog.prof - prog.prof.mean) / prog.prof.sd,
    
    std.math.comf = (math.comf - math.comf.mean) / math.comf.sd,
    std.stat.comf = (stat.comf - stat.comf.mean) / stat.comf.sd,
    std.prog.comf = (prog.comf - prog.comf.mean) / prog.comf.sd
  )

# Adding the difference variable for all three areas (comf level - prof level)
background <- background %>%
  mutate(
    diff.math = std.math.comf - std.math.prof,
    diff.stat = std.stat.comf - std.stat.prof,
    diff.prog = std.prog.comf - std.prog.prof
  )

# Visualizations 

## Grouped Boxplots: Each area has two boxplots (one for proficiency and one for comf)
background_long <- background %>%
  select(std.math.prof, std.stat.prof, std.prog.prof,
         std.math.comf, std.stat.comf, std.prog.comf) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("measure", "domain"),
    names_pattern = "std\\.(.*)\\.(.*)",
    values_to = "value"
  )

ggplot(background_long, aes(x = domain, y = value, fill = measure)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(
    title = "Standardized Proficiency vs Comfort by Domain",
    x = "Domain",
    y = "Standardized Value (Z-score)",
    fill = "Subject Area"
  ) +
  theme_bw()

# Boxplots of differences 
# directly show whether people generally over- or underestimate their ability.

diff_long <- background %>%
  select(diff.math, diff.stat, diff.prog) %>%
  pivot_longer(
    cols = everything(),
    names_to = "domain",
    values_to = "difference"
  )

ggplot(diff_long, aes(x = domain, y = difference, fill = domain)) +
  geom_boxplot(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Difference Between Comfort and Proficiency (Standardized)",
    x = "Domain",
    y = "Comfort – Proficiency (Z-score Difference)"
  ) +
  theme_bw()


# Interpretation of Plot 1: Standardized Proficiency vs Comfort by Domain
# Each subject area has two plots: comfort level and proficiency level 
# Because everything is standardized, the average can be compared to 0 so positive values are above average
  # and negative values are below average. 
# The medians for comfort levels are higher than proficiency for math and programming whereas
  # for stats it's the opposite. 
# The boxplot shows us that there is some noticeable discrepancies between the reported comfort level
  # vs the proficiency level which needs to be investigated.


# Interpretation of Plot 2: boxplot of the "Difference" between comfort and proficiency 
# Each boxplot shows the difference between standardized comfort and 
  # proficiency for each domain. (Positive = more comfort than proficiency; Negative = less comfort.)
# The dashed line at 0 marks "no difference"
# The most difference is seen in Statistics. The median difference between comf and prof for math 
  # and programming is almost the same.
# Positive medians mean students feel more confident than proficient and negative medians means
  # they feel more proficient than confident.
# The spread shows high individual variance which we saw in the Combinations table in Tuesday, oct 7th lecture
  # under the combinations slide 25/28 


# Both of the plots show that this difference is worth investigating to see whether students generally feel
  # over or under confident when reporting their comfort and proficiency levels.