library(tidyverse)

# retrieve class survey data
url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab2-tidyverse/data/'

background <- paste(url, 'background-clean.csv', sep = '') %>%
  read_csv()

interest <- paste(url, 'interest-clean.csv', sep = '') %>%
  read_csv()

metadata <- paste(url, 'survey-metadata.csv', sep = '') %>%
  read_csv()

# print the data frame for inspection in the console
background

# open as a spreadsheet in a separate viewer
view(background)

# filter rows
background %>%
  filter(math.comf > 3)

# select a column
background %>%
  select(math.comf)

# pull a single column
background %>%
  pull(rsrch)

# define a new variable
background %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3)

# sequence of verbs, chaining operations together
background %>%
  filter(stat.prof == 'adv') %>%
  mutate(avg.comf = (math.comf + prog.comf + stat.comf)/3) %>%
  select(avg.comf, rsrch) 

background %>%
  filter(updv.num == "6-8" & rsrch == "TRUE") %>%
  select(contains("prof"))

background %>%
  filter(updv.num == "6-8" & rsrch == "FALSE") %>%
  select(contains("prof"))

background %>%
  select(contains("comf")) %>%
  summarize_all(median)

background %>%
  group_by(updv.num) %>%
  select(contains("comf")) %>%
  summarize_all(median)
