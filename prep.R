library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

ss_names <- names(gender_pay_gap)[11:18]


ss <- gender_pay_gap %>%
  mutate(uuid = 1:n()) %>%
  select(ss_names) %>%
  # To long format
  gather(uuid, variable, value) %>%
  # Male / female?
  mutate(male = ifelse(str_detect(tolower(variable), "female"), 1, 0))

ss <- gender_pay_gap %>%
  select(ss_names) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarize(avg=mean(value))

  
