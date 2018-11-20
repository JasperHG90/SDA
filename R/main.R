## Run this script line-by-line for the main analysis
## Gabriel, Anne & Jasper
## Gender paygap project
## Survey data Analysis
## Supervisor: Dr. Peter Lugtig
## Methodology & Statistics Dpt, Utrecht University

# Clean environment
rm(list=ls())

# Preparation --------

# This script installs packages
source("R/utilities/install_dependencies.R")

# This script loads and preprocesses the data
source("R/utilities/preprocess_data.R")

# Making sense of the forward-operating pipe ('%>%'): ----

library(dplyr)

##  The forward-operating pipe is a method of coding that allows you to 'pipe' results from one command to the next.
##  Instead of doing something like this:
mean(sample(1:1000, 5))
##  Where we take the mean of a random sample of numbers ranging from 1:1000, we can do this:
sample(1:1000, 5) %>% 
  mean()
##  Which looks a LOT cleaner. 
##  If you want to see what the intermediate output looks like (e.g. the results of the above after the sample() command),
##   you can just comment the pipe out, like this:
sample(1:1000, 5) #%>%
mean()

# Analysis -----

# There are two datasets:
#  - gpg_meta: contains data that are useful to have but not directly related to the analysis
#  - gpg_core: contains data that are directly related to the analysis
# If needed, these data can be merged using the uuid variable
glimpse(gpg_core)
# In the gpg_core dataset, the data have been augmented with:
#  - postal codes extracted from the company address
#  - county names extracted from the company address (if present)
#  - percentage males in company ('male')
#  - percentage females in company ('female')
#  - industries according to SIC codess
# Furthermore, the data have been transformed to their 'proper' type. That is:
#  - characters with much fewer levels than observations have been transformed to factors
#  - all percentages have been transformed to proportions

library(ggplot2)
library(ggExtra)
library(tidyr)

# Number of males/females
gpg_core %>%
  # Subset for male / female columns
  select(male, female) %>%
  # Reshape the data from male, female column to variable, value columns where variable == female or male and value equals the percentages
  gather(variable, value) %>%
  # Plot
  ggplot(., aes(x = variable, y=value, fill = variable)) +
    geom_jitter(alpha=0.2) +
    geom_boxplot() +
    theme_bw() 

gpg_core %>%
  # Subset for male / female columns
  select(male, female) %>%
  # Reshape the data from male, female column to variable, value columns where variable == female or male and value equals the percentages
  gather(variable, value) %>%
  # Plot
  ggplot(., aes(x = value, fill = variable)) +
    geom_density(alpha=0.3) +
    theme_bw() 
## Nb. the graphs are symmetric because these are proportions. We can see that there are a lot more companies where there is a relatively small percentage of women. Does this hold across sectors?

# Look at percentages male/female by SIC division
perc_by_division <- gpg_core %>%
  # Select male/female/division columns
  select(male, female, division) %>%
  # Take columns and reshape to two columns (variable names and values), disregard division, which should stay in its own column
  gather(variable, value, -division) %>%
  # Group by division and gender (variable)
  group_by(division, variable) %>%
  # Per division and gender group, calculate the average percentage of males/females
  summarize(avgperc = mean(value)) %>%
  # Ungroup the data
  ungroup()

# Create an order (this is useful for the plot below). We are going to order the SIC divisions by the % of males
perc_by_division %>%
  # Filter for males
  filter(variable == "male") %>%
  # Order s.t. highest --> lowest
  arrange(desc(avgperc)) %>%
  # Add an ordering per SIC division
  mutate(order = 1:n()) %>%
  # Remove the variable and percentage columns
  select(-avgperc, -variable) %>%
  # Merge the data with the perc_by_division dataset, which now has a new column 'order' to specify the order of the divisions
  left_join(perc_by_division) %>%
  # Plot the data. Reorder the x-values (SIC division) by the order we just calculated.
  (function(data) {
    
    ## Plot
    p <- ggplot(data, aes(x=reorder(division,order) , y=avgperc, fill=variable)) +
      # Bar plot --> statistic to show is just the number
      geom_bar(stat = "identity") +
      # Modidify the x-axis s.t. we abbreviate the industry texts (some are very long)
      scale_x_discrete(label = function(x) abbreviate(x, minlength=20)) +
      # Set the x-axis labels at an angle and adjust the height
      theme(axis.text.x = element_text(angle = 45, hjust=1))
    
    ## Also print a table with SIC divisions, order, percentage of males, number of companies in that division & percentage
    knitr::kable(data %>% 
                   filter(variable == "male") %>% 
                   select(division, order, avgperc) %>%
                   mutate(perc_male = round(avgperc, digits = 2)) %>% 
                   select(-avgperc) %>%
                   # Join this data with a quick calculation of the number of companies / division
                   left_join(., gpg_core %>% 
                               group_by(division) %>% 
                               summarize(number_companies = n())) %>%
                   # Add percentages
                   mutate(companies_perc = round(number_companies / sum(number_companies), digits = 2)) %>%
                   # Add mean/median difference in income
                   left_join(., gpg_core %>%
                                  group_by(division) %>%
                                  summarize(avg_mean_diff = round(mean(DiffMeanHourlyPercent), digits=2),
                                            avg_med_diff = round(mean(DiffMedianHourlyPercent), digits=2)))) %>%
      # Cat to console
      print()
    
    # Return plot
    return(p)
    
  })
