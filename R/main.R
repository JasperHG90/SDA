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

# By county & division
## Not a lot of difference over these variables.
byCounty <- gpg_core %>%
  select(county, division, female, male, DiffMeanHourlyPercent, DiffMedianHourlyPercent) %>%
  group_by(county, division) %>%
  summarize(n = n(),
            avgmale = mean(male),
            avgfem = mean(female),
            avg_mean_perc = mean(DiffMeanHourlyPercent),
            avg_med_perc = mean(DiffMedianHourlyPercent)) %>%
  arrange(county,desc(n))

# Create upper and lower bound for the median difference to filter
repMean <- mean(gpg_core$DiffMedianHourlyPercent)
lower_bound <- repMean - 2*sd(gpg_core$DiffMedianHourlyPercent)
upper_bound <- repMean + 2*sd(gpg_core$DiffMedianHourlyPercent)

ggplot(gpg_core #%>% filter(DiffMedianHourlyPercent >= lower_bound,
                #           DiffMedianHourlyPercent <= upper_bound) 
       ,aes(x=EmployerSize , 
            y=DiffMedianHourlyPercent, 
            group=EmployerSize)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-1,1,0.2)) + 
  geom_hline(yintercept=0, color = "red")

## 1. For the population level data, there are several measures of pay inequality reported. First, inspect at the population level, whether there is a difference in the mean employment and payments made to males and females across the UK.

payments <- gpg_core %>%
  summarize(avg = mean(DiffMeanHourlyPercent)) %>%
  print()
## The average mean percentage difference between pay for men and women is 14.4%

## it is (I think) useful to look at the prevalence of the gender gap in favour of men, i.e. to see in how many companies do men earn a higher average wage
neg_gap <- gpg_core[which(gpg_core$DiffMeanHourlyPercent < 0), ]
nrow(neg_gap)
##the gender gap (in the mean hourly rate) favouring women is prevalent in 749, or 11.15%, of the companies listed in the dataset
pos_gap <- gpg_core[which(gpg_core$DiffMeanHourlyPercent >= 0), ]
nrow(pos_gap)
##in the remaining 5964 companies, or 88.85%, the mean wage is equal or is higher for male employees

hist(gpg_core$DiffMeanHourlyPercent, main = "Gender gap in mean hourly pay (%)", xlim = c(-1, 1), xlab = "Size of pay gap")
##the histogram clearly shows that the distribution of the gender pay gap is heavily skewed in favour of males. 

## 1b. Apart from regular (often monthly) payments there are also bonus payments. Include bonus payments as well now.

payments <- gpg_core %>%
  summarize(avg = mean(DiffMeanHourlyPercent),
            avgbonus = mean(DiffMeanBonusPercent)) %>%
  print()
## The average mean bonus percentage difference is 14.1% in favor of men

# 3. Apart from means, statistics are also included on quartiles of the income distributions, as well as the median. Again, study the payment gap at the population level. Is your conclusion in question 2 different from 1?

medPayments <- gpg_core %>%
  summarize(avg = mean(DiffMedianHourlyPercent)) %>%
  print()
## Median bonus percentage difference is 12.2% in favor of men

#again just looking at the number of companies in which the median pay favours males, etc.
neg_gap <- gpg_core[which(gpg_core$DiffMedianHourlyPercent < 0), ]
nrow(neg_gap)
##the gender gap (in the median hourly rate) favouring women is prevalent in 903, or 13.45%, of the companies listed in the dataset
pos_gap <- gpg_core[which(gpg_core$DiffMedianHourlyPercent >= 0), ]
nrow(pos_gap)
##in the remaining 5810 companies, or 86.55%, the median wage is equal or is higher for male employees

hist(gpg_core$DiffMedianHourlyPercent, main = "Gender gap in median hourly pay (%)", xlim = c(-1, 1), xlab = "Size of pay gap")
##this shows that the distribution of the percentage gender gap in the median payments is also heavily skewed in favour of males 

library(stringr)
library(purrr)
library(forcats)

gpg_core %>%
  select(18:25) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarize(avg = mean(value)) %>%
  mutate(gender = ifelse(str_detect(variable, "Female"), "female", "male"),
         quantile = map_chr(variable, function(x) str_replace_all(tolower(x), "[fe]{0,2}male", "")) %>%
                              as_factor() %>%
                              fct_relevel( c("lowerquartile", "lowermiddlequartile", "uppermiddlequartile", "topquartile"))) %>%
  ggplot(., aes(x=quantile, y=avg, color = gender)) +
    geom_line(aes(group = gender)) +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
## Q. this data gives us nothing about the payment gap PER level, right? We cannot see if, say, the payment gap is bigger at the top than at the bottom since we don't have these data. All we can say is that the income distributions are skewed in favor of men, which is likely one reason that the payment gap exists in the first place. 

# 3. Imagine that instead of using the population, we want to sample. What would your conclusions be for the size of the gender payment gap if you would randomly sample 1000 cases at most from this dataset?

library(survey)
library(sampling)

# Total number of observations
N <- nrow(gpg_core)
n <- 1000

# Take SRS 
set.seed(600)
samp <- srswor(n, N)

# Subset data
sample <- gpg_core %>%
  filter(as.logical(samp)) %>%
  mutate(fpc = N)

# Surveydesign with equal probabilities
swordesign <- svydesign(id=~0,fpc=~fpc, data = sample)

# Size of gender pay gap for mean and median pay
svymean(~DiffMedianHourlyPercent + DiffMeanHourlyPercent, swordesign)
svyquantile(~DiffMedianHourlyPercent + DiffMeanHourlyPercent, swordesign, c(.25,.50,.75),ci=TRUE)

# Calculate bias part and variance part