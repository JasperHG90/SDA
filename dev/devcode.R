## Development / throwaway code

library(dplyr)

# Preparation --------

# This script installs packages
source("R/utilities/install_dependencies.R")

# This script loads and preprocesses the data
source("R/utilities/preprocess_data.R")

# This script load pre-processing functions
source("R/utilities/functions.R")

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
## Graph is bimodal
##  - Women peak at +-22% and 50%, men peak at 50% and +-78%

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

# Answers to questions -----

## 1. For the population level data, there are several measures of pay inequality reported. First, inspect at the population level, whether there is a difference in the mean employment and payments made to males and females across the UK.

payments <- gpg_core %>%
  summarize(avg = mean(DiffMeanHourlyPercent)) %>%
  print()
## The average mean percentage difference between pay for men and women is 14.4%

sqrt(var(gpg_core$DiffMeanHourlyPercent))

## it is (I think) useful to look at the prevalence of the gender gap in favour of men, i.e. to see in how many companies do men earn a higher average wage
neg_gap <- gpg_core[which(gpg_core$DiffMeanHourlyPercent < 0), ]
nrow(neg_gap)
##the gender gap (in the mean hourly rate) favouring women is prevalent in 749, or 11.15%, of the companies listed in the dataset
pos_gap <- gpg_core[which(gpg_core$DiffMeanHourlyPercent > 0), ]
nrow(pos_gap)
##in 5910 companies, or 88.03%, the mean wage is higher for male employees
no_gap <- gpg_core[which(gpg_core$DiffMeanHourlyPercent == 0), ]
nrow(no_gap)
#in the remaining 54 companies, or 0.008% there exists no gender wage gap

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
pos_gap <- gpg_core[which(gpg_core$DiffMedianHourlyPercent > 0), ]
nrow(pos_gap)
##in 5258 companies, or 78.32%, the median wage is higher for male employees
no_gap <- gpg_core[which(gpg_core$DiffMedianHourlyPercent == 0), ]
nrow(no_gap)
#in the remaining 552 companies, or 8.22%, there is no disparity in the median hourly wage between males and females 

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
           fct_relevel( c("lowerquartile", "lowermiddlequartile", 
                          "uppermiddlequartile", "topquartile"))) %>%
  ggplot(., aes(x=quantile, y=avg, color = gender)) +
  geom_line(aes(group = gender)) +
  geom_point() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
## Q. this data gives us nothing about the payment gap PER level, right? We cannot see if, say, the payment gap is bigger at the top than at the bottom since we don't have these data. All we can say is that the income distributions are skewed in favor of men, which is likely one reason that the payment gap exists in the first place. 

# 3. Imagine that instead of using the population, we want to sample. What would your conclusions be for the size of the gender payment gap if you would randomly sample 1000 cases at most from this dataset?

library(survey)
library(sampling)

gpg_core <- gpg_core %>%
  dplyr::select(uuid, EmployerSize, DiffMeanHourlyPercent, DiffMedianHourlyPercent, female, male,
                division)

# Total number of observations
N <- nrow(gpg_core)
n <- 1000

# Take SRS 
set.seed(400)
samp <- srswor(n, N)

# Subset data
sample <- gpg_core %>%
  filter(as.logical(samp)) %>%
  mutate(fpc = N)

# Surveydesign with equal probabilities
swordesign <- svydesign(ids=sample$uuid, fpc=~fpc, data = sample)

# Size of gender pay gap for mean and median pay
svymean(~DiffMeanHourlyPercent, swordesign)
#working with this simple random sample of 1000 companies, we find that the gender pay gap (in %) stands at 12.56% and 14.88% when assessing the median and mean wage, respectively. Both of these pay gaps are in favour of men. These values do not differ drastically from the population values of 12.2% and 14.1%. 

#the SE of 0.0041 for the SRS yields a margin of error (sampling error) of 1.96*0.0041 = 0.008036 = 0.8% 

# Calculate bias part and variance part

# 4. Stratification can potentially yield a more efficient sample. The dataset provides two variables that can be used for stratification: 
#     1. The size of the company (measured in no. of employees), and 
#     2. the type of industry (“SicDivision”). Note that there are many division codes, you may decide to recode this variable into fewer categories. Describe if, why and how you recode this variable if need be. Both can be thought of as potential variables to stratify on. We can oversample from particular large or small companies, and do the same for industry type (SicDivision).

# Create upper and lower bound for the median difference to filter
#repMean <- mean(gpg_core$DiffMedianHourlyPercent)
#lower_bound <- repMean - 2*sd(gpg_core$DiffMedianHourlyPercent)
#upper_bound <- repMean + 2*sd(gpg_core$DiffMedianHourlyPercent)

library(gridExtra)
# Plot the mean and median hourly difference
p1 <- ggplot(gpg_core #%>% filter(DiffMedianHourlyPercent >= lower_bound,
             #           DiffMedianHourlyPercent <= upper_bound) 
             ,aes(x=EmployerSize , 
                  y=DiffMedianHourlyPercent, 
                  group=EmployerSize)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_flip() +
  #scale_y_continuous(breaks = seq(-1,1,0.2)) + 
  geom_hline(yintercept=0, color = "red")
p2 <- ggplot(gpg_core #%>% filter(DiffMedianHourlyPercent >= lower_bound,
             #           DiffMedianHourlyPercent <= upper_bound) 
             ,aes(x=EmployerSize , 
                  y=DiffMeanHourlyPercent, 
                  group=EmployerSize)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_flip() +
  #scale_y_continuous(breaks = seq(-1,1,0.2)) + 
  geom_hline(yintercept=0, color = "red")

grid.arrange(p1, p2)
## We see: high variance within strata --> not a lot of difference between strata. This is a problem if we want to stratify the data.

## We can test this using an ANOVA difference of means
a <- aov(DiffMeanHourlyPercent ~ EmployerSize, gpg_core)
# Post-hoc tests
TukeyHSD(a, conf.level=0.95) # --> no difference between the groups

## SIC divisions
library(RColorBrewer)
# Look at percentages male/female by SIC division
by_sic <- gpg_core %>%
  # Select male/female/division columns
  select(male, female, division) %>%
  # Take columns and reshape to two columns (variable names and values), disregard division, which should stay in its own column
  gather(variable, value, -division) %>%
  # Group by division and gender (variable)
  group_by(division, variable) %>%
  # Per division and gender group, calculate the average percentage of males/females
  summarize(avgperc = mean(value)) %>%
  # Ungroup the data
  ungroup() %>%
  # This anonymous function further subsets the data and then merges it with the results up until now
  (function(data) {
    
    # Create an order for the SIC sections (this is useful for the plot below). We are going to order the SIC divisions by the % of males
    
    # Save data in temporary variable
    perc_by_division <- data
    
    # Further subset the data
    data %>%
      # Filter for males
      filter(variable == "male") %>%
      # Order s.t. highest --> lowest
      arrange(desc(avgperc)) %>%
      # Add an ordering per SIC division
      mutate(order = 1:n()) %>%
      # Remove the variable and percentage columns
      select(-avgperc, -variable) %>%
      # Merge the data with the perc_by_division dataset, which now has a new column 'order' to specify the order of the divisions
      left_join(perc_by_division) 
    
  })

# Plot the data. Reorder the x-values (SIC division) by the order we just calculated.
p <- ggplot(by_sic, aes(x=reorder(division,order) , y=avgperc, fill=variable)) +
  # Bar plot --> statistic to show is just the number
  geom_bar(stat = "identity") +
  # Modidify the x-axis s.t. we abbreviate the industry texts (some are very long)
  scale_x_discrete(name = "", label = function(x) abbreviate(x, minlength=20)) +
  # Set the x-axis labels at an angle and adjust the height
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_brewer(palette="Set2", name = "Gender") +
  # y variable scale name
  scale_y_continuous(name = "Percentage of males/females",
                     labels = scales::percent)

p

## Also print a table with SIC divisions, order, percentage of males, number of companies in that division & percentage
knitr::kable(by_sic %>% 
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

## Look at variance within and between groups 

# Abbreviate division names
gpg_core$div_shortened <- abbreviate(gpg_core$division)
# Run ANOVA
a <- aov(DiffMedianHourlyPercent ~ div_shortened, gpg_core)
# Run post-hoc tests
TukeyHSD(a, conf.level=0.95) # --> there are differences between groups

# Answer the following question:
#
# Make a motivated choice for whether you want to stratify, how you made decision for how to stratify, and how you want to stratify. 

# Based on the above analysis, we would conclude the following:
#  (1) the EmployerSize variable is not a useful stratification variable for several reasons.
#     - The variance between groups is not big enough. The variance within groups is large.
#     - this variable is not available on the sampling frame but a question in the questionnaire. 
#  (2) SIC division looks more promising. We always have this variable on the sampling frame and there seems to be enough variability between groups

# Based on these observations, we would choose to stratify based on company division. 

# These two divisions are too small to be sampled:
# -ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES   3
# -ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS                     3

# We exclude these two divisions for the following reasons:
# 1. There are only included 6 data points, which are difficult to sampling.
# 2. They contain less than 0.09% of population.
# 3. The divisions themself have special characteristics. Therefore, it is not possible to combine with other divisions. 

# 6. (related to Q5.) If you would be limited to sample at most 1000 companies, what would your conclusion be about the size of the gender pay gap? What is your precision? How much more precise is your answer as compared to your answers under questions 1. And 2.? 

## We stratify using 'division' as stratification variable

# proportional-to-size stratified sample for 'division'
sample_stratum <- stratify(gpg_core, "division", n=1000, seed=400)

# Surveydesign for stratified sample
stratdesign_prop <- svydesign(ids=sample_stratum$uuid, 
                              fpc=~fpc, 
                              strata = ~Stratum,
                              data = sample_stratum)

# Calculate the population value & design effect
svymean(~DiffMeanHourlyPercent, design=stratdesign_prop, deff=TRUE)

# Use neyman allocation to stratify optimally
sample_optim <- stratify(gpg_core, 
                         "division", 
                         n=1000, 
                         seed=400,
                         optim=TRUE, 
                         optimVar = "DiffMeanHourlyPercent")

length(which(sample_optim$Stratum == 1))

# Surveydesign for stratified sample
sample_optim$Prob <- 1 / sample_optim$Prob
stratdesign_optim <- svydesign(ids=sample_optim$uuid, 
                               fpc=~fpc, 
                               strata = ~Stratum,
                               #weights = ~Prob,
                               data = sample_optim)


# Size of gender pay gap for mean and median pay
svymean(~DiffMedianHourlyPercent + DiffMeanHourlyPercent, swordesign)
svyquantile(~DiffMedianHourlyPercent + DiffMeanHourlyPercent, swordesign, c(.25,.50,.75),ci=TRUE)

## We would conclude pretty much the same thing: mean around 14%, median around 12%

# Get design effect
svymean(~DiffMeanHourlyPercent, design=stratdesign_optim, deff=TRUE)
svymean(~DiffMedianHourlyPercent, design=stratdesign_optim, deff=TRUE)

## What are our conclusions about the size of the gender gap based on this sample?
#   - The results show that we are still getting good estimates for the mean difference in hourly pay. The mean values are closer to the actual population values.
## What is our precision?
#   - The SE of both stratified samples is smaller than the SRS. The design effect is 0.8771 for the proportional-to-size stratified sample and 0.8685 for the optimally allocated stratified sample.
## Precision compared to stratified sample with proportion-to-size sampling & SRS?

# Summary for each division
summary(as.factor(gpg_core$division))


# 7.How large should the sample be to achieve a CV of 0.01 for the size of the gender pay gap in the mean income?
# proportional-to-size stratified sample for 'division'
cv(svymean(~DiffMeanHourlyPercent, design=stratdesign_prop, deff=TRUE))
# CV = 0.0261

# neyman allocation to stratify optimally
## this approach will occur problem of "not enough members in group", so I use the division as stratification
cv(svymean(~DiffMeanHourlyPercent, design=stratdesign_optim, deff=TRUE))
# CV = 0.0285

# To decrease CV, we incerase n until CV < 0.01
for (i in 1001:6707){
  sample_stratum <- stratify(gpg_core, "division", n=i, seed=400)
  
  stratdesign_prop <- svydesign(ids=sample_stratum$uuid, 
                                fpc=~fpc, 
                                strata = ~Stratum,
                                data = sample_stratum)
  
  if (cv(svymean(~DiffMeanHourlyPercent, design=stratdesign_prop, deff=TRUE))<0.01) break
}

i  ## 3919 is the smallest n which make CV < 0.01

svymean(~DiffMeanHourlyPercent, design=stratdesign_prop, deff=TRUE)
SE = 0.0037575
Ybar = 0.1441252
S2 = (991*SE^2)/(1-991/6713)
nprime = (S2/Ybar^2)/(0.01^2)
n = nprime/((nprime/6713)+1)
# n > 3629.658

svymean(~DiffMeanHourlyPercent, design=stratdesign_optim, deff=TRUE)
SE.op = 0.0040662
Ybar.op = 0.1424867
S2.op = (992*SE.op^2)/(1-992/6713)
nprime.op = (S2.op/Ybar.op^2)/(0.01^2)
n.op = nprime.op/((nprime.op/6713)+1)
# n > 3929.959

# 8.weight the data according to the size of the company
# We use the middle number of each group as weight. Except the largest one (20,000 or more) as 20,000 and smallest one as 0
Weight <- c(0, 250/2, (250+499)/2, (500+999)/2, (1000+4999)/2, (5000+19999)/2, 20000)
clustermean <- tapply(gpg_core$DiffMeanHourlyPercent,gpg_core$EmployerSize,mean)
weighted.mean(clustermean,Weight)
# From individual perspective, the pay gap for mean is 0.1560

clustermedian <-tapply(gpg_core$DiffMedianHourlyPercent,gpg_core$EmployerSize,mean)
weighted.mean(clustermedian,Weight)
# The pay gap for median is 0.1040