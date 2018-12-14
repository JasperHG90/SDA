## Run this script line-by-line for the main analysis
## Gabriel, Anne & Jasper
## Gender paygap project
## Survey data Analysis
## Supervisor: Dr. Peter Lugtig
## Methodology & Statistics Dpt, Utrecht University

# Clean environment
rm(list=ls())

# Clean environment
rm(list=ls())

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
#glimpse(gpg_core)
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
library(dplyr)
library(ggthemes)
library(RColorBrewer)

gpg_core %>%
  # Subset for male / female columns
  select(male, female) %>%
  # Reshape the data from male, female column to variable, value columns where variable == female or male and value equals the percentages
  gather(variable, value) %>%
  # Plot
  ggplot(., aes(x = value, fill = variable)) +
  geom_density(alpha=0.25) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2",
                    name = "Gender") +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "Density")

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

ggplot(gpg_core, aes(x=DiffMeanHourlyPercent)) +
  geom_histogram(color="black", fill = brewer.pal(1, "Set2")[1]) +
  scale_x_continuous(name = "Size of the mean pay gap",
                     limits = c(-1,1)) +
  theme_bw()
## Median
ggplot(gpg_core, aes(x=DiffMedianHourlyPercent)) +
  geom_histogram(color="black", fill = brewer.pal(1, "Set2")[2]) +
  scale_x_continuous(name = "Size of the median pay gap",
                     limits = c(-1,1)) +
  theme_bw()

# Look at mean/median bonus
meanb <- mean(gpg_core$DiffMeanBonusPercent)
medb <- mean(gpg_core$DiffMedianBonusPercent)

var(gpg_core$DiffMedianBonusPercent)
var(gpg_core$DiffMeanBonusPercent)

# One outlier of -1200 >.<
tr <- gpg_core %>% filter(abs(DiffMedianBonusPercent) <= 1) %>% select(DiffMedianBonusPercent)
medmean <- mean(tr$DiffMedianBonusPercent)

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
  geom_line(aes(group = gender), size=1.2) +
  geom_point(size=3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 15, hjust=1)) +
  scale_y_continuous(name = "") +
  scale_x_discrete(name = "", labels = c("Lower quartile", "Lower middle \nquartile", "Upper middle \nquartile", "Top quartile")) +
  scale_color_brewer(palette = "Set2") 

library(survey)
library(sampling)

## Subset data
gpg_core <- gpg_core %>%
  dplyr::select(uuid, EmployerSize, DiffMeanHourlyPercent, DiffMedianHourlyPercent, female, male,
                division)

# Total number of observations
N <- nrow(gpg_core)
n <- 1000

# Take SRS 
set.seed(402)
samp <- srswor(n, N)

# Subset data
sample <- gpg_core %>%
  filter(as.logical(samp)) %>%
  mutate(fpc = N)

# Surveydesign with equal probabilities
swordesign <- svydesign(ids=sample$uuid, fpc=~fpc, data = sample)

# Size of gender pay gap for mean and median pay
srs_mean <- svymean(~DiffMeanHourlyPercent + DiffMedianHourlyPercent, swordesign)

## Save SE
srs_se <- survey::SE(srs_mean)

## Save the margin of error in R object
marginerror <- survey::SE(srs_mean)*1.96

CI <- data.frame(
  "lower" = c(paste0(round(srs_mean[1] - marginerror[1], digits=4) * 100, "%"), 
              paste0(round(srs_mean[2] - marginerror[2], digits=4) * 100, "%")),
  "upper" = c(paste0(round(srs_mean[1] + marginerror[1], digits=4) * 100, "%"), 
              paste0(round(srs_mean[2] + marginerror[2], digits=4) * 100, "%"))
)
row.names(CI) <- c("Mean", "Median")
knitr::kable(CI, caption = "Lower and upper CI for the SRS design",
             col.names = c("Lower CI", "Upper CI"))

## For company size
order <- gpg_core %>%
  group_by(EmployerSize) %>%
  summarize(iqr_med = IQR(DiffMedianHourlyPercent),
            iqr_mean = IQR(DiffMeanHourlyPercent)) %>%
  arrange(desc(iqr_med)) %>%
  mutate(order_med = 1:n()) %>%
  arrange(desc(iqr_mean)) %>%
  mutate(order_mean = 1:n()) 

## Median plot
ggplot(gpg_core %>% left_join(order, by="EmployerSize"), aes(x=reorder(EmployerSize, order_med), 
                                                             y=DiffMedianHourlyPercent)) +
  geom_jitter(aes(color=gpg_core$EmployerSize)) +
  geom_boxplot(width=0.2,
               outlier.shape = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "None") +
  scale_x_discrete(name="") +
  scale_y_continuous(name = "Difference in Median Hourly Pay") +
  scale_color_brewer(palette = "Set2")

## Store in tmp data
tmp <- gpg_core %>% 
  left_join(order, by="EmployerSize") %>%
  ## Remove excessive values (+1 & -1)
  filter(DiffMeanHourlyPercent <= 1 & DiffMeanHourlyPercent >= -1)

## Mean plot
ggplot(tmp, 
       aes(x=reorder(EmployerSize, order_mean), 
           y=DiffMeanHourlyPercent)) +
  labs(caption=paste0("NB: ", nrow(gpg_core) - nrow(tmp), 
                      " observations have been removed due to excessive values (+-100% mean pay gap)"))+
  geom_jitter(aes(color=EmployerSize)) +
  geom_boxplot(width=0.2,
               outlier.shape = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "None",
        plot.caption = element_text(size=7)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name = "Difference in Mean Hourly Pay") +
  scale_color_brewer(palette = "Set2")


## NB. This codeblock is not included in the report because eval=FALSE & include = FALSE --> This is by design. You can still run the code manually if desired.

## Employersize

# Run ANOVA
a_EMPSIZE_mean <- aov(DiffMeanHourlyPercent ~ EmployerSize, gpg_core)
# Run post-hoc tests
TukeyHSD(a_EMPSIZE_mean, conf.level=0.95) # --> Almost no differences between grousp

# Run ANOVA
a_EMPSIZE_median <- aov(DiffMedianHourlyPercent ~ EmployerSize, gpg_core)
# Run post-hoc tests
TukeyHSD(a_EMPSIZE_median, conf.level=0.95) # --> Almost no differences between grousp

## SIC divions

# Abbreviate division names
gpg_core$div_shortened <- abbreviate(gpg_core$division)

# Run ANOVA
a_DIV_mean <- aov(DiffMeanHourlyPercent ~ div_shortened, gpg_core)
# Run post-hoc tests
TukeyHSD(a_DIV_mean, conf.level=0.95) # --> there are differences between groups

# Run ANOVA
a_DIV_median <- aov(DiffMedianHourlyPercent ~ div_shortened, gpg_core)
# Run post-hoc tests
TukeyHSD(a_DIV_median, conf.level=0.95) # --> there are differences between groups. Less pronounced than mean because of smaller variance.

## For sic division
order <- gpg_core %>%
  group_by(division) %>%
  summarize(iqr_med = IQR(DiffMedianHourlyPercent),
            iqr_mean = IQR(DiffMeanHourlyPercent)) %>%
  arrange(desc(iqr_med)) %>%
  mutate(order_med = 1:n()) %>%
  arrange(desc(iqr_mean)) %>%
  mutate(order_mean = 1:n()) 

## Plot median hourly pay
ggplot(gpg_core %>% left_join(order, by="division"), 
       aes(x=reorder(tolower(abbreviate(as.factor(division), 20)), order_med), 
           y=DiffMedianHourlyPercent)) +
  geom_jitter(aes(color=gpg_core$division)) +
  geom_boxplot(width=0.2,
               outlier.shape = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size=8),
        legend.position = "None",
        axis.text.y = element_text(size=9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name = "Difference in Median \nHourly Pay") 

## Plot mean hourly pay

## Make temp dataset
tmp <- gpg_core %>% 
  left_join(order, by="division") %>%
  ## Remove excessive values (+1 & -1)
  filter(DiffMeanHourlyPercent <= 1 & DiffMeanHourlyPercent >= -1)

ggplot(tmp, 
       aes(x=reorder(tolower(abbreviate(as.factor(division), 20)), order_mean), 
           y=DiffMeanHourlyPercent)) +
  geom_jitter(aes(color=division)) +
  labs(caption=paste0("NB: ", nrow(gpg_core) - nrow(tmp), 
                      " observations have been removed due to excessive values (+-100% mean pay gap)"))+
  geom_boxplot(width=0.2,
               outlier.shape = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1, size=8),
        legend.position = "None",
        plot.caption = element_text(size=7),
        axis.text.y = element_text(size=9)) +
  scale_x_discrete(name="") +
  scale_y_continuous(name = "Difference in Mean \nHourly Pay") 

# proportional-to-size stratified sample for 'division'
sample_stratum <- stratify(gpg_core, "division", n=1000, seed=402)

# Surveydesign for stratified sample
stratdesign_prop <- svydesign(ids=sample_stratum$uuid, 
                              fpc=~fpc, 
                              strata = ~Stratum,
                              data = sample_stratum)

# Calculate the population value & design effect
stratdesign_mean <- svymean(~DiffMedianHourlyPercent, design=stratdesign_prop, deff=TRUE)

# For mean
stratdesign_med <- svymean(~DiffMeanHourlyPercent, design=stratdesign_prop, deff=TRUE)

# Save stats
spm <- round(as.numeric(stratdesign_mean), digits=4)
spse <- round(as.numeric(SE(stratdesign_mean)), digits=6)
spsemoe <- round(spse * 1.96, digits=6)
spdeff <- round(as.numeric(deff(stratdesign_mean)), digits=4)

# Save stats
spmm <- round(as.numeric(stratdesign_med), digits=4)
spsem <- round(as.numeric(SE(stratdesign_med)), digits=6)
spsemoem <- round(spsem * 1.96, digits=6)
spdeffm <- round(as.numeric(deff(stratdesign_med)), digits=4)


## MEDIAN

# Use neyman allocation to stratify optimally
sample_optim <- stratify(gpg_core, 
                         "division", 
                         n=1000, 
                         seed=402,
                         optim=TRUE, 
                         optimVar = "DiffMedianHourlyPercent")

length_stratum_one <- length(which(sample_optim$Stratum == 1))

# Surveydesign for stratified sample
sample_optim$Prob <- 1 / sample_optim$Prob
stratdesign_optim <- svydesign(ids=sample_optim$uuid, 
                               fpc=~fpc, 
                               strata = ~Stratum,
                               #weights = ~Prob,
                               data = sample_optim)

# Calculate the population value & design effect
stratdesign_optim_mean_medpg <- svymean(~DiffMedianHourlyPercent, design=stratdesign_optim, deff=TRUE)

# Save values
som <- round(as.numeric(stratdesign_optim_mean_medpg), digits=4)
sose <- round(as.numeric(SE(stratdesign_optim_mean_medpg)), digits=6)
sosemoe <- round(sose * 1.96, digits=6)
sodeff <- round(as.numeric(deff(stratdesign_optim_mean_medpg)), digits=4)

## MEAN

# Use neyman allocation to stratify optimally
sample_optim <- stratify(gpg_core, 
                         "division", 
                         n=1000, 
                         seed=402,
                         optim=TRUE, 
                         optimVar = "DiffMeanHourlyPercent")

length_stratum_one <- length(which(sample_optim$Stratum == 1))

# Surveydesign for stratified sample
sample_optim$Prob <- 1 / sample_optim$Prob
stratdesign_optim <- svydesign(ids=sample_optim$uuid, 
                               fpc=~fpc, 
                               strata = ~Stratum,
                               #weights = ~Prob,
                               data = sample_optim)

# Calculate the population value & design effect
stratdesign_optim_mean_meanpg <- svymean(~DiffMeanHourlyPercent, design=stratdesign_optim, deff=TRUE)

# Save values
som_mpg <- round(as.numeric(stratdesign_optim_mean_meanpg), digits=4)
sose_mpg <- round(as.numeric(SE(stratdesign_optim_mean_meanpg)), digits=6)
sosemoe_mpg <- round(sose_mpg * 1.96, digits=6)
sodeff_mpg <- round(as.numeric(deff(stratdesign_optim_mean_meanpg)), digits=4)

## Make data frame
comparedesign <- data.frame(
  "Metric" = c(rep("Median", 3), rep("Mean", 3)),
  "Design" = c("SRS", "PPS", "Neyman", "SRS", "PPS", "Neyman"),
  "Estimate" = c(as.numeric(srs_mean[2]), spm,som, as.numeric(srs_mean[1]), spmm, som_mpg),
  "SE" = c(srs_se[2], spse, sose, srs_se[1] ,spsem,sose_mpg),
  "MOE" = c(marginerror[2], spsemoe, sosemoe, marginerror[1], spsemoem,sosemoe_mpg),
  "Deff" = c(1, spdeff, sodeff,1, spdeffm, sodeff_mpg)
)
knitr::kable(comparedesign,caption="Comparison of sampling designs.",
             col.names = c("PG Metric", "Design", "Estimate", "SE", "MOE", "Deff"))

cv_prop <- cv(svymean(~DiffMeanHourlyPercent, design=stratdesign_prop, deff=TRUE))
# CV = 0.0274

# neyman allocation to stratify optimally
## this approach will occur problem of "not enough members in group", so I use the division as stratification
cv_optim <- cv(svymean(~DiffMeanHourlyPercent, design=stratdesign_optim, deff=TRUE))
# CV = 0.0262

## Calculate statistics for manual sample size and CV of 0.01

# PPS
CV <- 0.01
N <- nrow(gpg_core)
n <- nrow(sample_stratum)
# mean and SE
svym_strat_prop <- svymean(~DiffMeanHourlyPercent, design=stratdesign_prop)
Ybar = as.numeric(svym_strat_prop)
se_prop <- as.numeric(SE(svym_strat_prop))
# Var
S2 = (n*se_prop^2)/(1-n/N)
nprime = (S2/Ybar^2)/(CV^2)
n_pps = nprime/((nprime/N)+1)
# n > 3797

## We need to rerun the optimal sample because we stratified w.r.t. the median income earlier, not the mean
sample_optim <- stratify(gpg_core, 
                         "division", 
                         n=1000, 
                         seed=402,
                         optim=TRUE, 
                         optimVar = "DiffMeanHourlyPercent")

# Surveydesign for stratified sample
stratdesign_optim <- svydesign(ids=sample_optim$uuid, 
                               fpc=~fpc, 
                               strata = ~Stratum,
                               data = sample_optim)

# Calculate the population value & design effect
stratdesign_optim_mean <- svymean(~DiffMedianHourlyPercent, design=stratdesign_optim, deff=TRUE)

# Calculate sample size
n <- nrow(sample_optim)
svym_strat_opt <- svymean(~DiffMeanHourlyPercent, design=stratdesign_optim)
Ybar = as.numeric(svym_strat_opt)
se_opt = as.numeric(SE(svym_strat_opt))
S2 = (n*se_opt^2)/(1-n/N)
nprime = (S2/Ybar^2)/(CV^2)
n_opt = nprime/((nprime/N)+1)
# n > 3643

## This code block is heavy and will NOT be evaluated a compilation time. We ran the algo in advance and stored the results
res <- vector("list", 10)
for(i in seq_along(1:10)) {
  res[[i]] <- required_sample_size(gpg_core, "division", 1000, nrow(gpg_core), 50)
}

## Save result
saveRDS(res, "data/calc_sample_size_data.rds")

## Load the data from the result above
res <- readRDS("data/calc_sample_size_data.rds")

## Get bounds
ssizes <- unlist(lapply(res, function(x) x$final_n))
min_ssize <- min(ssizes)
max_ssize <- max(ssizes)
avg_ssize <- mean(ssizes)

## Retrieve data for each run
d <- lapply(1:10, function(x) {
  tmp <- res[[x]]$data
  tmp$iteration <- x
  tmp
})
## Bind data
db <- do.call(rbind.data.frame, d)

## Plot
ggplot(db, aes(x=n, y=CV, color = as.factor(iteration))) +
  geom_line(size = 0.9, alpha=0.7) +
  theme_bw() +
  scale_color_brewer(palette = "Set3") +
  scale_x_continuous(name = "Sample size") +
  scale_y_continuous(name = "Coefficient of Variation") +
  theme(legend.position = "None") +
  labs(caption = "NB. each line is one run of the algorithm. The total number of runs is 10.")

# We use the middle number of each group as weight. Except the largest one (20,000 or more) as 20,000 and smallest one as 0
Weight <- c(0, 250/2, (250+499)/2, (500+999)/2, (1000+4999)/2, (5000+19999)/2, 20000)
clustermean <- tapply(gpg_core$DiffMeanHourlyPercent,gpg_core$EmployerSize,mean)
wm_mean <- weighted.mean(clustermean,Weight)
# From individual perspective, the pay gap for mean is 0.1560

clustermedian <-tapply(gpg_core$DiffMedianHourlyPercent,gpg_core$EmployerSize,mean)
wm_med <- weighted.mean(clustermedian,Weight)
# The pay gap for median is 0.1040

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
                                     avg_med_diff = round(mean(DiffMedianHourlyPercent), digits=2))) %>%
               mutate(division = abbreviate(division, 30)) %>%
               select(-order),
             col.names = c("SIC division", "Perc. Male",  "#Companies", "Perc. Companies", "Avg. Mean PG", "Avg. Median PG"),
             caption = "Gender distribution and mean and median gender gaps by SIC division") 