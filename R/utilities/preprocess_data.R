## This script pre-processes & joins different data to form final dataset
## Jasper Ginn
## SDA project
## 
## ----------------------------------------------------------------------------
##
## Instructions:
##  Do not run this file directly. Rather, call it from your main script using
##
##     source("practical/preprocess_data.R")
##
## ----------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)

# Load data
gpg <- readRDS("data/gender_pay_gap.Rds")
sic <- readRDS("data/sicCodes.Rds")

# Expand sic codes into multiple columns
# sic codes are comma-delimited. We split each observation into m separate vectors.

# Find the observation with the maximum number of sic codes
max_sc <- map_int(gpg$SicCodes, function(x) str_split(x, ",")[[1]] %>%
                                              length() ) %>%
  max()

gpg <- gpg %>% # Use the forward-operating pipe to chain together commands
  separate(SicCodes, c("SC1", "SC2", "SC3"))

# Use regular expressions to retrieve postal code