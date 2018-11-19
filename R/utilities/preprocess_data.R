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
library(rvest)
library(purrr)

## Helper functions ----

retrieve_postal_code <- function(data) {
  
  ## Match postal code
  ##  Pattern: Match 
  ##    -A-Z between 1, 2 times --> 
  ##    -0-9 between 1 and 2 times --> 
  ##    -A-Z between 0 and 2 times -->
  ##    -Match space -->
  ##    -0-9 between 1, 2 times -->
  ##    -A-Z between 1, 2 times
  pattern <- "([A-Z]{1,2}[0-9]{1,2}[A-Z]{0,2})\\s([0-9]{1,2}[A-Z]{1,2})"
  
  ## Match
  matched <- str_match(data, pattern)[,1]
  
  ## Return
  return(matched)
  
}

## Helper function to match counties against a list 
## Source: 
match_counties <- function(data) {
  
  ## Wikipedia url with GB counties
  wurl <- "https://en.wikipedia.org/wiki/List_of_counties_of_the_United_Kingdom"
  
  ## Load page
  p <- read_html(wurl) %>%
    # Extract all tables
    html_table(fill=TRUE) %>%
    # Places in brackets should be own observation
    map(., function(x) str_split(str_replace_all(x$County, "\\)", ""), "\\(")) %>%
    # Unlist the list
    unlist() %>%
    ## Remove all non-alphanumeric strings
    str_replace_all(., "[^[:alnum:][:space:]]", "")
  
}

# Load data
gpg <- readRDS("data/gender_pay_gap.Rds") %>%
  # Add unique id
  mutate(uuid = 1:n())
# Read sic code data
sic <- readRDS("data/sicCodes.Rds")

# Need the names of the quantiles for later
ss_names <- names(gpg)[11:18]

# Preprocess the data
#  1. Add columns with percentage men / women
#  2. Extract postal code / county from address
mfbreakdown <- gpg %>%
  # SUbset variables
  select(c("uuid", ss_names)) %>%
  # To long format
  gather(variable, value, -uuid) %>%
  # Make dummy variable for gender
  mutate(gender = ifelse(str_detect(tolower(variable), "female"), "female", "male")) %>%
  # Group by company id and male / female variable type
  group_by(uuid, gender) %>%
  # Calculate average 
  summarize(percentage = mean(value)) %>%
  # Spread the data
  spread(gender, percentage) %>%
  # Join with original data
  inner_join(gpg, "uuid") %>%
  # Extract postal code using regular expression
  mutate(postal_code = )

# Expand sic codes into multiple columns
# sic codes are comma-delimited. We split each observation into m separate vectors.

# Find the observation with the maximum number of sic codes
max_sc <- map_int(gpg$SicCodes, function(x) str_split(x, ",")[[1]] %>%
                                              length() ) %>%
  max()

gpg <- gpg %>% # Use the forward-operating pipe to chain together commands
  separate(SicCodes, c("SC1", "SC2", "SC3"))

# Use regular expressions to retrieve postal code