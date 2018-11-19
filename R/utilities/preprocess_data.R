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

#' Define a helper to extract postal codes from company address
#' 
#' @param data string. Vector containing company addresses
#' @return if postal code is found, function returns the postal code. If not found, returns NA
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

#' Helper function that matches company addresses to a list of UK counties
#' 
#' @param data string. Vector containing company addresses
#' @return if county is found, function will return county name. Else, it returns NA
#' @seealso: https://en.wikipedia.org/wiki/List_of_counties_of_the_United_Kingdom
match_counties <- function(data) {
  
  ## Wikipedia url with GB counties
  wurl <- "https://en.wikipedia.org/wiki/List_of_counties_of_the_United_Kingdom"
  
  ## Load page
  p <- read_html(wurl) %>%
    # Extract all tables
    html_table(fill=TRUE) %>%
    # Places in brackets should be own observation
    map(., function(x) str_replace_all(x$County, "\\)", "") %>%
                       # Split if string contains open bracket, split at this bracket
                          str_split("\\(")) %>%
    # Unlist the list (turn into vector)
    unlist() %>%
    ## Remove all non-alphanumeric strings
    str_replace_all(., "[^[:alnum:][:space:]]", "") %>%
    ## No caps
    tolower() %>%
    ## Trim whitespace at the beginning and end of each string
    trimws()
  
  ## For each address, try to match a province
  matches <- map(tolower(data), str_detect, p)
  
  ## For each element, look up province if found. Else return NA
  found <- map(matches, function(x) ifelse(all(x == FALSE), NA, p[x]))
  
  ## Return
  return(found)
  
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
#  3. Join with original dataset
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