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

# If data already downloaded, load and move on
if(all(c("gpg_core.rds", "gpg_meta.rds") %in% list.files("data"))) {
  
  gpg_core <- readRDS("data/gpg_core.rds")
  gpg_meta <- readRDS("data/gpg_meta.rds")
  
} else {
  
  # Load libraries
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(forcats)
  
  cat("\nPre-processed data not found. Running one-time pre-processing steps ...\n")
  t1 <- Sys.time()
  
  # Check if county data found. Else: download
  if(!"counties.rds" %in% list.files("data")) {
    
    # Load rvest
    library(rvest)
    
    # Download county data
    cat("\nCounty data not found. Downloading now ...\n")
    
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
    
    # To file
    saveRDS(p, "data/counties.rds")
    
  } else{
    
    p <- readRDS("data/counties.rds")
    
  }
  
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
    
    ## For each address, try to match a province
    matches <- map(tolower(data), str_detect, p)
    
    ## For each element, look up province if found. Else return NA
    found <- map(matches, function(x) ifelse(all(x == FALSE), NA, p[x])) %>%
      unlist()
    
    ## Return
    return(found)
    
  }
  
  # Load data
  gpg <- readRDS("data/gender_pay_gap.Rds") %>%
    # Add unique id
    mutate(uuid = 1:n())
  
  # Read sic code data
  sic <- readRDS("data/sicCodes.Rds") %>%
    mutate(SECTION = as.character(SECTION),
           DIVISION = as.character(DIVISION),
           SicDivision = as.character(SicDivision),
           CLASS = as.character(CLASS))
  
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
    # Ungroup
    ungroup() %>%
    # Spread the data
    spread(gender, percentage) %>%
    # Join with original data --> use unique id as join variable
    inner_join(gpg, "uuid") %>%
    # Extract postal code using regular expression
    # Match counties against list of counties downloaded from wikipedia
    mutate(postal_code = retrieve_postal_code(Address),
           county = match_counties(Address))
  
  ## Split the dataset into two separate datasets
  
  # Indices of column names for metadata
  meta_ind <- c(1, 4, 5, 6, 22, 23, 25)
  # Complement of these indices
  core_ind <- c(1, setdiff(1:ncol(mfbreakdown), meta_ind))
  
  # create dataset with metadata 
  gpg_meta <- mfbreakdown %>%
    select(meta_ind)
  
  # Create dataset with core data
  gpg_core <- mfbreakdown %>%
    select(core_ind) %>%
    # Rearrange data 
    select(1, 4, 20:25, 19, 2:3, 5:18)
  
  # Percentages should be decimals, not whole numbers
  pcts <- names(gpg_core)[10:25]
  for(col in pcts) {
    
    gpg_core[,col] <- gpg_core[,col] / 100
    
  }
  
  # Merge with sic cores & set proper data types
  gpg_core <- gpg_core %>%
    left_join(., sic, by="SicDivision") %>%
    mutate(county = as_factor(county),
           section = as_factor(SECTION),
           division = as_factor(DIVISION),
           class = as_factor(CLASS)) %>%
    # Drop capital-case columns
    select(-SECTION, -DIVISION, -CLASS)
  
  # Write data
  saveRDS(gpg_core, "data/gpg_core.rds")
  saveRDS(gpg_meta, "data/gpg_meta.rds")
  
  cat(paste0("\nDone in ", round(as.numeric(Sys.time() - t1)), " seconds\n"))
  
  # Remove objects that are no longer needed
  keep <- c("gpg_core", "gpg_meta")
  rm(list=setdiff(ls(), keep))
  
}

