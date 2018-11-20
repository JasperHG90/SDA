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

# Analysis -----

library(dplyr)

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