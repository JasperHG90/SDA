## This script installs necessary libraries if needed and loads these libraries if installed
## Jasper Ginn
## SDA project
## 
## ----------------------------------------------------------------------------
##
## Instructions:
##  Do not run this file directly. Rather, call it from your main script using
##
##     source("practical/install_dependencies.R")
##
## ----------------------------------------------------------------------------

# Retrieve installed packages
ip <- installed.packages()

# Packages to be installed (just add to the vector if you have new dependencies)
dependencies <- c("dplyr", "tidyr", "purrr", "stringr", 
                  "rvest", "forcats", "ggplot2", "ggExtra", "knitr")

# For each dependency, install and / or load
for(dependency in dependencies) {
  
  ## Install package if not in library
  if( !dependency %in% ip[,1] ) {
    
    cat(paste0("\nInstalling ", dependency, "\n"))
    
    install.packages(dependency, character.only=TRUE)
    
  }
  
  ## Load package
  #library(dependency, character.only = TRUE)
  
}

# Clean environment
rm(ip, dependencies, dependency)
