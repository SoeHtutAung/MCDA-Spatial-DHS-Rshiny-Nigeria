###########################
# Title: Setting up environment
# Purpose: Install and load required libraries
###########################

# --- install and load packages ---
# # create list of required packages
required_packages <- c(
  "tidyverse", "stringr",
  "sf", "rmapshaper", "raster", "terra", "exactextractr", "spatstat", # to process spatial files
  "mapview", "leaflet", "leafem", "leaflet.extras2", # to customize maps
  "xlsx", "openxlsx", # to process excel files
  "pacman" # more efficient handling of libraries
)

# # install packages 
# check for missing packages and install them
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# # load packages
invisible(lapply(required_packages, library, character.only = TRUE))

# --- install PATHtools from GitHub ---
# install and load necessary packages
pacman::p_load(
  "rgdal", # ***** noted that this package no longer exist and not compatible with R4.4.*, but this needs to be installed due to PATHtools 
  "devtools" # to install package from GitHub
  )
# install and load PATHtools
devtools::install_github("PATH-Global-Health/PATHtools", force = TRUE)
library(PATHtools) # load PATHtools

# --- create functions to ignore NA values at terra package ---
sum_no_na <- function(x) sum(x, na.rm = TRUE)  
mean_no_na <- function(x) mean(x, na.rm = TRUE)
