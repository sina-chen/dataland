
# 
# Scrape, clean dataland polls & compute trends
#
# Author: Sina Chen
# Source: https://cdn-dev.economistdatateam.com/jobs/pds/code-test/index.html
# 
#-------------------------------------------------------------------------------

# Libraries ---------------------------------------------------------------

library(rvest)
library(tidyverse)
library(zoo)


# Functions ---------------------------------------------------------------

source("code/dataland_helper_functions.R")



# Write/update polls and trends -------------------------------------------

dataland2024_polls_trends()

