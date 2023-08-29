# Dataland
This repository contains code and results related to the scraping and cleaning of polling data for the 2024 election in Dataland. It also includes code for calculating trends for each candidate based on a 7-day average of polling results.
To run the code, the "tidyverse", "rvest", and "zoo" package are required. The code was implemented with R version 4.2.2.

## Description of files

- The [code](https://github.com/sina-chen/dataland/edit/main/code/) folder contains all code

  - The [dataland_polls_trends.R](https://github.com/sina-chen/dataland/blob/main/code/dataland_polls_trends.R) apllies all functions to scrap, clean, and write/update polling data and generate/update polling trends.
  - The [dataland_helper_functions.R](https://github.com/sina-chen/dataland/blob/main/code/dataland_helper_functions.R) contains all functions for scraping, cleaning, updating, and generating polling and trend data.

- The [data](https://github.com/sina-chen/dataland/edit/main/data/) folder contains the generated data as well as previous versions of the data (indicated by "_old"), when the data is updataed.
- The [plots](https://github.com/sina-chen/dataland/edit/main/plots/) folder contains a plot visulising the scraped data and estimated trends to ensure that trends stay reasonable close the the polls. 
