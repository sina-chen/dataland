
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


#-------------------------------------------------------------------------------

# Write/update polls and trends -------------------------------------------

# polls
polls_update()

# trends
trends_update()



# Inspect results ---------------------------------------------------------

# read data
polls <- read.csv("data/polls.csv")
trends <- read.csv("data/trends.csv")

# reshape to long for visual inspection
trends_long <- trends %>%
  pivot_longer(cols = -date, names_to = "candidate", values_to = "trends") %>%
  mutate(date = as.Date(date),
         candidate = str_to_title(candidate))
polls_long <- polls %>%
  pivot_longer(cols = -c(date, pollster, n, overseas, alternate),
               names_to = "candidate", values_to = "support") %>%
  mutate(date = as.Date(date),
         candidate = str_to_title(candidate))

polls_trends_plot <- ggplot() +
  geom_point(data = polls_long, aes(x = rev(date), y = support, 
                                    color = candidate)) +
  geom_line(data = trends_long, aes(x = rev(date), y = trends, 
                                    color = candidate)) +
  theme_bw() +
  labs(x = "Date", y = "Support", color = "Candidate", 
       caption = "The points display observed poll results, while the lines illustrate the trends of the 7-day average.") +
  theme(text = element_text(size = 16))

ggsave(filename = "plots/polls_trends_plot.png", 
       plot = polls_trends_plot, 
       width = 12, height = 6, 
       bg='#ffffff')





