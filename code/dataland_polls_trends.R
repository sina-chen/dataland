# trend_long <- trend %>% 
#   pivot_longer(cols = -date, names_to = "candidate", values_to = "trend") %>% 
#   mutate(date = as.Date(date))
# polls_long <- polls %>% 
#   pivot_longer(cols = -c(date, pollster, sample, overseas, alternate),
#                names_to = "candidate", values_to = "support") %>% 
#   mutate(date = as.Date(date))
# 
# ggplot() +
#   geom_point(data = polls_long, aes(x = rev(date), y = support, color = candidate)) +
#   geom_line(data = trend_long, aes(x = rev(date), y = trend, color = candidate)) +
#   theme_bw() 



# Libraries ---------------------------------------------------------------

library(rvest)
library(tidyverse)
library(zoo)

source("code/dataland_helper_functions.R")

#-------------------------------------------------------------------------------

polls_update()

trend_update()


polls <- read.csv("data/polls.csv")


trend <- read.csv("data/trend.csv")

