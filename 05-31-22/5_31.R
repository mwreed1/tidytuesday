# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-05-31')

poll <- tuesdata$poll
rep <- tuesdata$reputation

# library(rvest)
library(tidyverse)
library(janitor)
# library(jsonlite)
# library(gt)

poll %>%
  clean_names() %>%
  group_by(industry) %>%
  summarize(avg_score = mean(x2022_rq)) %>%
  arrange(desc(avg_score))

best <- poll %>%
  clean_names() %>%
  group_by(industry) %>%
  summarize(avg_score = mean(x2022_rq)) %>%
  arrange(desc(avg_score)) %>%
  slice(1:3) %>%
  pull(industry)

worst <- poll %>%
  clean_names() %>%
  group_by(industry) %>%
  summarize(avg_score = mean(x2022_rq)) %>%
  arrange(avg_score) %>%
  filter(industry != "Other") %>%
  slice(1:3) %>%
  pull(industry)


poll %>%
  clean_names() %>%
  filter(industry %in% best) %>%
  ggplot(aes(x = year, y = rq)) +
  geom_line(aes(color = company)) +
  facet_grid(.~ industry, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")
