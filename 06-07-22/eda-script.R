library(tidyverse)
library(janitor)
library(scales)
library(colorblindr)

tuesdata <- tidytuesdayR::tt_load(2022, week = 23)

donations <- tuesdata$pride_aggregates

corp_raw <- tuesdata$static_list

spons <- tuesdata$pride_sponsors

fortune_raw <- tuesdata$fortune_aggregates

data <- tuesdata$contribution_data_all_states


corp <- corp_raw %>%
  clean_names()

fortune <- fortune_raw %>%
  clean_names()

corp %>%
  arrange(desc(amount_contributed_across_states)) %>%
  slice(1:20) %>%
  filter(company != "Grand Total") %>%
  mutate(pride = ifelse(pride, "Yes", "No")) %>%
  ggplot(aes(
    x = amount_contributed_across_states, 
    y = fct_reorder(company, amount_contributed_across_states)
    )) +
  geom_col(aes(fill = pride)) +
  scale_x_continuous(labels = number_format(
    prefix = "$",
    scale = 0.001,
    suffix = "k"
    )) +
  # scale_fill_manual() +
  scale_fill_OkabeIto() +
  labs(
    x = "Amount donated to anti-LGBTQ+ politicians across six different states",
    y = NULL,
    fill = "Is the company a Pride parade sponser?",
    title = "How much 'Pride' do these major companies have?",
    caption = "Source: Data for Progress via tidytuesday"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
