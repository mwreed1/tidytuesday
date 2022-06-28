library(tidyverse)
library(janitor)
library(scales)
library(colorblindr)
library(ggthemes)
library(hrbrthemes)

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
  mutate(pride = ifelse(pride, "Sponsors Pride", "Doesn't sponsor Pride")) %>%
  ggplot(aes(
    x = amount_contributed_across_states, 
    y = fct_reorder(company, amount_contributed_across_states)
    )) +
  geom_col(aes(fill = pride)) +
  scale_x_continuous(
    labels = label_dollar(
      prefix = "$ "
      #scale = 0.001,
      #suffix = "k"
      ),
    limits = c(0, NA)
    ) +
  scale_fill_OkabeIto() +
  labs(
    x = "Amount donated to anti-LGBTQ+ politicians across six different states",
    y = NULL,
    fill = NULL,
    title = "How much 'Pride' do these major companies have?",
    caption = "Source: Data for Progress via tidytuesday"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.key.height = unit(2, 'pt'),
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(margin = margin(r = -25, unit = "pt"))
  )

  