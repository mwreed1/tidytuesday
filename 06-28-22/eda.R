library(tidyverse)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load('2022-06-28')

paygap_raw <- tuesdata$paygap

paygap <- paygap_raw %>%
  mutate(
    employer_size = fct_relevel(
      employer_size,"20,000 or more", "5000 to 19,999", "1000 to 4999", 
                 "500 to 999", "250 to 499", "Less than 250", 
                 "Not Provided"
    ),
    year = year(date_submitted)
  )

paygap %>%
  group_by(employer_size) %>%
  summarize(
    mean_hourly = mean(diff_mean_hourly_percent),
    median_hourly = median(diff_median_hourly_percent),
    mean_bonus = mean(diff_mean_bonus_percent, na.rm = T),
    median_bonus = median(diff_median_bonus_percent, na.rm = T)
    )

paygap %>%
  count(year)

paygap %>%
  filter(employer_size == "20,000 or more", year == 2022) %>%
  group_by(employer_name) %>%
  slice(which.max(as.Date(date_submitted))) %>%
  ungroup() %>%
  mutate(ind = ifelse(diff_mean_hourly_percent > 0, TRUE, FALSE)) %>%
  ggplot(
    aes(
      x = fct_rev(fct_reorder(employer_name, diff_mean_hourly_percent)), 
      y = diff_mean_hourly_percent,
      fill = ind
    )
  ) +
  geom_col() +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  annotate("text", x = 14, y = 45, 
           label = "British Airways PLC has the largest paygap; \n men are payed 45.4% more than women on average") +
  annotate("text", x = 39, y = -11, 
           label = "Openreach Limited pays women 10.6% more than men on average") +
  annotate("point", x = 1, y = 45.4, size = 2) +
  annotate("point", x = 55, y = -10.6, size = 2) +
  labs(
    x = NULL,
    y = "Average percent difference in male and female hourly pay",
    title = "Wage discrepency in large* UK companies in 2022",
    subtitle = "*Companies with over 20,000 employees",
    caption = "Source: gender-pay-gap.service.gov.uk. via TidyTuesday" 
    ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  )


emp <- paygap %>%
  filter(employer_size == "20,000 or more", year == 2022) %>%
  group_by(employer_name) %>%
  slice(which.max(as.Date(date_submitted))) %>%
  ungroup() %>%
  arrange(desc(diff_mean_hourly_percent)) %>%
  select(employer_name, diff_mean_hourly_percent)
  