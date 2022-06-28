library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load('2022-06-14')

drought_raw <- tuesdata$drought
fips_raw <- tuesdata$`drought-fips`


drought <- drought_raw %>%
  pivot_longer(
    cols = matches("[A-Z]\\d"),
    names_to = "classification",
    values_to = "value"
    ) %>%
  # filter(value != 0) %>%
  clean_names() %>%
  select(date, state, classification, value, x0) %>%
  mutate(
    date = str_remove(date, "d_"),
    date = ymd(date),
    classification = case_when(
      classification == "D0" ~ "Abnormally dry",
      classification == "D1" ~ "Moderate drought",
      classification == "D2" ~ "Severe drought",
      classification == "D3" ~ "Extreme drought",
      classification == "D4" ~ "Exceptional drought",
      classification == "W0" ~ "Abnormally wet",
      classification == "W1" ~ "Moderate wet",
      classification == "W2" ~ "Severe wet",
      classification == "W3" ~ "Extreme wet",
      TRUE ~ "Exceptional wet"
      )
    )


drought %>%
  filter(state == "north-carolina", date > ymd(20000101)) %>%
  mutate(
    classification = ordered(
      classification, 
      levels = c("Exceptional wet",
                 "Extreme wet",
                 "Severe wet",
                 "Moderate wet",
                 "Abnormally wet",
                 "Abnormally dry",
                 "Moderate drought",
                 "Severe drought",
                 "Extreme drought",
                 "Exceptional drought")
      )
  ) %>%
  ggplot(
    aes(x = date, y = value)
  ) +
  geom_area(aes(fill = classification), position = "fill") +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Drought Severity and Coverage Index value",
    fill = "Drought classification",
    title = "Drought Rates in North Carolina",
    subtitle = "From 2000 - Present",
    caption = "Source: drought.gov via TidyTuesday"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
  )

