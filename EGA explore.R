library(tidyverse)
library(ggplot2)

park_visits <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-09-17/national_parks.csv")
state_pop <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-09-17/state_pop.csv")
gas_price <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-09-17/gas_price.csv")

park_visits <- park_visits %>% 
  filter(unit_type == "National Park")

park_visits <- park_visits %>%
  mutate(
    year_cat = case_when(
      year <= 1950 ~ "1904–1950",
      year <= 1980 ~ "1951–1980",
      TRUE         ~ "1981–2018"
    ),
    year_cat = factor(year_cat)
  )

park_visits %>% 
  ggplot(aes(x=parkname, y = visitors, color = year_cat))+
  geom_point()+
  facet_wrap(~region)


region_summary <- park_visits %>% 
  group_by(region, year_cat) %>% 
  summarise(total_visitors = sum(visitors, na.rm = TRUE), .groups = "drop")

region_summary %>% 
  ggplot(aes(x = year_cat, y = total_visitors, fill = region, group = region)) +
  geom_col(position = "dodge") +
 # scale_y_continuous(labels = scales::label_number_si()) +
  labs(
    x = "Time period",
    y = "Total visitors",
    fill = "Region",
    title = "Total National Park Visitation by Region and Era"
  ) +
  theme_minimal()

park_summary <- park_visits %>% 
  group_by(region, parkname, year_cat) %>% 
  summarise(
    mean_visitors = mean(visitors, na.rm = TRUE),
    .groups = "drop"
  )


park_summary %>% 
  ggplot(aes(x = year_cat,
             y = fct_reorder(parkname, mean_visitors),
             fill = mean_visitors)) +
  geom_tile() +
#  scale_fill_continuous(labels = scales::label_number_si()) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    x = "Time period",
    y = "National park",
    fill = "Mean visitors",
    title = "Changes in National Park Visitation Across Eras",
    subtitle = "Mean annual visitors by park, grouped into three time periods"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),
    strip.text = element_text(face = "bold")
  )


park_summary %>% 
  ggplot(aes(x = fct_reorder(parkname, mean_visitors),
             y = mean_visitors,
             color = year_cat)) +
  geom_point(alpha = 0.7, position = position_dodge(width = 0.4)) +
  coord_flip() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    x = "National park",
    y = "Mean annual visitors",
    color = "Time period",
    title = "Mean Visitors to U.S. National Parks by Region and Era"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))
park_summary %>% 
  ggplot(aes(
    x = mean_visitors,
    y = fct_reorder(parkname, mean_visitors),
    color = year_cat
  )) +
  geom_point(size = 2, alpha = 0.7) +
  facet_wrap(~ region, scales = "free_y", ncol = 3) +
  labs(
    title = "Mean Visitors to U.S. National Parks by Region and Era",
    x = "Mean annual visitors",
    y = "National park",
    color = "Time period"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

ggsave("park_plot.png", width = 16, height = 12, dpi = 300)


