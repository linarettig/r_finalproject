library(tidyverse)
library(ggplot2)

park_visits <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-09-17/national_parks.csv")
state_pop <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-09-17/state_pop.csv")
gas_price <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-09-17/gas_price.csv")

#explore <-read_csv("data/All National Park Visitation 1904-2016.csv")

park_visits <- park_visits %>% 
  filter(unit_type == "National Park")

park_visits <- park_visits %>%
  mutate(
    year_cat = case_when(
      year <= 1950 ~ "1904–1950",
      year <= 1980 ~ "1951–1980",
      TRUE         ~ "1981–2016"
    ),
    year_cat = factor(year_cat)
  )


region_summary <- park_visits %>% 
  group_by(region, year_cat, unit_name) %>% 
  summarise(total_visitors = sum(visitors, na.rm = TRUE), .groups = "drop")

#park_visits %>% 
#  table(parkname)


#figures

park_summary %>% 
  ggplot(aes(
    x = mean_visitors,
    y = fct_reorder(unit_name, mean_visitors),
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



#filtering 
im_parks <- park_visits %>% 
  filter(region == "IM") %>% 
  filter(year>1999) %>% 
  filter(year != "Total")%>% 
  filter(unit_code =="YELL")


im_parks %>% 
  ggplot(aes(x=year, y= visitors))+
  geom_point()+
  labs()
  theme_minimal()
