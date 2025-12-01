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

#filtering 
im_parks <- park_visits %>% 
  filter(region == "IM") %>% 
  filter(year>1999) %>% 
  filter(year != "Total")

im_parks %>% 
  ggplot(aes(x=year, y= visitors, color = unit_name))+
  geom_point()+
  labs()+
  theme_minimal()



