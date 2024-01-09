## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-17
##

library(tidyverse)
theme_set(theme_minimal())
## netflix graph

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv') %>% 
  janitor::clean_names()
tv_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv') %>% 
  janitor::clean_names()

## question - how can this visualization be improved? What is confusing about this? Is it inherently bad?
tv_ratings_graph <- tv_ratings %>% 
  group_by(title_id) %>% 
  mutate(max_season = max(season_number),
         min_season = min(season_number)) %>% 
  filter(season_number == max_season | season_number == min_season) %>% 
  add_count() %>% 
  filter(n > 1) %>% 
  mutate(max_season_rating = ifelse(max_season == season_number, av_rating, NA),
         min_season_rating = ifelse(min_season == season_number, av_rating, NA)) %>% 
  tidyr::fill(max_season_rating, .direction = "up") %>% 
  fill(min_season_rating) %>% 
  distinct(title_id, title, max_season, max_season_rating, min_season_rating) %>% 
  arrange(desc(max_season)) %>% 
  head(20) %>% 
  mutate(title = glue::glue("{title} ({max_season})")) %>% 
  mutate(start_better = ifelse(min_season_rating > max_season_rating, "Beginning Season Better", "Later Season Better")) %>% 
  ggplot(aes(color = start_better)) +
  geom_segment(aes(x = title, xend = title, y = min_season_rating, yend = max_season_rating)) +
  geom_point( aes(x=title, y=min_season_rating), color=rgb(0.2,0.7,0.1,0.5), size=3) +
  geom_point( aes(x=title, y=max_season_rating),  color=rgb(0.7,0.2,0.1,0.5), size=3) +
  labs(x = " ", y = "Season Rating", title = "Difference in First and Last Season Ratings", subtitle = "Green dots denote the first season rating. Red dots denote the last season rating.",
       color= " ") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggsci::scale_color_npg()
  
  
  
nurse_wage <- nurses %>% 
  group_by(year) %>% 
  summarize(avg_wage = mean(hourly_wage_avg,na.rm = T)) %>% 
  ggplot(aes(year, avg_wage)) +
  geom_point() +
  geom_line() +
  labs(x = " ", y = "Average Hourly Wage") +
  theme_minimal()

nurse_wage_correlation <- nurses %>% 
  ggplot(aes(hourly_wage_avg, total_employed_rn)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y~poly(x, 2)", se = F) +
  labs(x = "Average Hourly Wage", y = "Total Employed Registered Nurses")

nurse_wage_boxplot <- nurses %>% 
  filter(year >= 2000) %>% 
  ggplot(aes(factor(year), hourly_wage_avg)) +
  geom_boxplot() +
  labs(x = " ", y = "Hourly Wage Average in the US") +
  theme(axis.text.x = element_text(angle = 45))



netflix_releases <- netflix_titles %>% 
  mutate(date_added = lubridate::mdy(date_added)) %>% 
  mutate(year = lubridate::year(date_added)) %>% 
  group_by(year, type) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(type) %>% 
  mutate(max_count = max(n,na.rm = T)) %>% 
  mutate(max_year = ifelse(max_count == n, year, 0)) %>% 
  ggplot(aes(year, n, fill = max_year)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -1) +
  facet_wrap(~type) +
  labs(x = " ", y = "Number of Movies or TV Shows Released") +
  theme_minimal() +
  theme(legend.position = "none") 

