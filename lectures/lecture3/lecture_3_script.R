library(jsonlite)
library(tidyverse)
library(lubridate)
spotify <- read_json("/Users/michaeltopper/Downloads/MyData 2/StreamingHistory0.json")

spotify <- tibble(spotify)
spotify <- spotify %>% 
  unnest_wider(spotify)

spotify <- spotify %>% 
  janitor::clean_names() 

spotify %>% 
  count(artist_name, sort = T) %>% 
  mutate(artist_name = fct_reorder(artist_name, n)) %>% 
  head(15) %>% 
  ggplot(aes(artist_name, n)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

spotify %>% 
  filter(artist_name == "Vampire Weekend") %>% 
  mutate(end_time = ymd_hm(end_time)) %>% 
  mutate(hm = hms::as_hms(end_time)) %>% 
  mutate(end_date = as_date(end_time)) %>% 
  mutate(seconds_played = ms_played / 1000,
         minutes_played = seconds_played /60) %>% 
