## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-20
##

library(tidyverse)
library(jsonlite)
library(lubridate)
library(hms)

## cleaning names 
streaming <- streaming %>% 
  janitor::clean_names()

streaming <- streaming %>% 
  mutate(end_time = ymd_hm(end_time)) %>% 
  mutate(hm = hms::as_hms(end_time)) %>% 
  mutate(end_date = as_date(end_time)) %>% 
  mutate(seconds_played = ms_played / 1000,
         minutes_played = seconds_played /60)

## most songs played?
streaming %>% 
  count(artist_name, sort = T)

## most minutes played?
streaming %>% 
  group_by(artist_name) %>% 
  summarize(total_minutes = sum(minutes_played)) %>% 
  arrange(desc(total_minutes))
  
## favorite time of day to stream? notice that this probably doesn't make any sense - look up on spotify https://support.spotify.com/us/article/understanding-my-data/
## Use Sys.timezone()
## with_tz
streaming %>% 
  mutate(hour = hour(hm), minute = minute(hm)) %>% 
  count(hour, sort = T)

streaming <- streaming %>% 
  mutate(end_time = with_tz(end_time, tz = "America/Los_Angeles"),
         hm = hms::as_hms(end_time),
         end_date = as_date(end_time))

## favorite time of hour to stream
streaming %>% 
  mutate(hour = hour(hm), minute = minute(hm)) %>% 
  count(hour, sort = T)

## might be subject to bias since only counting rows. Get rid of anything with less than 5 seconds
streaming %>% 
  mutate(hour = hour(hm), minute = minute(hm)) %>% 
  filter(seconds_played > 5) %>% 
  count(hour, sort = T)


## artist with most skips?
streaming %>% 
  mutate(skip = ifelse(seconds_played < 5, 1, 0)) %>% 
  filter(skip == 1) %>% 
  count(artist_name, sort = T)
