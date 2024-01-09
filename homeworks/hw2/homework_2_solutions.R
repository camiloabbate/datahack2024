
## 
##
## Coder: Camilo Abbate
##
## Date Last Edited: 2023-12-12
##

# Clean environment:

rm(list = ls())

library(tidyverse)
library(janitor)

#load dataset:

food <- read_delim("homeworks/hw2/fruits_and_vegetables_4.csv")

## cleaning column names 
food <- clean_names(food)

#checking the column names:
colnames(food)
  
# types of fruits and vegetables (productnames)
unique(food$productname)

#How many times each product appears in the dataset?
count(food, productname)

#arrange in descending order:

count(food, productname) %>% arrange(desc(n))

#save int a dataframe:

how_many_times <- count(food, productname) %>% arrange(desc(n))


# Let's use group_by and calculate some descriptive statistics for the farm_price:

food_mean_price <- food %>% group_by(productname) %>% 
  summarize(mean_price = mean(farmprice_usd, na.rm = T))


food_stats <- food %>% group_by(productname) %>% 
  summarize(mean_price = mean(farmprice_usd, na.rm = T),
            median_price = median(farmprice_usd, na.rm = T),
            max_price = max(farmprice_usd, na.rm = T),
            min_price = min(farmprice_usd, na.rm = T),
            sd_price = sd(farmprice_usd, na.rm = T))

# Most volatile price:

food_stats %>% arrange(desc(sd_price)) %>% head(10)

# Let's focus on Avocados:

avocados <- food %>% filter(productname == "Avocados")

#what's the price spread in NY:
avocados <- avocados %>% mutate(newyorkspread = newyork_retail - farmprice_usd)

#in percentages?
avocados <- avocados %>% mutate(newyorkspread_pct = (newyorkspread/farmprice_usd)*100)

#is it above or below the average spread? Create a dummy variable:
#above  == 1, equal or below == 0

avocados <- avocados %>% mutate(more_expensive_than_avg = ifelse(newyorkspread_pct > averagespread, 1, 0))

# There are 711 dates. How many times was the price of avocado in NY above the average spread?

sum(avocados$more_expensive_than_avg)

#let's focus on oranges now:

oranges <- food %>% filter(productname == "Oranges")

# Let's find the avg price per year. For that we need lubridate:
oranges$date

oranges$date <- mdy(oranges$date)

oranges <- oranges %>% mutate(year = year(date))

oranges %>% group_by(year) %>% summarize(avg_price = mean(farmprice_usd, na.rm = T))

# But what if we want the avg price per year per city?

oranges %>% group_by(year) %>% summarize(avg_price_farm = mean(farmprice_usd, na.rm = T),
                                         avg_price_ny = mean(newyork_retail, na.rm = T),
                                         avg_price_la = mean(losangeles_retail, na.rm = T),
                                         avg_price_ch = mean(chicago_retail, na.rm = T),
                                         avg_price_at = mean(atlanta_retail, na.rm = T))


# Back to the food dataframe:

# Pivot:

food %>% pivot_longer(cols = c(newyork_retail, losangeles_retail, chicago_retail, atlanta_retail), 
                      names_to = "city", values_to = "retail_price")





# Let's start working with mutate
# Create a new column: newyorkspread_:






food %>% filter(year == 2006) %>% 
  select(entity,avocados_production_tonnes) %>% 
  arrange(desc(avocados_production_tonnes)) %>% 
  head(10)


food <- food %>% mutate()


mexico <- food %>% filter(entity == "Mexico")








#see each year in the x axis:

ggplot(mexico, aes(x = year, y = avocados_production_tonnes)) +
  geom_line() +
  labs(title = "Mexico's avocado production",
       x = "Year",
       y = "Avocados production (tonnes)") +
  scale_x_continuous(breaks = seq(1960, 2021, 3))


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
