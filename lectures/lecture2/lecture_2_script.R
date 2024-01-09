## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-04-02
##

library(tidyverse)
library(lubridate)

amazon <- read_csv("lectures/lecture_2/amazon.csv") 

## clean names
## explain what the dot dot is and how to call. Remind of pipes.
amazon <- amazon %>% 
  janitor:: clean_names()

## let's count some things. What are my most popular categories?
## talk about the sort = T argument and reading documentation
amazon %>% 
  count(category, sort = T)

## we can connect pipes together with the "and then" thought process. 
## filtering with logicals 
amazon %>% 
  count(category, sort = T) %>% 
  filter( n > 5)
amazon %>% 
  count(category, sort = T) %>% 
  filter(n < 5)  %>% 
  filter(n == 1)


## check the item_subtotal category. What is it? it's a character with weird strings.
## this is probably a typical problem is there a solution to use it? use google! make to sure to type in tidyverse solution
## we can use the mutate() function to change columns on the fly without actually having to create a new column. 
## change to just an integer using the round function and look up the documentation
## look at the arguments for mutate, what is the benefit of having .before = 1?
amazon <- amazon %>% 
  mutate(item_subtotal = parse_number(item_subtotal)) %>% 
  mutate(item_subtotal_integer = round(item_subtotal, 0), .before = 1) %>% 
  relocate(item_subtotal)



## look at the data a little. what kind of data are the dates?  these are in characters! R actually has a better type of data called date
## we're going to use the lubridate package to work with this.

#install.packages("lubridate")

## when you load in the lubridate package, what do you see??
## masking is important! it means that some functions that you typically used are being overriden by the function in package.
## you can override this with the janitor::clean_names() double dot. 

amazon %>% 
  mutate(order_date = mdy(order_date)) 

## what other columns have to do with dates? we can actually use the selct() function
## let's talk about tidy selectors
amazon %>% 
  select(starts_with("order"))
amazon %>% 
  select(ends_with("date"))

## change the release_date column to a date not a date-time. google this to find an answer
amazon %>% 
  mutate(order_date = mdy(order_date),
         shipment_date = mdy(shipment_date),
         release_date = as_date(release_date)) %>% 
  select(ends_with("date"))

## create a new column that shows the difference in shipment date and the order date
amazon %>% 
  mutate(order_date = mdy(order_date),
         shipment_date = mdy(shipment_date),
         release_date = as_date(release_date)) %>% 
  select(ends_with("date"))  %>% 
  mutate(ship_lag = shipment_date - order_date)  %>% 
  group_by(category) %>% 
  summarize(avg_ship_lag = mean(ship_lag))

## can we get the average shipment delay by category?
amazon %>% 
  mutate(order_date = mdy(order_date),
         shipment_date = mdy(shipment_date),
         release_date = as_date(release_date)) %>% 
  mutate(ship_lag = shipment_date - order_date)  %>% 
  group_by(category) %>% 
  summarize(avg_ship_lag = mean(ship_lag)) %>% 
  arrange(desc(avg_ship_lag)) 


## in class activity:
## how man different people use my account?
## how many different places have I lived?
## what year did i spend the most money?
## can you find what years I lived in San Diego and which dates I lived in Santa Barbara? - hint may need to get a "year" column

amazon %>% 
  mutate(order_date = mdy(order_date)) %>% 
  mutate(year = year(order_date), .before = 1) %>% 
  filter(shipping_address_name == "Michael Topper")  %>% 
  group_by(year) %>% 
  count(shipping_address_city) %>% View()
  
