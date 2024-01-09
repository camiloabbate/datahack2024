library(tidyverse)
library(lubridate)
library(pdftools)

x <- c("Hello! My name is Michael Topper. My phone number is (805) 0193-0223",
       "MichaelTopperMichaelTopperMichaelTopper")

str_view_all(string = x, pattern = "Michael Topper")
str_view_all(string = x, pattern = "\\d")
## match on exactly two digits in a row:
str_view_all(string = x, pattern = "\\d{2}")
str_view_all(string = x, pattern = "\\d{4,5}") ## match on 4 to five of any number

numbers <- c("12345", "123", "123456789")
str_view_all(string =numbers, pattern = "\\d{5,}")

letters <- c("aaabbbbccc")
str_view_all(string = letters, pattern = "a{2}")

example <- c("1", "a", "A")

str_view_all(string = example, pattern = "[a-z]") ## matches on any lowercase
str_view_all(string = example, pattern = "[A-Z]") ## matches on any uppercase
str_view_all(string = example, pattern = ".") ## match on all of these 

string_1 <- "Hello my name is MIchael and my phone number is (805) 914-4285, and my address is: 2509"

## see if you can match on the phone number 

str_view_all(string_1, pattern = "\\s")## matches on whitespace


str_view_all(string_1, pattern = ".\\d\\d\\d.\\s\\d\\d\\d-\\d\\d\\d\\d") ## this works
##something better and more concise:
str_view_all(string_1, pattern = "\\(\\d{3}\\)\\s\\d{3}-\\d{4}")

last_vector <- c("interesting interesting")
## match on the first interesting
str_view_all(last_vector, pattern = "^interesting")
## match on the last interesting
str_view_all(last_vector, pattern = "interesting$")

# read in this code
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')


## two new functions: extract() and separate()
## exact() is going to extract out a pattern we want, into a new column
survey %>% 
  extract(col = timestamp, into = "date", regex = "(\\d{3})", remove = F) %>% 
  relocate(date)


## multiple columns at once using regular expressions
## let's say i want the date and time column by itself. I'm going to use extract

survey %>% 
  extract(col = timestamp, into = c("date", "time"), regex = "(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d\\d:\\d\\d)")

## match on first number and last number of how old are you, and create columns for each.
survey %>% 
  extract(col = how_old_are_you, into = c("first_number", "last_number"), regex = "(\\d).{1,}(\\d)") %>% 
  View()


## separate()
survey %>% 
  separate(col = how_old_are_you, into = c("lower_age", "upper_age"), sep = "-") 


crime_log <- pdf_text("lectures/lecture_4/crime_log.pdf")
crime_log

## first step in basically all pdfs
crime_log <- crime_log %>% 
  str_split(pattern = "\n") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_to_lower()

## extracting the incident from PDF
incident_indexes <- crime_log %>% 
  str_detect(pattern = "^incident") %>% 
  which()

crime_log[incident_indexes] %>% 
  as_tibble() %>% 
  extract(col = value, into = "incident", regex = ".{1,}:\\s(.{1,})\\s{5,}") %>% 
  mutate(incident = str_trim(incident))

crime_log %>% 
  str_detect("^disposition") %>% 
  which()



