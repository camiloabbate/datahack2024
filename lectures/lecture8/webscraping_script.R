## Purpose of script: scraping the ucsb studnets
##
## Author: Michael Topper
##
## Date Last Edited: 2022-05-08
##

library(tidyverse)
library(rvest)

x <- read_html("https://econ.ucsb.edu/people/students") %>% 
  html_elements(".views-row") %>% 
  html_elements(".group-second") %>% 
  html_text2() %>% 
  as_tibble() 

x %>% 
  separate(value, into = c("student_name", "other"), sep = "\\n",
           extra= "merge") %>% 
  extract(other, "office_number", "(\\d{4})") %>% View()
read_html("https://econ.ucsb.edu/people/students") %>% 
  html_elements(".views-row") %>% 
  html_elements(".group-third") %>% ## get rid of this line because it changes dimensions! 
  html_text2()