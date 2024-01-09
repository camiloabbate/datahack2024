
# homework 8 pdf scraping solutions ---------------------------------------

## Question 1
crime_log <- pdftools::pdf_text("2021_f_hw6_v1/prompt_rubric/1-20-16.pdf")

## Question 2
crime_log_text <- crime_log %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_trim() %>% 
  str_to_lower()


## Question 3
date_reported_indices <- crime_log_text %>% 
  str_detect("^date reported") %>% which

location_indicies <- crime_log_text %>% 
  str_detect("^general location") %>% which

date_occurred_from_indice <- crime_log_text %>% 
  str_detect("^date occurred from") %>% which

date_occurred_to_indice <- crime_log_text %>% 
  str_detect("^date occurred to") %>% which

incident_indices <- crime_log_text %>% 
  str_detect("^incident") %>% which

dispostion_indices <- crime_log_text %>% 
  str_detect("^disposition") %>% which

modified_indices <- crime_log_text %>% 
  str_detect("^modified") %>% which


## Question 4
disposition <- crime_log_text[dispostion_indices] %>% 
  as_tibble() %>% 
  extract(value, "disposition","disposition:\\s(.{1,})")

## Question 5
location <- crime_log_text[location_indicies] %>% 
  as_tibble() %>% 
  extract(value, "location", ".{1,}:(.{1,})") %>% 
  mutate(location = location %>% str_trim())


## Question 6
incident <- crime_log_text[incident_indices] %>% 
  as_tibble() %>% 
  extract(value, "incident", ".{1,}\\s{2,}(.{1,})")

## Question 7
modified <- crime_log_text[modified_indices] %>% 
  as_tibble() %>% 
  extract(value, c('modified_date', 'modified_time'), "(\\d\\d/\\d\\d/\\d\\d).{1,}(\\d\\d:\\d\\d)")

## Question 8
date_occurred_from <- crime_log_text[date_occurred_from_indice] %>% 
  as_tibble() %>% 
  extract(value, c('date_occurred_from', 'time_occurred_from'), "(\\d\\d/\\d\\d/\\d\\d).{1,}(\\d\\d:\\d\\d)")


## Question 9
date_occurred_to <- crime_log_text[date_occurred_to_indice] %>% 
  as_tibble() %>% 
  extract(value, c('date_occurred_to', 'time_occurred_to'), "(\\d\\d/\\d\\d/\\d\\d).{1,}(\\d\\d:\\d\\d)")

## Question 10
date_reported <- crime_log_text[date_reported_indices] %>% 
  as_tibble() %>% 
  extract(value, c("date_reported", "time_reported", "report_number"),   "(\\d{1,2}/\\d{1,2}/\\d{1,2}).{1,}(\\d\\d:\\d\\d).{1,}(\\d{6})")



## Question 11
final_crime_log <- bind_cols(date_reported, date_occurred_from, date_occurred_to,
                             incident, location, disposition, modified)


## Question 12
final_crime_log <- final_crime_log %>% 
  mutate(across(starts_with("date"), ~mdy(.)))



