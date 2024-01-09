library(tidyverse)
library(rvest)

## get the companies on home page
companies <- read_html("https://brokers.interexo.com/search?country%5B0%5D=United%20States&state%5B0%5D=Alabama&state%5B1%5D=Alaska&state%5B2%5D=Arizona&state%5B3%5D=Arkansas&state%5B4%5D=California&state%5B5%5D=Colorado&state%5B6%5D=Connecticut&state%5B7%5D=Delaware&state%5B8%5D=District%20of%20Columbia&state%5B9%5D=Florida&state%5B10%5D=Georgia&state%5B11%5D=Hawaii&state%5B12%5D=Idaho&state%5B13%5D=Illinois&state%5B14%5D=Indiana&state%5B15%5D=Iowa&state%5B16%5D=Kansas&state%5B17%5D=Kentucky&state%5B18%5D=Louisiana&state%5B19%5D=Maine&state%5B20%5D=Maryland&state%5B21%5D=Massachusetts&state%5B22%5D=Michigan&state%5B23%5D=Minnesota&state%5B24%5D=Mississippi&state%5B25%5D=Missouri&state%5B26%5D=Montana&state%5B27%5D=Nebraska&state%5B28%5D=Nevada&state%5B29%5D=New%20Hampshire&state%5B30%5D=New%20Jersey&state%5B31%5D=New%20Mexico&state%5B32%5D=New%20York&state%5B33%5D=North%20Carolina&state%5B34%5D=Ohio&state%5B35%5D=Oklahoma&state%5B36%5D=Oregon&state%5B37%5D=Pennsylvania&state%5B38%5D=Rhode%20Island&state%5B39%5D=South%20Carolina&state%5B40%5D=South%20Dakota&state%5B41%5D=Tennessee&state%5B42%5D=Texas&state%5B43%5D=Utah&state%5B44%5D=Vermont&state%5B45%5D=Virginia&state%5B46%5D=Washington&state%5B47%5D=West%20Virginia&state%5B48%5D=Wisconsin&state%5B49%5D=Wyoming") %>% 
  html_elements(".grid-cell") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  filter(row_number()%%2 == 0) %>% 
  rename(companies = value)
## get the broker agents on the first page
agents <-  read_html("https://brokers.interexo.com/search?country%5B0%5D=United%20States&state%5B0%5D=Alabama&state%5B1%5D=Alaska&state%5B2%5D=Arizona&state%5B3%5D=Arkansas&state%5B4%5D=California&state%5B5%5D=Colorado&state%5B6%5D=Connecticut&state%5B7%5D=Delaware&state%5B8%5D=District%20of%20Columbia&state%5B9%5D=Florida&state%5B10%5D=Georgia&state%5B11%5D=Hawaii&state%5B12%5D=Idaho&state%5B13%5D=Illinois&state%5B14%5D=Indiana&state%5B15%5D=Iowa&state%5B16%5D=Kansas&state%5B17%5D=Kentucky&state%5B18%5D=Louisiana&state%5B19%5D=Maine&state%5B20%5D=Maryland&state%5B21%5D=Massachusetts&state%5B22%5D=Michigan&state%5B23%5D=Minnesota&state%5B24%5D=Mississippi&state%5B25%5D=Missouri&state%5B26%5D=Montana&state%5B27%5D=Nebraska&state%5B28%5D=Nevada&state%5B29%5D=New%20Hampshire&state%5B30%5D=New%20Jersey&state%5B31%5D=New%20Mexico&state%5B32%5D=New%20York&state%5B33%5D=North%20Carolina&state%5B34%5D=Ohio&state%5B35%5D=Oklahoma&state%5B36%5D=Oregon&state%5B37%5D=Pennsylvania&state%5B38%5D=Rhode%20Island&state%5B39%5D=South%20Carolina&state%5B40%5D=South%20Dakota&state%5B41%5D=Tennessee&state%5B42%5D=Texas&state%5B43%5D=Utah&state%5B44%5D=Vermont&state%5B45%5D=Virginia&state%5B46%5D=Washington&state%5B47%5D=West%20Virginia&state%5B48%5D=Wisconsin&state%5B49%5D=Wyoming") %>% 
  html_elements(".grid-cell") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  filter(row_number()%%2 != 0) %>% 
  rename(agents = value)
## get the broker links first page
agent_links <- read_html("https://brokers.interexo.com/search?country%5B0%5D=United%20States&state%5B0%5D=Alabama&state%5B1%5D=Alaska&state%5B2%5D=Arizona&state%5B3%5D=Arkansas&state%5B4%5D=California&state%5B5%5D=Colorado&state%5B6%5D=Connecticut&state%5B7%5D=Delaware&state%5B8%5D=District%20of%20Columbia&state%5B9%5D=Florida&state%5B10%5D=Georgia&state%5B11%5D=Hawaii&state%5B12%5D=Idaho&state%5B13%5D=Illinois&state%5B14%5D=Indiana&state%5B15%5D=Iowa&state%5B16%5D=Kansas&state%5B17%5D=Kentucky&state%5B18%5D=Louisiana&state%5B19%5D=Maine&state%5B20%5D=Maryland&state%5B21%5D=Massachusetts&state%5B22%5D=Michigan&state%5B23%5D=Minnesota&state%5B24%5D=Mississippi&state%5B25%5D=Missouri&state%5B26%5D=Montana&state%5B27%5D=Nebraska&state%5B28%5D=Nevada&state%5B29%5D=New%20Hampshire&state%5B30%5D=New%20Jersey&state%5B31%5D=New%20Mexico&state%5B32%5D=New%20York&state%5B33%5D=North%20Carolina&state%5B34%5D=Ohio&state%5B35%5D=Oklahoma&state%5B36%5D=Oregon&state%5B37%5D=Pennsylvania&state%5B38%5D=Rhode%20Island&state%5B39%5D=South%20Carolina&state%5B40%5D=South%20Dakota&state%5B41%5D=Tennessee&state%5B42%5D=Texas&state%5B43%5D=Utah&state%5B44%5D=Vermont&state%5B45%5D=Virginia&state%5B46%5D=Washington&state%5B47%5D=West%20Virginia&state%5B48%5D=Wisconsin&state%5B49%5D=Wyoming") %>% 
  html_elements(".grid-cell") %>% 
  html_children() %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  filter(row_number()%% 2 != 0) %>% 
  rename(link = value)

page_1 <- bind_cols(agents, companies, agent_links)

page_1 <- page_1 %>% 
  mutate(full_link = paste0("https://brokers.interexo.com/", link))

## example of scraping information on first link:
## broker column names
broker_cols <- c("brokerage", "info", "address", "email", "phone", "website")
broker_cols_1 <- read_html("https://brokers.interexo.com/united-states_new-york/company/bridge-brokers/aj-caro-8e1") %>% 
  html_elements(".col-auto") %>% 
  html_text2() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  pivot_wider(names_from = value, values_from= value) %>% 
  set_names(broker_cols)
  


## loop through the links

links <- page_1 %>% 
  select(full_link) %>% 
  unlist() 
links <- as.list(links)

broker_info <- map_df(links, ~read_html(.) %>% 
         html_elements(".col-auto") %>% 
         html_text2() %>% 
         str_split("\n") %>% 
         unlist() %>% 
         as_tibble() %>% 
         pivot_wider(names_from = value, values_from= value) %>% 
         set_names(broker_cols))

read_html(links[[2]]) %>% 
  html_elements(".pt-3") %>% 
  html_text2() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  pivot_wider(names_from = value, values_from= value) %>% 
  set_names(broker_cols)
