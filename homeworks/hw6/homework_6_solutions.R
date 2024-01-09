library(tictoc)
library(tidyverse)
set.seed(1992)

random_length <- as.integer(runif(1, 0, 100))
random_int <- as.integer(runif(random_length, min= 0, max = 1000))


sum(random_int %% 2)
count = 0
for (i in random_int) {
  if (i %% 2 == 0) {
    count = count + 1
  }
}
input <- c("Michael", "Camilo", "Data Hack")


input <- c("Illness", "Lions", "Love", "Universe", "Michael", "Intense", "Night", "Apple", "Tiger", "Icon")

final_letters <- rep(" ", length(input))

for (i in 1:length(final_letters)){
  final_letters[i] <- str_sub(input[i], 1, 1)
}

word_2 <- lapply(input, function (x) str_sub(x, 1, 1)) %>% unlist()
word <- ""
for (i in final_letters){
  word <- paste0(word, i)
}


final_letters

files <- list.files("homework_assignments/homework_6/", pattern = ".xlsx$")

for (file in files) {
  year <- str_extract(file, "\\d{4}")
  data <- readxl::read_excel(paste0("homework_assignments/homework_6/", file))
  assign(paste0("crime_", year), data)
}
