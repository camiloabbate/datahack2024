# Week 7 script
# C

rm(list = ls())

library(tidyverse
library(readxl)

# Ultimate goal: Import 9 years of enrollment from different highschools

# the primary goal of today's class is to learn about some programming  the for loop, the lapply function and to CREATE a function!
# Hopefully we will finish by designing a function that imports several excel files with similar names

#Let's start easy:

# for loop is what you imagine:

## for-loop syntax
#for (i in ""specified range""){
  ##do something
#}

#example:
#let's print the first 5 even natural numbers:
for (i in 1:5){
  print(i*2)
}

#great.. what if we want to store the values in a vector

#First, define where you want to store it:
even_numbers <- NULL
for (i in 1:5){
  even_numbers <- i*2
}
#bad! cuz only the last one is stored


#therefore, we want:
even_numbers <- NULL
for (i in 1:5){
  even_numbers[i] <- i*2
}
# great!

#Maybe something a little more sophisticated?

#let's learn if else
#if (test_expression) {
#  statement
# }

x <- 3
if (x>4){
  print("Hi")
}

x <- 56
if (x>4){
  print("Hi")
} else {
  print("Bye")
}

# Ok, now, let's create the more sophisticated sequence fibonacci sequence:

#Initialize vector
fibonacci <- c(1,1)
#let's fill this with the first 7 fibonacci numbers:
for (i in 1:7){
  if (i < 3) {
    
  } else {
    fibonacci[i] = fibonacci[i-1] + fibonacci[i-2]
  }
}
fibonacci

## Cool, and the last one, before we go into our data wrangling example:
# a function!! a very easy one: absolute value

#body of a function:
## function_name is the function's name that we create
## function() tells R that we are creating a function
## x is an input for the function
# function_name <- function(x) {
  ## body to add.
#  return()
# }

absolute <- function(number){
  sqrt(number^2)
}

absolute(33)

#last one: learn how to use lapply:

lapply(c(-33,-27,-99,-1),absolute)

#Great! Now, let's rock and roll!!

#The general idea here is the following:
# Check our folder with data, note the pattern in names
# import all of those files at once by building a function
# what are we trying to avoid? spending a bunch of time in the future..

#Where are we?

getwd()

enr2002 <- read_excel("lectures/Week 7/enrollment/enrollment_2002.xlsx")

# enr2002 %>% ggplot() + geom_point(aes(x =  , y = )) + geom_line()
# enr2002 %>% hist()

enr2003 <- read_excel("lectures/Week 7/enrollment/enrollment_2003.xlsx") #No, I'm not going to do that

#...

# 1

## 1.1 give me the path (folder with data) and I'll tell you the names of the files there:
# listy <- list.files(full_path)

full_path <- getwd()

full_path <- paste0(full_path,"/lectures/Week 7/enrollment")

listy <- list.files(full_path)

## 1.2 import all of them! but wait, one by one?

enr2002 <- read_excel(paste0(full_path,"/",listy[1]))
# Instead of doing: read_excel(".....listy[1]") 
# and....... doing: read_excel(".....listy[2]")
# ....
# and....... doing: read_excel(".....listy[9]")

# we do: Import everything at once!
#For that, we have lapply!!!
# give me the function and path and I'll tell you the data:

# full_path + listy
#lapply()
#?lapply()

paste0(full_path,"/",listy)

paste0(listy)

listy2 <- paste0(full_path,"/",listy)

list_data <- lapply(listy2,read_excel)  #WARNING! THIS IS GOING TO CREATE A LIST!

list_data[1]

list_data[9]

#Great! We need to transform this into dataframes so we can merge it eventually:
enr2008 <- as.data.frame(list_data[7])

# ? What would you do next?

# we are going to loop:

listy3 <- paste0()

#give me the path and I'll give you the dataframes:
power_import <- function(path){
  # The full path is a character unidimensional variable: the full directory of the
  # folder where the data files (xlsx) are stored
  thenames <- "enrollment_" #Just change this part if you want different df names
  f_listy <- list.files(full_path)
  f_listy2 = paste0(full_path,"/",f_listy)
  f_list_data <- lapply(f_listy2,read_excel)
  f_how_long <- length(f_list_data)   # 9 
  for (i in 1:f_how_long) {
    assign(paste0(thenames, i+2001), as.data.frame(list_data[[i]]),envir = .GlobalEnv)
  }
  
}
full_path

power_import(full_path)

#Same as before, but with a warning

big_import <- function(full_path){
  thenames <- "enrollment_" #Just change this part if you want a different name
  if (typeof(full_path)!="character"){
    print("Wrong, argument needs to be a character")
  }
  else {
    f_listy <- list.files(full_path)
    f_listy2 = paste0(full_path,"/",f_listy)
    f_list_data <- lapply(f_listy2,read_excel)
    f_how_long <- length(f_list_data)
    for (i in 1:f_how_long) {
      assign(paste0(thenames, i+2001), as.data.frame(list_data[[i]]),envir = .GlobalEnv)
    }
  }
}

big_import(c(2,3))

#More later
#disaggregate last part of function or leave them for hw?




# You want to store the data?

# filepath <- file.path("01_data")
# file = file.path(filepath,"rep.RData")
# save(DATAFRAME,file=file)





