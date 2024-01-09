# Week  

rm(list = ls())

library(tidyverse)
library(readxl)

for (i in 1:5){
  
  print(i*2)
  
  
}

vector = NULL
for (i in 1:5){
    vector[i] <- i*2
}


crazy_seq <- c("a","b")
for (j in crazy_seq){
  print(j)
}



# if else....

# if (something happens) {return}
# else {return something el}

x = 4

if (x > 5) {print("Hi")
} else {
    print("Bye")
  }


if (x > 5) {print("Hi")} else {print("Bye")}


fibonacci <- NULL
for (i in 1:300) {
  if(i < 3){ fibonacci[i] = 1  } else {
    
    fibonacci[i] = fibonacci[i-1] + fibonacci[i-2]
    
  }
}



#  FUNCTIONS

camilo_absolute <- function(real_number){
  
  #asduiosdfgiojsd
  # explain to you and other what you do here
  #asdklsdkf
  
  sqrt(real_number^2)
  
  
  
}

camilo_absolute(-5)

roots <- function(a,b,c){
  root1 <- (-b + sqrt(b^2 - 4*a*c))/2*a
  root2 <- (-b - sqrt(b^2 - 4*a*c))/2*a
  print(root1,root2)    

}
roots(1,-2,1)

#Last one before the data import example: LAPPLY:

?lapply()

first_list_example = lapply(c(-5,-7,-9),camilo_absolute)


getwd()

enr2002 <- read_excel("lectures/Week 7/enrollment/enrollment_2002.xlsx")
enr2003 <- read_excel("lectures/Week 7/enrollment/enrollment_2003.xlsx")



listy <- list.files("lectures/Week 7/enrollment")


paste0("a"," ","b","c")


full_directories <- paste0(getwd(),"/lectures/Week 7/enrollment/",listy)

read_excel(full_directories)


ugly_list <- lapply(full_directories,read_excel)


enrollment_2002 <- ugly_list[[1]]
.....

enrollment_2010 <- ugly_list[[9]]

# LAST PIECE OF THE IMPORTANT COMMANDS:

#assing function:

?assign()



for (i in 1:9){
  assign(paste0("enrol_", i+2001) , ugly_list[[i]] ) 
  }

assign(paste0("enrol_", 1+2001) , ugly_list[[1]] ) 

paste0("enrol_", 1+2001)

ugly_list[[1]]

































