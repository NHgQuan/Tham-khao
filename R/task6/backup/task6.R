setwd("D:/University/Video_semester2_year1/ctrr/btl/CTRR/R")
covid.data <- read.csv("owid-covid-data.csv")
name.country <- c("Canada", "Greenland", "United States")
for(index in 1:nrow(covid.data)){
  sum <-0
  if(is.na(covid.data$new_cases[index])){
  for(i in index-7:index-1){
    if(i<=0) next
    sum <- sum + covid.data$new_cases[i]
  }
  
  covid.data$new_cases[index] <- sum/7
  }
  sum <-0
  if(is.na(covid.data$new_deaths[index])){
    for(i in index-7:index-1){
      if(i<=0) next
      sum <- sum + covid.data$new_deaths[i]
    }
  
  covid.data$new_deaths[index] <- sum/7
  }
} 
