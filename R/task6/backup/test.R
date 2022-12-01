#if you haven't installed "pacman" packages, please delete the character "#" right below
#install.packages("pacman")
library("pacman")
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse   # data management and visualization
)
covid.data <- import("owid-covid-data.csv")
name.country <- c("Canada", "Greenland", "United States")
covid.data.first <- covid.data %>% 
  filter(location == name.country[1])
covid.data.second <- covid.data %>% 
  filter(location == name.country[2])
covid.data.third <- covid.data %>% 
  filter(location == name.country[3])
for(i in 1:nrow(covid.data.first)){
  if(is.na(covid.data.first[i])){
    if(i==0) covid.data.first[i]=0
    if(i==)
    if(i<=7) {covid.data.first[i] <- mean(covid.data.first[1;i-1])}  else{
      covid.data.first[i] <- mean(covid.data.first[i-7;i-1])
    }
    
  }
}