#if you haven't installed "pacman" packages, please delete the character "#" right below
#install.packages("pacman")
library("pacman")
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  #linelist,   # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  incidence2,  #draw plot
  ggplot2,
  cowplot
)
covid.data <- import("D:/University/Video_semester2_year1/ctrr/btl/CTRR/R/owid-covid-data.csv")
covid.data <- covid.data %>%
  mutate(date = lubridate::mdy(date))
name.country <- c("Canada", "Greenland", "United States")
covid.data.first <- covid.data %>% 
  filter(location == name.country[1])
covid.data.second <- covid.data %>% 
  filter(location == name.country[2])
covid.data.third <- covid.data %>% 
  filter(location == name.country[3])
#1st
for(i in 1:nrow(covid.data.first)){
  if(is.na(covid.data.first$new_cases[i])){
    if(i<=7){
      if(i==1){
        covid.data.first$new_cases[i] <- 0
      }else{
        if(i==2){
          covid.data.first$new_cases[i] <- covid.data.first$new_cases[1]
        }else{
          covid.data.first$new_cases[i] <- mean(covid.data.first$new_cases[1:i-1])
        }
      }
    }else{
      covid.data.first$new_cases[i] <- mean(covid.data.first$new_cases[(i-7):(i-1)])
    }
  }
}
for(i in 1:nrow(covid.data.first)){
  if(is.na(covid.data.first$new_deaths[i])){
    if(i<=7){
      if(i==1){
        covid.data.first$new_deaths[i] <- 0
      }else{
        if(i==2){
          covid.data.first$new_deaths[i] <- covid.data.first$new_deaths[1]
        }else{
          covid.data.first$new_deaths[i] <- mean(covid.data.first$new_deaths[1:(i-1)])
        }
      }
    }else{
      covid.data.first$new_deaths[i] <- mean(covid.data.first$new_deaths[(i-7):(i-1)])
    }
  }
}
#2nd
for(i in 1:nrow(covid.data.second)){
  if(is.na(covid.data.second$new_cases[i])){
    if(i<=7){
      if(i==1){
        covid.data.second$new_cases[i] <- 0
      }else{
        if(i==2){
          covid.data.second$new_cases[i] <- covid.data.second$new_cases[1]
        }else{
          covid.data.second$new_cases[i] <- mean(covid.data.second$new_cases[1:i-1])
        }
      }
    }else{
      covid.data.second$new_cases[i] <- mean(covid.data.second$new_cases[(i-7):(i-1)])
    }
  }
}
for(i in 1:nrow(covid.data.second)){
  if(is.na(covid.data.second$new_deaths[i])){
    if(i<=7){
      if(i==1){
        covid.data.second$new_deaths[i] <- 0
      }else{
        if(i==2){
          covid.data.second$new_deaths[i] <- covid.data.second$new_deaths[1]
        }else{
          covid.data.second$new_deaths[i] <- mean(covid.data.second$new_deaths[1:(i-1)])
        }
      }
    }
    if(i>7){
      covid.data.second$new_deaths[i] <- mean(covid.data.second$new_deaths[(i-7):(i-1)])
    }
  }
}
#3rd
for(i in 1:nrow(covid.data.third)){
  if(is.na(covid.data.third$new_cases[i])){
    if(i<=7){
      if(i==1){
        covid.data.third$new_cases[i] <- 0
      }else{
        if(i==2){
          covid.data.third$new_cases[i] <- covid.data.third$new_cases[1]
        }else{
          covid.data.third$new_cases[i] <- mean(covid.data.third$new_cases[1:i-1])
        }
      }
    }else{
      covid.data.third$new_cases[i] <- mean(covid.data.third$new_cases[(i-7):(i-1)])
    }
  }
}
for(i in 1:nrow(covid.data.third)){
  if(is.na(covid.data.third$new_deaths[i])){
    if(i<=7){
      if(i==1){
        covid.data.third$new_deaths[i] <- 0
      }else{
        if(i==2){
          covid.data.third$new_deaths[i] <- covid.data.third$new_deaths[1]
        }else{
          covid.data.third$new_deaths[i] <- mean(covid.data.third$new_deaths[1:(i-1)])
        }
      }
    }
    if(i>7){
      covid.data.third$new_deaths[i] <- mean(covid.data.third$new_deaths[(i-7):(i-1)])
    }
  }
}
setwd("D:/University/Video_semester2_year1/ctrr/btl/CTRR/R/task6")
pdf("cau1.pdf")
dev.off()
#cau 1
#Canada
new.case.each.day.1st.ct <- covid.data %>%
  select(location, date, new_cases) %>%
  filter(location == name.country[1])

plot.data.new.case.each.day.1st.ct <- incidence(       # create incidence object
  x = new.case.each.day.1st.ct,             # dataset
  date_index = date,  # date column
  count = new_cases, # column with counts
  interval = "month",          # date grouping interval
)
Canada <- plot(
  plot.data.new.case.each.day.1st.ct,
  legend = "top",                       # legend on top
  title = name.country[1],  # title
  ylab = "new cases",               # x-axis label
  xlab = "time",               # y-axis label
  )
#Greenland
new.case.each.day.2st.ct <- covid.data %>%
  select(location, date, new_cases) %>%
  filter(location == name.country[2])

plot.data.new.case.each.day.2st.ct <- incidence(       # create incidence object
  x = new.case.each.day.2st.ct,             # dataset
  date_index = date,  # date column
  count = new_cases, # column with counts
  interval = "month",          # date grouping interval
)
Greenland <- plot(
  plot.data.new.case.each.day.2st.ct,
  legend = "top",                       # legend on top
  title = name.country[2],  # title
  ylab = "new cases",               # x-axis label
  xlab = "time",               # y-axis label
)
#United States
new.case.each.day.3st.ct <- covid.data %>%
  select(location, date, new_cases) %>%
  filter(location == name.country[3])

plot.data.new.case.each.day.3st.ct <- incidence(       # create incidence object
  x = new.case.each.day.3st.ct,             # dataset
  date_index = date,  # date column
  count = new_cases, # column with counts
  interval = "month",          # date grouping interval
)
United.States <- plot(
  plot.data.new.case.each.day.3st.ct,
  legend = "top",                       # legend on top
  title = name.country[3],  # title
  ylab = "new cases",               # x-axis label
  xlab = "time",               # y-axis label
)
setwd("D:/University/Video_semester2_year1/ctrr/btl/CTRR/R/task6")
pdf("cau1.pdf",
    width = 865, # set chi???u r???ng c???a ???nh
    height = 571)
cowplot::plot_grid(Canada, Greenland, United.States)
dev.off()
#cau 2
#Canada
new.deaths.each.day.1st.ct <- covid.data %>%
  select(location, date, new_deaths) %>%
  filter(location == name.country[1])

plot.data.new.case.each.day.1st.ct <- incidence(       # create incidence object
  x = new.case.each.day.1st.ct,             # dataset
  date_index = date,  # date column
  count = new_deaths, # column with counts
  interval = "month",          # date grouping interval
)
Canada <- plot(
  plot.data.new.case.each.day.1st.ct,
  legend = "top",                       # legend on top
  title = name.country[1],  # title
  ylab = "new cases",               # x-axis label
  xlab = "time",               # y-axis label
)
#Greenland
new.case.each.day.2st.ct <- covid.data %>%
  select(location, date, new_deaths) %>%
  filter(location == name.country[2])

plot.data.new.case.each.day.2st.ct <- incidence(       # create incidence object
  x = new.case.each.day.2st.ct,             # dataset
  date_index = date,  # date column
  count = new_deaths, # column with counts
  interval = "month",          # date grouping interval
)
Greenland <- plot(
  plot.data.new.case.each.day.2st.ct,
  legend = "top",                       # legend on top
  title = name.country[2],  # title
  ylab = "new cases",               # x-axis label
  xlab = "time",               # y-axis label
)
#United States
new.case.each.day.3st.ct <- covid.data %>%
  select(location, date, new_deaths) %>%
  filter(location == name.country[3])

plot.data.new.case.each.day.3st.ct <- incidence(       # create incidence object
  x = new.case.each.day.3st.ct,             # dataset
  date_index = date,  # date column
  count = new_deaths, # column with counts
  interval = "month",          # date grouping interval
)
United.States <- plot(
  plot.data.new.case.each.day.3st.ct,
  legend = "top",                       # legend on top
  title = name.country[3],  # title
  ylab = "new cases",               # x-axis label
  xlab = "time",               # y-axis label
)
setwd("D:/University/Video_semester2_year1/ctrr/btl/CTRR/R/task6")
pdf("cau1.pdf",
    width = 865, # set chi???u r???ng c???a ???nh
    height = 571)
cowplot::plot_grid(Canada, Greenland, United.States)
dev.off()
