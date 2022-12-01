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
  linelist,   # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  incidence2,  #draw plot
  ggplot2
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
#cau1
new.case.for.each.month <- covid.data %>%
  select(location, date, new_cases) %>%
  filter(location == name.country[1]| location == name.country[2]|location == name.country[3])

plot.new.case.for.each.month <- incidence(       # create incidence object
  x = new.case.for.each.month,             # dataset
  date_index = date,  # date column
  count = new_cases, # column with counts
  interval = "month",          # date grouping interval
  groups = location
  )

incidence2::facet_plot(
  plot.new.case.for.each.month,      # incidence object
  facets = location,# facet column
  title = "S??? ca nhi???m theo tháng",
  xlab = " Th???i gian",
  ylab = " S??? ca nhi???m")
#cau2
new.deaths.for.each.month <- covid.data %>%
  select(location, date, new_deaths) %>%
  filter(location == name.country[1]| location == name.country[2]|location == name.country[3])

plot.new.deaths.for.each.month <- incidence(       # create incidence object
  x = new.deaths.for.each.month,             # dataset
  date_index = date,  # date column
  count = new_deaths, # column with counts
  interval = "month",          # date grouping interval
  groups = location
)

incidence2::facet_plot(
  plot.new.deaths.for.each.month,      # incidence object
  facets = location,# facet column
  title = "S??? ca t??? vong theo tháng",
  xlab = " Th???i gian",
  ylab = " S??? ca t??? vong")
#cau4
new.case.for.tow.last.month <- covid.data %>%
  select(location, date, new_cases) %>%
  filter((location == name.country[1]| location == name.country[2]|location == name.country[3])&
         (month(date)==11| month(date)==12))

plot.new.case.for.tow.last.month <- incidence(       # create incidence object
  x = new.case.for.tow.last.month,             # dataset
  date_index = date,  # date column
  count = new_cases, # column with counts
  interval = "month",          # date grouping interval
  groups = location
)

incidence2::facet_plot(
  plot.new.case.for.tow.last.month,      # incidence object
  facets = location,# facet column
  title = "S??? ca nhi???m theo tháng",
  xlab = " Th???i gian",
  ylab = " S??? ca nhi???m")
#cau5
new.case.for.tow.last.month <- covid.data %>%
  select(location, date, new_deaths) %>%
  filter((location == name.country[1]| location == name.country[2]|location == name.country[3])&
           (month(date)==11| month(date)==12)) %>%
  mutate(date = as.character(date))

plot.new.case.for.tow.last.month <- incidence(       # create incidence object
  x = new.case.for.tow.last.month,             # dataset
  date_index = date,  # date column
  count = new_deaths, # column with counts
  interval = "month",          # date grouping interval
  groups = location
)

incidence2::facet_plot(
  plot.new.case.for.tow.last.month,      # incidence object
  facets = location,# facet column
  title = "S??? ca nhi???m theo tháng",
  xlab = " Th???i gian",
  ylab = " S??? ca nhi???m")
#test
cumulative_case_counts <- plot.new.case.for.each.month %>% 
  #count(new_cases) %>%                # count of rows per day (returned in column "n")   
  mutate(                         
    cumulative_cases = cumsum(new_cases)       # new column of the cumulative number of rows at each date
  )
plot.cumulative_case_counts <- incidence(       # create incidence object
  x = cumulative_case_counts,             # dataset
  date_index = date,  # date column
  count = cumulative_cases, # column with counts
  #interval = "day",          # date grouping interval
  groups = location
)
incidence2::facet_plot(
  plot.cumulative_case_counts,      # incidence object
  facets = location,# facet column
  title = "S??? ca nhi???m theo tháng",
  xlab = " Th???i gian",
  ylab = " S??? ca nhi???m")

monthly_breaks_central <- seq.Date(
  from = floor_date(min(new.case.for.each.month$date, na.rm=T),   "month"), 
  to   = ceiling_date(max(new.case.for.each.month$date, na.rm=T), "month"), 
  by   = "month")    


ggplot(data = new.case.for.each.month, mapping = aes(x=date, y = new_cases)) + 
  
  # make histogram: specify bin break points: starts the Monday before first case, end Monday after last case
  geom_point(
    
    # mapping aesthetics
    #mapping = aes(x = date),  # date column mapped to x-axis
    
    # histogram bin breaks
    #breaks = monthly_breaks_central, # histogram bin breaks defined previously
    
    # bars
    color = "darkblue",     # color of lines around bars
    fill = "lightblue"      # color of fill within bars
  )+
  facet_wrap(
    ~location,
    scales = "free"
  )+
  scale_x_date(
    # 1 break every 1 month
    date_breaks = "1 days",
    # labels should show month then date
    #date_labels = "%m"
  )
cumulative_case_counts <- plot.new.case.for.each.month %>% 
  #count(new_cases) %>%                # count of rows per day (returned in column "n")   
  mutate(                         
    cumulative_cases = cumsum(new_cases)       # new column of the cumulative number of rows at each date
  )