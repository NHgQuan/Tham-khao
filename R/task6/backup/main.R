#if you haven't installed "pacman" packages, please delete the character "#" right below
#install.packages("pacman")
library("pacman")
##load cac packages con thieu
########## 
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse,   # data management and visualization
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  incidence2,  #draw plot
  ggplot2
)
##########
#nhap file du lieu va lam sach so qua
########
covid.data <- import(here("R", "owid-covid-data.csv"))
covid.data <- covid.data %>%
  mutate(date = lubridate::mdy(date))
name.country <- c("Canada", "Greenland", "United States")
########
#tao bo du lieu cua ba nuoc
####################################
covid.data.first <- covid.data %>% 
  filter(location == name.country[1])
covid.data.second <- covid.data %>% 
  filter(location == name.country[2])
covid.data.third <- covid.data %>% 
  filter(location == name.country[3])
#xu l� missing value c???a ba b??? d??? li???u tr�n
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
########
#cau1+2
########
#Canada
#new_case
covid.data.first.ncases.1.CND<-covid.data.first %>%
    select(-c(iso_code, continent, location, new_deaths)) %>%#xoa cac cot khong can thiet
    filter(month(date)==2|month(date)==3|month(date)==4|month(date)==6) %>%#giu lai cac thang can bieu dien
    mutate(month = month(date), year = year(date))#tao cot month va year de lat phan nhom
cau1.ncases.CND<-ggplot(covid.data.first.ncases.1.CND, aes(x=date, y=new_cases)) +
    geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Number of cases for each month in Canada"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_deaths 
covid.data.first.ndeaths.2.CND<-covid.data.first %>%
  select(-c(iso_code, continent, location, new_cases)) %>%
  filter(month(date)==2|month(date)==3|month(date)==4|month(date)==6) %>%
  mutate(month = month(date), year = year(date))
cau2.ndeaths.CND<-ggplot(covid.data.first.ndeaths.2.CND, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Number of newdeaths for each month in Canada"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#Greenland
#new_case
covid.data.second.ncases.1.GRL<-covid.data.second %>%
    select(-c(iso_code, continent, location, new_deaths)) %>%#xoa cac cot khong can thiet
    filter(month(date)==2|month(date)==3|month(date)==4|month(date)==6) %>%#giu lai cac thang can bieu dien
    mutate(month = month(date), year = year(date))#tao cot month va year de lat phan nhom
cau1.ncases.GRL<-ggplot(covid.data.second.ncases.1.GRL, aes(x=date, y=new_cases)) +
    geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Number of cases for each month in Greenland"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_deaths 
covid.data.second.ndeaths.2.GRL<-covid.data.second %>%
  select(-c(iso_code, continent, location, new_cases)) %>%
  filter(month(date)==2|month(date)==3|month(date)==4|month(date)==6) %>%
  mutate(month = month(date), year = year(date))
cau2.ndeaths.GRL<-ggplot(covid.data.second.ndeaths.2.GRL, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Number of newdeaths for each month in Greenland"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#United States
#new_case
covid.data.third.ncases.1.US<-covid.data.third %>%
    select(-c(iso_code, continent, location, new_deaths)) %>%#xoa cac cot khong can thiet
    filter(month(date)==2|month(date)==3|month(date)==4|month(date)==6) %>%#giu lai cac thang can bieu dien
    mutate(month = month(date), year = year(date))#tao cot month va year de lat phan nhom
cau1.ncases.US<-ggplot(covid.data.third.ncases.1.US, aes(x=date, y=new_cases)) +
    geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Number of cases for each month in United States"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_deaths 
covid.data.third.ndeaths.2.US<-covid.data.third %>%
  select(-c(iso_code, continent, location, new_cases)) %>%
  filter(month(date)==2|month(date)==3|month(date)==4|month(date)==6) %>%
  mutate(month = month(date), year = year(date))
cau2.ndeaths.US<-ggplot(covid.data.third.ndeaths.2.US, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Number of newdeaths for each month in United States"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
########
#cau4+5
########
#Canada
#new_case
covid.data.first.ncases.4.CND<-covid.data.first %>%
    select(-c(iso_code, continent, location, new_deaths)) %>%#xoa cac cot khong can thiet
    filter(month(date)==12|month(date)==11) %>%#giu lai cac thang can bieu dien
    mutate(month = month(date), year = year(date))#tao cot month va year de lat phan nhom
cau4.ncases.CND<-ggplot(covid.data.first.ncases.4.CND, aes(x=date, y=new_cases)) +
    geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Number of cases for two last months of the years in Canada"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_deaths 
covid.data.first.ndeaths.5.CND<-covid.data.first %>%
  select(-c(iso_code, continent, location, new_cases)) %>%
  filter(month(date)==12|month(date)==11) %>%#giu lai cac thang can bieu dien
  mutate(month = month(date), year = year(date))
cau5.ndeaths.CND<-ggplot(covid.data.first.ndeaths.5.CND, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Number of newdeaths for two last months of the years in Canada"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#Greenland
#new_case
covid.data.second.ncases.4.GRL<-covid.data.second %>%
    select(-c(iso_code, continent, location, new_deaths)) %>%#xoa cac cot khong can thiet
    filter(month(date)==12|month(date)==11) %>%#giu lai cac thang can bieu dien
    mutate(month = month(date), year = year(date))#tao cot month va year de lat phan nhom
cau4.ncases.GRL<-ggplot(covid.data.second.ncases.4.GRL, aes(x=date, y=new_cases)) +
    geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Number of cases for two last months of the years in Greenland"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_deaths 
covid.data.second.ndeaths.5.GRL<-covid.data.second %>%
  select(-c(iso_code, continent, location, new_cases)) %>%
  filter(month(date)==12|month(date)==11) %>%#giu lai cac thang can bieu dien
  mutate(month = month(date), year = year(date))
cau5.ndeaths.GRL<-ggplot(covid.data.second.ndeaths.5.GRL, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Number of newdeaths for two last months of the years in Greenland"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#United States
#new_case
covid.data.third.ncases.4.US<-covid.data.third %>%
    select(-c(iso_code, continent, location, new_deaths)) %>%#xoa cac cot khong can thiet
    filter(month(date)==12|month(date)==11) %>%#giu lai cac thang can bieu dien
    mutate(month = month(date), year = year(date))#tao cot month va year de lat phan nhom
cau4.ncases.US<-ggplot(covid.data.third.ncases.4.US, aes(x=date, y=new_cases)) +
    geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Number of cases for two last months of the years in United States"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_deaths 
covid.data.third.ndeaths.5.US<-covid.data.third %>%
  select(-c(iso_code, continent, location, new_cases)) %>%
  filter(month(date)==12|month(date)==11) %>%#giu lai cac thang can bieu dien
  mutate(month = month(date), year = year(date))
cau5.ndeaths.US<-ggplot(covid.data.third.ndeaths.5.US, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Number of newdeaths for two last months of the years in United States"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
########
#cau7+8
########
#Canada
#new_case
#thiet lap cac gia tri ban dau
months.present <-c(2,3,4,6)
years.present <-c(2020,2021,2022)
covid.data.first.ncases.7.CND <- data.frame(NULL)
#tong hop tong so ca nhiem theo tung thang va bo vao dataframe vua tao
for(i in years.present){
  for(j in months.present){
    temp <- covid.data.first.ncases.1.CND %>%
      filter(month == j&year==i) %>%
      mutate(new_cases = cumsum(new_cases))
    covid.data.first.ncases.7.CND <- bind_rows(covid.data.first.ncases.7.CND, temp)
  }
}
#vedothi
cau7.ncases.CND<-ggplot(covid.data.first.ncases.7.CND, aes(x=date, y=new_cases)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Cumulative number of new cases for each month in Canada"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_death
#thiet lap cac gia tri ban dau
months.present <-c(2,3,4,6)
years.present <-c(2020,2021,2022)
covid.data.first.ndeaths.8.CND <- data.frame(NULL)
#tong hop tong so ca nhiem theo tung thang va bo vao dataframe vua tao
for(i in years.present){
  for(j in months.present){
    temp <- covid.data.first.ndeaths.2.CND %>%
      filter(month == j&year==i) %>%
      mutate(new_deaths = cumsum(new_deaths))
    covid.data.first.ndeaths.8.CND <- bind_rows(covid.data.first.ndeaths.8.CND, temp)
  }
}
#vedothi
cau8.ndeaths.CND<-ggplot(covid.data.first.ndeaths.8.CND, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Cumulative number of new deaths for each month in Canada"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#Greenland
#new_case
#thiet lap cac gia tri ban dau
covid.data.second.ncases.7.GRL <- data.frame(NULL)
#tong hop tong so ca nhiem theo tung thang va bo vao dataframe vua tao
for(i in years.present){
  for(j in months.present){
    temp <- covid.data.second.ncases.1.GRL %>%
      filter(month == j&year==i) %>%
      mutate(new_cases = cumsum(new_cases))
    covid.data.second.ncases.7.GRL <- bind_rows(covid.data.second.ncases.7.GRL, temp)
  }
}
#vedothi
cau7.ncases.GRL<-ggplot(covid.data.second.ncases.7.GRL, aes(x=date, y=new_cases)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Cumulative number of new cases for each month in Greenland"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_death
#thiet lap cac gia tri ban dau
months.present <-c(2,3,4,6)
years.present <-c(2020,2021,2022)
covid.data.second.ndeaths.8.GRL <- data.frame(NULL)
#tong hop tong so ca nhiem theo tung thang va bo vao dataframe vua tao
for(i in years.present){
  for(j in months.present){
    temp <- covid.data.second.ndeaths.2.GRL %>%
      filter(month == j&year==i) %>%
      mutate(new_deaths = cumsum(new_deaths))
    covid.data.second.ndeaths.8.GRL <- bind_rows(covid.data.second.ndeaths.8.GRL, temp)
  }
}
#vedothi
cau8.ndeaths.GRL<-ggplot(covid.data.second.ndeaths.8.GRL, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Cumulative number of new deaths for each month in Greenland"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#United states
#new_case
#thiet lap cac gia tri ban dau
covid.data.third.ncases.7.US <- data.frame(NULL)
#tong hop tong so ca nhiem theo tung thang va bo vao dataframe vua tao
for(i in years.present){
  for(j in months.present){
    temp <- covid.data.third.ncases.1.US %>%
      filter(month == j&year==i) %>%
      mutate(new_cases = cumsum(new_cases))
    covid.data.third.ncases.7.US <- bind_rows(covid.data.third.ncases.7.US, temp)
  }
}
#vedothi
cau7.ncases.US<-ggplot(covid.data.third.ncases.7.US, aes(x=date, y=new_cases)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Cases",
    title = "Cumulative number of new cases for each month in United States"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#new_death
#thiet lap cac gia tri ban dau
months.present <-c(2,3,4,6)
years.present <-c(2020,2021,2022)
covid.data.third.ndeaths.8.US <- data.frame(NULL)
#tong hop tong so ca nhiem theo tung thang va bo vao dataframe vua tao
for(i in years.present){
  for(j in months.present){
    temp <- covid.data.third.ndeaths.2.US %>%
      filter(month == j&year==i) %>%
      mutate(new_deaths = cumsum(new_deaths))
    covid.data.third.ndeaths.8.US <- bind_rows(covid.data.third.ndeaths.8.US, temp)
  }
}
#vedothi
cau8.ndeaths.US<-ggplot(covid.data.third.ndeaths.8.US, aes(x=date, y=new_deaths)) +
  geom_col(width = 1) +
  labs(
    x="Time",
    y="Deaths",
    title = "Cumulative number of new deaths for each month in United States"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#########
#cau3
#########
#Canada
#tao bo du lieu moi
covid.data.first.ncases.3.CND <- covid.data.first.ncases.1.CND %>%
    mutate(Type = "new cases") %>%
    rename(number = new_cases)
covid.data.first.ndeaths.3.CND <- covid.data.first.ndeaths.2.CND %>%
  mutate(Type = "new deaths") %>%
  rename(number = new_deaths)
covid.data.first.3 <- bind_rows(covid.data.first.ncases.3.CND, covid.data.first.ndeaths.3.CND)
#vebieudo
cau3.CND<-ggplot(covid.data.first.3, aes(x=date, y=number, fill = Type)) +
  geom_col(
    width = 1,
    position = "dodge"
    ) +
  labs(
    x="Time",
    y="Number",
    title = "Number of new cases and new deaths for each month in Canada"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#Greenland
#tao bo du lieu moi
covid.data.second.ncases.3.GRL <- covid.data.second.ncases.1.GRL %>%
  mutate(Type = "new cases") %>%
  rename(number = new_cases)
covid.data.second.ndeaths.3.GRL <- covid.data.second.ndeaths.2.GRL %>%
  mutate(Type = "new deaths") %>%
  rename(number = new_deaths)
covid.data.second.3 <- bind_rows(covid.data.second.ncases.3.GRL, covid.data.second.ndeaths.3.GRL)
#vebieudo
cau3.GRL<-ggplot(covid.data.second.3, aes(x=date, y=number, fill = Type)) +
  geom_col(
    width = 1,
    position = "dodge"
  ) +
  labs(
    x="Time",
    y="Number",
    title = "Number of new cases and new deaths for each month in Greenland"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#United States
#tao bo du lieu moi
covid.data.third.ncases.3.US <- covid.data.third.ncases.1.US %>%
  mutate(Type = "new cases") %>%
  rename(number = new_cases)
covid.data.third.ndeaths.3.US <- covid.data.third.ndeaths.2.US %>%
  mutate(Type = "new deaths") %>%
  rename(number = new_deaths)
covid.data.third.3 <- bind_rows(covid.data.third.ncases.3.US, covid.data.third.ndeaths.3.US)
#vebieudo
cau3.US<-ggplot(covid.data.third.3, aes(x=date, y=number, fill = Type)) +
  geom_col(
    width = 1,
    position = "dodge"
  ) +
  labs(
    x="Time",
    y="Number",
    title = "Number of new cases and new deaths for each month in United States"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
########
#cau6
########
#Canada
#tao bo du lieu moi
covid.data.first.ncases.6.CND <- covid.data.first.ncases.4.CND %>%
  mutate(Type = "new cases") %>%
  rename(number = new_cases)
covid.data.first.ndeaths.6.CND <- covid.data.first.ndeaths.5.CND %>%
  mutate(Type = "new deaths") %>%
  rename(number = new_deaths)
covid.data.first.6 <- bind_rows(covid.data.first.ncases.6.CND, covid.data.first.ndeaths.6.CND)
#vebieudo
cau6.CND<-ggplot(covid.data.first.6, aes(x=date, y=number, fill = Type)) +
  geom_col(
    width = 1,
    position = "dodge"
  ) +
  labs(
    x="Time",
    y="Number",
    title = "Number of new cases and new deaths for two last months in Canada"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#Greenland
#tao bo du lieu moi
covid.data.second.ncases.6.GRL <- covid.data.second.ncases.4.GRL %>%
  mutate(Type = "new cases") %>%
  rename(number = new_cases)
covid.data.second.ndeaths.6.GRL <- covid.data.second.ndeaths.5.GRL %>%
  mutate(Type = "new deaths") %>%
  rename(number = new_deaths)
covid.data.second.6 <- bind_rows(covid.data.second.ncases.6.GRL, covid.data.second.ndeaths.6.GRL)
#vebieudo
cau6.GRL<-ggplot(covid.data.second.6, aes(x=date, y=number, fill = Type)) +
  geom_col(
    width = 1,
    position = "dodge"
  ) +
  labs(
    x="Time",
    y="Number",
    title = "Number of new cases and new deaths for two last months in Greenland"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#United States
#tao bo du lieu moi
covid.data.third.ncases.6.US <- covid.data.third.ncases.4.US %>%
  mutate(Type = "new cases") %>%
  rename(number = new_cases)
covid.data.third.ndeaths.6.US <- covid.data.third.ndeaths.5.US %>%
  mutate(Type = "new deaths") %>%
  rename(number = new_deaths)
covid.data.third.6 <- bind_rows(covid.data.third.ncases.6.US, covid.data.third.ndeaths.6.US)
#vebieudo
cau6.US<-ggplot(covid.data.third.6, aes(x=date, y=number, fill = Type)) +
  geom_col(
    width = 1,
    position = "dodge"
  ) +
  labs(
    x="Time",
    y="Number",
    title = "Number of new cases and new deaths for two last months in United States"
  ) +
  theme_bw() +
  scale_x_date(
    # labels should show month then date
    date_labels = "%b"
  ) +
  facet_grid(year ~ month, scales = "free_y")#phan nhom bang thang va nam
#########
#xuat file
#########
#cau1
##
ggsave( filename = "task6_subtask1_Cananda.pdf",
        plot = cau1.ncases.CND,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask1_Greenland.pdf",
        plot = cau1.ncases.GRL,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask1_UnitedStates.pdf",
        plot = cau1.ncases.US,
        scale = 1,
        units = c("in"),
        dpi = 300)
#cau2
##
ggsave( filename = "task6_subtask2_Cananda.pdf",
        plot = cau2.ndeaths.CND,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask2_Greenland.pdf",
        plot = cau2.ndeaths.GRL,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask2_UnitedStates.pdf",
        plot = cau2.ndeaths.US,
        scale = 1,
        units = c("in"),
        dpi = 300)
#cau3
##
ggsave( filename = "task6_subtask3_Cananda.pdf",
        plot = cau3.CND,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask3_Greenland.pdf",
        plot = cau3.GRL,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask3_UnitedStates.pdf",
        plot = cau3.US,
        scale = 1,
        units = c("in"),
        dpi = 300)
#cau4
##
ggsave( filename = "task6_subtask4_Cananda.pdf",
        plot = cau4.ncases.CND,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask4_Greenland.pdf",
        plot = cau4.ncases.GRL,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask4_UnitedStates.pdf",
        plot = cau4.ncases.US,
        scale = 1,
        units = c("in"),
        dpi = 300)
#cau5
##
ggsave( filename = "task6_subtask5_Cananda.pdf",
        plot = cau5.ndeaths.CND,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask5_Greenland.pdf",
        plot = cau5.ndeaths.GRL,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask5_UnitedStates.pdf",
        plot = cau5.ndeaths.US,
        scale = 1,
        units = c("in"),
        dpi = 300)
#cau6
##
ggsave( filename = "task6_subtask6_Cananda.pdf",
        plot = cau6.CND,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask6_Greenland.pdf",
        plot = cau6.GRL,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask6_UnitedStates.pdf",
        plot = cau6.US,
        scale = 1,
        units = c("in"),
        dpi = 300)
#cau7
##
ggsave( filename = "task6_subtask7_Cananda.pdf",
        plot = cau7.ncases.CND,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask7_Greenland.pdf",
        plot = cau7.ncases.GRL,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask7_UnitedStates.pdf",
        plot = cau7.ncases.US,
        scale = 1,
        units = c("in"),
        dpi = 300)
#cau8
##
ggsave( filename = "task6_subtask7_Cananda.pdf",
        plot = cau8.ndeaths.CND,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask8_Greenland.pdf",
        plot = cau8.ndeaths.GRL,
        scale = 1,
        units = c("in"),
        dpi = 300)
ggsave( filename = "task6_subtask8_UnitedStates.pdf",
        plot = cau8.ndeaths.US,
        scale = 1,
        units = c("in"),
        dpi = 300)