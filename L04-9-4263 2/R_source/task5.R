library(stringi)
library(ggplot2)
library(httpgd)
library(lubridate)
library(languageserver)
library(ggpubr)


setwd("~/R")
dataRaw <- read.csv2("owid-covid-data.csv", header = TRUE, sep = ",")
setwd("task5")

x <- length(dataRaw$continent)

MADEcountry <- c("CAN", "GRL", "USA")
dataMade <- dataRaw[0,]

y <- 0

for (ctry in 1:3) 
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == MADEcountry[ctry])
    {
      y <- y + 1
      dataMade[y,] <- dataRaw[i,]
    }
  }
dataMade$date <- as.Date(dataMade$date, "%m/%d/%Y")

dataMade$new_cases[which(is.na(dataMade$new_cases))] <- 0
dataMade$new_deaths[which(is.na(dataMade$new_deaths))] <- 0

data_year_2020 <- c("2020", "2020", "2020", "2020")
data_year_2021 <- c("2021", "2021", "2021", "2021")
data_year_2022 <- c("2022", "2022", "2022", "2022")

data_month <- c("2", "3", "4", "6")

x <- length(dataMade$continent)

#Canada 2020
data_can_2020 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
      {
        y <- y + 1
        data_can_2020[y,] <- dataMade[i,]
      }
    }
  }
}

#Greenland 2020
data_grl_2020 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_grl_2020[y,] <- dataMade[i,]
        }
    }
  }
}

#United States 2020
data_usa_2020 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_usa_2020[y,] <- dataMade[i,]
        }
    }
  }
}
#Bieu do bai 1 nam 2020

ggplot() +
  geom_line(data=data_can_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Canada in 2020", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_grl_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Greenland in 2020", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_usa_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in United States in 2020", x= "Date", y = "New cases") 


#Bieu do bai 2 nam 2020

ggplot() +
  geom_line(data=data_can_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Canada in 2020", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Greenland in 2020", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in United States in 2020", x= "Date", y = "New deaths") 

#Bieu do bai 3 nam 2020
ggplot() +
  geom_line(data=data_can_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_can_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Canada in 2020", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_grl_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Greenland in 2020", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_usa_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in United States in 2020", x= "Date", y = "New deaths") 

#Canada 2021
data_can_2021 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_can_2021[y,] <- dataMade[i,]
        }
    }
  }
}

#Greenland 2021
data_grl_2021 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_grl_2021[y,] <- dataMade[i,]
        }
    }
  }
}

#United States 2021
data_usa_2021 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_usa_2021[y,] <- dataMade[i,]
        }
    }
  }
}

#Bieu do bai 1 nam 2021
ggplot() +
  geom_line(data=data_can_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Canada in 2021", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_grl_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Greenland in 2021", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_usa_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in United States in 2021", x= "Date", y = "New cases") 

#Bieu do bai 2 nam 2021
ggplot() +
  geom_line(data=data_can_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Canada in 2021", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Greenland in 2021", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in United States in 2021", x= "Date", y = "New deaths") 

#Bieu do bai 3 nam 2021
ggplot() +
  geom_line(data=data_can_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_can_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Canada in 2021", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_grl_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Greenland in 2021", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_usa_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in United States in 2021", x= "Date", y = "New deaths") 

#Canada 2022
data_can_2022 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_can_2022[y,] <- dataMade[i,]
        }
    }
  }
}

#Greenland 2022
data_grl_2022 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_grl_2022[y,] <- dataMade[i,]
        }
    }
  }
}

#United States 2022
data_usa_2022 <- dataMade[0,]

y <- 0

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_usa_2022[y,] <- dataMade[i,]
        }
    }
  }
}

#Bieu do bai 1 nam 2022
ggplot() +
  geom_line(data=data_can_2022, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Canada in 2022", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_grl_2022, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Greenland in 2022", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_usa_2022, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in United States in 2022", x= "Date", y = "New cases") 


#Bieu do bai 2 nam 2022
ggplot() +
  geom_line(data=data_can_2022, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Canada in 2022", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_2022, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Greenland in 2022", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_2022, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in United States in 2022", x= "Date", y = "New deaths") 

#Bieu do bai 3 nam 2022
ggplot() +
  geom_line(data=data_can_2022, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_can_2022, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Canada in 2022", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_2022, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_grl_2022, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Greenland in 2022", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_2022, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_usa_2022, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in United States in 2022", x= "Date", y = "New deaths") 

#Bai 4 voi 6 last

data_lastmonth <- c("11", "12")

#Canada 2020 lm
data_can_lm_2020 <- dataMade[0,]

y <- 0

for (j in 1 : 2)
{
  curYear <- data_year_2020[j]
  curMonth <- data_lastmonth[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_can_lm_2020[y,] <- dataMade[i,]
        }
    }
  }
}
#Greenland 2020 lm
data_grl_lm_2020 <- dataMade[0,]

y <- 0

for (j in 1 : 2)
{
  curYear <- data_year_2020[j]
  curMonth <- data_lastmonth[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_grl_lm_2020[y,] <- dataMade[i,]
        }
    }
  }
}

#United States 2020 lm
data_usa_lm_2020 <- dataMade[0,]

y <- 0

for (j in 1 : 2)
{
  curYear <- data_year_2020[j]
  curMonth <- data_lastmonth[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_usa_lm_2020[y,] <- dataMade[i,]
        }
    }
  }
}
#Bieu do bai 4 nam 2020
ggplot() +
  geom_line(data=data_can_lm_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Canada in two last months in 2020", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_grl_lm_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Greenland in two last months in 2020", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_usa_lm_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in United States in two last months in 2020", x= "Date", y = "New cases") 

#Bieu do bai 5 nam 2020
ggplot() +
  geom_line(data=data_can_lm_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Canada in two last months in 2020", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_lm_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Greenland in two last months in 2020", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_lm_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in United States in two last months in 2020", x= "Date", y = "New deaths") 

#Bieu do bai 6 nam 2020

ggplot() +
  geom_line(data=data_can_lm_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_can_lm_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Canada in two last months in 2020", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_lm_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_grl_lm_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Greenland in two last months in 2020", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_lm_2020, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_usa_lm_2020, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in United States in two last months in 2020", x= "Date", y = "New deaths") 
#Canada 2021 lm
data_can_lm_2021 <- dataMade[0,]

y <- 0

for (j in 1 : 2)
{
  curYear <- data_year_2021[j]
  curMonth <- data_lastmonth[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_can_lm_2021[y,] <- dataMade[i,]
        }
    }
  }
}

#Greenland 2021 lm
data_grl_lm_2021 <- dataMade[0,]

y <- 0

for (j in 1 : 2)
{
  curYear <- data_year_2021[j]
  curMonth <- data_lastmonth[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_grl_lm_2021[y,] <- dataMade[i,]
        }
    }
  }
}

#United States 2021 lm
data_usa_lm_2021 <- dataMade[0,]

y <- 0

for (j in 1 : 2)
{
  curYear <- data_year_2021[j]
  curMonth <- data_lastmonth[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          y <- y + 1
          data_usa_lm_2021[y,] <- dataMade[i,]
        }
    }
  }
}

#Bieu do bai 4 nam 2021

ggplot() +
  geom_line(data=data_can_lm_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "New cases in Canada in two last months in 2021", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_grl_lm_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in Greenland in two last months in 2021", x= "Date", y = "New cases") 

ggplot() +
  geom_line(data=data_usa_lm_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases in United States in two last months in 2021", x= "Date", y = "New cases") 

#Bieu do bai 5 nam 2021

ggplot() +
  geom_line(data=data_can_lm_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "New deaths in Canada in two last months in 2021", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_lm_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in Greenland in two last months in 2021", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_lm_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New deaths in United States in two last months in 2021", x= "Date", y = "New deaths") 


#Bieu do bai 6 nam 2021

ggplot() +
  geom_line(data=data_can_lm_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_can_lm_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Canada in two last months in 2021", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_grl_lm_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_grl_lm_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in Greenland in two last months in 2021", x= "Date", y = "New deaths") 

ggplot() +
  geom_line(data=data_usa_lm_2021, mapping=aes(x=date, y=new_deaths, group = 1), color="red", size = 1.1) +
  geom_line(data=data_usa_lm_2021, mapping=aes(x=date, y=new_cases, group = 1), color="blue", size = 1.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "New cases and new deaths in United States in two last months in 2021", x= "Date", y = "New deaths") 

#7
#Canada 2020
can7_2020_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
    for (i in 1 : x)
    {
      if (dataMade$iso_code[i] == MADEcountry[1])
      {
        k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
            {
            can7_2020_sum[j] <- can7_2020_sum[j] + dataMade$new_cases[i]
            }
      }
    }
}
can7_2020 <- c(1 : length(data_can_2020$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2020$new_cases))
  {
    k <- as.Date(data_can_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_can_2020$new_cases[i]
        y <- y + 1
        can7_2020[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2020$new_cases))
  {
    k <- as.Date(data_can_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        can7_2020[i] <- can7_2020[i] / can7_2020_sum[j]
      }
  }
}
data_can7_2020 <- data.frame(data_can_2020$date, can7_2020)
ggplot(data = data_can7_2020, mapping = aes(x = data_can_2020$date, y = can7_2020)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in Canada in 2020", x= "Date", y = "Ratio") 

#Greenland 2020
grl7_2020_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          grl7_2020_sum[j] <- grl7_2020_sum[j] + dataMade$new_cases[i]
        }
    }
  }
}
grl7_2020 <- c(1 : length(data_grl_2020$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2020$new_cases))
  {
    k <- as.Date(data_grl_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_grl_2020$new_cases[i]
        y <- y + 1
        grl7_2020[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2020$new_cases))
  {
    k <- as.Date(data_grl_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        grl7_2020[i] <- grl7_2020[i] / grl7_2020_sum[j]
      }
  }
}
grl7_2020[which(is.na(grl7_2020))] <- 1
data_grl7_2020 <- data.frame(data_grl_2020$date, grl7_2020)
ggplot(data = data_grl7_2020, mapping = aes(x = data_grl_2020$date, y = grl7_2020)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in Greenland in 2020", x= "Date", y = "Ratio")

#United States 2020
usa7_2020_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          usa7_2020_sum[j] <- usa7_2020_sum[j] + dataMade$new_cases[i]
        }
    }
  }
}
usa7_2020 <- c(1 : length(data_usa_2020$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2020$new_cases))
  {
    k <- as.Date(data_usa_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_usa_2020$new_cases[i]
        y <- y + 1
        usa7_2020[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2020$new_cases))
  {
    k <- as.Date(data_usa_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        usa7_2020[i] <- usa7_2020[i] / usa7_2020_sum[j]
      }
  }
}
data_usa7_2020 <- data.frame(data_usa_2020$date, usa7_2020)
ggplot(data = data_usa7_2020, mapping = aes(x = data_usa_2020$date, y = usa7_2020)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in usaada in 2020", x= "Date", y = "Ratio")

#Canada 2021
can7_2021_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          can7_2021_sum[j] <- can7_2021_sum[j] + dataMade$new_cases[i]
        }
    }
  }
}
can7_2021 <- c(1 : length(data_can_2021$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2021$new_cases))
  {
    k <- as.Date(data_can_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_can_2021$new_cases[i]
        y <- y + 1
        can7_2021[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2021$new_cases))
  {
    k <- as.Date(data_can_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        can7_2021[i] <- can7_2021[i] / can7_2021_sum[j]
      }
  }
}
data_can7_2021 <- data.frame(data_can_2021$date, can7_2021)
ggplot(data = data_can7_2021, mapping = aes(x = data_can_2021$date, y = can7_2021)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in Canada in 2021", x= "Date", y = "Ratio") 

#Greenland 2021
grl7_2021_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          grl7_2021_sum[j] <- grl7_2021_sum[j] + dataMade$new_cases[i]
        }
    }
  }
}
grl7_2021 <- c(1 : length(data_grl_2021$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2021$new_cases))
  {
    k <- as.Date(data_grl_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_grl_2021$new_cases[i]
        y <- y + 1
        grl7_2021[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2021$new_cases))
  {
    k <- as.Date(data_grl_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        grl7_2021[i] <- grl7_2021[i] / grl7_2021_sum[j]
      }
  }
}
grl7_2021[which(is.na(grl7_2021))] <- 1
data_grl7_2021 <- data.frame(data_grl_2021$date, grl7_2021)
ggplot(data = data_grl7_2021, mapping = aes(x = data_grl_2021$date, y = grl7_2021)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in Greenland in 2021", x= "Date", y = "Ratio") 

#United States 2021
usa7_2021_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          usa7_2021_sum[j] <- usa7_2021_sum[j] + dataMade$new_cases[i]
        }
    }
  }
}
usa7_2021 <- c(1 : length(data_usa_2021$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2021$new_cases))
  {
    k <- as.Date(data_usa_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_usa_2021$new_cases[i]
        y <- y + 1
        usa7_2021[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2021$new_cases))
  {
    k <- as.Date(data_usa_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        usa7_2021[i] <- usa7_2021[i] / usa7_2021_sum[j]
      }
  }
}
data_usa7_2021 <- data.frame(data_usa_2021$date, usa7_2021)
ggplot(data = data_usa7_2021, mapping = aes(x = data_usa_2021$date, y = usa7_2021)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in United States in 2021", x= "Date", y = "Ratio") 

#Canada 2022
can7_2022_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          can7_2022_sum[j] <- can7_2022_sum[j] + dataMade$new_cases[i]
        }
    }
  }
}
can7_2022 <- c(1 : length(data_can_2022$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2022$new_cases))
  {
    k <- as.Date(data_can_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_can_2022$new_cases[i]
        y <- y + 1
        can7_2022[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2022$new_cases))
  {
    k <- as.Date(data_can_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        can7_2022[i] <- can7_2022[i] / can7_2022_sum[j]
      }
  }
}
data_can7_2022 <- data.frame(data_can_2022$date, can7_2022)
ggplot(data = data_can7_2022, mapping = aes(x = data_can_2022$date, y = can7_2022)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in Canada in 2022", x= "Date", y = "Ratio") 

#Greenland 2022
grl7_2022_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          grl7_2022_sum[j] <- grl7_2022_sum[j] + dataMade$new_cases[i]
        }
    }
  }
}
grl7_2022 <- c(1 : length(data_grl_2022$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2022$new_cases))
  {
    k <- as.Date(data_grl_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_grl_2022$new_cases[i]
        y <- y + 1
        grl7_2022[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2022$new_cases))
  {
    k <- as.Date(data_grl_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        grl7_2022[i] <- grl7_2022[i] / grl7_2022_sum[j]
      }
  }
}
data_grl7_2022 <- data.frame(data_grl_2022$date, grl7_2022)
ggplot(data = data_grl7_2022, mapping = aes(x = data_grl_2022$date, y = grl7_2022)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in Greenland in 2022", x= "Date", y = "Ratio") 

#United States 2022
usa7_2022_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          usa7_2022_sum[j] <- usa7_2022_sum[j] + dataMade$new_cases[i]
        }
    }
  }
}
usa7_2022 <- c(1 : length(data_usa_2022$new_cases))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2022$new_cases))
  {
    k <- as.Date(data_usa_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_usa_2022$new_cases[i]
        y <- y + 1
        usa7_2022[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2022$new_cases))
  {
    k <- as.Date(data_usa_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        usa7_2022[i] <- usa7_2022[i] / usa7_2022_sum[j]
      }
  }
}
data_usa7_2022 <- data.frame(data_usa_2022$date, usa7_2022)
ggplot(data = data_usa7_2022, mapping = aes(x = data_usa_2022$date, y = usa7_2022)) + geom_line(color = "blue", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in United States in 2022", x= "Date", y = "Ratio") 

#8
#Canada 2020
can8_2020_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          can8_2020_sum[j] <- can8_2020_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
can8_2020 <- c(1 : length(data_can_2020$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2020$new_deaths))
  {
    k <- as.Date(data_can_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_can_2020$new_deaths[i]
        y <- y + 1
        can8_2020[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2020$new_deaths))
  {
    k <- as.Date(data_can_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        can8_2020[i] <- can8_2020[i] / can8_2020_sum[j]
      }
  }
}
can8_2020[which(is.na(can8_2020))] <- 1

data_can8_2020 <- data.frame(data_can_2020$date, can8_2020)
ggplot(data = data_can8_2020, mapping = aes(x = data_can_2020$date, y = can8_2020)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in Canada in 2020", x= "Date", y = "Ratio") 

#Greenland 2020
grl8_2020_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          grl8_2020_sum[j] <- grl8_2020_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
grl8_2020 <- c(1 : length(data_grl_2020$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2020$new_deaths))
  {
    k <- as.Date(data_grl_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_grl_2020$new_deaths[i]
        y <- y + 1
        grl8_2020[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2020$new_deaths))
  {
    k <- as.Date(data_grl_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        grl8_2020[i] <- grl8_2020[i] / grl8_2020_sum[j]
      }
  }
}
grl8_2020[which(is.na(grl8_2020))] <- 1
data_grl8_2020 <- data.frame(data_grl_2020$date, grl8_2020)
ggplot(data = data_grl8_2020, mapping = aes(x = data_grl_2020$date, y = grl8_2020)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in Greenland in 2020", x= "Date", y = "Ratio")

#United States 2020
usa8_2020_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          usa8_2020_sum[j] <- usa8_2020_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
usa8_2020 <- c(1 : length(data_usa_2020$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2020$new_deaths))
  {
    k <- as.Date(data_usa_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_usa_2020$new_deaths[i]
        y <- y + 1
        usa8_2020[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2020[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2020$new_deaths))
  {
    k <- as.Date(data_usa_2020$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        usa8_2020[i] <- usa8_2020[i] / usa8_2020_sum[j]
      }
  }
}
data_usa8_2020 <- data.frame(data_usa_2020$date, usa8_2020)
ggplot(data = data_usa8_2020, mapping = aes(x = data_usa_2020$date, y = usa8_2020)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in United States in 2020", x= "Date", y = "Ratio")

#Canada 2021
can8_2021_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          can8_2021_sum[j] <- can8_2021_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
can8_2021 <- c(1 : length(data_can_2021$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2021$new_deaths))
  {
    k <- as.Date(data_can_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_can_2021$new_deaths[i]
        y <- y + 1
        can8_2021[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2021$new_deaths))
  {
    k <- as.Date(data_can_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        can8_2021[i] <- can8_2021[i] / can8_2021_sum[j]
      }
  }
}
data_can8_2021 <- data.frame(data_can_2021$date, can8_2021)
ggplot(data = data_can8_2021, mapping = aes(x = data_can_2021$date, y = can8_2021)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in Canada in 2021", x= "Date", y = "Ratio") 

#Greenland 2021
grl8_2021_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          grl8_2021_sum[j] <- grl8_2021_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
grl8_2021 <- c(1 : length(data_grl_2021$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2021$new_deaths))
  {
    k <- as.Date(data_grl_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_grl_2021$new_deaths[i]
        y <- y + 1
        grl8_2021[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2021$new_deaths))
  {
    k <- as.Date(data_grl_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        grl8_2021[i] <- grl8_2021[i] / grl8_2021_sum[j]
      }
  }
}
grl8_2021[which(is.na(grl8_2021))] <- 1
data_grl8_2021 <- data.frame(data_grl_2021$date, grl8_2021)
ggplot(data = data_grl8_2021, mapping = aes(x = data_grl_2021$date, y = grl8_2021)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in Greenland in 2021", x= "Date", y = "Ratio") 

#United States 2021
usa8_2021_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          usa8_2021_sum[j] <- usa8_2021_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
usa8_2021 <- c(1 : length(data_usa_2021$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2021$new_deaths))
  {
    k <- as.Date(data_usa_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_usa_2021$new_deaths[i]
        y <- y + 1
        usa8_2021[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2021[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2021$new_deaths))
  {
    k <- as.Date(data_usa_2021$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        usa8_2021[i] <- usa8_2021[i] / usa8_2021_sum[j]
      }
  }
}
data_usa8_2021 <- data.frame(data_usa_2021$date, usa8_2021)
ggplot(data = data_usa8_2021, mapping = aes(x = data_usa_2021$date, y = usa8_2021)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in United States in 2021", x= "Date", y = "Ratio") 

#Canada 2022
can8_2022_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          can8_2022_sum[j] <- can8_2022_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
can8_2022 <- c(1 : length(data_can_2022$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2022$new_deaths))
  {
    k <- as.Date(data_can_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_can_2022$new_deaths[i]
        y <- y + 1
        can8_2022[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_can_2022$new_deaths))
  {
    k <- as.Date(data_can_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        can8_2022[i] <- can8_2022[i] / can8_2022_sum[j]
      }
  }
}
data_can8_2022 <- data.frame(data_can_2022$date, can8_2022)
ggplot(data = data_can8_2022, mapping = aes(x = data_can_2022$date, y = can8_2022)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in Canada in 2022", x= "Date", y = "Ratio") 

#Greenland 2022
grl8_2022_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          grl8_2022_sum[j] <- grl8_2022_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
grl8_2022 <- c(1 : length(data_grl_2022$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2022$new_deaths))
  {
    k <- as.Date(data_grl_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_grl_2022$new_deaths[i]
        y <- y + 1
        grl8_2022[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_grl_2022$new_deaths))
  {
    k <- as.Date(data_grl_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        grl8_2022[i] <- grl8_2022[i] / grl8_2022_sum[j]
      }
  }
}
data_grl8_2022 <- data.frame(data_grl_2022$date, grl8_2022)
ggplot(data = data_grl8_2022, mapping = aes(x = data_grl_2022$date, y = grl8_2022)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in Greenland in 2022", x= "Date", y = "Ratio") 

#United States 2022
usa8_2022_sum <- c(0, 0, 0, 0)
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
        {
          usa8_2022_sum[j] <- usa8_2022_sum[j] + dataMade$new_deaths[i]
        }
    }
  }
}
usa8_2022 <- c(1 : length(data_usa_2022$new_deaths))

y <- 0
sum <- 0
for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2022$new_deaths))
  {
    k <- as.Date(data_usa_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        sum <- sum + data_usa_2022$new_deaths[i]
        y <- y + 1
        usa8_2022[y] <- sum
      }
  }
  sum <- 0
}

for (j in 1 : 4)
{
  curYear <- data_year_2022[j]
  curMonth <- data_month[j]
  for (i in 1 : length(data_usa_2022$new_deaths))
  {
    k <- as.Date(data_usa_2022$date[i], format = "%m/%d/%Y")
    if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        usa8_2022[i] <- usa8_2022[i] / usa8_2022_sum[j]
      }
  }
}
data_usa8_2022 <- data.frame(data_usa_2022$date, usa8_2022)
ggplot(data = data_usa8_2022, mapping = aes(x = data_usa_2022$date, y = usa8_2022)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in United States in 2022", x= "Date", y = "Ratio") 