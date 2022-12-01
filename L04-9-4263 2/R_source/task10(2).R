library(stringi)
library(ggplot2)
library(httpgd)
library(lubridate)
library(languageserver)
library(ggpubr)
library(dplyr)
setwd("~/R")
dataRaw <- read.csv("owid-covid-data.csv", header = TRUE, sep = ",")
setwd("task10")

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

dataMade$new_cases[which(is.na(dataMade$new_cases))] <- 0
dataMade$new_deaths[which(is.na(dataMade$new_deaths))] <- 0

data_date <- c("13/02/2022", "14/02/2022", "15/02/2022", "16/02/2022", "17/02/2022", "18/02/2022", "19/02/2022")
data_month <- rep(c(2), times = 7)
data_year <- rep(c(2022), times = 7)
data_day <- (13:19)
new_deaths_can  <- c(1:7)
new_deaths_grl  <- c(1:7)
new_deaths_usa  <- c(1:7)

x <- length(dataMade$continent)


for (j in 1 : 7)
{
  curYear <- data_year[j]
  curMonth <- data_month[j]
  curDay <- data_day[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[1])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (day(k) == curDay)
            {
                new_deaths_can[j] <- dataMade$new_deaths[i];
            }
    }
  }
}

for (j in 1 : 7)
{
  curYear <- data_year[j]
  curMonth <- data_month[j]
  curDay <- data_day[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[2])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (day(k) == curDay)
          {
            new_deaths_grl[j] <- dataMade$new_deaths[i];
          }
    }
  }
}

for (j in 1 : 7)
{
  curYear <- data_year[j]
  curMonth <- data_month[j]
  curDay <- data_day[j]
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[3])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (day(k) == curDay)
          {
            new_deaths_usa[j] <- dataMade$new_deaths[i];
          }
    }
  }
}

DATA_2 = data.frame(data_date, new_deaths_can, new_deaths_grl, new_deaths_usa)
  












