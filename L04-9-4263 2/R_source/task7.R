library(stringi)
library(ggplot2)
library(httpgd)
library(lubridate)
library(languageserver)
library(ggpubr)
library(dplyr)
setwd("~/R")
dataRaw <- read.csv("owid-covid-data.csv", header = TRUE, sep = ",")
setwd("task7")

x <- length(dataRaw$continent)

MADEcountry <- c("KEN","LSO","MAR","IDN","JPN","VNM","AND","SVN",
                 "GBR", "CAN", "GRL", "USA", "AUS", "NCL", "NZL", "BRA", "CHL", "VEN")
dataMade <- dataRaw[0,]
y <- 0

for (ctry in 1:18) 
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == MADEcountry[ctry])
    {
      y <- y + 1
      dataMade[y,] <- dataRaw[i,]
    }
  }

x <- y

dataMade$new_cases[which(is.na(dataMade$new_cases))] <- 0
dataMade$new_deaths[which(is.na(dataMade$new_deaths))] <- 0

#######################2#########################

countryname <- rep(c("Kenya", "Lesotho", "Morocco", "Indonesia", "Japan", "Vietnam",
                     "Andorra", "Slovenia", "United_Kingdom", "Canada", "Greenland",
                     "United_States", "Australia", "New_Caledonia", "New_Zealand",
                     "Brazil", "Chile", "Venezuela"), times=9)
data_month_year <- rep(c("2/2020", "3/2020", "4/2020", "6/2020", 
                         "2/2021", "3/2021", "4/2021", "6/2021", 
                         "2/2022"), each=18)
data_month <- c(2, 3, 4, 6, 
                2, 3, 4, 6,
                2)
data_year <- c(2020, 2020, 2020, 2020,
               2021, 2021, 2021, 2021,
               2022)
new_deaths_2  <- c(1:162)

d <- 0
for (j in 1 : 9) {
  for (c in 1:18) {
    curYear <- data_year[j]
    curMonth <- data_month[j]
    sum <- 0
    for (i in 1 : x)
    {
      if (dataMade$iso_code[i] == MADEcountry[c])
      {
        k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
            sum <- sum + dataMade$new_deaths[i]
        
      }
    }
    d <- d + 1
    new_deaths_2[d] <- sum
    sum <- 0
  }
}

dataCom2 <- data.frame(data_month_year, countryname, new_deaths_2)
dataCom2$data_month_year = factor(dataCom2$data_month_year,
                                  levels = c("2/2020", "3/2020", "4/2020", "6/2020", 
                                             "2/2021", "3/2021", "4/2021", "6/2021", 
                                             "2/2022"))
dataCom2 <- transform(dataCom2, Group=paste(countryname, new_deaths_2, sep = ""))

ggplot(dataCom2, aes(x = data_month_year, y = new_deaths_2, fill = countryname)) +
  geom_bar(stat= "identity") +
  labs(title = "New deaths in all of countries in months", x= "Date", y = "New deaths") 
#######################1#########################

new_cases_1  <- c(1:162)

d <- 0
for (j in 1 : 9) {
  for (c in 1:18) {
    curYear <- data_year[j]
    curMonth <- data_month[j]
    sum <- 0
    for (i in 1 : x)
    {
      if (dataMade$iso_code[i] == MADEcountry[c])
      {
        k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
            sum <- sum + dataMade$new_cases[i]
        
      }
    }
    d <- d + 1
    new_cases_1[d] <- sum
    sum <- 0
  }
}

dataCom1 <- data.frame(data_month_year, countryname, new_cases_1)
dataCom1$data_month_year = factor(dataCom1$data_month_year,
                                  levels = c("2/2020", "3/2020", "4/2020", "6/2020", 
                                             "2/2021", "3/2021", "4/2021", "6/2021", 
                                             "2/2022"))
dataCom1 <- transform(dataCom1, Group=paste(countryname, new_cases_1, sep = ""))

ggplot(dataCom1, aes(x = data_month_year, y = new_cases_1, fill = countryname)) +
  geom_bar(stat= "identity") +
  labs(title = "New cases in all of countries in months", x= "Date", y = "New cases") 

#######################3#########################

countryname2 <- rep(c("Kenya", "Lesotho", "Morocco", "Indonesia", "Japan", "Vietnam",
                      "Andorra", "Slovenia", "United_Kingdom", "Canada", "Greenland",
                      "United_States", "Australia", "New_Caledonia", "New_Zealand",
                      "Brazil", "Chile", "Venezuela"), times=4)
data_lastmonth_year <- rep(c("11/2020", "12/2020", "11/2021", "12/2021"), each=18)

data_lmonth <- c(11, 12, 11, 12)
data_lyear <- c(2020, 2020, 2021, 2021)


new_cases_lm_3   <- c(1:72)

new_deaths_lm_4  <- c(1:72)

d <- 0
for (j in 1 : 4) 
{
  for (c in 1 : 18) 
  {
    curYear <- data_lyear[j]
    curMonth <- data_lmonth[j]
    sum1 <- 0
    sum2 <- 0
    for (i in 1 : x)
    {
      if (dataMade$iso_code[i] == MADEcountry[c])
      {
        k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
          {
            sum1 <- sum1 + dataMade$new_cases[i]
            sum2 <- sum2 + dataMade$new_deaths[i]
          }
      }
    }
    d <- d + 1
    new_cases_lm_3[d] <- sum1
    new_deaths_lm_4[d] <- sum2
    sum1 <- 0
    sum2 <- 0
  }
}

dataCom3 <- data.frame(data_lastmonth_year, countryname2, new_cases_lm_3)
dataCom3$data_lastmonth_year = factor(dataCom3$data_lastmonth_year,
                                      levels = c("11/2020", "12/2020", "11/2021", "12/2021", "2/2020"))
ggplot(dataCom3, aes(x = data_lastmonth_year, y = new_cases_lm_3, fill = countryname2)) +
  geom_bar(stat= "identity") +
  labs(title = "New cases in all of countries in 2 last months", x= "Date", y = "New cases") 

#######################4#########################

dataCom4 <- data.frame(data_lastmonth_year, countryname2, new_deaths_lm_4)
dataCom4$data_lastmonth_year = factor(dataCom4$data_lastmonth_year,
                                      levels = c("11/2020", "12/2020", "11/2021", "12/2021", "2/2020"))
ggplot(dataCom4, aes(x = data_lastmonth_year, y = new_deaths_lm_4, fill = countryname2)) +
  geom_bar(stat= "identity") +
  labs(title = "New deaths in all of countries in 2 last months", x= "Date", y = "New deaths") 

#######################6#########################

data_day6 <- c(rep(c(1:30), times = 2), 31)
data_month6 <- c(rep(c("11", "12"), each = 30), 12)
data_year6 <- rep(c("2020"), times = 61)
data_year6_2 <- rep(c("2021"), times = 61)

date1 <- seq(as.Date("2020-11-01"), as.Date("2020-12-31"), by="days")
date2 <- seq(as.Date("2021-11-01"), as.Date("2021-12-31"), by="days")

new_death6 <- c(1:61)
y <- 0
sum <- 0
for (j in 1 : 61)
{
  curYear <- data_year6[j]
  curMonth <- data_month6[j]
  curDay <- data_day6[j]
  for (c in 1:18)
  {
    for (i in 1 : x)
    {
      if (dataMade$iso_code[i] == MADEcountry[c])
      {
        k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
            if (day(k) == curDay)
            {
              sum <- sum + dataMade$new_deaths[i]
            }
      }
    }
  }
  y <- y + 1
  new_death6[y] <- sum
}
mat_new_death6 <- c(1:61)
mat_new_death6 <- t(t(new_death6) / new_death6[61])
data6 <- data.frame(date1, mat_new_death6)
ggplot(data = data6, mapping = aes(x = date1, y = mat_new_death6)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in 2 last months in 2020", x= "Date", y = "Ratio") 


new_death6_2 <- c(1:61)
y <- 0
sum <- 0
for (j in 1 : 61)
{
  curYear <- data_year6_2[j]
  curMonth <- data_month6[j]
  curDay <- data_day6[j]
  for (c in 1:18)
  {
    for (i in 1 : x)
    {
      if (dataMade$iso_code[i] == MADEcountry[c])
      {
        k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
            if (day(k) == curDay)
            {
              sum <- sum + dataMade$new_deaths[i]
            }
      }
    }
  }
  y <- y + 1
  new_death6_2[y] <- sum
}
mat_new_death6_2 <- c(1:61)
mat_new_death6_2 <- t(t(new_death6_2) / new_death6_2[61])
data6_2 <- data.frame(date2, mat_new_death6_2)
ggplot(data = data6_2, mapping = aes(x = date1, y = mat_new_death6_2)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new deaths in 2 last months in 2021", x= "Date", y = "Ratio") 

#######################5#########################

new_case5 <- c(1:61)
y <- 0
sum <- 0
for (j in 1 : 61)
{
  curYear <- data_year6[j]
  curMonth <- data_month6[j]
  curDay <- data_day6[j]
  for (c in 1:18)
  {
    for (i in 1 : x)
    {
      if (dataMade$iso_code[i] == MADEcountry[c])
      {
        k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
            if (day(k) == curDay)
            {
              sum <- sum + dataMade$new_cases[i]
            }
      }
    }
  }
  y <- y + 1
  new_case5[y] <- sum
}
mat_new_case5 <- c(1:61)
mat_new_case5 <- t(t(new_case5) / new_case5[61])
data5 <- data.frame(date1, mat_new_case5)
ggplot(data = data5, mapping = aes(x = date1, y = mat_new_case5)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in 2 last months in 2020", x= "Date", y = "Ratio") 


new_case5_2 <- c(1:61)
y <- 0
sum <- 0
for (j in 1 : 61)
{
  curYear <- data_year6_2[j]
  curMonth <- data_month6[j]
  curDay <- data_day6[j]
  for (c in 1:18)
  {
    for (i in 1 : x)
    {
      if (dataMade$iso_code[i] == MADEcountry[c])
      {
        k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
        if (month(k) == curMonth)
          if (year(k) == curYear)
            if (day(k) == curDay)
            {
              sum <- sum + dataMade$new_cases[i]
            }
      }
    }
  }
  y <- y + 1
  new_case5_2[y] <- sum
}
mat_new_case5_2 <- c(1:61)
mat_new_case5_2 <- t(t(new_case5_2) / new_case5_2[61])
data5_2 <- data.frame(date2, mat_new_case5_2)
ggplot(data = data5_2, mapping = aes(x = date1, y = mat_new_case5_2)) + geom_line(color = "red", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Ratio of new cases in 2 last months in 2021", x= "Date", y = "Ratio") 