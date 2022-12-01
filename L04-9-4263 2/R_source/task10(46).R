library(zoo)
library(stringi)
library(ggplot2)
library(lubridate)
library(httpgd)
library(languageserver)
library(ggpubr)


setwd("R")
dataRaw <- read.csv2("owid-covid-data.csv", header = TRUE, sep = ",")
setwd("task10")
write("", file = "DateofOutbreaks.csv", append = FALSE)


x <- length(dataRaw$continent) #số dữ liệu trong file gốc

countryname <- c("Canada", "Greenland", "United_States")
MADEcountry <- c("CAN", "GRL", "USA")
dataMade <- dataRaw[0,]
y <- 0 #số dữ liệu theo Made

#lọc dữ liệu từ file gốc thành các dữ liệu thuộc MADE
  for (ctry in 1 : 3)
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == MADEcountry[ctry])
    {
      y <- y + 1
      dataMade[y,] <- dataRaw[i,]
    }
  }
x <- y

# Ta định nghĩa sự bùng phát dịch bệnh COVID-19 là khi: 
# trong vòng số ngày lớn hơn 7, khi số ca trung bình trong 7 ngày tiếp theo của ngày đó tăng lên so với ngày đó hơn 10 lần

BungPhat <- {}
cntBungPhat <- 0
for (ctry in 1 : 3)
{
  write(MADEcountry[ctry], file = "DateofOutbreaks.csv", append = TRUE)
  i <- 1
  while (i < x-21)
  {
        nexti <- i
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry] && stri_sub(dataMade$iso_code[i+21], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (is.na(dataMade$new_cases[i]) == F && is.na(dataMade$new_cases[i+21]) == F)
      if (dataMade$new_cases[i+21] > dataMade$new_cases[i] * 24 / 10 && dataMade$new_cases[i+21] > 50000)
      {
        cntBungPhat = cntBungPhat + 1
        BungPhat[cntBungPhat] = k
        write(paste(year(k),month(k),day(k), sep = "-"), file = "DateofOutbreaks.csv", append = TRUE)
        
        i <- nexti + 45
      }
    }
    i <- i + 1
  }
}