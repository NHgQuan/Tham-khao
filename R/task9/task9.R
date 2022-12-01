library(zoo)
library(stringi)
library(ggplot2)
library(lubridate)
library(httpgd)
library(languageserver)
library(ggpubr)

setwd("R")
dataRaw <- read.csv2("owid-covid-data.csv", header = TRUE, sep = ",")
setwd("task9")
#Gừi người đọc code: chỉ cần đọc đoạn xử lý trong một tháng 1 năm là đủ
#MADE 4263


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

#task 9 begin : 1 thang nhiem benh va chet di toi lai ve cat bui
for (ctry in 1 : 3)
{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2020-2-1"), as.Date("2020-2-29"), by = "days") #Danh sách các ngày trong tháng Hai
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2020
  #Khởi gán năm, tháng hiện tại
  curYear <- 2020
  curMonth <- 2
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_cases[i]) == F)
          sum_all <- sum_all + dataMade$new_cases[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_cases[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] = dataMade$new_cases[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tHai <- ggplot(NULL, aes(x = sum_thang_CacNgay, y = sum_thang_MoiNgay)) +
    geom_area(data = new_cases_thang, fill = "#5991fa57", col = "#0059ff") +
    labs(title = paste("Percented of total COVID-19 news by days reported in February, 2020, Country:", countryname[ctry]), x = "Date(2020)", y = "Total proportion (by percentage)") + 
    theme(panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2)) +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 90, label = "Total cases", color = "blue", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 88, ymax = 92, fill = "#5991fa57", col = "blue") +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 78.25, label = "Total deaths", color = "red", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 76.25, ymax = 80.25, fill = "#f5656056", col = "#ff0000") 
    
    temp <- eachDay
    tempAverage <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage[i] = s / i
    }

  #New deaths, sum_thang, Year 2020
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2020-2-1"), as.Date("2020-2-29"), by = "days") #Danh sách các ngày trong tháng Hai
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
  #Khởi gán năm, tháng hiện tại
  curYear <- 2020
  curMonth <- 2
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_deaths[i]) == F)
          sum_all <- sum_all + dataMade$new_deaths[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_deaths[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] = dataMade$new_deaths[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến

    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    geom_area(data = new_deaths_thang, fill = "#f5656056", col = "#ff0000") 

    correlation <- round(cor(eachDay, temp, method = "pearson"), digits = 4)
    corStrength <- "Check"
    if (is.na(correlation) == TRUE)
      corStrength <- "(None)"
    else
    if (abs(correlation) > 0.7)
      corStrength <- "(Strong)"
    else
    if (abs(correlation) > 0.5)
      corStrength <- "(Moderate)"
    else
    if (abs(correlation) > 0.3)
      corStrength <- "(Weak)"
    else
      corStrength <- "(Very Weak)"
    tempAverage2nd <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage2nd[i] = s / i
    }
    correlationAverage <- round(cor(tempAverage2nd, tempAverage, method = "pearson"), digits = 4)
    corAveStrength <- "Check"
    if (is.na(correlationAverage) == TRUE)
      corAveStrength <- "(None)"
    else
    if (correlationAverage > 0.7)
      corAveStrength <- "(Strong)"
    else
    if (correlationAverage > 0.5)
      corAveStrength <- "(Moderate)"
    else
    if (correlationAverage > 0.3)
      corAveStrength <- "(Weak)"
    else
      corAveStrength <- "(Very Weak)"
    if (is.na(correlation) == T)
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 66.25, label = paste("Pearson correlation:", as.character(correlation), corStrength), color = "#000000", size = 6)
    if (is.na(correlationAverage) == T)
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 54, label = paste("P.C 7day-average:", as.character(correlationAverage), corAveStrength), color = "#000000", size = 6)

  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Ba tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2020-3-1"), as.Date("2020-3-31"), by = "days") #Danh sách các ngày trong tháng Ba
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2020
  #Khởi gán năm, tháng hiện tại
  curYear <- 2020
  curMonth <- 3
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_cases[i]) == F)
          sum_all <- sum_all + dataMade$new_cases[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_cases[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] = dataMade$new_cases[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }

    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tBa <- ggplot(NULL, aes(x = sum_thang_CacNgay, y = sum_thang_MoiNgay)) +
    geom_area(data = new_cases_thang, fill = "#5991fa57", col = "#0059ff") +
    labs(title = paste("Percented of total COVID-19 news by days reported in March, 2020, Country:", countryname[ctry]), x = "Date(2020)", y = "Total proportion (by percentage)") + 
    theme(panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2)) +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 90, label = "Total cases", color = "blue", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 88, ymax = 92, fill = "#5991fa57", col = "blue") +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 78.25, label = "Total deaths", color = "red", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 76.25, ymax = 80.25, fill = "#f5656056", col = "#ff0000") 

    temp <- eachDay
    tempAverage <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage[i] = s / i
    }

  #New deaths, sum_thang, Year 2020
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Ba tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2020-3-1"), as.Date("2020-3-31"), by = "days") #Danh sách các ngày trong tháng Ba
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2020
  #Khởi gán năm, tháng hiện tại
  curYear <- 2020
  curMonth <- 3
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_deaths[i]) == F)
          sum_all <- sum_all + dataMade$new_deaths[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_deaths[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] = dataMade$new_deaths[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    geom_area(data = new_deaths_thang, fill = "#f5656056", col = "#ff0000")

    correlation <- round(cor(eachDay, temp, method = "pearson"), digits = 4)
    corStrength <- "Check"
    if (is.na(correlation) == TRUE)
      corStrength <- "(None)"
    else
    if (abs(correlation) > 0.7)
      corStrength <- "(Strong)"
    else
    if (abs(correlation) > 0.5)
      corStrength <- "(Moderate)"
    else
    if (abs(correlation) > 0.3)
      corStrength <- "(Weak)"
    else
      corStrength <- "(Very Weak)"
    tempAverage2nd <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage2nd[i] = s / i
    }
    correlationAverage <- round(cor(tempAverage2nd, tempAverage, method = "pearson"), digits = 4)
    corAveStrength <- "Check"
    if (is.na(correlationAverage) == TRUE)
      corAveStrength <- "(None)"
    else
    if (correlationAverage > 0.7)
      corAveStrength <- "(Strong)"
    else
    if (correlationAverage > 0.5)
      corAveStrength <- "(Moderate)"
    else
    if (correlationAverage > 0.3)
      corAveStrength <- "(Weak)"
    else
      corAveStrength <- "(Very Weak)"
    if (is.na(correlation) == T)
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 66.25, label = paste("Pearson correlation:", as.character(correlation), corStrength), color = "#000000", size = 6)
    if (is.na(correlationAverage) == T)
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 54, label = paste("P.C 7day-average:", as.character(correlationAverage), corAveStrength), color = "#000000", size = 6)

#
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Tu tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2020-4-1"), as.Date("2020-4-30"), by = "days") #Danh sách các ngày trong tháng Tu
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2020
  #Khởi gán năm, tháng hiện tại
  curYear <- 2020
  curMonth <- 4
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_cases[i]) == F)
          sum_all <- sum_all + dataMade$new_cases[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_cases[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] = dataMade$new_cases[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }

    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tTu <- ggplot(NULL, aes(x = sum_thang_CacNgay, y = sum_thang_MoiNgay)) +
    geom_area(data = new_cases_thang, fill = "#5991fa57", col = "#0059ff") +
    labs(title = paste("Percented of total COVID-19 news by days reported in April, 2020, Country:", countryname[ctry]), x = "Date(2020)", y = "Total proportion (by percentage)") + 
    theme(panel.grid = element_line(color = "#8ccde3", size = 0.75,linetype = 2)) +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 90, label = "Total cases", color = "blue", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 88, ymax = 92, fill = "#5991fa57", col = "blue") +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 78.25, label = "Total deaths", color = "red", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 76.25, ymax = 80.25, fill = "#f5656056", col = "#ff0000") 

    temp <- eachDay
    tempAverage <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage[i] = s / i
    }

  #New deaths, sum_thang, Year 2020
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Tu tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2020-4-1"), as.Date("2020-4-30"), by = "days") #Danh sách các ngày trong tháng Tu
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2020
  #Khởi gán năm, tháng hiện tại
  curYear <- 2020
  curMonth <- 4
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_deaths[i]) == F)
          sum_all <- sum_all + dataMade$new_deaths[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_deaths[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] = dataMade$new_deaths[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    geom_area(data = new_deaths_thang, fill = "#f5656056", col = "#ff0000")

    correlation <- round(cor(eachDay, temp, method = "pearson"), digits = 4)
    corStrength <- "Check"
    if (is.na(correlation) == TRUE)
      corStrength <- "(None)"
    else
    if (abs(correlation) > 0.7)
      corStrength <- "(Strong)"
    else
    if (abs(correlation) > 0.5)
      corStrength <- "(Moderate)"
    else
    if (abs(correlation) > 0.3)
      corStrength <- "(Weak)"
    else
      corStrength <- "(Very Weak)"
    tempAverage2nd <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage2nd[i] = s / i
    }
    correlationAverage <- round(cor(tempAverage2nd, tempAverage, method = "pearson"), digits = 4)
    corAveStrength <- "Check"
    if (is.na(correlationAverage) == TRUE)
      corAveStrength <- "(None)"
    else
    if (correlationAverage > 0.7)
      corAveStrength <- "(Strong)"
    else
    if (correlationAverage > 0.5)
      corAveStrength <- "(Moderate)"
    else
    if (correlationAverage > 0.3)
      corAveStrength <- "(Weak)"
    else
      corAveStrength <- "(Very Weak)"
    if (is.na(correlation) == T)
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 66.25, label = paste("Pearson correlation:", as.character(correlation), corStrength), color = "#000000", size = 6)
    if (is.na(correlationAverage) == T)
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 54, label = paste("P.C 7day-average:", as.character(correlationAverage), corAveStrength), color = "#000000", size = 6)

#Thang Sau
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Sau tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2020-6-1"), as.Date("2020-6-30"), by = "days") #Danh sách các ngày trong tháng Sau
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2020
  #Khởi gán năm, tháng hiện tại
  curYear <- 2020
  curMonth <- 6
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_cases[i]) == F)
          sum_all <- sum_all + dataMade$new_cases[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_cases[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] = dataMade$new_cases[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }

    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tSau <- ggplot(NULL, aes(x = sum_thang_CacNgay, y = sum_thang_MoiNgay)) +
    geom_area(data = new_cases_thang, fill = "#5991fa57", col = "#0059ff") +
    labs(title = paste("Percented of total COVID-19 cases by days reported in June, 2020, Country:", countryname[ctry]), x = "Date(2020)", y = "Total proportion (by percentage)") + 
    theme(panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2)) +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 90, label = "Total cases", color = "blue", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 88, ymax = 92, fill = "#5991fa57", col = "blue") +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 78.25, label = "Total deaths", color = "red", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 76.25, ymax = 80.25, fill = "#f5656056", col = "#ff0000") 
    
    temp <- eachDay
    tempAverage <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage[i] = s / i
    }

  #New deaths, sum_thang, Year 2020
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Sau tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2020-6-1"), as.Date("2020-6-30"), by = "days") #Danh sách các ngày trong tháng Sau
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2020
  #Khởi gán năm, tháng hiện tại
  curYear <- 2020
  curMonth <- 6
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_deaths[i]) == F)
          sum_all <- sum_all + dataMade$new_deaths[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_deaths[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] = dataMade$new_deaths[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    geom_area(data = new_deaths_thang, fill = "#f5656056", col = "#ff0000")

    correlation <- round(cor(eachDay, temp, method = "pearson"), digits = 4)
    corStrength <- "Check"
    if (is.na(correlation) == TRUE)
      corStrength <- "(None)"
    else
    if (abs(correlation) > 0.7)
      corStrength <- "(Strong)"
    else
    if (abs(correlation) > 0.5)
      corStrength <- "(Moderate)"
    else
    if (abs(correlation) > 0.3)
      corStrength <- "(Weak)"
    else
      corStrength <- "(Very Weak)"
    tempAverage2nd <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage2nd[i] = s / i
    }
    correlationAverage <- round(cor(tempAverage2nd, tempAverage, method = "pearson"), digits = 4)
    corAveStrength <- "Check"
    if (is.na(correlationAverage) == TRUE)
      corAveStrength <- "(None)"
    else
    if (correlationAverage > 0.7)
      corAveStrength <- "(Strong)"
    else
    if (correlationAverage > 0.5)
      corAveStrength <- "(Moderate)"
    else
    if (correlationAverage > 0.3)
      corAveStrength <- "(Weak)"
    else
      corAveStrength <- "(Very Weak)"
    if (is.na(correlation) == T)
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)

    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 66.25, label = paste("Pearson correlation:", as.character(correlation), corStrength), color = "#000000", size = 6)
    if (is.na(correlationAverage) == T)
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 54, label = paste("P.C 7day-average:", as.character(correlationAverage), corAveStrength), color = "#000000", size = 6)

    graph_name <- paste("task9", countryname[ctry], "news_2020.pdf", sep = "_")
#Tạo file lưu các biểu đồ
    ggsave( filename =  graph_name,
            plot = ggarrange(graph_cases_sum_tHai, graph_cases_sum_tBa, graph_cases_sum_tTu, graph_cases_sum_tSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)
}

#task 9 begin : 1 thang nhiem benh va chet di toi lai ve cat bui
for (ctry in 1 : 3)
{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2021-2-1"), as.Date("2021-2-28"), by = "days") #Danh sách các ngày trong tháng Hai
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2021
  #Khởi gán năm, tháng hiện tại
  curYear <- 2021
  curMonth <- 2
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_cases[i]) == F)
          sum_all <- sum_all + dataMade$new_cases[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_cases[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] = dataMade$new_cases[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tHai <- ggplot(NULL, aes(x = sum_thang_CacNgay, y = sum_thang_MoiNgay)) +
    geom_area(data = new_cases_thang, fill = "#5991fa57", col = "#0059ff") +
    labs(title = paste("Percented of total COVID-19 news by days reported in February, 2021, Country:", countryname[ctry]), x = "Date(2021)", y = "Total proportion (by percentage)") + 
    theme(panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2)) +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 90, label = "Total cases", color = "blue", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 88, ymax = 92, fill = "#5991fa57", col = "blue") +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 78.25, label = "Total deaths", color = "red", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 76.25, ymax = 80.25, fill = "#f5656056", col = "#ff0000")
    
    temp <- eachDay
    tempAverage <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage[i] = s / i
    }

  #New deaths, sum_thang, Year 2021
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2021-2-1"), as.Date("2021-2-28"), by = "days") #Danh sách các ngày trong tháng Hai
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
  #Khởi gán năm, tháng hiện tại
  curYear <- 2021
  curMonth <- 2
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_deaths[i]) == F)
          sum_all <- sum_all + dataMade$new_deaths[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_deaths[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] = dataMade$new_deaths[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    geom_area(data = new_deaths_thang, fill = "#f5656056", col = "#ff0000")
  
    correlation <- round(cor(eachDay, temp, method = "pearson"), digits = 4)
    corStrength <- "Check"
    if (is.na(correlation) == TRUE)
      corStrength <- "(None)"
    else
    if (abs(correlation) > 0.7)
      corStrength <- "(Strong)"
    else
    if (abs(correlation) > 0.5)
      corStrength <- "(Moderate)"
    else
    if (abs(correlation) > 0.3)
      corStrength <- "(Weak)"
    else
      corStrength <- "(Very Weak)"
    tempAverage2nd <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage2nd[i] = s / i
    }
    correlationAverage <- round(cor(tempAverage2nd, tempAverage, method = "pearson"), digits = 4)
    corAveStrength <- "Check"
    if (is.na(correlationAverage) == TRUE)
      corAveStrength <- "(None)"
    else
    if (correlationAverage > 0.7)
      corAveStrength <- "(Strong)"
    else
    if (correlationAverage > 0.5)
      corAveStrength <- "(Moderate)"
    else
    if (correlationAverage > 0.3)
      corAveStrength <- "(Weak)"
    else
      corAveStrength <- "(Very Weak)"
    if (is.na(correlation) == T)
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 66.25, label = paste("Pearson correlation:", as.character(correlation), corStrength), color = "#000000", size = 6)
    if (is.na(correlationAverage) == T)
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tHai <- graph_cases_sum_tHai +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 54, label = paste("P.C 7day-average:", as.character(correlationAverage), corAveStrength), color = "#000000", size = 6)
#
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Ba tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2021-3-1"), as.Date("2021-3-31"), by = "days") #Danh sách các ngày trong tháng Ba
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2021
  #Khởi gán năm, tháng hiện tại
  curYear <- 2021
  curMonth <- 3
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_cases[i]) == F)
          sum_all <- sum_all + dataMade$new_cases[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_cases[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] = dataMade$new_cases[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }

    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tBa <- ggplot(NULL, aes(x = sum_thang_CacNgay, y = sum_thang_MoiNgay)) +
    geom_area(data = new_cases_thang, fill = "#5991fa57", col = "#0059ff") +
    labs(title = paste("Percented of total COVID-19 news by days reported in March, 2021, Country:", countryname[ctry]), x = "Date(2021)", y = "Total proportion (by percentage)") + 
    theme(panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2)) +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 90, label = "Total cases", color = "blue", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 88, ymax = 92, fill = "#5991fa57", col = "blue") +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 78.25, label = "Total deaths", color = "red", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 76.25, ymax = 80.25, fill = "#f5656056", col = "#ff0000") 
    
    temp <- eachDay
    tempAverage <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage[i] = s / i
    }

  #New deaths, sum_thang, Year 2021
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Ba tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2021-3-1"), as.Date("2021-3-31"), by = "days") #Danh sách các ngày trong tháng Ba
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2021
  #Khởi gán năm, tháng hiện tại
  curYear <- 2021
  curMonth <- 3
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_deaths[i]) == F)
          sum_all <- sum_all + dataMade$new_deaths[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_deaths[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] = dataMade$new_deaths[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    geom_area(data = new_deaths_thang, fill = "#f5656056", col = "#ff0000")

    correlation <- round(cor(eachDay, temp, method = "pearson"), digits = 4)
    corStrength <- "Check"
    if (is.na(correlation) == TRUE)
      corStrength <- "(None)"
    else
    if (abs(correlation) > 0.7)
      corStrength <- "(Strong)"
    else
    if (abs(correlation) > 0.5)
      corStrength <- "(Moderate)"
    else
    if (abs(correlation) > 0.3)
      corStrength <- "(Weak)"
    else
      corStrength <- "(Very Weak)"
    tempAverage2nd <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage2nd[i] = s / i
    }
    correlationAverage <- round(cor(tempAverage2nd, tempAverage, method = "pearson"), digits = 4)
    corAveStrength <- "Check"
    if (is.na(correlationAverage) == TRUE)
      corAveStrength <- "(None)"
    else
    if (correlationAverage > 0.7)
      corAveStrength <- "(Strong)"
    else
    if (correlationAverage > 0.5)
      corAveStrength <- "(Moderate)"
    else
    if (correlationAverage > 0.3)
      corAveStrength <- "(Weak)"
    else
      corAveStrength <- "(Very Weak)"
    if (is.na(correlation) == T)
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 66.25, label = paste("Pearson correlation:", as.character(correlation), corStrength), color = "#000000", size = 6)
    if (is.na(correlationAverage) == T)
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tBa <- graph_cases_sum_tBa +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 54, label = paste("P.C 7day-average:", as.character(correlationAverage), corAveStrength), color = "#000000", size = 6)

#
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Tu tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2021-4-1"), as.Date("2021-4-30"), by = "days") #Danh sách các ngày trong tháng Tu
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2021
  #Khởi gán năm, tháng hiện tại
  curYear <- 2021
  curMonth <- 4
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_cases[i]) == F)
          sum_all <- sum_all + dataMade$new_cases[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_cases[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] = dataMade$new_cases[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }

    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tTu <- ggplot(NULL, aes(x = sum_thang_CacNgay, y = sum_thang_MoiNgay)) +
    geom_area(data = new_cases_thang, fill = "#5991fa57", col = "#0059ff") +
    labs(title = paste("Percented of total COVID-19 news by days reported in April, 2021, Country:", countryname[ctry]), x = "Date(2021)", y = "Total proportion (by percentage)") + 
    theme(panel.grid = element_line(color = "#8ccde3", size = 0.75,linetype = 2)) +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 90, label = "Total cases", color = "blue", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 88, ymax = 92, fill = "#5991fa57", col = "blue") +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 78.25, label = "Total deaths", color = "red", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 76.25, ymax = 80.25, fill = "#f5656056", col = "#ff0000") 
    
    temp <- eachDay
    tempAverage <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage[i] = s / i
    }

  #New deaths, sum_thang, Year 2021
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Tu tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2021-4-1"), as.Date("2021-4-30"), by = "days") #Danh sách các ngày trong tháng Tu
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2021
  #Khởi gán năm, tháng hiện tại
  curYear <- 2021
  curMonth <- 4
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_deaths[i]) == F)
          sum_all <- sum_all + dataMade$new_deaths[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_deaths[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] = dataMade$new_deaths[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    geom_area(data = new_deaths_thang, fill = "#f5656056", col = "#ff0000")

    correlation <- round(cor(eachDay, temp, method = "pearson"), digits = 4)
    corStrength <- "Check"
    if (is.na(correlation) == TRUE)
      corStrength <- "(None)"
    else
    if (abs(correlation) > 0.7)
      corStrength <- "(Strong)"
    else
    if (abs(correlation) > 0.5)
      corStrength <- "(Moderate)"
    else
    if (abs(correlation) > 0.3)
      corStrength <- "(Weak)"
    else
      corStrength <- "(Very Weak)"
    tempAverage2nd <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage2nd[i] = s / i
    }
    correlationAverage <- round(cor(tempAverage2nd, tempAverage, method = "pearson"), digits = 4)
    corAveStrength <- "Check"
    if (is.na(correlationAverage) == TRUE)
      corAveStrength <- "(None)"
    else
    if (correlationAverage > 0.7)
      corAveStrength <- "(Strong)"
    else
    if (correlationAverage > 0.5)
      corAveStrength <- "(Moderate)"
    else
    if (correlationAverage > 0.3)
      corAveStrength <- "(Weak)"
    else
      corAveStrength <- "(Very Weak)"
    if (is.na(correlation) == T)
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 66.25, label = paste("Pearson correlation:", as.character(correlation), corStrength), color = "#000000", size = 6)
    if (is.na(correlationAverage) == T)
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tTu <- graph_cases_sum_tTu +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 54, label = paste("P.C 7day-average:", as.character(correlationAverage), corAveStrength), color = "#000000", size = 6)

#Thang Sau
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Sau tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2021-6-1"), as.Date("2021-6-30"), by = "days") #Danh sách các ngày trong tháng Sau
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2021
  #Khởi gán năm, tháng hiện tại
  curYear <- 2021
  curMonth <- 6
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_cases[i]) == F)
          sum_all <- sum_all + dataMade$new_cases[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_cases[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_cases[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_cases[i]
            sum[dayCount] = dataMade$new_cases[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }

    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tSau <- ggplot(NULL, aes(x = sum_thang_CacNgay, y = sum_thang_MoiNgay)) +
    geom_area(data = new_cases_thang, fill = "#5991fa57", col = "#0059ff") +
    labs(title = paste("Percented of total COVID-19 cases by days reported in June, 2021, Country:", countryname[ctry]), x = "Date(2021)", y = "Total proportion (by percentage)") + 
    theme(panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2)) +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 90, label = "Total cases", color = "blue", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 88, ymax = 92, fill = "#5991fa57", col = "blue") +
    annotate(geom = "text", x = sum_thang_CacNgay[5], y = 78.25, label = "Total deaths", color = "red", size = 6) +
    annotate("rect", xmin = sum_thang_CacNgay[1], xmax = sum_thang_CacNgay[2], ymin = 76.25, ymax = 80.25, fill = "#f5656056", col = "#ff0000") 

    temp <- eachDay
    tempAverage <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage[i] = s / i
    }

  #New deaths, sum_thang, Year 2021
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thang <- {}            #Tổng các dữ liệu từ ngày 1 tháng Sau tới ngày i
  sum_thang_MoiNgay <- {}      #Tỉ lệ phần trăm của tổng từ ngày 1 tới ngày i và tổng số ca cả tháng
  sum_thang_CacNgay <- seq(as.Date("2021-6-1"), as.Date("2021-6-30"), by = "days") #Danh sách các ngày trong tháng Sau
  r <- length(sum_thang_CacNgay)
  for (i in 1 : r)
  {
    sum_thang_MoiNgay[i] = 0; #Khởi gán tránh trường hợp không có dữ liệu
  }
#New cases, sum_thang, Year 2021
  #Khởi gán năm, tháng hiện tại
  curYear <- 2021
  curMonth <- 6
  sum <- {} #Mảng lưu tổng số ca mỗi ngày
  eachDay <- {} #Mảng lưu số ca mỗi ngày (phục vụ xét tương quan)
  for (i in 1 : r)
  {
    sum[i] = 0; eachDay[i] = 0 #Khởi gán tránh trường hợp không có dữ liệu
  }
  sum_all <- 0
  for (i in 1 : x)
  {
    if (dataMade$iso_code[i] == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
        if (year(k) == curYear)
          if (is.na(dataMade$new_deaths[i]) == F)
          sum_all <- sum_all + dataMade$new_deaths[i]
    }
  }
  for (i in 1 : x)
  {
    if (stri_sub(dataMade$iso_code[i], -3) == MADEcountry[ctry])
    {
      k <- as.Date(dataMade$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] <- sum[dayCount - 1] + dataMade$new_deaths[i]
          }
        }
        else
        {
          if (is.na(dataMade$new_deaths[i]) == F)
          {
            eachDay[dayCount] = dataMade$new_deaths[i]
            sum[dayCount] = dataMade$new_deaths[i]
          }
        }

        #Cập nhật tỉ lệ phần trăm hiện tại so với tổng trong tháng
          if (is.na(sum[dayCount]) == F && sum_all !=0)
          sum_thang_MoiNgay[dayCount] = sum[dayCount] * 100 / sum_all
      }
    }
  }
    #Lưu mối quan hệ f(sum_thang_CacNgay) = sum_thang_MoiNgay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_thang <- data.frame(sum_thang_MoiNgay, sum_thang_CacNgay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    geom_area(data = new_deaths_thang, fill = "#f5656056", col = "#ff0000")

    correlation <- round(cor(eachDay, temp, method = "pearson"), digits = 4)
    corStrength <- "Check"
    if (is.na(correlation) == TRUE)
      corStrength <- "(None)"
    else
    if (abs(correlation) > 0.7)
      corStrength <- "(Strong)"
    else
    if (abs(correlation) > 0.5)
      corStrength <- "(Moderate)"
    else
    if (abs(correlation) > 0.3)
      corStrength <- "(Weak)"
    else
      corStrength <- "(Very Weak)"
    tempAverage2nd <- rollmean(eachDay, k = 7, fill = 0)
    s <- 0
    for (i in 1:7)
    {
      s = s + eachDay[i]
      tempAverage2nd[i] = s / i
    }
    correlationAverage <- round(cor(tempAverage2nd, tempAverage, method = "pearson"), digits = 4)
    corAveStrength <- "Check"
    if (is.na(correlationAverage) == TRUE)
      corAveStrength <- "(None)"
    else
    if (correlationAverage > 0.7)
      corAveStrength <- "(Strong)"
    else
    if (correlationAverage > 0.5)
      corAveStrength <- "(Moderate)"
    else
    if (correlationAverage > 0.3)
      corAveStrength <- "(Weak)"
    else
      corAveStrength <- "(Very Weak)"
    if (is.na(correlation) == T)
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 66.25, label = paste("Pearson correlation:", as.character(correlation), corStrength), color = "#000000", size = 6)
    if (is.na(correlationAverage) == T)
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    annotate(geom = "text", x = sum_thang_CacNgay[15], y = 94, label = "404 not found :(", color = "#5c5c5c", size = 6)
    graph_cases_sum_tSau <- graph_cases_sum_tSau +
    annotate(geom = "text", x = sum_thang_CacNgay[7], y = 54, label = paste("P.C 7day-average:", as.character(correlationAverage), corAveStrength), color = "#000000", size = 6)

    graph_name <- paste("task9", countryname[ctry], "news_2021.pdf", sep = "_")
#Tạo file lưu các biểu đồ
    ggsave( filename =  graph_name,
            plot = ggarrange(graph_cases_sum_tHai, graph_cases_sum_tBa, graph_cases_sum_tTu, graph_cases_sum_tSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)
}