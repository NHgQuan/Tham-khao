
library(stringi)
library(ggplot2)
library(lubridate)
library(httpgd)
library(languageserver)
library(ggpubr)


setwd("R")
dataRaw <- read.csv2("owid-covid-data.csv", header = TRUE, sep = ",")
setwd("task8")
#MADE 4263
#Gừi người đọc code: chỉ cần đọc đoạn xử lý trong một tháng 1 năm là đủ

x <- length(dataRaw$continent)



# (1) (2) 1 thang nhiem benh va chet di toi lai ve cat bui
{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  sum_thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangHai_Cacngay <- seq(as.Date("2020-2-1"), as.Date("2020-3-31"), by = "days")#Danh sách các ngày trong tháng Hai
  #New cases, sum_thangHai, Year 2020
  curYear <- 2020
  curMonth <- 2
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangHai[dayCount] = sum_thangHai[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangHai[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangHai[dayCount] = sum_thangHai[dayCount] - sum[dayCount - 7]
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / 7
        }
        else
        {
          sum_thangHai_7Ngay[dayCount] <- sum_thangHai[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangHai_Cacngay) = sum_thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangHai <- data.frame(sum_thangHai_7Ngay, sum_thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangHai <- ggplot(NULL, aes(x = sum_thangHai_Cacngay, y = sum_thangHai_7Ngay)) +
    geom_area(data = new_cases_ThangHai, fill = "#59a4fa6b", col = "#0059ff") +
    labs(title = "Total COVID-19 cases reported in February, March, 2020", x = "Date(2020)", y = "Cases (Average in last 7 days)") + 
  theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

  #New deaths, sum_thangHai, Year 2020
  dayCount <- 0
  sum_thangHai <- {}
  sum_thangHai_7Ngay <- {}
  sum_thangHai_Cacngay <- seq(as.Date("2020-2-1"), as.Date("2020-3-31"), by = "days")
  #Year 2020
  curYear <- 2020
  curMonth <- 2
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangHai[dayCount] = sum_thangHai[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangHai[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangHai[dayCount] = sum_thangHai[dayCount] - sum[dayCount - 7]
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / 7
        }
        else
        {
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangHai_Cacngay) = sum_thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangHai <- data.frame(sum_thangHai_7Ngay, sum_thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangHai <- ggplot(NULL, aes(x = sum_thangHai_Cacngay, y = sum_thangHai_7Ngay)) +
    geom_area(data = new_deaths_ThangHai, fill = "#59a4fa6b", col = "#0059ff") +
    labs(title = "Total COVID-19 deaths reported in February, March, 2020", x = "Date(2020)", y = "Deaths (Average in last 7 days)") + 
  theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
}

{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangBa <- {}            #Tổng các dữ liệu từ ngày 1 tháng Ba tới ngày i
  sum_thangBa_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangBa_Cacngay <- seq(as.Date("2020-3-1"), as.Date("2020-4-30"), by = "days")#Danh sách các ngày trong tháng Ba
  #New cases, sum_thangBa, Year 2020
  curYear <- 2020
  curMonth <- 3
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangBa[dayCount] = sum_thangBa[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangBa[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangBa[dayCount] = sum_thangBa[dayCount] - sum[dayCount - 7]
          sum_thangBa_7Ngay[dayCount] = sum_thangBa[dayCount] / 7
        }
        else
        {
          sum_thangBa_7Ngay[dayCount] <- sum_thangBa[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangBa_Cacngay) = sum_thangBa_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangBa <- data.frame(sum_thangBa_7Ngay, sum_thangBa_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangBa <- ggplot(NULL, aes(x = sum_thangBa_Cacngay, y = sum_thangBa_7Ngay)) +
    geom_area(data = new_cases_ThangBa, fill = "#dcf38a6b", col = "yellow") +
    labs(title = "Total COVID-19 cases reported in March, April, 2020", x = "Date(2020)", y = "Cases (Average in last 7 days)") + 
  theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

  #New deaths, sum_thangBa, Year 2020
  dayCount <- 0
  sum_thangBa <- {}
  sum_thangBa_7Ngay <- {}
  sum_thangBa_Cacngay <- seq(as.Date("2020-3-1"), as.Date("2020-4-30"), by = "days")
  #Year 2020
  curYear <- 2020
  curMonth <- 3
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangBa[dayCount] = sum_thangBa[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangBa[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangBa[dayCount] = sum_thangBa[dayCount] - sum[dayCount - 7]
          sum_thangBa_7Ngay[dayCount] = sum_thangBa[dayCount] / 7
        }
        else
        {
          sum_thangBa_7Ngay[dayCount] = sum_thangBa[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangBa_Cacngay) = sum_thangBa_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangBa <- data.frame(sum_thangBa_7Ngay, sum_thangBa_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangBa <- ggplot(NULL, aes(x = sum_thangBa_Cacngay, y = sum_thangBa_7Ngay)) +
    geom_area(data = new_deaths_ThangBa, fill = "#dcf38a6b", col = "yellow") +
    labs(title = "Total COVID-19 deaths reported in March, 2020", x = "Date(2020)", y = "Deaths (Average in last 7 days)") +
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
}

{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangTu <- {}            #Tổng các dữ liệu từ ngày 1 tháng Tu tới ngày i
  sum_thangTu_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangTu_Cacngay <- seq(as.Date("2020-4-1"), as.Date("2020-5-31"), by = "days")#Danh sách các ngày trong tháng Tu
  #New cases, sum_thangTu, Year 2020
  curYear <- 2020
  curMonth <- 4
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangTu[dayCount] = sum_thangTu[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangTu[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangTu[dayCount] = sum_thangTu[dayCount] - sum[dayCount - 7]
          sum_thangTu_7Ngay[dayCount] = sum_thangTu[dayCount] / 7
        }
        else
        {
          sum_thangTu_7Ngay[dayCount] <- sum_thangTu[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangTu_Cacngay) = sum_thangTu_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangTu <- data.frame(sum_thangTu_7Ngay, sum_thangTu_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangTu <- ggplot(NULL, aes(x = sum_thangTu_Cacngay, y = sum_thangTu_7Ngay)) +
    geom_area(data = new_cases_ThangTu, fill = "#f78b8b59", col = "red") +
    labs(title = "Total COVID-19 cases reported in April, May, 2020", x = "Date(2020)", y = "Cases (Average in last 7 days)") + 
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

  #New deaths, sum_thangTu, Year 2020
  dayCount <- 0
  sum_thangTu <- {}
  sum_thangTu_7Ngay <- {}
  sum_thangTu_Cacngay <- seq(as.Date("2020-4-1"), as.Date("2020-5-31"), by = "days")
  #Year 2020
  curYear <- 2020
  curMonth <- 4
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangTu[dayCount] = sum_thangTu[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangTu[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangTu[dayCount] = sum_thangTu[dayCount] - sum[dayCount - 7]
          sum_thangTu_7Ngay[dayCount] = sum_thangTu[dayCount] / 7
        }
        else
        {
          sum_thangTu_7Ngay[dayCount] = sum_thangTu[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangTu_Cacngay) = sum_thangTu_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangTu <- data.frame(sum_thangTu_7Ngay, sum_thangTu_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangTu <- ggplot(NULL, aes(x = sum_thangTu_Cacngay, y = sum_thangTu_7Ngay)) +
    geom_area(data = new_deaths_ThangTu, fill = "#f78b8b59", col = "red") +
    labs(title = "Total COVID-19 deaths reported in April, 2020", x = "Date(2020)", y = "Deaths (Average in last 7 days)") +
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

}

{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangSau <- {}            #Tổng các dữ liệu từ ngày 1 tháng Sau tới ngày i
  sum_thangSau_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangSau_Cacngay <- seq(as.Date("2020-6-1"), as.Date("2020-7-31"), by = "days")#Danh sách các ngày trong tháng Sau
  #New cases, sum_thangSau, Year 2020
  curYear <- 2020
  curMonth <- 6
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangSau[dayCount] = sum_thangSau[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangSau[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangSau[dayCount] = sum_thangSau[dayCount] - sum[dayCount - 7]
          sum_thangSau_7Ngay[dayCount] = sum_thangSau[dayCount] / 7
        }
        else
        {
          sum_thangSau_7Ngay[dayCount] <- sum_thangSau[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangSau_Cacngay) = sum_thangSau_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangSau <- data.frame(sum_thangSau_7Ngay, sum_thangSau_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangSau <- ggplot(NULL, aes(x = sum_thangSau_Cacngay, y = sum_thangSau_7Ngay)) +
    geom_area(data = new_cases_ThangSau, fill = "#a85fec62", col = "blueviolet") +
    labs(title = "Total COVID-19 cases reported in June, July, 2020", x = "Date(2020)", y = "Cases (Average in last 7 days)") + 
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
  #New deaths, sum_thangSau, Year 2020
  dayCount <- 0
  sum_thangSau <- {}
  sum_thangSau_7Ngay <- {}
  sum_thangSau_Cacngay <- seq(as.Date("2020-6-1"), as.Date("2020-7-31"), by = "days")
  #Year 2020
  curYear <- 2020
  curMonth <- 6
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangSau[dayCount] = sum_thangSau[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangSau[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangSau[dayCount] = sum_thangSau[dayCount] - sum[dayCount - 7]
          sum_thangSau_7Ngay[dayCount] = sum_thangSau[dayCount] / 7
        }
        else
        {
          sum_thangSau_7Ngay[dayCount] = sum_thangSau[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangSau_Cacngay) = sum_thangSau_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangSau <- data.frame(sum_thangSau_7Ngay, sum_thangSau_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangSau <- ggplot(NULL, aes(x = sum_thangSau_Cacngay, y = sum_thangSau_7Ngay))+
    geom_area(data = new_deaths_ThangSau, fill = "#a85fec62", col = "blueviolet") +
    labs(title = "Total COVID-19 deaths reported in June, 2020", x = "Date(2020)", y = "Deaths (Average in last 7 days)") +
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
}
    ggsave( filename = "task8_subtask5_case_2020.pdf",
            plot = ggarrange(graph_cases_sum_thangHai, graph_cases_sum_thangBa, graph_cases_sum_thangTu, graph_cases_sum_thangSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)
    ggsave( filename = "task8_subtask6_death_2020.pdf",
            plot = ggarrange(graph_deaths_sum_thangHai, graph_deaths_sum_thangBa, graph_deaths_sum_thangTu, graph_deaths_sum_thangSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)

{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  sum_thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangHai_Cacngay <- seq(as.Date("2021-2-1"), as.Date("2021-3-31"), by = "days")#Danh sách các ngày trong tháng Hai
  #New cases, sum_thangHai, Year 2021
  curYear <- 2021
  curMonth <- 2
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangHai[dayCount] = sum_thangHai[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangHai[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangHai[dayCount] = sum_thangHai[dayCount] - sum[dayCount - 7]
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / 7
        }
        else
        {
          sum_thangHai_7Ngay[dayCount] <- sum_thangHai[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangHai_Cacngay) = sum_thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangHai <- data.frame(sum_thangHai_7Ngay, sum_thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangHai <- ggplot(NULL, aes(x = sum_thangHai_Cacngay, y = sum_thangHai_7Ngay)) +
    geom_area(data = new_cases_ThangHai, fill = "#59a4fa6b", col = "#0059ff") +
    labs(title = "Total COVID-19 cases reported in February, March, 2021", x = "Date(2021)", y = "Cases (Average in last 7 days)") + 
  theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

  #New deaths, sum_thangHai, Year 2021
  dayCount <- 0
  sum_thangHai <- {}
  sum_thangHai_7Ngay <- {}
  sum_thangHai_Cacngay <- seq(as.Date("2021-2-1"), as.Date("2021-3-31"), by = "days")
  #Year 2021
  curYear <- 2021
  curMonth <- 2
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangHai[dayCount] = sum_thangHai[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangHai[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangHai[dayCount] = sum_thangHai[dayCount] - sum[dayCount - 7]
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / 7
        }
        else
        {
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangHai_Cacngay) = sum_thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangHai <- data.frame(sum_thangHai_7Ngay, sum_thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangHai <- ggplot(NULL, aes(x = sum_thangHai_Cacngay, y = sum_thangHai_7Ngay)) +
    geom_area(data = new_deaths_ThangHai, fill = "#59a4fa6b", col = "#0059ff") +
    labs(title = "Total COVID-19 deaths reported in February, March, 2021", x = "Date(2021)", y = "Deaths (Average in last 7 days)") + 
  theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
}

{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangBa <- {}            #Tổng các dữ liệu từ ngày 1 tháng Ba tới ngày i
  sum_thangBa_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangBa_Cacngay <- seq(as.Date("2021-3-1"), as.Date("2021-4-30"), by = "days")#Danh sách các ngày trong tháng Ba
  #New cases, sum_thangBa, Year 2021
  curYear <- 2021
  curMonth <- 3
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangBa[dayCount] = sum_thangBa[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangBa[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangBa[dayCount] = sum_thangBa[dayCount] - sum[dayCount - 7]
          sum_thangBa_7Ngay[dayCount] = sum_thangBa[dayCount] / 7
        }
        else
        {
          sum_thangBa_7Ngay[dayCount] <- sum_thangBa[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangBa_Cacngay) = sum_thangBa_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangBa <- data.frame(sum_thangBa_7Ngay, sum_thangBa_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangBa <- ggplot(NULL, aes(x = sum_thangBa_Cacngay, y = sum_thangBa_7Ngay)) +
    geom_area(data = new_cases_ThangBa, fill = "#dcf38a6b", col = "yellow") +
    labs(title = "Total COVID-19 cases reported in March, April, 2021", x = "Date(2021)", y = "Cases (Average in last 7 days)") +
  theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

  #New deaths, sum_thangBa, Year 2021
  dayCount <- 0
  sum_thangBa <- {}
  sum_thangBa_7Ngay <- {}
  sum_thangBa_Cacngay <- seq(as.Date("2021-3-1"), as.Date("2021-4-30"), by = "days")
  #Year 2021
  curYear <- 2021
  curMonth <- 3
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangBa[dayCount] = sum_thangBa[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangBa[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangBa[dayCount] = sum_thangBa[dayCount] - sum[dayCount - 7]
          sum_thangBa_7Ngay[dayCount] = sum_thangBa[dayCount] / 7
        }
        else
        {
          sum_thangBa_7Ngay[dayCount] = sum_thangBa[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangBa_Cacngay) = sum_thangBa_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangBa <- data.frame(sum_thangBa_7Ngay, sum_thangBa_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangBa <- ggplot(NULL, aes(x = sum_thangBa_Cacngay, y = sum_thangBa_7Ngay)) +
    geom_area(data = new_deaths_ThangBa, fill = "#dcf38a6b", col = "yellow") +
    labs(title = "Total COVID-19 deaths reported in March, 2021", x = "Date(2021)", y = "Deaths (Average in last 7 days)") +
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
}

{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangTu <- {}            #Tổng các dữ liệu từ ngày 1 tháng Tu tới ngày i
  sum_thangTu_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangTu_Cacngay <- seq(as.Date("2021-4-1"), as.Date("2021-5-31"), by = "days")#Danh sách các ngày trong tháng Tu
  #New cases, sum_thangTu, Year 2021
  curYear <- 2021
  curMonth <- 4
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangTu[dayCount] = sum_thangTu[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangTu[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangTu[dayCount] = sum_thangTu[dayCount] - sum[dayCount - 7]
          sum_thangTu_7Ngay[dayCount] = sum_thangTu[dayCount] / 7
        }
        else
        {
          sum_thangTu_7Ngay[dayCount] <- sum_thangTu[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangTu_Cacngay) = sum_thangTu_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangTu <- data.frame(sum_thangTu_7Ngay, sum_thangTu_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangTu <- ggplot(NULL, aes(x = sum_thangTu_Cacngay, y = sum_thangTu_7Ngay)) +
    geom_area(data = new_cases_ThangTu, fill = "#f78b8b59", col = "red") +
    labs(title = "Total COVID-19 cases reported in April, May, 2021", x = "Date(2021)", y = "Cases (Average in last 7 days)") + 
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

  #New deaths, sum_thangTu, Year 2021
  dayCount <- 0
  sum_thangTu <- {}
  sum_thangTu_7Ngay <- {}
  sum_thangTu_Cacngay <- seq(as.Date("2021-4-1"), as.Date("2021-5-31"), by = "days")
  #Year 2021
  curYear <- 2021
  curMonth <- 4
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangTu[dayCount] = sum_thangTu[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangTu[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangTu[dayCount] = sum_thangTu[dayCount] - sum[dayCount - 7]
          sum_thangTu_7Ngay[dayCount] = sum_thangTu[dayCount] / 7
        }
        else
        {
          sum_thangTu_7Ngay[dayCount] = sum_thangTu[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangTu_Cacngay) = sum_thangTu_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangTu <- data.frame(sum_thangTu_7Ngay, sum_thangTu_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangTu <- ggplot(NULL, aes(x = sum_thangTu_Cacngay, y = sum_thangTu_7Ngay)) +
    geom_area(data = new_deaths_ThangTu, fill = "#f78b8b59", col = "red") +
    labs(title = "Total COVID-19 deaths reported in April, 2021", x = "Date(2021)", y = "Deaths (Average in last 7 days)") +
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

}

{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangSau <- {}            #Tổng các dữ liệu từ ngày 1 tháng Sau tới ngày i
  sum_thangSau_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangSau_Cacngay <- seq(as.Date("2021-6-1"), as.Date("2021-7-31"), by = "days")#Danh sách các ngày trong tháng Sau
  #New cases, sum_thangSau, Year 2021
  curYear <- 2021
  curMonth <- 6
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangSau[dayCount] = sum_thangSau[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangSau[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangSau[dayCount] = sum_thangSau[dayCount] - sum[dayCount - 7]
          sum_thangSau_7Ngay[dayCount] = sum_thangSau[dayCount] / 7
        }
        else
        {
          sum_thangSau_7Ngay[dayCount] <- sum_thangSau[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangSau_Cacngay) = sum_thangSau_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangSau <- data.frame(sum_thangSau_7Ngay, sum_thangSau_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangSau <- ggplot(NULL, aes(x = sum_thangSau_Cacngay, y = sum_thangSau_7Ngay)) +
    geom_area(data = new_cases_ThangSau, fill = "#a85fec62", col = "blueviolet") +
    labs(title = "Total COVID-19 cases reported in June, July, 2021", x = "Date(2021)", y = "Cases (Average in last 7 days)") + 
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
  #New deaths, sum_thangSau, Year 2021
  dayCount <- 0
  sum_thangSau <- {}
  sum_thangSau_7Ngay <- {}
  sum_thangSau_Cacngay <- seq(as.Date("2021-6-1"), as.Date("2021-7-31"), by = "days")
  #Year 2021
  curYear <- 2021
  curMonth <- 6
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangSau[dayCount] = sum_thangSau[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangSau[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangSau[dayCount] = sum_thangSau[dayCount] - sum[dayCount - 7]
          sum_thangSau_7Ngay[dayCount] = sum_thangSau[dayCount] / 7
        }
        else
        {
          sum_thangSau_7Ngay[dayCount] = sum_thangSau[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangSau_Cacngay) = sum_thangSau_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangSau <- data.frame(sum_thangSau_7Ngay, sum_thangSau_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangSau <- ggplot(NULL, aes(x = sum_thangSau_Cacngay, y = sum_thangSau_7Ngay))+
    geom_area(data = new_deaths_ThangSau, fill = "#a85fec62", col = "blueviolet") +
    labs(title = "Total COVID-19 deaths reported in June, 2021", x = "Date(2021)", y = "Deaths (Average in last 7 days)") +
    theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
}
    ggsave( filename = "task8_subtask5_case_2021.pdf",
            plot = ggarrange(graph_cases_sum_thangHai, graph_cases_sum_thangBa, graph_cases_sum_thangTu, graph_cases_sum_thangSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)
    ggsave( filename = "task8_subtask6_death_2021.pdf",
            plot = ggarrange(graph_deaths_sum_thangHai, graph_deaths_sum_thangBa, graph_deaths_sum_thangTu, graph_deaths_sum_thangSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)


{
  dayCount <- 0                 #Đếm ngày trong tháng
  sum_thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  sum_thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  sum_thangHai_Cacngay <- seq(as.Date("2022-2-1"), as.Date("2022-2-19"), by = "days")#Danh sách các ngày trong tháng Hai
  #New cases, sum_thangHai, Year 2022
  curYear <- 2022
  curMonth <- 2
  sum <- 0
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_cases[i]
          sum_thangHai[dayCount] = sum_thangHai[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_cases[i]
          sum_thangHai[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangHai[dayCount] = sum_thangHai[dayCount] - sum[dayCount - 7]
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / 7
        }
        else
        {
          sum_thangHai_7Ngay[dayCount] <- sum_thangHai[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangHai_Cacngay) = sum_thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangHai <- data.frame(sum_thangHai_7Ngay, sum_thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_sum_thangHai <- ggplot(NULL, aes(x = sum_thangHai_Cacngay, y = sum_thangHai_7Ngay)) +
    geom_area(data = new_cases_ThangHai, fill = "#59a4fa6b", col = "#0059ff") +
    labs(title = "Total COVID-19 cases reported in February, March, 2022", x = "Date(2022)", y = "Cases (Average in last 7 days)") + 
  theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))

  #New deaths, sum_thangHai, Year 2022
  dayCount <- 0
  sum_thangHai <- {}
  sum_thangHai_7Ngay <- {}
  sum_thangHai_Cacngay <- seq(as.Date("2022-2-1"), as.Date("2022-2-19"), by = "days")
  #Year 2022
  curYear <- 2022
  curMonth <- 2
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    if (dataRaw$new_deaths[i] < 0)
    {
      dataRaw$new_deaths[i] = -dataRaw$new_deaths[i]
    }
  }

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCount = dayCount + 1

        #Cập nhật tổng tích lũy
        if (dayCount > 1)
        {
          sum[dayCount] <- sum[dayCount - 1] + dataRaw$new_deaths[i]
          sum_thangHai[dayCount] = sum_thangHai[dayCount - 1] + sum[dayCount]
        }
        else
        {
          sum[dayCount] = dataRaw$new_deaths[i]
          sum_thangHai[dayCount] = sum[dayCount]
        }

        #Cập nhật giá trị trung bình tích lũy trong 7 ngày
        if (dayCount > 7)
        {
          sum_thangHai[dayCount] = sum_thangHai[dayCount] - sum[dayCount - 7]
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / 7
        }
        else
        {
          sum_thangHai_7Ngay[dayCount] = sum_thangHai[dayCount] / dayCount
        }
      }
    }
  }
    #Lưu mối quan hệ f(sum_thangHai_Cacngay) = sum_thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangHai <- data.frame(sum_thangHai_7Ngay, sum_thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_sum_thangHai <- ggplot(NULL, aes(x = sum_thangHai_Cacngay, y = sum_thangHai_7Ngay), ) +
    geom_area(data = new_deaths_ThangHai, fill = "#59a4fa6b", col = "#0059ff") +
    labs(title = "Total COVID-19 deaths reported in February, March, 2022", x = "Date(2022)", y = "Deaths (Average in last 7 days)") + 
  theme(panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2))
}
    ggsave( filename = "task8_subtask5_case_2022.pdf",
            plot = ggarrange(graph_cases_sum_thangHai, nrow = 1, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 7,
            units = c("in"),
            dpi = 300)
    ggsave( filename = "task8_subtask6_death_2022.pdf",
            plot = ggarrange(graph_deaths_sum_thangHai, nrow = 1, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 7,
            units = c("in"),
            dpi = 300)
