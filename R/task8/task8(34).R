
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




# (3) (4) 1 thang nhiem benh va chet di toi lai ve cat bui

x <- length(dataRaw$continent)




# (3) (4) 1 thang nhiem benh va chet di toi lai ve cat bui

{
  dayCountHai <- 0          #Đếm ngày trong tháng Hai
  thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangHai_Cacngay <- seq(as.Date("2020-2-1"), as.Date("2020-3-31"), by = "days") #Danh sách các ngày trong 2 tháng Hai
  daysession <- length(thangHai_Cacngay)
  #New cases, thangHai, Year 2020
  curYear <- 2020
  curMonth <- 2
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountHai = dayCountHai + 1
        thangHai[dayCountHai] = dataRaw$new_cases[i]
        if (dayCountHai > 1)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] + thangHai[dayCountHai - 1]
        }
        if (dayCountHai > 7)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] - dataRaw$new_cases[i - 7]
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / 7
        }
        else{
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / dayCountHai
        }
        if (dayCountHai == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangHai_Cacngay) = thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangHai <- data.frame(thangHai_Cacngay, thangHai_7Ngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangHai <- ggplot(NULL, aes(x = thangHai_Cacngay, y = thangHai_7Ngay)) + geom_line(data = new_cases_ThangHai, col = "blueviolet") +
    labs(title = "Corona cases reported in February, March, 2020", x = "Date(2020)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangHai, Year 2020
  dayCountHai <- 0          #Đếm ngày trong tháng Hai
  thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangHai_Cacngay <- seq(as.Date("2020-2-1"), as.Date("2020-3-31"), by = "days")#Danh sách các ngày trong 2 tháng Hai
  daysession <- length(thangHai_Cacngay)
  #Year 2020
  curYear <- 2020
  curMonth <- 2
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountHai = dayCountHai + 1
        thangHai[dayCountHai] = dataRaw$new_deaths[i]
        if (dayCountHai > 1)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] + thangHai[dayCountHai - 1]
        }
        if (dayCountHai > 7)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] - dataRaw$new_deaths[i - 7]
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / 7
        }
        else{
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / dayCountHai
        }
        if (dayCountHai == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangHai_Cacngay) = thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangHai <- data.frame(thangHai_7Ngay, thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangHai <- ggplot(NULL, aes(x = thangHai_Cacngay, y = thangHai_7Ngay)) + geom_line(data = new_deaths_ThangHai, col = "chocolate1") +
    labs(title = "Corona deaths reported in February, March, 2020", x = "Date(2020)", y = "Deaths (Average in last 7 days)")
}

{
  dayCountBa <- 0          #Đếm ngày trong tháng Ba
  thangBa <- {}            #Tổng các dữ liệu từ ngày 1 tháng Ba tới ngày i
  thangBa_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangBa_Cacngay <- {seq(as.Date("2020-3-1"), as.Date("2020-4-30"), by = "days")}#Danh sách các ngày trong tháng Ba
  daysession <- length(thangBa_Cacngay)
  #New cases, thangBa, Year 2020
  curYear <- 2020
  curMonth <- 3
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountBa = dayCountBa + 1
        thangBa[dayCountBa] = dataRaw$new_cases[i]
        if (dayCountBa > 1)
        {
          thangBa[dayCountBa] = thangBa[dayCountBa] + thangBa[dayCountBa - 1]
        }
        if (dayCountBa > 7)
        {
          thangBa[dayCountBa] = thangBa[dayCountBa] - dataRaw$new_cases[i - 7]
          thangBa_7Ngay[dayCountBa] = thangBa[dayCountBa] / 7
        }
        else{
          thangBa_7Ngay[dayCountBa] = thangBa[dayCountBa] / dayCountBa
        }
        if (dayCountBa == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangBa_Cacngay) = thangBa_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangBa <- data.frame(thangBa_7Ngay, thangBa_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangBa <- ggplot(NULL, aes(x = thangBa_Cacngay, y = thangBa_7Ngay)) + geom_line(data = new_cases_ThangBa, col = "blueviolet") +
    labs(title = "Corona cases reported in March, April, 2020", x = "Date(2020)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangBa, Year 2020
  dayCountBa <- 0
  thangBa <- {}
  thangBa_7Ngay <- {}
  thangBa_Cacngay <- seq(as.Date("2020-3-1"), as.Date("2020-4-30"), by = "days")
  daysession <- length(thangBa_Cacngay)
  #Year 2020
  curYear <- 2020
  curMonth <- 3
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountBa = dayCountBa + 1
        thangBa[dayCountBa] = dataRaw$new_deaths[i]
        if (dayCountBa > 1)
        {
          thangBa[dayCountBa] = thangBa[dayCountBa] + thangBa[dayCountBa - 1]
        }
        if (dayCountBa > 7)
        {
          thangBa[dayCountBa] = thangBa[dayCountBa] - dataRaw$new_deaths[i - 7]
          thangBa_7Ngay[dayCountBa] = thangBa[dayCountBa] / 7
        }
        else{
          thangBa_7Ngay[dayCountBa] = thangBa[dayCountBa] / dayCountBa
        }
        if (dayCountBa == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangBa_Cacngay) = thangBa_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangBa <- data.frame(thangBa_7Ngay, thangBa_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangBa <- ggplot(NULL, aes(x = thangBa_Cacngay, y = thangBa_7Ngay)) + geom_line(data = new_deaths_ThangBa, col = "chocolate2") +
    labs(title = "Corona deaths reported in March, 2020", x = "Date(2020)", y = "Deaths (Average in last 7 days)")
}

{
  dayCountTu <- 0          #Đếm ngày trong tháng Tu
  thangTu <- {}            #Tổng các dữ liệu từ ngày 1 tháng Tu tới ngày i
  thangTu_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangTu_Cacngay <- seq(as.Date("2020-4-1"), as.Date("2020-5-31"), by = "days")
  daysession <- length(thangTu_Cacngay)
  #New cases, thangTu, Year 2020
  curYear <- 2020
  curMonth <- 4
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountTu = dayCountTu + 1
        thangTu[dayCountTu] = dataRaw$new_cases[i]
        if (dayCountTu > 1)
        {
          thangTu[dayCountTu] = thangTu[dayCountTu] + thangTu[dayCountTu - 1]
        }
        if (dayCountTu > 7)
        {
          thangTu[dayCountTu] = thangTu[dayCountTu] - dataRaw$new_cases[i - 7]
          thangTu_7Ngay[dayCountTu] = thangTu[dayCountTu] / 7
        }
        else{
          thangTu_7Ngay[dayCountTu] = thangTu[dayCountTu] / dayCountTu
        }
        if (dayCountTu == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangTu_Cacngay) = thangTu_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangTu <- data.frame(thangTu_7Ngay, thangTu_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangTu <- ggplot(NULL, aes(x = thangTu_Cacngay, y = thangTu_7Ngay)) + geom_line(data = new_cases_ThangTu, col = "blueviolet") +
    labs(title = "Corona cases reported in April, May, 2020", x = "Date(2020)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangTu, Year 2020
  dayCountTu <- 0
  thangTu <- {}
  thangTu_7Ngay <- {}
  thangTu_Cacngay <- seq(as.Date("2020-4-1"), as.Date("2020-5-31"), by = "days")
  daysession <- length(thangTu_Cacngay)
  #Year 2020
  curYear <- 2020
  curMonth <- 4
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountTu = dayCountTu + 1
        thangTu[dayCountTu] = dataRaw$new_deaths[i]
        if (dayCountTu > 1)
        {
          thangTu[dayCountTu] = thangTu[dayCountTu] + thangTu[dayCountTu - 1]
        }
        if (dayCountTu > 7)
        {
          thangTu[dayCountTu] = thangTu[dayCountTu] - dataRaw$new_deaths[i - 7]
          thangTu_7Ngay[dayCountTu] = thangTu[dayCountTu] / 7
        }
        else{
          thangTu_7Ngay[dayCountTu] = thangTu[dayCountTu] / dayCountTu
        }
        if (dayCountTu == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangTu_Cacngay) = thangTu_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangTu <- data.frame(thangTu_7Ngay, thangTu_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangTu <- ggplot(NULL, aes(x = thangTu_Cacngay, y = thangTu_7Ngay)) + geom_line(data = new_deaths_ThangTu, col = "chocolate3") +
    labs(title = "Corona deaths reported in April, 2020", x = "Date(2020)", y = "Deaths (Average in last 7 days)")
}

{
  dayCountSau <- 0          #Đếm ngày trong tháng Sau
  thangSau <- {}            #Tổng các dữ liệu từ ngày 1 tháng Sau tới ngày i
  thangSau_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangSau_Cacngay <- seq(as.Date("2020-6-1"), as.Date("2020-7-31"), by = "days")
  daysession <- length(thangSau_Cacngay)
  #New cases, thangSau, Year 2020
  curYear <- 2020
  curMonth <- 6
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountSau = dayCountSau + 1
        thangSau[dayCountSau] = dataRaw$new_cases[i]
        if (dayCountSau > 1)
        {
          thangSau[dayCountSau] = thangSau[dayCountSau] + thangSau[dayCountSau - 1]
        }
        if (dayCountSau > 7)
        {
          thangSau[dayCountSau] = thangSau[dayCountSau] - dataRaw$new_cases[i - 7]
          thangSau_7Ngay[dayCountSau] = thangSau[dayCountSau] / 7
        }
        else{
          thangSau_7Ngay[dayCountSau] = thangSau[dayCountSau] / dayCountSau
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangSau_Cacngay) = thangSau_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangSau <- data.frame(thangSau_7Ngay, thangSau_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangSau <- ggplot(NULL, aes(x = thangSau_Cacngay, y = thangSau_7Ngay)) + geom_line(data = new_cases_ThangSau, col = "blueviolet") +
    labs(title = "Corona cases reported in June, July, 2020", x = "Date(2020)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangSau, Year 2020
  dayCountSau <- 0
  thangSau <- {}
  thangSau_7Ngay <- {}
  thangSau_Cacngay <-seq(as.Date("2020-6-1"), as.Date("2020-7-31"), by = "days")
  daysession <- length(thangSau_Cacngay)
  #Year 2020
  curYear <- 2020
  curMonth <- 6
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountSau = dayCountSau + 1
        thangSau[dayCountSau] = dataRaw$new_deaths[i]
        if (dayCountSau > 1)
        {
          thangSau[dayCountSau] = thangSau[dayCountSau] + thangSau[dayCountSau - 1]
        }
        if (dayCountSau > 7)
        {
          thangSau[dayCountSau] = thangSau[dayCountSau] - dataRaw$new_deaths[i - 7]
          thangSau_7Ngay[dayCountSau] = thangSau[dayCountSau] / 7
        }
        else{
          thangSau_7Ngay[dayCountSau] = thangSau[dayCountSau] / dayCountSau
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangSau_Cacngay) = thangSau_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangSau <- data.frame(thangSau_7Ngay, thangSau_Cacngay)

    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangSau <- ggplot(NULL, aes(x = thangSau_Cacngay, y = thangSau_7Ngay)) + geom_line(data = new_deaths_ThangSau, col = "chocolate") +
    labs(title = "Corona deaths reported in June, 2020", x = "Date(2020)", y = "Deaths (Average in last 7 days)")
    ggsave( filename = "task8_subtask3_case_2020.pdf",
            plot = ggarrange(graph_cases_thangHai, graph_cases_thangBa, graph_cases_thangTu, graph_cases_thangSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)
    ggsave( filename = "task8_subtask4_death_2020.pdf",
            plot = ggarrange(graph_deaths_thangHai, graph_deaths_thangBa, graph_deaths_thangTu, graph_deaths_thangSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)
}


{
  dayCountHai <- 0          #Đếm ngày trong tháng Hai
  thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangHai_Cacngay <- seq(as.Date("2021-2-1"), as.Date("2021-3-31"), by = "days") #Danh sách các ngày trong 2 tháng Hai
  daysession <- length(thangHai_Cacngay)
  #New cases, thangHai, Year 2021
  curYear <- 2021
  curMonth <- 2
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountHai = dayCountHai + 1
        thangHai[dayCountHai] = dataRaw$new_cases[i]
        if (dayCountHai > 1)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] + thangHai[dayCountHai - 1]
        }
        if (dayCountHai > 7)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] - dataRaw$new_cases[i - 7]
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / 7
        }
        else{
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / dayCountHai
        }
        if (dayCountHai == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangHai_Cacngay) = thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangHai <- data.frame(thangHai_Cacngay, thangHai_7Ngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangHai <- ggplot(NULL, aes(x = thangHai_Cacngay, y = thangHai_7Ngay)) + geom_line(data = new_cases_ThangHai, col = "blueviolet") +
    labs(title = "Corona cases reported in February, March, 2021", x = "Date(2021)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangHai, Year 2021
  dayCountHai <- 0          #Đếm ngày trong tháng Hai
  thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangHai_Cacngay <- seq(as.Date("2021-2-1"), as.Date("2021-3-31"), by = "days")#Danh sách các ngày trong 2 tháng Hai
  daysession <- length(thangHai_Cacngay)
  #Year 2021
  curYear <- 2021
  curMonth <- 2
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountHai = dayCountHai + 1
        thangHai[dayCountHai] = dataRaw$new_deaths[i]
        if (dayCountHai > 1)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] + thangHai[dayCountHai - 1]
        }
        if (dayCountHai > 7)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] - dataRaw$new_deaths[i - 7]
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / 7
        }
        else{
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / dayCountHai
        }
        if (dayCountHai == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangHai_Cacngay) = thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangHai <- data.frame(thangHai_7Ngay, thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangHai <- ggplot(NULL, aes(x = thangHai_Cacngay, y = thangHai_7Ngay)) + geom_line(data = new_deaths_ThangHai, col = "chocolate1") +
    labs(title = "Corona deaths reported in February, March, 2021", x = "Date(2021)", y = "Deaths (Average in last 7 days)")
}

{
  dayCountBa <- 0          #Đếm ngày trong tháng Ba
  thangBa <- {}            #Tổng các dữ liệu từ ngày 1 tháng Ba tới ngày i
  thangBa_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangBa_Cacngay <- {seq(as.Date("2021-3-1"), as.Date("2021-4-30"), by = "days")}#Danh sách các ngày trong tháng Ba
  daysession <- length(thangBa_Cacngay)
  #New cases, thangBa, Year 2021
  curYear <- 2021
  curMonth <- 3
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountBa = dayCountBa + 1
        thangBa[dayCountBa] = dataRaw$new_cases[i]
        if (dayCountBa > 1)
        {
          thangBa[dayCountBa] = thangBa[dayCountBa] + thangBa[dayCountBa - 1]
        }
        if (dayCountBa > 7)
        {
          thangBa[dayCountBa] = thangBa[dayCountBa] - dataRaw$new_cases[i - 7]
          thangBa_7Ngay[dayCountBa] = thangBa[dayCountBa] / 7
        }
        else{
          thangBa_7Ngay[dayCountBa] = thangBa[dayCountBa] / dayCountBa
        }
        if (dayCountBa == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangBa_Cacngay) = thangBa_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangBa <- data.frame(thangBa_7Ngay, thangBa_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangBa <- ggplot(NULL, aes(x = thangBa_Cacngay, y = thangBa_7Ngay)) + geom_line(data = new_cases_ThangBa, col = "blueviolet") +
    labs(title = "Corona cases reported in March, April, 2021", x = "Date(2021)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangBa, Year 2021
  dayCountBa <- 0
  thangBa <- {}
  thangBa_7Ngay <- {}
  thangBa_Cacngay <- seq(as.Date("2021-3-1"), as.Date("2021-4-30"), by = "days")
  daysession <- length(thangBa_Cacngay)
  #Year 2021
  curYear <- 2021
  curMonth <- 3
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountBa = dayCountBa + 1
        thangBa[dayCountBa] = dataRaw$new_deaths[i]
        if (dayCountBa > 1)
        {
          thangBa[dayCountBa] = thangBa[dayCountBa] + thangBa[dayCountBa - 1]
        }
        if (dayCountBa > 7)
        {
          thangBa[dayCountBa] = thangBa[dayCountBa] - dataRaw$new_deaths[i - 7]
          thangBa_7Ngay[dayCountBa] = thangBa[dayCountBa] / 7
        }
        else{
          thangBa_7Ngay[dayCountBa] = thangBa[dayCountBa] / dayCountBa
        }
        if (dayCountBa == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangBa_Cacngay) = thangBa_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangBa <- data.frame(thangBa_7Ngay, thangBa_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangBa <- ggplot(NULL, aes(x = thangBa_Cacngay, y = thangBa_7Ngay)) + geom_line(data = new_deaths_ThangBa, col = "chocolate2") +
    labs(title = "Corona deaths reported in March, 2021", x = "Date(2021)", y = "Deaths (Average in last 7 days)")
}

{
  dayCountTu <- 0          #Đếm ngày trong tháng Tu
  thangTu <- {}            #Tổng các dữ liệu từ ngày 1 tháng Tu tới ngày i
  thangTu_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangTu_Cacngay <- seq(as.Date("2021-4-1"), as.Date("2021-5-31"), by = "days")
  daysession <- length(thangTu_Cacngay)
  #New cases, thangTu, Year 2021
  curYear <- 2021
  curMonth <- 4
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountTu = dayCountTu + 1
        thangTu[dayCountTu] = dataRaw$new_cases[i]
        if (dayCountTu > 1)
        {
          thangTu[dayCountTu] = thangTu[dayCountTu] + thangTu[dayCountTu - 1]
        }
        if (dayCountTu > 7)
        {
          thangTu[dayCountTu] = thangTu[dayCountTu] - dataRaw$new_cases[i - 7]
          thangTu_7Ngay[dayCountTu] = thangTu[dayCountTu] / 7
        }
        else{
          thangTu_7Ngay[dayCountTu] = thangTu[dayCountTu] / dayCountTu
        }
        if (dayCountTu == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangTu_Cacngay) = thangTu_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangTu <- data.frame(thangTu_7Ngay, thangTu_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangTu <- ggplot(NULL, aes(x = thangTu_Cacngay, y = thangTu_7Ngay)) + geom_line(data = new_cases_ThangTu, col = "blueviolet") +
    labs(title = "Corona cases reported in April, May, 2021", x = "Date(2021)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangTu, Year 2021
  dayCountTu <- 0
  thangTu <- {}
  thangTu_7Ngay <- {}
  thangTu_Cacngay <- seq(as.Date("2021-4-1"), as.Date("2021-5-31"), by = "days")
  daysession <- length(thangTu_Cacngay)
  #Year 2021
  curYear <- 2021
  curMonth <- 4
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountTu = dayCountTu + 1
        thangTu[dayCountTu] = dataRaw$new_deaths[i]
        if (dayCountTu > 1)
        {
          thangTu[dayCountTu] = thangTu[dayCountTu] + thangTu[dayCountTu - 1]
        }
        if (dayCountTu > 7)
        {
          thangTu[dayCountTu] = thangTu[dayCountTu] - dataRaw$new_deaths[i - 7]
          thangTu_7Ngay[dayCountTu] = thangTu[dayCountTu] / 7
        }
        else{
          thangTu_7Ngay[dayCountTu] = thangTu[dayCountTu] / dayCountTu
        }
        if (dayCountTu == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangTu_Cacngay) = thangTu_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangTu <- data.frame(thangTu_7Ngay, thangTu_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangTu <- ggplot(NULL, aes(x = thangTu_Cacngay, y = thangTu_7Ngay)) + geom_line(data = new_deaths_ThangTu, col = "chocolate3") +
    labs(title = "Corona deaths reported in April, 2021", x = "Date(2021)", y = "Deaths (Average in last 7 days)")
}

{
  dayCountSau <- 0          #Đếm ngày trong tháng Sau
  thangSau <- {}            #Tổng các dữ liệu từ ngày 1 tháng Sau tới ngày i
  thangSau_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangSau_Cacngay <- seq(as.Date("2021-6-1"), as.Date("2021-7-31"), by = "days")
  daysession <- length(thangSau_Cacngay)
  #New cases, thangSau, Year 2021
  curYear <- 2021
  curMonth <- 6
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountSau = dayCountSau + 1
        thangSau[dayCountSau] = dataRaw$new_cases[i]
        if (dayCountSau > 1)
        {
          thangSau[dayCountSau] = thangSau[dayCountSau] + thangSau[dayCountSau - 1]
        }
        if (dayCountSau > 7)
        {
          thangSau[dayCountSau] = thangSau[dayCountSau] - dataRaw$new_cases[i - 7]
          thangSau_7Ngay[dayCountSau] = thangSau[dayCountSau] / 7
        }
        else{
          thangSau_7Ngay[dayCountSau] = thangSau[dayCountSau] / dayCountSau
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangSau_Cacngay) = thangSau_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangSau <- data.frame(thangSau_7Ngay, thangSau_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangSau <- ggplot(NULL, aes(x = thangSau_Cacngay, y = thangSau_7Ngay)) + geom_line(data = new_cases_ThangSau, col = "blueviolet") +
    labs(title = "Corona cases reported in June, July, 2021", x = "Date(2021)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangSau, Year 2021
  dayCountSau <- 0
  thangSau <- {}
  thangSau_7Ngay <- {}
  thangSau_Cacngay <-seq(as.Date("2021-6-1"), as.Date("2021-7-31"), by = "days")
  daysession <- length(thangSau_Cacngay)
  #Year 2021
  curYear <- 2021
  curMonth <- 6
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountSau = dayCountSau + 1
        thangSau[dayCountSau] = dataRaw$new_deaths[i]
        if (dayCountSau > 1)
        {
          thangSau[dayCountSau] = thangSau[dayCountSau] + thangSau[dayCountSau - 1]
        }
        if (dayCountSau > 7)
        {
          thangSau[dayCountSau] = thangSau[dayCountSau] - dataRaw$new_deaths[i - 7]
          thangSau_7Ngay[dayCountSau] = thangSau[dayCountSau] / 7
        }
        else{
          thangSau_7Ngay[dayCountSau] = thangSau[dayCountSau] / dayCountSau
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangSau_Cacngay) = thangSau_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangSau <- data.frame(thangSau_7Ngay, thangSau_Cacngay)

    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangSau <- ggplot(NULL, aes(x = thangSau_Cacngay, y = thangSau_7Ngay)) + geom_line(data = new_deaths_ThangSau, col = "chocolate") +
    labs(title = "Corona deaths reported in June, 2021", x = "Date(2021)", y = "Deaths (Average in last 7 days)")
    ggsave( filename = "task8_subtask3_case_2021.pdf",
            plot = ggarrange(graph_cases_thangHai, graph_cases_thangBa, graph_cases_thangTu, graph_cases_thangSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)
    ggsave( filename = "task8_subtask4_death_2021.pdf",
            plot = ggarrange(graph_deaths_thangHai, graph_deaths_thangBa, graph_deaths_thangTu, graph_deaths_thangSau, nrow = 4, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 25,
            units = c("in"),
            dpi = 300)
}

x <- length(dataRaw$continent)




# (3) (4) 1 thang nhiem benh va chet di toi lai ve cat bui

{
  dayCountHai <- 0          #Đếm ngày trong tháng Hai
  thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangHai_Cacngay <- seq(as.Date("2022-2-1"), as.Date("2022-2-19"), by = "days") #Danh sách các ngày trong 2 tháng Hai
  daysession <- length(thangHai_Cacngay)
  #New cases, thangHai, Year 2022
  curYear <- 2022
  curMonth <- 2
  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountHai = dayCountHai + 1
        thangHai[dayCountHai] = dataRaw$new_cases[i]
        if (dayCountHai > 1)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] + thangHai[dayCountHai - 1]
        }
        if (dayCountHai > 7)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] - dataRaw$new_cases[i - 7]
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / 7
        }
        else{
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / dayCountHai
        }
        if (dayCountHai == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangHai_Cacngay) = thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người nhiễm bệnh
    new_cases_ThangHai <- data.frame(thangHai_Cacngay, thangHai_7Ngay)
    #Tạo biểu đồ lưu vào biến
    graph_cases_thangHai <- ggplot(NULL, aes(x = thangHai_Cacngay, y = thangHai_7Ngay)) + geom_line(data = new_cases_ThangHai, col = "blueviolet") +
    labs(title = "Corona cases reported in February, March, 2022", x = "Date(2022)", y = "Cases (Average in last 7 days)") + theme_light()

  #New deaths, thangHai, Year 2022
  dayCountHai <- 0          #Đếm ngày trong tháng Hai
  thangHai <- {}            #Tổng các dữ liệu từ ngày 1 tháng Hai tới ngày i
  thangHai_7Ngay <- {}      #dữ liệu trung bình trong 7 ngày gần nhất
  thangHai_Cacngay <- seq(as.Date("2022-2-1"), as.Date("2022-2-19"), by = "days")#Danh sách các ngày trong 2 tháng Hai
  daysession <- length(thangHai_Cacngay)
  #Year 2022
  curYear <- 2022
  curMonth <- 2
  

  for (i in 1 : x)
  {
    if (stri_sub(dataRaw$iso_code[i], -3) == "WRL")
    {
      k <- as.Date(dataRaw$date[i], format = "%m/%d/%Y")
      if (month(k) == curMonth || month(k) == curMonth + 1)
      if (year(k) == curYear)
      {
        dayCountHai = dayCountHai + 1
        thangHai[dayCountHai] = dataRaw$new_deaths[i]
        if (dayCountHai > 1)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] + thangHai[dayCountHai - 1]
        }
        if (dayCountHai > 7)
        {
          thangHai[dayCountHai] = thangHai[dayCountHai] - dataRaw$new_deaths[i - 7]
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / 7
        }
        else{
          thangHai_7Ngay[dayCountHai] = thangHai[dayCountHai] / dayCountHai
        }
        if (dayCountHai == daysession)
        {
          break;
        }
      }
    }
  }
    #Lưu mối quan hệ f(thangHai_Cacngay) = thangHai_7Ngay vào vecto với f(x) = y thì trong ngày x có y người c.h.ế.t
    new_deaths_ThangHai <- data.frame(thangHai_7Ngay, thangHai_Cacngay)
    #Tạo biểu đồ lưu vào biến
    graph_deaths_thangHai <- ggplot(NULL, aes(x = thangHai_Cacngay, y = thangHai_7Ngay)) + geom_line(data = new_deaths_ThangHai, col = "chocolate1") +
    labs(title = "Corona deaths reported in February, March, 2022", x = "Date(2022)", y = "Deaths (Average in last 7 days)")
    ggsave( filename = "task8_subtask3_case_2022.pdf",
            plot = ggarrange(graph_cases_thangHai, nrow = 1, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 7,
            units = c("in"),
            dpi = 300)
    ggsave( filename = "task8_subtask4_death_2022.pdf",
            plot = ggarrange(graph_deaths_thangHai, nrow = 1, ncol = 1, common.legend = TRUE),
            scale = 1,
            width = 10,
            height = 7,
            units = c("in"),
            dpi = 300)
}

