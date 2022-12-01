# Tính các ký tự 1 2 3 4 từ thực tế
kytu1 <- 4
kytu2 <- 2
kytu3 <- 6
kytu4 <- 3
kq <- (kytu1 + kytu2 + kytu3 + kytu4) %% 6
stt <- c(11, 12, 13)


bigTable <- (read.csv("owid-covid-data.csv", header = TRUE)) # bigTable là bảng thô chưa được chỉnh sửa

tableNoNull <- subset (bigTable, continent != "")

# truy cập vào từng phần tử của mảng bằng $
# công việc: sau khi đọc R phải liệt ra các nước Canada,  Greenland và United States thôi
ourGrouptable <- subset(bigTable, location == "Canada" | location == "Greenland" |
    location == "United States") # tách Canada, loction và Greenland ra theo đề bài

allDate <- bigTable$ date # tìm ngày tháng
date <- table$ date

save.image("workspace/preTask.RData")