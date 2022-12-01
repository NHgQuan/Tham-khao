library(readr)
library(ggplot2)
library(stringr)
library(dplyr)


load("workspace/preTask.RData")

setwd ('task1')


wrt <- function (str) { #để dễ đàng sử dụng lại nhiều lần
    write (str,file = "task1.txt",append=TRUE)
}

# i1
#các năm được thu thập lưu trong biến years
#Dùng hàm str_sub để trích xuất các phần tử cuối
years <- bigTable $ date
for (i in 1 : length(years)) {
    years[i] <- str_sub (years[i], start = -4)
}
years <- unique (years)
#write(paste ("Các năm thu thập dữ liệu:"), file = "task1.txt")
write(years, file = "1.1 Các năm thu thập dữ liệu.txt")


# tính số lượng đất nước
numLoca <- length(unique(bigTable$ location))
write(numLoca,file = "1.2 Số lượng đất nước.txt")


# i2
# Bảng hiển thị 10 đất nước đầu tiên
countries <- unique(bigTable[, c(1, 3)])
top10Countries <- head(countries, n = 10)
write.csv(top10Countries, file = "2 danh sách 10 nước đầu tiên.csv")

# i3
# tìm số lượng châu lục trong tập mẫu
numConti <- length(unique(tableNoNull$ continent))
write (numConti , file = "3 Số lượng châu lục.txt")
#wrt (numConti)

# i45
# tính số lượng dữ liệu thu thập được trong từng châu lục và tổng số
# ở đây, tôi giả sử "dữ liệu" đề bài nói đến chính là số hàng
continents <- unique(tableNoNull$ continent)
values <- c()

for (string in continents) {
    values <- append(values, nrow(subset(tableNoNull, continent == string)))
}
continents <- append(continents, "Sum")
values <- append(values, sum(values)) # cần tính tổng nữa
nValuesOfContinents <- data.frame(continents, values) # bước hoàn thiện bảng cần tính
write.csv(nValuesOfContinents, file ="4 số lượng dữ liệu thu thập được trong từng châu lục.csv")

# i5 làm hoàn toàn tương tự như i4
locations <- unique(bigTable$ location)
values <- c()

for (string in locations) {
    values <- append(values, nrow(subset(bigTable, location == string)))
}
locations <- append(locations, "Sum")
values <- append(values, sum(values)) # cần tính tổng nữa
nValuesOfLoca <- data.frame(locations, values)
write.csv(nValuesOfLoca, file ="5.1 số lượng dữ liệu thu thập được trong từng đất nước.csv")



# hiển thị 10 nước cuối, sử dụng hàm tail(), ngược lại dùng hàm head()
write.csv (tail(nValuesOfLoca, n = 11), file = "5.2 Số lượng dữ liệu thu thập được từng nước và tổng số (thể hiện 10 nước cuối cùng).csv")

# i6789
#print("Châu lục có số lượng dữ liệu thu thập nhỏ nhất")
minConti <- (subset(
    nValuesOfContinents,
    values == min(nValuesOfContinents$ values)
))
write (unlist(minConti), file = "6 Châu lục có số lượng dữ liệu thu thập nhỏ nhất.txt")
#wrt (unlist(minConti))

#print("Châu lục có số lượng dữ liệu thu thập lớn nhất")
maxConti <- (subset(
    nValuesOfContinents,
    values == max(nValuesOfContinents[-nrow(nValuesOfContinents), ]$ values)
)) #- nrow để loại đi cái cuối cùng là sum
write (unlist (maxConti), file = "7 Châu lục có số lượng dữ liệu thu thập lớn nhất.txt")
#wrt (unlist (maxConti))

#print("Nước có số lượng dữ liệu thu thập nhỏ nhất")
minLoca <- (subset(
    nValuesOfLoca,
    values == min(nValuesOfLoca$ values)
))
write (unlist(minLoca), "8 Nước có số lượng dữ liệu thu thập nhỏ nhất.txt")

#print("Nước có số lượng dữ liệu thu thập lớn nhất")
maxLoca <- (subset(
    nValuesOfLoca,
    values == max(nValuesOfLoca[-nrow(nValuesOfLoca), ]$ values)
))
write (unlist(maxLoca), file = "9 Nước có số lượng dữ liệu thu thập lớn nhất.txt")

# 10 & 11
# phải lập bảng dữ liệu thu thập được theo từng date
# như trên thôi
dates <- unique(bigTable$ date)
values <- c()
for (string in dates) {
    values <- append(values, nrow(subset(bigTable, date == string)))
}
nValuesOfDates <- data.frame(dates, values)

#print("Ngày có số lượng dữ liệu thu thập nhỏ nhất")
minDate <- (subset(
    nValuesOfDates,
    values == min(nValuesOfDates$ values)
))
write.csv ((minDate), file="10 Ngày có số lượng dữ liệu thu thập nhỏ nhất.csv")

#print("Ngày có số lượng dữ liệu thu thập lớn nhất")
maxDate <- (subset(
    nValuesOfDates,
    values == max(nValuesOfDates$ values)
))
write.csv ((maxDate), file="11 Ngày có số lượng dữ liệu thu thập lớn nhất.csv")



# i12 13 14
# Bây giờ là theo cả Date và cả Châu Lục
contiAndDate <- unique(tableNoNull[, c(2, 4)])
values <- c()
for (i in (1:nrow(contiAndDate))) {
    values <- append(values, nrow(subset(
        tableNoNull,
        date == (contiAndDate[i, ])$ date &
            continent == (contiAndDate[i, ])$ continent
    )))
}
nValuesOfContiAndDate <- data.frame(contiAndDate, values)
write.csv (nValuesOfContiAndDate, "12 Số lượng dữ liệu thu thập được theo ngày và châu lục.csv")

#print("số lượng dữ liệu thu thập lớn nhất theo ngày và Châu Lục")
maxDateConti <- (subset(
    nValuesOfContiAndDate,
    values == max(nValuesOfContiAndDate$ values)
))
write.csv ((maxDateConti) ,file = "13 số lượng dữ liệu thu thập lớn nhất theo ngày và Châu Lục.csv")

#print("số lượng dữ liệu thu thập nhỏ nhất theo ngày và Châu Lục")
minDateConti <- (subset(
    nValuesOfContiAndDate,
    values == min(nValuesOfContiAndDate$ values)
))
write.csv ((minDateConti) ,file = "14 số lượng dữ liệu thu thập nhỏ nhất theo ngày và Châu Lục.csv")


# i15
# Với một date là k và châu lục t cho trước, hãy cho biết số lượng dữ liệu thu thập được.
# function
findValuesDC <- function(k = "", t = "") {
    return (subset(
        nValuesOfContiAndDate,
        continent == k &
            date == t
    )$ values)
}
# findValuesDC ("Africa", "2/19/2022")

# i16
# tìm các đất nước có số lượng dữ liệu thu thập bằng nhau
# ở đây tôi không biết là in tất cả hay in một số!
# tạo một bảng mới có iso_code ở đầu, đề yêu cầu in iso_cdoe
#wrt ("\nCác đất nước có số lượng dữ liệu thu thập được bằng nhau")
cached <- c()
nValuesOfLoca2 <- data.frame(append(unique(bigTable$ iso_code), 0), nValuesOfLoca)
for (i in 1:(nrow(nValuesOfLoca) - 1)) {
    for (j in ((i + 1):nrow(nValuesOfLoca))) {
        if (nValuesOfLoca[i, 2] == nValuesOfLoca[j, 2] &
            !(nValuesOfLoca[i, 2] %in% cached)) { # toán tử %in% để kiểm tra xem một phần tử có ở trong một dãy hay không
            write(
                subset(nValuesOfLoca2, values == nValuesOfLoca[i, 2])[, 1],
                file = "16 Các đất nước có số lượng dữ liệu thu thập được bằng nhau.txt",
                append = TRUE
            )
            cached <- append(cached, nValuesOfLoca[i, 2])
            # ở đây tôi không biết là in tất cả hay in một số!
        }
    }
}

nValuesOfLoca <- head (nValuesOfLoca, -1)
cached <-nValuesOfLoca %>% group_by(values) %>% filter(n() > 1) %>% summarise(countries = paste (locations, collapse=","))
write.csv(
                (cached),
                file = "16 Các đất nước có số lượng dữ liệu thu thập được bằng nhau.csv",
)

# lỗi ở đâyasdfdaf

# i17 liệt kê tên đất nước, chiều dài iso_code >= 3
longNameCountry <- (subset(
    bigTable,
    str_length(iso_code) > 3
)[, c(1, 3)])
write.csv (unique ((longNameCountry)), file = "17 tên đất nước có chiều dài iso_code lớn hơn 3.csv")


setwd('..')
save.image("workspace/mission1.RData")
