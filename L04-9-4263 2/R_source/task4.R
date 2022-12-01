load('workspace/preTask.RData')
load('workspace/mission1.RData')
load('workspace/mission2.RData')
library(ggplot2)
library(agricolae)
library(ggpubr)
library(lubridate)

setwd('task4')

#Reference:  https://stackoverflow.com/a/48820173
#            https://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot/48820173#48820173   
rotate_x <- function(data, column_to_plot, labels_vec, rot_angle) {
    plt <- barplot(data[[column_to_plot]], col='steelblue', xaxt="n",
        ylim = c(0, a = min (c( max(percentage) * 3 / 2, 100 ))),
        main = paste ( "Relative frequency of", conti ),
        ylab = "Relative Frequency (%)")
    text(plt, par("usr")[3], labels = labels_vec, srt = rot_angle, adj = c(1.1,1.1), xpd = TRUE, cex=0.6) 
}   
    #dùng [[ ]] để truy cập từ chuỗi character thành tên một biến thực
    #hàm text có một parameter nào đó giúp cho các chuỗi ký tự có thể viết chồng lên nhau
    
# 1
# Vẽ biểu đồ tần số tích lũy quốc gia cho các châu lục
# ỏ đây t đoán là vẽ từng Châu Lục, mỗi Châu Lục vẽ các quốc gia, trong đó tần số thể hiện số quan sát
# (số dữ liệu thu thập được)
continents <- head(continents, n = -1)
subTable <- c()
for (conti in continents) { # cho chạy từng châu lục, có 6 châu lục tất cả
    # tách từng châu lục ra
    subTable <- subset(bigTable, continent == conti)
    sublocations <- unique(subTable$ location)

    # cái này giống i4 i5
    values <- c()
    for (string in sublocations) {
        values <- append(values, nrow(subset(subTable, location == string)))
    }
    locaInContiTable <- data.frame(sublocations, values) # Đây là bảng cuối, tiếp theo ta sẽ triển khai bảng này ra thực tế

    # tần số tích luỹ
    # xem https://www.diendat.net/cumulative-frequency-plots/
    x <- locaInContiTable$ values
    arrange <- seq(max(x) / 10, max(x), by = max(x) / 10)
    cumulative <- 1:10
    for (i in 1:10) {
        cumulative[i] <- length(x[x <= arrange[i]])
    }
    cumFreqTable <- ggplot(data.frame(arrange, cumulative), aes(arrange, cumulative)) +
        geom_bar(stat = "identity", fill = "blue") +
        ggtitle (paste("Cummulative frequency of",conti )) +
        labs (x = "Frequency", y= "Number of contries")
         
    ggexport(cumFreqTable, filename = paste("cumulativeFreqOf", conti, ".pdf"))

    # vẽ tần số tương đối
    s <- sum(x)
    percentage <- 1:length(x)
    for (i in 1:length(x)) {
        percentage[i] <- 100 * x[i] / s
    }
    data <- data.frame( sublocations, percentage)
    pdf(file= paste("relativeFreqOf", conti, ".pdf") )
    rotate_x (data,'percentage', sublocations,80 )
    dev.off()
    #viết lại tên ngay lên trên bar luôn, viết ở dưới sẽ rất phí

}



#3,4
#Vẽ biểu đồ thể hiện nhiễm bệnh đã báo cáo của các quốc gia mà thuộc về nhóm trong 7 ngày cuối của năm cuối cùng
#tìm 7 năm cuối cùng trước.
processDates <- strptime(dates, format = "%m/%d/%Y")

c("Canada", "United States","Greenland")

lastest7days <- c(max (processDates))  #thêm vào để dòng cuối không bị lỗi
for (i in 1:6) {
    lastest7days <- append(lastest7days, max (processDates [
        processDates < min (lastest7days)
    ]) )
}


#đập đi xây lại bảng mới
#bảng mới có cột date đã được xử lý
date <- strptime(bigTable[ , 4], format = "%m/%d/%Y")
bigTable[ , 4] <- NULL
bigTable <- data.frame (bigTable, date)

test = data.frame(c(1,2,3),c(5,6,7))

#7 ngày cuối cùng, mỗi ngày có bao nhiêu ca nhiễm bệnh, và chết?
for (qg in c("Canada", "United States","Greenland")) {
    numCase <- numDeath <- c()
    for (i in 1 : length(lastest7days)) {   #dùng for day in list sẽ gặp lỗi
        case <- sum (  subset(bigTable, date == lastest7days[i] & location ==qg ) $ new_cases   )
        numCase <- append(numCase, case)

        death <- sum (  subset(bigTable, date == lastest7days[i] & location ==qg) $ new_deaths )
        numDeath <- append(numDeath, death)
    }
    caseAndDeath <- data.frame (lastest7days, numCase, numDeath ) #đây là bảng cần xây dựng

    #Đến bước vẽ biểu đồ cột (bar) thể hiện số người mắc bệnh mới và số người mất
    #ý tưởng, vẽ biểu đồ bar, để, cả 2 số liệu cùng thể hiện với nhau
    #màu xanh là số ca mắc mới, màu đỏ là số người chét
    # as.character (lastest7days) đẻ đặt tên cho biểu đồ

    pdf(file= paste ("số ca nhiễm mới 7 ngày cuối cùng của", qg, ".pdf") )
    cached <- barplot(numCase, col='blue',
            main =paste  ("New cases in last 7 days in",qg),
            xlab = "Date", xaxt="n",
            ylab = "Number of new cases",
            )    
    text(cached, par("usr")[3], labels = as.character(lastest7days), srt = 0, adj = c(0.5,1.1), xpd = TRUE, cex=0.6) 
    dev.off()

    pdf(file= paste ("số ca mất 7 ngày cuối cùng của", qg, ".pdf") )
    cached <- barplot(numDeath, col='blue',
            main =paste  ("New deaths in last 7 days in",qg),
            xlab = "Date", xaxt="n",
            ylab = "Number of new deaths",
            )    
    text(cached, par("usr")[3], labels = as.character(lastest7days), srt = 0, adj = c(0.5,1.1), xpd = TRUE, cex=0.6) 
    dev.off()
}



#vẽ biểu đồ phổ
#từ task2
#barplot (unlist (fullCaseTable[ ,9]))
#ggplot (unlist (fullCaseTable), aes (x = ctr_i, y = Outlier))

  #chỉ cần quan tâm đến số liệu, tên không còn quan trọng nữa
pdf(file= "Biểu đồ phồ Outliers ca nhiễm mới.pdf" )
hist(unlist (fullCaseTable[,9]), main = "Infection cases oulier distribution", col = 'blue', xlab ="Number of outliers", ylab = "Number of countries",
    breaks = 25)
dev.off()

pdf(file= "Biểu đồ phồ Outliers tử vong mới.pdf" )
hist(unlist (fullDeathTable[,9]), main = "Infection cases oulier distribution", col = 'blue', xlab ="Number of outliers", ylab = "Number of countries",
    breaks = 25)
dev.off()

setwd("..")


