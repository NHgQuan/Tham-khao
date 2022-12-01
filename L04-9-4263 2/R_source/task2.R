# một số hàm được sử dụng
# tính tứ phân vị, dùng hàm quantile() hoặc summary()
# na.omit() để loại bỏ giá trị NA
# hàm summary() cho cả giá trị trung bình
# sd() để tính giá trị lệch chuẩn
load("workspace/preTask.RData")
load("workspace/mission1.RData")

swtch <- function(x,i,j) {
    x[c(i,j)] <- x[c(j,i)];
    names (x)[c(i,j)] <- names (x) [c(j,i)]
    x
} 

#làm lại từ đầu
#phân mỗi nước thành newcase, newdeath riêng
locations <- head (locations, n = -1)
fullCaseTable <- vector ("list", length = 9)
fullDeathTable <- vector ("list", length = 9)

for (loca in locations) {
    #newcase truoc
    caseinLoca  <-  na.omit (subset (bigTable, location == loca) $ new_cases) #đây là bảng được tách
    ctr_i <- c(loca)
    caseTable <- as.list(as.array (summary (caseinLoca)))
    caseTable <-  append(ctr_i,caseTable )  #dùng data.farme là sai ngay
    names (caseTable) [1]='ctr_i'
    names (caseTable) [5]='Avg'
    #sắp xếp lại
    caseTable <- swtch(caseTable,5,7)
    caseTable <- swtch(caseTable,5,6)
    names (caseTable) [c(3,4,5)] <- c('Q1','Q2','Q3')
    
    #thêm giá trị lệch chuẩn
    caseTable <- append( caseTable, sd(caseinLoca, na.rm = TRUE) )
    names (caseTable) [8] <- 'Std'

    #tính outlier
    Q1 <- summary (caseinLoca)[2]
    Q3 <- summary (caseinLoca)[5]
    caseIQR <- Q3 - Q1 #tính từ caseTable không được.. hoặc phải dùng hàm as.numeric()
    caseOutliers <- length(caseinLoca[caseinLoca < Q1 - 1.5 * caseIQR |
        caseinLoca > Q3 + 1.5 * caseIQR])

    #hoàn tất bảng, hulala
    caseTable <- append( caseTable, caseOutliers)
    names (caseTable) [9] <- 'Outlier'

    fullCaseTable <- rbind (caseTable,fullCaseTable)



    #bây giờ tính riêng cho death, đơn gỉan là copye và paste lại từ case
    deathinLoca <- na.omit (subset (bigTable, location == loca) $ new_deaths)
    ctr_i <- c(loca)
    deathTable <- as.list(as.array (summary (deathinLoca)))
    deathTable <-  append(ctr_i,deathTable )
    names (deathTable) [1]='ctr_i'
    names (deathTable) [5]='Avg'
    #sắp xếp lại
    deathTable <- swtch(deathTable,5,7)
    deathTable <- swtch(deathTable,5,6)
    names (deathTable) [c(3,4,5)] <- c('Q1','Q2','Q3')
    
    #thêm giá trị lệch chuẩn
    deathTable <- append( deathTable, sd(deathinLoca, na.rm = TRUE) )
    names (deathTable) [8] <- 'Std'

    #tính outlier
    Q1 <- summary (deathinLoca)[2]
    Q3 <- summary (deathinLoca)[5]
    deathIQR <- Q3 - Q1 #tính từ caseTable không được.. hoặc phải dùng hàm as.numeric()
    deathOutliers <- length(deathinLoca[deathinLoca < Q1 - 1.5 * deathIQR |
        deathinLoca > Q3 + 1.5 * deathIQR])

    #hoàn tất bảng, hulala
    deathTable <- append( deathTable, deathOutliers)
    names (deathTable) [9] <- 'Outlier'
    fullDeathTable <- rbind (deathTable,fullDeathTable)
}

#dùng rbind các list để xếp chồng các list với nhau
#rownames(x) <- NULL để xoá tên hàng
rownames(fullCaseTable) <- NULL
fullCaseTable <- head(fullCaseTable, n = -1)  #để xoá hàng null cuối cùng
rownames(fullDeathTable) <- NULL
fullDeathTable <- head(fullDeathTable, n = -1)

colnames(fullCaseTable) <- c( "ctr_i", "Min", "Q1", "Q2", "Q3", "Max", "Avg", "Std", "Outlier")
colnames(fullDeathTable) <- c( "ctr_i", "Min", "Q1", "Q2", "Q3", "Max", "Avg", "Std", "Outlier")

setwd ('task2')
write.csv(fullCaseTable, file = "Bảng số liệu thống kê số lượng ca nhiễm mới từng đất nước.csv") 
write.csv(fullCaseTable, file = "Bảng số liệu thống kê số lượng tử vong mới từng đất nước.csv") 

#phần 6, đất nước thuộc về nhóm? t nghi dề có nghĩa là đất nước thuộc về nhóm theo mã đề
x <- c(match (c("Canada", "United States","Greenland"), fullCaseTable[ ,1]) )
write.csv(fullCaseTable[x , ], 
    file = "Bảng số liệu thống kê số lượng ca nhiễm mới từng đất nước theo nhóm.csv") #một cách làm mới để tìm các phần tử cần tìm khi không dùng được subset, dùng hàm match
write.csv(fullDeathTable[x , ], 
    file = "Bảng số liệu thống kê số lượng tử vong mới từng đất nước theo nhóm.csv") 

#7
canadaCase <-na.omit (subset (bigTable, location == "Canada") $ new_cases)
USCase <- na.omit (subset (bigTable, location == "United States") $ new_cases)
GrLandCase <- na.omit (subset (bigTable, location == "Greenland") $ new_cases)

pdf(file= "Biểu đồ boxplot cho số ca nhiễm mới của 3 nước thuộc nhóm.pdf" )
boxplot(list (canadaCase,USCase,GrLandCase), names = c("Canada", "United States","Greenland"))
dev.off()

setwd('..')

save.image("workspace/mission2.RData")
