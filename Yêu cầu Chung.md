NHỮNG ĐIỀU CẦN LƯU Ý VỀ TÊN FILE, KIỂU FILE

Tất cả file code R nên được định dạng theo cú pháp: "task<mục số>.R" VD: task69.R

Các file output thể hiện danh sách dữ liệu nên được định dạng .csv, đặt tên giống như file code VD: task69.csv
Dùng hàm write.table(<biến>, file = "<tênfile>", ....) để ghi ra output. 
VD: write.table(x69 , file = "task69.csv", row.names = FALSE, col.names = FALSE, quote = FALSE, append = TRUE)
row.names, cal.names: tên hàng, cột: = FALSE để không hiện hàng cột, = "abc" để đặt tên là abc
quote = FALSE để không hiện dấu ngoặc kép
append = TRUE để tiếp tục ghi lên con trỏ của file output, 
       = FALSE sẽ xóa tất cả dữ liệu và ghi lại ban đầu (thích hợp để đặt vào biến write.table đầu tiên)
       
Các file biểu đồ đặt tên giống như file code, theo tôi định dạng pdf vẽ đẹp nhất VD: task8_subtask69_task69.pdf

Update: https://github.com/owid/covid-19-data/issues/66
Các giá trị âm trong dữ liệu là do sự đính chính của bên cung cấp dữ liệu, số người chết âm là vì được hồi sinh <(") (chim cánh cụt)

thanks for reading
