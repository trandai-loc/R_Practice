### LẤY ĐIỂM THỰC HÀNH
### Các bài tập ######################################################################
# 1. Đọc mô tả dữ liệu phân tích tính cách khách hàng và tải 
# vào R (file clients.csv) với tên biến là "clients".
clients <- read.csv("D:/R/R_Practice/Data/clients.csv", stringsAsFactors = FALSE)
# 2. Xem qua cấu trúc dữ liệu và kiểm tra các lớp (classes) đã được gán 
# cho các biến trong bộ dữ liệu.
str(clients)
sapply(clients, class)
# 3. Kiểm tra xem có giá trị nào bị thiếu trong bộ dữ liệu không.
colSums(is.na(clients))
# a) Những biến nào có chứa giá trị bị thiếu?
names(which(colSums(is.na(clients)) > 0))
# b) Điền các giá trị bị thiếu bằng giá trị trung bình hoặc trung vị của biến đó.
# Trước khi điền, hãy xem xét bản chất của biến. Nếu là số nguyên (ví dụ: năm sinh),
# thì hãy điền giá trị phù hợp với bản chất của biến (chúng ta không muốn năm sinh là 1995.832, phải không? ;)).
clients$Income[is.na(clients$Income)] <- median(clients$Income, na.rm = TRUE)
# c) Bạn sử dụng đoạn mã nào để điền các giá trị bị thiếu của Year_Birth (nếu có)?
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(median(clients$Year_Birth, na.rm = TRUE))
# 4. a) Kiểm tra xem tất cả các giá trị bị thiếu đã được điền đầy đủ chưa. Nếu chưa, lặp lại bước 3.
colSums(is.na(clients))
# b) Bạn sẽ dùng đoạn mã nào để hiển thị tất cả các dòng vẫn còn chứa dữ liệu bị thiếu?
clients[!complete.cases(clients), ]
# 5. a) Xem xét những biến nào nên chuyển đổi thành kiểu "factor"?
# Gợi ý: Đây thường là các biến văn bản có một số giá trị cụ thể và lặp lại.
# Chúng cũng có thể là các biến được biểu diễn bằng số nhưng không mang "ý nghĩa số học"
# - ví dụ: biến "education" với các giá trị 2, 3, 4 thực chất đại diện cho các cấp độ
# giáo dục liên tiếp (ý nghĩa logic) thay vì số năm học tập chính xác (ý nghĩa số học).
# b) Bạn sẽ dùng đoạn mã ngắn nhất nào để chuyển đổi biến Marital_Status?
clients$Marital_Status <- factor(clients$Marital_Status)
# 6. a) Xem xét biến nào trong số các biến đã xác định ở trên nên được
# chuyển đổi thành kiểu 'ordered factor' (biến phân loại có thứ tự).
# Gợi ý: Biến kiểu 'ordered factor' nên chứa các mức có thứ tự logic
# - ví dụ: biến 'education' với các giá trị 'primary', 'secondary'
# và 'tertiary'. Trong trường hợp này, việc giữ thứ tự các mức là quan trọng.
# Một ví dụ điển hình khác của biến ordered factor là các câu trả lời
# khảo sát sử dụng thang đo Likert (https://en.wikipedia.org/wiki/Likert_scale).
# b) Bạn sẽ dùng đoạn mã nào để chuyển đổi biến Education? Giả sử rằng
# 2n nghĩa là giáo dục trung học và graduation tương đương với bảo vệ bằng cử nhân.
clients$Education <- factor(clients$Education,levels = c("Basic","2n Cycle","Graduation","Master","PhD"),ordered = TRUE)
# 7. Chuyển đổi các biến đã xác định trong bước 5 và 6 thành các lớp thích hợp.
clients$Education <- factor(clients$Education, ordered = TRUE)
clients$Marital_Status <- factor(clients$Marital_Status)
clients$Complain <- factor(clients$Complain)
clients$Response <- factor(clients$Response)
# 8. Lưu kết quả để tham khảo sau này! Sử dụng file RData với tên "clientsInR".
save(clients, file = "clientsInR.RData")

