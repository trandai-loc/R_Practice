### BÀI THỰC HÀNH VỀ NHÀ
### Dataset: marketing_campaign.csv
### 2240 hàng x 29 cột

## BƯỚC 1: Load và Khám phá Dữ liệu
clients <- read.csv("D:/R/R_Practice/Data/marketing_campaign.csv", stringsAsFactors = FALSE)

# Xem 6 dòng đầu
head(clients)

# Xem cấu trúc dữ liệu và kiểm tra class của từng biến
str(clients)
sapply(clients, class)

# Tóm tắt thống kê tổng quan
summary(clients)

# Loại bỏ 2 cột không có ý nghĩa phân tích
head(clients[, c("Z_CostContact", "Z_Revenue")]) 
clients <- clients[, !(names(clients) %in% c("Z_CostContact", "Z_Revenue"))]

## BƯỚC 2: Xử lý Missing Data
# Kiểm tra giá trị bị thiếu
colSums(is.na(clients))

# Xem biến nào có giá trị bị thiếu
names(which(colSums(is.na(clients)) > 0))

# Hiển thị các dòng có missing
clients[!complete.cases(clients), ]
# Đếm tổng số dòng bị thiếu
length(clients[!complete.cases(clients), 1])

# Điền missing values cho Income 
summary(clients$Income)
median(clients$Income, na.rm = TRUE)
clients$Income[is.na(clients$Income)] <- median(clients$Income, na.rm = TRUE)

# Điền missing values cho Year_Birth bằng round(median)
# (Year_Birth là số nguyên → không muốn kết quả như 1969.5)
summary(clients$Year_Birth)
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(median(clients$Year_Birth, na.rm = TRUE))

# Tổng missing values còn lại theo từng cột
colSums(is.na(clients))

# Hiển thị các dòng vẫn còn missing 
clients[!complete.cases(clients), ]

## BƯỚC 3: Chuyển đổi Categorical Data thành Factor

# Xem xét biến nào nên chuyển thành factor
str(clients)

# Cột Marital_Status - Tình trạng hôn nhân
summary(factor(clients$Marital_Status))
# Có một số giá trị lạ như "Absurd", "YOLO", "Alone" nên nhóm lại để thành các nhóm có ý nghĩa hơn
clients$Marital_Status[clients$Marital_Status %in% c("Absurd", "YOLO")] <- "Single"
clients$Marital_Status[clients$Marital_Status == "Alone"] <- "Single"
summary(factor(clients$Marital_Status))

# Chuyển Marital_Status thành factor
clients$Marital_Status <- factor(clients$Marital_Status)

# Education có thứ tự rõ ràng từ thấp đến cao:
# Basic < 2n Cycle (trung học) < Graduation (cử nhân) < Master < PhD
summary(factor(clients$Education))

# Chuyển Education thành ordered factor
clients$Education <- factor(clients$Education,
                             levels  = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"),
                             ordered = TRUE)

# Chuyển đổi các biến còn lại thành class phù hợp
binaryVariables <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3",
                     "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")

# Kiểm tra trước
lapply(clients[, binaryVariables], function(x) summary(factor(x)))

# Chuyển đổi tất cả 1 lần
clients[, binaryVariables] <- lapply(clients[, binaryVariables], factor)

# Kiểm tra lại toàn bộ 
str(clients)
summary(clients)

# Kiểm tra levels của Education 
levels(clients$Education)
# Thử so sánh Education
clients$Education[1] < clients$Education[2]

## BƯỚC 4: Kiểm tra kết quả cuối cùng
sum(!complete.cases(clients))

# Tóm tắt dataset
cat("Dataset sau khi xử lý:\n")
cat(" - Số hàng   :", nrow(clients), "\n")
cat(" - Số cột    :", ncol(clients), "\n")
cat(" - Missing   :", sum(is.na(clients)), "\n")
cat(" - Số factor :", sum(sapply(clients, is.factor)), "biến\n")

# Lưu kết quả
save(clients, file = "clientsInR.RData")




