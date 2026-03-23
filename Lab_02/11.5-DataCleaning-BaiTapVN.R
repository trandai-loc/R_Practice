# BÀI THỰC HÀNH VỀ NHÀ
# Tên dataset : marketing_campaign.csv
# Nguồn       : Kaggle - Customer Personality Analysis
# Mô tả       : Dữ liệu phân tích tính cách và hành vi mua hàng
#               của khách hàng trong các chiến dịch marketing
#
# Các biến quan trọng:
#   Year_Birth      : Năm sinh của khách hàng
#   Education       : Trình độ học vấn (Basic → PhD)
#   Marital_Status  : Tình trạng hôn nhân
#   Income          : Thu nhập hàng năm (USD)
#   Kidhome         : Số trẻ nhỏ trong hộ gia đình
#   Teenhome        : Số thiếu niên trong hộ gia đình
#   Recency         : Số ngày kể từ lần mua hàng gần nhất
#   MntWines        : Chi tiêu cho rượu vang (2 năm gần nhất)
#   AcceptedCmp1-5  : Có chấp nhận chiến dịch marketing không (0/1)
#   Response        : Có phản hồi chiến dịch cuối không (0/1)


# BƯỚC 1: Load và Khám phá Dữ liệu

# Load dữ liệu
clients <- read.csv("D:/R/R_Practice/Data/marketing_campaign.csv", stringsAsFactors = FALSE)

# Xem 6 dòng đầu tiên
head(clients)

# Kiểm tra cấu trúc
str(clients)

# Kiểm tra class của từng biến
sapply(clients, class)

# Tóm tắt thống kê
summary(clients)

# Loại bỏ các cột không có ý nghĩa phân tích
# Z_CostContact (luôn = 3) và Z_Revenue (luôn = 11) → không có giá trị thống kê
head(clients[, c("Z_CostContact", "Z_Revenue")])  # Xem trước khi xóa
clients <- clients[, !(names(clients) %in% c("Z_CostContact", "Z_Revenue"))]

# Lưu ý:
#   !(names(clients) %in% c(...)) → lấy tất cả cột NGOẠI TRỪ các cột trong danh sách
#   Luôn kiểm tra trước khi xóa để tránh mất dữ liệu quan trọng


# BƯỚC 2: Xử lý Missing Data

# Phát hiện missing data
# Đếm số NA theo từng cột
colSums(is.na(clients))

# Biến nào có giá trị bị thiếu?
names(which(colSums(is.na(clients)) > 0))

# Tìm các dòng có dữ liệu thiếu
clients[!complete.cases(clients), ]

# Đếm số dòng bị thiếu
length(clients[!complete.cases(clients), 1])


# --- Xử lý biến số Numeric: Income ---

# Kiểm tra phân bố thu nhập
summary(clients$Income)

# Tính trung vị (bỏ qua NA)
# Dùng median vì phân phối thu nhập thường lệch phải (right-skewed)
median(clients$Income, na.rm = TRUE)

# Điền missing values bằng median
clients$Income[is.na(clients$Income)] <- median(clients$Income, na.rm = TRUE)

# Kiểm tra xem còn NA không
clients$Income[is.na(clients$Income)]


# --- Xử lý biến số Numeric: Year_Birth ---

# Kiểm tra phân bố năm sinh
summary(clients$Year_Birth)

# Year_Birth là số nguyên → dùng round(median) để tránh kết quả thập phân
# (Chúng ta không muốn năm sinh là 1969.5!)
median(clients$Year_Birth, na.rm = TRUE)

# Điền missing values
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(median(clients$Year_Birth, na.rm = TRUE))

# Kiểm tra xem còn NA không
clients$Year_Birth[is.na(clients$Year_Birth)]


# Kiểm tra lại toàn bộ sau khi xử lý
clients[!complete.cases(clients), ]
colSums(is.na(clients))


# BƯỚC 3: Chuyển đổi Categorical Data thành Factor

str(clients)

# ------------------------------------------------------------
# Biến nominal đơn giản (Nominal Variables - không có thứ tự)
# ------------------------------------------------------------

# Marital_Status - Tình trạng hôn nhân
summary(factor(clients$Marital_Status))

# Lưu ý: Có một số giá trị lạ cần xử lý trước khi chuyển factor:
#   "Absurd", "YOLO" → không phải tình trạng hôn nhân hợp lệ → gộp vào "Single"
#   "Alone"          → tương đương "Single" về mặt ý nghĩa
clients$Marital_Status[clients$Marital_Status %in% c("Absurd", "YOLO")] <- "Single"
clients$Marital_Status[clients$Marital_Status == "Alone"]                <- "Single"

# Kiểm tra lại sau khi chuẩn hóa
summary(factor(clients$Marital_Status))

# Chuyển thành factor
clients$Marital_Status <- factor(clients$Marital_Status)

# Bài học: Trong thực tế, dữ liệu thường không "sạch" → cần kiểm tra kỹ trước khi chuyển!


# ------------------------------------------------------------
# Ordinal Factors (Biến có thứ tự)
# ------------------------------------------------------------

# Education - Trình độ học vấn
summary(factor(clients$Education))

# Thứ tự logic từ thấp đến cao:
#   Basic < 2n Cycle (trung học) < Graduation (cử nhân) < Master < PhD
clients$Education <- factor(clients$Education,
                            levels  = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"),
                            ordered = TRUE)

# Kiểm tra kết quả
summary(clients$Education)
levels(clients$Education)

# Ưu điểm của ordered factor: có thể so sánh trực tiếp
clients$Education[1] < clients$Education[2]  # Trả về TRUE hoặc FALSE


# BƯỚC 4: Tự động hóa với lapply()

# --- Các biến nhị phân 0/1 (Binary Variables) ---
# Nhận xét: Có 7 biến cùng dạng 0/1 → dùng lapply() thay vì lặp code 7 lần!

binaryVariables <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3",
                     "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")

# Xem dữ liệu
clients[, binaryVariables]

# Kiểm tra từng biến trước khi chuyển đổi
lapply(clients[, binaryVariables], function(x) summary(factor(x)))

# Giải thích lapply:
#   lapply(danh_sách, hàm): áp dụng hàm lên từng phần tử của danh sách
#   Trả về một list kết quả
#   → Hiệu quả hơn nhiều so với viết từng dòng riêng lẻ

# Chuyển đổi tất cả 7 biến cùng lúc
clients[, binaryVariables] <- lapply(clients[, binaryVariables], factor)

# Kiểm tra lại
lapply(clients[, binaryVariables], summary)


# --- Các biến số lượng con (Kidhome, Teenhome) ---
# Kidhome  : 0, 1, 2 → số con nhỏ, không có ý nghĩa số học → nên là factor
# Teenhome : 0, 1, 2 → số con thiếu niên, tương tự

summary(factor(clients$Kidhome))
summary(factor(clients$Teenhome))

countVariables <- c("Kidhome", "Teenhome")
clients[, countVariables] <- lapply(clients[, countVariables], factor)


# BƯỚC 5: Kiểm tra kết quả cuối cùng

# Xem cấu trúc dữ liệu sau khi clean
str(clients)

# Tóm tắt thống kê
summary(clients)

# Kiểm tra missing data - phải = 0
sum(!complete.cases(clients))

# Tóm tắt dataset sạch
cat("Dataset sau khi xử lý:\n")
cat(" - Số hàng       :", nrow(clients), "\n")
cat(" - Số cột        :", ncol(clients), "\n")
cat(" - Missing values:", sum(is.na(clients)), "\n")
cat(" - Số factor     :", sum(sapply(clients, is.factor)), "biến\n")
cat(" - Số numeric    :", sum(sapply(clients, is.numeric)), "biến\n")


# BƯỚC 6: Lưu dữ liệu sạch

# Lưu dạng RData để dùng lại trong R (giữ nguyên kiểu dữ liệu)
save(clients, file = "clientsInR.RData")

# Lưu thêm dạng CSV để dùng ngoài R
write.csv(clients, "marketing_campaign_cleaned.csv", row.names = FALSE)

# Kiểm tra đã lưu thành công chưa
load("clientsInR.RData")
cat("Đã lưu thành công! File clientsInR.RData sẵn sàng sử dụng.\n")


# MÔ TẢ CÁC BIẾN TRONG DATASET
# (29 biến gốc → còn 27 sau khi xóa Z_CostContact và Z_Revenue)

# --- NHÂN KHẨU HỌC ---
# ID                  : Mã định danh khách hàng              [numeric]
# Year_Birth          : Năm sinh                              [numeric]
# Education           : Trình độ học vấn                      [ordered factor]
#                         Basic < 2n Cycle < Graduation < Master < PhD
# Marital_Status      : Tình trạng hôn nhân                   [factor]
#                         Divorced / Married / Single / Together / Widow
# Income              : Thu nhập hàng năm (USD)                [numeric]
# Kidhome             : Số trẻ nhỏ trong hộ gia đình          [factor: 0/1/2]
# Teenhome            : Số thiếu niên trong hộ gia đình        [factor: 0/1/2]
# Dt_Customer         : Ngày trở thành khách hàng              [character]

# --- HÀNH VI MUA HÀNG ---
# Recency             : Số ngày kể từ lần mua gần nhất        [numeric]
# MntWines            : Chi tiêu rượu vang (2 năm)             [numeric]
# MntFruits           : Chi tiêu trái cây                      [numeric]
# MntMeatProducts     : Chi tiêu thịt                          [numeric]
# MntFishProducts     : Chi tiêu hải sản                       [numeric]
# MntSweetProducts    : Chi tiêu đồ ngọt                       [numeric]
# MntGoldProds        : Chi tiêu sản phẩm cao cấp              [numeric]
# NumDealsPurchases   : Số lần mua qua khuyến mãi              [numeric]
# NumWebPurchases     : Số lần mua qua website                 [numeric]
# NumCatalogPurchases : Số lần mua qua catalog                 [numeric]
# NumStorePurchases   : Số lần mua tại cửa hàng                [numeric]
# NumWebVisitsMonth   : Lượt truy cập website trong tháng      [numeric]

# --- CHIẾN DỊCH MARKETING (0 = Không, 1 = Có) ---
# AcceptedCmp1        : Chấp nhận chiến dịch 1                 [factor]
# AcceptedCmp2        : Chấp nhận chiến dịch 2                 [factor]
# AcceptedCmp3        : Chấp nhận chiến dịch 3                 [factor]
# AcceptedCmp4        : Chấp nhận chiến dịch 4                 [factor]
# AcceptedCmp5        : Chấp nhận chiến dịch 5                 [factor]
# Complain            : Có khiếu nại trong 2 năm qua           [factor]
# Response            : Phản hồi chiến dịch cuối cùng          [factor]

