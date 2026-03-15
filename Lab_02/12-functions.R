## BÀI TẬP THỰC HÀNH

### Bài tập 1: Function cơ bản
# 1. Viết function tính diện tích hình chữ nhật
# Input: chiều dài, chiều rộng
# Output: diện tích
rectangle_area <- function(length, width) {length * width}

# 2. Viết function tính chu vi hình tròn
# Input: bán kính
# Output: chu vi
circle_perimeter <- function(radius) {2 * pi * radius}

# 3. Viết function chuyển đổi nhiệt độ từ Celsius sang Fahrenheit
# Công thức: F = C * 9/5 + 322
celsius_to_fahrenheit <- function(c) {c * 9/5 + 32}

### Bài tập 2: Function với validation

# 1. Viết function kiểm tra số chẵn/lẻ
# Input: một số nguyên
# Output: "Chẵn" hoặc "Lẻ"
# Validate: input phải là số nguyên
check_even_odd <- function(n) {
  if (!is.numeric(n) || n %% 1 != 0) {return("Input phải là số nguyên")}
  if (n %% 2 == 0) {return("Chẵn")} else {return("Lẻ")}
}

# 2. Viết function tính điểm trung bình
# Input: vector điểm số
# Output: điểm trung bình
# Validate: 
#   - Điểm phải từ 0 đến 10
#   - Loại bỏ giá trị NA
average_score <- function(scores) {scores <- scores[!is.na(scores)]
  if (any(scores < 0 | scores > 10)) {return("Điểm phải từ 0 đến 10")}mean(scores)}

### Bài tập 3: Function thống kê

# 1. Viết function tính toán tổng quan
# Input: vector số
# Output: list(mean, median, sd, min, max, range)
statistics_summary <- function(x) {
  list(
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    min = min(x),
    max = max(x),
    range = max(x) - min(x)
  )
}
# 2. Viết function tính hoán vị P(n, r)
# Công thức: P(n,r) = n! / (n-r)!
permutation <- function(n, r) {
  factorial(n) / factorial(n - r)
}
# 3. Viết function tính tổ hợp C(n, r)
# Công thức: C(n,r) = n! / (r! * (n-r)!)
combination <- function(n, r) {
  factorial(n) / (factorial(r) * factorial(n - r))
}

### Bài tập 4: Function nâng cao

# 1. Viết function tìm các số nguyên tố từ 1 đến n
# Input: n
# Output: vector các số nguyên tố
primes_to_n <- function(n) {
  primes <- c()
  
  for (num in 2:n) {
    is_prime <- TRUE
    
    for (i in 2:sqrt(num)) {
      if (num %% i == 0) {
        is_prime <- FALSE
        break
      }
    }
    
    if (is_prime) {
      primes <- c(primes, num)
    }
  }
  
  primes
}
# 2. Viết function tạo tam giác Pascal với n hàng
# Gợi ý: Sử dụng tổ hợp C(n, k)
pascal_triangle <- function(n) {
  triangle <- list()
  
  for (i in 0:(n-1)) {
    row <- c()
    for (k in 0:i) {
      row <- c(row, choose(i, k))
    }
    triangle[[i+1]] <- row
  }
  
  triangle
}
# 3. Viết function phân loại sinh viên dựa vào điểm
# Input: điểm số
# Output: xếp loại (Xuất sắc, Giỏi, Khá, TB, Yếu)
# Kèm theo GPA scale 4.0
student_classification <- function(score) {
  grade <- if (score >= 85) {
    "Xuất sắc"
  } else if (score >= 70) {
    "Giỏi"
  } else if (score >= 55) {
    "Khá"
  } else if (score >= 40) {
    "Trung bình"
  } else {
    "Yếu"
  }
  
  gpa <- if (score >= 85) {
    4.0
  } else if (score >= 70) {
    3.0
  } else if (score >= 55) {
    2.0
  } else if (score >= 40) {
    1.0
  } else {
    0
  }
  
  list(grade = grade, gpa = gpa)
}

### Bài tập 5: Ứng dụng thực tế

# 1. Viết function tính lương ròng
# Input: lương cơ bản, phụ cấp, số ngày làm việc, số giờ tăng ca
# Output: lương ròng sau thuế
net_salary <- function(base_salary, allowance, work_days, overtime_hours) {
  
  daily_salary <- base_salary / 26
  
  salary <- daily_salary * work_days
  
  overtime_pay <- overtime_hours * (daily_salary / 8) * 1.5
  
  gross_salary <- salary + allowance + overtime_pay
  
  tax <- gross_salary * 0.1
  
  net <- gross_salary - tax
  
  net
}
# 2. Viết function chuẩn hóa điểm thi
# Input: vector điểm thô
# Output: vector điểm chuẩn hóa (0-100)
# Công thức: (điểm - min) / (max - min) * 100
normalize_scores <- function(scores) {
  
  min_val <- min(scores)
  max_val <- max(scores)
  
  (scores - min_val) / (max_val - min_val) * 100
}
# 3. Viết function phân tích dữ liệu sinh viên
# Input: data frame (tên, tuổi, điểm)
# Output: thống kê mô tả đầy đủ
student_analysis <- function(df) {
  
  list(
    average_age = mean(df$age),
    average_score = mean(df$score),
    max_score = max(df$score),
    min_score = min(df$score),
    sd_score = sd(df$score)
  )
}
