# 加载必要的库
library(forecast)  # 包含 auto.arima 和季节性分解函数
library(tseries)   # 包含 ADF 平稳性检验
library(lmtest)

# 读取数据
file_path <- "D:/桌面/黄金期货历史数据2.csv"
data <- read.csv(file_path)

# 检查并转换日期格式
data$date <- as.Date(data$date, format="%Y-%m-%d")

# 创建时间序列对象，假设数据为月度数据，起始时间为2008年1月1日
avg_price_ts <- ts(data$ave, start = c(2008, 1), frequency = 365)

ljung_box_test <- Box.test(avg_price_ts, lag = 10, type = "Ljung-Box")
print(ljung_box_test)

adf_test_ave <- adf.test(avg_price_ts, alternative = "stationary")
print(adf_test_ave)

ave_diff <- diff(avg_price_ts)
adf_test_ave_diff <- adf.test(ave_diff, alternative = "stationary")
print(adf_test_ave_diff)

# 计算原始数据的均值、方差和中位数
mean_avg_price <- mean(avg_price_ts)
var_avg_price <- var(avg_price_ts)
median_avg_price <- median(avg_price_ts)

cat("日均价均值:", mean_avg_price, "\n")
cat("日均价方差:", var_avg_price, "\n")
cat("日均价中位数:", median_avg_price, "\n")

#使用 STL 分解观察季节性成分
decomposition <- stl(avg_price_ts, s.window = "periodic")
plot(decomposition, main="STL分解：日均价")

# 提取并绘制季节性成分
seasonal_component <- decomposition$time.series[, "seasonal"]
plot(seasonal_component, main="季节性成分", ylab="季节性", xlab="时间")


par(mfrow = c(1, 2))  
acf(ave_diff, main = "日均价 ACF 图")
pacf(ave_diff, main = "日均价 PACF 图")
par(mfrow = c(1, 1))  

volumn_ts <- ts(data$volumn, start = c(2008, 1), frequency = 365)

adf_test_volumn <- adf.test(volumn_ts, alternative = "stationary")
print(adf_test_volumn)

# 计算原始数据的均值、方差和中位数
mean_volumn <- mean(volumn_ts)
var_volumn <- var(volumn_ts)
median_volumn <- median(volumn_ts)

cat("日交易量均值:", mean_volumn, "\n")
cat("日交易量方差:", var_volumn, "\n")
cat("日交易量中位数:", median_volumn, "\n")

#使用 STL 分解观察季节性成分
decomposition <- stl(volumn_ts, s.window = "periodic")
plot(decomposition, main="STL分解：日交易量")

# 提取并绘制季节性成分
seasonal_component <- decomposition$time.series[, "seasonal"]
plot(seasonal_component, main="季节性成分", ylab="季节性", xlab="时间")

par(mfrow = c(1, 2))  
acf(volumn_ts, main = "交易量 ACF 图")
pacf(volumn_ts, main = "交易量 PACF 图")
par(mfrow = c(1, 1)) 
