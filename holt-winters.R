library(forecast)

# 读取时间序列数据
file_path <- "D:/桌面/黄金期货历史数据2.csv"
data <- read.csv(file_path)

# 频率为365
ts_data <- ts(data$ave, start = c(2008, 1), frequency = 365)

# 绘制原始时间序列
plot(ts_data, main = "原始时间序列", xlab = "时间", ylab = "值")

# 使用 Holt-Winters 三参数指数平滑进行建模（加法模型）
hw_model_add <- HoltWinters(ts_data, seasonal = "additive")

# 使用 Holt-Winters 三参数指数平滑进行建模（乘法模型）
hw_model_mult <- HoltWinters(ts_data, seasonal = "multiplicative")

# 提取残差并进行白噪声检验（加法模型）
residuals_add <- residuals(hw_model_add)
print("加法模型残差序列的白噪声检验结果：")
box_test_add <- Box.test(residuals_add, lag = 10, type = "Ljung-Box")
print(box_test_add)

# 提取残差并进行白噪声检验（乘法模型）
residuals_mult <- residuals(hw_model_mult)
print("乘法模型残差序列的白噪声检验结果：")
box_test_mult <- Box.test(residuals_mult, lag = 10, type = "Ljung-Box")
print(box_test_mult)

