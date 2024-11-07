library(forecast)
library(tseries)
library(lmtest)
library(FinTS)
library(mclust)
library(rugarch)

file_path <- "D:/桌面/黄金期货历史数据2.csv"
data <- read.csv(file_path)

# 检查并转换日期格式
data$date <- as.Date(data$date, format="%Y-%m-%d")

# 创建时间序列对象
avg_price_ts <- ts(data$ave, start = c(2008, 1), frequency = 365)

# 拟合 ARIMA 模型
arima_model0 <- auto.arima(avg_price_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
print("ARIMA 模型0:")
print(arima_model0)

arima_model1 <- auto.arima(avg_price_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE, lambda = 0)
print("ARIMA 模型1 (lambda=0):")
print(arima_model1)

# 使用 auto.arima 自动选择最优 ARIMA 模型
arima_model2 <- auto.arima(avg_price_ts, D = 1, seasonal = TRUE, stepwise = TRUE, approximation = TRUE, 
                           lambda = 0)  
print("ARIMA 模型2 (D=1, lambda=0):")
print(arima_model2)

residuals_arima_model2 <- residuals(arima_model2)

# 白噪声性检验
box_test_multiplicative <- Box.test(residuals_arima_model2, type = "Ljung-Box")
print("Box-Ljung 检验结果 (ARIMA 模型2 残差):")
print(box_test_multiplicative)

# 异方差性检验
arch <- ArchTest(residuals_arima_model2, lags = 12)  
print("ARIMA 模型2 残差的 ARCH 异方差检验结果:")
print(arch)

# 拟合 EGARCH 模型
print("残差存在显著的异方差性，拟合 EGARCH 模型。")

spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(arimaorder(arima_model2)[1], arimaorder(arima_model2)[3]), include.mean = TRUE),
  distribution.model = "std"
)

egarch_fit <- tryCatch(
  ugarchfit(spec = spec_egarch, data = residuals_arima_model2,
            solver = "hybrid",
            solver.control = list(trace = 1, maxit = 10000)),
)
print("EGARCH 模型拟合结果:")
show(egarch_fit)

# 提取 EGARCH 模型的标准化残差
std_residuals_egarch <- residuals(egarch_fit, standardize = TRUE)

# 检查 EGARCH 模型残差的白噪声性
box_test_egarch <- Box.test(std_residuals_egarch, type = "Ljung-Box")
print("EGARCH 模型标准化残差的白噪声性检验结果:")
print(box_test_egarch)

# 检查 EGARCH 模型残差的异方差性
arch_test_egarch <- ArchTest(std_residuals_egarch, lags = 12)
print("EGARCH 模型标准化残差的 ARCH 异方差检验结果:")
print(arch_test_egarch)

# 预测未来30天的平均价格
forecast_arima <- forecast(arima_model2, h = 30)
print("ARIMA 模型的未来30天预测结果:")
print(forecast_arima)
plot(forecast_arima, main = "未来30天的平均价格预测", ylab = "平均价格")

# 预测波动性
forecast_egarch <- ugarchforecast(egarch_fit, n.ahead = 30)
predicted_sigma <- sigma(forecast_egarch)

print("EGARCH 模型的未来30天波动性预测结果:")
print(predicted_sigma)

plot(predicted_sigma, type = "l", main = "未来30天的波动性预测", ylab = "波动性 (标准差)", col = "blue")

# 综合绘制均值和波动性预测
par(mfrow = c(2, 1))
plot(forecast_arima, main = "未来30天的平均价格预测", ylab = "平均价格")
plot(predicted_sigma, type = "l", main = "未来30天的波动性预测", ylab = "波动性 (标准差)", col = "blue")
par(mfrow = c(1, 1))

# 联合 ARIMA-EGARCH 模型
spec_joint <- ugarchspec(
  mean.model = list(armaOrder = c(3,1,1), include.mean = TRUE),
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  distribution.model = "std"
)

# 拟合联合模型
joint_fit <- tryCatch(
  ugarchfit(spec = spec_joint, data = avg_price_ts,
            solver = "hybrid",
            solver.control = list(trace = 1, maxit = 10000)),
)
print("联合 ARIMA-EGARCH 模型拟合结果:")
show(joint_fit)

# 进行预测
forecast_joint <- ugarchforecast(joint_fit, n.ahead = 30)

# 提取均值预测和波动性预测
predicted_mean_joint <- fitted(forecast_joint)
predicted_sigma_joint <- sigma(forecast_joint)

# 打印预测结果
print("联合模型未来30天的平均价格预测:")
print(predicted_mean_joint)

print("联合模型未来30天的波动性预测 (标准差):")
print(predicted_sigma_joint)

# 绘制联合模型的均值和波动性预测
par(mfrow = c(2,1))

plot(predicted_mean_joint, type = "l", main = "联合模型下的未来30天平均价格预测", ylab = "平均价格", col = "red")
plot(predicted_sigma_joint, type = "l", main = "联合模型下的未来30天波动性预测", ylab = "波动性 (标准差)", col = "green")

par(mfrow = c(1,1))

# 结合原始时间序列与联合模型的均值预测进行绘图
# 获取原始时间序列的结束时间
original_end <- end(avg_price_ts)

# 计算预测时间序列的开始时间
forecast_start_year <- original_end[1]
forecast_start_day <- original_end[2] + 1
if(forecast_start_day > 365){
  forecast_start_year <- forecast_start_year + 1
  forecast_start_day <- forecast_start_day - 365
}

# 创建预测均值的时间序列对象
predicted_mean_joint_ts <- ts(predicted_mean_joint, start = c(forecast_start_year, forecast_start_day), frequency = 365)

# 合并原始时间序列与预测时间序列
combined_ts <- ts(c(avg_price_ts, predicted_mean_joint_ts), start = start(avg_price_ts), frequency = 365)

# 绘制合并后的时间序列图
plot(combined_ts, 
     main = "原时间序列与联合模型预测", 
     ylab = "平均价格", 
     xlab = "时间", 
     col = "black")

# 添加联合模型的均值预测线
lines(predicted_mean_joint_ts, col = "red", lwd = 2)

# 添加图例
legend("topleft", 
       legend = c("原时间序列", "联合模型预测"), 
       col = c("black", "red"), 
       lty = 1, 
       lwd = c(1,2))
