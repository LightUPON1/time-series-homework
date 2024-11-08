library(readr)      
library(dplyr)      
library(ggplot2)   
library(tidyr)     
library(lubridate)  
library(forecast)   
library(tseries)  
library(urca)       
library(lmtest)    
library(vars)      
library(dynlm)      
library(reshape2)

# 读取数据
file_path <- "D:/桌面/黄金期货历史数据2.csv"  
data <- read_csv(file_path)

data <- data %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

# 对 'ave' 和 'volumn' 进行对数转换
data <- data %>%
  mutate(
    log_ave = log(ave),
    log_volumn = log(volumn)
  )

# 创建时间序列对象
log_ave_ts <- ts(data$log_ave, frequency = 365)
log_volumn_ts <- ts(data$log_volumn, frequency = 365)

# 计算一阶差分
d_log_ave <- diff(data$log_ave)
d_log_volumn <- diff(data$log_volumn)

var_data <- data.frame(
  d_log_ave = d_log_ave,
  d_log_volumn = d_log_volumn,
  log_volumn = data$log_volumn[-1] 
)

var_data <- na.omit(var_data)

var_data_ts <- ts(var_data[, c("d_log_ave", "d_log_volumn")])

exogen_data <- as.matrix(var_data$log_volumn)

# 选择 VAR 模型的滞后阶数，包含外生变量
lag_selection <- VARselect(var_data_ts, lag.max = 10, type = "const", exogen = exogen_data)
print(lag_selection$selection)

selected_lag <- lag_selection$selection["AIC(n)"]
var_model <- VAR(var_data_ts, p = selected_lag, type = "const", exogen = exogen_data)

summary(var_model)

# 预测未来20天
future_exogen_data <- matrix(rep(tail(var_data$log_volumn, 1), 20), ncol = 1)

# 进行预测
var_forecast <- predict(var_model, n.ahead = 20, dumvar = future_exogen_data)

forecast_d_log_ave_var <- var_forecast$fcst$d_log_ave[, "fcst"]

# 转换为水平预测
last_log_ave <- tail(data$log_ave, 1)
forecast_log_ave_var <- cumsum(c(last_log_ave, forecast_d_log_ave_var))
forecast_log_ave_var <- forecast_log_ave_var[-1]  # 移除初始值

# 创建未来日期
future_dates <- seq(from = max(data$date) + 1, by = 1, length.out = 20)

# 绘制预测结果
plot(
  x = c(data$date, future_dates),
  y = c(data$log_ave, forecast_log_ave_var),
  type = "l", col = "blue", lwd = 2,
  ylab = "log_ave", xlab = "日期",
  main = "带外生变量的 VAR 模型预测未来20天的黄金基金价格（对数）"
)
abline(v = max(data$date), col = "red", lty = 2)
legend("topright", legend = c("历史数据", "预测数据"), col = c("blue", "blue"), lwd = 2)

# 构建 ARIMAX 模型
train_d_log_ave_ts <- d_log_ave  # 使用所有差分后的 log_ave
train_xreg <- as.matrix(data$log_volumn[-1])  # 除去第一个元素，因为差分后少一个数据点

arimax_model <- auto.arima(
  train_d_log_ave_ts,
  xreg = train_xreg,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(arimax_model)

# 预测未来20天
future_volumn_arimax <- rep(tail(data$log_volumn, 1), 20)  # 假设 volumn 不变
forecast_arimax <- forecast(arimax_model, xreg = as.matrix(future_volumn_arimax), h = 20)

# 预测的差分序列
predicted_d_log_ave_arimax <- forecast_arimax$mean

# 转换为水平预测
last_log_ave_arimax <- tail(data$log_ave, 1)
predicted_log_ave_arimax <- cumsum(c(last_log_ave_arimax, predicted_d_log_ave_arimax))
predicted_log_ave_arimax <- predicted_log_ave_arimax[-1]

# 绘制预测结果
plot(
  x = c(data$date, future_dates),
  y = c(data$log_ave, predicted_log_ave_arimax),
  type = "l", col = "blue", lwd = 2,
  ylab = "log_ave", xlab = "日期",
  main = "ARIMAX 模型预测未来20天的黄金基金价格（对数）"
)
abline(v = max(data$date), col = "red", lty = 2)
legend("topright", legend = c("历史数据", "预测数据"), col = c("blue", "blue"), lwd = 2)

# 干预分析
# 定义干预事件及其日期
intervention_events <- data.frame(
  event = c("Obama Inauguration", "Fed QE3 Announcement", "Ukraine Crisis Escalation"),
  date = as.Date(c("2009-01-20", "2012-09-13", "2014-07-14"))
)

print("新的干预事件及其日期：")
print(intervention_events)

# 创建干预变量
for (i in 1:nrow(intervention_events)) {
  var_name <- paste0("intervention_", i)
  data[[var_name]] <- ifelse(data$date >= intervention_events$date[i], 1, 0)
}

# 更新 data_diff 中的干预变量
intervention_vars <- paste0("intervention_", 1:nrow(intervention_events))
data_diff <- data.frame(
  date = data$date[-1],
  d_log_ave = diff(data$log_ave),
  log_volumn = data$log_volumn[-1]
)

for (var in intervention_vars) {
  data_diff[[var]] <- data[[var]][-1]
}

# 使用所有数据进行训练
train_d_log_ave_ts_interv <- ts(data_diff$d_log_ave)
train_xreg_interv <- as.matrix(data_diff[, c("log_volumn", intervention_vars)])

arimax_model_interv <- auto.arima(
  train_d_log_ave_ts_interv,
  xreg = train_xreg_interv,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(arimax_model_interv)

# 预测未来20天
future_volumn_interv <- rep(tail(data$log_volumn, 1), 20)  # 假设 volumn 不变
future_interventions <- matrix(0, nrow = 20, ncol = length(intervention_vars))
colnames(future_interventions) <- intervention_vars

# 更新未来干预变量的值
for (i in 1:nrow(intervention_events)) {
  if (max(data$date) + 1 <= intervention_events$date[i]) {
    future_interventions[, i] <- 0
  } else {
    future_interventions[, i] <- 1
  }
}

future_xreg_interv <- cbind(future_volumn_interv, future_interventions)

forecast_arimax_interv <- forecast(
  arimax_model_interv,
  xreg = future_xreg_interv,
  h = 20
)

predicted_d_log_ave_arimax_interv <- forecast_arimax_interv$mean

# 转换为水平预测
last_log_ave_arimax_interv <- tail(data$log_ave, 1)
predicted_log_ave_arimax_interv <- cumsum(c(last_log_ave_arimax_interv, predicted_d_log_ave_arimax_interv))
predicted_log_ave_arimax_interv <- predicted_log_ave_arimax_interv[-1]

# 绘制预测结果
plot(
  x = c(data$date, future_dates),
  y = c(data$log_ave, predicted_log_ave_arimax_interv),
  type = "l", col = "blue", lwd = 2,
  ylab = "log_ave", xlab = "日期",
  main = "ARIMAX 含干预模型预测未来20天的黄金基金价格（对数）"
)
abline(v = max(data$date), col = "red", lty = 2)
legend("topright", legend = c("历史数据", "预测数据"), col = c("blue", "blue"), lwd = 2)

# 预测结果比较

forecast_data <- data.frame(
  date = future_dates,
  VAR预测 = forecast_log_ave_var,
  ARIMAX预测 = predicted_log_ave_arimax,
  ARIMAX含干预预测 = predicted_log_ave_arimax_interv
)

forecast_data_melt <- melt(forecast_data, id.vars = "date", variable.name = "模型", value.name = "log_ave")

# 绘制所有模型的预测结果对比图
ggplot(forecast_data_melt, aes(x = date, y = log_ave, color = 模型)) +
  geom_line(size = 1) +
  labs(
    title = "不同模型预测未来20天的黄金基金价格（对数）",
    x = "日期",
    y = "log_ave"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print("含干预的 ARIMAX 模型系数：")
print(coef(arimax_model_interv))
