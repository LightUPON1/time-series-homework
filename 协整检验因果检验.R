# 加载必要的包
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

file_path <- "D:/桌面/黄金期货历史数据2.csv"
data <- read_csv(file_path)

data <- data %>%
  mutate(date = ymd(date)) %>%
  arrange(date)

# 对 ave 和 volumn 进行对数转换
data <- data %>%
  mutate(log_ave = log(ave),
         log_volumn = log(volumn))

# 将对数转换后的数据转换为时间序列对象
start_year <- year(min(data$date))
start_day <- yday(min(data$date))

# 创建时间序列对象，假设数据为月度数据，起始时间为2006年1月
log_ave_ts <- ts(data$log_ave, start = c(start_year, start_day), frequency = 365)
log_volumn_ts <- ts(data$log_volumn, start = c(start_year, start_day), frequency = 365)
log_data_ts <- ts(data[, c("log_ave", "log_volumn")], start = c(start_year, start_day), frequency = 365)

# 进行差分
log_ave_diff <- diff(log_ave_ts)
data_diff <- data.frame(date = data$date[-1],  # 因为差分后少一个数据点
                        log_ave_diff = log_ave_diff,
                        log_volumn = log_volumn_ts[-1])

log_ave_volumn_ts <- ts(data_diff[, c("log_ave_diff", "log_volumn")], start = c(start_year, start_day + 1), frequency = 365)

# 查看新构造的时间序列
plot.ts(log_ave_volumn_ts)

#格兰杰因果检验
granger_test_1 <- grangertest(log_ave_diff ~ log_volumn, order = 1, data = data_diff)
granger_test_2 <- grangertest(log_volumn ~ log_ave_diff, order = 1, data = data_diff)

# 查看格兰杰因果检验结果
print("格兰杰因果检验结果：log_ave_diff 是否影响 log_volumn")
print(granger_test_1)
print("格兰杰因果检验结果：log_volumn 是否影响 log_ave_diff")
print(granger_test_2)
