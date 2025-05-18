# 加载必要的包
if (!require("quantmod")) install.packages("quantmod")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("PerformanceAnalytics")) install.packages("PerformanceAnalytics")
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(dygraphs)

# 设置参数
symbol <- "AAPL"        # 标的代码（示例使用苹果公司）
start_date <- "2020-01-01"
end_date <- Sys.Date()  # 获取当前日期
fast_ma <- 50           # 短期均线周期
slow_ma <- 200          # 长期均线周期
trade_size <- 1000      # 每次交易金额

# 获取股票数据
# getSymbols(symbol, from = start_date, to = end_date, auto.assign = TRUE)

AAPL <- readRDS("data/AAPL.rds")
price_data <- Cl(get(symbol))# 使用调整后收盘价
colnames(price_data) <- "Price"

# 计算技术指标
price_data$Fast_MA <- SMA(price_data$Price, n = fast_ma)
price_data$Slow_MA <- SMA(price_data$Price, n = slow_ma)

# 生成交易信号
price_data$Signal <- ifelse(price_data$Fast_MA > price_data$Slow_MA, 1, -1)
price_data$Signal <- lag(price_data$Signal, 1)  # 避免未来函数

# 计算持仓和收益
price_data$Returns <- dailyReturn(price_data$Price)
price_data$Strategy <- price_data$Signal * price_data$Returns

# 绩效分析
performance <- na.omit(price_data$Strategy)
charts.PerformanceSummary(performance, main = "策略绩效")
table.AnnualizedReturns(performance)

# 准备数据（在原有数据基础上添加买卖点标记）
price_data$Buy_Signal <- ifelse(price_data$Signal == 1, price_data$Price, NA)
price_data$Sell_Signal <- ifelse(price_data$Signal == -1, price_data$Price, NA)

# 创建dygraphs可视化
dygraph(price_data[, c("Price", "Fast_MA", "Slow_MA", "Buy_Signal", "Sell_Signal")], 
        main = "趋势跟踪策略可视化") %>%
  dySeries("Price", label = "价格", color = "#357ebd") %>%
  dySeries("Fast_MA", label = paste(fast_ma, "日均线"), color = "#d35400") %>%
  dySeries("Slow_MA", label = paste(slow_ma, "日均线"), color = "#28b463") %>%
  dySeries("Buy_Signal", label = "买入信号", strokePattern = "dashed", 
           color = "#28b463", pointShape = "triangle", strokeWidth = 0, 
           drawPoints = TRUE, pointSize = 1) %>%
  dySeries("Sell_Signal", label = "卖出信号", strokePattern = "dashed",
           color = "#c0392b", pointShape = "circle", strokeWidth = 0,
           drawPoints = TRUE, pointSize = 1) %>%
  dyAxis("y", label = "价格") %>%
  dyOptions(axisLineWidth = 1.5, 
            gridLineColor = "#e0e0e0", 
            includeZero = FALSE,
            connectSeparatedPoints = TRUE) %>%
  dyRangeSelector(height = 30) %>%
  dyLegend(show = "always", width = 400) %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 4, 
              highlightSeriesBackgroundAlpha = 0.3,
              hideOnMouseOut = TRUE)

# 风险提示
message("\n重要提示：")
message("1. 本策略仅为示例，实际投资需考虑交易成本、滑点等因素")
message("2. 历史表现不代表未来收益")
message("3. 建议在不同市场环境下进行充分测试")
message("4. 投资有风险，决策需谨慎")