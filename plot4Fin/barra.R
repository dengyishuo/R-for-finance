# 加载必要包
library(dplyr)
library(lubridate)
library(PerformanceAnalytics)
library(xts)

# 示例数据格式（需替换为Wind导出的真实数据）
# stock_data 应包含以下字段：date, symbol, market_cap, close
# 模拟数据生成
set.seed(123)
dates <- seq.Date(as.Date("2018-01-01"), as.Date("2020-12-31"), by = "day")
symbols <- paste0("Stock", 1:500)
stock_data <- expand.grid(date = dates, symbol = symbols) %>%
  mutate(
    market_cap = runif(n(), 1e8, 1e10),  # 随机生成市值
    close = runif(n(), 10, 100)         # 随机生成收盘价
  )

# 参数设置
N <- 30                   # 每次调仓选取股票数量
transaction_cost <- 0.001 # 单边交易成本

# 生成交易日序列
trading_dates <- unique(stock_data$date) %>% sort()

# 调仓日期生成函数（已修复）
generate_rebalance_dates <- function(trading_dates, weeks = 1) {
  start_date <- min(trading_dates)
  end_date <- max(trading_dates)

  # 生成理论调仓日（每周起始）
  theoretical_dates <- seq(start_date, end_date, by = paste(weeks, "weeks"))

  # 对齐到最近的交易日（修复核心逻辑）
  rebalance_dates <- lapply(theoretical_dates, function(d) {
    candidates <- trading_dates[trading_dates >= d]
    if (length(candidates) == 0) return(NA)
    candidates
  }) %>%
    unlist() %>%
    na.omit() %>%
    as.Date(origin = "1970-01-01")

  return(rebalance_dates)
}

# 回测函数
backtest_strategy <- function(data, rebalance_dates, n_stocks = N, cost = transaction_cost) {
  returns <- numeric(length(rebalance_dates) - 1)
  prev_stocks <- character(0)

  for (i in 1:(length(rebalance_dates) - 1)) {
    current_date <- rebalance_dates[i]
    next_date <- rebalance_dates[i + 1]

    # 获取当前可投资股票
    current_data <- data %>% filter(date == current_date)
    selected_stocks <- current_data %>%
      arrange(market_cap) %>%
      head(n_stocks) %>%
      pull(symbol)

    # 计算调仓成本（卖出旧组合+买入新组合）
    if (i > 1) {
      turnover <- length(setdiff(selected_stocks, prev_stocks)) / n_stocks
      cost_penalty <- turnover * cost * 2  # 双边成本
    } else {
      cost_penalty <- 0
    }

    # 计算持有期收益
    period_returns <- data %>%
      filter(date > current_date & date <= next_date) %>%
      group_by(symbol) %>%
      filter(symbol %in% selected_stocks) %>%
      summarise(
        entry_price = first(close),
        exit_price = last(close),
        .groups = 'drop'
      ) %>%
      mutate(ret = exit_price / entry_price - 1) %>%
      pull(ret)

    # 等权重组合收益（考虑交易成本）
    period_ret <- mean(period_returns, na.rm = TRUE) - cost_penalty
    returns[i] <- ifelse(is.na(period_ret), 0, period_ret)

    prev_stocks <- selected_stocks
  }

  # 转换为xts对象
  dates_vec <- rebalance_dates[-1]
  returns_xts <- xts(returns, order.by = dates_vec)
  return(returns_xts)
}

# 参数优化（测试1-4周轮动周期）
results <- list()
for (weeks in 1:4) {
  rebalance_dates <- generate_rebalance_dates(trading_dates, weeks)
  if (length(rebalance_dates) < 2) next  # 跳过不足两次调仓的情况

  returns <- backtest_strategy(stock_data, rebalance_dates)
  metrics <- list(
    AnnualizedReturn = Return.annualized(returns),
    AnnualizedVolatility = StdDev.annualized(returns),
    SharpeRatio = SharpeRatio.annualized(returns),
    MaxDrawdown = maxDrawdown(returns)
  )

  results[[paste(weeks, "周")]] <- metrics
}

# 结果比较
result_table <- bind_rows(results, .id = "周期") %>%
  mutate(across(where(is.numeric), ~round(., 3)))

print(result_table)

# 可视化最佳周期收益曲线
best_period <- result_table %>%
  arrange(desc(SharpeRatio)) %>%
  head(1) %>% pull(周期)

best_returns <- backtest_strategy(
  stock_data,
  generate_rebalance_dates(trading_dates, as.numeric(gsub("\\D", "", best_period)))
)

chart.CumReturns(best_returns, main = paste("最佳轮动周期:", best_period))
