# 加载必要的包
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(quadprog)
library(xts)

# ================== 参数设置 ==================
# 策略参数
symbols <- c('AAPL', 'BABA', 'TSLA') # 股票池
start_date <- "2020-01-01"
end_date <- "2024-12-31"
short_ma <- 20    # 短期均线周期
long_ma <- 50     # 长期均线周期

# 筛选参数
liquidity_days <- 30    # 流动性计算周期
volatility_days <- 20   # 波动率计算周期
min_turnover <- 1e6     # 最小日均成交额(美元)
max_volatility <- 0.03  # 最大日波动率

# 组合优化参数
optim_method <- "min_var" # 可选：equal, min_var, max_sharpe

# 交易参数
transaction_cost <- 0.001  # 交易费率0.1%
slippage <- 0.001          # 滑点率0.1%
initial_capital <- 1e6     # 初始资金

# ================== 数据准备 ==================
get_data <- function(symbols) {
  env <- new.env()
  getSymbols(symbols, src = 'yahoo', from = start_date, to = end_date, env = env)
  merge_data <- do.call(merge, lapply(env, function(x) Cl(get(x))))
  colnames(merge_data) <- symbols
  return(merge_data)
}

prices <- get_data(symbols)
returns <- na.omit(ROC(prices, type = "discrete"))


# 加载所有股票数据到环境
# 获取所有股票代码对应的文件名

symbols <- tools::file_path_sans_ext(list.files( path = "./data",pattern = "\\.rds$"))

# 批量读取并赋值到全局环境
for(sym in symbols){
  assign(sym, readRDS(paste0("data/",sym, ".rds")), envir = .GlobalEnv)
}

# 验证数据加载（示例）
head(AAPL)  # 直接调用苹果公司数据对象

# ================== 生成交易信号 ==================
generate_signals <- function(prices, short=20, long=50) {
  signals <- list()
  for(sym in colnames(prices)) {
    price <- prices[,sym]
    sma_short <- SMA(price, n = short)
    sma_long <- SMA(price, n = long)
    signal <- ifelse(sma_short > sma_long, 1, -1)
    signals[[sym]] <- lag.xts(signal) # 使用滞后信号避免未来数据
  }
  do.call(merge.xts, signals)
}

signals <- generate_signals(prices, short_ma, long_ma)

# ================== 股票筛选 ==================
filter_stocks <- function(prices, signals, liq_days, vol_days, min_to, max_vol) {
  # 计算流动性（最近n日平均成交额）
  env <- new.env()
  getSymbols(colnames(prices), src = 'yahoo', env = env)
  turnover <- lapply(colnames(prices), function(x) {
    Vo(get(x, envir = env)) * Ad(get(x, envir = env))
  })
  turnover <- do.call(merge, turnover)
  colnames(turnover) <- colnames(prices)
  liquidity <- rollapply(turnover, liq_days, mean)
  
  # 计算波动率（n日收益率标准差）
  volatility <- rollapply(returns, vol_days, sd)
  
  # 筛选条件
  selected <- (liquidity >= min_to) & (volatility <= max_vol)
  return(na.omit(selected))
}

selected <- filter_stocks(prices, signals, liquidity_days, volatility_days,
                          min_turnover, max_volatility)

# ================== 组合优化 ==================
optimize_weights <- function(returns, method) {
  valid_cols <- colnames(returns)[colSums(is.na(returns)) == 0]
  returns <- returns[, valid_cols]
  
  if(method == "equal") {
    n <- ncol(returns)
    return(rep(1/n, n))
  }
  
  if(method == "min_var") {
    cov_matrix <- cov(returns, use = "complete.obs")
    Dmat <- 2 * cov_matrix
    dvec <- rep(0, ncol(returns))
    Amat <- cbind(rep(1, ncol(returns)), diag(ncol(returns)))
    bvec <- c(1, rep(0, ncol(returns)))
    result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
    return(result$solution)
  }
  
  if(method == "max_sharpe") {
    stop("Max Sharpe optimization requires PortfolioAnalytics package")
  }
}

# ================== 优化后的回测引擎 ==================
backtest <- function(prices, signals, weights, tc, slippage, init_cap) {
  # 初始化数据结构
  dates <- index(prices)
  symbols <- colnames(prices)
  
  # 初始化记录器
  portfolio_records <- list(
    position = xts(matrix(0, nrow = nrow(prices), ncol = ncol(prices)),
                   order.by = dates),
    cash = xts(rep(init_cap, nrow(prices)), order.by = dates),
    transactions = data.frame()
  )
  colnames(portfolio_records$position) <- symbols
  
  current_cash <- init_cap
  current_position <- rep(0, ncol(prices))
  names(current_position) <- symbols
  
  for(i in 2:nrow(prices)) {
    current_date <- dates[i]
    prev_date <- dates[i-1]
    
    # 获取当前数据
    current_signal <- as.numeric(signals[i,])
    current_price <- as.numeric(prices[i,])
    target_weights <- weights
    
    # 计算目标市值
    total_asset <- current_cash + sum(current_position * current_price)
    target_value <- total_asset * target_weights
    target_shares <- round(target_value / current_price, 0)
    
    # 计算交易量
    shares_change <- target_shares - current_position
    
    # 记录交易
    if(any(shares_change != 0)) {
      trades <- data.frame(
        Date = current_date,
        Symbol = symbols,
        Shares = shares_change,
        Price = current_price,
        Cost = abs(shares_change) * current_price * tc,
        Slippage = abs(shares_change) * current_price * slippage
      )
      trades <- trades[trades$Shares != 0, ]
      portfolio_records$transactions <- rbind(portfolio_records$transactions, trades)
    }
    
    # 计算交易成本
    total_cost <- sum(abs(shares_change) * current_price * (tc + slippage))
    
    # 更新头寸和现金
    current_position <- target_shares
    current_cash <- total_asset - sum(current_position * current_price) - total_cost
    
    # 保存记录
    portfolio_records$position[i,] <- current_position
    portfolio_records$cash[i] <- current_cash
  }
  
  # 计算净值曲线
  position_value <- rowSums(portfolio_records$position * prices, na.rm = TRUE)
  portfolio_records$equity <- position_value + portfolio_records$cash
  
  return(portfolio_records)
}

# ================== 执行回测 ==================
selected_symbols <- colnames(selected)[colSums(selected) > 0]
optim_weights <- optimize_weights(returns[selected_symbols], optim_method)
names(optim_weights) <- selected_symbols

backtest_results <- backtest(prices = prices[, selected_symbols],
                             signals = signals[, selected_symbols],
                             weights = optim_weights,
                             tc = transaction_cost,
                             slippage = slippage,
                             init_cap = initial_capital)

# ================== 结果分析 ==================
# 净值曲线
plot(backtest_results$equity, main = "Portfolio Equity Curve")

# 持仓分析
head(backtest_results$position)

# 交易记录分析
str(backtest_results$transactions)

# 绩效指标计算
portfolio_returns <- ROC(backtest_results$equity, type = "discrete")
table.AnnualizedReturns(portfolio_returns)
maxDrawdown(portfolio_returns)
charts.PerformanceSummary(portfolio_returns)