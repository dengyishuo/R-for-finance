# 1. 连接到MySQL数据库
con <- dbConnect(
  MariaDB(),
  host = "localhost",
  user = "dengyishuo",
  password = "880125@Deng",
  dbname = "mkdata",
  port = 3306
)

# 2. 构造SQL查询语句
query1 <- "SELECT * FROM stock_data WHERE symbol = 'TSLA' AND date BETWEEN '2020-01-01' AND '2025-05-01'"
query2 <- "SELECT * FROM stock_data WHERE symbol = 'AAPL' AND date BETWEEN '2020-01-01' AND '2025-05-01'"

# 3. 数据查询
# TSLA数据查询处理
tryCatch(
  {
    tsla_data <- dbGetQuery(con, query1)

    if (nrow(tsla_data) > 0) {
      cat("成功获取", nrow(tsla_data), "条TSLA数据\n")
      print(head(tsla_data)) # 显示前6行数据
    } else {
      message("未找到TSLA相关数据")
    }
  },
  error = function(e) {
    message("TSLA数据查询失败: ", e$message)
  }
)

# AAPL数据查询处理
tryCatch(
  {
    aapl_data <- dbGetQuery(con, query2)

    if (nrow(aapl_data) > 0) {
      cat("成功获取", nrow(aapl_data), "条AAPL数据\n") # 修正股票代码显示
      print(head(aapl_data)) # 显示前6行数据
    } else {
      message("未找到AAPL相关数据") # 修正股票代码显示
    }
  },
  error = function(e) {
    message("AAPL数据查询失败: ", e$message) # 修正错误提示
  }
)

# 4. 将数据转化为 xts 格式

TSLA <- xts(tsla_data[, 3:8], order.by = as.Date(tsla_data$date))
colnames(TSLA) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

AAPL <- xts(aapl_data[, 3:8], order.by = as.Date(aapl_data$date))
colnames(AAPL) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

# 5. 关闭连接（重要）
dbDisconnect(con)
