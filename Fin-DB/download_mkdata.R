library(quantmod)
library(DBI)
library(RMariaDB)

# ----------------------------
# 1. 连接到MySQL数据库
# ----------------------------
# 配置MySQL参数
con <- dbConnect(
  MariaDB(),
  host = "localhost",
  user = "dengyishuo",
  password = "880125@Deng",
  dbname = "mkdata",
  port = 3306
)
# ----------------------------
# 2. 创建数据表（如果不存在）
# ----------------------------
# 定义表结构
create_table_sql <- "
CREATE TABLE IF NOT EXISTS stock_data (
  symbol VARCHAR(10),
  date DATE,
  open DECIMAL(18,4),
  high DECIMAL(18,4),
  low DECIMAL(18,4),
  close DECIMAL(18,4),
  volume BIGINT,
  adjusted DECIMAL(18,4),
  PRIMARY KEY (symbol, date)
)"
dbExecute(con, create_table_sql)


# ----------------------------
# 3. 定义批量下载函数
# ----------------------------
download_and_save <- function(symbol) {
  tryCatch(
    {
      # 从Yahoo Finance下载数据
      getSymbols(
        Symbols = symbol,
        src = "yahoo",
        env = globalenv(),
        from = "2000-01-01",
        to = Sys.Date(),
        warnings = FALSE,
        auto.assign = TRUE
      )

      # 提取数据并转换为数据框
      data <- get(symbol)
      df <- data.frame(
        symbol = symbol,
        date = index(data),
        open = as.numeric(Op(data)),
        high = as.numeric(Hi(data)),
        low = as.numeric(Lo(data)),
        close = as.numeric(Cl(data)),
        volume = as.numeric(Vo(data)),
        adjusted = as.numeric(Ad(data))
      )

      # 写入MySQL数据库
      dbWriteTable(
        con,
        name = "stock_data",
        value = df,
        append = TRUE, # 追加模式
        overwrite = FALSE, # 不覆盖原有数据
        row.names = FALSE
      )

      message(sprintf("[成功] %s 数据已保存", symbol))
    },
    error = function(e) {
      message(sprintf("[失败] %s 下载失败: %s", symbol, e$message))
    }
  )
}
# ----------------------------
# 4. 批量下载股票数据
# ----------------------------
# 定义股票代码列表（支持任意市场代码）
symbols <- c("AAPL", "MSFT", "GOOG", "YHOO", "TSLA", "0700.HK", "BTC-USD")

# 遍历所有股票代码
invisible(lapply("YHOO", download_and_save))


# ----------------------------
# 5. 关闭数据库连接
# ----------------------------
dbDisconnect(con)



# 数据库的使用
# ----------------------------
# 1. 连接到MySQL数据库
# ----------------------------
#
library(quantmod)
library(DBI)
library(RMariaDB)

con <- dbConnect(
  MariaDB(),
  host = "localhost",
  user = "dengyishuo",
  password = "880125@Deng",
  dbname = "mkdata",
  port = 3306
)

# 2. 构造SQL查询语句
query <- "SELECT * FROM stock_data WHERE symbol = 'TSLA' AND date BETWEEN '2020-01-01' AND '2025-05-01'"

# 3. 执行查询并处理结果
tryCatch(
  {
    tsla_data <- dbGetQuery(con, query)

    if (nrow(tsla_data) > 0) {
      cat("成功获取", nrow(tsla_data), "条TSLA数据\n")
      print(head(tsla_data)) # 显示前6行数据
    } else {
      message("未找到TSLA相关数据")
    }
  },
  error = function(e) {
    message("数据库查询失败: ", e$message)
  }
)

# 4. 将数据转化为 xts 格式
library(xts)
TSLA <- xts(tsla_data[, 3:8], order.by = as.Date(tsla_data$date))

# 关闭连接（重要）
dbDisconnect(con)
