# 从yahoo批量下载股票数据并写出为rds格式数据

if (!dir.exists("data")) {
  dir.create("data")
}

library(quantmod)
symbols <- c("AAPL","TSLA","BABA","QQQQ","DIA","CHL","TBILL","YHOO")

for (sym in symbols) {
  # 下载股票数据（数据自动加载到当前环境）
  getSymbols(sym, src = "yahoo", from = "2020-01-01", to = Sys.Date())

  # 获取数据对象（使用get函数动态获取）
  stock_data <- get(sym)

  # 保存为.rds文件到data目录
  saveRDS(stock_data,
       file = paste0("data/", sym, ".rds"),
       compress = "xz")

  # 清除临时变量（可选）
  rm(stock_data)
}

# 验证文件保存结果
list.files("data", pattern = "\\.rds$")

######################################################################################

# 加载所有股票数据到环境
# 获取所有股票代码对应的文件名

symbols <- tools::file_path_sans_ext(list.files("data", pattern = "\\.rds$"))

# 批量读取并赋值到全局环境
for(sym in symbols){
  assign(sym, readRDS(paste0("data/", sym, ".rds")), envir = .GlobalEnv)
}

# 验证数据加载（示例）
head(AAPL)  # 直接调用苹果公司数据对象


###############################################
###循环读入csv数据并转化为xts格式#########
##############################################
symbols <- c("AAPL","TSLA","BABA")

for(sym in symbols){
  # 批量读入.csv数据
  assign(sym,read.csv(paste0(sym,".csv")),envir=.GlobalEnv);
  raw_df <- get(sym)
  # 创建xts对象（保留所有数值列，排除日期列）
  xts_data <- xts(
    x = raw_df[, colnames(raw_df) != "Date"],  # 排除日期列
    order.by = as.Date(raw_df$Date)  # 设置时间索引
  )
  # 更新全局环境中的对象
  assign(sym, xts_data, envir = .GlobalEnv)
  rm(xts_data)
  rm(raw_df)
}

# 验证转换结果
class(AAPL)        # 应返回 "xts" "zoo"
head(AAPL$Close)   # 查看苹果收盘价
head(AAPL)


##################################################################
#########批量读入.csv数据并写出为.rds数据
###################################################################
symbols <- c("AAPL","TSLA","BABA")

for(sym in symbols){
  # 批量读入.csv数据
  assign(sym,read.csv(paste0(sym,".csv")),envir=.GlobalEnv);
  raw_df <- get(sym)
  # 创建xts对象（保留所有数值列，排除日期列）
  xts_data <- xts(
    x = raw_df[, colnames(raw_df) != "Date"],  # 排除日期列
    order.by = as.Date(raw_df$Date)  # 设置时间索引
  )
  saveRDS(xts_data,paste0(sym,".rds"))
}

# 加载所有股票数据到环境
# 获取所有股票代码对应的文件名

symbols <- tools::file_path_sans_ext(list.files( pattern = "\\.rds$"))

# 批量读取并赋值到全局环境
for(sym in symbols){
  assign(sym, readRDS(paste0(sym, ".rds")), envir = .GlobalEnv)
}

# 验证数据加载（示例）
head(AAPL)  # 直接调用苹果公司数据对象

