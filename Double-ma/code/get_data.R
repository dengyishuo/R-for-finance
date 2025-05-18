# 加载所有股票数据到环境
# 获取所有股票代码对应的文件名

symbols <- tools::file_path_sans_ext(list.files( path = "./data",pattern = "\\.rds$"))

# 批量读取并赋值到全局环境
for(sym in symbols){
  assign(sym, readRDS(paste0("data/",sym, ".rds")), envir = .GlobalEnv)
}

# 验证数据加载（示例）
head(AAPL)  # 直接调用苹果公司数据对象



load_rds_data <- function(data_path = "data/",...){
  # 0. 加载必要包
  library(dplyr)
  # 1. 获取文件列表
  file_list <- list.files(
    path = data_path,        # 指定目录
    pattern = "\\.rds$",     # 匹配RDS后缀
    full.names = TRUE        # 获取完整路径
  )
  
  # 2. 定义读取函数（带股票代码标记）
  read_stock_data <- function(file_path) {
    # 读取RDS文件
    df <- as.data.frame(readRDS(file_path))
    
    # 从文件名提取股票代码（去除路径和后缀）
    stock_code <- tools::file_path_sans_ext(basename(file_path))
    
    # 添加股票代码列
    df %>% mutate(stock_code = stock_code)
  }
  
  # 3. 批量读取所有文件
  data_list <- lapply(file_list, read_stock_data)
  
  # 4. 合并为长格式数据
  combined_data <- bind_rows(data_list)
  # 5. 返回结果
  return(combined_data)
}



# 验证结果
combined_data <- load_rds_data()
glimpse(combined_data)