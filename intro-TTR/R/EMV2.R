# 自定义 EMV 函数，添加分母保护
EMV2 <- function(OHLC, n = 14, threshold = 0.01) {
  H <- as.numeric(Hi(OHLC))
  L <- as.numeric(Lo(OHLC))
  V <- as.numeric(Vo(OHLC))

  # 计算价格中心
  midpoint <- (H + L) / 2
  midpoint_change <- midpoint - lag(midpoint)

  # 计算价格区间
  price_range <- H - L

  # 保护：当价格区间过窄时，使用一个极小值替代
  price_range[price_range < threshold] <- threshold

  # 计算 EMV
  emv <- (midpoint_change / (V / price_range)) * 10000

  # 计算移动平均
  emv_ma <- SMA(emv, n = n)

  # 返回结果
  result <- cbind(emv, emv_ma)
  colnames(result) <- c("emv", "maEMV")
  return(result)
}
