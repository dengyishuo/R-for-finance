# 安装必要包（若未安装）
# install.packages(c("quantmod", "ggplot2", "TTR", "scales", "gridExtra", "cowplot", "showtext"))

library(quantmod)
library(ggplot2)
library(TTR)
library(scales)
library(gridExtra)
library(cowplot)
library(showtext)

# 设置中文字体
font_add("SimHei", "simhei.ttf")
showtext_auto()

# 获取AAPL数据（最近365天）
getSymbols("AAPL", src = "yahoo", from = Sys.Date() - 365)

# 准备数据
aapl_data <- data.frame(
  Date = index(AAPL),
  Open = as.numeric(Op(AAPL)),
  High = as.numeric(Hi(AAPL)),
  Low = as.numeric(Lo(AAPL)),
  Close = as.numeric(Cl(AAPL)),
  Volume = as.numeric(Vo(AAPL))
)

# 计算技术指标
aapl_data$SMA5 <- SMA(aapl_data$Close, n = 5)
aapl_data$SMA10 <- SMA(aapl_data$Close, n = 10)
aapl_data$SMA20 <- SMA(aapl_data$Close, n = 20)
aapl_data$VolumeSMA5 <- SMA(aapl_data$Volume, n = 5) # 成交量5日均线

# 计算MACD
macd_data <- MACD(aapl_data$Close, nFast = 12, nSlow = 26, nSig = 9)
aapl_data$MACD <- macd_data[, "macd"]
aapl_data$Signal <- macd_data[, "signal"]
aapl_data$Histogram <- aapl_data$MACD - aapl_data$Signal

# 计算RSI
aapl_data$RSI <- RSI(aapl_data$Close, n = 14)

# 计算KDJ
kdj <- stoch(aapl_data[, c("High", "Low", "Close")], nFastK = 14, nFastD = 3, nSlowD = 3)
aapl_data$K <- kdj[, "fastK"] * 100
aapl_data$D <- kdj[, "fastD"] * 100
aapl_data$J <- 3 * aapl_data$K - 2 * aapl_data$D

# 筛选最近6个月的数据
six_months_ago <- Sys.Date() - 180
recent_data <- aapl_data[aapl_data$Date >= six_months_ago, ]

# 1. 蜡烛图 + 均线
candle_plot <- ggplot(recent_data, aes(x = Date)) +
  geom_linerange(aes(ymin = Low, ymax = High), color = "black", linewidth = 0.3) +
  geom_rect(aes(
    xmin = Date - 0.4, xmax = Date + 0.4,
    ymin = pmin(Open, Close), ymax = pmax(Open, Close),
    fill = Close > Open
  ), color = "black") +
  geom_line(aes(y = SMA5, color = "SMA5"), linewidth = 0.5) +
  geom_line(aes(y = SMA10, color = "SMA10"), linewidth = 0.5) +
  geom_line(aes(y = SMA20, color = "SMA20"), linewidth = 0.5) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green"), guide = FALSE) +
  scale_color_manual(values = c("SMA5" = "blue", "SMA10" = "orange", "SMA20" = "purple")) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = alpha("white", 0.9)),
    legend.box = "horizontal",
    legend.margin = margin(2, 2, 2, 2),
    plot.margin = unit(c(5.5, 5.5, 0, 5.5), "points"),
    text = element_text(family = "SimHei")
  ) +
  labs(y = "价格") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# 2. 成交量图（按涨跌着色） + 5日均线
volume_plot <- ggplot(recent_data, aes(x = Date)) +
  geom_col(aes(y = Volume, fill = Close > Open)) +
  geom_line(aes(y = VolumeSMA5, color = "成交量5日均线"), linewidth = 0.2) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green"), guide = FALSE) +
  scale_color_manual(values = c("成交量5日均线" = "blue")) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = alpha("white", 0.9)),
    legend.box = "horizontal",
    legend.margin = margin(2, 2, 2, 2),
    plot.margin = unit(c(0, 5.5, 0, 5.5), "points"),
    text = element_text(family = "SimHei")
  ) +
  labs(y = "成交量") +
  scale_y_continuous(labels = comma) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# 3. MACD图
macd_plot <- ggplot(recent_data, aes(x = Date)) +
  geom_line(aes(y = MACD, color = "MACD"), linewidth = 0.5) +
  geom_line(aes(y = Signal, color = "Signal"), linewidth = 0.5) +
  geom_col(aes(y = Histogram, fill = Histogram > 0), alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("MACD" = "blue", "Signal" = "red")) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green"), guide = FALSE) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = alpha("white", 0.9)),
    legend.box = "horizontal",
    legend.margin = margin(2, 2, 2, 2),
    plot.margin = unit(c(0, 5.5, 0, 5.5), "points"),
    text = element_text(family = "SimHei")
  ) +
  labs(y = "MACD") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# 4. RSI图
rsi_plot <- ggplot(recent_data, aes(x = Date, y = RSI)) +
  geom_line(color = "purple", linewidth = 0.5) +
  geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray") +
  geom_ribbon(aes(ymin = 70, ymax = 100), fill = "red", alpha = 0.1) +
  geom_ribbon(aes(ymin = 0, ymax = 30), fill = "green", alpha = 0.1) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = unit(c(0, 5.5, 0, 5.5), "points"),
    text = element_text(family = "SimHei")
  ) +
  labs(y = "RSI") +
  ylim(0, 100)

# 5. KDJ图
kdj_plot <- ggplot(recent_data, aes(x = Date)) +
  geom_line(aes(y = K, color = "K"), linewidth = 0.5) +
  geom_line(aes(y = D, color = "D"), linewidth = 0.5) +
  geom_line(aes(y = J, color = "J"), linewidth = 0.5) +
  geom_hline(yintercept = c(20, 80), linetype = "dashed", color = "gray") +
  geom_ribbon(aes(ymin = 80, ymax = 100), fill = "red", alpha = 0.1) +
  geom_ribbon(aes(ymin = 0, ymax = 20), fill = "green", alpha = 0.1) +
  scale_color_manual(values = c("K" = "blue", "D" = "red", "J" = "green")) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = alpha("white", 0.9)),
    legend.box = "horizontal",
    legend.margin = margin(2, 2, 2, 2),
    plot.margin = unit(c(0, 5.5, 5.5, 5.5), "points"),
    text = element_text(family = "SimHei")
  ) +
  labs(y = "KDJ") +
  ylim(0, 100) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

# 组合图表
combined_plot <- plot_grid(
  candle_plot,
  volume_plot,
  macd_plot,
  rsi_plot,
  kdj_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(2, 1, 1, 1, 1.2)
)

# 显示图表
print(combined_plot)
