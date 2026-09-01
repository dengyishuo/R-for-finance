# 时间序列图在R中的实现

时间序列图用于展示数据随时间的变化趋势，在 R 语言中可通过基础绘图系统（`base`）、`ggplot2`包及专业的时间序列分析包（如`zoo`、`xts`）实现，以下是具体方法：


### 一、基础绘图系统（`base`）实现时间序列图&#xA;



1.  **简单时间序列图绘制**

    使用`plot()`函数，直接输入时间序列对象或 “时间 + 数值” 向量即可。例如，基于内置的`AirPassengers`数据集（1949-1960 年每月航空乘客数）绘制趋势图：




```
\# 查看数据（ts类时间序列，按月记录）
data(AirPassengers)

\# 绘制基础时间序列图
plot(AirPassengers,
&#x20;    main = "1949-1960年航空乘客数量变化",  # 标题
&#x20;    xlab = "年份",  # x轴标签
&#x20;    ylab = "乘客数（千）",  # y轴标签
&#x20;    col = "blue",  # 线条颜色
&#x20;    lwd = 2)  # 线条宽度


data(AirPassengers)

\# 绘制基础时间序列图
plot(AirPassengers,
&#x20;    main = "1949-1960年航空乘客数量变化",  # 标题
&#x20;    xlab = "年份",  # x轴标签
&#x20;    ylab = "乘客数（千）",  # y轴标签
&#x20;    col = "blue",  # 线条颜色
&#x20;    lwd = 2)  # 线条宽度


\# 绘制基础时间序列图
plot(AirPassengers,
&#x20;    main = "1949-1960年航空乘客数量变化",  # 标题
&#x20;    xlab = "年份",  # x轴标签
&#x20;    ylab = "乘客数（千）",  # y轴标签
&#x20;    col = "blue",  # 线条颜色
&#x20;    lwd = 2)  # 线条宽度


plot(AirPassengers,
&#x20;    main = "1949-1960年航空乘客数量变化",  # 标题
&#x20;    xlab = "年份",  # x轴标签
&#x20;    ylab = "乘客数（千）",  # y轴标签
&#x20;    col = "blue",  # 线条颜色
&#x20;    lwd = 2)  # 线条宽度


&#x20;    main = "1949-1960年航空乘客数量变化",  # 标题
&#x20;    xlab = "年份",  # x轴标签
&#x20;    ylab = "乘客数（千）",  # y轴标签
&#x20;    col = "blue",  # 线条颜色
&#x20;    lwd = 2)  # 线条宽度


&#x20;    xlab = "年份",  # x轴标签
&#x20;    ylab = "乘客数（千）",  # y轴标签
&#x20;    col = "blue",  # 线条颜色
&#x20;    lwd = 2)  # 线条宽度


&#x20;    ylab = "乘客数（千）",  # y轴标签
&#x20;    col = "blue",  # 线条颜色
&#x20;    lwd = 2)  # 线条宽度


&#x20;    col = "blue",  # 线条颜色
&#x20;    lwd = 2)  # 线条宽度


&#x20;    lwd = 2)  # 线条宽度
```

`AirPassengers`是`ts`类对象（已包含时间信息），`plot()`会自动按时间顺序绘制线条，清晰呈现长期增长趋势和季节性波动（每年夏季为峰值）。




1.  **多组时间序列对比**

    当存在多组同期数据时，可通过`lines()`函数叠加绘制。例如，对比`EuStockMarkets`数据集中 4 个欧洲股市指数的走势：




```
data(EuStockMarkets)  # 包含DAX、SMI、CAC、FTSE四个指数的周数据

\# 绘制第一个指数（DAX）
plot(EuStockMarkets\[, "DAX"],
&#x20;    main = "欧洲股市指数走势对比",
&#x20;    xlab = "时间（1991-1998）",
&#x20;    ylab = "指数值",
&#x20;    col = "black", lwd = 1.5)
\# 叠加其他指数
lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


\# 绘制第一个指数（DAX）
plot(EuStockMarkets\[, "DAX"],
&#x20;    main = "欧洲股市指数走势对比",
&#x20;    xlab = "时间（1991-1998）",
&#x20;    ylab = "指数值",
&#x20;    col = "black", lwd = 1.5)
\# 叠加其他指数
lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


plot(EuStockMarkets\[, "DAX"],
&#x20;    main = "欧洲股市指数走势对比",
&#x20;    xlab = "时间（1991-1998）",
&#x20;    ylab = "指数值",
&#x20;    col = "black", lwd = 1.5)
\# 叠加其他指数
lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


&#x20;    main = "欧洲股市指数走势对比",
&#x20;    xlab = "时间（1991-1998）",
&#x20;    ylab = "指数值",
&#x20;    col = "black", lwd = 1.5)
\# 叠加其他指数
lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


&#x20;    xlab = "时间（1991-1998）",
&#x20;    ylab = "指数值",
&#x20;    col = "black", lwd = 1.5)
\# 叠加其他指数
lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


&#x20;    ylab = "指数值",
&#x20;    col = "black", lwd = 1.5)
\# 叠加其他指数
lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


&#x20;    col = "black", lwd = 1.5)
\# 叠加其他指数
lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


\# 叠加其他指数
lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


lines(EuStockMarkets\[, "SMI"], col = "red", lwd = 1.5)
lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


lines(EuStockMarkets\[, "CAC"], col = "green", lwd = 1.5)
lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


lines(EuStockMarkets\[, "FTSE"], col = "blue", lwd = 1.5)
\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


\# 添加图例
legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


legend("topleft", legend = colnames(EuStockMarkets),
&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)


&#x20;      col = c("black", "red", "green", "blue"), lwd = 1.5)
```

多组线条通过颜色区分，可直观观察指数的整体趋势一致性（如整体上涨）和局部差异（如 DAX 波动更大）。


### 二、ggplot2 包实现时间序列图（灵活方法）&#xA;

ggplot2 需将时间序列数据转换为数据框（包含时间列和数值列），通过`geom_line()`绘制，适合自定义样式和添加统计信息。




1.  **基础时间序列图**

    使用`ggplot()`+`geom_line()`函数，输入包含时间和数值的数据框。例如，基于`economics`数据集（美国经济指标月度数据）绘制失业率走势：




```
library(ggplot2)
data(economics)  # 包含date（日期）、unemploy（失业率）等列

\# 绘制基础时间序列图
ggplot(economics, aes(x = date, y = unemploy)) +
&#x20; geom\_line(color = "darkred", linewidth = 1) +  # 绘制线条
&#x20; labs(title = "美国失业率月度变化（1967-2015）",
&#x20;      x = "年份", y = "失业率（千人）") +
&#x20; theme\_minimal() +
&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签


data(economics)  # 包含date（日期）、unemploy（失业率）等列

\# 绘制基础时间序列图
ggplot(economics, aes(x = date, y = unemploy)) +
&#x20; geom\_line(color = "darkred", linewidth = 1) +  # 绘制线条
&#x20; labs(title = "美国失业率月度变化（1967-2015）",
&#x20;      x = "年份", y = "失业率（千人）") +
&#x20; theme\_minimal() +
&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签


\# 绘制基础时间序列图
ggplot(economics, aes(x = date, y = unemploy)) +
&#x20; geom\_line(color = "darkred", linewidth = 1) +  # 绘制线条
&#x20; labs(title = "美国失业率月度变化（1967-2015）",
&#x20;      x = "年份", y = "失业率（千人）") +
&#x20; theme\_minimal() +
&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签


ggplot(economics, aes(x = date, y = unemploy)) +
&#x20; geom\_line(color = "darkred", linewidth = 1) +  # 绘制线条
&#x20; labs(title = "美国失业率月度变化（1967-2015）",
&#x20;      x = "年份", y = "失业率（千人）") +
&#x20; theme\_minimal() +
&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签


&#x20; geom\_line(color = "darkred", linewidth = 1) +  # 绘制线条
&#x20; labs(title = "美国失业率月度变化（1967-2015）",
&#x20;      x = "年份", y = "失业率（千人）") +
&#x20; theme\_minimal() +
&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签


&#x20; labs(title = "美国失业率月度变化（1967-2015）",
&#x20;      x = "年份", y = "失业率（千人）") +
&#x20; theme\_minimal() +
&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签


&#x20;      x = "年份", y = "失业率（千人）") +
&#x20; theme\_minimal() +
&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签


&#x20; theme\_minimal() +
&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签


&#x20; theme(axis.text.x = element\_text(angle = 45, hjust = 1))  # 旋转x轴标签
```

`economics$date`是`Date`类对象，ggplot2 自动识别并按时间顺序排列，`geom_line()`连接数据点形成趋势线，适合展示长期时间序列的变化。




1.  **添加趋势线与季节性分解**

    结合`geom_smooth()`添加平滑趋势线，或通过`stl()`函数分解季节性成分并可视化。例如，分析`AirPassengers`的趋势与季节性：




```
\# 将ts对象转换为数据框（便于ggplot2处理）
air\_df <- data.frame(
&#x20; date = as.Date(time(AirPassengers)),  # 转换时间为Date类
&#x20; passengers = as.numeric(AirPassengers)
)

\# 绘制带平滑趋势线的时间序列图
ggplot(air\_df, aes(x = date, y = passengers)) +
&#x20; geom\_line(color = "gray50") +  # 原始数据线条（灰色）
&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


air\_df <- data.frame(
&#x20; date = as.Date(time(AirPassengers)),  # 转换时间为Date类
&#x20; passengers = as.numeric(AirPassengers)
)

\# 绘制带平滑趋势线的时间序列图
ggplot(air\_df, aes(x = date, y = passengers)) +
&#x20; geom\_line(color = "gray50") +  # 原始数据线条（灰色）
&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


&#x20; date = as.Date(time(AirPassengers)),  # 转换时间为Date类
&#x20; passengers = as.numeric(AirPassengers)
)

\# 绘制带平滑趋势线的时间序列图
ggplot(air\_df, aes(x = date, y = passengers)) +
&#x20; geom\_line(color = "gray50") +  # 原始数据线条（灰色）
&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


&#x20; passengers = as.numeric(AirPassengers)
)

\# 绘制带平滑趋势线的时间序列图
ggplot(air\_df, aes(x = date, y = passengers)) +
&#x20; geom\_line(color = "gray50") +  # 原始数据线条（灰色）
&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


)

\# 绘制带平滑趋势线的时间序列图
ggplot(air\_df, aes(x = date, y = passengers)) +
&#x20; geom\_line(color = "gray50") +  # 原始数据线条（灰色）
&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


\# 绘制带平滑趋势线的时间序列图
ggplot(air\_df, aes(x = date, y = passengers)) +
&#x20; geom\_line(color = "gray50") +  # 原始数据线条（灰色）
&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


ggplot(air\_df, aes(x = date, y = passengers)) +
&#x20; geom\_line(color = "gray50") +  # 原始数据线条（灰色）
&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


&#x20; geom\_line(color = "gray50") +  # 原始数据线条（灰色）
&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


&#x20; geom\_smooth(method = "loess", span = 0.2, color = "blue") +  # 局部平滑趋势线
&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


&#x20; labs(title = "航空乘客数量变化（含趋势线）",
&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


&#x20;      x = "年份", y = "乘客数（千）") +
&#x20; theme\_bw()


&#x20; theme\_bw()
```

`geom_smooth(method = "loess")`添加局部加权回归曲线，消除短期波动，突出长期趋势；`span`参数控制平滑程度（值越小越接近原始数据）。




1.  **多组数据分组展示与 facet**

    当数据包含分组变量时，可通过`color`参数区分或`facet_wrap()`分面展示。例如，使用`tidyr`包的`pedestrian`数据集（简化版，不同地点的行人流量）：




```
library(tidyr)
library(dplyr)

\# 模拟数据：3个地点的每日行人流量（2023年1-3月）
set.seed(123)
dates <- seq(as.Date("2023-01-01"), as.Date("2023-03-31"), by = "day")
pedestrian <- data.frame(
&#x20; date = rep(dates, 3),
&#x20; location = rep(c("A", "B", "C"), each = length(dates)),
&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


library(dplyr)

\# 模拟数据：3个地点的每日行人流量（2023年1-3月）
set.seed(123)
dates <- seq(as.Date("2023-01-01"), as.Date("2023-03-31"), by = "day")
pedestrian <- data.frame(
&#x20; date = rep(dates, 3),
&#x20; location = rep(c("A", "B", "C"), each = length(dates)),
&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


\# 模拟数据：3个地点的每日行人流量（2023年1-3月）
set.seed(123)
dates <- seq(as.Date("2023-01-01"), as.Date("2023-03-31"), by = "day")
pedestrian <- data.frame(
&#x20; date = rep(dates, 3),
&#x20; location = rep(c("A", "B", "C"), each = length(dates)),
&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


set.seed(123)
dates <- seq(as.Date("2023-01-01"), as.Date("2023-03-31"), by = "day")
pedestrian <- data.frame(
&#x20; date = rep(dates, 3),
&#x20; location = rep(c("A", "B", "C"), each = length(dates)),
&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


dates <- seq(as.Date("2023-01-01"), as.Date("2023-03-31"), by = "day")
pedestrian <- data.frame(
&#x20; date = rep(dates, 3),
&#x20; location = rep(c("A", "B", "C"), each = length(dates)),
&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


pedestrian <- data.frame(
&#x20; date = rep(dates, 3),
&#x20; location = rep(c("A", "B", "C"), each = length(dates)),
&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20; date = rep(dates, 3),
&#x20; location = rep(c("A", "B", "C"), each = length(dates)),
&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20; location = rep(c("A", "B", "C"), each = length(dates)),
&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20; count = c(rnorm(length(dates), 500, 50),  # 地点A
&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20;          rnorm(length(dates), 800, 80),  # 地点B
&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20;          rnorm(length(dates), 300, 30))  # 地点C
)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


)

\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


\# 分面展示各地点的流量趋势
ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


ggplot(pedestrian, aes(x = date, y = count, color = location)) +
&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20; geom\_line() +
&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20; facet\_wrap(\~ location, ncol = 1) +  # 按地点分面（纵向排列）
&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20; labs(title = "不同地点的行人流量变化", x = "日期", y = "流量（人）") +
&#x20; theme\_light()


&#x20; theme\_light()
```

`facet_wrap(~ location)`将不同地点的时间序列分面展示，避免单图中线条重叠，适合多组时间序列的独立对比。




1.  **自定义时间刻度与格式**

    通过`scale_x_date()`调整时间轴的刻度间隔和标签格式，使时间序列更易读。例如，优化`economics`数据的年份显示：




```
ggplot(economics, aes(x = date, y = psavert)) +  # psavert：个人储蓄率
&#x20; geom\_line(color = "darkgreen") +
&#x20; scale\_x\_date(
&#x20;   date\_breaks = "5 years",  # 每5年一个刻度
&#x20;   date\_labels = "%Y"  # 标签格式为四位数年份（如2000）
&#x20; ) +
&#x20; labs(title = "美国个人储蓄率变化（1967-2015）",
&#x20;      x = "年份", y = "储蓄率（%）") +
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; geom\_line(color = "darkgreen") +
&#x20; scale\_x\_date(
&#x20;   date\_breaks = "5 years",  # 每5年一个刻度
&#x20;   date\_labels = "%Y"  # 标签格式为四位数年份（如2000）
&#x20; ) +
&#x20; labs(title = "美国个人储蓄率变化（1967-2015）",
&#x20;      x = "年份", y = "储蓄率（%）") +
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; scale\_x\_date(
&#x20;   date\_breaks = "5 years",  # 每5年一个刻度
&#x20;   date\_labels = "%Y"  # 标签格式为四位数年份（如2000）
&#x20; ) +
&#x20; labs(title = "美国个人储蓄率变化（1967-2015）",
&#x20;      x = "年份", y = "储蓄率（%）") +
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20;   date\_breaks = "5 years",  # 每5年一个刻度
&#x20;   date\_labels = "%Y"  # 标签格式为四位数年份（如2000）
&#x20; ) +
&#x20; labs(title = "美国个人储蓄率变化（1967-2015）",
&#x20;      x = "年份", y = "储蓄率（%）") +
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20;   date\_labels = "%Y"  # 标签格式为四位数年份（如2000）
&#x20; ) +
&#x20; labs(title = "美国个人储蓄率变化（1967-2015）",
&#x20;      x = "年份", y = "储蓄率（%）") +
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; ) +
&#x20; labs(title = "美国个人储蓄率变化（1967-2015）",
&#x20;      x = "年份", y = "储蓄率（%）") +
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; labs(title = "美国个人储蓄率变化（1967-2015）",
&#x20;      x = "年份", y = "储蓄率（%）") +
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20;      x = "年份", y = "储蓄率（%）") +
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中
```

`date_breaks`和`date_labels`参数分别控制刻度间隔和显示格式（`%Y`表示年份，`%m-%Y`表示月 - 年），适合长周期时间序列的刻度优化。


### 三、专业时间序列包（`zoo`/`xts`）的应用&#xA;

对于高频或不规则时间序列（如分钟级数据、非连续日期），`zoo`和`xts`包提供更高效的处理功能，结合 ggplot2 可实现灵活可视化：




```
library(zoo)
library(xts)

\# 创建不规则时间序列（zoo对象）
dates <- as.Date(c("2023-01-01", "2023-01-03", "2023-01-06"))
values <- c(10, 15, 12)
zoo\_ts <- zoo(values, order.by = dates)

\# 转换为数据框并绘图
zoo\_df <- data.frame(date = index(zoo\_ts), value = coredata(zoo\_ts))
ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


library(xts)

\# 创建不规则时间序列（zoo对象）
dates <- as.Date(c("2023-01-01", "2023-01-03", "2023-01-06"))
values <- c(10, 15, 12)
zoo\_ts <- zoo(values, order.by = dates)

\# 转换为数据框并绘图
zoo\_df <- data.frame(date = index(zoo\_ts), value = coredata(zoo\_ts))
ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


\# 创建不规则时间序列（zoo对象）
dates <- as.Date(c("2023-01-01", "2023-01-03", "2023-01-06"))
values <- c(10, 15, 12)
zoo\_ts <- zoo(values, order.by = dates)

\# 转换为数据框并绘图
zoo\_df <- data.frame(date = index(zoo\_ts), value = coredata(zoo\_ts))
ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


dates <- as.Date(c("2023-01-01", "2023-01-03", "2023-01-06"))
values <- c(10, 15, 12)
zoo\_ts <- zoo(values, order.by = dates)

\# 转换为数据框并绘图
zoo\_df <- data.frame(date = index(zoo\_ts), value = coredata(zoo\_ts))
ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


values <- c(10, 15, 12)
zoo\_ts <- zoo(values, order.by = dates)

\# 转换为数据框并绘图
zoo\_df <- data.frame(date = index(zoo\_ts), value = coredata(zoo\_ts))
ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


zoo\_ts <- zoo(values, order.by = dates)

\# 转换为数据框并绘图
zoo\_df <- data.frame(date = index(zoo\_ts), value = coredata(zoo\_ts))
ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


\# 转换为数据框并绘图
zoo\_df <- data.frame(date = index(zoo\_ts), value = coredata(zoo\_ts))
ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


zoo\_df <- data.frame(date = index(zoo\_ts), value = coredata(zoo\_ts))
ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


ggplot(zoo\_df, aes(x = date, y = value)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


&#x20; geom\_line(color = "purple") +
&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


&#x20; geom\_point(size = 3, color = "red") +  # 突出显示数据点
&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


&#x20; labs(title = "不规则时间序列图", x = "日期", y = "数值") +
&#x20; theme\_light()


&#x20; theme\_light()
```

`zoo`包的`index()`和`coredata()`函数分别提取时间和数值，适合处理非均匀间隔的时间序列，确保缺失日期不被误判为零值。


### 四、关键参数与场景选择&#xA;



*   **数据类型**：基础系统适合`ts`类规则时间序列（如月度、季度数据）；ggplot2 需`Date`或`POSIXct`类时间列，适合灵活样式调整；`zoo`/`xts`适合高频或不规则时间序列。


*   **趋势展示**：短期波动用`geom_line()`直接绘制，长期趋势用`geom_smooth()`添加平滑线，季节性数据可结合分面或颜色区分不同季节。


*   **时间刻度**：根据数据周期调整`date_breaks`（如日数据用 “week”，年数据用 “5 years”），避免刻度拥挤。


*   **场景适配**：基础系统适合快速探索，ggplot2 适合制作汇报级图表，`zoo`/`xts`适合专业时间序列分析（如金融高频数据）。


时间序列图在经济预测、销售分析、环境监测等领域应用广泛，通过 R 语言的上述方法，可灵活呈现数据随时间的变化规律，辅助识别趋势、季节性和异常点。


> （注：文档部分内容可能由 AI 生成）
>