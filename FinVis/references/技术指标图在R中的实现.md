# 技术指标图在R中的实现

技术指标图是金融分析中判断价格趋势和市场情绪的重要工具，在 R 语言中可通过`quantmod`包和`TTR`包实现，结合基础绘图或`ggplot2`包展示，以下是具体方法：


### 一、核心包与数据准备&#xA;

技术指标计算依赖`TTR`包（包含各类技术指标函数），绘图可结合`quantmod`的快捷函数或`ggplot2`的自定义图层。需先安装并加载包：




```
install.packages(c("quantmod", "TTR"))
library(quantmod)
library(TTR)


library(quantmod)
library(TTR)


library(TTR)
```

以苹果公司（AAPL）2023 年数据为例，获取并预处理数据：




```
\# 获取数据（雅虎财经源）
getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2023-12-31")
\# 提取收盘价（后续指标计算基础）
aapl\_close <- Cl(AAPL)  # Cl()函数从quantmod对象中提取收盘价


getSymbols("AAPL", src = "yahoo", from = "2023-01-01", to = "2023-12-31")
\# 提取收盘价（后续指标计算基础）
aapl\_close <- Cl(AAPL)  # Cl()函数从quantmod对象中提取收盘价


\# 提取收盘价（后续指标计算基础）
aapl\_close <- Cl(AAPL)  # Cl()函数从quantmod对象中提取收盘价


aapl\_close <- Cl(AAPL)  # Cl()函数从quantmod对象中提取收盘价
```

### 二、常用技术指标图的实现&#xA;

#### 1. 移动平均线（MA）&#xA;

移动平均线是最基础的趋势指标，包括简单移动平均线（SMA）和指数移动平均线（EMA）。




*   `quantmod`**快捷绘图**：




```
\# 绘制收盘价与均线
chartSeries(AAPL, name = "AAPL收盘价与均线", type = "line")
addSMA(n = 5, col = "blue")  # 5日简单移动平均线
addSMA(n = 20, col = "red")  # 20日简单移动平均线
addEMA(n = 50, col = "green")  # 50日指数移动平均线


chartSeries(AAPL, name = "AAPL收盘价与均线", type = "line")
addSMA(n = 5, col = "blue")  # 5日简单移动平均线
addSMA(n = 20, col = "red")  # 20日简单移动平均线
addEMA(n = 50, col = "green")  # 50日指数移动平均线


addSMA(n = 5, col = "blue")  # 5日简单移动平均线
addSMA(n = 20, col = "red")  # 20日简单移动平均线
addEMA(n = 50, col = "green")  # 50日指数移动平均线


addSMA(n = 20, col = "red")  # 20日简单移动平均线
addEMA(n = 50, col = "green")  # 50日指数移动平均线


addEMA(n = 50, col = "green")  # 50日指数移动平均线
```

`addSMA()`和`addEMA()`直接在价格图上叠加均线，不同周期的均线交叉（如 5 日线上穿 20 日线）可作为趋势信号。




*   `ggplot2`**自定义绘图**：




```
library(ggplot2)
library(dplyr)

\# 转换数据为数据框并计算均线
aapl\_df <- data.frame(
&#x20; date = index(AAPL),
&#x20; close = as.numeric(aapl\_close),
&#x20; sma5 = SMA(aapl\_close, n = 5),
&#x20; sma20 = SMA(aapl\_close, n = 20)
)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


library(dplyr)

\# 转换数据为数据框并计算均线
aapl\_df <- data.frame(
&#x20; date = index(AAPL),
&#x20; close = as.numeric(aapl\_close),
&#x20; sma5 = SMA(aapl\_close, n = 5),
&#x20; sma20 = SMA(aapl\_close, n = 20)
)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


\# 转换数据为数据框并计算均线
aapl\_df <- data.frame(
&#x20; date = index(AAPL),
&#x20; close = as.numeric(aapl\_close),
&#x20; sma5 = SMA(aapl\_close, n = 5),
&#x20; sma20 = SMA(aapl\_close, n = 20)
)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


aapl\_df <- data.frame(
&#x20; date = index(AAPL),
&#x20; close = as.numeric(aapl\_close),
&#x20; sma5 = SMA(aapl\_close, n = 5),
&#x20; sma20 = SMA(aapl\_close, n = 20)
)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; date = index(AAPL),
&#x20; close = as.numeric(aapl\_close),
&#x20; sma5 = SMA(aapl\_close, n = 5),
&#x20; sma20 = SMA(aapl\_close, n = 20)
)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; close = as.numeric(aapl\_close),
&#x20; sma5 = SMA(aapl\_close, n = 5),
&#x20; sma20 = SMA(aapl\_close, n = 20)
)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; sma5 = SMA(aapl\_close, n = 5),
&#x20; sma20 = SMA(aapl\_close, n = 20)
)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; sma20 = SMA(aapl\_close, n = 20)
)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


)

\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


\# 绘制均线图
ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


ggplot(aapl\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; geom\_line(aes(y = close, color = "收盘价"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; geom\_line(aes(y = sma5, color = "5日均线"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; geom\_line(aes(y = sma20, color = "20日均线"), linewidth = 0.8) +
&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; scale\_color\_manual(values = c("black", "blue", "red")) +
&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; labs(title = "AAPL收盘价与移动平均线", x = "日期", y = "价格（美元）", color = "指标") +
&#x20; theme\_minimal()


&#x20; theme\_minimal()
```

#### 2. 指数平滑异同平均线（MACD）&#xA;

MACD 由快线（DIF）、慢线（DEA）和柱状线（MACD）组成，用于判断趋势强度和转折。




*   `quantmod`**实现**：




```
\# 绘制价格与MACD指标（副图）
chartSeries(AAPL, name = "AAPL价格与MACD")
addMACD(fast = 12, slow = 26, signal = 9, col = c("red", "blue", "green"))


chartSeries(AAPL, name = "AAPL价格与MACD")
addMACD(fast = 12, slow = 26, signal = 9, col = c("red", "blue", "green"))


addMACD(fast = 12, slow = 26, signal = 9, col = c("red", "blue", "green"))
```

`addMACD()`默认在主图下方添加副图，展示 MACD 柱状线（红绿交替）和快慢线，柱状线由正转负（或反之）常作为趋势反转信号。




*   **手动计算与**`ggplot2`**绘图**：




```
\# 计算MACD指标
macd <- MACD(aapl\_close, fast = 12, slow = 26, signal = 9)

\# 合并数据
macd\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; dif = macd\[, "macd"],
&#x20; dea = macd\[, "signal"],
&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


macd <- MACD(aapl\_close, fast = 12, slow = 26, signal = 9)

\# 合并数据
macd\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; dif = macd\[, "macd"],
&#x20; dea = macd\[, "signal"],
&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


\# 合并数据
macd\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; dif = macd\[, "macd"],
&#x20; dea = macd\[, "signal"],
&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


macd\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; dif = macd\[, "macd"],
&#x20; dea = macd\[, "signal"],
&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; dif = macd\[, "macd"],
&#x20; dea = macd\[, "signal"],
&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; close = as.numeric(aapl\_close),
&#x20; dif = macd\[, "macd"],
&#x20; dea = macd\[, "signal"],
&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; dif = macd\[, "macd"],
&#x20; dea = macd\[, "signal"],
&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; dea = macd\[, "signal"],
&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; macd\_bar = macd\[, "macd"] - macd\[, "signal"]  # 柱状线 = DIF - DEA
)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


)

\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


\# 绘制主图（价格）和副图（MACD）
library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


library(gridExtra)  # 用于组合图形
p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


p1 <- ggplot(macd\_df, aes(x = date, y = close)) +
&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; geom\_line(color = "black") +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; theme\_minimal()

p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


p2 <- ggplot(macd\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; geom\_line(aes(y = dif, color = "DIF"), linewidth = 0.8) +
&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; geom\_line(aes(y = dea, color = "DEA"), linewidth = 0.8) +
&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; geom\_col(aes(y = macd\_bar, fill = macd\_bar > 0), alpha = 0.5) +
&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; scale\_fill\_manual(values = c("red", "green")) +
&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; scale\_color\_manual(values = c("blue", "orange")) +
&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; labs(title = "MACD指标", x = "日期", y = "值", color = "", fill = "") +
&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; theme\_minimal() +
&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


&#x20; theme(legend.position = "bottom")

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）


grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))  # 主副图组合（高度比2:1）
```

#### 3. 相对强弱指数（RSI）&#xA;

RSI 衡量资产超买（>70）或超卖（<30）状态，取值范围 0-100。




*   `quantmod`**实现**：




```
chartSeries(AAPL, name = "AAPL价格与RSI")
addRSI(n = 14, col = "purple")  # 14日RSI指标
abline(h = c(30, 70), col = "red", lty = 2)  # 添加超买超卖警戒线


addRSI(n = 14, col = "purple")  # 14日RSI指标
abline(h = c(30, 70), col = "red", lty = 2)  # 添加超买超卖警戒线


abline(h = c(30, 70), col = "red", lty = 2)  # 添加超买超卖警戒线
```

`addRSI()`在副图中绘制 RSI 曲线，配合 30 和 70 的水平线，直观判断市场情绪。




*   `ggplot2`**实现**：




```
\# 计算14日RSI
rsi <- RSI(aapl\_close, n = 14)

rsi\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; rsi = as.numeric(rsi)
)

\# 绘制主图和RSI副图
p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


rsi <- RSI(aapl\_close, n = 14)

rsi\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; rsi = as.numeric(rsi)
)

\# 绘制主图和RSI副图
p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


rsi\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; rsi = as.numeric(rsi)
)

\# 绘制主图和RSI副图
p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; rsi = as.numeric(rsi)
)

\# 绘制主图和RSI副图
p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; close = as.numeric(aapl\_close),
&#x20; rsi = as.numeric(rsi)
)

\# 绘制主图和RSI副图
p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; rsi = as.numeric(rsi)
)

\# 绘制主图和RSI副图
p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


)

\# 绘制主图和RSI副图
p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


\# 绘制主图和RSI副图
p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


p1 <- ggplot(rsi\_df, aes(x = date, y = close)) +
&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; geom\_line() +
&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; labs(title = "AAPL收盘价", x = "", y = "价格") +
&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; theme\_minimal()

p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


p2 <- ggplot(rsi\_df, aes(x = date, y = rsi)) +
&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; geom\_line(color = "purple") +
&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; geom\_hline(yintercept = c(30, 70), color = "red", linetype = 2) +
&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; ylim(0, 100) +  # RSI取值范围固定为0-100
&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; labs(title = "14日RSI指标", x = "日期", y = "RSI值") +
&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


&#x20; theme\_minimal()

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))
```

#### 4. 布林带（Bollinger Bands）&#xA;

布林带由中轨（均线）和上下轨（中轨 ±2 倍标准差）组成，用于衡量价格波动范围。




*   `quantmod`**实现**：




```
chartSeries(AAPL, name = "AAPL价格与布林带")
addBBands(n = 20, sd = 2, col = c("black", "blue", "blue"))  # 20日布林带


addBBands(n = 20, sd = 2, col = c("black", "blue", "blue"))  # 20日布林带
```

中轨为 20 日均线，上下轨随价格波动扩张或收缩，价格触及上轨可能超买，触及下轨可能超卖。




*   `ggplot2`**实现**：




```
\# 计算布林带
bbands <- BBands(aapl\_close, n = 20, sd = 2)

bbands\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; mid = bbands\[, "mavg"],  # 中轨
&#x20; upper = bbands\[, "up"],  # 上轨
&#x20; lower = bbands\[, "dn"]   # 下轨
)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


bbands <- BBands(aapl\_close, n = 20, sd = 2)

bbands\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; mid = bbands\[, "mavg"],  # 中轨
&#x20; upper = bbands\[, "up"],  # 上轨
&#x20; lower = bbands\[, "dn"]   # 下轨
)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


bbands\_df <- data.frame(
&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; mid = bbands\[, "mavg"],  # 中轨
&#x20; upper = bbands\[, "up"],  # 上轨
&#x20; lower = bbands\[, "dn"]   # 下轨
)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; date = index(aapl\_close),
&#x20; close = as.numeric(aapl\_close),
&#x20; mid = bbands\[, "mavg"],  # 中轨
&#x20; upper = bbands\[, "up"],  # 上轨
&#x20; lower = bbands\[, "dn"]   # 下轨
)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; close = as.numeric(aapl\_close),
&#x20; mid = bbands\[, "mavg"],  # 中轨
&#x20; upper = bbands\[, "up"],  # 上轨
&#x20; lower = bbands\[, "dn"]   # 下轨
)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; mid = bbands\[, "mavg"],  # 中轨
&#x20; upper = bbands\[, "up"],  # 上轨
&#x20; lower = bbands\[, "dn"]   # 下轨
)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; upper = bbands\[, "up"],  # 上轨
&#x20; lower = bbands\[, "dn"]   # 下轨
)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; lower = bbands\[, "dn"]   # 下轨
)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


)

\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


\# 绘制布林带
ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


ggplot(bbands\_df, aes(x = date)) +
&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; geom\_line(aes(y = close, color = "收盘价")) +
&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; geom\_line(aes(y = mid, color = "中轨（20日均线）")) +
&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; geom\_line(aes(y = upper, color = "上轨")) +
&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; geom\_line(aes(y = lower, color = "下轨")) +
&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; geom\_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +  # 填充轨间区域
&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; scale\_color\_manual(values = c("black", "red", "blue", "blue")) +
&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; labs(title = "AAPL布林带指标", x = "日期", y = "价格", color = "指标") +
&#x20; theme\_minimal()


&#x20; theme\_minimal()
```

### 三、多指标组合图（专业分析场景）&#xA;

实际分析中常组合多个指标，`quantmod`的`chartSeries()`支持叠加多个副图：




```
\# 绘制价格+MACD+RSI组合图
chartSeries(AAPL, name = "AAPL多指标组合分析", type = "candlesticks")
addSMA(n = 5, col = "blue")  # 主图叠加5日均线
addMACD()  # 副图1：MACD
addRSI()   # 副图2：RSI
addBBands()  # 主图叠加布林带


chartSeries(AAPL, name = "AAPL多指标组合分析", type = "candlesticks")
addSMA(n = 5, col = "blue")  # 主图叠加5日均线
addMACD()  # 副图1：MACD
addRSI()   # 副图2：RSI
addBBands()  # 主图叠加布林带


addSMA(n = 5, col = "blue")  # 主图叠加5日均线
addMACD()  # 副图1：MACD
addRSI()   # 副图2：RSI
addBBands()  # 主图叠加布林带


addMACD()  # 副图1：MACD
addRSI()   # 副图2：RSI
addBBands()  # 主图叠加布林带


addRSI()   # 副图2：RSI
addBBands()  # 主图叠加布林带


addBBands()  # 主图叠加布林带
```

组合图中，主图展示价格与趋势指标（均线、布林带），副图展示震荡指标（MACD、RSI），全方位捕捉市场信号。


### 四、关键参数与场景选择&#xA;



*   **指标周期**：短期指标（如 5 日 RSI、12/26 MACD）适合日内交易，长期指标（如 50 日 EMA、20 日布林带）适合中长期投资，需根据分析周期调整`n`参数。


*   **绘图工具**：`quantmod`适合快速生成专业组合图，代码简洁，支持交互式缩放；`ggplot2`适合自定义样式（如颜色、标签、主题），输出更美观，适合汇报展示。


*   **信号解读**：单一指标有局限性，需结合多个指标验证（如 MACD 金叉 + RSI 突破 30，确认上涨信号），避免误判。


技术指标图在金融交易策略制定、趋势研判中至关重要，通过 R 语言的上述方法，可灵活实现各类指标的计算与可视化，辅助构建系统化的分析框架。


> （注：文档部分内容可能由 AI 生成）
>