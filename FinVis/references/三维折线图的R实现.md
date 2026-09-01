# 三维折线图的R实现

三维折线图通过在三维坐标系中连接数据点形成折线，直观展示三个变量间的动态趋势关系。在 R 中，可通过`plotly`包（交互式）和`rgl`包（三维交互）实现，以下是具体方法及金融领域应用：


### 一、`plotly`包实现交互式三维折线图&#xA;

`plotly`包支持将三维折线图转换为可旋转、悬停查看细节的交互式图表，适合展示随时间或连续变量变化的多指标趋势。




1.  **基础三维折线图绘制**

    使用`plot_ly()`函数，指定 x、y、z 轴变量，设置`type = "scatter3d"`和`mode = "lines"`。例如，展示某产品 “研发投入（x）、时间（y）、销售额（z）” 的三维趋势：




```
library(plotly)

\# 模拟数据：12个月的研发投入、时间与销售额
set.seed(123)
data <- data.frame(
&#x20; month = 1:12,  # 时间（月份）
&#x20; investment = seq(100, 300, by = 20) + rnorm(12, 0, 10),  # 研发投入
&#x20; sales = seq(500, 1200, by = 60) + rnorm(12, 0, 50)  # 销售额
)

\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


\# 模拟数据：12个月的研发投入、时间与销售额
set.seed(123)
data <- data.frame(
&#x20; month = 1:12,  # 时间（月份）
&#x20; investment = seq(100, 300, by = 20) + rnorm(12, 0, 10),  # 研发投入
&#x20; sales = seq(500, 1200, by = 60) + rnorm(12, 0, 50)  # 销售额
)

\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


set.seed(123)
data <- data.frame(
&#x20; month = 1:12,  # 时间（月份）
&#x20; investment = seq(100, 300, by = 20) + rnorm(12, 0, 10),  # 研发投入
&#x20; sales = seq(500, 1200, by = 60) + rnorm(12, 0, 50)  # 销售额
)

\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


data <- data.frame(
&#x20; month = 1:12,  # 时间（月份）
&#x20; investment = seq(100, 300, by = 20) + rnorm(12, 0, 10),  # 研发投入
&#x20; sales = seq(500, 1200, by = 60) + rnorm(12, 0, 50)  # 销售额
)

\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; month = 1:12,  # 时间（月份）
&#x20; investment = seq(100, 300, by = 20) + rnorm(12, 0, 10),  # 研发投入
&#x20; sales = seq(500, 1200, by = 60) + rnorm(12, 0, 50)  # 销售额
)

\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; investment = seq(100, 300, by = 20) + rnorm(12, 0, 10),  # 研发投入
&#x20; sales = seq(500, 1200, by = 60) + rnorm(12, 0, 50)  # 销售额
)

\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; sales = seq(500, 1200, by = 60) + rnorm(12, 0, 50)  # 销售额
)

\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


)

\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


\# 绘制基础三维折线图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; data = data,
&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; x = \~investment,  # x轴：研发投入
&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; y = \~month,       # y轴：时间
&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; z = \~sales,       # z轴：销售额
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; mode = "lines+markers",  # 线+点组合
&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; line = list(width = 3, color = "blue"),  # 线样式
&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; marker = list(size = 6, color = "red")    # 点样式
) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


) %>%
&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; layout(
&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   title = "研发投入、时间与销售额的三维趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   scene = list(
&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;     xaxis = list(title = "研发投入（万元）"),
&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;     yaxis = list(title = "月份"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


print(p)  # 支持旋转视角和悬停查看具体数值
```

图形特点：




*   折线连接按时间顺序排列的点，展示 “投入增加→销售额增长” 的协同趋势；


*   悬停时显示每个月的具体数据（如 “investment=150, month=3, sales=680”）；


*   旋转视角可观察折线的陡峭程度，判断变量关系的强弱（如某阶段投入增加但销售额增长平缓）。


1.  **金融场景应用：多资产收益率趋势对比**

    对比三只股票在 12 个月内的 “波动率（x）、时间（y）、收益率（z）” 的三维趋势：




```
\# 模拟三只股票的月度数据
set.seed(456)
dates <- seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "month")
stock1 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.15, 0.03),
&#x20; return = rnorm(12, 0.01, 0.02),
&#x20; stock = "股票A"
)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


set.seed(456)
dates <- seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "month")
stock1 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.15, 0.03),
&#x20; return = rnorm(12, 0.01, 0.02),
&#x20; stock = "股票A"
)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


dates <- seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "month")
stock1 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.15, 0.03),
&#x20; return = rnorm(12, 0.01, 0.02),
&#x20; stock = "股票A"
)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


stock1 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.15, 0.03),
&#x20; return = rnorm(12, 0.01, 0.02),
&#x20; stock = "股票A"
)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; date = dates,
&#x20; vol = rnorm(12, 0.15, 0.03),
&#x20; return = rnorm(12, 0.01, 0.02),
&#x20; stock = "股票A"
)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; vol = rnorm(12, 0.15, 0.03),
&#x20; return = rnorm(12, 0.01, 0.02),
&#x20; stock = "股票A"
)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; return = rnorm(12, 0.01, 0.02),
&#x20; stock = "股票A"
)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; stock = "股票A"
)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


)
stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


stock2 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; date = dates,
&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; vol = rnorm(12, 0.2, 0.04),
&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; return = rnorm(12, 0.02, 0.03),
&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; stock = "股票B"
)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


)
stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


stock3 <- data.frame(
&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; date = dates,
&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; vol = rnorm(12, 0.1, 0.02),
&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; return = rnorm(12, 0.005, 0.01),
&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; stock = "股票C"
)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


)
stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


stock\_data <- rbind(stock1, stock2, stock3)

\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


\# 绘制多组三维折线图（按股票分组着色）
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; data = stock\_data,
&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; x = \~vol,
&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; y = \~date,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; type = "scatter3d",
&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; mode = "lines+markers",
&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; color = \~stock,  # 按股票分组着色
&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; colors = c("red", "blue", "green"),
&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; line = list(width = 2),
&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; marker = list(size = 4)
) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


) %>%
&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; layout(
&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   title = "三只股票的波动率、时间与收益率趋势",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   scene = list(
&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;     xaxis = list(title = "波动率"),
&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;     yaxis = list(title = "日期"),
&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;     zaxis = list(title = "月收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   )
&#x20; )

print(p)


&#x20; )

print(p)


print(p)
```

交互功能：




*   旋转视角观察 “高波动率是否伴随高收益”（如股票 B 波动率高但收益不稳定）；


*   悬停查看具体日期的三个指标数值，对比极端行情下的表现（如某月股票 A 收益率暴跌但波动率上升）。


### 二、`rgl`包实现三维交互折线图&#xA;

`rgl`包的三维折线图支持实时旋转、缩放，适合深度探索多变量趋势的空间特征。




1.  **基础三维折线图绘制**

    使用`lines3d()`函数在`plot3d()`基础上添加折线，图形在独立窗口中显示，支持鼠标交互。例如，展示 “温度（x）、压力（y）、反应速率（z）” 的实验数据趋势：




```
library(rgl)

\# 模拟实验数据
temp <- seq(20, 80, by = 5)
pressure <- seq(1, 5, by = 0.3)
rate <- 0.02\*temp + 0.5\*pressure + rnorm(length(temp), 0, 0.8)
data <- data.frame(temp, pressure, rate)

\# 绘制三维散点+折线图
plot3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


\# 模拟实验数据
temp <- seq(20, 80, by = 5)
pressure <- seq(1, 5, by = 0.3)
rate <- 0.02\*temp + 0.5\*pressure + rnorm(length(temp), 0, 0.8)
data <- data.frame(temp, pressure, rate)

\# 绘制三维散点+折线图
plot3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


temp <- seq(20, 80, by = 5)
pressure <- seq(1, 5, by = 0.3)
rate <- 0.02\*temp + 0.5\*pressure + rnorm(length(temp), 0, 0.8)
data <- data.frame(temp, pressure, rate)

\# 绘制三维散点+折线图
plot3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


pressure <- seq(1, 5, by = 0.3)
rate <- 0.02\*temp + 0.5\*pressure + rnorm(length(temp), 0, 0.8)
data <- data.frame(temp, pressure, rate)

\# 绘制三维散点+折线图
plot3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


rate <- 0.02\*temp + 0.5\*pressure + rnorm(length(temp), 0, 0.8)
data <- data.frame(temp, pressure, rate)

\# 绘制三维散点+折线图
plot3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


data <- data.frame(temp, pressure, rate)

\# 绘制三维散点+折线图
plot3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


\# 绘制三维散点+折线图
plot3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


plot3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; z = data\$rate,
&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; col = "black",
&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; size = 3,
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "反应速率"
)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


)
lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


lines3d(
&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; x = data\$temp,
&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; y = data\$pressure,
&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; z = data\$rate,
&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; color = "red",
&#x20; width = 2  # 线宽
)


&#x20; width = 2  # 线宽
)


)
```

交互操作：




*   左键拖拽旋转视角，观察折线的整体趋势（如温度升高时反应速率是否线性增长）；


*   右键缩放图形，聚焦折线的陡峭或平缓段（如低压力区间反应速率增长更快）。


1.  **金融场景应用：利率、汇率与股价的联动趋势**

    分析 “国债利率（x）、美元汇率（y）、大盘指数（z）” 的月度趋势，识别宏观指标对股市的影响：




```
\# 模拟宏观经济数据（12个月）
set.seed(789)
rate <- seq(2, 3.5, by = 0.15) + rnorm(12, 0, 0.1)  # 国债利率（%）
exchange <- seq(6.3, 6.8, by = 0.05) + rnorm(12, 0, 0.03)  # 美元汇率
index <- seq(3000, 3500, by = 40) + rnorm(12, 0, 50)  # 大盘指数
macro\_data <- data.frame(rate, exchange, index)

\# 绘制三维折线图（突出趋势变化）
plot3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


set.seed(789)
rate <- seq(2, 3.5, by = 0.15) + rnorm(12, 0, 0.1)  # 国债利率（%）
exchange <- seq(6.3, 6.8, by = 0.05) + rnorm(12, 0, 0.03)  # 美元汇率
index <- seq(3000, 3500, by = 40) + rnorm(12, 0, 50)  # 大盘指数
macro\_data <- data.frame(rate, exchange, index)

\# 绘制三维折线图（突出趋势变化）
plot3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


rate <- seq(2, 3.5, by = 0.15) + rnorm(12, 0, 0.1)  # 国债利率（%）
exchange <- seq(6.3, 6.8, by = 0.05) + rnorm(12, 0, 0.03)  # 美元汇率
index <- seq(3000, 3500, by = 40) + rnorm(12, 0, 50)  # 大盘指数
macro\_data <- data.frame(rate, exchange, index)

\# 绘制三维折线图（突出趋势变化）
plot3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


exchange <- seq(6.3, 6.8, by = 0.05) + rnorm(12, 0, 0.03)  # 美元汇率
index <- seq(3000, 3500, by = 40) + rnorm(12, 0, 50)  # 大盘指数
macro\_data <- data.frame(rate, exchange, index)

\# 绘制三维折线图（突出趋势变化）
plot3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


index <- seq(3000, 3500, by = 40) + rnorm(12, 0, 50)  # 大盘指数
macro\_data <- data.frame(rate, exchange, index)

\# 绘制三维折线图（突出趋势变化）
plot3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


macro\_data <- data.frame(rate, exchange, index)

\# 绘制三维折线图（突出趋势变化）
plot3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


\# 绘制三维折线图（突出趋势变化）
plot3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


plot3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; z = macro\_data\$index,
&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; col = "gray", size = 4,
&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; xlab = "国债利率（%）", ylab = "美元汇率", zlab = "大盘指数",
&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; main = "宏观指标与股市趋势的三维关系"
)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


)
lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


lines3d(
&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; x = macro\_data\$rate,
&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; y = macro\_data\$exchange,
&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; z = macro\_data\$index,
&#x20; color = "blue", width = 3
)


&#x20; color = "blue", width = 3
)


)
```

分析价值：




*   旋转视角观察 “利率上升、汇率升值时，大盘指数是否下跌”（典型宏观紧缩场景）；


*   结合`rgl`的`snapshot3d()`函数保存关键视角的图片，用于报告展示。


### 三、关键参数与场景选择&#xA;



*   `plotly`**包优势**：



    *   支持分组着色和悬停信息，适合多组数据对比（如不同股票、不同产品）；


    *   可导出为 HTML 文件，便于在网页或汇报中分享；


    *   金融场景推荐用于客户沟通（如展示投资组合的动态风险收益特征）。


*   `rgl`**包优势**：



    *   三维交互更流畅，适合大规模时间序列数据（如分钟级高频数据）；


    *   支持自定义视角和快照保存，适合深度趋势分析；


    *   金融场景推荐用于内部研究（如宏观经济指标联动分析）。


*   **核心参数**：



    *   线样式（`line`）：通过`width`和`dash`（实线 / 虚线）区分不同组别；


    *   点线组合（`mode = "lines+markers"`）：既展示趋势（线）又突出关键数据点（点）；


    *   时间轴处理：`plotly`对`Date`类变量支持更好，自动按时间排序；`rgl`需手动排序数据确保折线连续。


### 四、金融领域典型应用&#xA;



1.  **投资组合动态监控**：展示 “股票占比（x）、债券占比（y）、组合收益（z）” 随时间的折线，观察资产配置调整效果；


2.  **衍生品 Greeks 分析**：绘制 “标的价格（x）、到期时间（y）、Delta 值（z）” 的折线，观察期权风险参数的变化趋势；


3.  **利率曲线分析**：以 “期限（x）、时间（y）、收益率（z）” 为轴，展示国债收益率曲线的动态变化（如扁平化或陡峭化）；


4.  **高频交易策略回测**：展示 “持仓时间（x）、波动率（y）、策略收益（z）” 的折线，优化持仓周期参数。


三维折线图通过空间趋势的直观展示，帮助金融分析师捕捉多变量间的动态关联，尤其适合时间序列数据的深度探索。


> （注：文档部分内容可能由 AI 生成）
>