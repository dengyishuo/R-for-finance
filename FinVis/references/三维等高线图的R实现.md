# 三维等高线图的R实现

三维等高线图通过二维平面上的闭合曲线（等高线）展示三个变量的关系，其中 x、y 轴为自变量，等高线的形状和疏密反映因变量 z 的分布。在 R 中，可通过`plotly`包（交互式）和`ggplot2`+`metR`包（静态）实现，以下是具体方法及金融领域应用：


### 一、`plotly`包实现交互式三维等高线图&#xA;

`plotly`包的等高线图支持悬停查看数值、缩放和平移，适合探索 z 值的等值分布，尤其在金融领域用于风险阈值、收益率等值线分析。




1.  **基础三维等高线图绘制**

    使用`plot_ly()`函数，指定 x、y 为网格状自变量，z 为因变量矩阵，设置`type = "contour"`。例如，展示 “温度（x）、压力（y）、反应速率（z）” 的等值分布：




```
library(plotly)

\# 生成网格数据（与三维曲面图一致）
temp <- seq(20, 80, by = 2)  # 温度
pressure <- seq(1, 5, by = 0.2)  # 压力
z <- outer(temp, pressure, function(t, p) 0.05\*t + 0.8\*p + rnorm(length(t), 0, 1))  # 反应速率

\# 绘制基础等高线图
p <- plot\_ly(
&#x20; x = \~temp,
&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


\# 生成网格数据（与三维曲面图一致）
temp <- seq(20, 80, by = 2)  # 温度
pressure <- seq(1, 5, by = 0.2)  # 压力
z <- outer(temp, pressure, function(t, p) 0.05\*t + 0.8\*p + rnorm(length(t), 0, 1))  # 反应速率

\# 绘制基础等高线图
p <- plot\_ly(
&#x20; x = \~temp,
&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


temp <- seq(20, 80, by = 2)  # 温度
pressure <- seq(1, 5, by = 0.2)  # 压力
z <- outer(temp, pressure, function(t, p) 0.05\*t + 0.8\*p + rnorm(length(t), 0, 1))  # 反应速率

\# 绘制基础等高线图
p <- plot\_ly(
&#x20; x = \~temp,
&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


pressure <- seq(1, 5, by = 0.2)  # 压力
z <- outer(temp, pressure, function(t, p) 0.05\*t + 0.8\*p + rnorm(length(t), 0, 1))  # 反应速率

\# 绘制基础等高线图
p <- plot\_ly(
&#x20; x = \~temp,
&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


z <- outer(temp, pressure, function(t, p) 0.05\*t + 0.8\*p + rnorm(length(t), 0, 1))  # 反应速率

\# 绘制基础等高线图
p <- plot\_ly(
&#x20; x = \~temp,
&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


\# 绘制基础等高线图
p <- plot\_ly(
&#x20; x = \~temp,
&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


p <- plot\_ly(
&#x20; x = \~temp,
&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; x = \~temp,
&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; y = \~pressure,
&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; type = "contour",
&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; colorscale = "Viridis",  # 色板（z值越高颜色越深）
&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; contours = list(
&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20;   start = min(z),  # 等高线起始值
&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20;   end = max(z),    # 等高线结束值
&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20;   size = 0.5       # 等高线间隔（z值每增加0.5画一条线）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


) %>%
&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; layout(
&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20;   title = "温度、压力与反应速率的等高线图",
&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20;   xaxis = list(title = "温度（℃）"),
&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20;   yaxis = list(title = "压力（atm）")
&#x20; )

print(p)  # 支持悬停查看z值和缩放


&#x20; )

print(p)  # 支持悬停查看z值和缩放


print(p)  # 支持悬停查看z值和缩放
```

图形特点：




*   等高线的疏密反映 z 值变化速率（密集→变化快，稀疏→变化慢），如高压力区域线条密集，说明压力对反应速率影响更大；


*   悬停时显示 “x=50, y=3, z=4.2”，直接读取特定点的 z 值；


*   颜色梯度与等高线协同，直观区分高值区（暖色）和低值区（冷色）。


1.  **添加标签与自定义样式**

    通过`contours`参数添加数值标签，增强等高线的可读性。例如，优化上述图形：




```
p <- plot\_ly(
&#x20; x = \~temp, y = \~pressure, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "RdBu",
&#x20; contours = list(
&#x20;   showlabels = TRUE,  # 显示等高线数值标签
&#x20;   labelfont = list(size = 10, color = "black"),  # 标签字体
&#x20;   size = 1,  # 增大间隔，避免标签拥挤
&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20; x = \~temp, y = \~pressure, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "RdBu",
&#x20; contours = list(
&#x20;   showlabels = TRUE,  # 显示等高线数值标签
&#x20;   labelfont = list(size = 10, color = "black"),  # 标签字体
&#x20;   size = 1,  # 增大间隔，避免标签拥挤
&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20; type = "contour",
&#x20; colorscale = "RdBu",
&#x20; contours = list(
&#x20;   showlabels = TRUE,  # 显示等高线数值标签
&#x20;   labelfont = list(size = 10, color = "black"),  # 标签字体
&#x20;   size = 1,  # 增大间隔，避免标签拥挤
&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20; colorscale = "RdBu",
&#x20; contours = list(
&#x20;   showlabels = TRUE,  # 显示等高线数值标签
&#x20;   labelfont = list(size = 10, color = "black"),  # 标签字体
&#x20;   size = 1,  # 增大间隔，避免标签拥挤
&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20; contours = list(
&#x20;   showlabels = TRUE,  # 显示等高线数值标签
&#x20;   labelfont = list(size = 10, color = "black"),  # 标签字体
&#x20;   size = 1,  # 增大间隔，避免标签拥挤
&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20;   showlabels = TRUE,  # 显示等高线数值标签
&#x20;   labelfont = list(size = 10, color = "black"),  # 标签字体
&#x20;   size = 1,  # 增大间隔，避免标签拥挤
&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20;   labelfont = list(size = 10, color = "black"),  # 标签字体
&#x20;   size = 1,  # 增大间隔，避免标签拥挤
&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20;   size = 1,  # 增大间隔，避免标签拥挤
&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20;   coloring = "fill"  # 填充等高线之间的区域（默认）
&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20; ),
&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20; line = list(width = 2)  # 等高线线条宽度
) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


) %>%
&#x20; layout(title = "带标签的等高线图")

print(p)


&#x20; layout(title = "带标签的等高线图")

print(p)


print(p)
```

标签作用：




*   直接标注每条等高线的 z 值（如 “4.0”“5.0”），避免仅通过颜色判断数值；


*   结合线条宽度和颜色，突出关键等值线（如 z=6.0 的高反应速率线）。


1.  **金融场景应用：期权隐含波动率等高线**

    展示 “行权价（x）、到期时间（y）、隐含波动率（z）” 的等值分布，识别波动率微笑特征：




```
\# 模拟期权数据（与三维曲面图一致）
strike <- seq(80, 120, by = 2)  # 行权价
maturity <- seq(1, 12, by = 0.5)  # 到期时间
z <- outer(strike, maturity, function(s, m) 0.2 + 0.001\*(s - 100)^2 + 0.01/m + rnorm(length(s), 0, 0.01))

\# 绘制波动率等高线图
p <- plot\_ly(
&#x20; x = \~strike, y = \~maturity, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


strike <- seq(80, 120, by = 2)  # 行权价
maturity <- seq(1, 12, by = 0.5)  # 到期时间
z <- outer(strike, maturity, function(s, m) 0.2 + 0.001\*(s - 100)^2 + 0.01/m + rnorm(length(s), 0, 0.01))

\# 绘制波动率等高线图
p <- plot\_ly(
&#x20; x = \~strike, y = \~maturity, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


maturity <- seq(1, 12, by = 0.5)  # 到期时间
z <- outer(strike, maturity, function(s, m) 0.2 + 0.001\*(s - 100)^2 + 0.01/m + rnorm(length(s), 0, 0.01))

\# 绘制波动率等高线图
p <- plot\_ly(
&#x20; x = \~strike, y = \~maturity, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


z <- outer(strike, maturity, function(s, m) 0.2 + 0.001\*(s - 100)^2 + 0.01/m + rnorm(length(s), 0, 0.01))

\# 绘制波动率等高线图
p <- plot\_ly(
&#x20; x = \~strike, y = \~maturity, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


\# 绘制波动率等高线图
p <- plot\_ly(
&#x20; x = \~strike, y = \~maturity, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


p <- plot\_ly(
&#x20; x = \~strike, y = \~maturity, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20; x = \~strike, y = \~maturity, z = \~z,
&#x20; type = "contour",
&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20; type = "contour",
&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20; colorscale = "YlOrRd",  # 高波动率用暖色
&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20; contours = list(
&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20;   showlabels = TRUE,
&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20;   size = 0.02,  # 波动率间隔0.02（2%）
&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20;   start = 0.15, end = 0.3  # 聚焦关键区间
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


) %>%
&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20; layout(
&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20;   title = "期权隐含波动率等高线",
&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20;   xaxis = list(title = "行权价"),
&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20;   yaxis = list(title = "到期时间（月）")
&#x20; )

print(p)


&#x20; )

print(p)


print(p)
```

交互分析：




*   悬停查看特定行权价和到期时间的波动率（如 “行权价 = 100，到期 3 个月，波动率 = 0.22”）；


*   观察等高线形状，识别 “波动率微笑”（行权价偏离 100 时，等高线向高值区弯曲）；


*   缩放聚焦短期期权（y<3）的波动率变化，发现其对行权价更敏感（等高线更密集）。


### 二、`ggplot2`+`metR`包实现静态三维等高线图&#xA;

`metR`包扩展了 ggplot2 的功能，支持绘制 publication 级别的静态等高线图，适合需要精确控制样式的场景。




1.  **基础静态等高线图绘制**

    使用`metR::geom_contour()`函数，输入长格式数据（x、y、z 列）。例如，基于上述反应速率数据：




```
library(ggplot2)
library(metR)
library(dplyr)
library(tidyr)

\# 将矩阵数据转换为长格式
contour\_data <- expand.grid(temp = temp, pressure = pressure) %>%
&#x20; mutate(rate = c(z))  # z矩阵转为向量

\# 绘制基础静态等高线图
ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


library(metR)
library(dplyr)
library(tidyr)

\# 将矩阵数据转换为长格式
contour\_data <- expand.grid(temp = temp, pressure = pressure) %>%
&#x20; mutate(rate = c(z))  # z矩阵转为向量

\# 绘制基础静态等高线图
ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


library(dplyr)
library(tidyr)

\# 将矩阵数据转换为长格式
contour\_data <- expand.grid(temp = temp, pressure = pressure) %>%
&#x20; mutate(rate = c(z))  # z矩阵转为向量

\# 绘制基础静态等高线图
ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


library(tidyr)

\# 将矩阵数据转换为长格式
contour\_data <- expand.grid(temp = temp, pressure = pressure) %>%
&#x20; mutate(rate = c(z))  # z矩阵转为向量

\# 绘制基础静态等高线图
ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


\# 将矩阵数据转换为长格式
contour\_data <- expand.grid(temp = temp, pressure = pressure) %>%
&#x20; mutate(rate = c(z))  # z矩阵转为向量

\# 绘制基础静态等高线图
ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


contour\_data <- expand.grid(temp = temp, pressure = pressure) %>%
&#x20; mutate(rate = c(z))  # z矩阵转为向量

\# 绘制基础静态等高线图
ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20; mutate(rate = c(z))  # z矩阵转为向量

\# 绘制基础静态等高线图
ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


\# 绘制基础静态等高线图
ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


ggplot(contour\_data, aes(x = temp, y = pressure, z = rate)) +
&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20; geom\_contour\_filled(bins = 10) +  # 填充等高线（按z值分10级）
&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20; geom\_contour(color = "black", linewidth = 0.5) +  # 叠加黑色等高线
&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20; scale\_fill\_viridis\_d(option = "plasma") +  # 填充色板
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 3) +  # 添加数值标签
&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20; labs(
&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20;   title = "温度、压力与反应速率的静态等高线图",
&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20;   x = "温度（℃）", y = "压力（atm）", fill = "反应速率"
&#x20; ) +
&#x20; theme\_minimal()


&#x20; ) +
&#x20; theme\_minimal()


&#x20; theme\_minimal()
```

图形特点：




*   `geom_contour_filled()`填充等高线之间的区域，颜色分级清晰；


*   `geom_text_contour()`标注等高线数值，位置自动避开重叠；


*   适合输出为 PDF/PNG 等静态格式，用于论文或报告。


1.  **金融场景应用：资产组合风险等高线**

    展示 “股票占比（x）、债券占比（y）、风险值（z）” 的等值分布，识别低风险区域：




```
\# 模拟资产配置风险数据（z为VaR值，越小风险越低）
stock\_ratio <- seq(0.1, 0.8, by = 0.05)
bond\_ratio <- seq(0.1, 0.8, by = 0.05)
risk\_data <- expand.grid(stock = stock\_ratio, bond = bond\_ratio) %>%
&#x20; filter(stock + bond <= 0.9) %>%  # 确保第三类资产占比合理
&#x20; mutate(
&#x20;   var = 0.1\*stock + 0.05\*bond + 0.02\*(1 - stock - bond) + 0.01\*stock\*bond + rnorm(n(), 0, 0.003)
&#x20; )

\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


stock\_ratio <- seq(0.1, 0.8, by = 0.05)
bond\_ratio <- seq(0.1, 0.8, by = 0.05)
risk\_data <- expand.grid(stock = stock\_ratio, bond = bond\_ratio) %>%
&#x20; filter(stock + bond <= 0.9) %>%  # 确保第三类资产占比合理
&#x20; mutate(
&#x20;   var = 0.1\*stock + 0.05\*bond + 0.02\*(1 - stock - bond) + 0.01\*stock\*bond + rnorm(n(), 0, 0.003)
&#x20; )

\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


bond\_ratio <- seq(0.1, 0.8, by = 0.05)
risk\_data <- expand.grid(stock = stock\_ratio, bond = bond\_ratio) %>%
&#x20; filter(stock + bond <= 0.9) %>%  # 确保第三类资产占比合理
&#x20; mutate(
&#x20;   var = 0.1\*stock + 0.05\*bond + 0.02\*(1 - stock - bond) + 0.01\*stock\*bond + rnorm(n(), 0, 0.003)
&#x20; )

\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


risk\_data <- expand.grid(stock = stock\_ratio, bond = bond\_ratio) %>%
&#x20; filter(stock + bond <= 0.9) %>%  # 确保第三类资产占比合理
&#x20; mutate(
&#x20;   var = 0.1\*stock + 0.05\*bond + 0.02\*(1 - stock - bond) + 0.01\*stock\*bond + rnorm(n(), 0, 0.003)
&#x20; )

\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; filter(stock + bond <= 0.9) %>%  # 确保第三类资产占比合理
&#x20; mutate(
&#x20;   var = 0.1\*stock + 0.05\*bond + 0.02\*(1 - stock - bond) + 0.01\*stock\*bond + rnorm(n(), 0, 0.003)
&#x20; )

\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; mutate(
&#x20;   var = 0.1\*stock + 0.05\*bond + 0.02\*(1 - stock - bond) + 0.01\*stock\*bond + rnorm(n(), 0, 0.003)
&#x20; )

\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20;   var = 0.1\*stock + 0.05\*bond + 0.02\*(1 - stock - bond) + 0.01\*stock\*bond + rnorm(n(), 0, 0.003)
&#x20; )

\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; )

\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


\# 绘制风险等高线图
ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


ggplot(risk\_data, aes(x = stock, y = bond, z = var)) +
&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; geom\_contour\_filled(breaks = seq(0.03, 0.1, by = 0.01)) +  # 自定义风险区间
&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; geom\_contour(color = "white", linewidth = 0.3) +
&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; metR::geom\_text\_contour(aes(label = ..level..), size = 2.5) +
&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; scale\_fill\_brewer(palette = "YlGnBu", direction = -1) +  # 低风险（低值）用深色
&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; labs(
&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20;   title = "资产组合风险值（VaR）等高线",
&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20;   x = "股票占比", y = "债券占比", fill = "风险值（VaR）"
&#x20; ) +
&#x20; theme\_bw()


&#x20; ) +
&#x20; theme\_bw()


&#x20; theme\_bw()
```

分析价值：




*   深色区域（如风险值 = 0.03-0.04）对应低风险配置，可直接读取股票和债券的最优比例（如 stock=0.3, bond=0.5）；


*   等高线的走向（如向右上方倾斜）说明 “股票占比增加时，需降低债券占比以维持风险不变”。


### 三、关键参数与场景选择&#xA;



*   `plotly`**包优势**：



    *   支持交互式探索（悬停、缩放），适合动态分析 z 值分布；


    *   可与三维曲面图联动（同一数据生成两种视图），增强解读；


    *   金融场景推荐用于实时风险监控（如市场波动等值线）。


*   `ggplot2`**+**`metR`**包优势**：



    *   静态图形样式可控（如填充色、标签位置），适合出版或汇报；


    *   支持自定义等高线间隔（`breaks`参数），聚焦关键数值区间；


    *   金融场景推荐用于报告输出（如资产配置风险区间图）。


*   **核心参数**：



    *   等高线间隔（`size`/`breaks`）：间隔越小，细节越丰富但图形越复杂（建议根据 z 值范围设置 5-15 条等高线）；


    *   颜色映射：`plotly`用`colorscale`，`ggplot2`用`scale_fill_*`，优先选择渐变且对比度高的色板（如`Viridis`）；


    *   标签显示：`showlabels`参数控制标签开关，避免密集区域标签重叠（可减小字体或增大间隔）。


### 四、金融领域典型应用&#xA;



1.  **风险阈值监控**：绘制 “时间（x）、仓位（y）、VaR 值（z）” 的等高线，识别风险超阈值的区域（如 z>0.05 的红色区域）；


2.  **收益率等值线分析**：展示 “利率（x）、汇率（y）、资产收益率（z）” 的等值分布，寻找稳定收益区间；


3.  **客户分群风险**：以 “资产规模（x）、年龄（y）、违约概率（z）” 为轴，划分低 / 中 / 高风险客户群的边界；


4.  **衍生品套利机会**：通过 “行权价（x）、波动率（y）、套利收益（z）” 的等高线，定位 z>0 的套利区间。


三维等高线图通过简化三维关系为二维等值线，既保留 z 值分布规律，又避免三维视角的干扰，是平衡信息完整性与解读效率的重要工具，尤其适合金融领域的风险划分和阈值监控。


> （注：文档部分内容可能由 AI 生成）
>