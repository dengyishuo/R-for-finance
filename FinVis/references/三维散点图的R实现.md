# 三维散点图的R实现

三维散点图用于展示三个连续变量之间的关系，在 R 中可通过`plotly`包（交互式）和`rgl`包（三维交互）实现，以下是具体方法：


### 一、`plotly`包实现交互式三维散点图&#xA;

`plotly`包支持将三维散点图转换为可旋转、缩放、悬停查看细节的交互式图表，适合探索多变量关系，尤其在金融领域分析资产相关性等场景。




1.  **基础三维散点图绘制**

    使用`plot_ly()`函数，指定 x、y、z 轴变量，设置`type = "scatter3d"`和`mode = "markers"`。例如，基于`mtcars`数据集探索汽车重量（`wt`）、马力（`hp`）与油耗（`mpg`）的关系：




```
library(plotly)

\# 绘制基础三维散点图
p <- plot\_ly(
&#x20; data = mtcars,
&#x20; x = \~wt,  # x轴：重量
&#x20; y = \~hp,  # y轴：马力
&#x20; z = \~mpg, # z轴：油耗
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


\# 绘制基础三维散点图
p <- plot\_ly(
&#x20; data = mtcars,
&#x20; x = \~wt,  # x轴：重量
&#x20; y = \~hp,  # y轴：马力
&#x20; z = \~mpg, # z轴：油耗
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


p <- plot\_ly(
&#x20; data = mtcars,
&#x20; x = \~wt,  # x轴：重量
&#x20; y = \~hp,  # y轴：马力
&#x20; z = \~mpg, # z轴：油耗
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; data = mtcars,
&#x20; x = \~wt,  # x轴：重量
&#x20; y = \~hp,  # y轴：马力
&#x20; z = \~mpg, # z轴：油耗
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; x = \~wt,  # x轴：重量
&#x20; y = \~hp,  # y轴：马力
&#x20; z = \~mpg, # z轴：油耗
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; y = \~hp,  # y轴：马力
&#x20; z = \~mpg, # z轴：油耗
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; z = \~mpg, # z轴：油耗
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; mode = "markers",
&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; marker = list(size = 5, color = \~mpg, colorscale = "Viridis")  # 按油耗着色
) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


) %>%
&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; layout(
&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20;   title = "汽车重量、马力与油耗的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20;   scene = list(
&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20;     xaxis = list(title = "重量（吨）"),
&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20;     yaxis = list(title = "马力"),
&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20;     zaxis = list(title = "油耗（mpg）")
&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20;   )
&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


&#x20; )

print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停


print(p)  # 在浏览器或RStudio Viewer中显示，支持旋转和悬停
```

图形特点：




*   可通过鼠标拖拽旋转视角，观察三个变量的空间分布；


*   悬停时显示每个点的具体数值（如 “wt=3.2, hp=150, mpg=21”）；


*   点的颜色按油耗（`mpg`）渐变，增强第四维度信息。


1.  **金融场景应用：资产风险与收益分析**

    分析股票的市盈率（`pe`）、波动率（`vol`）与收益率（`return`）的三维关系，用颜色区分行业：




```
\# 模拟金融数据（50只股票）
set.seed(123)
stock\_data <- data.frame(
&#x20; pe = rnorm(50, mean = 20, sd = 5),  # 市盈率
&#x20; vol = rnorm(50, mean = 0.15, sd = 0.05),  # 波动率
&#x20; return = rnorm(50, mean = 0.08, sd = 0.03),  # 年化收益率
&#x20; industry = sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业
)

\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


set.seed(123)
stock\_data <- data.frame(
&#x20; pe = rnorm(50, mean = 20, sd = 5),  # 市盈率
&#x20; vol = rnorm(50, mean = 0.15, sd = 0.05),  # 波动率
&#x20; return = rnorm(50, mean = 0.08, sd = 0.03),  # 年化收益率
&#x20; industry = sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业
)

\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


stock\_data <- data.frame(
&#x20; pe = rnorm(50, mean = 20, sd = 5),  # 市盈率
&#x20; vol = rnorm(50, mean = 0.15, sd = 0.05),  # 波动率
&#x20; return = rnorm(50, mean = 0.08, sd = 0.03),  # 年化收益率
&#x20; industry = sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业
)

\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; pe = rnorm(50, mean = 20, sd = 5),  # 市盈率
&#x20; vol = rnorm(50, mean = 0.15, sd = 0.05),  # 波动率
&#x20; return = rnorm(50, mean = 0.08, sd = 0.03),  # 年化收益率
&#x20; industry = sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业
)

\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; vol = rnorm(50, mean = 0.15, sd = 0.05),  # 波动率
&#x20; return = rnorm(50, mean = 0.08, sd = 0.03),  # 年化收益率
&#x20; industry = sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业
)

\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; return = rnorm(50, mean = 0.08, sd = 0.03),  # 年化收益率
&#x20; industry = sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业
)

\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; industry = sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业
)

\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


)

\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


\# 绘制带行业分组的三维散点图
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; marker = list(size = 6, opacity = 0.8)  # 点大小和透明度
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; layout(
&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   title = "股票市盈率、波动率与收益率的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;     zaxis = list(title = "年化收益率")
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




*   旋转视角观察 “高市盈率、低波动率、高收益” 的股票集群；


*   悬停查看具体股票的三个指标数值，辅助筛选优质资产。


### 二、`rgl`包实现三维交互散点图&#xA;

`rgl`包专注于三维图形的实时交互，支持旋转、缩放、选择点，适合需要深度探索空间分布的场景。




1.  **基础三维散点图绘制**

    使用`plot3d()`函数，直接指定 x、y、z 轴变量，图形会在独立窗口中显示，可通过鼠标交互操作。例如，基于`iris`数据集展示花瓣长度、宽度与花萼长度的关系：




```
library(rgl)

\# 绘制基础三维散点图
plot3d(
&#x20; x = iris\$Petal.Length,  # x轴：花瓣长度
&#x20; y = iris\$Petal.Width,   # y轴：花瓣宽度
&#x20; z = iris\$Sepal.Length,  # z轴：花萼长度
&#x20; col = iris\$Species,     # 按品种着色
&#x20; size = 3,               # 点大小
&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


\# 绘制基础三维散点图
plot3d(
&#x20; x = iris\$Petal.Length,  # x轴：花瓣长度
&#x20; y = iris\$Petal.Width,   # y轴：花瓣宽度
&#x20; z = iris\$Sepal.Length,  # z轴：花萼长度
&#x20; col = iris\$Species,     # 按品种着色
&#x20; size = 3,               # 点大小
&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


plot3d(
&#x20; x = iris\$Petal.Length,  # x轴：花瓣长度
&#x20; y = iris\$Petal.Width,   # y轴：花瓣宽度
&#x20; z = iris\$Sepal.Length,  # z轴：花萼长度
&#x20; col = iris\$Species,     # 按品种着色
&#x20; size = 3,               # 点大小
&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


&#x20; x = iris\$Petal.Length,  # x轴：花瓣长度
&#x20; y = iris\$Petal.Width,   # y轴：花瓣宽度
&#x20; z = iris\$Sepal.Length,  # z轴：花萼长度
&#x20; col = iris\$Species,     # 按品种着色
&#x20; size = 3,               # 点大小
&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


&#x20; y = iris\$Petal.Width,   # y轴：花瓣宽度
&#x20; z = iris\$Sepal.Length,  # z轴：花萼长度
&#x20; col = iris\$Species,     # 按品种着色
&#x20; size = 3,               # 点大小
&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


&#x20; z = iris\$Sepal.Length,  # z轴：花萼长度
&#x20; col = iris\$Species,     # 按品种着色
&#x20; size = 3,               # 点大小
&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


&#x20; col = iris\$Species,     # 按品种着色
&#x20; size = 3,               # 点大小
&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


&#x20; size = 3,               # 点大小
&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


&#x20; xlab = "花瓣长度", ylab = "花瓣宽度", zlab = "花萼长度",
&#x20; main = "鸢尾花三维特征散点图"
)


&#x20; main = "鸢尾花三维特征散点图"
)


)
```

交互操作：




*   左键拖拽：旋转视角；


*   右键拖拽：缩放图形；


*   中键拖拽：平移图形；


*   点击点可选中并显示索引（需结合`identify3d()`函数）。


1.  **金融场景应用：风险价值（VaR）分析**

    展示资产组合的 “权重 1（`w1`）、权重 2（`w2`）、VaR” 的三维关系，识别风险最低的权重组合：




```
\# 模拟资产组合数据（权重1+权重2+权重3=1，VaR为风险价值）
set.seed(456)
w1 <- seq(0.1, 0.8, by = 0.1)
w2 <- seq(0.1, 0.8, by = 0.1)
risk\_data <- expand.grid(w1 = w1, w2 = w2)
risk\_data\$w3 <- 1 - risk\_data\$w1 - risk\_data\$w2  # 权重3
risk\_data\$VaR <- with(risk\_data, 0.02\*w1 + 0.05\*w2 + 0.03\*w3 + rnorm(nrow(.), 0, 0.005))  # 模拟VaR

\# 绘制三维散点图（点颜色按VaR大小渐变）
plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


set.seed(456)
w1 <- seq(0.1, 0.8, by = 0.1)
w2 <- seq(0.1, 0.8, by = 0.1)
risk\_data <- expand.grid(w1 = w1, w2 = w2)
risk\_data\$w3 <- 1 - risk\_data\$w1 - risk\_data\$w2  # 权重3
risk\_data\$VaR <- with(risk\_data, 0.02\*w1 + 0.05\*w2 + 0.03\*w3 + rnorm(nrow(.), 0, 0.005))  # 模拟VaR

\# 绘制三维散点图（点颜色按VaR大小渐变）
plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


w1 <- seq(0.1, 0.8, by = 0.1)
w2 <- seq(0.1, 0.8, by = 0.1)
risk\_data <- expand.grid(w1 = w1, w2 = w2)
risk\_data\$w3 <- 1 - risk\_data\$w1 - risk\_data\$w2  # 权重3
risk\_data\$VaR <- with(risk\_data, 0.02\*w1 + 0.05\*w2 + 0.03\*w3 + rnorm(nrow(.), 0, 0.005))  # 模拟VaR

\# 绘制三维散点图（点颜色按VaR大小渐变）
plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


w2 <- seq(0.1, 0.8, by = 0.1)
risk\_data <- expand.grid(w1 = w1, w2 = w2)
risk\_data\$w3 <- 1 - risk\_data\$w1 - risk\_data\$w2  # 权重3
risk\_data\$VaR <- with(risk\_data, 0.02\*w1 + 0.05\*w2 + 0.03\*w3 + rnorm(nrow(.), 0, 0.005))  # 模拟VaR

\# 绘制三维散点图（点颜色按VaR大小渐变）
plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


risk\_data <- expand.grid(w1 = w1, w2 = w2)
risk\_data\$w3 <- 1 - risk\_data\$w1 - risk\_data\$w2  # 权重3
risk\_data\$VaR <- with(risk\_data, 0.02\*w1 + 0.05\*w2 + 0.03\*w3 + rnorm(nrow(.), 0, 0.005))  # 模拟VaR

\# 绘制三维散点图（点颜色按VaR大小渐变）
plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


risk\_data\$w3 <- 1 - risk\_data\$w1 - risk\_data\$w2  # 权重3
risk\_data\$VaR <- with(risk\_data, 0.02\*w1 + 0.05\*w2 + 0.03\*w3 + rnorm(nrow(.), 0, 0.005))  # 模拟VaR

\# 绘制三维散点图（点颜色按VaR大小渐变）
plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


risk\_data\$VaR <- with(risk\_data, 0.02\*w1 + 0.05\*w2 + 0.03\*w3 + rnorm(nrow(.), 0, 0.005))  # 模拟VaR

\# 绘制三维散点图（点颜色按VaR大小渐变）
plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


\# 绘制三维散点图（点颜色按VaR大小渐变）
plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


plot3d(
&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


&#x20; x = risk\_data\$w1,
&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


&#x20; y = risk\_data\$w2,
&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


&#x20; z = risk\_data\$VaR,
&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


&#x20; col = rainbow(nrow(risk\_data))\[order(risk\_data\$VaR)],  # 按VaR着色
&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


&#x20; size = 5,
&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


&#x20; xlab = "资产1权重", ylab = "资产2权重", zlab = "VaR",
&#x20; main = "资产组合权重与VaR的三维关系"
)


&#x20; main = "资产组合权重与VaR的三维关系"
)


)
```

分析价值：




*   旋转视角找到 VaR 最低的区域（深色点集中区），对应最优权重组合；


*   结合`rgl`的`select3d()`函数可框选低风险区域，提取具体权重值。


### 三、关键参数与场景选择&#xA;



*   `plotly`**包优势**：



    *   支持网页导出和分享，适合汇报展示；


    *   可与 ggplot2 语法结合，快速将静态图转为交互图；


    *   金融场景中推荐用于客户汇报（如资产配置方案可视化）。


*   `rgl`**包优势**：



    *   三维交互更流畅，适合大规模数据（如 10 万 + 点）；


    *   支持点选择和区域筛选，适合深度数据分析；


    *   金融场景中推荐用于内部风险建模（如识别极端风险点）。


*   **核心参数**：



    *   颜色映射（`color`）：引入第四个变量（如行业、时间），增强信息维度；


    *   点大小（`size`）：突出重要数据（如市值大的股票用大点）；


    *   透明度（`opacity`）：避免点重叠，适合高密度数据。


### 四、金融领域典型应用&#xA;



1.  **资产相关性分析**：展示 “收益率、波动率、市值” 的三维关系，识别低波动高收益的资产集群；


2.  **风险管理**：分析 “时间、仓位、VaR” 的动态关系，旋转视角发现极端风险发生的条件；


3.  **客户分群**：以 “资产规模、风险偏好、交易频率” 为轴，聚类高价值客户群体；


4.  **衍生品定价**：探索 “行权价、到期时间、隐含波动率” 的三维分布，验证定价模型合理性。


通过交互式三维散点图，金融分析师可突破二维限制，直观捕捉多变量间的隐性关联，提升决策效率。


> （注：文档部分内容可能由 AI 生成）
>
