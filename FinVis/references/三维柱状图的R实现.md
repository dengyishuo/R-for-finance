# 三维柱状图的R实现

三维柱状图通过在三维坐标系中以柱形的高度、x 轴位置和 y 轴位置展示三个变量的关系，在 R 中可通过`plotly`包（交互式）和`rgl`包（三维交互）实现，以下是具体方法及金融领域应用：


### 一、`plotly`包实现交互式三维柱状图&#xA;

`plotly`包的三维柱状图支持悬停查看数据、旋转视角和缩放，适合多维度数据对比，尤其在金融领域用于展示不同分类下的数值分布。




1.  **基础三维柱状图绘制**

    使用`plot_ly()`函数，指定 x、y 轴为分类或连续变量，z 轴为数值变量，设置`type = "bar3d"`。例如，展示 “产品类型（x）、地区（y）、销售额（z）” 的分布：




```
library(plotly)

\# 模拟销售数据
products <- c("产品A", "产品B", "产品C")
regions <- c("华东", "华南", "华北", "西部")
sales\_data <- expand.grid(产品 = products, 地区 = regions)
sales\_data\$销售额 <- round(runif(nrow(sales\_data), 50, 200), 1)  # 随机销售额

\# 绘制基础三维柱状图
p <- plot\_ly(
&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


\# 模拟销售数据
products <- c("产品A", "产品B", "产品C")
regions <- c("华东", "华南", "华北", "西部")
sales\_data <- expand.grid(产品 = products, 地区 = regions)
sales\_data\$销售额 <- round(runif(nrow(sales\_data), 50, 200), 1)  # 随机销售额

\# 绘制基础三维柱状图
p <- plot\_ly(
&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


products <- c("产品A", "产品B", "产品C")
regions <- c("华东", "华南", "华北", "西部")
sales\_data <- expand.grid(产品 = products, 地区 = regions)
sales\_data\$销售额 <- round(runif(nrow(sales\_data), 50, 200), 1)  # 随机销售额

\# 绘制基础三维柱状图
p <- plot\_ly(
&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


regions <- c("华东", "华南", "华北", "西部")
sales\_data <- expand.grid(产品 = products, 地区 = regions)
sales\_data\$销售额 <- round(runif(nrow(sales\_data), 50, 200), 1)  # 随机销售额

\# 绘制基础三维柱状图
p <- plot\_ly(
&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


sales\_data <- expand.grid(产品 = products, 地区 = regions)
sales\_data\$销售额 <- round(runif(nrow(sales\_data), 50, 200), 1)  # 随机销售额

\# 绘制基础三维柱状图
p <- plot\_ly(
&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


sales\_data\$销售额 <- round(runif(nrow(sales\_data), 50, 200), 1)  # 随机销售额

\# 绘制基础三维柱状图
p <- plot\_ly(
&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


\# 绘制基础三维柱状图
p <- plot\_ly(
&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


p <- plot\_ly(
&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; data = sales\_data,
&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; x = \~产品,  # x轴：产品类型
&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; y = \~地区,  # y轴：销售地区
&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; z = \~销售额,  # z轴：销售额（柱高）
&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; type = "bar3d",
&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; color = \~销售额,  # 按销售额着色（颜色越深值越大）
&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; colors = "Viridis"  # 色板
) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


) %>%
&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; layout(
&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   title = "不同产品在各地区的销售额分布",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   scene = list(
&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;     xaxis = list(title = "产品类型"),
&#x20;     yaxis = list(title = "地区"),
&#x20;     zaxis = list(title = "销售额（万元）")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;     yaxis = list(title = "地区"),
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




*   柱形的 x、y 位置对应产品和地区，高度对应销售额，颜色梯度增强数值差异的可读性；


*   悬停时显示 “产品 A、华东、销售额 = 120.5” 等具体信息；


*   旋转视角可观察不同维度的对比（如从 “地区” 视角看华南地区各产品的销售优势）。


1.  **金融场景应用：多资产类别收益对比**

    展示 “资产类型（x）、年份（y）、年化收益率（z）” 的三维分布，对比不同资产在各年份的表现：




```
\# 模拟金融资产收益数据
assets <- c("股票", "债券", "商品", "现金")
years <- 2018:2023
returns\_data <- expand.grid(资产 = assets, 年份 = years)
returns\_data\$收益率 <- c(
&#x20; rnorm(6, 0.08, 0.05),  # 股票收益（波动大）
&#x20; rnorm(6, 0.03, 0.01),  # 债券收益（稳定）
&#x20; rnorm(6, 0.05, 0.03),  # 商品收益
&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


assets <- c("股票", "债券", "商品", "现金")
years <- 2018:2023
returns\_data <- expand.grid(资产 = assets, 年份 = years)
returns\_data\$收益率 <- c(
&#x20; rnorm(6, 0.08, 0.05),  # 股票收益（波动大）
&#x20; rnorm(6, 0.03, 0.01),  # 债券收益（稳定）
&#x20; rnorm(6, 0.05, 0.03),  # 商品收益
&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


years <- 2018:2023
returns\_data <- expand.grid(资产 = assets, 年份 = years)
returns\_data\$收益率 <- c(
&#x20; rnorm(6, 0.08, 0.05),  # 股票收益（波动大）
&#x20; rnorm(6, 0.03, 0.01),  # 债券收益（稳定）
&#x20; rnorm(6, 0.05, 0.03),  # 商品收益
&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


returns\_data <- expand.grid(资产 = assets, 年份 = years)
returns\_data\$收益率 <- c(
&#x20; rnorm(6, 0.08, 0.05),  # 股票收益（波动大）
&#x20; rnorm(6, 0.03, 0.01),  # 债券收益（稳定）
&#x20; rnorm(6, 0.05, 0.03),  # 商品收益
&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


returns\_data\$收益率 <- c(
&#x20; rnorm(6, 0.08, 0.05),  # 股票收益（波动大）
&#x20; rnorm(6, 0.03, 0.01),  # 债券收益（稳定）
&#x20; rnorm(6, 0.05, 0.03),  # 商品收益
&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; rnorm(6, 0.08, 0.05),  # 股票收益（波动大）
&#x20; rnorm(6, 0.03, 0.01),  # 债券收益（稳定）
&#x20; rnorm(6, 0.05, 0.03),  # 商品收益
&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; rnorm(6, 0.03, 0.01),  # 债券收益（稳定）
&#x20; rnorm(6, 0.05, 0.03),  # 商品收益
&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; rnorm(6, 0.05, 0.03),  # 商品收益
&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; rnorm(6, 0.01, 0.005)  # 现金收益
)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


)
returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


returns\_data\$收益率 <- round(returns\_data\$收益率, 3)

\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


\# 绘制带分组的三维柱状图
p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


p <- plot\_ly(
&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; data = returns\_data,
&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; x = \~资产,
&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; y = \~年份,
&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; z = \~收益率,
&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; type = "bar3d",
&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; color = \~资产,  # 按资产类型着色
&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; colors = c("red", "blue", "green", "gray")
) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


) %>%
&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; layout(
&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   title = "2018-2023年各类资产年化收益率",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   scene = list(
&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;     xaxis = list(title = "资产类型"),
&#x20;     yaxis = list(title = "年份"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;     yaxis = list(title = "年份"),
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




*   旋转视角观察 “股票在 2020 年收益率暴跌” 等极端情况，或 “债券在各年份收益稳定” 的特征；


*   悬停对比同一年份不同资产的收益（如 2021 年商品收益高于股票），辅助资产配置决策。


### 二、`rgl`包实现三维交互柱状图&#xA;

`rgl`包的三维柱状图支持实时旋转、缩放，适合深度探索柱形的空间分布特征，尤其适合展示密集数据的分布模式。




1.  **基础三维柱状图绘制**

    使用`rgl`包的`bar3d()`函数，需手动计算柱形的位置和高度，图形在独立窗口中显示，支持鼠标交互。例如，展示 “温度（x）、压力（y）、产量（z）” 的实验数据：




```
library(rgl)

\# 模拟实验数据（网格状分布）
temp <- seq(20, 40, by = 5)
pressure <- seq(1, 3, by = 0.5)
data <- expand.grid(temp = temp, pressure = pressure)
data\$yield <- 0.5\*data\$temp + 2\*data\$pressure + rnorm(nrow(data), 0, 1.2)  # 产量

\# 计算柱形位置（x和y轴的中心坐标）
x <- as.numeric(factor(data\$temp))  # 温度转换为x轴坐标（1,2,...）
y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


\# 模拟实验数据（网格状分布）
temp <- seq(20, 40, by = 5)
pressure <- seq(1, 3, by = 0.5)
data <- expand.grid(temp = temp, pressure = pressure)
data\$yield <- 0.5\*data\$temp + 2\*data\$pressure + rnorm(nrow(data), 0, 1.2)  # 产量

\# 计算柱形位置（x和y轴的中心坐标）
x <- as.numeric(factor(data\$temp))  # 温度转换为x轴坐标（1,2,...）
y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


temp <- seq(20, 40, by = 5)
pressure <- seq(1, 3, by = 0.5)
data <- expand.grid(temp = temp, pressure = pressure)
data\$yield <- 0.5\*data\$temp + 2\*data\$pressure + rnorm(nrow(data), 0, 1.2)  # 产量

\# 计算柱形位置（x和y轴的中心坐标）
x <- as.numeric(factor(data\$temp))  # 温度转换为x轴坐标（1,2,...）
y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


pressure <- seq(1, 3, by = 0.5)
data <- expand.grid(temp = temp, pressure = pressure)
data\$yield <- 0.5\*data\$temp + 2\*data\$pressure + rnorm(nrow(data), 0, 1.2)  # 产量

\# 计算柱形位置（x和y轴的中心坐标）
x <- as.numeric(factor(data\$temp))  # 温度转换为x轴坐标（1,2,...）
y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


data <- expand.grid(temp = temp, pressure = pressure)
data\$yield <- 0.5\*data\$temp + 2\*data\$pressure + rnorm(nrow(data), 0, 1.2)  # 产量

\# 计算柱形位置（x和y轴的中心坐标）
x <- as.numeric(factor(data\$temp))  # 温度转换为x轴坐标（1,2,...）
y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


data\$yield <- 0.5\*data\$temp + 2\*data\$pressure + rnorm(nrow(data), 0, 1.2)  # 产量

\# 计算柱形位置（x和y轴的中心坐标）
x <- as.numeric(factor(data\$temp))  # 温度转换为x轴坐标（1,2,...）
y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


\# 计算柱形位置（x和y轴的中心坐标）
x <- as.numeric(factor(data\$temp))  # 温度转换为x轴坐标（1,2,...）
y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


x <- as.numeric(factor(data\$temp))  # 温度转换为x轴坐标（1,2,...）
y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


y <- as.numeric(factor(data\$pressure))  # 压力转换为y轴坐标（1,2,...）
z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


z <- data\$yield  # 柱高

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


bar3d(
&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


&#x20; x = x, y = y, z = z,  # 柱形中心坐标和高度
&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


&#x20; width = 0.7, depth = 0.7,  # 柱形宽度和深度（控制粗细）
&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


&#x20; color = rainbow(nrow(data))\[order(z)],  # 按产量着色（值高则色深）
&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


&#x20; xlab = "温度（℃）", ylab = "压力（atm）", zlab = "产量（kg）"
)
title3d(main = "温度、压力与产量的三维关系")


)
title3d(main = "温度、压力与产量的三维关系")


title3d(main = "温度、压力与产量的三维关系")
```

交互操作：




*   左键拖拽旋转视角，观察柱形的高低分布（如高温高压下产量是否显著增加）；


*   右键缩放图形，聚焦低产量或高产量区域的细节（如某温度区间产量普遍偏低）。


1.  **金融场景应用：行业 - 规模 - 风险评级分布**

    展示 “行业（x）、公司规模（y）、风险评级（z）” 的三维分布，风险评级越低（数值越小）代表风险越高：




```
\# 模拟金融数据
industries <- c("金融", "科技", "制造", "消费")
sizes <- c("小型", "中型", "大型")
risk\_data <- expand.grid(行业 = industries, 规模 = sizes)
risk\_data\$风险评级 <- round(runif(nrow(risk\_data), 1, 10), 1)  # 1-10分，10分最低风险

\# 转换分类变量为坐标
x <- as.numeric(factor(risk\_data\$行业))
y <- as.numeric(factor(risk\_data\$规模))
z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


industries <- c("金融", "科技", "制造", "消费")
sizes <- c("小型", "中型", "大型")
risk\_data <- expand.grid(行业 = industries, 规模 = sizes)
risk\_data\$风险评级 <- round(runif(nrow(risk\_data), 1, 10), 1)  # 1-10分，10分最低风险

\# 转换分类变量为坐标
x <- as.numeric(factor(risk\_data\$行业))
y <- as.numeric(factor(risk\_data\$规模))
z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


sizes <- c("小型", "中型", "大型")
risk\_data <- expand.grid(行业 = industries, 规模 = sizes)
risk\_data\$风险评级 <- round(runif(nrow(risk\_data), 1, 10), 1)  # 1-10分，10分最低风险

\# 转换分类变量为坐标
x <- as.numeric(factor(risk\_data\$行业))
y <- as.numeric(factor(risk\_data\$规模))
z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


risk\_data <- expand.grid(行业 = industries, 规模 = sizes)
risk\_data\$风险评级 <- round(runif(nrow(risk\_data), 1, 10), 1)  # 1-10分，10分最低风险

\# 转换分类变量为坐标
x <- as.numeric(factor(risk\_data\$行业))
y <- as.numeric(factor(risk\_data\$规模))
z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


risk\_data\$风险评级 <- round(runif(nrow(risk\_data), 1, 10), 1)  # 1-10分，10分最低风险

\# 转换分类变量为坐标
x <- as.numeric(factor(risk\_data\$行业))
y <- as.numeric(factor(risk\_data\$规模))
z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


\# 转换分类变量为坐标
x <- as.numeric(factor(risk\_data\$行业))
y <- as.numeric(factor(risk\_data\$规模))
z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


x <- as.numeric(factor(risk\_data\$行业))
y <- as.numeric(factor(risk\_data\$规模))
z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


y <- as.numeric(factor(risk\_data\$规模))
z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


z <- risk\_data\$风险评级

\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


\# 绘制三维柱状图
bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


bar3d(
&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


&#x20; x = x, y = y, z = z,
&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


&#x20; width = 0.6, depth = 0.6,
&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


&#x20; color = terrain.colors(nrow(risk\_data))\[order(z)],  # 低风险（高值）为绿色
&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


&#x20; xlab = "行业", ylab = "公司规模", zlab = "风险评级（1-10）"
)
title3d(main = "不同行业与规模的公司风险评级")


)
title3d(main = "不同行业与规模的公司风险评级")


title3d(main = "不同行业与规模的公司风险评级")
```

分析价值：




*   旋转视角发现 “大型科技公司风险评级普遍较高（风险低）” 等规律；


*   对比同行业不同规模公司的风险（如金融行业中小型公司风险评级更低），辅助信贷审批或投资决策。


### 三、关键参数与场景选择&#xA;



*   `plotly`**包优势**：



    *   自动处理分类变量，无需手动转换坐标，适合非编程背景用户；


    *   悬停信息丰富，支持导出为 HTML，适合汇报展示；


    *   金融场景推荐用于客户沟通（如展示不同产品的收益分布）。


*   `rgl`**包优势**：



    *   柱形大小（宽度、深度）可精确控制，适合调整视觉比例；


    *   三维交互更流畅，适合大规模网格数据（如 100 + 柱形）；


    *   金融场景推荐用于内部分析（如行业 - 规模 - 风险的密集数据分布）。


*   **核心参数**：



    *   柱形尺寸：`plotly`通过自动适配避免重叠；`rgl`需手动设置`width`和`depth`（建议小于 1，避免拥挤）；


    *   颜色映射：优先按数值变量着色（如收益率、风险评级），增强数值差异的可读性；


    *   分类变量处理：`plotly`直接支持因子型变量；`rgl`需转换为数值坐标（`as.numeric(factor())`）。


### 四、金融领域典型应用&#xA;



1.  **多维度业绩对比**：展示 “基金类型（x）、成立年限（y）、年化收益（z）” 的三维分布，对比不同类型基金的长期表现；


2.  **风险敞口分析**：以 “行业（x）、地区（y）、风险敞口金额（z）” 为轴，识别高风险区域的集中分布；


3.  **客户分群价值评估**：按 “客户等级（x）、年龄组（y）、年度贡献（z）” 绘制，聚焦高价值客户群体的分布特征；


4.  **衍生品合约分布**：展示 “标的资产（x）、到期月份（y）、持仓量（z）”，监控合约的流动性分布。


通过三维柱状图，金融分析师可直观对比多分类变量下的数值差异，旋转视角发现隐藏的分布规律（如某行业 + 某地区的风险敞口异常高），提升多维度决策的效率。


> （注：文档部分内容可能由 AI 生成）
>
