# 三维气泡图的R实现

三维气泡图通过气泡在三维坐标系中的位置（x、y、z 轴）和大小展示四个变量的关系，在 R 中可通过`plotly`包（交互式）和`rgl`包（三维交互）实现，以下是具体方法及金融领域应用：


### 一、`plotly`包实现交互式三维气泡图&#xA;

`plotly`包的三维气泡图支持悬停查看详细数据、旋转视角和缩放，能直观呈现多变量关联，尤其适合金融领域展示资产特征、客户分群等场景。




1.  **基础三维气泡图绘制**

    使用`plot_ly()`函数，指定 x、y、z 轴为三个连续变量，通过`marker = list(size = ...)`设置气泡大小（对应第四个变量），`type = "scatter3d"`。例如，展示 “体重（x）、身高（y）、年龄（z）、收入（气泡大小）” 的关系：




```
library(plotly)

\# 模拟数据
set.seed(123)
n <- 50
data <- data.frame(
&#x20; weight = rnorm(n, 65, 8),  # 体重（kg）
&#x20; height = rnorm(n, 170, 10),  # 身高（cm）
&#x20; age = sample(20:60, n, replace = TRUE),  # 年龄
&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


\# 模拟数据
set.seed(123)
n <- 50
data <- data.frame(
&#x20; weight = rnorm(n, 65, 8),  # 体重（kg）
&#x20; height = rnorm(n, 170, 10),  # 身高（cm）
&#x20; age = sample(20:60, n, replace = TRUE),  # 年龄
&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


set.seed(123)
n <- 50
data <- data.frame(
&#x20; weight = rnorm(n, 65, 8),  # 体重（kg）
&#x20; height = rnorm(n, 170, 10),  # 身高（cm）
&#x20; age = sample(20:60, n, replace = TRUE),  # 年龄
&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


n <- 50
data <- data.frame(
&#x20; weight = rnorm(n, 65, 8),  # 体重（kg）
&#x20; height = rnorm(n, 170, 10),  # 身高（cm）
&#x20; age = sample(20:60, n, replace = TRUE),  # 年龄
&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


data <- data.frame(
&#x20; weight = rnorm(n, 65, 8),  # 体重（kg）
&#x20; height = rnorm(n, 170, 10),  # 身高（cm）
&#x20; age = sample(20:60, n, replace = TRUE),  # 年龄
&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; weight = rnorm(n, 65, 8),  # 体重（kg）
&#x20; height = rnorm(n, 170, 10),  # 身高（cm）
&#x20; age = sample(20:60, n, replace = TRUE),  # 年龄
&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; height = rnorm(n, 170, 10),  # 身高（cm）
&#x20; age = sample(20:60, n, replace = TRUE),  # 年龄
&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; age = sample(20:60, n, replace = TRUE),  # 年龄
&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; income = abs(rnorm(n, 5000, 2000))  # 收入（气泡大小）
)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


)

\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


\# 绘制基础三维气泡图
p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


p <- plot\_ly(
&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; data = data,
&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; x = \~weight,  # x轴：体重
&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; y = \~height,  # y轴：身高
&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; z = \~age,     # z轴：年龄
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; mode = "markers",
&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; marker = list(
&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   size = \~income/100,  # 气泡大小（收入/100，避免过大）
&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   sizemin = 3,  # 最小气泡大小
&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   color = \~income,  # 按收入着色（值高则色深）
&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   colors = "RdBu",  # 色板
&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   opacity = 0.7  # 透明度（避免重叠遮挡）
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


) %>%
&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20; layout(
&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   title = "体重、身高、年龄与收入的三维关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;   scene = list(
&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;     xaxis = list(title = "体重（kg）"),
&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;     yaxis = list(title = "身高（cm）"),
&#x20;     zaxis = list(title = "年龄")
&#x20;   )
&#x20; )

print(p)  # 支持旋转视角和悬停查看具体数值


&#x20;     zaxis = list(title = "年龄")
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




*   气泡的 x、y、z 位置对应体重、身高、年龄，大小和颜色对应收入，同时呈现四个变量的关联；


*   悬停时显示 “weight=70, height=175, age=35, income=6200” 等完整信息；


*   旋转视角可观察 “高收入群体是否集中在特定年龄或体型区间”（如中年、中等体重群体收入较高）。


1.  **金融场景应用：股票特征多维度分析**

    展示 “市盈率（x）、波动率（y）、收益率（z）、市值（气泡大小）” 的关系，分析股票的风险收益特征：




```
\# 模拟股票数据（50只股票）
set.seed(456)
stock\_data <- data.frame(
&#x20; pe = rnorm(50, 20, 5),  # 市盈率
&#x20; vol = rnorm(50, 0.15, 0.05),  # 波动率
&#x20; return = rnorm(50, 0.08, 0.03),  # 年化收益率
&#x20; market\_cap = abs(rnorm(50, 100, 50))  # 市值（亿元，气泡大小）
)

\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


set.seed(456)
stock\_data <- data.frame(
&#x20; pe = rnorm(50, 20, 5),  # 市盈率
&#x20; vol = rnorm(50, 0.15, 0.05),  # 波动率
&#x20; return = rnorm(50, 0.08, 0.03),  # 年化收益率
&#x20; market\_cap = abs(rnorm(50, 100, 50))  # 市值（亿元，气泡大小）
)

\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


stock\_data <- data.frame(
&#x20; pe = rnorm(50, 20, 5),  # 市盈率
&#x20; vol = rnorm(50, 0.15, 0.05),  # 波动率
&#x20; return = rnorm(50, 0.08, 0.03),  # 年化收益率
&#x20; market\_cap = abs(rnorm(50, 100, 50))  # 市值（亿元，气泡大小）
)

\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; pe = rnorm(50, 20, 5),  # 市盈率
&#x20; vol = rnorm(50, 0.15, 0.05),  # 波动率
&#x20; return = rnorm(50, 0.08, 0.03),  # 年化收益率
&#x20; market\_cap = abs(rnorm(50, 100, 50))  # 市值（亿元，气泡大小）
)

\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; vol = rnorm(50, 0.15, 0.05),  # 波动率
&#x20; return = rnorm(50, 0.08, 0.03),  # 年化收益率
&#x20; market\_cap = abs(rnorm(50, 100, 50))  # 市值（亿元，气泡大小）
)

\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; return = rnorm(50, 0.08, 0.03),  # 年化收益率
&#x20; market\_cap = abs(rnorm(50, 100, 50))  # 市值（亿元，气泡大小）
)

\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; market\_cap = abs(rnorm(50, 100, 50))  # 市值（亿元，气泡大小）
)

\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


)

\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


\# 绘制带分组的三维气泡图（按行业着色）
stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


stock\_data\$industry <- sample(c("科技", "金融", "消费"), 50, replace = TRUE)  # 行业分组
p <- plot\_ly(
&#x20; data = stock\_data,
&#x20; x = \~pe,
&#x20; y = \~vol,
&#x20; z = \~return,
&#x20; type = "scatter3d",
&#x20; mode = "markers",
&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
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
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
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
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
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
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
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
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
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
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
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
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
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
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; color = \~industry,  # 按行业着色
&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; colors = c("red", "blue", "green"),
&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; marker = list(
&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   size = \~market\_cap/5,  # 市值/5控制气泡大小
&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   sizemin = 4,
&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   opacity = 0.8
&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; )
) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


) %>%
&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20; layout(
&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
&#x20;   scene = list(
&#x20;     xaxis = list(title = "市盈率"),
&#x20;     yaxis = list(title = "波动率"),
&#x20;     zaxis = list(title = "年化收益率")
&#x20;   )
&#x20; )

print(p)


&#x20;   title = "股票市盈率、波动率、收益率与市值的关系",
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




*   旋转视角发现 “科技股波动率高但收益率分化大，金融股市值大但收益率稳定” 等规律；


*   悬停对比高市值股票的风险收益特征（如某消费股市值大、低波动、高收益，属优质资产）；


*   通过气泡大小快速识别大盘股（大气泡）的分布区域，辅助资产配置。


### 二、`rgl`包实现三维交互气泡图&#xA;

`rgl`包的三维气泡图支持实时旋转、缩放，适合深度探索气泡的空间分布，尤其适合高密度数据的模式识别。




1.  **基础三维气泡图绘制**

    使用`rgl`包的`spheres3d()`函数，通过`radius`参数控制气泡半径（对应第四个变量），图形在独立窗口中显示，支持鼠标交互。例如，展示 “温度（x）、pH 值（y）、反应速率（z）、浓度（气泡大小）” 的实验数据：




```
library(rgl)

\# 模拟实验数据
temp <- seq(20, 40, by = 2)
ph <- seq(5, 9, by = 0.5)
data <- expand.grid(temp = temp, ph = ph)
data\$rate <- 0.3\*data\$temp - 0.5\*data\$ph + rnorm(nrow(data), 0, 1)  # 反应速率
data\$concentration <- abs(0.1\*data\$temp + 0.2\*data\$ph + rnorm(nrow(data), 0, 0.5))  # 浓度

\# 绘制三维气泡图（气泡半径与浓度成正比）
spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


\# 模拟实验数据
temp <- seq(20, 40, by = 2)
ph <- seq(5, 9, by = 0.5)
data <- expand.grid(temp = temp, ph = ph)
data\$rate <- 0.3\*data\$temp - 0.5\*data\$ph + rnorm(nrow(data), 0, 1)  # 反应速率
data\$concentration <- abs(0.1\*data\$temp + 0.2\*data\$ph + rnorm(nrow(data), 0, 0.5))  # 浓度

\# 绘制三维气泡图（气泡半径与浓度成正比）
spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


temp <- seq(20, 40, by = 2)
ph <- seq(5, 9, by = 0.5)
data <- expand.grid(temp = temp, ph = ph)
data\$rate <- 0.3\*data\$temp - 0.5\*data\$ph + rnorm(nrow(data), 0, 1)  # 反应速率
data\$concentration <- abs(0.1\*data\$temp + 0.2\*data\$ph + rnorm(nrow(data), 0, 0.5))  # 浓度

\# 绘制三维气泡图（气泡半径与浓度成正比）
spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


ph <- seq(5, 9, by = 0.5)
data <- expand.grid(temp = temp, ph = ph)
data\$rate <- 0.3\*data\$temp - 0.5\*data\$ph + rnorm(nrow(data), 0, 1)  # 反应速率
data\$concentration <- abs(0.1\*data\$temp + 0.2\*data\$ph + rnorm(nrow(data), 0, 0.5))  # 浓度

\# 绘制三维气泡图（气泡半径与浓度成正比）
spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


data <- expand.grid(temp = temp, ph = ph)
data\$rate <- 0.3\*data\$temp - 0.5\*data\$ph + rnorm(nrow(data), 0, 1)  # 反应速率
data\$concentration <- abs(0.1\*data\$temp + 0.2\*data\$ph + rnorm(nrow(data), 0, 0.5))  # 浓度

\# 绘制三维气泡图（气泡半径与浓度成正比）
spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


data\$rate <- 0.3\*data\$temp - 0.5\*data\$ph + rnorm(nrow(data), 0, 1)  # 反应速率
data\$concentration <- abs(0.1\*data\$temp + 0.2\*data\$ph + rnorm(nrow(data), 0, 0.5))  # 浓度

\# 绘制三维气泡图（气泡半径与浓度成正比）
spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


data\$concentration <- abs(0.1\*data\$temp + 0.2\*data\$ph + rnorm(nrow(data), 0, 0.5))  # 浓度

\# 绘制三维气泡图（气泡半径与浓度成正比）
spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


\# 绘制三维气泡图（气泡半径与浓度成正比）
spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


spheres3d(
&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


&#x20; x = data\$temp,
&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


&#x20; y = data\$ph,
&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


&#x20; z = data\$rate,
&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


&#x20; radius = data\$concentration\*0.3,  # 半径=浓度\*0.3（控制大小）
&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


&#x20; color = rainbow(nrow(data))\[order(data\$rate)],  # 按反应速率着色
&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


&#x20; alpha = 0.7  # 透明度
)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


)
axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


axes3d(xlab = "温度（℃）", ylab = "pH值", zlab = "反应速率")
title3d(main = "温度、pH值、反应速率与浓度的三维关系")


title3d(main = "温度、pH值、反应速率与浓度的三维关系")
```

交互操作：




*   左键拖拽旋转视角，观察 “高浓度下反应速率是否显著提升”（如大半径气泡是否集中在高 z 值区域）；


*   右键缩放图形，聚焦低浓度或高浓度区域的细节（如低 pH 值时浓度对反应速率的影响）。


1.  **金融场景应用：贷款客户风险评估**

    分析 “收入（x）、负债比例（y）、信用评分（z）、贷款金额（气泡大小）” 的关系，评估客户的贷款风险：




```
\# 模拟贷款客户数据（100个客户）
set.seed(789)
loan\_data <- data.frame(
&#x20; income = rnorm(100, 10, 3),  # 年收入（万元）
&#x20; debt\_ratio = runif(100, 0.1, 0.7),  # 负债比例
&#x20; credit\_score = rnorm(100, 650, 50),  # 信用评分
&#x20; loan\_amount = abs(rnorm(100, 50, 20))  # 贷款金额（万元，气泡大小）
)

\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


set.seed(789)
loan\_data <- data.frame(
&#x20; income = rnorm(100, 10, 3),  # 年收入（万元）
&#x20; debt\_ratio = runif(100, 0.1, 0.7),  # 负债比例
&#x20; credit\_score = rnorm(100, 650, 50),  # 信用评分
&#x20; loan\_amount = abs(rnorm(100, 50, 20))  # 贷款金额（万元，气泡大小）
)

\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


loan\_data <- data.frame(
&#x20; income = rnorm(100, 10, 3),  # 年收入（万元）
&#x20; debt\_ratio = runif(100, 0.1, 0.7),  # 负债比例
&#x20; credit\_score = rnorm(100, 650, 50),  # 信用评分
&#x20; loan\_amount = abs(rnorm(100, 50, 20))  # 贷款金额（万元，气泡大小）
)

\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; income = rnorm(100, 10, 3),  # 年收入（万元）
&#x20; debt\_ratio = runif(100, 0.1, 0.7),  # 负债比例
&#x20; credit\_score = rnorm(100, 650, 50),  # 信用评分
&#x20; loan\_amount = abs(rnorm(100, 50, 20))  # 贷款金额（万元，气泡大小）
)

\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; debt\_ratio = runif(100, 0.1, 0.7),  # 负债比例
&#x20; credit\_score = rnorm(100, 650, 50),  # 信用评分
&#x20; loan\_amount = abs(rnorm(100, 50, 20))  # 贷款金额（万元，气泡大小）
)

\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; credit\_score = rnorm(100, 650, 50),  # 信用评分
&#x20; loan\_amount = abs(rnorm(100, 50, 20))  # 贷款金额（万元，气泡大小）
)

\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; loan\_amount = abs(rnorm(100, 50, 20))  # 贷款金额（万元，气泡大小）
)

\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


)

\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


\# 绘制三维气泡图（按风险等级着色：信用评分<600为高风险）
loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


loan\_data\$risk <- ifelse(loan\_data\$credit\_score < 600, "高风险", "低风险")
spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


spheres3d(
&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; x = loan\_data\$income,
&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; y = loan\_data\$debt\_ratio,
&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; z = loan\_data\$credit\_score,
&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; radius = loan\_data\$loan\_amount/10,  # 贷款金额/10控制半径
&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; color = ifelse(loan\_data\$risk == "高风险", "red", "green"),
&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


&#x20; alpha = 0.6
)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


)
axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


axes3d(xlab = "年收入（万元）", ylab = "负债比例", zlab = "信用评分")
title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


title3d(main = "贷款客户风险评估三维气泡图")
legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)


legend3d("topright", legend = c("高风险", "低风险"), col = c("red", "green"), pch = 16)
```

分析价值：




*   旋转视角发现 “高负债比例且低收入的高风险客户（红色）是否申请了大额贷款”（大半径红色气泡）；


*   对比低风险客户的特征（如绿色气泡集中在高收入、低负债、高信用评分区域），辅助贷款审批决策。


### 三、关键参数与场景选择&#xA;



*   `plotly`**包优势**：



    *   支持自动映射分类变量（如行业、风险等级），无需手动转换颜色；


    *   悬停信息完整，直接显示四个变量的数值，适合汇报展示；


    *   金融场景推荐用于客户沟通（如资产配置方案中的股票特征分析）。


*   `rgl`**包优势**：



    *   气泡半径（`radius`）可精确控制，适合调整视觉比例（如避免大气泡遮挡小气泡）；


    *   三维交互更流畅，适合大规模数据（如 1000 + 气泡）；


    *   金融场景推荐用于内部风险建模（如贷款客户的密集数据分布）。


*   **核心参数**：



    *   气泡大小：`plotly`通过`size`参数设置（建议用原始值 / 缩放因子，避免过大）；`rgl`通过`radius`参数控制（通常≤1，避免重叠）；


    *   颜色与透明度：优先按分类变量（如行业、风险等级）着色，`alpha`或`opacity`设置为 0.5-0.8（平衡可读性和重叠问题）；


    *   数据预处理：确保气泡大小变量为非负值（如市值、贷款金额），负值需取绝对值（`abs()`）。


### 四、金融领域典型应用&#xA;



1.  **资产组合优化**：展示 “收益率（x）、波动率（y）、夏普比率（z）、市值（气泡大小）” 的三维分布，筛选低波动高收益的资产；


2.  **客户分群管理**：以 “资产规模（x）、交易频率（y）、风险偏好（z）、贡献度（气泡大小）” 为轴，识别高价值客户群体；


3.  **衍生品风险对冲**：分析 “标的价格（x）、到期时间（y）、Delta 值（z）、持仓量（气泡大小）” 的关系，评估对冲效果；


4.  **宏观经济指标联动**：绘制 “GDP 增速（x）、通胀率（y）、利率（z）、货币供应量（气泡大小）” 的气泡图，观察经济指标的协同变化。


通过三维气泡图，金融分析师可同时捕捉四个变量的关联模式，旋转视角发现传统二维图中隐藏的规律（如 “高市值股票是否集中在低波动高收益区域”），提升多维度决策的准确性。


> （注：文档部分内容可能由 AI 生成）
>