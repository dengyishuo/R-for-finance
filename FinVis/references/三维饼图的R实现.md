# 三维饼图的R实现

在 R 语言中，三维饼图的实现依赖可视化包的立体效果支持，常用`plotly`包（交互式）和`rgl`包（三维渲染），以下是具体方法及注意事项：


### 一、`plotly`包实现交互式三维饼图&#xA;

`plotly`包通过`type = "pie"`并设置`hole`参数模拟三维效果，支持悬停查看数据和旋转视角，适合需要简单立体效果的场景。




1.  **基础三维饼图绘制**

    使用`plot_ly()`函数，输入类别和数值向量，通过`opacity`和`marker`参数调整立体视觉效果。例如，展示不同产品的销售额占比：




```
library(plotly)

\# 数据准备（类别≤6个，符合三维饼图适用场景）
products <- c("产品A", "产品B", "产品C", "产品D")
sales <- c(35, 25, 20, 20)  # 销售额占比（总和100）

\# 绘制交互式三维饼图（模拟立体效果）
p <- plot\_ly(
&#x20; labels = \~products,
&#x20; values = \~sales,
&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


\# 数据准备（类别≤6个，符合三维饼图适用场景）
products <- c("产品A", "产品B", "产品C", "产品D")
sales <- c(35, 25, 20, 20)  # 销售额占比（总和100）

\# 绘制交互式三维饼图（模拟立体效果）
p <- plot\_ly(
&#x20; labels = \~products,
&#x20; values = \~sales,
&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


products <- c("产品A", "产品B", "产品C", "产品D")
sales <- c(35, 25, 20, 20)  # 销售额占比（总和100）

\# 绘制交互式三维饼图（模拟立体效果）
p <- plot\_ly(
&#x20; labels = \~products,
&#x20; values = \~sales,
&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


sales <- c(35, 25, 20, 20)  # 销售额占比（总和100）

\# 绘制交互式三维饼图（模拟立体效果）
p <- plot\_ly(
&#x20; labels = \~products,
&#x20; values = \~sales,
&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


\# 绘制交互式三维饼图（模拟立体效果）
p <- plot\_ly(
&#x20; labels = \~products,
&#x20; values = \~sales,
&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


p <- plot\_ly(
&#x20; labels = \~products,
&#x20; values = \~sales,
&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; labels = \~products,
&#x20; values = \~sales,
&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; values = \~sales,
&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; type = "pie",
&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; hole = 0.3,  # 中间空心比例（增强立体视觉）
&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; marker = list(
&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20;   colors = rainbow(length(products)),  # 类别颜色
&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20;   line = list(color = "white", width = 2)  # 扇形边界
&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; ),
&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; opacity = 0.9,  # 透明度（增强层次感）
&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; textinfo = "label+percent",  # 显示标签和百分比
&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; insidetextorientation = "radial"  # 标签径向排列
) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


) %>%
&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; layout(
&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20;   title = "产品销售额占比三维饼图",
&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20;   scene = list(aspectmode = "cube")  # 调整三维比例
&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


&#x20; )

print(p)  # 支持旋转和悬停查看具体数值


print(p)  # 支持旋转和悬停查看具体数值
```

图形特点：




*   通过空心设计（`hole = 0.3`）和颜色渐变模拟立体效果，旋转视角时扇形的叠加关系增强三维感；


*   悬停显示 “产品 A：35%（35）” 等信息，避免比例误读；


*   适合非专业汇报，平衡视觉吸引力和信息传递。


1.  **金融场景应用：投资组合资产占比**

    展示股票、债券、现金等资产的配置比例：




```
\# 模拟资产配置数据
assets <- c("股票", "债券", "现金", "商品")
weights <- c(40, 30, 15, 15)  # 占比

p <- plot\_ly(
&#x20; labels = \~assets,
&#x20; values = \~weights,
&#x20; type = "pie",
&#x20; hole = 0.2,
&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


assets <- c("股票", "债券", "现金", "商品")
weights <- c(40, 30, 15, 15)  # 占比

p <- plot\_ly(
&#x20; labels = \~assets,
&#x20; values = \~weights,
&#x20; type = "pie",
&#x20; hole = 0.2,
&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


weights <- c(40, 30, 15, 15)  # 占比

p <- plot\_ly(
&#x20; labels = \~assets,
&#x20; values = \~weights,
&#x20; type = "pie",
&#x20; hole = 0.2,
&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


p <- plot\_ly(
&#x20; labels = \~assets,
&#x20; values = \~weights,
&#x20; type = "pie",
&#x20; hole = 0.2,
&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


&#x20; labels = \~assets,
&#x20; values = \~weights,
&#x20; type = "pie",
&#x20; hole = 0.2,
&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


&#x20; values = \~weights,
&#x20; type = "pie",
&#x20; hole = 0.2,
&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


&#x20; type = "pie",
&#x20; hole = 0.2,
&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


&#x20; hole = 0.2,
&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


&#x20; marker = list(colors = c("red", "blue", "gray", "green")),
&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


&#x20; textinfo = "label+value",  # 显示具体数值（%）
&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


&#x20; title = "投资组合资产配置占比"
) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


) %>%
&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


&#x20; layout(showlegend = TRUE)  # 显示图例

print(p)


print(p)
```

交互价值：




*   旋转视角让受众从不同角度观察资产结构，适合客户沟通中直观展示配置策略；


*   结合`textinfo`参数确保数值清晰，弥补三维透视可能导致的比例误读。


### 二、`rgl`包实现真实三维饼图&#xA;

`rgl`包通过 3D 渲染绘制立体饼图，支持调整扇形角度、高度和旋转视角，适合需要强立体效果的场景，但代码稍复杂。




1.  **基础三维饼图绘制**

    使用`rgl`包的`pie3d()`函数（需从`plotrix`包扩展），计算扇形角度和高度，实现真实三维效果：




```
library(rgl)
library(plotrix)  # 提供pie3d函数

\# 数据准备
categories <- c("A", "B", "C", "D", "E")
values <- c(20, 15, 25, 10, 30)

\# 绘制三维饼图
open3d()  # 打开3D窗口
pie3d(
&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


library(plotrix)  # 提供pie3d函数

\# 数据准备
categories <- c("A", "B", "C", "D", "E")
values <- c(20, 15, 25, 10, 30)

\# 绘制三维饼图
open3d()  # 打开3D窗口
pie3d(
&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


\# 数据准备
categories <- c("A", "B", "C", "D", "E")
values <- c(20, 15, 25, 10, 30)

\# 绘制三维饼图
open3d()  # 打开3D窗口
pie3d(
&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


categories <- c("A", "B", "C", "D", "E")
values <- c(20, 15, 25, 10, 30)

\# 绘制三维饼图
open3d()  # 打开3D窗口
pie3d(
&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


values <- c(20, 15, 25, 10, 30)

\# 绘制三维饼图
open3d()  # 打开3D窗口
pie3d(
&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


\# 绘制三维饼图
open3d()  # 打开3D窗口
pie3d(
&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


open3d()  # 打开3D窗口
pie3d(
&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


pie3d(
&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


&#x20; values,
&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


&#x20; labels = categories,
&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


&#x20; col = rainbow(length(categories)),
&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


&#x20; radius = 1,  # 饼图半径
&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


&#x20; height = 0.5,  # 立体高度（增强三维感）
&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


&#x20; explode = 0.1,  # 扇形分离度（避免重叠）
&#x20; main = "三维饼图示例"
)


&#x20; main = "三维饼图示例"
)


)
```

图形特点：




*   扇形具有真实的三维体积，高度参数（`height = 0.5`）控制立体厚度，旋转视角可观察侧面和顶部；


*   `explode = 0.1`使扇形轻微分离，避免重叠，适合类别稍多（≤5 个）的场景；


*   需在独立 3D 窗口中操作，支持鼠标拖拽旋转、缩放。


1.  **样式自定义与输出**

    调整颜色、标签和视角，并保存为图片：




```
open3d()
pie3d(
&#x20; values,
&#x20; labels = paste0(categories, " (", values, "%)"),  # 标签含百分比
&#x20; col = terrain.colors(length(categories)),
&#x20; radius = 1.2,
&#x20; height = 0.6,
&#x20; border = "white",  # 扇形边界颜色
&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


pie3d(
&#x20; values,
&#x20; labels = paste0(categories, " (", values, "%)"),  # 标签含百分比
&#x20; col = terrain.colors(length(categories)),
&#x20; radius = 1.2,
&#x20; height = 0.6,
&#x20; border = "white",  # 扇形边界颜色
&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


&#x20; values,
&#x20; labels = paste0(categories, " (", values, "%)"),  # 标签含百分比
&#x20; col = terrain.colors(length(categories)),
&#x20; radius = 1.2,
&#x20; height = 0.6,
&#x20; border = "white",  # 扇形边界颜色
&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


&#x20; labels = paste0(categories, " (", values, "%)"),  # 标签含百分比
&#x20; col = terrain.colors(length(categories)),
&#x20; radius = 1.2,
&#x20; height = 0.6,
&#x20; border = "white",  # 扇形边界颜色
&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


&#x20; col = terrain.colors(length(categories)),
&#x20; radius = 1.2,
&#x20; height = 0.6,
&#x20; border = "white",  # 扇形边界颜色
&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


&#x20; radius = 1.2,
&#x20; height = 0.6,
&#x20; border = "white",  # 扇形边界颜色
&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


&#x20; height = 0.6,
&#x20; border = "white",  # 扇形边界颜色
&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


&#x20; border = "white",  # 扇形边界颜色
&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


&#x20; theta = 45  # 初始视角角度（45度）
)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


)
\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


\# 保存为PNG图片
rgl.snapshot("3d\_pie.png")


rgl.snapshot("3d\_pie.png")
```

自定义价值：




*   标签包含具体数值（如 “A (20%)”），弥补三维透视可能的比例误读；


*   初始视角（`theta = 45`）设置为正面，避免默认角度导致的扇形遮挡。


### 三、关键参数与注意事项&#xA;



1.  **核心参数对比**



| 工具包&#xA; | 立体效果&#xA;        | 交互性&#xA;      | 适用场景&#xA;  | 核心参数&#xA;          |
| -------- | ---------------- | ------------- | ---------- | ------------------ |
| `plotly` | 模拟（空心 + 颜色）&#xA; | 高（旋转、悬停）&#xA; | 汇报、网页&#xA; | `hole`、`opacity`   |
| `rgl`    | 真实（3D 渲染）&#xA;   | 中（窗口交互）&#xA;  | 演示、教学&#xA; | `height`、`explode` |



1.  **注意事项**

*   **类别数量控制**：无论哪种方法，类别数均需≤6 个，否则扇形拥挤（如`rgl`中`explode`参数需增大至 0.2 以上）；


*   **比例准确性**：三维透视可能扭曲视觉比例，务必添加数据标签（如百分比、具体数值）；


*   **适用场景限制**：金融分析、科研等精确场景优先用二维饼图或环形图，三维仅用于非精确的视觉展示；


*   **输出格式**：`plotly`导出为 HTML 便于分享；`rgl`通过`rgl.snapshot()`保存为图片，适合插入报告。


### 四、替代方案推荐&#xA;

鉴于三维饼图的局限性，实际应用中建议优先考虑以下更可靠的可视化方式：




1.  **二维饼图或环形图**：用`ggplot2::geom_col()`或`plotly`的非三维模式，避免透视干扰；


2.  **堆叠柱状图**：`ggplot2::geom_col(position = "fill")`，适合多类别占比对比；


3.  **treemap（树状图）**：`treemap`包，通过面积而非角度展示比例，更直观。


三维饼图在 R 中的实现主要用于特定视觉需求，但其核心价值仍是视觉呈现而非精确分析，使用时需结合场景权衡利弊，避免因形式牺牲数据解读的准确性。


> （注：文档部分内容可能由 AI 生成）
>