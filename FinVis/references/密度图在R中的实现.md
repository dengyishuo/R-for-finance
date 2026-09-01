# 密度图在R中的实现

密度图通过平滑曲线展示连续数据的分布规律，在 R 语言中可通过基础绘图系统（`base`）和 ggplot2 包实现，以下是具体方法：


### 一、基础绘图系统（`base`）实现密度图



1.  **简单密度图绘制**

    使用`density()`函数计算密度值，再通过`plot()`函数可视化。例如，基于`mtcars`数据集展示汽车油耗（`mpg`）的密度分布：




```
# 计算密度值
mpg_density <- density(mtcars$mpg)

# 绘制基础密度图
plot(mpg_density,
   main = "汽车油耗密度分布",  # 标题
   xlab = "每加仑英里数",  # x轴标签
   col = "blue",  # 曲线颜色
   lwd = 2)  # 曲线宽度
# 填充曲线下方区域
polygon(mpg_density, col = "lightblue", border = "blue")
```

`density()`函数默认使用高斯核函数计算密度，`plot()`直接绘制曲线，`polygon()`用于填充曲线与 x 轴之间的区域，增强视觉效果。


1.  **多组数据密度对比**

    对多组数据分别计算密度，通过`lines()`函数叠加绘制。例如，按变速箱类型（`am`）对比汽车马力（`hp`）的密度分布：




```
# 按变速箱类型分组
hp_am0 <- mtcars$hp[mtcars$am == 0]  # 自动变速箱
hp_am1 <- mtcars$hp[mtcars$am == 1]  # 手动变速箱

# 计算各组密度
density_am0 <- density(hp_am0)
density_am1 <- density(hp_am1)

# 绘制基础密度图（自动变速箱）
plot(density_am0, main = "不同变速箱类型的马力密度分布",
   xlab = "马力", ylim = c(0, 0.01), col = "red", lwd = 2)
# 叠加手动变速箱密度曲线
lines(density_am1, col = "green", lwd = 2)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     col = c("red", "green"), lwd = 2)
```

通过`ylim`参数统一纵轴范围，确保两组曲线的可比性；不同颜色的曲线直观区分组别差异。




1.  **调整带宽（bandwidth）**

    带宽（`bw`）决定曲线平滑度，带宽越小曲线越灵敏（易受噪声影响），越大则越平滑（可能掩盖细节）。例如，对比不同带宽下的花瓣长度（`Petal.Length`）密度：




```
x <- iris$Petal.Length
# 计算不同带宽的密度
dens_bw1 <- density(x, bw = 0.1)  # 窄带宽
dens_bw2 <- density(x, bw = 0.5)  # 宽带宽

# 绘制对比图
plot(dens_bw1, main = "不同带宽的花瓣长度密度分布",
   xlab = "花瓣长度（cm）", col = "purple", lwd = 2)
lines(dens_bw2, col = "orange", lwd = 2, lty = 2)  # 虚线
legend("topright", legend = c("带宽=0.1", "带宽=0.5"),
     col = c("purple", "orange"), lwd = 2, lty = c(1, 2))

```

带宽可通过`bw`参数手动设置，或使用`bw.nrd0`（默认）等方法自动计算，需根据数据特点调整。


### 二、ggplot2 包实现密度图

ggplot2 通过`geom_density()`函数绘制密度图，支持更灵活的分组对比和样式调整。




1.  **基础密度图**

    使用`ggplot()`+`geom_density()`函数，指定数据和映射关系。例如，展示`iris`数据集花萼宽度（`Sepal.Width`）的密度分布：




```
library(ggplot2)
ggplot(iris, aes(x = Sepal.Width)) +
geom_density(fill = "lightgreen",  # 填充色
             color = "darkgreen",  # 曲线颜色
             alpha = 0.5) +  # 透明度
labs(title = "花萼宽度密度分布", x = "花萼宽度（cm）", y = "密度") +
theme_bw()  # 白色背景主题



```

`fill`参数控制曲线下方填充色，`alpha`调整透明度（避免多组叠加时过于拥挤），语法简洁且样式美观。




1.  **分组密度图与差异对比**

    通过`fill`或`color`参数按分组变量区分曲线，实现多组分布对比。例如，按鸢尾花品种（`Species`）展示花瓣长度（`Petal.Length`）的密度分布：




```
ggplot(iris, aes(x = Petal.Length, fill = Species, color = Species)) +
geom_density(alpha = 0.3) +  # 低透明度便于叠加查看
labs(title = "不同品种鸢尾花的花瓣长度密度分布",
     x = "花瓣长度（cm）", y = "密度",
     fill = "品种", color = "品种") +  # 图例标签
theme_minimal()
```

同一品种的曲线和填充色保持一致，通过曲线的峰值位置和分布范围，可直观判断品种间的花瓣长度差异（如`Setosa`品种峰值最低，`Virginica`品种分布范围最广）。




1.  **调整带宽与坐标轴范围**

    通过`adjust`参数调整带宽（值越大曲线越平滑），结合`xlim()`限制 x 轴范围，聚焦核心分布区域。例如，优化汽车重量（`wt`）的密度图：




```
ggplot(mtcars, aes(x = wt)) +
geom_density(adjust = 1.5,  # 带宽调整为默认值的1.5倍（更平滑）
             fill = "pink", color = "red") +
xlim(1.5, 5.5) +  # 限制x轴范围
labs(title = "汽车重量密度分布（调整带宽）",
     x = "重量（吨）", y = "密度") +
theme(plot.title = element_text(hjust = 0.5))  # 标题居中
```

`adjust`参数比基础系统的`bw`更直观（默认值 1，大于 1 平滑度增加），适合非统计专业用户快速调整曲线形态。




1.  **与直方图结合展示**

    密度图常与直方图配合使用，通过`geom_histogram()`+`geom_density()`叠加图层，兼顾分布细节与整体趋势。例如：




```
ggplot(mtcars, aes(x = hp)) +
geom_histogram(aes(y = ..density..),  # 直方图纵轴转换为密度（与密度图对齐）
               binwidth = 20, fill = "gray", color = "black") +
geom_density(color = "blue", lwd = 1.5) +  # 叠加密度曲线
labs(title = "汽车马力分布（直方图+密度图）",
     x = "马力", y = "密度") +
theme_light()



```

`..density..`是 ggplot2 的内部变量，将直方图的频数转换为密度刻度，使两者纵轴保持一致，便于对比分布形态。


### 三、关键参数与场景选择



*   **带宽设置**：基础系统用`bw`，ggplot2 用`adjust`，核心是平衡平滑度与细节 —— 数据噪声多时增大带宽，需捕捉细微波动时减小带宽。


*   **分组对比**：ggplot2 的`fill`+`alpha`组合更适合多组叠加，基础系统需手动叠加`lines()`，适合简单两组对比。


*   **场景适配**：基础系统适合快速探索数据分布，代码轻量；ggplot2 适合制作汇报级图表，支持自定义主题、图例和标签，扩展性更强。


密度图在数据分布形态分析（如多峰识别、偏态判断）、组间差异对比等场景中应用广泛，通过 R 语言的上述方法，可灵活呈现数据的分布规律，辅助统计分析和决策。


> （注：文档部分内容可能由 AI 生成）
>
