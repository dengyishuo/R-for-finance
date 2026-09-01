# 箱线图在R中的实现

箱线图是呈现数据分布特征（如中位数、四分位距、异常值）的有效工具，在 R 语言中可通过基础绘图系统（`base`）和 ggplot2 包实现，以下是具体方法：


### 一、基础绘图系统（`base`）实现箱线图



1.  **简单箱线图绘制**

    使用`boxplot()`函数，直接指定数据即可生成箱线图。例如，基于`mtcars`数据集展示汽车油耗（`mpg`）的分布：




```
# 绘制基础箱线图
boxplot(mtcars$mpg,
       main = "汽车油耗分布",  # 标题
       xlab = "汽车类型",  # x轴标签（单组数据可简化）
       ylab = "每加仑英里数",  # y轴标签
       col = "lightblue",  # 箱体颜色
       border = "black",  # 边框颜色
       horizontal = FALSE)  # 垂直箱线图（默认）
```

箱线图中，箱体代表四分位距（Q1-Q3），中间横线为中位数，须线延伸至 1.5 倍四分位距内的最值，超出部分为异常值（圆点），直观呈现数据的集中趋势和离散程度。




1.  **多组数据箱线图**

    当数据包含分组变量时，可通过公式形式（`y ~ group`）绘制多组箱线图。例如，按气缸数（`cyl`）分组展示汽车马力（`hp`）的分布差异：




```
# 按气缸数分组绘制箱线图
boxplot(hp ~ cyl, data = mtcars,
       main = "不同气缸数的汽车马力分布",
       xlab = "气缸数", ylab = "马力",
       col = terrain.colors(3),  # 每组箱体颜色不同
       notch = TRUE)  # 箱体添加凹口（用于中位数差异检验）
```

公式`hp ~ cyl`表示 “马力（y 轴）按气缸数（x 轴）分组”，`notch = TRUE`添加的凹口若不重叠，提示两组中位数存在显著差异（近似统计检验），增强组间对比的信息量。




1.  **添加数据点与自定义须线**

    结合`stripchart()`函数添加原始数据点，展示数据分布细节；通过`range`参数调整须线范围。例如：




```
# 绘制箱线图
boxplot(mpg ~ am, data = mtcars,  # 按变速箱类型（am）分组
       main = "不同变速箱类型的油耗分布（含原始数据）",
       xlab = "变速箱类型（0=自动，1=手动）", ylab = "油耗",
       col = c("pink", "lightgreen"),
       range = 2)  # 须线延伸至2倍四分位距（默认1.5）
# 叠加原始数据点（随机抖动避免重叠）
stripchart(mpg ~ am, data = mtcars, add = TRUE,
          vertical = TRUE, method = "jitter",  # 垂直抖动
          pch = 16, col = "darkgray", cex = 0.8)  # 点的样式
```

`range`参数控制须线长度（值越大，异常值越少），`stripchart()`添加的原始数据点可补充箱线图未呈现的分布细节（如数据密度）。


### 二、ggplot2 包实现箱线图

ggplot2 通过`geom_boxplot()`函数绘制箱线图，支持更灵活的分组对比和样式调整。




1.  **基础箱线图**

    使用`ggplot()`+`geom_boxplot()`函数，指定数据和映射关系。例如，展示`iris`数据集花萼长度（`Sepal.Length`）的分布：




```
library(ggplot2)
ggplot(iris, aes(x = "", y = Sepal.Length)) +  # x轴为空（单组数据）
 geom_boxplot(fill = "purple",  # 箱体填充色
              color = "black",  # 边框色
              outlier.color = "red",  # 异常值颜色
              outlier.size = 2) +  # 异常值大小
 labs(title = "花萼长度分布", x = "", y = "花萼长度（cm）") +
 theme_minimal()
```

`outlier.color`和`outlier.size`参数可单独设置异常值样式，使其更易识别。




1.  **分组箱线图与统计信息**

    通过`x`参数指定分组变量，结合`stat_summary()`添加自定义统计量（如均值点）。例如，按鸢尾花品种（`Species`）分组展示花瓣宽度（`Petal.Width`）的分布：




```
ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) +
 geom_boxplot(alpha = 0.7) +  # 箱体透明度
 stat_summary(fun = mean,  # 添加均值点
              geom = "point", shape = 23, size = 3, fill = "white") +
 labs(title = "不同品种鸢尾花的花瓣宽度分布",
      x = "品种", y = "花瓣宽度（cm）", fill = "品种") +
 theme(plot.title = element_text(hjust = 0.5))  # 标题居中
```

`fill = Species`使不同品种的箱体颜色不同，`stat_summary()`添加的均值点（钻石形）与中位数线（箱体中的横线）对比，可判断数据分布是否对称（均值≈中位数则对称）。




1.  **调整箱体样式与坐标转换**

    自定义箱体边框、须线样式，或对偏态数据进行坐标转换（如对数转换）。例如，优化汽车重量（`wt`）按变速箱类型（`am`）分组的箱线图：




```
ggplot(mtcars, aes(x = factor(am), y = wt, fill = factor(am))) +
 geom_boxplot(linewidth = 1,  # 边框线宽
              linetype = "dashed",  # 边框线型
              outlier.shape = 17) +  # 异常值形状（三角形）
 scale_x_discrete(labels = c("自动", "手动")) +  # x轴标签替换
 scale_fill_brewer(palette = "Set2") +  # 使用预定义色板
 labs(title = "不同变速箱类型的汽车重量分布",
      x = "变速箱类型", y = "重量（吨）", fill = "变速箱类型") +
 theme_bw()
```

对分类变量（如`am`）使用`factor()`转换，确保按类别而非数值排序；`scale_x_discrete()`可自定义分组标签，增强可读性。




1.  **添加原始数据点与抖动**

    结合`geom_jitter()`添加原始数据点（随机抖动避免重叠），补充箱线图的分布细节。例如：




```
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
 geom_boxplot(fill = "lightgray", outlier.color = NA) +  # 隐藏箱线图异常值（避免重复）
 geom_jitter(alpha = 0.6, size = 2, color = "blue") +  # 抖动显示所有原始数据点
 labs(title = "不同气缸数的油耗分布（含原始数据）",
      x = "气缸数", y = "油耗") +
 theme_light()
```

`outlier.color = NA`隐藏箱线图自带的异常值，避免与`geom_jitter()`的点重复；抖动点可展示数据在箱体范围内的实际分布密度。


### 三、关键参数与场景选择



*   **分组方式**：基础系统通过公式`y ~ group`，ggplot2 通过`x = group`，均支持多组数据并行对比，适合展示类别变量对连续变量的影响。


*   **异常值处理**：默认通过 1.5 倍四分位距识别异常值，可通过`range`（基础）或`outlier.xxx`（ggplot2）参数调整样式或隐藏，避免异常值干扰整体分布的观察。


*   **统计补充**：ggplot2 的`stat_summary()`可灵活添加均值、标准差等统计量，基础系统需通过`points()`手动叠加，适合需要增强统计信息的场景。


*   **场景适配**：基础系统的`boxplot()`适合快速数据探索，代码简洁；ggplot2 在分组样式、统计叠加、主题美化上更优，适合制作汇报或 publication 级图表。


箱线图在实验数据对比、质量控制、群体差异分析等场景中应用广泛，通过 R 语言的上述方法，可根据数据特点选择合适的实现方式，高效呈现数据的分布特征与组间差异。


> （注：文档部分内容可能由 AI 生成）
>
