# 散点图在R中的实现

散点图是探索两个变量之间关系的重要工具，在 R 语言中可通过基础绘图系统（`base`）和 ggplot2 包实现，以下是具体方法：


### 一、基础绘图系统（`base`）实现散点图&#xA;



1.  **简单散点图绘制**

    使用`plot()`函数，直接指定 x 轴和 y 轴变量即可生成散点图。例如，基于`mtcars`数据集探索汽车重量（`wt`）与马力（`hp`）的关系：




```
# 绘制基础散点图
plot(mtcars$wt, mtcars$hp,
   xlab = "汽车重量（吨）",  # x轴标签
   ylab = "马力",  # y轴标签
   main = "汽车重量与马力的关系",  # 标题
   pch = 16,  # 点的形状（16为实心圆）
   col = "blue",  # 点的颜色
   cex = 1.2)  # 点的大小（1.2倍默认值）


plot(mtcars$wt, mtcars$hp,
   xlab = "汽车重量（吨）",  # x轴标签
   ylab = "马力",  # y轴标签
   main = "汽车重量与马力的关系",  # 标题
   pch = 16,  # 点的形状（16为实心圆）
   col = "blue",  # 点的颜色
   cex = 1.2)  # 点的大小（1.2倍默认值）


   xlab = "汽车重量（吨）",  # x轴标签
   ylab = "马力",  # y轴标签
   main = "汽车重量与马力的关系",  # 标题
   pch = 16,  # 点的形状（16为实心圆）
   col = "blue",  # 点的颜色
   cex = 1.2)  # 点的大小（1.2倍默认值）


   ylab = "马力",  # y轴标签
   main = "汽车重量与马力的关系",  # 标题
   pch = 16,  # 点的形状（16为实心圆）
   col = "blue",  # 点的颜色
   cex = 1.2)  # 点的大小（1.2倍默认值）


   main = "汽车重量与马力的关系",  # 标题
   pch = 16,  # 点的形状（16为实心圆）
   col = "blue",  # 点的颜色
   cex = 1.2)  # 点的大小（1.2倍默认值）


   pch = 16,  # 点的形状（16为实心圆）
   col = "blue",  # 点的颜色
   cex = 1.2)  # 点的大小（1.2倍默认值）


   col = "blue",  # 点的颜色
   cex = 1.2)  # 点的大小（1.2倍默认值）


   cex = 1.2)  # 点的大小（1.2倍默认值）
```

`pch`参数控制点的形状（0-25 代表不同形状），`cex`调整点的大小，`col`设置颜色，通过这些参数可使数据点更易区分。




1.  **分组散点图**

    当数据包含分组变量时，可通过颜色或形状区分不同组别。例如，按变速箱类型（`am`，0 为自动，1 为手动）区分点的样式：




```
# 定义分组颜色和形状
colors <- c("red", "green")[mtcars$am + 1]  # 自动（0）为红色，手动（1）为绿色
shapes <- c(17, 19)[mtcars$am + 1]  # 自动为三角形（17），手动为实心圆（19）

# 绘制分组散点图
plot(mtcars$wt, mtcars$hp,
   xlab = "重量", ylab = "马力",
   main = "按变速箱类型分组的重量与马力关系",
   pch = shapes, col = colors, cex = 1.1)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


colors <- c("red", "green")[mtcars$am + 1]  # 自动（0）为红色，手动（1）为绿色
shapes <- c(17, 19)[mtcars$am + 1]  # 自动为三角形（17），手动为实心圆（19）

# 绘制分组散点图
plot(mtcars$wt, mtcars$hp,
   xlab = "重量", ylab = "马力",
   main = "按变速箱类型分组的重量与马力关系",
   pch = shapes, col = colors, cex = 1.1)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


shapes <- c(17, 19)[mtcars$am + 1]  # 自动为三角形（17），手动为实心圆（19）

# 绘制分组散点图
plot(mtcars$wt, mtcars$hp,
   xlab = "重量", ylab = "马力",
   main = "按变速箱类型分组的重量与马力关系",
   pch = shapes, col = colors, cex = 1.1)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


# 绘制分组散点图
plot(mtcars$wt, mtcars$hp,
   xlab = "重量", ylab = "马力",
   main = "按变速箱类型分组的重量与马力关系",
   pch = shapes, col = colors, cex = 1.1)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


plot(mtcars$wt, mtcars$hp,
   xlab = "重量", ylab = "马力",
   main = "按变速箱类型分组的重量与马力关系",
   pch = shapes, col = colors, cex = 1.1)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


   xlab = "重量", ylab = "马力",
   main = "按变速箱类型分组的重量与马力关系",
   pch = shapes, col = colors, cex = 1.1)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


   main = "按变速箱类型分组的重量与马力关系",
   pch = shapes, col = colors, cex = 1.1)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


   pch = shapes, col = colors, cex = 1.1)
# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


# 添加图例
legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


legend("topright", legend = c("自动变速箱", "手动变速箱"),
     pch = c(17, 19), col = c("red", "green"))


     pch = c(17, 19), col = c("red", "green"))
```

通过分组变量动态分配颜色和形状，能直观呈现不同子群体的分布差异，图例则用于解释分组含义。




1.  **添加拟合线**

    为探索变量间的趋势，可在散点图上添加回归线或平滑曲线。例如，添加线性回归线和局部加权回归曲线（LOESS）：




```
# 绘制散点图
plot(mtcars$wt, mtcars$hp, xlab = "重量", ylab = "马力", pch = 16, col = "darkgray")
# 添加线性回归线（lm()计算线性模型，abline()绘制）
abline(lm(hp ~ wt, data = mtcars), col = "blue", lwd = 2)  # lwd为线宽
# 添加LOESS平滑曲线（lowess()计算，lines()绘制）
lines(lowess(mtcars$wt, mtcars$hp), col = "red", lwd = 2, lty = 2)  # lty为线型（虚线）
# 添加图例
legend("topright", legend = c("数据点", "线性回归", "LOESS平滑"),
     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))


plot(mtcars$wt, mtcars$hp, xlab = "重量", ylab = "马力", pch = 16, col = "darkgray")
# 添加线性回归线（lm()计算线性模型，abline()绘制）
abline(lm(hp ~ wt, data = mtcars), col = "blue", lwd = 2)  # lwd为线宽
# 添加LOESS平滑曲线（lowess()计算，lines()绘制）
lines(lowess(mtcars$wt, mtcars$hp), col = "red", lwd = 2, lty = 2)  # lty为线型（虚线）
# 添加图例
legend("topright", legend = c("数据点", "线性回归", "LOESS平滑"),
     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))


# 添加线性回归线（lm()计算线性模型，abline()绘制）
abline(lm(hp ~ wt, data = mtcars), col = "blue", lwd = 2)  # lwd为线宽
# 添加LOESS平滑曲线（lowess()计算，lines()绘制）
lines(lowess(mtcars$wt, mtcars$hp), col = "red", lwd = 2, lty = 2)  # lty为线型（虚线）
# 添加图例
legend("topright", legend = c("数据点", "线性回归", "LOESS平滑"),
     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))


abline(lm(hp ~ wt, data = mtcars), col = "blue", lwd = 2)  # lwd为线宽
# 添加LOESS平滑曲线（lowess()计算，lines()绘制）
lines(lowess(mtcars$wt, mtcars$hp), col = "red", lwd = 2, lty = 2)  # lty为线型（虚线）
# 添加图例
legend("topright", legend = c("数据点", "线性回归", "LOESS平滑"),
     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))


# 添加LOESS平滑曲线（lowess()计算，lines()绘制）
lines(lowess(mtcars$wt, mtcars$hp), col = "red", lwd = 2, lty = 2)  # lty为线型（虚线）
# 添加图例
legend("topright", legend = c("数据点", "线性回归", "LOESS平滑"),
     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))


lines(lowess(mtcars$wt, mtcars$hp), col = "red", lwd = 2, lty = 2)  # lty为线型（虚线）
# 添加图例
legend("topright", legend = c("数据点", "线性回归", "LOESS平滑"),
     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))


# 添加图例
legend("topright", legend = c("数据点", "线性回归", "LOESS平滑"),
     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))


legend("topright", legend = c("数据点", "线性回归", "LOESS平滑"),
     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))


     pch = c(16, NA, NA), lty = c(NA, 1, 2), col = c("darkgray", "blue", "red"))
```

线性回归线适合呈现线性关系，LOESS 曲线则能捕捉非线性趋势，两者结合可更全面地反映变量关系。


### 二、ggplot2 包实现散点图&#xA;

ggplot2 通过图层叠加的方式构建散点图，灵活性更高，适合复杂可视化需求。




1.  **基础散点图**

    使用`ggplot()`+`geom_point()`函数，指定 x 轴、y 轴和数据来源。例如，使用`iris`数据集展示花瓣长度（`Petal.Length`）与花瓣宽度（`Petal.Width`）的关系：




```
library(ggplot2)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
geom_point(size = 2, color = "purple", shape = 18) +  # 点的大小、颜色、形状
labs(title = "花瓣长度与宽度的关系", x = "花瓣长度（cm）", y = "花瓣宽度（cm）") +
theme_bw()  # 白色背景主题


ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
geom_point(size = 2, color = "purple", shape = 18) +  # 点的大小、颜色、形状
labs(title = "花瓣长度与宽度的关系", x = "花瓣长度（cm）", y = "花瓣宽度（cm）") +
theme_bw()  # 白色背景主题


geom_point(size = 2, color = "purple", shape = 18) +  # 点的大小、颜色、形状
labs(title = "花瓣长度与宽度的关系", x = "花瓣长度（cm）", y = "花瓣宽度（cm）") +
theme_bw()  # 白色背景主题


labs(title = "花瓣长度与宽度的关系", x = "花瓣长度（cm）", y = "花瓣宽度（cm）") +
theme_bw()  # 白色背景主题


theme_bw()  # 白色背景主题
```

`aes()`函数用于映射变量到坐标轴，`geom_point()`的参数直接控制点的外观，主题函数（如`theme_bw()`）可快速调整整体风格。




1.  **按分组变量美化**

    通过`aes()`中的`color`、`shape`或`size`参数将分组变量映射到视觉属性，实现分组展示。例如，按鸢尾花品种（`Species`）区分点的颜色和形状：




```
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species, shape = Species)) +
geom_point(size = 2.5) +  # 点的大小
scale_color_brewer(palette = "Set1") +  # 使用预定义色板
labs(title = "不同品种鸢尾花的花瓣特征", x = "花瓣长度", y = "花瓣宽度") +
theme(plot.title = element_text(hjust = 0.5))  # 标题居中


geom_point(size = 2.5) +  # 点的大小
scale_color_brewer(palette = "Set1") +  # 使用预定义色板
labs(title = "不同品种鸢尾花的花瓣特征", x = "花瓣长度", y = "花瓣宽度") +
theme(plot.title = element_text(hjust = 0.5))  # 标题居中


scale_color_brewer(palette = "Set1") +  # 使用预定义色板
labs(title = "不同品种鸢尾花的花瓣特征", x = "花瓣长度", y = "花瓣宽度") +
theme(plot.title = element_text(hjust = 0.5))  # 标题居中


labs(title = "不同品种鸢尾花的花瓣特征", x = "花瓣长度", y = "花瓣宽度") +
theme(plot.title = element_text(hjust = 0.5))  # 标题居中


theme(plot.title = element_text(hjust = 0.5))  # 标题居中
```

此处`color`和`shape`同时关联`Species`，使不同品种的点在颜色和形状上双重区分，图例自动生成且与分组对应。




1.  **添加统计图层**

    结合`geom_smooth()`添加回归线或置信区间，增强对变量关系的解读：




```
ggplot(mtcars, aes(x = wt, y = hp)) +
geom_point(aes(color = factor(am))) +  # 按am分组着色
geom_smooth(method = "lm", se = TRUE, color = "black") +  # 添加线性回归线及置信区间
labs(title = "汽车重量与马力的关系（含回归线）",&#x20;
     x = "重量", y = "马力",&#x20;
     color = "变速箱类型（0=自动，1=手动）") +
theme_minimal()


geom_point(aes(color = factor(am))) +  # 按am分组着色
geom_smooth(method = "lm", se = TRUE, color = "black") +  # 添加线性回归线及置信区间
labs(title = "汽车重量与马力的关系（含回归线）",&#x20;
     x = "重量", y = "马力",&#x20;
     color = "变速箱类型（0=自动，1=手动）") +
theme_minimal()


geom_smooth(method = "lm", se = TRUE, color = "black") +  # 添加线性回归线及置信区间
labs(title = "汽车重量与马力的关系（含回归线）",&#x20;
     x = "重量", y = "马力",&#x20;
     color = "变速箱类型（0=自动，1=手动）") +
theme_minimal()


labs(title = "汽车重量与马力的关系（含回归线）",&#x20;
     x = "重量", y = "马力",&#x20;
     color = "变速箱类型（0=自动，1=手动）") +
theme_minimal()


     x = "重量", y = "马力",&#x20;
     color = "变速箱类型（0=自动，1=手动）") +
theme_minimal()


     color = "变速箱类型（0=自动，1=手动）") +
theme_minimal()


theme_minimal()
```

`method = "lm"`指定线性回归，`se = TRUE`显示置信区间（灰色阴影部分），帮助判断回归关系的可靠性；若变量关系为非线性，可将`method`改为`"loess"`。




1.  **自定义样式细节**

    调整点的透明度、添加文本标签或参考线，使散点图更清晰：




```
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
geom_point(alpha = 0.7, size = 3) +  # alpha控制透明度（0.7为半透明，避免点重叠）
geom_hline(yintercept = mean(iris$Sepal.Width), color = "red", linetype = "dashed") +  # 水平线（均值）
geom_vline(xintercept = mean(iris$Sepal.Length), color = "blue", linetype = "dashed") +  # 垂直线（均值）
annotate("text", x = 7, y = 4.5, label = "均值参考线", color = "darkgray") +  # 添加注释文本
labs(title = "花萼长度与宽度的分布", x = "花萼长度", y = "花萼宽度")


geom_point(alpha = 0.7, size = 3) +  # alpha控制透明度（0.7为半透明，避免点重叠）
geom_hline(yintercept = mean(iris$Sepal.Width), color = "red", linetype = "dashed") +  # 水平线（均值）
geom_vline(xintercept = mean(iris$Sepal.Length), color = "blue", linetype = "dashed") +  # 垂直线（均值）
annotate("text", x = 7, y = 4.5, label = "均值参考线", color = "darkgray") +  # 添加注释文本
labs(title = "花萼长度与宽度的分布", x = "花萼长度", y = "花萼宽度")


geom_hline(yintercept = mean(iris$Sepal.Width), color = "red", linetype = "dashed") +  # 水平线（均值）
geom_vline(xintercept = mean(iris$Sepal.Length), color = "blue", linetype = "dashed") +  # 垂直线（均值）
annotate("text", x = 7, y = 4.5, label = "均值参考线", color = "darkgray") +  # 添加注释文本
labs(title = "花萼长度与宽度的分布", x = "花萼长度", y = "花萼宽度")


geom_vline(xintercept = mean(iris$Sepal.Length), color = "blue", linetype = "dashed") +  # 垂直线（均值）
annotate("text", x = 7, y = 4.5, label = "均值参考线", color = "darkgray") +  # 添加注释文本
labs(title = "花萼长度与宽度的分布", x = "花萼长度", y = "花萼宽度")


annotate("text", x = 7, y = 4.5, label = "均值参考线", color = "darkgray") +  # 添加注释文本
labs(title = "花萼长度与宽度的分布", x = "花萼长度", y = "花萼宽度")


labs(title = "花萼长度与宽度的分布", x = "花萼长度", y = "花萼宽度")
```

透明度（`alpha`）在数据点密集时尤其有用，可避免重叠导致的信息丢失；参考线和注释能突出关键统计量或特征。


### 三、关键参数与场景选择&#xA;



*   **基础系统**：适合快速绘制简单散点图或进行临时数据探索，代码简洁，通过`plot()`+`points()`+`lines()`可灵活叠加元素。


*   **ggplot2**：适合需要精美样式、多分组对比或复杂统计图层的场景，语法结构化，便于重复使用和修改，尤其适合生成汇报或 publication 级图表。


*   **核心参数**：点的形状（`pch`/`shape`）、颜色（`col`/`color`）、大小（`cex`/`size`）是调整外观的关键；分组变量通过颜色、形状区分可增强数据解读；回归线则用于量化变量关系。


散点图在相关性分析、异常值检测、数据分布探索等场景中应用广泛，通过 R 语言的上述方法，可根据数据特点选择合适的实现方式，高效呈现变量间的关联模式。


> （注：文档部分内容可能由 AI 生成）
>
