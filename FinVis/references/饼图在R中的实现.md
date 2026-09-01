# 饼图在R中的实现

在 R 语言中，饼图（含环形图）的实现可通过基础绘图系统（`base`）和 ggplot2 包完成，两种方式各有优势，以下是具体实现方法：


### 一、基础绘图系统（`base`）实现饼图&#xA;



1.  **简单饼图绘制**

    使用`pie()`函数，直接输入数值向量即可生成饼图，数值对应各扇形的比例。例如，基于`mtcars`数据集统计不同气缸数（`cyl`）的车辆占比：




```
\# 统计频数并计算占比
cyl\_counts <- table(mtcars\$cyl)
cyl\_percent <- prop.table(cyl\_counts) \* 100  # 转换为百分比

\# 绘制简单饼图
pie(cyl\_percent,
&#x20;   labels = paste(names(cyl\_percent), paste0(round(cyl\_percent, 1), "%")),  # 标签（类别+百分比）
&#x20;   col = rainbow(length(cyl\_counts)),  # 彩虹色配色
&#x20;   main = "不同气缸数车辆的占比分布")  # 标题


cyl\_counts <- table(mtcars\$cyl)
cyl\_percent <- prop.table(cyl\_counts) \* 100  # 转换为百分比

\# 绘制简单饼图
pie(cyl\_percent,
&#x20;   labels = paste(names(cyl\_percent), paste0(round(cyl\_percent, 1), "%")),  # 标签（类别+百分比）
&#x20;   col = rainbow(length(cyl\_counts)),  # 彩虹色配色
&#x20;   main = "不同气缸数车辆的占比分布")  # 标题


cyl\_percent <- prop.table(cyl\_counts) \* 100  # 转换为百分比

\# 绘制简单饼图
pie(cyl\_percent,
&#x20;   labels = paste(names(cyl\_percent), paste0(round(cyl\_percent, 1), "%")),  # 标签（类别+百分比）
&#x20;   col = rainbow(length(cyl\_counts)),  # 彩虹色配色
&#x20;   main = "不同气缸数车辆的占比分布")  # 标题


\# 绘制简单饼图
pie(cyl\_percent,
&#x20;   labels = paste(names(cyl\_percent), paste0(round(cyl\_percent, 1), "%")),  # 标签（类别+百分比）
&#x20;   col = rainbow(length(cyl\_counts)),  # 彩虹色配色
&#x20;   main = "不同气缸数车辆的占比分布")  # 标题


pie(cyl\_percent,
&#x20;   labels = paste(names(cyl\_percent), paste0(round(cyl\_percent, 1), "%")),  # 标签（类别+百分比）
&#x20;   col = rainbow(length(cyl\_counts)),  # 彩虹色配色
&#x20;   main = "不同气缸数车辆的占比分布")  # 标题


&#x20;   labels = paste(names(cyl\_percent), paste0(round(cyl\_percent, 1), "%")),  # 标签（类别+百分比）
&#x20;   col = rainbow(length(cyl\_counts)),  # 彩虹色配色
&#x20;   main = "不同气缸数车辆的占比分布")  # 标题


&#x20;   col = rainbow(length(cyl\_counts)),  # 彩虹色配色
&#x20;   main = "不同气缸数车辆的占比分布")  # 标题


&#x20;   main = "不同气缸数车辆的占比分布")  # 标题
```

`pie()`函数默认按输入数值的比例分配扇形面积，`labels`参数用于添加类别标签，`col`指定扇形颜色。




1.  **带图例与突出显示的饼图**

    当类别名称较长时，可将标签放在图例中，并通过`explode`参数突出某一扇形。例如，展示不同变速箱类型（`am`）的车辆占比：




```
am\_counts <- table(mtcars\$am)
names(am\_counts) <- c("自动变速箱", "手动变速箱")  # 重命名类别

\# 设置突出参数（第2个扇形向外突出）
explode <- c(0, 0.1)

\# 绘制带图例的饼图
pie(am\_counts,
&#x20;   labels = "",  # 不显示扇区标签（通过图例展示）
&#x20;   col = c("lightcoral", "lightgreen"),
&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


names(am\_counts) <- c("自动变速箱", "手动变速箱")  # 重命名类别

\# 设置突出参数（第2个扇形向外突出）
explode <- c(0, 0.1)

\# 绘制带图例的饼图
pie(am\_counts,
&#x20;   labels = "",  # 不显示扇区标签（通过图例展示）
&#x20;   col = c("lightcoral", "lightgreen"),
&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


\# 设置突出参数（第2个扇形向外突出）
explode <- c(0, 0.1)

\# 绘制带图例的饼图
pie(am\_counts,
&#x20;   labels = "",  # 不显示扇区标签（通过图例展示）
&#x20;   col = c("lightcoral", "lightgreen"),
&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


explode <- c(0, 0.1)

\# 绘制带图例的饼图
pie(am\_counts,
&#x20;   labels = "",  # 不显示扇区标签（通过图例展示）
&#x20;   col = c("lightcoral", "lightgreen"),
&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


\# 绘制带图例的饼图
pie(am\_counts,
&#x20;   labels = "",  # 不显示扇区标签（通过图例展示）
&#x20;   col = c("lightcoral", "lightgreen"),
&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


pie(am\_counts,
&#x20;   labels = "",  # 不显示扇区标签（通过图例展示）
&#x20;   col = c("lightcoral", "lightgreen"),
&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


&#x20;   labels = "",  # 不显示扇区标签（通过图例展示）
&#x20;   col = c("lightcoral", "lightgreen"),
&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


&#x20;   col = c("lightcoral", "lightgreen"),
&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


&#x20;   explode = explode,  # 突出指定扇形
&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


&#x20;   main = "变速箱类型占比（突出手动变速箱）")
\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


\# 添加图例
legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))


legend("right", legend = names(am\_counts), fill = c("lightcoral", "lightgreen"))
```

`explode`参数为向量，每个元素对应一扇形的突出距离（0 为不突出），适合强调关键类别。




1.  **环形图绘制**

    环形图可通过在饼图中心添加白色圆形实现。例如，基于上述气缸数数据绘制环形图：




```
\# 绘制基础饼图
pie(cyl\_percent,
&#x20;   labels = names(cyl\_percent),
&#x20;   col = terrain.colors(length(cyl\_counts)),
&#x20;   main = "车辆气缸数环形图")
\# 添加白色圆形（形成环形）
par(new = TRUE)  # 允许在同一图上叠加
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆


pie(cyl\_percent,
&#x20;   labels = names(cyl\_percent),
&#x20;   col = terrain.colors(length(cyl\_counts)),
&#x20;   main = "车辆气缸数环形图")
\# 添加白色圆形（形成环形）
par(new = TRUE)  # 允许在同一图上叠加
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆


&#x20;   labels = names(cyl\_percent),
&#x20;   col = terrain.colors(length(cyl\_counts)),
&#x20;   main = "车辆气缸数环形图")
\# 添加白色圆形（形成环形）
par(new = TRUE)  # 允许在同一图上叠加
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆


&#x20;   col = terrain.colors(length(cyl\_counts)),
&#x20;   main = "车辆气缸数环形图")
\# 添加白色圆形（形成环形）
par(new = TRUE)  # 允许在同一图上叠加
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆


&#x20;   main = "车辆气缸数环形图")
\# 添加白色圆形（形成环形）
par(new = TRUE)  # 允许在同一图上叠加
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆


\# 添加白色圆形（形成环形）
par(new = TRUE)  # 允许在同一图上叠加
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆


par(new = TRUE)  # 允许在同一图上叠加
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆


plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆


symbols(0, 0, circles = 0.7, add = TRUE, bg = "white", inches = FALSE)  # 中心白色圆
```

核心思路是在饼图中心绘制一个白色圆形，通过调整`circles`参数的大小控制环形的宽度。


### 二、ggplot2 包实现饼图&#xA;

ggplot2 本身没有专门的饼图函数，但可通过极坐标转换（`coord_polar()`）将柱状图转换为饼图，步骤更灵活。




1.  **基础饼图**

    先绘制堆叠柱状图，再通过`coord_polar(theta = "y")`转换为极坐标（即饼图）。例如，使用`diamonds`数据集展示不同切割等级（`cut`）的占比：




```
library(ggplot2)
\# 统计频数并转换为数据框
cut\_counts <- as.data.frame(table(diamonds\$cut))
colnames(cut\_counts) <- c("cut", "count")  # 重命名列

\# 绘制饼图
ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +  # 绘制堆叠柱状图（宽度为1）
&#x20; coord\_polar(theta = "y") +  # 转换为极坐标（y轴为角度）
&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


\# 统计频数并转换为数据框
cut\_counts <- as.data.frame(table(diamonds\$cut))
colnames(cut\_counts) <- c("cut", "count")  # 重命名列

\# 绘制饼图
ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +  # 绘制堆叠柱状图（宽度为1）
&#x20; coord\_polar(theta = "y") +  # 转换为极坐标（y轴为角度）
&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


cut\_counts <- as.data.frame(table(diamonds\$cut))
colnames(cut\_counts) <- c("cut", "count")  # 重命名列

\# 绘制饼图
ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +  # 绘制堆叠柱状图（宽度为1）
&#x20; coord\_polar(theta = "y") +  # 转换为极坐标（y轴为角度）
&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


colnames(cut\_counts) <- c("cut", "count")  # 重命名列

\# 绘制饼图
ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +  # 绘制堆叠柱状图（宽度为1）
&#x20; coord\_polar(theta = "y") +  # 转换为极坐标（y轴为角度）
&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


\# 绘制饼图
ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +  # 绘制堆叠柱状图（宽度为1）
&#x20; coord\_polar(theta = "y") +  # 转换为极坐标（y轴为角度）
&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +  # 绘制堆叠柱状图（宽度为1）
&#x20; coord\_polar(theta = "y") +  # 转换为极坐标（y轴为角度）
&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; geom\_col(width = 1) +  # 绘制堆叠柱状图（宽度为1）
&#x20; coord\_polar(theta = "y") +  # 转换为极坐标（y轴为角度）
&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; coord\_polar(theta = "y") +  # 转换为极坐标（y轴为角度）
&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; labs(title = "钻石切割等级的占比分布", fill = "切割等级") +
&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; theme\_void() +  # 去除背景和坐标轴
&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中


&#x20; theme(plot.title = element\_text(hjust = 0.5))  # 标题居中
```

`geom_col(width = 1)`确保柱状图形成一个完整的 “柱”，极坐标转换后变为饼图，`theme_void()`去除冗余元素使图形更简洁。




1.  **带标签的饼图**

    添加百分比标签可增强可读性，需先计算占比并通过`geom_text()`实现：




```
\# 计算百分比
cut\_counts <- transform(cut\_counts,
&#x20;                      percent = paste0(round(count / sum(count) \* 100, 1), "%"))

ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +
&#x20; coord\_polar(theta = "y") +
&#x20; geom\_text(aes(label = percent),  # 显示百分比标签
&#x20;           position = position\_stack(vjust = 0.5)) +  # 标签位于扇形中心
&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


cut\_counts <- transform(cut\_counts,
&#x20;                      percent = paste0(round(count / sum(count) \* 100, 1), "%"))

ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +
&#x20; coord\_polar(theta = "y") +
&#x20; geom\_text(aes(label = percent),  # 显示百分比标签
&#x20;           position = position\_stack(vjust = 0.5)) +  # 标签位于扇形中心
&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20;                      percent = paste0(round(count / sum(count) \* 100, 1), "%"))

ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +
&#x20; coord\_polar(theta = "y") +
&#x20; geom\_text(aes(label = percent),  # 显示百分比标签
&#x20;           position = position\_stack(vjust = 0.5)) +  # 标签位于扇形中心
&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


ggplot(cut\_counts, aes(x = "", y = count, fill = cut)) +
&#x20; geom\_col(width = 1) +
&#x20; coord\_polar(theta = "y") +
&#x20; geom\_text(aes(label = percent),  # 显示百分比标签
&#x20;           position = position\_stack(vjust = 0.5)) +  # 标签位于扇形中心
&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; geom\_col(width = 1) +
&#x20; coord\_polar(theta = "y") +
&#x20; geom\_text(aes(label = percent),  # 显示百分比标签
&#x20;           position = position\_stack(vjust = 0.5)) +  # 标签位于扇形中心
&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; coord\_polar(theta = "y") +
&#x20; geom\_text(aes(label = percent),  # 显示百分比标签
&#x20;           position = position\_stack(vjust = 0.5)) +  # 标签位于扇形中心
&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; geom\_text(aes(label = percent),  # 显示百分比标签
&#x20;           position = position\_stack(vjust = 0.5)) +  # 标签位于扇形中心
&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20;           position = position\_stack(vjust = 0.5)) +  # 标签位于扇形中心
&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; labs(title = "切割等级占比（带百分比标签）", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; theme(plot.title = element\_text(hjust = 0.5))
```

`position_stack(vjust = 0.5)`确保标签居中对齐到每个扇形的中间位置。




1.  **环形图**

    通过设置`x`轴的偏移实现环形效果，即让柱状图的 “柱” 有内半径：




```
ggplot(cut\_counts, aes(x = 2, y = count, fill = cut)) +  # x=2 控制外半径
&#x20; geom\_col(width = 1) +  # 宽度1确保环形厚度
&#x20; coord\_polar(theta = "y") +
&#x20; xlim(0.5, 2.5) +  # x轴范围：0.5为内半径，2.5为外半径（控制环形宽度）
&#x20; geom\_text(aes(label = percent), position = position\_stack(vjust = 0.5)) +
&#x20; labs(title = "钻石切割等级环形图", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; geom\_col(width = 1) +  # 宽度1确保环形厚度
&#x20; coord\_polar(theta = "y") +
&#x20; xlim(0.5, 2.5) +  # x轴范围：0.5为内半径，2.5为外半径（控制环形宽度）
&#x20; geom\_text(aes(label = percent), position = position\_stack(vjust = 0.5)) +
&#x20; labs(title = "钻石切割等级环形图", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; coord\_polar(theta = "y") +
&#x20; xlim(0.5, 2.5) +  # x轴范围：0.5为内半径，2.5为外半径（控制环形宽度）
&#x20; geom\_text(aes(label = percent), position = position\_stack(vjust = 0.5)) +
&#x20; labs(title = "钻石切割等级环形图", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; xlim(0.5, 2.5) +  # x轴范围：0.5为内半径，2.5为外半径（控制环形宽度）
&#x20; geom\_text(aes(label = percent), position = position\_stack(vjust = 0.5)) +
&#x20; labs(title = "钻石切割等级环形图", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; geom\_text(aes(label = percent), position = position\_stack(vjust = 0.5)) +
&#x20; labs(title = "钻石切割等级环形图", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; labs(title = "钻石切割等级环形图", fill = "切割等级") +
&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; theme\_void() +
&#x20; theme(plot.title = element\_text(hjust = 0.5))


&#x20; theme(plot.title = element\_text(hjust = 0.5))
```

`x`的取值决定外半径，`xlim`的第一个值控制内半径，两者差值越大，环形越宽。


### 三、关键参数说明&#xA;



*   **数据格式**：基础绘图系统需输入数值向量（频数或占比）；ggplot2 需长格式数据框（包含类别和对应数值列）。


*   **颜色设置**：`col`（基础）或`fill`（ggplot2）指定颜色，可使用`rainbow()`、`brewer.pal()`（RColorBrewer 包）等函数生成配色。


*   **标签与图例**：基础系统通过`labels`参数直接添加扇区标签，或用`legend()`单独设置图例；ggplot2 中`fill`参数自动关联图例，`geom_text()`用于添加数值标签。


*   **环形图控制**：基础系统通过中心白色圆实现，ggplot2 通过`x`轴偏移和`xlim`控制内、外半径。


### 四、应用场景&#xA;

基础绘图系统的`pie()`函数适合快速绘制简单饼图，代码简洁，适合初步数据探索；ggplot2 的方法更灵活，便于自定义样式（如标签位置、颜色主题）和扩展功能（如添加注释、结合其他图层），适合制作 publication 级图表。需注意，当类别过多（超过 6-8 个）时，饼图可能显得拥挤，此时建议合并次要类别为 “其他”，或改用柱状图展示。


> （注：文档部分内容可能由 AI 生成）
>