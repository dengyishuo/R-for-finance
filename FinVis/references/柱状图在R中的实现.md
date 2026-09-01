# 柱状图在R中的实现

在 R 语言中，柱状图的实现同样可以通过基础绘图系统（`base`）和 ggplot2 包完成，两种方式各有侧重，以下是具体实现方法：


### 一、基础绘图系统（`base`）实现柱状图&#xA;



1.  **简单柱状图绘制**

    使用`barplot()`函数，直接输入向量或矩阵数据即可生成柱状图。例如，基于内置数据集`mtcars`统计不同气缸数（`cyl`）的车辆数量：




```
\# 统计各气缸数的频数
cyl\_counts <- table(mtcars\$cyl)

\# 绘制简单柱状图
barplot(cyl\_counts,
&#x20;       main = "不同气缸数的车辆数量",  # 标题
&#x20;       xlab = "气缸数",  # x轴标签
&#x20;       ylab = "车辆数量",  # y轴标签
&#x20;       col = "lightblue",  # 柱子颜色
&#x20;       border = "black")  # 柱子边框颜色


cyl\_counts <- table(mtcars\$cyl)

\# 绘制简单柱状图
barplot(cyl\_counts,
&#x20;       main = "不同气缸数的车辆数量",  # 标题
&#x20;       xlab = "气缸数",  # x轴标签
&#x20;       ylab = "车辆数量",  # y轴标签
&#x20;       col = "lightblue",  # 柱子颜色
&#x20;       border = "black")  # 柱子边框颜色


\# 绘制简单柱状图
barplot(cyl\_counts,
&#x20;       main = "不同气缸数的车辆数量",  # 标题
&#x20;       xlab = "气缸数",  # x轴标签
&#x20;       ylab = "车辆数量",  # y轴标签
&#x20;       col = "lightblue",  # 柱子颜色
&#x20;       border = "black")  # 柱子边框颜色


barplot(cyl\_counts,
&#x20;       main = "不同气缸数的车辆数量",  # 标题
&#x20;       xlab = "气缸数",  # x轴标签
&#x20;       ylab = "车辆数量",  # y轴标签
&#x20;       col = "lightblue",  # 柱子颜色
&#x20;       border = "black")  # 柱子边框颜色


&#x20;       main = "不同气缸数的车辆数量",  # 标题
&#x20;       xlab = "气缸数",  # x轴标签
&#x20;       ylab = "车辆数量",  # y轴标签
&#x20;       col = "lightblue",  # 柱子颜色
&#x20;       border = "black")  # 柱子边框颜色


&#x20;       xlab = "气缸数",  # x轴标签
&#x20;       ylab = "车辆数量",  # y轴标签
&#x20;       col = "lightblue",  # 柱子颜色
&#x20;       border = "black")  # 柱子边框颜色


&#x20;       ylab = "车辆数量",  # y轴标签
&#x20;       col = "lightblue",  # 柱子颜色
&#x20;       border = "black")  # 柱子边框颜色


&#x20;       col = "lightblue",  # 柱子颜色
&#x20;       border = "black")  # 柱子边框颜色


&#x20;       border = "black")  # 柱子边框颜色
```

`table()`函数用于统计分类变量的频数，`barplot()`通过输入频数向量绘制柱子，柱子高度对应频数大小。




1.  **分组柱状图**

    当需要对比多组数据在不同类别下的数值时，可将数据整理为矩阵，`barplot()`会自动按列分组。例如，对比不同气缸数车辆的平均马力（`hp`）和平均重量（`wt`）：




```
\# 按气缸数分组计算均值
hp\_wt\_mean <- aggregate(cbind(hp, wt) \~ cyl, data = mtcars, FUN = mean)
\# 转换为矩阵（行名为气缸数，列名为指标）
hp\_wt\_matrix <- as.matrix(hp\_wt\_mean\[, -1])
rownames(hp\_wt\_matrix) <- hp\_wt\_mean\$cyl

\# 绘制分组柱状图
barplot(hp\_wt\_matrix,
&#x20;       beside = TRUE,  # 分组并列显示（默认堆叠）
&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


hp\_wt\_mean <- aggregate(cbind(hp, wt) \~ cyl, data = mtcars, FUN = mean)
\# 转换为矩阵（行名为气缸数，列名为指标）
hp\_wt\_matrix <- as.matrix(hp\_wt\_mean\[, -1])
rownames(hp\_wt\_matrix) <- hp\_wt\_mean\$cyl

\# 绘制分组柱状图
barplot(hp\_wt\_matrix,
&#x20;       beside = TRUE,  # 分组并列显示（默认堆叠）
&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


\# 转换为矩阵（行名为气缸数，列名为指标）
hp\_wt\_matrix <- as.matrix(hp\_wt\_mean\[, -1])
rownames(hp\_wt\_matrix) <- hp\_wt\_mean\$cyl

\# 绘制分组柱状图
barplot(hp\_wt\_matrix,
&#x20;       beside = TRUE,  # 分组并列显示（默认堆叠）
&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


hp\_wt\_matrix <- as.matrix(hp\_wt\_mean\[, -1])
rownames(hp\_wt\_matrix) <- hp\_wt\_mean\$cyl

\# 绘制分组柱状图
barplot(hp\_wt\_matrix,
&#x20;       beside = TRUE,  # 分组并列显示（默认堆叠）
&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


rownames(hp\_wt\_matrix) <- hp\_wt\_mean\$cyl

\# 绘制分组柱状图
barplot(hp\_wt\_matrix,
&#x20;       beside = TRUE,  # 分组并列显示（默认堆叠）
&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


\# 绘制分组柱状图
barplot(hp\_wt\_matrix,
&#x20;       beside = TRUE,  # 分组并列显示（默认堆叠）
&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


barplot(hp\_wt\_matrix,
&#x20;       beside = TRUE,  # 分组并列显示（默认堆叠）
&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


&#x20;       beside = TRUE,  # 分组并列显示（默认堆叠）
&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


&#x20;       col = c("coral", "seagreen"),  # 不同组的颜色
&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


&#x20;       legend.text = c("平均马力", "平均重量"),  # 图例
&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


&#x20;       main = "不同气缸数车辆的性能指标对比",
&#x20;       xlab = "气缸数", ylab = "数值")


&#x20;       xlab = "气缸数", ylab = "数值")
```

`beside = TRUE`确保分组柱子并列排列，`legend.text`用于添加图例区分不同组别。




1.  **堆叠柱状图**

    若需展示 “整体与部分” 的关系，可使用默认的堆叠模式（`beside = FALSE`）。例如，按气缸数和变速箱类型（`am`，0 为自动，1 为手动）统计车辆数量：




```
\# 生成交叉表（行：气缸数，列：变速箱类型）
cyl\_am\_table <- table(mtcars\$cyl, mtcars\$am)

\# 绘制堆叠柱状图
barplot(cyl\_am\_table,
&#x20;       col = c("gold", "purple"),
&#x20;       legend.text = c("自动变速箱", "手动变速箱"),
&#x20;       main = "不同气缸数与变速箱类型的车辆分布",
&#x20;       xlab = "气缸数", ylab = "车辆数量")


cyl\_am\_table <- table(mtcars\$cyl, mtcars\$am)

\# 绘制堆叠柱状图
barplot(cyl\_am\_table,
&#x20;       col = c("gold", "purple"),
&#x20;       legend.text = c("自动变速箱", "手动变速箱"),
&#x20;       main = "不同气缸数与变速箱类型的车辆分布",
&#x20;       xlab = "气缸数", ylab = "车辆数量")


\# 绘制堆叠柱状图
barplot(cyl\_am\_table,
&#x20;       col = c("gold", "purple"),
&#x20;       legend.text = c("自动变速箱", "手动变速箱"),
&#x20;       main = "不同气缸数与变速箱类型的车辆分布",
&#x20;       xlab = "气缸数", ylab = "车辆数量")


barplot(cyl\_am\_table,
&#x20;       col = c("gold", "purple"),
&#x20;       legend.text = c("自动变速箱", "手动变速箱"),
&#x20;       main = "不同气缸数与变速箱类型的车辆分布",
&#x20;       xlab = "气缸数", ylab = "车辆数量")


&#x20;       col = c("gold", "purple"),
&#x20;       legend.text = c("自动变速箱", "手动变速箱"),
&#x20;       main = "不同气缸数与变速箱类型的车辆分布",
&#x20;       xlab = "气缸数", ylab = "车辆数量")


&#x20;       legend.text = c("自动变速箱", "手动变速箱"),
&#x20;       main = "不同气缸数与变速箱类型的车辆分布",
&#x20;       xlab = "气缸数", ylab = "车辆数量")


&#x20;       main = "不同气缸数与变速箱类型的车辆分布",
&#x20;       xlab = "气缸数", ylab = "车辆数量")


&#x20;       xlab = "气缸数", ylab = "车辆数量")
```

堆叠柱状图中，每个柱子的总高度为该类别的总和，不同颜色的分段代表各子组的占比。


### 二、ggplot2 包实现柱状图&#xA;



1.  **基础柱状图**

    使用`ggplot()`+`geom_col()`（或`geom_bar()`），`geom_col()`适用于已有数值数据，`geom_bar()`默认统计频数。例如，基于`diamonds`数据集绘制不同切割等级（`cut`）的钻石数量：




```
library(ggplot2)
ggplot(diamonds, aes(x = cut)) +
&#x20; geom\_bar(fill = "royalblue", color = "black") +  # 统计频数并绘图，设置填充色和边框
&#x20; labs(title = "不同切割等级的钻石数量", x = "切割等级", y = "数量")


ggplot(diamonds, aes(x = cut)) +
&#x20; geom\_bar(fill = "royalblue", color = "black") +  # 统计频数并绘图，设置填充色和边框
&#x20; labs(title = "不同切割等级的钻石数量", x = "切割等级", y = "数量")


&#x20; geom\_bar(fill = "royalblue", color = "black") +  # 统计频数并绘图，设置填充色和边框
&#x20; labs(title = "不同切割等级的钻石数量", x = "切割等级", y = "数量")


&#x20; labs(title = "不同切割等级的钻石数量", x = "切割等级", y = "数量")
```

`geom_bar(stat = "count")`是默认模式（统计频数），若已有汇总数据（如均值、总和），可用`geom_col(stat = "identity")`直接映射数值。




1.  **分组与堆叠柱状图**

    通过`fill`参数指定分组变量，结合`position`参数控制排列方式。例如，对比不同性别（`sex`）在各教育水平（`education`）下的收入均值（使用`gapminder`包的模拟数据）：




```
\# 模拟数据
set.seed(123)
edu\_data <- data.frame(
&#x20; education = rep(c("高中", "本科", "硕士", "博士"), 2),
&#x20; sex = rep(c("男", "女"), each = 4),
&#x20; income = c(3000, 5000, 7000, 9000, 2800, 4500, 6500, 8000)
)

\# 分组柱状图（并列）
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


set.seed(123)
edu\_data <- data.frame(
&#x20; education = rep(c("高中", "本科", "硕士", "博士"), 2),
&#x20; sex = rep(c("男", "女"), each = 4),
&#x20; income = c(3000, 5000, 7000, 9000, 2800, 4500, 6500, 8000)
)

\# 分组柱状图（并列）
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


edu\_data <- data.frame(
&#x20; education = rep(c("高中", "本科", "硕士", "博士"), 2),
&#x20; sex = rep(c("男", "女"), each = 4),
&#x20; income = c(3000, 5000, 7000, 9000, 2800, 4500, 6500, 8000)
)

\# 分组柱状图（并列）
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


&#x20; education = rep(c("高中", "本科", "硕士", "博士"), 2),
&#x20; sex = rep(c("男", "女"), each = 4),
&#x20; income = c(3000, 5000, 7000, 9000, 2800, 4500, 6500, 8000)
)

\# 分组柱状图（并列）
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


&#x20; sex = rep(c("男", "女"), each = 4),
&#x20; income = c(3000, 5000, 7000, 9000, 2800, 4500, 6500, 8000)
)

\# 分组柱状图（并列）
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


&#x20; income = c(3000, 5000, 7000, 9000, 2800, 4500, 6500, 8000)
)

\# 分组柱状图（并列）
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


)

\# 分组柱状图（并列）
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


\# 分组柱状图（并列）
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


&#x20; geom\_col(position = "dodge", width = 0.7) +  # 并列排列，宽度0.7
&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


&#x20; scale\_fill\_brewer(palette = "Pastel1") +  # 使用预定义色板
&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


&#x20; labs(title = "不同教育水平与性别的收入对比", x = "教育水平", y = "平均收入")

\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


\# 堆叠柱状图
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


&#x20; geom\_col(position = "stack") +  # 堆叠排列（默认）
&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")


&#x20; labs(title = "不同教育水平的收入构成（按性别）", x = "教育水平", y = "平均收入")
```

`position = "dodge"`实现分组并列，`position = "stack"`实现堆叠，`fill`参数用于区分组别并自动生成图例。




1.  **自定义样式与标签**

    可通过`theme()`调整外观，或添加数据标签（`geom_text()`）显示具体数值：




```
ggplot(edu\_data, aes(x = education, y = income, fill = sex)) +
&#x20; geom\_col(position = "dodge") +
&#x20; geom\_text(aes(label = income),  # 显示数值标签
&#x20;           position = position\_dodge(width = 0.7),  # 标签与柱子对齐
&#x20;           vjust = -0.3, size = 3.5) +  # 标签位置（上方）和大小
&#x20; ylim(0, 10000) +  # 调整y轴范围，避免标签超出
&#x20; theme\_minimal() +  # 简洁主题
&#x20; labs(title = "收入对比（带数值标签）", x = "教育水平", y = "平均收入")


&#x20; geom\_col(position = "dodge") +
&#x20; geom\_text(aes(label = income),  # 显示数值标签
&#x20;           position = position\_dodge(width = 0.7),  # 标签与柱子对齐
&#x20;           vjust = -0.3, size = 3.5) +  # 标签位置（上方）和大小
&#x20; ylim(0, 10000) +  # 调整y轴范围，避免标签超出
&#x20; theme\_minimal() +  # 简洁主题
&#x20; labs(title = "收入对比（带数值标签）", x = "教育水平", y = "平均收入")


&#x20; geom\_text(aes(label = income),  # 显示数值标签
&#x20;           position = position\_dodge(width = 0.7),  # 标签与柱子对齐
&#x20;           vjust = -0.3, size = 3.5) +  # 标签位置（上方）和大小
&#x20; ylim(0, 10000) +  # 调整y轴范围，避免标签超出
&#x20; theme\_minimal() +  # 简洁主题
&#x20; labs(title = "收入对比（带数值标签）", x = "教育水平", y = "平均收入")


&#x20;           position = position\_dodge(width = 0.7),  # 标签与柱子对齐
&#x20;           vjust = -0.3, size = 3.5) +  # 标签位置（上方）和大小
&#x20; ylim(0, 10000) +  # 调整y轴范围，避免标签超出
&#x20; theme\_minimal() +  # 简洁主题
&#x20; labs(title = "收入对比（带数值标签）", x = "教育水平", y = "平均收入")


&#x20;           vjust = -0.3, size = 3.5) +  # 标签位置（上方）和大小
&#x20; ylim(0, 10000) +  # 调整y轴范围，避免标签超出
&#x20; theme\_minimal() +  # 简洁主题
&#x20; labs(title = "收入对比（带数值标签）", x = "教育水平", y = "平均收入")


&#x20; ylim(0, 10000) +  # 调整y轴范围，避免标签超出
&#x20; theme\_minimal() +  # 简洁主题
&#x20; labs(title = "收入对比（带数值标签）", x = "教育水平", y = "平均收入")


&#x20; theme\_minimal() +  # 简洁主题
&#x20; labs(title = "收入对比（带数值标签）", x = "教育水平", y = "平均收入")


&#x20; labs(title = "收入对比（带数值标签）", x = "教育水平", y = "平均收入")
```

### 二、关键参数说明&#xA;



*   **数据格式**：基础绘图系统中，分组数据需为矩阵；ggplot2 中，长格式数据（每行一个观测，包含分组变量）更易处理。


*   **柱子排列**：`beside = TRUE`（基础）或`position = "dodge"`（ggplot2）用于并列分组，默认堆叠模式适合展示 “整体 - 部分” 关系。


*   **样式调整**：`col`（基础）或`fill`（ggplot2）控制颜色，`width`调整柱子宽度，`border`（基础）或`color`（ggplot2）设置边框。


### 三、应用场景&#xA;

基础绘图系统的`barplot()`适合快速生成简单柱状图或进行初步数据探索，代码简洁直接；ggplot2 的语法更灵活，在处理分组数据、自定义样式（如主题、标签）、添加统计信息（如误差线）时更高效，适合需要 publication 级图表或复杂对比分析的场景。实际使用中，可根据数据复杂度和美观需求选择工具，例如快速查看频数分布用基础系统，制作汇报用图表则优先考虑 ggplot2。


> （注：文档部分内容可能由 AI 生成）
>