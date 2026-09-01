# 雷达图在R中的实现

雷达图是展示多维度数据的有效工具，能直观呈现多个指标在不同对象上的分布与差异。在 R 语言中，可通过`fmsb`包和 ggplot2 包实现，以下是具体方法：


### 一、`fmsb`包实现雷达图（经典方法）

`fmsb`包专为雷达图设计，语法简洁，适合快速绘制基础雷达图。需先安装并加载包：`install.packages("fmsb")`、`library(fmsb)`。




1.  **简单雷达图绘制**

    使用`radarchart()`函数，输入矩阵数据（每行代表一个对象，每列代表一个维度），且需确保数据包含 “最大值” 和 “最小值” 行（用于界定坐标轴范围）。例如，基于虚构的学生成绩数据绘制雷达图：




```
library(fmsb)
# 准备数据（数学、语文、英语、物理、化学，包含最大值和最小值行）
scores <- matrix(
 c(100, 0, 85, 70,  # 最大值、最小值、学生A
   100, 0, 75, 90,  # 学生B
   100, 0, 90, 65), # 学生C
 nrow = 5, byrow = TRUE,
 dimnames = list(c("max", "min", "学生A", "学生B", "学生C"),
                c("数学", "语文", "英语", "物理"))
)

# 绘制基础雷达图
radarchart(scores,
          title = "学生成绩雷达图",  # 标题
          pcol = c("red", "blue", "green"),  # 每条线的颜色
          plwd = 2,  # 线宽
          pfcol = adjustcolor(c("red", "blue", "green"), alpha = 0.2),  # 填充色（透明）
          cglcol = "gray",  # 网格线颜色
          cglty = 1)  # 网格线类型（实线）
# 添加图例
legend("bottomright", legend = c("学生A", "学生B", "学生C"),
      col = c("red", "blue", "green"), lwd = 2)
```

数据矩阵需以 “最大值行” 和 “最小值行” 开头，界定各维度的范围；`pcol`和`pfcol`分别控制线条颜色和填充色，网格线使各维度的数值对比更清晰。




1.  **多组数据对比与参数调整**

    调整线条样式、填充透明度和网格密度，增强多组数据的区分度。例如，对比不同品牌汽车的性能指标：




```
# 提取mtcars数据的部分指标（按品牌分组，简化处理）
cars_data <- mtcars[, c("mpg", "hp", "wt", "qsec")]
# 取前3个品牌（行名）并添加最大/最小值行
cars_radar <- rbind(
 apply(cars_data, 2, max),  # 最大值行
 apply(cars_data, 2, min),  # 最小值行
 cars_data[1:3, ]  # 前3辆车的数据
)
rownames(cars_radar)[3:5] <- rownames(cars_data)[1:3]  # 重命名行

# 绘制优化后的雷达图
radarchart(cars_radar,
          pcol = c("darkred", "darkblue", "darkgreen"),
          plty = c(1, 2, 3),  # 线条类型（实线、虚线、点线）
          plwd = 2,
          pfcol = adjustcolor(c("darkred", "darkblue", "darkgreen"), alpha = 0.1),
          cglwd = 1,  # 网格线宽
          cglty = 2,  # 网格线类型（虚线）
          axistype = 1,  # 坐标轴显示方式（带刻度）
          title = "不同品牌汽车性能雷达图")
legend("bottomleft", legend = rownames(cars_radar)[3:5],
      col = c("darkred", "darkblue", "darkgreen"), lty = c(1, 2, 3), lwd = 2)
```

通过`plty`参数设置不同线条类型，结合颜色差异，使多组数据在雷达图上的区分度更高；`axistype = 1`显示刻度值，便于读取具体数值。


#### ggplot2 包实现雷达图（灵活方法）

ggplot2 需通过极坐标转换（`coord_polar()`）实现雷达图，需先将数据转换为长格式，步骤稍复杂但样式更灵活。




1.  **基础雷达图**

    使用`ggplot()`+`geom_polygon()`+`geom_path()`函数，输入长格式数据（包含对象、维度、数值三列）。例如，基于`iris`数据集的前 4 个指标绘制雷达图（按品种分组）：




```
library(ggplot2)
library(dplyr)  # 用于数据处理
library(tidyr)  # 用于数据转换

# 数据处理：取每个品种的均值，转换为长格式
iris_mean <- iris %>%
 group_by(Species) %>%
 summarise(across(Sepal.Length:Petal.Width, mean)) %>%
 pivot_longer(cols = -Species, names_to = "metric", values_to = "value")

# 添加每个指标的最大值（用于闭合多边形）
max_values <- iris_mean %>%
 group_by(metric) %>%
 summarise(value = max(value), Species = "max")
iris_radar <- bind_rows(iris_mean, max_values)

# 绘制基础雷达图
ggplot(iris_radar, aes(x = metric, y = value, group = Species, color = Species)) +
 geom_polygon(fill = NA, linewidth = 1) +  # 绘制多边形（无填充）
 geom_path(linewidth = 1) +  # 绘制连接线
 coord_polar() +  # 转换为极坐标（核心步骤）
 scale_y_continuous(limits = c(0, max(iris_radar$value) * 1.1)) +  # 调整y轴范围
 labs(title = "不同品种鸢尾花的指标均值雷达图", x = "", y = "") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 0, hjust = 1))  # 调整x轴标签角度
```

`coord_polar()`将笛卡尔坐标转换为极坐标，实现雷达图效果；`geom_polygon()`绘制多边形轮廓，`geom_path()`确保线条连续，适合展示多维度的均值对比。




1.  **带填充与标签的雷达图**

    添加填充色和数值标签，增强可读性。例如，优化学生成绩雷达图：




```
# 准备学生成绩数据（长格式）
students <- data.frame(
 student = rep(c("学生A", "学生B", "学生C"), each = 4),
 subject = rep(c("数学", "语文", "英语", "物理"), 3),
 score = c(85, 70, 90, 75, 75, 90, 65, 80, 90, 65, 85, 70)
)

# 绘制带填充和标签的雷达图
ggplot(students, aes(x = subject, y = score, group = student, color = student, fill = student)) +
 geom_polygon(alpha = 0.3, linewidth = 1) +  # 半透明填充
 geom_text(aes(label = score), position = position_dodge(0.1), size = 3) +  # 数值标签
 coord_polar() +
 scale_y_continuous(limits = c(0, 100)) +  # 成绩范围0-100
 labs(title = "学生成绩雷达图（带分数标签）", x = "", y = "分数") +
 theme_light() +
 theme(legend.position = "bottom")  # 图例放在底部
```

填充色（`alpha`控制透明度）使不同对象的区域更易区分，数值标签直接展示各维度的具体数值，避免仅通过图形判断的误差。




1.  **自定义坐标轴与网格线**

    调整网格线样式和坐标轴刻度，使雷达图更清晰。例如，优化汽车性能雷达图：




```
# 使用mtcars数据（前3辆车，4个指标）
cars_long <- mtcars[1:3, c("mpg", "hp", "wt", "qsec")] %>%
 mutate(car = rownames(.)) %>%
 pivot_longer(cols = -car, names_to = "metric", values_to = "value")

# 标准化数值（消除量纲影响）
cars_long <- cars_long %>%
 group_by(metric) %>%
 mutate(value = scales::rescale(value, to = c(0, 100)))  # 标准化到0-100

# 绘制自定义雷达图
ggplot(cars_long, aes(x = metric, y = value, group = car, color = car, fill = car)) +
 geom_polygon(alpha = 0.2, linewidth = 1.2) +
 geom_point(size = 3) +  # 添加数据点
 coord_polar() +
 scale_y_continuous(breaks = seq(0, 100, 20)) +  # 设置y轴刻度
 labs(title = "汽车性能标准化雷达图（0-100）", x = "", y = "标准化值") +
 theme(
   panel.grid.major = element_line(color = "gray80", linetype = "dashed"),  # 主网格线
   panel.grid.minor = element_blank()  # 隐藏次网格线
 )
```

标准化数值（`scales::rescale()`）消除不同指标的量纲差异（如马力和重量的单位不同），使多维度对比更合理；自定义网格线增强可读性，适合跨指标的综合评估。


**关键参数与场景选择**



*   **数据格式**：`fmsb`包需矩阵格式（含最大 / 最小值行），适合快速绘制；ggplot2 需长格式数据，适合复杂样式调整和分组展示。


*   **样式控制**：`fmsb`通过`pcol`、`plty`等参数控制线条，ggplot2 通过`geom_polygon()`和`geom_point()`自定义填充、点形状等，样式更丰富。


*   **维度数量**：雷达图适合 5-8 个维度，过多维度会导致图形拥挤（可合并次要维度），过少则失去多维度对比意义。


*   **场景适配**：`fmsb`适合科研报告或快速分析，ggplot2 适合制作汇报级图表（如企业竞争力评估、学生综合素质对比）。


雷达图在多维度评估场景（如产品测评、绩效评估、竞品分析）中应用广泛，通过 R 语言的上述方法，可灵活呈现不同对象在多个指标上的优劣势，辅助综合决策。
