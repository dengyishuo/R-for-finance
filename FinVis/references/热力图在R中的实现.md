# 热力图在R中的实现

热力图通过颜色梯度展示数据矩阵中数值的大小关系，在 R 语言中可通过`pheatmap`包和 ggplot2 包实现，以下是具体方法：


### 一、`pheatmap`包实现热力图（高效方法

`pheatmap`包专为热力图设计，支持聚类分析和注释功能，是科研和数据分析中常用的工具。需先安装并加载包：`install.packages("pheatmap")`、`library(pheatmap)`。




1.  **基础热力图绘制**

    使用`pheatmap()`函数，输入矩阵数据即可生成热力图。例如，基于`mtcars`数据集的相关性矩阵绘制热力图：




```
library(pheatmap)
# 计算变量相关性矩阵
mtcars_cor <- cor(mtcars)

# 绘制基础热力图
pheatmap(mtcars_cor,
        main = "mtcars数据集变量相关性热力图",  # 标题
        treeheight_row = 15,  # 行聚类树高度
        treeheight_col = 15)  # 列聚类树高度
```

`cor()`函数计算变量间的相关系数（矩阵形式），`pheatmap()`默认对行和列进行聚类（通过树状图展示），颜色从蓝到红表示相关性从负到正，直观呈现变量间的关联强度。




1.  **带注释的分组热力图**

当数据包含分组信息时，可通过`annotation_row`或`annotation_col`参数添加行 / 列注释，区分不同组别。例如，对`mtcars`数据按气缸数（`cyl`）分组并添加注释：




```
# 准备分组注释数据（数据框形式）
annotation <- data.frame(cyl = factor(mtcars$cyl))
rownames(annotation) <- rownames(mtcars)  # 注释行名与数据行名一致

# 绘制带注释的热力图
pheatmap(mtcars,  # 直接使用原始数据（非相关性矩阵）
        annotation_row = annotation,  # 行注释（按cyl分组）
        scale = "row",  # 按行标准化（消除量纲影响）
        show_rownames = FALSE,  # 隐藏行名（避免拥挤）
        main = "汽车特征热力图（按气缸数分组）")
```

`scale = "row"`对每行数据进行标准化（适合不同量纲的变量），注释条通过颜色区分气缸数组别，结合聚类树可观察同组样本的特征相似性。


#### ggplot2 包实现热力图（灵活方法

ggplot2 通过`geom_tile()`函数绘制热力图，需先将矩阵数据转换为长格式（适合处理非聚类或自定义分组的数据）。




1.  **基础热力图**

使用`ggplot()`+`geom_tile()`函数，输入长格式数据（包含行名、列名、数值三列）。例如，基于`volcano`数据集（内置火山高度矩阵）绘制热力图：




```{R}
library(ggplot2)
library(reshape2)  # 用于数据格式转换

# 将矩阵转换为长格式（行、列、值）
volcano_melt <- melt(volcano)
colnames(volcano_melt) <- c("x", "y", "height")  # 重命名列

# 绘制基础热力图
ggplot(volcano_melt, aes(x = x, y = y, fill = height)) +
 geom_tile() +  # 核心函数，绘制方块
 scale_fill_gradient(low = "green", high = "red") +  # 颜色梯度（低-高）
 labs(title = "火山高度热力图", x = "x坐标", y = "y坐标", fill = "高度") +
 theme_bw() +
 theme(axis.text = element_text(size = 6))  # 缩小坐标轴文本（避免拥挤）
```

`melt()`函数将矩阵转换为长格式（每行一个坐标点的高度值），`scale_fill_gradient()`定义颜色梯度，适合展示空间分布类数据（如地形、温度分布）。




1.  **相关性热力图与自定义标注**

对相关性矩阵进行可视化，并添加数值标签增强解读。例如，展示`iris`数据集变量的相关性热力图：




```
# 计算相关性矩阵并转换为长格式
iris_cor <- cor(iris[, 1:4])  # 取前4个数值变量
iris_melt <- melt(iris_cor)

# 绘制带数值标签的相关性热力图
ggplot(iris_melt, aes(x = Var1, y = Var2, fill = value)) +
 geom_tile(color = "white") +  # 方块边框为白色
 geom_text(aes(label = round(value, 2)), size = 3) +  # 添加数值标签（保留2位小数）
 scale_fill_gradient2(low = "blue", mid = "white", high = "red",  # 正负对称颜色
                     midpoint = 0) +  # 中点为0（适合相关性数据）
 labs(title = "鸢尾花变量相关性热力图", x = "", y = "", fill = "相关系数") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x轴标签旋转
```

`scale_fill_gradient2()`适合正负值对称的数据（如相关性系数），数值标签直接展示相关系数大小，避免仅通过颜色判断的误差。




1.  **时间序列热力图**

    对时间序列数据按 “时间 - 类别” 矩阵绘制热力图，展示随时间变化的分布。例如，使用`economics`数据集的月度数据（简化为年度 - 月度矩阵）：




```
# 处理数据：提取年份和月份，计算年度-月度的失业率均值
economics$year <- format(economics$date, "%Y")
economics$month <- format(economics$date, "%m")
econ_heat <- aggregate(unemploy ~ year + month, economics, mean)

# 绘制时间序列热力图
ggplot(econ_heat, aes(x = month, y = year, fill = unemploy)) +
 geom_tile() +
 scale_fill_viridis_c(option = "plasma") +  # 使用viridis色板（色盲友好）
 labs(title = "美国失业率年度-月度热力图", x = "月份", y = "年份", fill = "失业率（千人）") +
 theme_dark() +
 theme(plot.title = element_text(hjust = 0.5))
```

通过年份和月份的交叉矩阵，可直观观察失业率的季节性波动（如某些月份失业率常年偏高）和长期趋势（如某年份整体颜色偏深）。


**关键参数与场景选择**



*   **数据格式**：`pheatmap`直接支持矩阵输入，适合相关性分析和聚类展示；ggplot2 需长格式数据（`melt()`转换），适合自定义分组、时间序列等非聚类场景。


*   **聚类功能**：`pheatmap`默认进行行 / 列聚类（可通过`cluster_rows = FALSE`关闭），适合探索数据内在结构；ggplot2 需手动计算聚类结果后排序，灵活性低但可控性强。


*   **颜色选择**：相关性数据常用`scale_fill_gradient2()`（正负对称），单变量数据常用`scale_fill_gradient()`（从低到高），`viridis`色系（`scale_fill_viridis_c()`）适合色盲友好的可视化。


*   **场景适配**：`pheatmap`适合快速生成带聚类和注释的热力图（如基因表达、变量相关性分析）；ggplot2 适合需要高度自定义（如标签、主题、坐标变换）的场景，输出更美观。


热力图在基因芯片分析、金融风险矩阵、环境监测等领域应用广泛，通过 R 语言的上述方法，可灵活呈现数据矩阵的分布规律，辅助发现数据中的热点区域和隐藏模式。


> （注：文档部分内容可能由 AI 生成）
>
