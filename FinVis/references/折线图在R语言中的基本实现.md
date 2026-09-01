# 折线图在R语言中的基本实现

在 R 语言中，折线图的实现主要依赖基础绘图系统（`base`）和 ggplot2 包，两种方式各有特点，以下是基本实现方法：

### 一、基础绘图系统（`base`）实现折线图

1.  **简单折线图绘制**

    使用`plot()`函数，通过指定`type = "l"`参数绘制折线图。例
    如，基于内置数据集`airquality`绘制臭氧浓度随日期的变化：

```         
# 准备数据：提取月份为5的数据，添加日期列
data(airquality)
sub_data <- airquality[airquality$Month == 5, ]
sub_data$Date <- as.Date(paste(1973, sub_data$Month, sub_data$Day, sep = "-"))

# 绘制折线图
plot(sub_data$Date, sub_data$Ozone,
    type = "l",  # 指定类型为折线
    xlab = "日期",  # x轴标签
    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


data(airquality)
sub_data <- airquality[airquality$Month == 5, ]
sub_data$Date <- as.Date(paste(1973, sub_data$Month, sub_data$Day, sep = "-"))

# 绘制折线图
plot(sub_data$Date, sub_data$Ozone,
    type = "l",  # 指定类型为折线
    xlab = "日期",  # x轴标签
    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


sub_data <- airquality[airquality$Month == 5, ]
sub_data$Date <- as.Date(paste(1973, sub_data$Month, sub_data$Day, sep = "-"))

# 绘制折线图
plot(sub_data$Date, sub_data$Ozone,
    type = "l",  # 指定类型为折线
    xlab = "日期",  # x轴标签
    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


sub_data$Date <- as.Date(paste(1973, sub_data$Month, sub_data$Day, sep = "-"))

# 绘制折线图
plot(sub_data$Date, sub_data$Ozone,
    type = "l",  # 指定类型为折线
    xlab = "日期",  # x轴标签
    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


# 绘制折线图
plot(sub_data$Date, sub_data$Ozone,
    type = "l",  # 指定类型为折线
    xlab = "日期",  # x轴标签
    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


plot(sub_data$Date, sub_data$Ozone,
    type = "l",  # 指定类型为折线
    xlab = "日期",  # x轴标签
    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


    type = "l",  # 指定类型为折线
    xlab = "日期",  # x轴标签
    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


    xlab = "日期",  # x轴标签
    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


    ylab = "臭氧浓度",  # y轴标签
    main = "5月臭氧浓度变化趋势")  # 标题


    main = "5月臭氧浓度变化趋势")  # 标题
```

运行后将生成以日期为横轴、臭氧浓度为纵轴的折线图，`type = "l"`确保连接数据点形成线条。

1.  **添加数据点与细节**

    若需同时显示数据点，可先绘制点再连线，或使用`type = "b"`（点线结合）：

```         
plot(sub_data$Date, sub_data$Ozone,
    type = "b",  # 点线结合
    pch = 16,  # 点的形状（16为实心圆）
    col = "blue",  # 线条和点的颜色
    lwd = 2,  # 线条宽度
    cex = 1.2)  # 点的大小


    type = "b",  # 点线结合
    pch = 16,  # 点的形状（16为实心圆）
    col = "blue",  # 线条和点的颜色
    lwd = 2,  # 线条宽度
    cex = 1.2)  # 点的大小


    pch = 16,  # 点的形状（16为实心圆）
    col = "blue",  # 线条和点的颜色
    lwd = 2,  # 线条宽度
    cex = 1.2)  # 点的大小


    col = "blue",  # 线条和点的颜色
    lwd = 2,  # 线条宽度
    cex = 1.2)  # 点的大小


    lwd = 2,  # 线条宽度
    cex = 1.2)  # 点的大小


    cex = 1.2)  # 点的大小
```

### 二、ggplot2 包实现折线图

ggplot2 基于 “图层” 思想，代码更具可读性，适合复杂图形绘制，需先安装并加载包：`install.packages("ggplot2")`、`library(ggplot2)`。

1.  **基础折线图**

    使用`ggplot()`+`geom_line()`函数，以`economics`数据集（包含美国经济指标）为例，绘制失业率随时间的变化：

```         
data(economics)
ggplot(economics, aes(x = date, y = unemploy)) +  # 映射x轴（时间）和y轴（失业率）
 geom_line(color = "darkred") +  # 添加折线，指定颜色
 labs(title = "美国失业率变化趋势", x = "年份", y = "失业率（千人）")  # 添加标签


ggplot(economics, aes(x = date, y = unemploy)) +  # 映射x轴（时间）和y轴（失业率）
 geom_line(color = "darkred") +  # 添加折线，指定颜色
 labs(title = "美国失业率变化趋势", x = "年份", y = "失业率（千人）")  # 添加标签


 geom_line(color = "darkred") +  # 添加折线，指定颜色
 labs(title = "美国失业率变化趋势", x = "年份", y = "失业率（千人）")  # 添加标签


 labs(title = "美国失业率变化趋势", x = "年份", y = "失业率（千人）")  # 添加标签
```

`aes()`函数用于指定数据映射关系，`geom_line()`是绘制折线的核心图层。

1.  **多组数据折线图**

    当数据包含分组变量时，可通过`color`或`group`参数区分组别。例
    如，使用`ToothGrowth`数据集（不同剂量维生素 C 对牙齿生长的影响）：

```         
data(ToothGrowth)
# 将剂量转换为因子变量，便于分组
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

ggplot(ToothGrowth, aes(x = len, y = supp, color = dose, group = dose)) +
 geom_line(linetype = "dashed") +  # 线条类型为虚线
 labs(title = "不同剂量维生素C下的牙齿生长", x = "牙齿长度", y = "补充方式")


# 将剂量转换为因子变量，便于分组
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

ggplot(ToothGrowth, aes(x = len, y = supp, color = dose, group = dose)) +
 geom_line(linetype = "dashed") +  # 线条类型为虚线
 labs(title = "不同剂量维生素C下的牙齿生长", x = "牙齿长度", y = "补充方式")


ToothGrowth$dose <- as.factor(ToothGrowth$dose)

ggplot(ToothGrowth, aes(x = len, y = supp, color = dose, group = dose)) +
 geom_line(linetype = "dashed") +  # 线条类型为虚线
 labs(title = "不同剂量维生素C下的牙齿生长", x = "牙齿长度", y = "补充方式")


ggplot(ToothGrowth, aes(x = len, y = supp, color = dose, group = dose)) +
 geom_line(linetype = "dashed") +  # 线条类型为虚线
 labs(title = "不同剂量维生素C下的牙齿生长", x = "牙齿长度", y = "补充方式")


 geom_line(linetype = "dashed") +  # 线条类型为虚线
 labs(title = "不同剂量维生素C下的牙齿生长", x = "牙齿长度", y = "补充方式")


 labs(title = "不同剂量维生素C下的牙齿生长", x = "牙齿长度", y = "补充方式")
```

此处`color = dose`使不同剂量组的折线显示不同颜色，`group = dose`确保每组数据单独连线。

1.  **自定义样式与主题**

    通过`theme()`函数调整图表样式，例如设置灰色背景、旋转 x 轴标签：

```         
ggplot(economics, aes(x = date, y = unemploy)) +
 geom_line(color = "steelblue", size = 1) +
 labs(title = "美国失业率趋势", x = "年份", y = "人数") +
 theme_bw() +  # 使用白色背景主题
 theme(plot.title = element_text(hjust = 0.5),  # 标题居中
       axis.text.x = element_text(angle = 45, hjust = 1))  # x轴标签旋转45度


 geom_line(color = "steelblue", size = 1) +
 labs(title = "美国失业率趋势", x = "年份", y = "人数") +
 theme_bw() +  # 使用白色背景主题
 theme(plot.title = element_text(hjust = 0.5),  # 标题居中
       axis.text.x = element_text(angle = 45, hjust = 1))  # x轴标签旋转45度


 labs(title = "美国失业率趋势", x = "年份", y = "人数") +
 theme_bw() +  # 使用白色背景主题
 theme(plot.title = element_text(hjust = 0.5),  # 标题居中
       axis.text.x = element_text(angle = 45, hjust = 1))  # x轴标签旋转45度


 theme_bw() +  # 使用白色背景主题
 theme(plot.title = element_text(hjust = 0.5),  # 标题居中
       axis.text.x = element_text(angle = 45, hjust = 1))  # x轴标签旋转45度


 theme(plot.title = element_text(hjust = 0.5),  # 标题居中
       axis.text.x = element_text(angle = 45, hjust = 1))  # x轴标签旋转45度


       axis.text.x = element_text(angle = 45, hjust = 1))  # x轴标签旋转45度
```

### 二、关键参数说明

-   **数据映射**：`x`和`y`分别指定横轴和纵轴数据，分组变量通过`color`、`linetype`或`group`设置。

-   **线条属性**：`size`（线条粗细）、`linetype`（线条类型，如 1 为实线、2 为虚线）、`color`（颜色）。

-   **点属性**：若结合`geom_point()`，可通过`pch`（点形状）、`cex`（点大小）调整。

### 三、应用场景

基础绘图系统适合快速绘制简单折线图，代码简洁；ggplot2 更适合复杂可视化（如多组对比、自定义主题），语法更系统。实
际应用中，可根据数据复杂度和美观需求选择工具，例如时间序列分析常用`base`包快速查看趋势，而需要 publication 级图表时优先使用 ggplot2。
