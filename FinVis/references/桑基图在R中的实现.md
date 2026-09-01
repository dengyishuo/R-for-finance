# 桑基图在R中的实现

桑基图（Sankey Diagram）是展示数据流动与分配关系的可视化工具，通过节点和流量连线直观呈现不同类别间的数值传递。在 R 语言中，可通过`networkD3`包（交互式）和`ggplot2`+`ggalluvial`包（静态）实现，以下是具体方法：


### 一、`networkD3`包实现交互式桑基图&#xA;

`networkD3`包生成的桑基图支持鼠标悬停查看数值、缩放和平移，适合网页展示或交互式分析。需先安装并加载包：`install.packages("networkD3")`、`library(networkD3)`。




1.  **数据格式要求**

    桑基图需要 “边数据框”（包含源节点、目标节点、流量值），节点需用数字索引（从 0 开始）而非名称。例如，模拟产品销售渠道的流量数据：




```
\# 边数据框（source: 源节点索引, target: 目标节点索引, value: 流量）
links <- data.frame(
&#x20; source = c(0, 0, 1, 1, 2),  # 源节点：0=工厂A, 1=工厂B, 2=工厂C
&#x20; target = c(3, 4, 3, 5, 4),  # 目标节点：3=华东, 4=华南, 5=华北
&#x20; value = c(10, 20, 15, 25, 30)
)

\# 节点名称（顺序对应索引0,1,2,3,4,5）
nodes <- data.frame(name = c("工厂A", "工厂B", "工厂C", "华东", "华南", "华北"))


links <- data.frame(
&#x20; source = c(0, 0, 1, 1, 2),  # 源节点：0=工厂A, 1=工厂B, 2=工厂C
&#x20; target = c(3, 4, 3, 5, 4),  # 目标节点：3=华东, 4=华南, 5=华北
&#x20; value = c(10, 20, 15, 25, 30)
)

\# 节点名称（顺序对应索引0,1,2,3,4,5）
nodes <- data.frame(name = c("工厂A", "工厂B", "工厂C", "华东", "华南", "华北"))


&#x20; source = c(0, 0, 1, 1, 2),  # 源节点：0=工厂A, 1=工厂B, 2=工厂C
&#x20; target = c(3, 4, 3, 5, 4),  # 目标节点：3=华东, 4=华南, 5=华北
&#x20; value = c(10, 20, 15, 25, 30)
)

\# 节点名称（顺序对应索引0,1,2,3,4,5）
nodes <- data.frame(name = c("工厂A", "工厂B", "工厂C", "华东", "华南", "华北"))


&#x20; target = c(3, 4, 3, 5, 4),  # 目标节点：3=华东, 4=华南, 5=华北
&#x20; value = c(10, 20, 15, 25, 30)
)

\# 节点名称（顺序对应索引0,1,2,3,4,5）
nodes <- data.frame(name = c("工厂A", "工厂B", "工厂C", "华东", "华南", "华北"))


&#x20; value = c(10, 20, 15, 25, 30)
)

\# 节点名称（顺序对应索引0,1,2,3,4,5）
nodes <- data.frame(name = c("工厂A", "工厂B", "工厂C", "华东", "华南", "华北"))


)

\# 节点名称（顺序对应索引0,1,2,3,4,5）
nodes <- data.frame(name = c("工厂A", "工厂B", "工厂C", "华东", "华南", "华北"))


\# 节点名称（顺序对应索引0,1,2,3,4,5）
nodes <- data.frame(name = c("工厂A", "工厂B", "工厂C", "华东", "华南", "华北"))


nodes <- data.frame(name = c("工厂A", "工厂B", "工厂C", "华东", "华南", "华北"))
```



1.  **基础交互式桑基图**

    使用`sankeyNetwork()`函数绘制，指定边数据、节点数据、节点名称列及流量列：




```
sankeyNetwork(
&#x20; Links = links,  # 边数据
&#x20; Nodes = nodes,  # 节点数据
&#x20; Source = "source",  # 源节点列名
&#x20; Target = "target",  # 目标节点列名
&#x20; Value = "value",  # 流量列名
&#x20; NodeID = "name",  # 节点名称列名
&#x20; fontSize = 12,  # 节点标签字体大小
&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; Links = links,  # 边数据
&#x20; Nodes = nodes,  # 节点数据
&#x20; Source = "source",  # 源节点列名
&#x20; Target = "target",  # 目标节点列名
&#x20; Value = "value",  # 流量列名
&#x20; NodeID = "name",  # 节点名称列名
&#x20; fontSize = 12,  # 节点标签字体大小
&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; Nodes = nodes,  # 节点数据
&#x20; Source = "source",  # 源节点列名
&#x20; Target = "target",  # 目标节点列名
&#x20; Value = "value",  # 流量列名
&#x20; NodeID = "name",  # 节点名称列名
&#x20; fontSize = 12,  # 节点标签字体大小
&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; Source = "source",  # 源节点列名
&#x20; Target = "target",  # 目标节点列名
&#x20; Value = "value",  # 流量列名
&#x20; NodeID = "name",  # 节点名称列名
&#x20; fontSize = 12,  # 节点标签字体大小
&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; Target = "target",  # 目标节点列名
&#x20; Value = "value",  # 流量列名
&#x20; NodeID = "name",  # 节点名称列名
&#x20; fontSize = 12,  # 节点标签字体大小
&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; Value = "value",  # 流量列名
&#x20; NodeID = "name",  # 节点名称列名
&#x20; fontSize = 12,  # 节点标签字体大小
&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; NodeID = "name",  # 节点名称列名
&#x20; fontSize = 12,  # 节点标签字体大小
&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; fontSize = 12,  # 节点标签字体大小
&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; nodeWidth = 30,  # 节点宽度
&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


&#x20; colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")  # D3.js颜色方案
)


)
```

生成的交互式桑基图中，节点宽度与流经的总流量成正比，连线宽度对应具体流量值，鼠标悬停时显示详细数值，支持拖拽节点调整位置。




1.  **自定义颜色与样式**

    通过`colourScale`参数指定节点颜色，或通过`LinkGroup`/`NodeGroup`按分组着色：




```
\# 为边添加分组（用于着色）
links\$group <- c(1, 1, 2, 2, 3)

sankeyNetwork(
&#x20; Links = links, Nodes = nodes,
&#x20; Source = "source", Target = "target", Value = "value",
&#x20; NodeID = "name",
&#x20; LinkGroup = "group",  # 按边分组着色
&#x20; NodeGroup = factor(nodes\$name),  # 按节点名称分组着色
&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


links\$group <- c(1, 1, 2, 2, 3)

sankeyNetwork(
&#x20; Links = links, Nodes = nodes,
&#x20; Source = "source", Target = "target", Value = "value",
&#x20; NodeID = "name",
&#x20; LinkGroup = "group",  # 按边分组着色
&#x20; NodeGroup = factor(nodes\$name),  # 按节点名称分组着色
&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


sankeyNetwork(
&#x20; Links = links, Nodes = nodes,
&#x20; Source = "source", Target = "target", Value = "value",
&#x20; NodeID = "name",
&#x20; LinkGroup = "group",  # 按边分组着色
&#x20; NodeGroup = factor(nodes\$name),  # 按节点名称分组着色
&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; Links = links, Nodes = nodes,
&#x20; Source = "source", Target = "target", Value = "value",
&#x20; NodeID = "name",
&#x20; LinkGroup = "group",  # 按边分组着色
&#x20; NodeGroup = factor(nodes\$name),  # 按节点名称分组着色
&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; Source = "source", Target = "target", Value = "value",
&#x20; NodeID = "name",
&#x20; LinkGroup = "group",  # 按边分组着色
&#x20; NodeGroup = factor(nodes\$name),  # 按节点名称分组着色
&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; NodeID = "name",
&#x20; LinkGroup = "group",  # 按边分组着色
&#x20; NodeGroup = factor(nodes\$name),  # 按节点名称分组着色
&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; LinkGroup = "group",  # 按边分组着色
&#x20; NodeGroup = factor(nodes\$name),  # 按节点名称分组着色
&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; NodeGroup = factor(nodes\$name),  # 按节点名称分组着色
&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; colourScale = JS("d3.scaleOrdinal(\['#ff6b6b', '#4ecdc4', '#45b7d1']);"),  # 自定义颜色
&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; nodeWidth = 20,
&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; fontSize = 10,
&#x20; title = "产品销售渠道流量分布"
)


&#x20; title = "产品销售渠道流量分布"
)


)
```

自定义颜色需使用 D3.js 的颜色语法（`JS()`函数包裹），适合突出特定节点或流量路径。


### 二、`ggalluvial`包实现静态桑基图（基于 ggplot2）&#xA;

`ggalluvial`包将桑基图视为 “冲积图” 的一种形式，基于 ggplot2 语法，适合生成静态、可出版级别的图表。需先安装并加载包：`install.packages("ggalluvial")`、`library(ggalluvial)`。




1.  **长格式数据绘制基础桑基图**

    数据需为长格式（每行代表一个流量路径，包含源、目标和数值）。例如，使用`Titanic`数据集（乘客存活情况的交叉表）：




```
library(ggplot2)
library(ggalluvial)

\# 将Titanic数据转换为长格式（数据框）
titanic\_df <- as.data.frame(Titanic)

\# 绘制桑基图（展示乘客等级-性别-存活状态的流量）
ggplot(titanic\_df,
&#x20;      aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Class), width = 1/12) +  # 流量连线（冲积层）
&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


library(ggalluvial)

\# 将Titanic数据转换为长格式（数据框）
titanic\_df <- as.data.frame(Titanic)

\# 绘制桑基图（展示乘客等级-性别-存活状态的流量）
ggplot(titanic\_df,
&#x20;      aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Class), width = 1/12) +  # 流量连线（冲积层）
&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


\# 将Titanic数据转换为长格式（数据框）
titanic\_df <- as.data.frame(Titanic)

\# 绘制桑基图（展示乘客等级-性别-存活状态的流量）
ggplot(titanic\_df,
&#x20;      aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Class), width = 1/12) +  # 流量连线（冲积层）
&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


titanic\_df <- as.data.frame(Titanic)

\# 绘制桑基图（展示乘客等级-性别-存活状态的流量）
ggplot(titanic\_df,
&#x20;      aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Class), width = 1/12) +  # 流量连线（冲积层）
&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


\# 绘制桑基图（展示乘客等级-性别-存活状态的流量）
ggplot(titanic\_df,
&#x20;      aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Class), width = 1/12) +  # 流量连线（冲积层）
&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


ggplot(titanic\_df,
&#x20;      aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Class), width = 1/12) +  # 流量连线（冲积层）
&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


&#x20;      aes(y = Freq, axis1 = Class, axis2 = Sex, axis3 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Class), width = 1/12) +  # 流量连线（冲积层）
&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


&#x20; geom\_alluvium(aes(fill = Class), width = 1/12) +  # 流量连线（冲积层）
&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


&#x20; geom\_stratum(width = 1/12, fill = "white", color = "black") +  # 节点（ strata ）
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +  # 节点标签
&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


&#x20; scale\_x\_discrete(limits = c("乘客等级", "性别", "存活状态"), expand = c(0.05, 0.05)) +
&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


&#x20; scale\_fill\_brewer(type = "qual", palette = "Set1") +
&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


&#x20; labs(title = "泰坦尼克号乘客存活情况桑基图", y = "人数") +
&#x20; theme\_minimal()


&#x20; theme\_minimal()
```

`geom_alluvium()`绘制流量连线（冲积层），`geom_stratum()`绘制节点（矩形），`axis1-axis3`指定流动的层级（源→中间→目标），适合多阶段流量展示。




1.  **调整连线样式与节点位置**

    通过`width`参数控制节点和连线宽度，`gap.width`调整层级间距，优化图表可读性：




```
ggplot(titanic\_df,
&#x20;      aes(y = Freq, axis1 = Class, axis2 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Survived), width = 1/8, alpha = 0.8) +  # 连线宽度与透明度
&#x20; geom\_stratum(width = 1/8, color = "gray50") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum)), size = 3) +
&#x20; scale\_x\_discrete(limits = c("乘客等级", "存活状态"), expand = c(0.1, 0.1)) +
&#x20; scale\_fill\_manual(values = c("darkred", "darkgreen")) +
&#x20; labs(title = "泰坦尼克号乘客等级与存活情况的关系") +
&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20;      aes(y = Freq, axis1 = Class, axis2 = Survived)) +
&#x20; geom\_alluvium(aes(fill = Survived), width = 1/8, alpha = 0.8) +  # 连线宽度与透明度
&#x20; geom\_stratum(width = 1/8, color = "gray50") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum)), size = 3) +
&#x20; scale\_x\_discrete(limits = c("乘客等级", "存活状态"), expand = c(0.1, 0.1)) +
&#x20; scale\_fill\_manual(values = c("darkred", "darkgreen")) +
&#x20; labs(title = "泰坦尼克号乘客等级与存活情况的关系") +
&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20; geom\_alluvium(aes(fill = Survived), width = 1/8, alpha = 0.8) +  # 连线宽度与透明度
&#x20; geom\_stratum(width = 1/8, color = "gray50") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum)), size = 3) +
&#x20; scale\_x\_discrete(limits = c("乘客等级", "存活状态"), expand = c(0.1, 0.1)) +
&#x20; scale\_fill\_manual(values = c("darkred", "darkgreen")) +
&#x20; labs(title = "泰坦尼克号乘客等级与存活情况的关系") +
&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20; geom\_stratum(width = 1/8, color = "gray50") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum)), size = 3) +
&#x20; scale\_x\_discrete(limits = c("乘客等级", "存活状态"), expand = c(0.1, 0.1)) +
&#x20; scale\_fill\_manual(values = c("darkred", "darkgreen")) +
&#x20; labs(title = "泰坦尼克号乘客等级与存活情况的关系") +
&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum)), size = 3) +
&#x20; scale\_x\_discrete(limits = c("乘客等级", "存活状态"), expand = c(0.1, 0.1)) +
&#x20; scale\_fill\_manual(values = c("darkred", "darkgreen")) +
&#x20; labs(title = "泰坦尼克号乘客等级与存活情况的关系") +
&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20; scale\_x\_discrete(limits = c("乘客等级", "存活状态"), expand = c(0.1, 0.1)) +
&#x20; scale\_fill\_manual(values = c("darkred", "darkgreen")) +
&#x20; labs(title = "泰坦尼克号乘客等级与存活情况的关系") +
&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20; scale\_fill\_manual(values = c("darkred", "darkgreen")) +
&#x20; labs(title = "泰坦尼克号乘客等级与存活情况的关系") +
&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20; labs(title = "泰坦尼克号乘客等级与存活情况的关系") +
&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20; theme(legend.position = "bottom") +
&#x20; guides(fill = guide\_legend(title = "存活状态"))


&#x20; guides(fill = guide\_legend(title = "存活状态"))
```

`alpha = 0.8`降低连线透明度，避免重叠时过于拥挤；`width`参数控制节点和连线的宽度比例，使整体布局更协调。




1.  **自定义流量颜色与主题**

    根据流量目标或数值范围着色，结合 ggplot2 主题调整整体风格：




```
\# 模拟产品销售渠道数据（长格式）
sales\_df <- data.frame(
&#x20; product = rep(c("手机", "电脑", "平板"), each = 3),
&#x20; region = rep(c("华东", "华南", "华北"), 3),
&#x20; revenue = c(150, 100, 80, 200, 120, 90, 90, 70, 50)
)

\# 按地区着色的桑基图
ggplot(sales\_df,
&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


sales\_df <- data.frame(
&#x20; product = rep(c("手机", "电脑", "平板"), each = 3),
&#x20; region = rep(c("华东", "华南", "华北"), 3),
&#x20; revenue = c(150, 100, 80, 200, 120, 90, 90, 70, 50)
)

\# 按地区着色的桑基图
ggplot(sales\_df,
&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; product = rep(c("手机", "电脑", "平板"), each = 3),
&#x20; region = rep(c("华东", "华南", "华北"), 3),
&#x20; revenue = c(150, 100, 80, 200, 120, 90, 90, 70, 50)
)

\# 按地区着色的桑基图
ggplot(sales\_df,
&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; region = rep(c("华东", "华南", "华北"), 3),
&#x20; revenue = c(150, 100, 80, 200, 120, 90, 90, 70, 50)
)

\# 按地区着色的桑基图
ggplot(sales\_df,
&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; revenue = c(150, 100, 80, 200, 120, 90, 90, 70, 50)
)

\# 按地区着色的桑基图
ggplot(sales\_df,
&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


)

\# 按地区着色的桑基图
ggplot(sales\_df,
&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


\# 按地区着色的桑基图
ggplot(sales\_df,
&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


ggplot(sales\_df,
&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20;      aes(y = revenue, axis1 = product, axis2 = region)) +
&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; geom\_alluvium(aes(fill = region), width = 1/10) +
&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; geom\_stratum(width = 1/10, fill = "white", color = "black") +
&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; geom\_text(stat = "stratum", aes(label = after\_stat(stratum))) +
&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; scale\_fill\_brewer(palette = "Pastel1") +
&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; labs(title = "产品销售额地区分布桑基图", y = "销售额（万元）") +
&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; theme\_bw() +
&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线


&#x20; theme(panel.grid = element\_blank())  # 隐藏网格线
```

按目标节点（`region`）着色，使不同流向的流量更易区分，适合强调终点分布的场景。


### 三、关键参数与场景选择&#xA;



*   **数据格式**：`networkD3`需节点索引 + 边数据框，适合任意流向的复杂网络；`ggalluvial`需长格式数据，适合层级明确的线性流动（如 A→B→C）。


*   **交互性**：`networkD3`生成的 HTML 交互式图表支持悬停查看数值，适合网页展示；`ggalluvial`生成静态图片，适合论文、报告等印刷场景。


*   **样式控制**：`networkD3`通过 D3.js 语法自定义颜色和样式，`ggalluvial`借助 ggplot2 的主题系统，更易与其他 ggplot 图表保持风格一致。


*   **适用场景**：适合展示能量流动、资金分配、用户转化路径、供应链物流等具有 “流量” 特征的数据，节点宽度与流量成正比，直观呈现比例关系。


桑基图在能源、金融、市场营销等领域应用广泛，通过 R 语言的上述方法，可灵活呈现数据的流动路径和分配比例，帮助发现关键节点和流量瓶颈。


> （注：文档部分内容可能由 AI 生成）
>