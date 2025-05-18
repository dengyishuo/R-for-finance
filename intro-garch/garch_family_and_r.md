# GARCH族模型及其在R中的实现

信息|内容
----|------
Author|邓一硕
Update|2010年4月


## 序言

一个伟大的人物存在的意义在于他可以将一个复杂的过程表述的简单明了，从而，免去常人为之苦苦思索的痛楚过程，这样便可以使得更多的人能够享受知识的快乐！

## 1.GARCH模型宗族的诞生

### 1.1 传统时间序列分析的危机

金融数据越来越占据数据分析的主流之后，传统的时间序列分析方法面临了一次小小的危机。由于金融数据一般具有以下四个特点：
* 金融资产价格数据序列通常是非平稳的，然而，收益率序列却通常是平稳的。越来越多的数据表明一些金融数据序列是分数单整的。
* 收益率序列通常表现出无自相关或者很小的自相关。然而，平方值序列的相互独立性经常被拒绝，这说明子序列的观测值之间存在非线性关系，且收益率序列的波动呈现出群集性。这些现象都与依时间变化的条件变量有关。
* 由于金融时间序列数据往往表现出肥尾性，正态性往往被拒绝。序列中存在的无条件超风度传统的ARMA模型等无法有效的刻画金融数据的四个主要特征。
* 一些金融时间序列表现出杠杆效应。

### 1.2 勇敢的Engel和ARCH模型的诞生

时间无情地流淌着，转眼就到了1982年。1982年对很多人而言只是众多年月中的其中之一，并没有任何特别意义。这一年，Engel正在苦心研究英国通货膨胀指数的波动性。在研究波动性的过程中，他也面临了传统时间序列无法完美解决的问题之一，即时间序列波动的积聚性。勇敢的Engel在面临这个难题的时候选择了一个与众不同的道路，他没有避而远之，而是决定独自攻克这个顽固的碉堡。经过一些不为人知的努力，他终于提出了令后世瞩目的自回归条件异方差模型（Auto Regressive Conditional Heteroskedasticity Model），也就是ARCH模型，从而掀开了时间序列分析的新篇章。毫不夸张地说，ARCH模型是Engel的学术恩人，正是这个模型使Engel获得了2003年度诺贝尔经济学奖，同时也使其名扬四海。我想这正是命运的巧妙之处，它让面临同样问题的人们采用截然不同的解决方案，从而为这个世界不断地建立新的里程碑！

在 Engel 最初的理论体系中，ARCH 模型没有任何神秘的色彩。假设$$ \epsilon_t $$是一个满足ARCH模型的时间序列，那么，它一般具有如下形式：

$$ \epsilon_t|\phi_{t-1}=h_t\inta_t $$
$$ \h_t^2=\alpha_0+\sum_{i=1}^{r}\alpha_i\epsilon_{t-i}^2 $$
$$ \inta ~idd N(0,1) $$
 
 
其中，\phi_{t-1} 为时刻t及其之前所有时刻的信息集，符号 iid 表示独立同分布。从（1）-（3）式出发很容易计算出 $$ \epsilon_t $$ 的条件期望和条件方差分别为：

$$ E(\epsilon_{t}|\phi_{t-1})=0 $$
$$ V(\epsilon_{t}|\phi_{t-1})=h_t^2 $$
 
 
那么显然， $$ \epsilon_t $$的条件分布为：


 
可以看出，序列 的条件方差 是一个随时间而变化各异的量，而且这个条件方差是序列 过去n项平方的线性组合，这句话的前半部分解释的是异方差的来历，后半部分解释的是自回归的来历。
值得指出的一点是，在ARCH模型的参数中，一般会要求 ， ，以保证条件方差 为正。那么，万一有一天你在某一次拟合过程中发现 ，或者 的情况，该怎么处理呢？赶快去检查数据、检查软件拟合模型的过程，如果这一切都没有问题，那么恭喜你，你遇到了一个不适合建立ARCH模型的时间序列！

### 1.3 GARCH模型的出世
 在 模型阶数非常富有弹性，当阶数过高时，有人意识到可以在式（2）右边加入过去的条件方差项。这样广义自回归条件异方差模型（Generalized Autoregressive Conditional Heteroskedasticity Model），即GARCH模型，就横空出世了。 模型的一般形式如下：
                         （7）                       （8）                                                    （9）
这个过程其实类似于AR模型到ARMA的推广过程，它是由Bollerslve在1986年完成的。说到这里，如果你在意一下时间跨度，你会明白我的一个惶惑，“为什么这个简单的扩展过程竟然耗费了整整四年的时间”。这一点的确让人感到遗憾，不过好在从此以后，GARCH模型便开始枝繁叶茂，人丁兴旺，并逐步在金融领域打下了自己的一片天下，成为了名副其实的显赫家族。

## 2.GARCH模型族的成长史

##@ 2.1 GARCH模型家族树                

                                          

### 2.2 NGARCH模型
英文名称：Nonlinear Asymmetric GARCH(1,1)，
作者：Engle and Ng in 1993
中文名：非线性非对称GARCH模型
简介：
 
其中， ， 。
特点：对股票收益率序列的估计中 一般是正值，它的经济学意义是指刻画了杠杆效应。说明，负的收益率对未来走势的影响大于正的收益率对未来走势的影响。

### 2.3 IGARCH模型
英文全称：Integrated general autoregressive conditional heteroskedastic
中文名：单整GARCH模型。
别称：方差无穷大的GARCH模型。
简介：IGARCH模型是在GARCH模型的基础上加入下面这个约束条件：
 
以GARCH(1,1)为例，式（8）可以写成关于 的ARMA(1,1)模型。
 
 
当 时，自回归部分的特征方程 有一个单位根，上式是一个关于 的单整ARMA(1,1)模型，这也是它名字的来历。
特点：在IGARCH模型中任何对条件方差的影响都将持续下去，也就是 具有持续的记忆性， 的无条件方差无穷大。

### 2.4 EGARCH
英文全称：exponential general autoregressive conditional heteroskedastic 
作者： Nelson (1991)
中文名：指数GARCH模型
简介：将GARCH模型的 部分换为 即可得到EGARCH模型，其具体形式如下：
 
其中，
 
特点：它的一个开创之初在于避免了对参数的限制。

### 2.5 GARCH-M模型
英文全称：GARCH-in-mean        
作者：Engel、Lilie、Robbins
中文名：均值GARCH回归模型
简介：均值GARCH模型调整了GARCH模型的回归方程，具体形式如下：
 
 
 
特点：不仅描述自回归条件异方差模型，而且把条件方差作为回归因子引入相应的回归或均值方程。这个模型说明，金融资产收益率的大小除了受其它因素影响之外，也受到收益率本身波动性大小的影响。

### 2.6 QGARCH模型
英文全称： Quadratic GARCH (QGARCH) model 
作者： Sentana (1995)
中文名：二次GARCH模型
简介：
 
 
特点：也是刻画不对称。

### 2.7 GJR-GARCH
英文全称：Glosten-Jagannathan-Runkle GARCH (GJR-GARCH) model by 
作者：Glosten, Jagannathan and Runkle (1993) 
简介：
 
 
如果 ，那么 ，如果 ， 
特点：刻画不对称。

### 2.8 TGARCH模型
英文全称：Threshold GARCH (TGARCH) model 
作者：Zakoian (1994) 
简介：
 
如果 ， ， 。如果 ，那么 ， 。
特点：刻画不对称。

### 2.9 FGARCH模型
英文全称：Family GARCH
作者：Hentschel（1995）
中文名：FGARCH
简介：它并不是一个独立的GARCH模型，更像是一个GARCH模型包，包罗APARCH, GJR, AVGARCH, NGARCH,等。

# 3.GARCH模型与R的联姻

近年来，GARCH模型的研究主要是实证方面的，理论上没有再取得可歌可颂的成果。最值得歌颂的是GARCH模型与R的联姻。

## 3.1 收容GARCH模型的各种R包

* GO-GARCH: http://r-forge.r-project.org/projects/gogarch/
* tseries: http://cran.r-project.org/web/packages/tseries/index.html
* ccgarch: http://cran.r-project.org/web/packages/ccgarch/index.html
* fGarch: http://cran.r-project.org/web/packages/fGarch/index.html
* fOptions：http://cran.r-project.org/web/packages/fOptions/index.html
* rgarch: http://rgarch.r-forge.r-project.org/

## 3.2 rgarch:GARCH模型的乐园

### 3.2.1 RGARCH主页剪影
以上各个R包里最厉害的当属rgarh，Rgarch是R-Forge旗下的一个致力于GARCH模型的一个包。
 

### 3.2.2 rgarch的安装

* 与众多R包一样，rgarch有两种安装方式：

在R中下载pkg，运行R软件，键入如下代码：

```
install.packages("packagename",repos="http://R-Forge.R-project.org")
```

* 手动下载，打链接http://r-forge.r-project.org/projects/rgarch/， 看到下载界面：
选择适合的下载类型，手动安装即可。

### 3.2.3 一个完整的模型拟合过程

* 数据基本描述
*	数据检验
　* 正态性检验
  * 自相关检验
  * 异方差性检验——LM检验或检验
* 模型定阶
* 模型参数估计
* 条件方差的估计
* 适应性检验
* 条件方差的预测


# 4.礼节性的总结

## 4.1 致谢
 非常感谢
 
## 4.2 参考文献
[1]《应用时间序列分析》，王振龙、胡永宏主编，科学出版社。

[2]《金融中的统计方法》，[美]，G.S.马达拉 C.R.拉奥 上海人民出版社。
