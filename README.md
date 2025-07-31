---
title: "EasyStat_help"
author: "wentao"
date: "2020/1/11"
output: html_document
---
```{css include=FALSE}
 pre code,pre,code {
 white-space:pre!important;
 overflow-x: scroll!important; 
} 
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,
                      fig.width = 7,
                      fig.height = 5,
                      fig.align = "center",
                      warning = FALSE,
                      message = FALSE
                      
                      )
```

# 差异分析完整解决方案

前一段时间推出了包存在一些细节问题：譬如：
- 非参数检验无字母标注混乱。
- 其次，由于我在做差异检测和出图的时候需要制定同样的数据，所以i设置要相同，之前我没有设置相同，所有将其他列的差异检测结果映射到了别的数据，导致上一篇推送字母标注问题。

目前本包函数其他函数有些小问题，我已经修正，所以大家使用起来应该放心，我修改了示例数据，增加到每个处理6个重复，所以可以更好的作为示例数据再次为大家演示一遍。

> 感谢大家的关注，也会让这个包更加好用，本次处理修正一些错误之外，我还添加的新的出图样式。来源于2020年2月刚刚online的NBT文章。大家继续往后看吧。

## EasyStat 使用指南

安装EasyStat包

```{R eval=FALSE, include=FALSE}
library(devtools)
install_github("taowenmicro/EasyStat")

# 如果国外不可用，则使用国内备份安装
remotes::install_git('https://gitee.com/wentaomicro/EasyStat')

```

导入包和数据，数据均来自真实试验和文公开文献下载，通过调整分组加入。

```{R}
# 导入差异分析包
library(EasyStat)
# 导入作图所需要的ggplot包
library(ggplot2)

#使用内置数据1
data(data_wt)

#内置数据2
data(env)


```

![image](./0.png)

### 基于单个指标的统计分析

正态检验和方差齐性分析，使用?NorNorCVTest查看帮助信息，你会发现目前帮助信息都是使用中文写的，不过随着包的逐渐成熟，和功能的不断完善，将逐渐使用英文编写帮助文档。


```{R}
# 使用?NorNorCVTest查看帮助信息
##使用案例
NorCV = NorNorCVTest(data = data_wt, i= 4,method_cv = "leveneTest")
#提取正态检验结果
NorCV[[1]]
#提取方差齐性检验结果
NorCV[[2]]

```
norm.test会按照分组告诉大家是否符合正态分布。

### 方差分析(aovMcomper)

- data:输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了

- i:代表您想要进行统计的列，比如：第三列：i = 3

- method_Mc:选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc == "LSD";method_Mc == "SNK";method_Mc == "Duncan";method_Mc == "scheffe"
```{R}
# ?aovMcomper
result= aovMcomper (data = data_wt, i= 5,method_Mc = "Tukey")
# 提取多重比较结果
result[[1]]
#提取方差检验结果
result[[2]]
```
结果中多重比较的展示全部使用字母表示了，虽然许多多种比较方法默认展示方式不同，但是我已经在包中将这些展示方式调整一致为字母。


### 非参数检验
两个参数代表的意义与方差分析的两个相同；

- data:输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是妮妮测定或者收集的指标了

- i:代表您想要进行统计的列，比如：第三列：i = 3


```{R}
# ?KwWlx
res = KwWlx(data = data_wt, i= 5)
# 调用非参数两两比较结果：字母标记展示
res[[1]]
#表格展示两两之间差异结果
res[[2]]


```

## 差异可视化方案（两种差异表示，三种图形展示）


### 柱状图展示方差分析或非参数检验结果（aovMuiBarPlot）
在这个包中将差异检测和出图部分分离，方便选择合适的图表和差异可视化的策略。最终要的参数是result ：为前面差异分析结果中的第一个表单，格式为第一列差异显著字母，第二列分组标签，列名，分组标签。如果只是用可视化的函数，直接从外面导入类似数据即可。

```{R}
# ?aovMuiBarPlot
###----使用方差检验结果和多重比较结果做展示：  柱状图展示
PlotresultBar = aovMuiBarPlot(data = data_wt, i= 5,sig_show ="abc",result = result[[1]])
#提取结果
PlotresultBar[[1]]
#提取方差分析或非参数检验结果
PlotresultBar[[2]]
```

```{R}
# ?aovMuiBarPlot
###----使用方差检验结果和多重比较结果做展示：  柱状图展示
PlotresultBar = aovMuiBarPlot(data = data_wt, i= 5,sig_show ="line",result = result[[1]])
#提取结果
PlotresultBar[[1]]
#提取方差分析或非参数检验结果
PlotresultBar[[2]]
```

- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了

- i：代表您想要进行统计的列，比如：第三列：i = 3

- sig_show：代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果

- result：代表显著性差异分析结果，是一个数据框，第一列是显著性差异字母，第二列是分组group

#### 箱线图展示方差分析或非参数检验结果（aovMuiBoxP）

- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了

- i：代表您想要进行统计的列，比如：第三列：i = 3

- sig_show：代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果

- result：代表显著性差异分析结果，是一个数据框，第一列是显著性差异字母，第二列是分组group

```{R}
# ?aovMuiBoxP
# #使用案例
PlotresultBox = aovMuiBoxP(data = data_wt, i= 5,sig_show ="abc",result = result[[1]])
# 提取检验结果
PlotresultBox[[2]]
#提取图片
p = PlotresultBox[[1]]
p

```


```{R}
# ?aovMuiBoxP
# #使用案例
PlotresultBox = aovMuiBoxP(data = data_wt, i= 5,sig_show ="line",result = result[[1]])
# 提取检验结果
PlotresultBox[[2]]
#提取图片
p = PlotresultBox[[1]]
p

```
### 点柱图-完美解决柱状图无法展示样本信息的缺陷

```{R}
# result= aovMcomper (data = data_wt, i= 5,method_Mc = "Tukey")
# # 提取多重比较结果
# result[[1]]
PlotresultBox = aovMuiBoxBarP(data = data_wt, i= 5,sig_show ="abc",result = result[[1]])
#提取图片
p = PlotresultBox[[1]]
p
```

EasyStat包为什么能完美解决差异分析呢.因为他比你想象的要强大。下面使用连线形式展示差异，显著的差异按照标注星号，不显著的标注ns。

```{R}
PlotresultBox = aovMuiBoxBarP(data = data_wt, i= 5,sig_show ="line",result = result[[1]])
#提取图片
p = PlotresultBox[[1]]
p
```

## 多指标模式

### 多个指标同时做正态检验和方差齐性分析（MuiNorCV）

这里对多组数据进行分析，结果我是用T或F代表，方便阅读。

- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了

- num：代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)

- method_cv：代表选择方差齐性的方法，有两种可供选择：method_cv == "bartlett.test" ;method_cv == "leveneTest"
```{R}
dim(data_wt)
# ?MuiNorCV
# 使用案例
norCv = MuiNorCV(data = data_wt,num = c(4:10),method_cv = "leveneTest")
#展示正态检验和方差齐性结果
norCv

```
这里由于指标比较多，所以我将结果进行简化，直接使用ture和false来提示大家，cor是正态性检测组，cv是方差齐性检测。

#### 多个指标方差检验（MuiaovMcomper）

- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了

- num：代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)

- method_Mc：选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc = "LSD";method_Mc = "SNK";method_Mc = "Duncan";method_Mc ="scheffe"


```{R}
# ? MuiaovMcomper
# #使用案例
result = MuiaovMcomper(data = data_wt,num = c(4:6),method_Mc = "Tukey")
#提取每个指标方差检验多重比较结果
result

```
同样，多个指标展示按照指标每列为一组检测结果。

### 多个指标非参数检验（MuiKwWlx）

- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是测定或者收集的指标了

- num：代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)

```{R}
# ? MuiKwWlx
# #使用案例
result = MuiKwWlx(data = data_wt,num = c(4:6))
#提取每个指标非参数检验多重比较结果
result

```

结果和多组方差分析结果一样。很好用于后面的出图，同样也适合自己导入数据，使用出图


### 多组数据可视化差异分析结果： 柱状图（MuiPlotresultBar）

多组指标分开出图，比较麻烦的是图形的保存，如果还需要让你一个一个保存图片，那也是相当繁琐的，所以这里我设置了自动保存，也只有这种方式是自动保存，其他单个，多组分面图形较少，所以就可以自己保存。

我让该函数自动保存每个指标的出图文件到当前文件夹中。这些文件以该指标名称命名；


- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是妮妮测定或者收集的指标了

- num：代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)

- sig_show：代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果

- result：代表显著性差异分析结果，是一个数据框，每一列是显著性标记字母,MuiKwWlx

```{R}
# ?MuiPlotresultBar
# # #使用案例
result = MuiKwWlx(data = data_wt,num = c(4:6))
result
# #结果直接输出到文件夹中
MuiPlotresultBar(data = data_wt,num = c(4:6),result = result ,sig_show ="line")


```

### 多组数据可视化差异分析结果：箱线图（MuiPlotresultBox）

我让该函数自动保存每个指标的出图文件到当前文件夹中。这些文件以该指标名称命名；


- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是妮妮测定或者收集的指标了

- num：代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)

- sig_show：代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果

- result：代表显著性差异分析结果，是一个数据框，每一列是显著性标记字母,MuiKwWlx
```{R}
# ?MuiPlotresultBox
#使用案例
result = MuiKwWlx(data = data_wt,num = c(4:8))
result
# #直接出图到文件夹中
MuiPlotresultBox(data = data_wt,num = c(4:8),result = result,sig_show ="abc")

```
### 线柱图
输入和箱线图一致
```{R}
# ?MuiPlotresultBox
#使用案例
result = MuiKwWlx(data = data_wt,num = c(4:8))
result
# #直接出图到文件夹中
MuiPlotReBoxBar(data = data_wt,num = c(4:8),result = result,sig_show ="line")

```

#### 差异结果展示：分面展示柱状图：(FacetMuiPlotresultBar)

- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是妮妮测定或者收集的指标了

- num：代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)

- sig_show：代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果

- result：代表显著性差异分析结果，是一个数据框，每一列是显著性标记字母,MuiKwWlx
- ncol:代表分面展示每一行放几张图
```{R}
# ?FacetMuiPlotresultBar
# # #使用案例
result = MuiaovMcomper(data = data_wt,num = c(4:11),method_Mc = "Tukey")
result
result1 = FacetMuiPlotresultBar(data = data_wt,num = c(4:11),result = result,sig_show ="abc",ncol = 4 )
result1[[1]]

```

### 差异结果展示：分面展示柱状图：(FacetMuiPlotresultBox)

- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是妮妮测定或者收集的指标了

- num：代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)

- sig_show：代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果

- result：代表显著性差异分析结果，是一个数据框，每一列是显著性标记字母,MuiKwWlx
- ncol:代表分面展示每一行放几张图
```{R}
# ?FacetMuiPlotresultBox
# #使用案例
result = MuiKwWlx(data = data_wt,num = c(4:11))
result
#
result1 = FacetMuiPlotresultBox(data = data_wt,num = c(4:11),result = result,sig_show ="abc",ncol = 4 )
result1[[1]]

```




使用两种方法，我们可以对比非参数检验和方差检验结果是否一致

### 下面使用线柱图展示
```{R}
library(ggplot2)
# ?FacetMuiPlotresultBox
# #使用案例
result = MuiKwWlx(data = data_wt,num = c(4:11))
result
#
result1 = FacetMuiPlotReBoxBar(data = data_wt,num = c(4:11),result = result,sig_show ="abc",ncol = 4 )
result1[[1]] + theme_bw()

```


注意以上三种分面展示的目前仅支持字母标注显著性，连线形式的尚未添加。

### 单个指标一体化分析（SingleStat）

这个函数可以将我们的目标列做正态检验和方差齐性，然后根据结果选择方差检验或者多重比较方法，最后选择自己需要的出图方式和显著性标记方式展示。

- data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是妮妮测定或者收集的指标了

- i：代表您想要进行统计的列，比如：第三列：i = 3

- method_Mc：选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc == "LSD";method_Mc == "SNK";method_Mc == "Duncan";method_Mc == "scheffe"

- plot：可以选择需要的出图类型，柱状图和箱线图


```{R}
# ?SingleStat
# # #使用案例
# #输出结果第一个为图片，第二个是统计结果，第三个是统计方法
result = SingleStat(data = data_wt,plot = "bar",method_Mc = "Tukey",i= 4,sig_show ="abc")
# #导出图片
p = result[[1]]
p

```

可以更换出图方式,当然这里会自动判断使用方差分析，还是非参数检验。选择结果会展示在结果的第三个列表中，可自行查看。
```{R}
# ?SingleStat
# # #使用案例
# #输出结果第一个为图片，第二个是统计结果，第三个是统计方法
result = SingleStat(data = data_wt,plot = "box",method_Mc = "Tukey",i= 4,sig_show ="abc")
# 提取差异检测结果
result[[2]]
# 提取差异检测放啊
result[[3]]

# #导出图片
p = result[[1]]
p


```

```{R}
# ?SingleStat
# # #使用案例
# #输出结果第一个为图片，第二个是统计结果，第三个是统计方法
result = SingleStat(data = data_wt,plot = "boxbar",method_Mc = "Tukey",i= 4,sig_show ="abc")
# 提取差异检测结果
result[[2]]
# 提取差异检测放啊
result[[3]]

# #导出图片
p = result[[1]]
p


```




### 多个指标一体化分析(MuiStat)

实现了多个指标批量整体运行；这个函数可以将我们的目标列做正态检验和方差齐性，然后根据结果选择方差检验或者多重比较方法，最后选择自己需要的出图方式和显著性标记方式展示。

```{R}
# ?MuiStat
#使用案例
result = MuiStat(data = data_wt,num = c(4:11),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 4,plot = "box",plottype = "mui")
result[[1]]

# 提取方差检测的列
result$aov
# 提取f非参数检测的列
result$wlx
# 提取差异检测结果
result$table
```

柱状图

```{R}

result = MuiStat(data = data_wt,num = c(4:11),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 4,plot = "bar",plottype = "mui")
result[[1]]

```

```{R}

result = MuiStat(data = data_wt,num = c(4:11),method_cv = "leveneTest",method_Mc = "Tukey",sig_show  = "abc",ncol = 4,plot = "boxbar",plottype = "mui")
result[[1]]

```

data：输入数据框，第一列为样本编号，第二列为分组，注意分组标签必须设定为group，第三列以后就是妮妮测定或者收集的指标了

num：代表您想要进行统计的列,这里可以输入多个列，只需要指定列号即可：例如：num = c(4:6)

method_cv：代表选择方差齐性的方法，有两种可供选择：method_cv == "bartlett.test" ;method_cv == "leveneTest"

method_Mc：选择需要使用的多重比较方法，这里又多种方法可供选择：method_Mc == "LSD";method_Mc == "SNK";method_Mc == "Duncan";method_Mc == "scheffe"

plot：可以选择需要的出图类型，柱状图和箱线图

sig_show：代表差异展示方式；sig_show ="abc"是使用字母表示;sig_show ="line"是使用连线和星号表示；如果是NA，那么就不显示显著性结果

ncol：代表分面展示每一行放几张图

plottype：输出图形是分面展示plottype =mui，还是单张展示:plottype == "single"












# EasyStat
# EasyStat
