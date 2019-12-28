# 基础结构

R为对象语言，R语言因为历史关系，对象的模式较多。最早期为S3，后引入S4，但个人认为构建较为合理的是R6。

## 基本类型及赋值
对象底层为基本类型由`typeof`决定，atomic type包括integer, numeric, character，而结构上则是vector 和 list。data.frame本质上就是list.

------------------------------------
`assgin(x, value)`将value赋值给x，其中x为字符串

`get(x)/mget(x)`获取x中的值，x为字符串

`eval(expr)`能够运行expr中的表达式。但expr需要时expression对象

`parse(text = x)`生成内容为字符串x的expression对象

## S3 

**对象构造**

R基础的面向对象的方式。其中使用class标记类型，如构造名为student的类。 本质上继承就是添加新的`class`

```R
stu <- list(name='lao',id=1101,weight=65) 
attr(stu,'class') <- 'student' # class(stua) <- 'student' 
```

其中`attr`用于具体变量属性，常见的包括dim,class,names等

---------------------

**对象函数**

针对某一类的函数

```R
# 构造一个范式函数
gprint <- function(x) UseMethod('gprint') # 构造具体的函数
gprint.student <- function(x){
    cat("names:",x$name,'\n')
    cat("id:",x$id)
}
gprint(stu)
# 输出结果为：
# names: lao  
# id: 1101
```

通过给变量同时设置多个class来继承。 在调用范式函数时，按先后顺序调用。 如果在范式函数中加入NextMethod()则会继续调用下一个类型的范式。

常见的函数如下：

- `methods(fun, class)` 
  - fun：范式函数,查看某一范式函数的所以类 
  - class:类，查看某一类的所有函数
-  `getAnywhere(fun)`代码不可查询时，可以查看具体的代码

## S4

部分后续的开发会采用S4模型，如`DBI`

```R
## 定义类型
track <- setClass("track", slots = c(x="numeric", y="numeric"))
## 构建变量
t1 <- track(x = 1:10, y = 1:10 + rnorm(10))
## 继承
trackCurve <- setClass("trackCurve",
                       slots = c(smooth = "numeric"),
                       contains = "track")

```

相关函数如下：

- `setClass(class, slot, contain)`用于定义类

  - class：character用于设定类型
  - slots：list.设定类型的属性
  - contain：'character'继承父类

- `slot(aa,'name')`访问对象可以使用，或直接使用@

  或者直接使用@调用，如aa@name

-----------------------------

```R
## 建立函数母型
setGeneric('work',function(obj) attributes('work'))
## 构造函数对象
setMethod('work',signature(obj='track'), function(obj) cat(obj@x,'is work'))
work(t1)

```

流程较为固定，需要注意的是不能直接兼容S3，也就是不能直接在S3的泛函上构建函数。

相关函数如下：

- `showMethods('work')`/`getMethod('work','track')`由于无法直接查看函数代码，需要先查看类型，然后查看函数

## R6

尽管有R4/R5，但由于结构越复杂使得运行速度约慢，因此直接使用R6更为有效。R6为一个包，运作方式与C++相同，其实质上是一个S3类型。

对象化时首选R6。

框架方法
```R
## 构建原型
stock_acount <- R6Class("stock_acount",
public = list(
	con = NULL,
	initialize = function(acount_f = 0)
	{private$acount_f <- acount_f
     self$con <- con},
     stock_buy = function(){}),
private = list(
	acount_f = 0)
)
## 构建对象
stock_acount(acount_f = 10000)
```
# 文件操作读写

## 文件操作

dir系列主要是路径操作，file是文件操作

`list.files();list.dirs()`两者是一致的。

- recursive (logical) 是否列出子文件（前者否，后者是）
- full.names (logical) 是否显示完整路径（前者否，后者是）
- pattern 即正则式
- path 设置具体路径

`dir.create(recursive=T)`
recursive使之能够创建多层子路径，否则只有当前文件夹下。

`file.create()`创建文件

`file.exists()`检验文件是否存在

`file.remove()`删除文件

`file.append(filea,fileb)`将文件b添加在文件a尾部。如`file.append('a.txt',rep('b.txt',3))`

## 数据读写

`source('xx.R')`执行一个文件

`sink('filename',split=T)`运行结果输出(仅文本)，split为屏幕和文件同时输出，sink()时即返回屏幕

`png`/`jpeg`等为图像输出。结束后需使用`dev.off()`关闭

--------------------------
常规数据如csv建议采用`readr`

该包的读入逻辑为

1. 用`parse_guess`读入前n行判断类型（guess_max参数控制，1000）
2. 用`parse_*`的函数来读取文本向量。

`read_csv`csv文件/`read_fwf`定宽文件

- `col_names = T` 默认读入列名，但是可以用c("a","b")等文本向量赋值列名
- `na = c("","NA")` 空值的数据
- `col_type = NULL` 例如cols(x = col_double(), y = col_number())，对于所有行来说可以设置.default = col_character()。所有参数与parse_*类一致。

此外可以协助文本处理，如`parse_number('$123')`能够输出123

通过`guess_endcoding(charToRaw(x))`来判断后试用`parse_character(x, locale = locale(encoding = ))`转码

----------------------------

excel文件相对麻烦，采用`readxl`读入。输出推荐`openxlsx`。但更为负债的操作需要`XLConnect`

-----------------------------

数据库采用`DBI`统一接口，通过不同包如`RMySQL`,`odbc`

```R
# 用于读取接口，建立远程链接是使用host参数
con <- dbConnect(MySQL(), dbname = "rmysql", username="rmysql", password="rmysql",client.flag=CLIENT_MULTI_STATEMENTS) 
# 关闭接口
dbDisconnect(con)
# 展示存在的表
dbListTables(con)
# 提取结果
dbGetQuery(con, "SELECT * FROM t_user")
# 写入表
# append=T可以继续插入数据，overwrite=T为覆盖
# field.types控制输入的数据格式
dbWriteTable(con, "temp", data, row.names = F, field.types = list(time='TIMESTAMP'))

```

由于格式问题有些数据库需要先输入`dbSendQuery(con,'set names GBK')`

DBI在windows下使用group隐藏密码的功能不能使用。推荐直接在R安装目录etc文件夹下修改Rprofile.site

------------------------------

**matlab**

`library(R.matlab)`

**sav,dta**

`library(foreign)`或`library(haven)`

`read.spss("xx.sav")`

`read.dta("xx.dta")`

# 数据操作

使用`tidyverse`

`options(tibble.print_max = n, tibble.print_min = m)`用于设置显示的行数

`options(tibble.width = n)`用于设置列数

## 数据变形

**数据规整**

`gather(data, key, value, ...,factor_key = F)` 短数据框边长

`spread(data, key, value, fill = NA)`长数据框边宽

**行列组合**

`unite(data, col, ..., sep = "_", remove = TRUE)`用于合并多列

`separate(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, convert = FALSE`用于分割列

**数据压缩**

`nest(data, ..., .key = data)` 压缩数据为list，注意group会影响结果

`unnest(data,...)`解压

配合`map`系列使用

--------------

**数据合并**

`full_join`,`left_join`,`right_join`,`inner_join`

两种非常见方式

`semi_join(x,y)`保留x中跟y对应的行

`anti_join(x,y)`剔除x中跟y对应的行

集合运算系列

`intersect(x,y)`,`union(x,y)`,`setdiff(x,y)`

与base中的相同，但是可以基于数据框操作




## 组合操作

该系列运算效率弱于data.table，但语法更人性化

`select`用于列选择，同时可以命名

- `start_with`/`ends_with`/`contains`文本包含
- `matches`正则表达式
- `one_of(c("a","b"))`用于文本的列名选择
- `everything`剩下所有变量选择
- 此外`select(a:b)`可以选择之间的变量

`group_by`组运算标记

`filter`用于筛选，可配合秩函数使用，如`min_rank`/`row_number`/`dense_rank`等

> 该函数效率低于subset，但可配合group

`arrange`用于排序，配合`desc`降序

`mutate_*`/`transmuta_*`用于生成变量

`summarise`分组统计运算

## 时间操作

**概念**

R中时间格式包括Date和Datetime两种，后者为POSlXct。`format(x,)`输出，标签格式包括%y%Y%B%b%A%a%d%H等。

常规运算操作建议统一采用`lubridate`

总共分为三个类

- datetime/date,特定日期
- period，时间长度概念，如1小时
- timespan，特定的时间区间，如2011年-2015年

--------------------------

**生成**

`ymd`/`mdy`/`ymd_hms`等，生成datetime，对应的代价是相对于严格的as.Date更慢。

- 特定时间格式可以提取或复制特定时间，`second/minute/hour/day/wday/yday/week/month/year/tz` 一系列可用于提取部分信息。其中`month`和`wday`可以设置label=T

`interval(time_beg, time_end)`，生成时间区间，也可以通过特定日期`%--%`时间相减获得

- `int_overlaps`是否重叠，`intersect`提取重叠期间，`int_start/int_end`开始及结尾的提取或赋值

`year/hours/minutes/days`等，生成period，也可以通过`as.period`时间区间转化得到

------------------------------

**时间运算**

`round_date/floor_date/ceiling_date(x, unit = 'week')`根据单位取整，就可以按需要的内容进行调整

```r
##date + periods = date
ymd(20160101)+years(1:2)
##[1] "2017-01-01" "2018-01-01"
##date - date = timespan
ymd(20160101)%--%ymd(20150101)
##[1] 2016-01-01 UTC--2015-01-01 UTC
##timespan / periods = dbl
interval(ymd(20090101), ymd(20090103)) / days(1)
##[1] 2
```

唯一需要注意的是对于月份的处理，由于存在月末不一致。需要引入符号%m+%

## 文本处理
`toupper;tolower`大小写转换

`str_pad`将字符串补充为固定长度，超出则不变

`strsplit`字符串分割

--------------------------------------------

`grep;grepl`正则匹配文本，前者返回序列，后者返回逻辑值

`sub;gsub`用于替换

----------------------------------

包`stringr`提供更多操作

`str_extract`抽取内容

`str_locate`定位

```R
str_extract('my email is laokaij@qq.com','\\w+@\\w+\\.\\w+')
```

# 数学运算

## 基本数学

**数据运行**

`ceiling;floor;round;trunc`上下取整等

`factorial`/`choose`排列组合

**复数相关**

`complex(re=1,im=2)`构造复数

`Re(z);Im(z)`实部虚部

`Mod;Arg(z);Conj(z)`模，辐角，共轭

**集合系列**

`intersect`/`union`/`setdiff`并，交，差

`setequal`集合比较，不关心元素顺序

**积分/差分**

包`numDeriv`

`grad(function(x){1/x},1)`导数

`integrate(function(x){1/x^2},lower=1,upper=Inf)`积分



## 规划求解

### 简单求解

一元函数

`uniroot(fun,interval)` 单根求解，fun为函数，interval为区间

区间必须是两头符号相反。难以找到时可以考虑用规划问题求函数平方的极值点。

一元多项式

`polyroot(1:3)` 内部为系数，按照$c+a_1x+a_2x^2$的顺序

### 最优规划-ROI

`library(ROI)`提供了一个稳定的应用接口。

`OP(objective, constraints = NULL, types = NULL, bounds = NULL,maximum = FALSE)`用于构建问题。

`solve(op)`求解

----------------------

`object`目标函数

`objective(op);objective(op)<-value`可以提取并修改原优化条件

- `L_objective(c)`对应$c^Tx$
- `Q_objective(Q,c)`对应$\frac{1}{2}x^TQx+c^Tx$
- `F_objective(F,n)`对应$F(x)$，其中n表示变量个数

----------------------------

`bounds`求解范围

`bounds(op);bounds(op)<-value`可以提取并修改原范围

`V_bound(li, ui, lb, ub, nobj, ld = 0, ud = Inf)`

- `ui/li`，int，上界，下界使用非默认的变量
- `ub,lb`,vector，对应`ui/li`下界使用的数据
- nobj 变量个数
- `ud/ld` 默认上下界

--------------------------
`constraints`约束条件

`constraints(op);constraints(op)<-value`可以提取并修改原约束

其中dir可以替换约束条件方向，包括"<=","==",">="。
- `L_constraint(L, dir, rhs)`对应$L^Tx<=rhs$，有几个约束条件使用多少个dir。
- `Q_constraint(Q, L, dir, rhs)`对应$\frac{1}{2}x^TQx+L^Tx <= rhs$
- `F_constraint(F, dir, rhs)`对应$F(x) <= rhs$，
- `C_constraint(L, cones, rhs)`主要用到为
	- KSOC $\{(t,x)|||x||_2<=t\}$

对于约束条件可以使用`rbind`进行合并。

-------------------------------
匹配可行优化器

`ROI_applicable_solvers(op)`当前可用

`ROI_available_solvers(op)`网络可用

----------------------------------
求解

`ROI_solve(op, solver, control = list(), ...)`solve用于设定具体规划器



### 最优规划-CVXR

本质上通过CVXR能够实现的规划通过ROI重新构造实现，ROI胜在支持的优化器数量，CVXR胜在构造简便。



# 运行效率

在循环时，最快的就是向量化。

必须使用for循环时应当避免直接使用`c`和`rbind`等。尽量使用list,然后`do.call`连接，以免不断开辟新内存。基本上与apply系列的速度相当。for可调式，apply系列更为简洁且方便并行改造。

同样的，用空间换时间有着天然的优势。

## 效率评价

`library(microbenchmark)`，主要用于不同代码间的比较
`library(profvis)`，用于分析代码块的瓶颈。该包已经集成在RStudio，使用profile中工程即可。

## 并行

####parallel
R自带的并行包为`parallel`。该包继承于`snow`和`multicore`。

继承snow的主要cluster的方式并行，实际上开启了多个Rscript。采用能够兼容sock和Rmpi多种通信方式。但是传递变量时采用的是`serialize()`和`unserialize()`这一组函数，因此不兼容的都将无法运行。如`Rcpp`。

继承miticore的为单机多核运行，内存共用，仅变量变动时才复制为新变量，因此效率较高。但无法在windows上使用。

继承于snow的标记为par系列，而multicore的是mc系列。

更多的多线程类型。详细可以参考[High-Performance and Parallel Computing with R](https://cran.r-project.org/web/views/HighPerformanceComputing.html)。

------------------------------------
**集群建立**
parallel继承了snow建立集群的方式。
`cl = makecluster(spec,type)`
`stopcluster(cl)`
- `spec`可以是数字，表示集群数量；或字符向量，表示各个集群的名字。
- `type`,windows下默认`PSOCK`，否则为`FORK`。前者对应多cluster，后者本质为多核运行的内存共享。

其他参数，如`outfile`能输出并行程序中的结果，通常用于debug。

-----------------------------
**并行函数**
`parLapply(cl, x, fun)`
`mclapply(x, fun)`

对应lapply，其中mclapply采用multicore的形式，即用即销。而parLapply在原snow的基础上提供了兼容fork的模式。

对于par系列的还有
`parApply`对于Apply,`parRapply`对应matrix行的运算。
实际上parallel中还提供了snow中原始的cluser系列（如clusterApply等）的继承，但是**运行效率低于par系列**。


特别介绍的是`parLapplyLB`，当各个分块的运算量不均衡时明显优于parLapply。标准的方案是采用push的模型，即将任务推送给每个worker完成后推送下一轮。LB的特点是当worker完成后会pull一个新的任务进行处理。

---------------------------
**集群调整**
parellal提供了更多继承于snow的函数。
`clusterEvalQ(cl, expr)`expr为程序语句，但不兼容函数。
`clusterCall(cl, fun)`运行函数。

`clusterExport(cl, "xx")`传入变量。

-------------------------------
**随机数**
对于集群模式影响更小，但是对于fork模式由于共享内存使得随机结果不确定。

集群提供的方案是
`clusterSetRNGStream(cl, 221)`
fork模式
`RNGkind(kind)` kind常见为"L'Ecuyer-CMRG"
####foreach
`foreach`包结合`doparallel`等包提供了一个较为简单的并行解决方案，而不用过多考虑集群中包加载和变量的输入问题。`%do%`和`%dopar%`提供是否并行两种模式。

foreach会自动加载当前环境的变量（该加载是指若集群中被调用时，因此不会增加过量的负担），但不会加载父环境变量，使用`.export = c('a','b')`加载。
同样，使用`.packages = c('dplyr')`加载包，需要注意的是当线程不关闭时加载过的包和变量会一直保留。
其他参数
`.combine`包括'+'等，实际上可以用任何自己构建的函数去组合结果
`.multicombine = T` 如果.combine的函数可以用于多变量如rbind，则选择T
`.inorder = T`，默认T，结果是否需要按输入排序

但是运行效率上，foreach %do%相对于lapply与foreach %dopar%相对于parLapply都明显更慢。
``` r
library(foreach)
library(doParallel)

fun <- function(x)
{
  mean(rnorm(x))
}

system.time(mapply(fun, 1:20000))
# user  system elapsed 
# 21.44    0.17   21.76 
system.time(foreach(i = 1:20000, .combine = 'c') %do% fun(i))
# user  system elapsed 
# 27.83    0.05   28.10 
cl <- makeCluster(3)

clusterSetRNGStream(cl, 221)
system.time(parLapply(cl, 1:20000, fun) %>% do.call('c',.))
# user  system elapsed 
# 0.00    0.00   13.53

clusterSetRNGStream(cl, 221)
system.time(parLapplyLB(cl, 1:20000, fun) %>% do.call('c',.))
# user  system elapsed 
# 0.05    0.01   13.83

clusterSetRNGStream(cl, 221)
system.time(foreach(i = 1:20000, .combine = 'c') %dopar% fun(i))
# user  system elapsed 
# 27.97    0.03   28.22

stopCluster(cl)
```

####multidplyr
hadlay大神教，基于`parallel`，提供了par_tbl类型匹配dpylr中`group_by`,`filter`,`mutate`,`summarise`,`do`。
建立cluster
`cl <- create_cluster`
复制入变量
`a <- 10;cl %>% cluster_copy(a)`或者`cl %>% cluster_assign_value('a', 10)`
加载包
`cl %>%cluster_library(c("magrittr", "stats"))`
运行
`cl %>%cluster_eval(fun())`
数据切分
`flights %>% partition(dest, cluster = cluster)`当cluster不存在的时候会调用默认cluster，切分后的partition可以作为cl被上述函数直接调用。

同时函数兼容dplyr中操作，自动进行并行。

当变量删除后，gc自动清除相关内容。

## Rcpp

从结果来看，进行数值运算时还是Rcpp的运算速度最快。
需要先下载安装Rtools，尽量减少路径更改，会有各类不知名问题。

依赖于`library(Rcpp)`
主要函数即`cppFunction`和`sourceCpp`
```r
library(Rcpp)

fun <- function(x)
{
  times <- 1000
  x <- matrix(rbinom(100 * times,1,0.5), nrow = times)
  mean(apply(x, 2, function(x) min(which(x[-1] * x[-100]==1))))
}


cppFunction(
  'int fun1()
  {
    int a = 0, n = 0;
    while (a !=2)
    {
      if(rand()%2 == 1)
      {a = a + 1;
      }else{a = 0;}
      n = n + 1;
    }
    return n;
  }')

sourceCpp("C:\\Users\\cloud\\Desktop\\fun2.cpp")

system.time(mean(replicate(1000,fun())))
## 用户  系统  流逝 
## 16.20  0.22 16.58 
system.time(mean(replicate(1000000,fun1())))
## 用户 系统 流逝 
## 3.23 0.00 3.25
system.time(mean(replicate(1000000,fun2())))
## 用户 系统 流逝 
## 2.94 0.00 2.95

library(rbenchmark)
benchmark(mean(replicate(100,fun())),mean(replicate(100000,fun1())),mean(replicate(100000,fun2())))

```
可以看出效率明显提高。甚至高于分布式。但sourceCpp并没有明显效率高于cppFunction，数量增加时会有一定提升。

在使用soureCpp时所写的cpp函数
``` c++
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int fun2()
{
  int a = 0, n = 0;
  while (a !=2)
  {
    if(rand()%2 == 1)
    {
      a = a + 1;
    }else{
      a = 0;
    }
    n = n + 1;
  }
  return n;
}
```
使用时返回向量或数据集时采用SEXP对象。

##openblas
该开源项目在底层的矩阵运算上能够大量的提高R的运算效率。
替换的是底层包中使用libopenblas.dll替换Rblas.dll文件。其他缺失文件网上索引即可。

##debug
###问题构建
需要能够复现的数据，当数据较为麻烦时可以使用dput和dget。
有时可以直接适用dput拆解数据查看细节差异。

##开发包
基于`devtools`。hadley大神教。
RStudio中包括相关方法。
点击file -> new project -> new Directory -> R pacakge
然后将.R文件放入。

-------------------------
编辑DESCRIPTION
```
Package: backtest
Type: Package
Title: back test for stock acount
Version: 0.1.0
Author: person(family = 'Lao', first = 'Kaijun', email = 'cloud_brain@qq.com')
License: MIT
LazyData: TRUE
Imports:
    dplyr,
    R6,
    PerformanceAnalytics,
    RODBC,
    stargazer,
    xts
```

------------------
编辑函数文件，函数本身无问题，但由于需要提供注释，因此依赖于`roxygen2`，通过#'作为标识符号。

```
 #' hello world
 #'
 #' hello word for n times
 #'
 #' @param times integer, the times say hello world
 #'
 #' @return
 #' vector n times for 'hello world' 
 #'
 #' @examples
 #' \dontrun{
 #' hello(10)
 #' }
 #' @export
 #'
 #' @import dplyr
hello_world <- function(times = 1)
{
	'hello, world' %>%　rep(5)
}
```
@@title
@description
@details
按顺序出现，可以省略
@param 输入变量
@examples 表示例子

@@export表示输出改函数进入namespace，如果不包括export则函数可在内部调用，但无法使用包的人调用。

@@import表示需要加载的包。需要说明的是，加载仅表示在该包内部能够调用，在函数外部使用时仍需要加载函数。
另为了能够同时下载其他的包，需要通过`use_package('dplyr')`形式加入到DESCRIPTION中。

对于特殊的函数形式，如R6，应设置
```
#' @docType class
#' @importFrom R6 R6Class
```

-----------------------
编辑包文档和数据文档。
```
#' equal weight index
#'
#' the index change stock the first business day of each month
#'
#' \itemize{
#'   \item date. date of index
#'   \item acount.
#' }
#'
#' @docType data
#' @keywords datasets
#' @format A data frame with 2638 rows and 2 variables
#' @name m_index_acount
NULL
```

use_data(x) 加载数据到包中

```
#' make a stock acount for back test
#'
#' make a stock acount for initial parameter, then buy or sell stock with function
#'
#' @import dplyr xts R6 
#' @docType package
#' @name backtest
NULL
```
通常在该文件中加载所有包

可以看到特殊的标识为
`@docType data/package用于表示文档类型
`@name 用于表示文档的名字

------------
document()能够重新生成文档
load_all()能够在项目结构下载入当前编辑包的所有函数，尝试运行

check()检验包中是否有问题
build()建立包，build(binary = T)为二进制包，但系统依赖

