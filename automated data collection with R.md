8.1.1 精确字符匹配

我们在这里以及这章节余下部分使用的函数是来自`stringr`包的`str_extract()`，这里已经假定它被载入了。这个函数是这么定义的：`str_extract(string, pattern)`，我们先传入字符串`string`， 我们将在其中寻找第二个表达式。注意与`grep()`或者`grepl()`等基础函数的区别，它们的第一个参数一般是正则表达式。这个函数将返回在给定字符串中找到与正则表达式相匹配的第一个实例。我们也可以通过调用`str_extract_all()`让`R`提取所有匹配项：
```r
R> unlist(str_extract_all(example.obj, "sentence"))
[1] "sentence" "sentence"
```
`stringr`包提供了很多类似于`str_whatever()`和`str_whatever_all()`的函数。前者定位于第一个匹配的字符串，后者则取得全部匹配实例。所有这些函数的语法都是被研究的字符向量作为第一个参数，正则表达式作为第二个，所有可能的其他值都跟随其后。这些函数的一致性就是为什么我们更喜欢用由Hadley Wickham (2010)开发的`stringr`包的原因。我们将在8.2节中介绍更多这个包的细节。表8.5展示了`stringr`包在`R`基础环境下的等价函数。
由于一般对多个字符串操作时调用`str_extract_all()`函数，所以其结果是一个`list`，`list`中每个元素对应于一个字符串的匹配结果。在上面的调用中我们输入的字符串是一个长度为1的字符向量，因此，函数返回是一个长度为1的`list`，对其我们用`unlist()`以获得更方便的呈现。为了与此进行比较，我们将在多字符串上调用这个函数。我们创建一个包含字符串`text`，`manipulation`和`basics`的向量。我们用`str_extract_all()`函数提取所有匹配模式`a`的实例：
```r
R> out <- str_extract_all(c("text", "manipulation", "basics"), "a")
R> out
[[1]]
character(0)
[[2]]
[1] "a" "a"
[[3]]
[1] "a"
```
这个函数返回了与输入向量等长的`list`，长度为3，`list`中各个元素对应了各个字符串的匹配结果。因为在第一个字符串里没有`a`，所以第一个元素是一个空的字符向量。第二个字符串包含两个`a`，第三个包含一个。
字符匹配默认是区分大小写的。因此，在正则表达式中大小写字符是不一样的。
```r
R> str_extract(example.obj, "small")
[1] "small"
small is contained in the example string while SMALL is not.
R> str_extract(example.obj, "SMALL")
[1] NA
```
所以，函数没有提取出匹配的值。我们可以通过附上函数`ignore.case()`来改变这种行为。
```r
R> str_extract(example.obj, ignore.case("SMALL"))
[1] "small"
```
我们没有必要一定用正则表达式。字符串可以是简单的字符序列。因此，我们也可以匹配字...
```r
R> unlist(str_extract_all(example.obj, "en"))
[1] "en" "en" "en" "en"
```
...或者是字母与空格的混合。
```r
R> str_extract(example.obj, "mall sent")
[1] "mall sent"
```
在这个例子中搜索模式`en`返回所有模式的实例，也就是在`sentence`里所有的匹配项，这个例子里出现了两次。有时我们不是简单地关心在字符串中找到匹配项，而是关注字符串特定的位置。我们可以通过两个简单的正则表达式附加参数来指定位置。正则表达式开始处的`^`符号注明了字符串的起始，`$`注明了结尾。因此，从下面的例子中提取2会返回2。
```r
R> str_extract(example.obj, "2")
[1] "2"
```
而从字符串的起始提取2就会失败。
```r
R> str_extract(example.obj, "ˆ2")
[1] NA
```
同样的，符号`$`注明了字符串结尾，也就是...
```r
R> unlist(str_extract_all(example.obj, "sentence$"))
character(0)
```
...返回了空字符向量，在`sentence`中没有找到匹配项。另外一个正则表达式附加参数是管道操作符，表示为`|`。这个字符被看做`or`操作符，所以函数将返回这个表达式在管道操作符之前和之后所有模式的匹配项。
```r
R> unlist(str_extract_all(example.obj, "tiny|sentence"))
[1] "sentence" "tiny" "sentence"
```

8.1.2 普适正则表达式
到目前为止，我们仅仅匹配了固定的表达式。但是正则表达式的能力来自于可以利用其写出更加灵活的、普适的查询条件。其中最为普适的是句号，它能够匹配任何字符。
```r
R> str_extract(example.obj, "sm.ll")
[1] "small
```
正则表达式另外一个强大的普适性就是字符集，用括号`[]`括起来。字符集表示任何在其中的字符都将被匹配。
```r
R> str_extract(example.obj, "sm[abc]ll")
[1] "small"
```
由于`a`是字符集`[abc]`的一部分，所以上面的代码提取了单词`small`。另外一种指明字符集元素的方法是使用符号`-`来指明字符范围。



==============================================================================


```r
R> str_extract(example.obj, "A.+sentence")
[1] "A small sentence. - 2. Another tiny sentence"
```
`R`提供贪婪的量化。这意味着这段程序尝试从前面的字符中提取最大可能的序列。由于`.`匹配任意字符，所以这个函数返回了`sentence`前面最大可能的序列。我们可以通过给表达式增加一个`?`来改变这种行为，从而指明我们仅仅要查找`sentence`前面最短的可能序列。符号`?`意味着前面的元素是可选项，而且将最多被匹配一次（参加表8.2）。
```r
R> str_extract(example.obj, "A.+?sentence")
[1] "A small sentence"
```
我们不仅可以提供量词来指明单字符，也可以用括号把一组字符括起来。
```r
R> unlist(str_extract_all(example.obj, "(.en){1,5}"))
[1] "senten" "senten"
```
在这种情况下，我们要求函数返回一个首字符可以是任意字符、第二个和第三个字符必须是`e`和`n`的字符序列。我们要求函数对所有至少出现过一次、但最多五次这个序列的实例进行返回。符合这个要求的最长可能序列将有`3 * 5 = 15`个字符，而且第二个和第三个字符是`e`和`n`。在下一个代码片段中我们去掉了括号，这个函数将匹配所有从`e`到`n`的序列，`n`最少出现一次最多五次。考虑一下前后两次结果的区别：
```r
R> unlist(str_extract_all(example.obj, ".en{1,5}"))
[1] "sen" "ten" "sen" "ten"
```

表8.3 选择有特殊意义的字符
元字符 | 描述 
---|----
\w | 任意一个字母或数字或下划线: [[:alnum:]_]
\W | 匹配所有的字母、数字、下划线以外的字符: [ˆ[:alnum:]_]
\s | 包括空格、制表符、换页符等空白字符的其中任意一个: [[:blank:]]
\S | 匹配所有非空白字符: [ˆ[:blank:]]
\d | 任意一个数字，0~9 中的任意一个: [[:digit:]]
\D | 匹配所有的非数字字符: [ˆ[:digit:]]
\b | 匹配一个单词边界，也就是单词和空格之间的位置，不匹配任何字符
\B | 匹配非单词边界，即左右两边都是 "\w" 范围或者左右两边都不是 "\w" 范围时的字符缝隙
\< | 匹配单词起始
\> | 匹配单词结束

到目前为止，我们碰到了很多在正则表达式里有特殊意义的字符。他们被称作元字符。为了从字面意义上去匹配这些字符，可以在他们前面加上两个反斜线。为了从一直在使用的例子中提取字面意义上的这些字符，我们要这么写
```r
R> unlist(str_extract_all(example.obj, "\\."))
[1] "." "." "." "."
```
字符前的双反斜线被解释成字面意义上的单反斜线。在正则表达式中输入单反斜线将被解释成引入一个转义序列。对于网络抓取任务来说，这些转义序列非常普遍，你应该熟悉他们。最常见的是`\m`和`\t`，分别表示新的一行和制表符。举个栗子，`a\n\n\na`被解释成`a`，新的三行，再一个`a`。如果我们希望整个正则表达式被按字面意义来解释，我们有一个比在所有元字符前面加反斜线更好的办法。我们可以用`fixed()`把表达式括起来以达到按字面意义解释元字符的目的。
```r
R> unlist(str_extract_all(example.obj, fixed(".")))
[1] "." "." "." "."
```
大多数元字符在字符集中都会失去其特殊意义。举个栗子，在字符集中一个圆点字符仅会匹配一个字面意义上的圆点字符。对于这条规则有两个例外，就是`^`和`-`。把前者放到一个字符集的开始将会匹配字符集内容的补集，后者在字符集中则会描述字符的范围。要改变这种行为，需要将`-`放到字符集的开始或者结尾，在这种情况下，它会被按字面意义解释。
关于正则表达式我们这里最后想介绍的是一些被赋到特殊字符集上的快捷键，表8.3说明了可用快捷键的概况。
考虑一下`\w`字符，这个字符匹配我们例子中的任意字，就像这样...
```r
R> unlist(str_extract_all(example.obj, "\\w+"))
[1] "1" "A" "small" "sentence" "2" "Another"
[7] "tiny" "sentence"
```
...提取了被空格或标点分隔的每一个字。注意到`\w`相当于`[[:alnum:]_] `因此前面的数字被解释成了单字。考虑一下`\>`，`\<`和`\b`，利用他们我们可以在匹配位置方面更加精确。想象一下我们要从我们的例子中提取所有位于单词末尾的`e`。为了达到这个效果，我们可以使用下面两个表达式：
```r
R> unlist(str_extract_all(example.obj, "e\\>"))
[1] "e" "e"
R> unlist(str_extract_all(example.obj, "e\\b"))
[1] "e" "e"
```
这个查询从`sentence`这个词的边界处提取出了两个`e`（因为`example.obj`中有两个`sentence`）。最后，我们甚至可以匹配一个已经被正则表达式匹配的序列，这叫做反向引用。假设说，我们在我们的例子中查找第一个字母——不管是什么——并用其匹配其他的实例。为了达到这个效果，我们用括号把这些元素括起来——举个栗子，`([[:alpha:]])`，用`\1`引用它。
```r
R> str_extract(example.obj, "([[:alpha:]]).+?\\1")
[1] "A small sentence. - 2. A"
```
在这个例子中，这个字母是`A`。这个函数返回了匹配项直到下一个`A`之间的字符。为了让例子再复杂点，我们现在查找一个不包含`a`的小写单词直到后面再出现这个单词间的字符。
```r
R> str_extract(example.obj, "(\\<[b-z]+\\>).+?\\1")
[1] "sentence. - 2. Another tiny sentence"
```
我们用的表达式是`(\\<[b-z]+\\>).+?\\1`。首先考虑`[b-z]+`这个部分。


===============================================================================

...`str_extract_all()`提取了所有匹配项。
```r
R> str_extract_all(example.obj, "[[:digit:]]")
[[1]]
[1] "1" "2"
```

Table 8.4 `stringr`包里的函数
函数 | 描述 | 输出
-----|------|-------
使用正则表达式的函数| |
`str_extract()` | 提取匹配模式的第一个字符串 | 字符向量
`str_extract_all()` | 提取匹配模式的所有字符串 | 字符向量列表
`str_locate()` | 返回第一个匹配项的位置 | 起始/结束位置矩阵
`str_locate_all()` | 返回所有匹配项的位置 | 矩阵列表
`str_replace()` | 替换第一个匹配项 | 字符向量
`str_replace_all()` | 替换所有匹配项 | 字符向量
`str_split()` | 在该模式下分割字符串 | 字符向量列表
`str_split_fixed()` | 在该模式下将字符串分割成固定数量的片段 | 字符向量矩阵
`str_detect()` | 在字符串中探测模式 | 布尔值向量
`str_count()` | 对字符串中模式计数 | 数值向量
更多的函数 | |
`str_sub()` | 按位置提取字符串 | 字符向量
`str_dup()` | 复制字符串 | 字符向量
`str_length()` | 返回字符串长度 | 数值向量
`str_pad()` | 补充字符串长度 | 字符向量
`str_trim()` | 去掉字符串填充物（空格等） | 字符向量
`str_c()`| 连接字符串 | 字符向量

我们已经指出了函数输出的不同之处。前面的例子返回了一个字符向量，而后面的则返回了列表。表8.4给出了本章节将会介绍的不同函数的概况。第二列对这些函数进行了简短地描述，第三列指明了返回值格式。如果相比提取，我们更关心给定字符串中匹配项的位置，我们要使用`str_locate()`和`str_locate_all()`函数。
```r
R> str_locate(example.obj, "tiny")
start end
[1,] 35 38
```
这个函数输出了第一个匹配项的起始和结束位置的矩阵，在这个例子中分别是第35和第38个字符。我们可以利用位置信息和`str_sub()`函数来提取子串。
```r
R> str_sub(example.obj, start = 35, end = 38)
[1] "tiny"
```
这里我们提取了已经知道是`tiny`单词的第35到第38个字符。可能我们更经常做的是替换给定子串，通常可以利用赋值操作符来完成。
```r
R> str_sub(example.obj, 35, 38) <- "huge"
R> example.obj
[1] "1. A small sentence. - 2. Another huge sentence."
```
`str_replace()`和`str_replace_all()`更加常用在替换过程上。
```r
R> str_replace(example.obj, pattern = "huge", replacement = "giant")
[1] "1. A small sentence. - 2. Another giant sentence."
```
我们可能会关心将字符串分割成几个较小的子串。举个最简单的栗子，我们定义一个分割符，比如说`-`。
```r
R> unlist(str_split(example.obj, "-"))
[1] "1. A small sentence. " " 2. Another huge sentence."
```
我们可以设定字符串被分割的份数。如果我们想按照每个空格分割字符串，但不希望结果超过5个，我们可以写成
```r
R> as.character(str_split_fixed(example.obj, "[[:blank:]]", 5))
[1] "1." "A"
[3] "small" "sentence."
[5] "- 2. Another huge sentence."
```
我们可以同时在多个字符串上调用这个函数。假设有一个由多个字符串组成的字符向量：
```r
R> char.vec <- c("this", "and this", "and that")
```
我们首先可以在这个字符向量里检查一下特定模式是否出现。假设我们想知道模式`this`是否出现在给定向量的元素中。我们用`str_detect()`来完成这个。
```r
R> str_detect(char.vec, "this")
[1] TRUE TRUE FALSE
```
此外，我们可能想知道这个特定的词出现的频率...
```r
R> str_count(char.vec, "this")
[1] 1 1 0
```
...或者想知道各个元素有多少个字
```r
R> str_count(char.vec, "\\w+")
[1] 1 2 2
```
我们可以复制字符串...
```r
R> dup.obj <- str_dup(char.vec, 3)
R> dup.obj
[1] "thisthisthis" "and thisand thisand this"
[3] "and thatand thatand that"
```
...或者数一下给定字符串的字符数。
```r
R> length.char.vec <- str_length(char.vec)
R> length.char.vec
[1] 4 8 8
```
在操作网络数据时`str_pad()`和`str_trim()`两个函数非常重要，他们被用来在字符串边界上增加字符或者删除空格。

=======================================================================

尽管已经有很多好使的函数，但当缺少一个特殊的必要函数时总会导致一些问题。其中可能就有下面说的问题。想象我们必须在一个字符向量中检查不止一个模式，并且想得到一个指明符合匹配条件行的逻辑向量，或者一个列出所有符合匹配条件行号的索引。为了检查模式，我们知道可以用`grep()`，`grepl()`或者`str_detect()`。因为`grep()`提供了一个返回匹配文本或者行索引向量的开关，所以我们会尝试用`grep()`构建一个解决方案。我们首先下载一个*Simpsons*剧集的测试集，并把它保存在本地文件`episodes.Rdata`中。
```r
R> library(XML)
R> # download file
R> if(!file.exists("listOfSimpsonsEpisodes.html")){
link <- "http://en.wikipedia.org/wiki/List_of_The_Simpsons_episodes"
download.file(link, "listOfSimpsonsEpisodes.html", mode="wb")
}
R> # getting the table
R> tables <- readHTMLTable("listOfSimpsonsEpisodes.html",
header=T, stringsAsFactors=F)
R> tmpcols <- names(tables[[3]])
R> for(i in 3:20){
tmpcols <- intersect(tmpcols, names(tables[[i]]))
}
R> episodes <- NULL
R> for(i in 3:20){
episodes <- rbind(episodes[,tmpcols],tables[[i]][,tmpcols])
}
R> for(i in 1:dim(episodes)[2]){
Encoding(episodes[,i]) <- "UTF-8"
}
R> names(episodes) <- c("pnr", "nr", "title", "directedby",
"Writtenby", "airdate", "productioncode")
R> save(episodes,file="episodes.Rdata")
```
让我们载入包含所有*Simpsons*剧集的表格。
```r
R> load("episodes.Rdata")
```
就像你下面看到的，可以简单地在同一个问题——哪一集标题提到了Homer——的不同答案之间切换，使用`grep()`和`grepl()`并且让参数`value = TRUE`。这个简单的改变让这些函数有时候特别有用，比如我们最后可能需要一个索引或逻辑向量，但是我们可以用这个`value`参数去检查一下使用的模式是不是真的起作用了。
```r
R> grep("Homer",episodes$title[1:10], value=T)
[1] "Homer's Odyssey" "Homer's Night Out"
R> grepl("Homer",episodes$title[1:10])
[1] FALSE FALSE TRUE FALSE FALSE FALSE FALSE FALSE FALSE TRUE
```
但是，这个参数选项不能同时匹配多个模式。假设我们想知道这些剧集是否在标题里同时提到了Homer和Lisa。标准的作法是分别给匹配的模式做一个逻辑向量，然后合二为一，`TRUE`代表所有模式都找到了。
```r
R> iffer1 <- grepl("Homer",episodes$title)
R> iffer2 <- grepl("Lisa",episodes$title)
R> iffer <- iffer1 & iffer2
R> episodes$title[iffer]
[1] "Homer vs. Lisa and the 8th Commandment"
```
尽管在两个模式的时候这种办法看起来还可以接受，但当模式的数量增加或需要反复做这项工作时它就变得越来越麻烦。因此我们将基于`grep()`创造一个新的函数。
```r
R> grepall <- function(pattern, x,
ignore.case = FALSE, perl = FALSE,
fixed = FALSE, useBytes = FALSE,
value=FALSE, logic=FALSE){
214 AUTOMATED DATA COLLECTION WITH R
# error and exception handling
if(length(pattern)==0 | length(x)==0){
warning("Length of pattern or data equals zero.")
return(NULL)
}
# apply grepl() and all()
indicies <- sapply(pattern, grepl, x,
ignore.case, perl, fixed, useBytes)
index <- apply(indicies, 1, all)
# indexation and return of results
if(logic==T) return(index)
if(value==F) return((1:length(x))[index])
if(value==T) return(x[index])
}
R> grepall(c("Lisa","Homer"), episodes$title)
[1] 26
R> grepall(c("Lisa","Homer"), episodes$title, value=T)
[1] "Homer vs. Lisa and the 8th Commandment"
```
`grepall()`函数的思路是我们需要重复一系列的模式搜索——就像前面代码片段中两个单独的模式搜索。重复一系列的事可以使用循环或者使用更高效（？）的apply函数族。因此，我们首先调用`grepl()`函数以获得指明哪行找到模式的逻辑向量。由于我们的输入是向量而且希望输出是一个矩阵所以我们使用了`sapply()`函数。我们得到的矩阵的列指的是用来搜索的不同模式，行指的是独立的字符串。为了确定在某个特定的行中找到了所有的模式我们使用了第二个apply函数——这次我们用`apply()`函数，因为输入是矩阵——当一行里所有值为真时`all()`函数返回`TRUE`，但凡有一个为假则返回`FALSE`，`value`参数的选项取决于我们想要返回一个匹配所有模式的索引向量还是要返回对应的文本。为了得到逻辑向量我们可以用`logic`参数。除了向多搜索项提供像`grep()`和`grepl()`一样的函数功能外，所有其他的参数像`ignore.case`、`perl`、`fixed`或者`useBytes`都被直接传给了第一步的sapply，所以新函数同样具有这些功能。

8.3 字符编码
当处理网络数据时——特别是非英语的数据——我们很快就会碰到编码问题。没有处理这些问题的简单办法，通常来说，字符编码是关于如何将二进制编码转换成人类可读的字符。举个栗子，`01100100`表示的是一个`d`。世界上有许多语言，也有许多特殊字符，像`a`、`ø`、` c`等。
