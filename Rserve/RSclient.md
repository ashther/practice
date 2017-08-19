### 利用RSclient测试Rserve服务
```r
library(RSclient)

con <- RS.connect(host = '192.168.1.201', port = 6311)

RS.eval(con, rnorm(10))
RS.eval(con, ls())
RS.eval(con, (.packages()))

RS.close(con)
```
