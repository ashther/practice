1### 编译R语言环境
#### 安装依赖库
```bash
yum install -y readline-devel      
# 解决错误“ --with-readline=yes (default) and headers/libs are not available”
yum install -y libXt-devel         
# 解决错误“–with-x=yes (default) and X11 headers/libs are not available”
yum install -y texinfo
yum install -y texlive-pdftex-doc

yum install -y libcurl-devel
yum install -y libxml2-devel
yum install -y openssl-devel
yum install -y udunits2
yum install -y udunits2-devel
```

#### 下载R.3.4.1.tar
```bash
wget https://cloud.r-project.org/src/base/R-3/R-3.4.1.tar.gz
```

#### 解压R.3.4.1.tar
```bash
tar -xvf R.3.4.1.tar
cd R.3.4.1.tar
./configure --enable-R-shlib
make
make install
```

#### 测试R语言环境是否安装成功
```bash
R -e 'version'
```

#### 安装R相关包
```bash
sudo ./rPckInstall.R	
#以root权限进行安装，同目录下rPckDepend.json中指明需要安装的包
```

### 启动Rserve服务
```bash
R CMD Rserve	
# 远程连接时需要参数`--RS-enable-remote`
# 遇到Rserve: not found错误时，可能是由于Rserve安装位置与路径不一致导致，可以在/usr/local/lib64/R/bin/创建软连接：ln -s /usr/lib64/R/library/Rserve/libs/Rserve Rserve
# 可以在/etc/rc.local中加入su slj -c "/usr/local/lib64/R/bin/R CMD Rserve --RS-conf /home/slj/Rserv_conf/Rserv.conf"
 以保证Rserve服务在服务器启动时自动开启，其中以slj用户启动的目的是保证相关脚本运行环境与其中代码逻辑保持一致，如以root身份启动Rserve，则需要在/root下增加/log、/sna、/topic_model、/topic_model/data等目录及基础数据文件以供脚本使用
```

### Java访问R语言代码

#### Jar包
```bash
#路径可能不一样，要看R的library路径
/usr/local/lib64/R/library/Rserve/java/REngine.jar 
/usr/local/lib64/R/library/Rserve/java/Rserve.jar
```

#### 读取R语言脚本
参考https://github.com/ashther/practice/tree/master/Rserve

### 常见错误
* 载入jiebaR时可能会报`Error in unzip(file.path(jiebapath, "dict", "jieba.dict.zip"), exdir = file.path(jiebapath,  : cannot open file '/usr/local/lib64/R/library/jiebaRD/dict/jieba.dict.utf8': Permission denied`，这是由于jiebaR的部分zip格式字典文件没有解压，需要将zip文件解压后再调用该包；