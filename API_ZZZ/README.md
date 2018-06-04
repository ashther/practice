### 离线安装R包的准备工作
```R
getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                                which = c("Depends", "Imports"), recursive = TRUE)
  )
  packages <- union(packs, packages)
  packages[!packages %in% unname(installed.packages()[
    !is.na(installed.packages()[, 'Priority']), 
    'Package'
    ])]
}

pkg_to_install_cran <- c('dplyr', 'igraph', 'luzlogr', 'jsonlite', 'Rserve', 'R6', 
                         'yaml', 'swagger', 'mime', 'curl', 'testthat', 'devtools')
pkg_to_install_gh <- c('dselivanov/RestRserve')

pkg_to_download_cran <- getPackages(pkg_to_install_cran)
download.packages(pkg_to_download_cran, '~/docker/pkg')

purrr::walk(
  pkg_to_install_gh, 
  ~ system(
    sprintf('git clone https://github.com/%s %s', 
            .x, 
            file.path('~/docker/pkg', unlist(strsplit(pkg_to_install_gh, '/'))[2]))
  )
)

tools::write_PACKAGES('~/docker/pkg/', verbose = TRUE)
```

### 容器启动
由于需要RestRserve以前台模式启动并保持，所以不使用`restrserve_start`的启动方式，而是在原定义接口函数的脚本末添加：
```R
RestRserveApp$run(http_port = "8000", 
                  encoding = 'utf8', 
                  port = '6311', 
                  remote = 'enable')
```

Docker启动，api_*.R为定义了各接口函数的脚本，`~/docker/RestRserve`目录下包括接口函数脚本和所需要的rds文件
```bash
$ sudo docker run --rm -e TZ=Asia/Shanghai -p 8000:8000 -v $HOME/docker/RestRserve:/home/rstudio/ -d r-api 
```

注意修改RestRserve.R脚本起始的`HOME_PATH`为`/home/rstudio`
