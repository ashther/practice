FROM rocker/r-ver
RUN apt-get update && apt-get install -y libssl-dev
RUN R -e "\
install.packages(c('plumber', 'jsonlite', 'futile.logger', 'uuid', 'igraph', 'dplyr', 'tibble', 'purrr', 'memoise', 'jose'), repos = 'http://mirror.lzu.edu.cn/CRAN/');"
EXPOSE 8003
CMD Rscript /home/rstudio/plumber.R
