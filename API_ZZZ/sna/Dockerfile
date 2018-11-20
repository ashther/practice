FROM rocker/r-ver
RUN R -e "\
install.packages(c('plumber', 'jsonlite', 'futile.logger', 'uuid', 'igraph', 'dplyr', 'tibble', 'purrr', 'memoise'));"
EXPOSE 8003
CMD Rscript /home/rstudio/plumber.R
