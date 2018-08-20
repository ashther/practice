FROM rocker/r-ver
COPY pkg /pkg
RUN apt-get update \ 
    && apt-get install -y libcurl4-openssl-dev libmariadbclient-dev
RUN R -e "\
install.packages(c('plumber', 'futile.logger', 'uuid', 'pool', 'RMySQL',\
                   'dplyr', 'tidyr', 'memoise'), \
                 type = 'source', contriburl = 'file:///pkg');"
EXPOSE 8001
CMD Rscript /home/rstudio/plumber.R
