FROM rocker/r-ver
COPY pkg /pkg
RUN apt-get update \ 
    && apt-get install -y libcurl4-openssl-dev libmariadbclient-dev
RUN R -e "\
install.packages(c('plumber', 'futile.logger', 'uuid', 'pool', 'RMySQL',\
                   'webutils', 'purrr'), \
                 type = 'source', contriburl = 'file:///pkg');"
# devtools::install_local(path = '/pkg/RestRserve');"
EXPOSE 8002
CMD Rscript /home/rstudio/plumber.R
