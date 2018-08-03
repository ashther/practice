FROM rocker/r-ver
COPY pkg /pkg
# change to 163 source, for some reason, r-base may be removed after apt-get update,
# r-base and r-base-dev must be installed
# RUN echo "deb http://mirrors.163.com/debian/ strech main" > /etc/apt/sources.list \
#     && echo "deb-src http://mirrors.163.com/debian/ strech main" >> /etc/apt/sources.list 
RUN apt-get update \ 
    && apt-get install -y libcurl4-openssl-dev libssl-dev libmariadbclient-dev
RUN R -e "\
install.packages(c('pool', 'RMySQL', 'jsonlite', 'purrr', 'Rserve', 'R6', \
                    'uuid', 'base64enc', 'yaml', 'swagger', 'mime', 'curl', \
                    'testthat', 'devtools'), \
                    type = 'source', contriburl = 'file:///pkg');\
devtools::install_local(path = '/pkg/RestRserve');"
EXPOSE 8002
CMD Rscript /home/rstudio/RestRserve.R --vanilla
