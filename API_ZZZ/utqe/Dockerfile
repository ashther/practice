FROM r-base
COPY pkg /pkg
RUN apt-get update \
    && apt-get install -y libcurl4-openssl-dev libssl-dev libmariadbclient-dev
RUN R -e "\
install.packages(c('pool', 'RMySQL', 'jsonlite', 'purrr', 'Rserve', 'R6', \
                    'uuid', 'base64enc', 'yaml', 'swagger', 'mime', 'curl', \
                    'testthat', 'devtools'), \
                    type = 'source', contriburl = 'file:///pkg');\
devtools::install_local(path = '/pkg/RestRserve');"
EXPOSE 8002
CMD Rscript /home/rstudio/RestRserve.R
