FROM r-base
COPY pkg /pkg
RUN apt-get update \
    && apt-get install -y libcurl4-openssl-dev libssl-dev
RUN R -e "\
install.packages(c('dplyr', 'igraph', 'luzlogr', 'jsonlite', 'Rserve', 'R6', \
                         'yaml', 'swagger', 'mime', 'curl', 'testthat', 'devtools'), \
                         type = 'source', contriburl = 'file:///pkg');\
devtools::install_local(path = '/pkg/RestRserve');"
EXPOSE 8000
CMD Rscript /home/rstudio/RestRserve/current_app_snapshot.R
