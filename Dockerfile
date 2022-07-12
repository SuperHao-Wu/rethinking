FROM rocker/rstudio:latest
RUN apt-get update \
  && apt-get -y install libxt6 libglpk-dev  libcairo2-dev libxml2-dev libxml2 libcurl4-openssl-dev libssl-dev libv8-dev\
  && apt-get clean
COPY install.R /home/install.R
# COPY Renviron /home/.Renviron
RUN Rscript /home/install.R

