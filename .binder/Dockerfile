FROM rocker/binder:3.6.0
LABEL maintainer='Michael D. Sumner'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/Trackage/traipse/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-08-23/')); devtools::install_deps()"

RUN rm DESCRIPTION.1
