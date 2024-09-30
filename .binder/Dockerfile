FROM rocker/binder:4.3.2
LABEL maintainer='Infectious Disease Modeling Hubs'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/hubverse-org/hubEnsemblesManuscript/raw/master/DESCRIPTION && R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('devtools'); devtools::install(dependencies = TRUE)"

RUN rm DESCRIPTION.1; exit 0
