FROM rocker/binder:4.3.2
LABEL maintainer='Unknown'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}



RUN wget https://github.com/Infectious-Disease-Modeling-Hubs/hubEnsemblesManuscript/raw/binder/DESCRIPTION && R -e "options(repos = list(CRAN = 'https://packagemanager.posit.co/cran/__linux__/focal/2024-02-23/')); install.packages('pak'); pak::local_install()"

RUN rm DESCRIPTION.1; exit 0
