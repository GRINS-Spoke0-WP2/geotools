FROM rocker/r-ver:4.5.1

COPY deploy-service.R /deploy-service.R

RUN apt update && \
    apt install --no-install-recommends -y vim libsodium-dev libcurl4-openssl-dev

# install geomatching R package from GitHub
RUN R -e "install.packages(c('devtools', 'plumber', 'jsonlite', 'httr', 'future', 'dplyr'))" && \
    R -e "devtools::install_github('GRINS-Spoke0-WP2/geotools')"

EXPOSE 8000

# run the plumber API
CMD [ "R", "-e", "pr <- plumber::plumb('/deploy-service.R'); pr$run(host='0.0.0.0', port=8000)" ]