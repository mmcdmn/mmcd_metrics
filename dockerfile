FROM rocker/shiny:latest

# Install system dependencies for geospatial and database packages + nginx
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev libpq-dev \
    libgdal-dev libudunits2-dev libproj-dev \
    libfontconfig1-dev libfreetype-dev libpng-dev \
    libharfbuzz-dev libfribidi-dev gfortran cmake gdebi-core \
    libgl1-mesa-dev libglu1-mesa libx11-dev libxt-dev libxft-dev \
    libtiff-dev libjpeg-dev libgeos-dev libgmp-dev libgsl-dev \
    libv8-dev libpoppler-cpp-dev libmagick++-dev \
    nginx

# Install libsodium for plumber API
RUN apt-get install -y libsodium-dev

# Install required R packages
RUN R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'shinyjs', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble' \
), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Install additional visualization packages for SF mapping
RUN R -e "install.packages(c('viridis', 'gridExtra', 'ggspatial', 'rosm', 'prettymapr'), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Install PooledInfRate for MLE calculations (trap_survillance_test app)
RUN R -e "install.packages('remotes', lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/'); remotes::install_github('CDCgov/PooledInfRate')"

# Install tidyr separately to ensure it's available
RUN R -e "install.packages(c('tidyr'), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Install pool package for database connection pooling
RUN R -e "install.packages('pool', lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Install plumber for API server
RUN R -e "install.packages('plumber', lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Copy app files and config to correct locations
COPY apps /srv/shiny-server/apps
COPY shared /srv/shiny-server/shared
COPY tests /srv/shiny-server/tests
COPY api /srv/api
COPY index.html /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY nginx.conf /etc/nginx/nginx.conf
COPY startup.sh /startup.sh

# Make startup script executable, set ownership, create nginx dirs
RUN chmod +x /startup.sh && \
    chown -R shiny:shiny /srv/shiny-server && \
    mkdir -p /run /var/log/nginx && \
    rm -f /etc/nginx/sites-enabled/default

EXPOSE 3838
CMD ["/startup.sh"]
