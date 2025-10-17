FROM rocker/shiny:latest

# Install system dependencies for geospatial and database packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev libpq-dev \
    libgdal-dev libudunits2-dev libproj-dev \
    libfontconfig1-dev libfreetype-dev libpng-dev \
    libharfbuzz-dev libfribidi-dev gfortran cmake gdebi-core \
    libgl1-mesa-dev libglu1-mesa libx11-dev libxt-dev libxft-dev \
    libtiff-dev libjpeg-dev libgeos-dev libgmp-dev libgsl-dev \
    libv8-dev libpoppler-cpp-dev libmagick++-dev

# Install required R packages
RUN R -e "install.packages(c( \
  'shiny', 'shinydashboard', 'shinyWidgets', 'DBI', 'RPostgreSQL', 'RPostgres', \
  'dplyr', 'ggplot2', 'lubridate', 'scales', 'stringr', 'DT', \
  'plotrix', 'dtplyr', 'vroom', 'tidyverse', \
  'classInt', 's2', \
  'sf', 'leaflet', 'terra', 'textshaping', 'units', 'raster', \
  'plotly', 'purrr', 'tibble' \
), lib='/usr/local/lib/R/site-library', repos='https://cran.rstudio.com/')"

# Copy app files and config to correct locations
COPY apps /srv/shiny-server/apps
COPY index.html /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY startup.sh /startup.sh

# Make startup script executable and set ownership
RUN chmod +x /startup.sh && chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
CMD ["/startup.sh"]
