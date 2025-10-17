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

# Create .env file from environment variables at runtime
# This allows secure deployment without committing secrets to git
RUN echo "# Environment variables for MMCD Dashboard" > /srv/shiny-server/.env \
    && echo "# Set these via Docker environment variables or AWS secrets" >> /srv/shiny-server/.env \
    && echo "DB_HOST=${DB_HOST:-localhost}" >> /srv/shiny-server/.env \
    && echo "DB_NAME=${DB_NAME:-mmcd}" >> /srv/shiny-server/.env \
    && echo "DB_USER=${DB_USER:-shiny}" >> /srv/shiny-server/.env \
    && echo "DB_PASSWORD=${DB_PASSWORD:-}" >> /srv/shiny-server/.env

# Set ownership
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
