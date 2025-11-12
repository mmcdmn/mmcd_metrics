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

# Install QGIS Server dependencies
RUN apt-get update && apt-get install -y \
    qgis-server apache2 libapache2-mod-fcgid \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Enable Apache modules for QGIS Server
RUN a2enmod fcgid rewrite proxy proxy_http

# Configure Apache for QGIS Server
RUN echo '<VirtualHost *:80>' > /etc/apache2/sites-available/qgis.conf && \
    echo '    ServerAdmin webmaster@localhost' >> /etc/apache2/sites-available/qgis.conf && \
    echo '    DocumentRoot /var/www/html' >> /etc/apache2/sites-available/qgis.conf && \
    echo '' >> /etc/apache2/sites-available/qgis.conf && \
    echo '    # QGIS Server FastCGI' >> /etc/apache2/sites-available/qgis.conf && \
    echo '    ScriptAlias /qgis/ /usr/lib/cgi-bin/qgis_mapserv.fcgi' >> /etc/apache2/sites-available/qgis.conf && \
    echo '    <Location /qgis/>' >> /etc/apache2/sites-available/qgis.conf && \
    echo '        SetHandler fcgid-script' >> /etc/apache2/sites-available/qgis.conf && \
    echo '        Options +ExecCGI' >> /etc/apache2/sites-available/qgis.conf && \
    echo '        Require all granted' >> /etc/apache2/sites-available/qgis.conf && \
    echo '    </Location>' >> /etc/apache2/sites-available/qgis.conf && \
    echo '' >> /etc/apache2/sites-available/qgis.conf && \
    echo '    # Proxy Shiny Server' >> /etc/apache2/sites-available/qgis.conf && \
    echo '    ProxyPass /shiny/ http://localhost:3838/' >> /etc/apache2/sites-available/qgis.conf && \
    echo '    ProxyPassReverse /shiny/ http://localhost:3838/' >> /etc/apache2/sites-available/qgis.conf && \
    echo '</VirtualHost>' >> /etc/apache2/sites-available/qgis.conf && \
    a2ensite qgis && \
    a2dissite 000-default

# Create QGIS project directory
RUN mkdir -p /qgis/projects && chmod 755 /qgis/projects

# Set QGIS Server environment variables
ENV QGIS_SERVER_LOG_LEVEL=0 \
    QGIS_SERVER_LOG_STDERR=1 \
    QGIS_PROJECT_FILE=/qgis/projects/default.qgs

# Copy app files and config to correct locations
COPY apps /srv/shiny-server/apps
COPY shared /srv/shiny-server/shared
COPY index.html /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY .env /srv/shiny-server/.env
COPY startup.sh /startup.sh

# Make startup script executable and set ownership
RUN chmod +x /startup.sh && chown -R shiny:shiny /srv/shiny-server

# Expose ports: 3838 for Shiny (direct), 80 for Apache (QGIS + Shiny proxy)
EXPOSE 3838 80

# Start both Apache and Shiny Server
CMD ["/bin/bash", "-c", "service apache2 start && /startup.sh"]
