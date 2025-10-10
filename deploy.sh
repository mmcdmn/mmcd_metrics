#!/bin/bash
# MMCD Shiny Applications Deployment Script

echo "🚀 Starting MMCD Shiny Applications Deployment..."

# Update system packages
echo "📦 Updating system packages..."
sudo apt update

# Install R and required system dependencies
echo "📊 Installing R and system dependencies..."
sudo apt install -y r-base r-base-dev libcurl4-openssl-dev libssl-dev libxml2-dev \
    libpq-dev libgdal-dev libudunits2-dev libproj-dev gdebi-core nginx

# Install Shiny Server
echo "🌐 Installing Shiny Server..."
if ! command -v shiny-server &> /dev/null; then
    wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb
    sudo gdebi -n shiny-server-1.5.22.1017-amd64.deb
    rm shiny-server-1.5.22.1017-amd64.deb
fi

# Install required R packages
echo "📚 Installing R packages..."
sudo R -e "
packages <- c('shiny', 'DBI', 'RPostgres', 'dplyr', 'ggplot2', 'lubridate', 
              'scales', 'leaflet', 'sf', 'stringr', 'DT', 'vroom', 'tidyverse',
              'shinydashboard', 'shinyWidgets', 'plotrix')
install.packages(packages, repos='https://cran.rstudio.com/')
"

# Create necessary directories
echo "📁 Creating directories..."
sudo mkdir -p /var/log/shiny-server
sudo mkdir -p /srv/shiny-server
sudo mkdir -p /etc/shiny-server

# Copy applications to Shiny Server directory
echo "📋 Copying applications..."
sudo cp -r /home/alexpdyak32/Documents/mmcd/apps/* /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server

# Copy configuration
echo "⚙️ Setting up configuration..."
sudo cp /home/alexpdyak32/Documents/mmcd/shiny-server.conf /etc/shiny-server/shiny-server.conf

# Configure Nginx reverse proxy
echo "🔄 Configuring Nginx..."
sudo tee /etc/nginx/sites-available/mmcd-apps > /dev/null <<EOF
server {
    listen 80;
    server_name your-domain.com;  # Replace with your domain
    
    # Main landing page
    location / {
        root /srv/shiny-server;
        index index.html;
        try_files \$uri \$uri/ =404;
    }
    
    # Shiny applications
    location /mosquito-monitoring/ {
        proxy_pass http://127.0.0.1:3838/mosquito-monitoring/;
        proxy_redirect http://127.0.0.1:3838/ \$scheme://\$host/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade \$http_upgrade;
        proxy_set_header Connection \$connection_upgrade;
        proxy_read_timeout 20d;
        proxy_buffering off;
    }
    
    location /suco-analysis/ {
        proxy_pass http://127.0.0.1:3838/suco-analysis/;
        proxy_redirect http://127.0.0.1:3838/ \$scheme://\$host/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade \$http_upgrade;
        proxy_set_header Connection \$connection_upgrade;
        proxy_read_timeout 20d;
        proxy_buffering off;
    }
    
    location /treatment-analysis/ {
        proxy_pass http://127.0.0.1:3838/treatment-analysis/;
        proxy_redirect http://127.0.0.1:3838/ \$scheme://\$host/;
        proxy_http_version 1.1;
        proxy_set_header Upgrade \$http_upgrade;
        proxy_set_header Connection \$connection_upgrade;
        proxy_read_timeout 20d;
        proxy_buffering off;
    }
}

map \$http_upgrade \$connection_upgrade {
    default upgrade;
    ''      close;
}
EOF

# Enable the site
sudo ln -sf /etc/nginx/sites-available/mmcd-apps /etc/nginx/sites-enabled/
sudo rm -f /etc/nginx/sites-enabled/default

# Test Nginx configuration
sudo nginx -t

# Start and enable services
echo "🔧 Starting services..."
sudo systemctl restart shiny-server
sudo systemctl enable shiny-server
sudo systemctl restart nginx
sudo systemctl enable nginx

# Configure firewall
echo "🔥 Configuring firewall..."
sudo ufw allow 22/tcp
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw allow 3838/tcp
sudo ufw --force enable

echo "✅ Deployment complete!"
echo ""
echo "🌐 Your applications are now available at:"
echo "   Main Dashboard: http://your-server-ip/"
echo "   Mosquito Monitoring: http://your-server-ip/mosquito-monitoring/"
echo "   SUCO Analysis: http://your-server-ip/suco-analysis/"
echo "   Treatment Analysis: http://your-server-ip/treatment-analysis/"
echo ""
echo "📋 Next steps:"
echo "   1. Replace 'your-domain.com' in /etc/nginx/sites-available/mmcd-apps with your actual domain"
echo "   2. Configure SSL certificates (recommended)"
echo "   3. Set up monitoring and backups"
