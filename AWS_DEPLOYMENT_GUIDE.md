# MMCD Metrics Dashboard - AWS Deployment Guide

This guide provides step-by-step instructions for deploying the MMCD Metrics Dashboard to AWS EC2, following the same hosting approach as the original mosquito monitoring application.

## Table of Contents
- [AWS EC2 Setup](#aws-ec2-setup)
- [Ubuntu/Linux Deployment](#ubuntulinux-deployment)
- [Windows Deployment](#windows-deployment)
- [SSL Certificate Setup](#ssl-certificate-setup)
- [Application Management](#application-management)
- [Troubleshooting](#troubleshooting)

---

## AWS EC2 Setup

### Step 1: Launch EC2 Instance

1. **Login to AWS Console** and navigate to EC2
2. **Launch Instance** with these specifications:
   - **AMI**: Ubuntu 22.04 LTS (ami-0e83be366243f524a) 
   - **Instance Type**: t2.micro (Free Tier eligible)
   - **Storage**: 8GB GP2 (minimum, 16GB recommended)
   - **Key Pair**: Create or select an existing key pair

### Step 2: Configure Security Group

Create a security group with these inbound rules:

| Type | Protocol | Port Range | Source | Description |
|------|----------|------------|--------|-------------|
| SSH | TCP | 22 | 0.0.0.0/0 | SSH access |
| HTTP | TCP | 80 | 0.0.0.0/0 | HTTP web traffic |
| HTTPS | TCP | 443 | 0.0.0.0/0 | HTTPS web traffic |
| Custom TCP | TCP | 3838 | 0.0.0.0/0 | Shiny Server |

### Step 3: Elastic IP (Optional but Recommended)

1. Allocate an **Elastic IP** address
2. Associate it with your EC2 instance
3. Update your domain DNS records if applicable

---

## Ubuntu/Linux Deployment

### Prerequisites

Connect to your EC2 instance:
```bash
# Replace with your key file and instance IP
ssh -i your-key.pem ubuntu@your-ec2-ip
```

### Step 1: System Updates and Dependencies

```bash
# Update system packages
sudo apt update && sudo apt upgrade -y

# Install essential dependencies
sudo apt install -y software-properties-common dirmngr curl wget
sudo apt install -y r-base r-base-dev
sudo apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev
sudo apt install -y libpq-dev libgdal-dev libudunits2-dev libproj-dev
sudo apt install -y nginx certbot python3-certbot-nginx
sudo apt install -y gdebi-core git
```

### Step 2: Install Shiny Server

```bash
# Download Shiny Server
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb

# Install Shiny Server
sudo gdebi -n shiny-server-1.5.22.1017-amd64.deb

# Start and enable Shiny Server
sudo systemctl start shiny-server
sudo systemctl enable shiny-server

# Verify installation
sudo systemctl status shiny-server
```

### Step 3: Install R Packages

**IMPORTANT**: Install packages as the `shiny` user to ensure proper permissions:

```bash
# Switch to shiny user
sudo su - shiny

# Launch R
R

# Install all required packages
install.packages(c(
  'shiny', 'vroom', 'tidyverse', 'shinydashboard', 
  'ggplot2', 'dplyr', 'dtplyr', 'shinyWidgets', 
  'plotrix', 'DBI', 'RPostgres', 'lubridate', 
  'scales', 'leaflet', 'sf', 'stringr', 'DT'
), repos='https://cran.rstudio.com/')

# Exit R
quit(save = "no")

# Return to ubuntu user
exit
```

### Step 4: Deploy Applications

```bash
# Clone the repository
cd /tmp
git clone https://github.com/ablepacifist/mmcd_metrics_1.git

# Copy applications to Shiny Server directory
sudo cp -r mmcd_metrics_1/apps/* /srv/shiny-server/

# Set proper ownership and permissions
sudo chown -R shiny:shiny /srv/shiny-server
sudo chmod -R 755 /srv/shiny-server

# Create shortcuts (optional)
cd /srv/shiny-server
sudo ln -sf mosquito-monitoring mosquito
sudo ln -sf suco-analysis suco  
sudo ln -sf treatment-analysis treatment

# Restart Shiny Server
sudo systemctl restart shiny-server
```

### Step 5: Configure Nginx Reverse Proxy

```bash
# Create Nginx configuration
sudo tee /etc/nginx/sites-available/mmcd-apps > /dev/null <<EOF
server {
    listen 80;
    server_name your-domain.com your-ec2-ip;  # Replace with your domain/IP
    
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
sudo ln -s /etc/nginx/sites-available/mmcd-apps /etc/nginx/sites-enabled/
sudo rm -f /etc/nginx/sites-enabled/default

# Test Nginx configuration
sudo nginx -t

# Start and enable Nginx
sudo systemctl restart nginx
sudo systemctl enable nginx
```

---

## Windows Deployment

For Windows users who need to manage the deployment from a Windows machine:

### Prerequisites

1. **Install Windows Subsystem for Linux (WSL2)**:
   ```powershell
   # Run as Administrator
   wsl --install
   # Restart computer when prompted
   ```

2. **Install Ubuntu in WSL**:
   ```powershell
   wsl --install -d Ubuntu
   ```

3. **Install AWS CLI**:
   ```powershell
   # Download and install AWS CLI from:
   # https://aws.amazon.com/cli/
   ```

### Step 1: Setup WSL Environment

```bash
# Update WSL Ubuntu
sudo apt update && sudo apt upgrade -y

# Install required tools
sudo apt install -y git curl wget openssh-client
```

### Step 2: Connect to EC2 from Windows

**Option A: Using WSL**
```bash
# Copy your .pem key to WSL (from Windows)
cp /mnt/c/path/to/your-key.pem ~/your-key.pem
chmod 400 ~/your-key.pem

# Connect to EC2
ssh -i ~/your-key.pem ubuntu@your-ec2-ip
```

**Option B: Using PuTTY**
1. Download and install PuTTY
2. Convert .pem key to .ppk using PuTTYgen
3. Connect using PuTTY with the .ppk key

### Step 3: Continue with Linux Steps

Once connected to your EC2 instance via SSH, follow the Ubuntu deployment steps above.

### Windows Development Workflow

```powershell
# Clone repository in Windows
git clone https://github.com/ablepacifist/mmcd_metrics_1.git
cd mmcd_metrics_1

# Make changes to applications
# Test locally if R is installed on Windows

# Commit and push changes
git add .
git commit -m "Update applications"
git push origin main

# Deploy to EC2 (via WSL)
wsl
ssh -i ~/your-key.pem ubuntu@your-ec2-ip
cd /tmp
git pull origin main
sudo cp -r apps/* /srv/shiny-server/
sudo systemctl restart shiny-server
exit
```

---

## SSL Certificate Setup

### Using Certbot (Let's Encrypt)

```bash
# Install SSL certificate (replace your-domain.com)
sudo certbot --nginx -d your-domain.com

# Verify auto-renewal
sudo certbot renew --dry-run

# Check certificate status
sudo certbot certificates
```

### Manual Certificate Setup

If you have your own SSL certificates:

```bash
# Copy certificates to proper location
sudo mkdir -p /etc/nginx/ssl
sudo cp your-certificate.crt /etc/nginx/ssl/
sudo cp your-private-key.key /etc/nginx/ssl/

# Update Nginx configuration for SSL
sudo nano /etc/nginx/sites-available/mmcd-apps
# Add SSL configuration block
```

---

## Application Management

### Adding New Applications

1. **Create application locally**:
   ```bash
   mkdir -p apps/new-app-name
   # Create app.R file
   ```

2. **Test locally**:
   ```bash
   R -e "shiny::runApp('apps/new-app-name')"
   ```

3. **Deploy to server**:
   ```bash
   # Copy to server
   scp -r apps/new-app-name ubuntu@your-ec2-ip:/tmp/
   
   # On server
   sudo cp -r /tmp/new-app-name /srv/shiny-server/
   sudo chown -R shiny:shiny /srv/shiny-server/new-app-name
   sudo systemctl restart shiny-server
   ```

### Updating Applications

```bash
# On EC2 server
cd /tmp
git clone https://github.com/ablepacifist/mmcd_metrics_1.git
sudo cp -r mmcd_metrics_1/apps/* /srv/shiny-server/
sudo chown -R shiny:shiny /srv/shiny-server
sudo systemctl restart shiny-server
```

### Backup and Restore

```bash
# Backup applications
sudo tar -czf /tmp/shiny-backup-$(date +%Y%m%d).tar.gz /srv/shiny-server

# Restore from backup
sudo tar -xzf /tmp/shiny-backup-YYYYMMDD.tar.gz -C /
sudo chown -R shiny:shiny /srv/shiny-server
```

---

## Troubleshooting

### Common Issues

**1. Shiny Server Not Starting**
```bash
sudo systemctl status shiny-server
sudo journalctl -u shiny-server -f
```

**2. R Package Installation Errors**
```bash
# Install system dependencies
sudo apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev

# Reinstall packages as shiny user
sudo su - shiny
R
install.packages("package-name", repos='https://cran.rstudio.com/')
```

**3. Permission Issues**
```bash
sudo chown -R shiny:shiny /srv/shiny-server
sudo chmod -R 755 /srv/shiny-server
```

**4. Database Connection Issues**
```bash
# Test database connectivity
R -e "
library(DBI)
library(RPostgres)
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'mmcd_data',
                 host = 'rds-readonly.mmcd.org',
                 port = 5432,
                 user = 'mmcd_read',
                 password = 'mmcd2012')
dbListTables(con)
"
```

**5. Nginx Configuration Issues**
```bash
sudo nginx -t
sudo systemctl status nginx
sudo tail -f /var/log/nginx/error.log
```

### Monitoring and Logs

```bash
# Application logs
sudo tail -f /var/log/shiny-server/*.log

# System logs
sudo journalctl -u shiny-server -f
sudo journalctl -u nginx -f

# Check running processes
ps aux | grep shiny
ps aux | grep nginx

# Check port usage
sudo netstat -tlnp | grep :3838
sudo netstat -tlnp | grep :80
```

### Performance Optimization

```bash
# Monitor system resources
htop
df -h
free -m

# Optimize R memory usage
# Add to /etc/shiny-server/shiny-server.conf:
# location / {
#   simple_scheduler 15;
#   dir_index off;
# }
```

---

## Security Considerations

1. **Firewall Configuration**:
   ```bash
   sudo ufw enable
   sudo ufw allow 22/tcp
   sudo ufw allow 80/tcp
   sudo ufw allow 443/tcp
   sudo ufw allow 3838/tcp
   ```

2. **Regular Updates**:
   ```bash
   sudo apt update && sudo apt upgrade -y
   sudo certbot renew
   ```

3. **Access Control**:
   - Use strong SSH keys
   - Disable password authentication
   - Consider VPN access for sensitive applications

---

## URLs After Deployment

After successful deployment, your applications will be available at:

- **Main Dashboard**: `https://your-domain.com/` or `http://your-ec2-ip/`
- **Mosquito Monitoring**: `https://your-domain.com/mosquito-monitoring/`
- **SUCO Analysis**: `https://your-domain.com/suco-analysis/`
- **Treatment Analysis**: `https://your-domain.com/treatment-analysis/`
- **Direct Shiny Access**: `http://your-domain.com:3838/app-name/`

---

*This deployment guide follows the same hosting approach as the original MMCD mosquito monitoring application, ensuring consistency and reliability across all MMCD analytics platforms.*
