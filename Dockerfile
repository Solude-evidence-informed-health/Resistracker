# Use the official Shiny base image
FROM rocker/shiny

# Set the working directory to /srv/shiny-server/app
WORKDIR /srv/shiny-server/app

# Install necessary R packages
RUN R -e "install.packages(c('shiny', 'tidyverse', 'shinyWidgets', 'googlesheets4', 'apexcharter', 'DT', 'shinybusy'))"

# Copy the app code into the image
COPY . /srv/shiny-server/app

# Expose the Shiny port
EXPOSE 3838

# Run Shiny app
CMD ["/usr/bin/shiny-server.sh"]