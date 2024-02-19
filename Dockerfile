# Base R Shiny image
FROM rocker/shiny:latest

RUN mkdir /home/shiny-app

# Install R dependencies
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyWidgets', 'shinythemes', 'shinyjs', 'DT', 'tidyverse', 'shinycssloaders', 'rmarkdown', 'lubridate', 'ggplot2', 'toastui', 'shinyDatetimePickers', 'colourpicker', 'dplyr'))"

# Expose the application port
EXPOSE 8180

COPY shiny-app/ /home/shiny-app

# Run the R Shiny app
CMD Rscript -e "shiny::runApp('/home/shiny-app/app.R', port = 8180, host = '0.0.0.0')"