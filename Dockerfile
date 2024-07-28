FROM rocker/r-ver:4.3.1
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev
# Install R packages
RUN R -e "install.packages(c('GGally', 'tidyverse', 'ggplot2', 'knitr', 'kableExtra', 'outliers', 'forcats', 'gridExtra', 'dplyr', 'tidyr', 'patchwork', 'plumber', 'caret', 'rpart', 'rpart.plot', 'randomForest'))"


# Copy your API and data file into the Docker image
COPY myAPI.R /app/myAPI.R
COPY diabetes_binary_health_indicators_BRFSS2015.csv /app/diabetes_binary_health_indicators_BRFSS2015.csv

# Set the working directory
WORKDIR /app

#COPY myAPI.R myAPI.R
EXPOSE 8000
ENTRYPOINT ["R", "-e", \
"pr <- plumber::plumb('myAPI.R'); pr$run(host='0.0.0.0', port=8000)"]
