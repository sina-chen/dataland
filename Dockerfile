FROM rocker/tidyverse:4.0.1

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev 

# Install required R packages
RUN R -e "install.packages(c('tidyverse', 'zoo', 'rvest'), repos='http://cran.rstudio.com/')"

# Set the working directory 
WORKDIR /dataland

# Copy the current directory contents into the container 
COPY . /dataland/

# Run an R script when the container launches
CMD ["Rscript", "code/dataland_polls_trends.R"]

EXPOSE 8787

