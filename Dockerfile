# get the base image, this one has R, RStudio, pandoc, and a bunch of R packages that I use often
FROM rocker/hadleyverse

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

# install some packages that not in the base image, these have to be manually identified from my package's Description -> Imports list
RUN apt-get update \
# install a few packages from GitHub for the most recent versions (or if they're not on CRAN)
  && installGithub.r --deps TRUE \
    # install my package that is the focus of this image
    benmarwick/ktc11

# make a directory to hold the github repo for my package
  && mkdir /home/rstudio/ktc11

# Get my package source files from github and download onto Docker. The built package that we already got above is no good because it doesn't have the analysis directory in the same structure as the package source
  && git clone https://github.com/benmarwick/ktc11  /home/rstudio/ktc11

# Set the working directory to where the main document is
  && cd /home/rstudio/ktc11/analysis


# to build this image:
# docker build -t benmarwick/ktc11 https://raw.githubusercontent.com/benmarwick/ktc11/master/Dockerfile

# to run this container to work on the project:
# docker run -dp 8787:8787  -v /c/Users/bmarwick/docker:/home/rstudio/ -e ROOT=TRUE  benmarwick/ktc11
# then open broswer at localhost:8787 or run `docker-machine ip default` in the shell to find the correct IP address

# to add CI for the docker image
# add .circle.yml file
# - Pushes new image to hub on successful complete of test
# - And gives a badge to indicate test status

# to manually update the container at the end of a work session:
# docker push benmarwick/ktc11

