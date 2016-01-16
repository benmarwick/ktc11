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
RUN mkdir /home/rstudio/ktc11

# Get my package source files from github and download onto Docker. The built package that we already got above is no good because it doesn't have the analysis directory in the same structure as the package source
RUN git clone https://github.com/benmarwick/ktc11  /home/rstudio/ktc11

# Set the working directory to where the main document is
WORKDIR /home/rstudio/ktc11/analysis


# to build this image:
# docker build -t benmarwick/ktc11 https://raw.githubusercontent.com/benmarwick/ktc11/master/Dockerfile

# to run this container:
# docker -dp 8787:8787 benmarwick/ktc11
# then open broswer at localhost:8787 or http://192.168.59.103:8787/

