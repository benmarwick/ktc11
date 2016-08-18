# get the base image, this one has R, RStudio, pandoc, and a bunch of R packages that I use often
FROM rocker/hadleyverse

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

# install some packages that not in the base image, these have to be manually identified from my package's Description -> Imports list
RUN apt-get update \
  && sudo apt-get install r-cran-rjava \
# install a few packages from GitHub for the most recent versions (or if they're not on CRAN)
  && installGithub.r --deps TRUE \
    # install my package that is the focus of this image
    benmarwick/ktc11 \

# make a directory to hold the github repo for my package
  && mkdir /home/rstudio/ktc11
  WORKDIR /home/rstudio/ktc11

# Get my package source files from github and download onto Docker. The built package that we already got above is no good because it doesn't have the analysis directory in the same structure as the package source
  RUN git clone https://github.com/benmarwick/ktc11  . \

# build the package
  && R CMD build ktc11

#################### Notes to self ###############################
# a suitable disposable test env:
# docker run -dp 8787:8787 rocker/rstudio

# to build this image:
# docker build -t benmarwick/ktc11 https://raw.githubusercontent.com/benmarwick/ktc11/master/Dockerfile

# to run this container to work on the project:
# docker run -dp 8787:8787  -v /c/Users/bmarwick/docker:/home/rstudio/ -e ROOT=TRUE  benmarwick/ktc11
# then open broswer at localhost:8787 or run `docker-machine ip default` in the shell to find the correct IP address

# go to hub.docker.com
# create empty repo for this repo, then

# to add CI for the docker image
# add .circle.yml file
# - Pushes new image to hub on successful complete of test
# - And gives a badge to indicate test status

# On https://circleci.com/gh/benmarwick/this_repo
# I need to set BUILD_ENVIRONMENT to Ubuntu 12
# I need to set Environment Variables:
# DOCKER_EMAIL
# DOCKER_PASS
# DOCKER_USER

# Circle will push to docker hub automatically after each commit, but
# to manually update the container at the end of a work session:
# docker login # to authenticate with hub
# docker push benmarwick/ktc11

