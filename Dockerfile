# get the base image, this one has R, RStudio, pandoc, and a bunch of R packages that I use often
FROM rocker/verse:3.3.2

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

# install some things...

RUN apt-get update \
  && sudo apt-get install libjpeg-dev  libpng-dev openjdk-7-jdk  libglu1-mesa-dev freeglut3-dev mesa-common-dev -y \

  # get repo from GH...
  && git clone https://github.com/benmarwick/ktc11.git \

  # make it writable and cd...
  && chmod 777 -R ktc11 \
  && cd /ktc11 \

  # start R and build pkgs that we depend on from local sources that we have collected with packrat...
  && R -e "0" --args --bootstrap-packrat \

  # install the compendium package...
  # && R -e "devtools::install_github('benmarwick/ktc11')"


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

# When running this container, the ktc11 dir is not writable, so we need to
# sudo chmod 777 -R ktc11

#

