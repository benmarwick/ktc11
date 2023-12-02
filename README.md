<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Last-changedate](https://img.shields.io/badge/last%20change-2016--11--16-brightgreen.svg)](https://github.com/benmarwick/ktc11/commits/master) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.1-brightgreen.svg)](https://cran.r-project.org/) [![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/) [![Travis-CI Build Status](https://travis-ci.org/benmarwick/ktc11.png?branch=master)](https://travis-ci.org/benmarwick/ktc11) [![Circle CI](https://circleci.com/gh/benmarwick/ktc11.svg?style=shield&circle-token=:circle-token)](https://circleci.com/gh/benmarwick/ktc11) [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0001--7879--4531-green.svg)](http://orcid.org/0000-0001-7879-4531)

Research compendium for a report on archaeological excavations at Khao Toh Chong, Krabi, Thailand
-------------------------------------------------------------------------------------------------

### Compendium DOI:

<https://doi.org/10.6084/m9.figshare.2065602>

The files at the URL above will generate the results as found in the publication. The files hosted at <https://github.com/benmarwick/ktc11> are the development versions and may have changed since the report was published

### Author of this repository:

Ben Marwick (<benmarwick@gmail.com>)

### Published in:

Marwick, B., Van Vlack, H.G., Conrad, C., Shoocongdej, R., Thongcharoenchaikit, C., Kwak, S. 2016 Adaptations to sea level change and transitions to agriculture at Khao Toh Chong rockshelter, Peninsular Thailand, *Journal of Archaeological Science* <https://doi.org/10.1016/j.jas.2016.10.010>

### Overview of contents

This repository is our research compendium for our analysis of archaeological excavations at Khao Toh Chong, Krabi, Thailand. The compendium contains all data, code, and text associated with the publication (which is currently under review). The `ktc_11_paper.Rmd` file in the `analysis/paper/` directory contains details of how all the analyses reported in the paper were conducted, as well as instructions on how to rerun the analysis to reproduce the results. The `data/` directory in the `analysis/` directory contains all the raw data. You can download the contents as a zip file from https://github.com/benmarwick/ktc11/archive/refs/heads/master.zip

### The supplementary files

The `analysis/` directory contains all the data files (in CSV format, in the `data/` directory), the manuscript as submitted (in MS Word format, in the `paper/` directory), a source file for the submitted paper (in R markdown format in the `paper/` directory), an executed version of the supplementary file (in HTML format, in the `paper/` directory) and all the figures that are included in the paper (in the `figures/` directory).

### The R package

This repository is organized as an R package. These functions are provided as a package because this makes it simpler to reuse the functions many times in the paper. It also makes it easier for others to use and adapt these functions on their own data. Nevertheless, this package has been written explicitly for this project and may not yet be suitable for more general purpose use.

To download the package source as you see it on GitHub, for offline browsing, use this line at the shell prompt:

``` shell
git clone https://github.com/benmarwick/ktc11.git
```

Once the download is complete, open the `ktc11.Rproj` in RStudio to begin working with the package and compendium files.

If you want to re-run all the analyses in R, you can start by installing the compendium package with this line at the R prompt:

``` r
install.packages("devtools") # which in turn requires Rtools (if Windows) or Xcode (if OSX)

# outside of R, install Java using the lastest JDK from
# https://www.oracle.com/java/technologies/downloads/, then:
install.packages("rJava")

# this pkg has been removed from CRAN since the paper was published,
# so it wont install with the rest of the dependencies, and
# we need to get it from the archive:
devtools::install_version("maptools", "1.1-8")

# finally, install the package for this compendium:
devtools::install_github("benmarwick/ktc11")
```

Then set the working directory to the `paper/` directory in this repository, and run `rmarkdown::render("analysis/paper/ktc_11_paper.Rmd")` file to generate the rendered copy (HTML or Word) and re-compute all the analyses.

The package has a number of dependencies on other R packages, and programs outside of R. Installing these can be time-consuming and complicated, so we've done two things to simplify access to the compendium. First is the packrat directory, which contains the source code for all the packages we depend on. If all works well, these will be installed on your computer when you open `researchcompendium.Rproj` in RStudio. Second is our Docker image that includes all the necessary software, code and data to run our analysis. The Docker image may give a quicker entry point to the project, and is more self-contained, so might save some fiddling with installing things.

### The Docker image

A Docker image is a lightweight GNU/Linux virtual computer that can be run as a piece of software on Windows and OSX (and other Linux systems). To capture the complete computational environment used for this project we have a Dockerfile that specifies how to make the Docker image that we developed this project in. The Docker image includes all of the software dependencies needed to run the code in this project, as well as the R package and other compendium files. To launch the Docker image for this project, first, [install Docker](https://docs.docker.com/installation/) on your computer. At the Docker prompt, enter:

    docker run -dp 8787:8787 benmarwick/ktc11

This will start a server instance of RStudio. Then open your web browser at localhost:8787 or or run `docker-machine ip default` in the shell to find the correct IP address, and log in with rstudio/rstudio.

Once logged in, use the Files pane (bottom right) to navigate to `/` (the root directory), then open the folder for this project, and open the `.Rproj` file for this project. Once that's open, you'll see the `analysis/paper` directory in the Files pane where you can find the R markdown document, and knit them to produce the results in the paper. More information about using RStudio in Docker is available at the [Rocker](https://github.com/rocker-org) [wiki](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image) pages.

We developed and tested the package on this Docker container, so this is the only platform that We're confident it works on, and so recommend to anyone wanting to use this package to generate the vignette, etc.

### Licenses:

Manuscript: CC-BY-4.0 <http://creativecommons.org/licenses/by/4.0/>

Code: MIT <http://opensource.org/licenses/MIT> year: 2016, copyright holder: Ben Marwick

Data: CC0 <http://creativecommons.org/publicdomain/zero/1.0/> attribution requested in reuse

### Dependencies:

I used [RStudio](http://www.rstudio.com/products/rstudio/) on Ubuntu 16.04 and Windows 7. See the colophon section of the docx file in `analysis/paper` for a full list of the packages that this project depends on.

### Contact:

Ben Marwick, Department of Anthropology Denny Hall 117, Box 353100, University of Washington Seattle, WA 98195-3100 USA

1.  (+1) 206.552.9450 e. <bmarwick@uw.edu>
2.  (+1) 206.543.3285 w. <http://faculty.washington.edu/bmarwick/>
