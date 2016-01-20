<!-- README.md is generated from README.Rmd. Please edit that file -->
Research compendium for a report on archaeological excavations at Khao Toh Chong, Krabi, Thailand
-------------------------------------------------------------------------------------------------

### Compendium DOI:

<https://dx.doi.org/10.6084/m9.figshare.2065602>

The files at the URL above will generate the results as found in the publication. The files hosted at <https://github.com/benmarwick/ktc11> are the development versions and may have changed since the report was published

### Author of this repository:

Ben Marwick (<benmarwick@gmail.com>)

### Published in:

...

### Overview of contents

This repository is our research compendium for our analysis of archaeological excavations at Khao Toh Chong, Krabi, Thailand. The compendium contains all data, code, and text associated with the publication (which is currently under review). The `ktc_11_paper.Rmd` file in the `analysis/paper/` directory contains details of how all the analyses reported in the paper were conducted, as well as instructions on how to rerun the analysis to reproduce the results. The `data/` directory in the `analysis/` directory contains all the raw data.

### The supplementary files

The `analysis/` directory contains all the data files (in CSV format, in the `data/` directory), the manuscript as submitted (in MS Word format, in the `paper/` directory), a source file for the submitted paper (in R markdown format in the `paper/` directory), an executed version of the supplementary file (in HTML format, in the `paper/` directory) and all the figures that are included in the paper (in the `figures/` directory).

### The R package [![Travis-CI Build Status](https://travis-ci.org/benmarwick/ktc11.svg?branch=master)](https://travis-ci.org/benmarwick/ktc11)

This repository is organized as an R package. These functions are provided as a package because this makes it simpler to resue the functions many times in the paper. It also makes it easier for others to use and adapt these fucntions on their own data. Nevertheless, this package has been written explicitly for this project and may not yet be suitable for more general purpose use.

To download the package source as you see it on GitHub, for offline browsing, use this line at the shell prompt:

``` r
git clone https://github.com/benmarwick/ktc11.git
```

Once the download is complete, open the `ktc11.Rproj` in RStudio to begin working with the package and compendium files.

If you want to re-run all the analyses in R, you can start by installing the compendium package with this line at the R prompt:

``` r
# install.packages("devtools") # which in turn requires Rtools (if Windows) or Xcode (if OSX)
devtools::install_github("benmarwick/ktc11")
```

Then set the working directory to the `paper/` directory in this repository, and run `knitr::knit("ktc_11_paper.Rmd")` file to generate the rendered copy (HTML or Word) and re-compute all the analyses.

### Licenses:

Manuscript: CC-BY-4.0 <http://creativecommons.org/licenses/by/4.0/>

Code: MIT <http://opensource.org/licenses/MIT> year: 2015, copyright holder: Ben Marwick

Data: CC0 <http://creativecommons.org/publicdomain/zero/1.0/> attribution requested in reuse

### Dependencies:

I used [RStudio](http://www.rstudio.com/products/rstudio/) (version 0.98.953) on Ubuntu 14.04 and these packages:

Identified using `sessionInfo()`:

``` r
sessionInfo()
#> R version 3.2.2 (2015-08-14)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 7 x64 (build 7601) Service Pack 1
#> 
#> locale:
#> [1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252   
#> [3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
#> [5] LC_TIME=English_Australia.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] magrittr_1.5    formatR_1.2.1   tools_3.2.2     htmltools_0.2.6
#>  [5] yaml_2.1.13     stringi_1.0-1   rmarkdown_0.9.2 knitr_1.11     
#>  [9] stringr_1.0.0   digest_0.6.9    evaluate_0.8
```

### Contact:

Ben Marwick, Department of Anthropology Denny Hall 117, Box 353100, University of Washington Seattle, WA 98195-3100 USA

1.  (+1) 206.552.9450 e. <bmarwick@uw.edu>
2.  (+1) 206.543.3285 w. <http://faculty.washington.edu/bmarwick/>
