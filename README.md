
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sasrvira

<!-- badges: start -->

<!-- badges: end -->

## Overview

This package provides an experimental framework for parsing and
transpiling SAS `PROC` statements into R code. Each SAS procedure is
represented as a structured S7 object, making it easier to inspect,
transform, and generate R equivalents.

## Features

- Parse SAS `PROC` statements into structured objects
- Extract metadata such as dataset, variables, class, by-groups, and
  output specifications
- Subclass specific procedures (e.g., `PROC CONTENTS`, `PROC PRINT`,
  `PROC MEANS`)
- Generate corresponding R code via the `transpile()` method

## Installation

You can install the development version of sasrvira from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("AurelieFrechet/sasrvira")
```

## Example Usage

``` r
library(sasrvira)
#> Le chargement a nécessité le package : stringi
#> Le chargement a nécessité le package : S7
sas_code <- "proc means data=mydata; var x y; class group; output out=summary mean=mx my; run;"
proc <- ProcMeans(sas_code)
proc
#> <sasrvira::ProcMeans>
#>  @ source      : chr "proc means data=mydata; var x y; class group; output out=summary mean=mx my; run;"
#>  @ proc_data   : chr "mydata"
#>  @ proc_options: chr(0) 
#>  @ pm_var      : chr [1:2] "x" "y"
#>  @ pm_by       : chr(0) 
#>  @ pm_class    : chr "group"
#>  @ pm_format   : chr(0) 
#>  @ pm_freq     : chr(0) 
#>  @ pm_id       : chr(0) 
#>  @ pm_types    : chr(0) 
#>  @ pm_weight   : chr(0) 
#>  @ pm_ways     : chr(0) 
#>  @ pm_output   :List of 2
#>  .. $ out : chr "summary"
#>  .. $ mean: chr [1:2] "mx" "my"
transpile(proc)
#> [1] "summary <- mydata %>%\n\tgroup_by(group) %>%\n\tsummarize(mx = mean(x), my = mean(y))"
```

## Supported Procedures

| SAS Procedure | S7 Class | Notes |
|----|----|----|
| PROC CONTENTS | `ProcContents` | Dataset structure inspection |
| PROC PRINT | `ProcPrint` | Dataset preview / printing |
| PROC MEANS | `ProcMeans` | Summary statistics with parsing of VAR, BY, CLASS, OUTPUT, etc. |

## Disclaimer

This is an experimental package. The transpiled R code is intended as a
starting point and may require manual adjustments for complex SAS
options.
