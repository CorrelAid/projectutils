projectutils
================

# projectutils

This R package aims to make it easier to coordinate CorrelAid projects
by providing utility functions. Additional, more text-focused help can
be found in the [documentation for project
coordinators](https://docs.correlaid.org/project-manual/project-coordinators).

## Installation

This package is only available on GitHub and there are no plans to
submit it to CRAN.

with `{devtools}` :package:

``` r
devtools::install_github("CorrelAid/projectutils")
```

or with the `{remotes}` :package:

``` r
remotes::install_github("CorrelAid/projectutils")
```

#### Create new projects data

#### Download applications for your project

This requires an access token for the surveymonkey account. You can
acquire this token from Frie.

Please see the README of the [`surveymonkey`](https://github.com/tntp/surveymonkey#add-your-oauth-token-to-your-rprofile) package on how to make this token accessible to your R session.

``` r
library(projectutils)
```
