projectutils
================

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/CorrelAid/projectutils/branch/master/graph/badge.svg)](https://codecov.io/gh/CorrelAid/projectutils?branch=master)
[![R build
status](https://github.com/CorrelAid/projectutils/workflows/R-CMD-check/badge.svg)](https://github.com/CorrelAid/projectutils/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

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

## Manage project data

This package can help you to create and update entries for the
[CorrelAid project “database”](https://github.com/correlaid/projectsdb).
For instructions related to this functionality, please refer to the
README of the [CorrelAid project
“database”](https://github.com/correlaid/projectsdb).

## Surveymonkey integration

We use [surveymonkey](https://surveymonkey.de) for the following
project-related things:

  - collect applications for projects
  - collect feedback on the project from NPOs
  - collect feedback on the project from volunteers

Functions to get data from surveymonkey are under development (see
branch `surveymonkey`)
