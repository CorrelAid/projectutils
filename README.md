projectutils
================

<!-- badges: start -->

[![codecov](https://codecov.io/gh/CorrelAid/projectutils/branch/main/graph/badge.svg?token=1AxThNtabA)](https://codecov.io/gh/CorrelAid/projectutils)
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

## Manage team selection

`projectutils` provides templates for scripts with all the necessary
workflow steps for team selection.

Those templates can be installed / created in a (newly created)
`team_selection` subfolder with:

``` r
use_team_selection_workflow("2020-12-TES")
```

#### 01\_{prefix}\_prepare\_team\_selection.R

  - download applications from [kobotoolbox](https://kobo.correlaid.org)
    and do data cleaning (`load_applications()`)
  - anonymize applications (`anonymize_applications()`)
  - store the mapping of applicant\_ids to emails and names
  - knit a PDF report that includes helpful visualizations of the
    self-rating questions as well as the answers to the open-ended
    questions

#### 02\_{prefix}\_send\_confirmation\_emails.R

  - based on the `applicant_id`s of the selected team members, extract
    the email addresses of the selected / declined applicants and write
    a “;” - separated string to the clipboard for convenient
    copy-pasting into Outlook.

#### Report templates

Executing `use_team_selection_workflow()` will also create two RMarkdown
templates in the folder. Those are supposed to be knitted by
`01_{prefix}_prepare_team_selection.R` and not directly. You can adapt
them if needed.

## Reporting on projects

Yet to be implemented:

  - reports on the feedback on the project from NPOs
  - reports on the feedback on the project from volunteers
