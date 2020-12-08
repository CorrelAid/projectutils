Sys.setenv("R_TESTS" = "")
library(testthat)
library(mockery)
library(projectutils)

test_check("projectutils")
