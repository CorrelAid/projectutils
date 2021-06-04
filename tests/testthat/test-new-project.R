context("creating projects")
test_that("invalid project id throws error", {
  expect_error(new_project("ERL-239-23"), regexp = "Invalid project id", class = "usethis_error")
}) 

test_that("invalid project id throws error", {
  expect_error(new_project("2020-03-low"), regexp = "Invalid project id", class = "usethis_error")
})

test_that("invalid year throws error", {
  expect_error(new_project("1000-02-ERL"), regexp = "Invalid year", class = "usethis_error")
})

test_that("invalid month throws error", {
  expect_error(new_project("2019-23-ERL"), regexp = "Invalid month in project id", class = "usethis_error")
})

test_that("invalid prefix throws an error", {
  expect_error(new_project("2020-02-ERJLE"), regexp = "Invalid project id", class = "usethis_error")
})
