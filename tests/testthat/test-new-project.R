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

test_that("project creation works as expected", {
  # create files
  fs::path_file(".")
  new_project("2020-10-FOO")
  expect_true(dir.exists(here::here("2020-10-FOO/")))
  expect_true(dir.exists(here::here("2020-10-FOO/en")))
  expect_true(dir.exists(here::here("2020-10-FOO/de")))
  expect_true(file.exists(here::here("2020-10-FOO/meta.json")))
  expect_true(file.exists(here::here("2020-10-FOO/en/00_about.md")))
  expect_true(file.exists(here::here("2020-10-FOO/en/00_summary.md")))
  expect_true(file.exists(here::here("2020-10-FOO/en/01_problem.md")))
  expect_true(file.exists(here::here("2020-10-FOO/en/02_data.md")))
  expect_true(file.exists(here::here("2020-10-FOO/en/03_approach.md")))
  expect_true(file.exists(here::here("2020-10-FOO/en/04_impact.md")))
})

teardown({
  unlink(here::here("2020-10-FOO/"), recursive = TRUE)
})
