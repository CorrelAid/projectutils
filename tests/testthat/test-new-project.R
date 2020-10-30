test_that("invalid year throws error", {
  expect_error(new_project("ERL", 293, 12), regexp = "Invalid year", class = "usethis_error")
})

test_that("invalid year throws error", {
  expect_error(new_project("ERL", "2019", 12), regexp = "Invalid year", class = "usethis_error")
})

test_that("invalid year throws error", {
  expect_error(new_project("ERL", -2019, 12), regexp = "Invalid year", class = "usethis_error")
})

test_that("invalid year throws error", {
  expect_error(new_project("ERL", 0000, 12), regexp = "Invalid year", class = "usethis_error")
})

test_that("invalid month throws error", {
  expect_error(new_project("ERL", 2018, -1), regexp = "Invalid month", class = "usethis_error")
})

test_that("invalid month throws error", {
  expect_error(new_project("ERL", 2018, "5"), regexp = "Invalid month", class = "usethis_error")
})

test_that("invalid month throws error", {
  expect_error(new_project("ERL", 2018, 53), regexp = "Invalid month", class = "usethis_error")
})

test_that("invalid prefix throws an error", {
  expect_error(new_project("ERJLA", 2018, 2), regexp = "Invalid prefix", class = "usethis_error")
})

test_that("project creation works as expected", {
  # create files
  new_project("FOO", 2020, 10, "tests/testthat/test_data/")
  expect_true(dir.exists(here::here("tests/testthat/test_data/2020-10-FOO/")))
  expect_true(dir.exists(here::here("tests/testthat/test_data/2020-10-FOO/en")))
  expect_true(dir.exists(here::here("tests/testthat/test_data/2020-10-FOO/de")))
  expect_true(file.exists(here::here("tests/testthat/test_data/2020-10-FOO/meta.json")))
  expect_true(file.exists(here::here("tests/testthat/test_data/2020-10-FOO/en/00_about.md")))
  expect_true(file.exists(here::here("tests/testthat/test_data/2020-10-FOO/en/00_summary.md")))
  expect_true(file.exists(here::here("tests/testthat/test_data/2020-10-FOO/en/01_problem.md")))
  expect_true(file.exists(here::here("tests/testthat/test_data/2020-10-FOO/en/02_data.md")))
  expect_true(file.exists(here::here("tests/testthat/test_data/2020-10-FOO/en/03_approach.md")))
  expect_true(file.exists(here::here("tests/testthat/test_data/2020-10-FOO/en/04_impact.md")))
})

teardown({
  unlink(here::here("tests/testthat/test_data/2020-10-FOO/"), recursive = TRUE)
})