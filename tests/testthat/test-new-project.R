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