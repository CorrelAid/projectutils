test_that("that non-project folders are ignored when loading projects", {
  projects <- load_projects("tests/testthat/test_data/")
  expect_equal(length(projects), 1)
})