context("loading projects")
test_that("that non-project folders are ignored when loading projects", {
  projects <- load_projects(data_folder = here::here("tests/testthat/test_data"))
  expect_equal(length(projects), 1)
})