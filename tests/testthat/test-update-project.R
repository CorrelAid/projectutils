context("Project utils")

# load project 
test_that("test that non-existing folder throws error", {
  expect_error(load_project("foo", data = "tests/testthat/test_data"), regexp = "Folder tests/testthat/test_data/foo does not exist.", class = "usethis_error")
})

test_that("test that loading project works", {
  p <- load_project("2020-01-TES", data = "tests/testthat/test_data")
  expect_equal(is.list(p), TRUE)
})

# update project
test_that("test that non-existing field throws error", {
    project <- load_project("2020-01-TES", data = "tests/testthat/test_data")
    expect_error(update_project(project, "foobar", "al"), "Field foobar does not exist.", class = "usethis_error")
})

test_that("test updating a top-level field works", {
  new_title <- "new_title"
  project <- load_project("2020-01-TES", data = "tests/testthat/test_data")
  expect_equal(update_project(project, "title", new_title)$title, new_title)
})
