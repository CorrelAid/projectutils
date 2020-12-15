context("update projects")

# find out who is testing - devtools or cmd check
tb <- rlang::trace_back(1)
call_fn_name <- as.character(tb[[1]][[1]][1])
if (call_fn_name == "devtools::test") {
  DATA_FOLDER <- here::here("tests/testthat/test_data")
} else if (call_fn_name == "testthat::test_check") {
  DATA_FOLDER <- "test_data"
} 

test_that("that non-project folders are ignored when loading projects", {
  projects <- load_projects(data_folder = DATA_FOLDER)
  expect_equal(length(projects), 1)
})

# load project 
test_that("test that non-existing folder throws error", {
  expect_error(load_project("foo", data_folder = DATA_FOLDER), regexp = "foo does not exist.", class = "usethis_error")
})


test_that("test that loading project works", {
  p <- load_project("2020-01-TES", data_folder = DATA_FOLDER)
  expect_equal(is.list(p), TRUE)
})

# update project
test_that("test that non-existing field throws error", {
  
    project <- load_project("2020-01-TES", data_folder = DATA_FOLDER)
    expect_error(update_project(project, "foobar", "al"), "Field foobar does not exist.", class = "usethis_error")
})

test_that("test updating a top-level field works", {
  new_title <- "new_title"
  project <- load_project("2020-01-TES", data_folder = DATA_FOLDER)
  expect_equal(update_project(project, "title", new_title)$title, new_title)
})
