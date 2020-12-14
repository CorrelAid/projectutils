context("loading projects")
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