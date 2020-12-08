test_that("cleaning colnames works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  expect_true("project_id" %in% colnames(clean_application_colnames(test_data, "en")))
})

test_that("separating title and project id works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  cleaned <- clean_application_colnames(test_data, "en")
  expect_equal(unique(cleaned$project_id), c("COR-11-2020", "CIT-10-2020"))
})

test_that("filtering works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  mockery::stub(load_applications, 'get_surveymonkey', test_data)
  expect_equal(nrow(load_applications("CIT-10-2020")), 37)
})
