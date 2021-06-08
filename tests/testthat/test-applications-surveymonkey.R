context("surveymonkey applications")

# find out who is testing - devtools or cmd check
tb <- rlang::trace_back(1)
call_fn_name <- as.character(tb[[1]][[1]][1])
if (call_fn_name == "devtools::test") {
  DATA_FOLDER <- here::here("tests/testthat/test_data")
} else if (call_fn_name == "testthat::test_check") {
  DATA_FOLDER <- "test_data"
} 


test_that("cleaning colnames works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  expect_true("project_id" %in% colnames(clean_application_colnames(test_data, "en")))
})

test_that("separating title and project id works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  cleaned <- clean_application_colnames(test_data, "en")
  expect_equal(unique(cleaned$project_id), c("COR-11-2020", "CIT-10-2020"))
})

test_that("cleaning data exported via surveymonkey UI works", {
  applications <- load_applications_export("test_data/surveymonkey/applications_fake_export.csv", "CIT-10-2020")
  expect_equal(nrow(applications), 30)
  expect_true("skills_r" %in% colnames(applications))
  expect_true("skills_python" %in% colnames(applications))
  expect_true("skills_sql" %in% colnames(applications))
  expect_true("skills_javascript_frontend" %in% colnames(applications))
  expect_true("skills_html_css" %in% colnames(applications))
  expect_true("skills_node_js_javascript_backend" %in% colnames(applications))
  expect_true("consent_privacy_policy" %in% colnames(applications))
  expect_true("motivation_skills" %in% colnames(applications))
  expect_true("motivation_why_involved" %in% colnames(applications))
  expect_true("project_role" %in% colnames(applications))
  expect_true("gender" %in% colnames(applications))
  expect_true("first_name" %in% colnames(applications))
  expect_true("email" %in% colnames(applications))
  expect_true("techniques_data_cleaning" %in% colnames(applications))
  expect_true("techniques_data_cleaning" %in% colnames(applications))
  expect_true("techniques_data_anonymization" %in% colnames(applications))
  expect_true("techniques_database_management" %in% colnames(applications))
  expect_true("techniques_descriptive_statistics" %in% colnames(applications))
  expect_true("techniques_data_visualization" %in% colnames(applications))
  expect_true("techniques_inferential_statistics" %in% colnames(applications))
  expect_true("techniques_regressions_and_modelling" %in% colnames(applications))
  expect_true("techniques_classification" %in% colnames(applications))
  expect_true("techniques_clustering" %in% colnames(applications))
  expect_true("techniques_natural_language_processing" %in% colnames(applications))
  expect_true("techniques_neural_networks_deep_learning" %in% colnames(applications))
  expect_true("techniques_processing_of_image_data" %in% colnames(applications))
  expect_true("techniques_processing_of_audio_data" %in% colnames(applications))
})


test_that("cleaning data exported via UI works for German data", {
  applications <- load_applications_export("test_data/surveymonkey/applications_fake_export_de.csv", "CIT-10-2020", lang = "de")
  expect_equal(nrow(applications), 30)
  expect_true("consent_privacy_policy" %in% colnames(applications))
  expect_true("motivation_skills" %in% colnames(applications))
  expect_true("motivation_why_involved" %in% colnames(applications))
  expect_true("project_role" %in% colnames(applications))
  expect_true("gender" %in% colnames(applications))
  expect_true("first_name" %in% colnames(applications))
  expect_true("email" %in% colnames(applications))
  expect_equal(length(colnames(applications)[stringr::str_detect(colnames(applications), "^techniques_.+?")]), 13)
  expect_equal(length(colnames(applications)[stringr::str_detect(colnames(applications), "^skills.+?")]), 5)
  expect_equal(length(colnames(applications)[stringr::str_detect(colnames(applications), "^topics.+?")]), 8)
})

test_that("anonymization works for surveymonkey data", {
  applications <- load_applications_export("test_data/surveymonkey/applications_fake_export.csv", "CIT-10-2020") %>% 
      anonymize_applications()
  expect_false("email" %in% colnames(applications))
  expect_false("first_name" %in% colnames(applications))
  expect_false("last_name" %in% colnames(applications))
  expect_false("ip_address" %in% colnames(applications))
})