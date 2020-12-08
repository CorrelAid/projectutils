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

test_that("renaming skill variables works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  mockery::stub(load_applications, 'get_surveymonkey', test_data)
  applications <- load_applications("CIT-10-2020")
  expect_true("skills_r" %in% colnames(applications))
  expect_true("skills_python" %in% colnames(applications))
  expect_true("skills_sql" %in% colnames(applications))
  expect_true("skills_javascript_frontend" %in% colnames(applications))
  expect_true("skills_html_css" %in% colnames(applications))
  expect_true("skills_node_js_javascript_backend" %in% colnames(applications))
})

test_that("renaming techniques variables works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  mockery::stub(load_applications, 'get_surveymonkey', test_data)
  applications <- load_applications("CIT-10-2020")
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

test_that("renaming topics variables works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  mockery::stub(load_applications, 'get_surveymonkey', test_data)
  applications <- load_applications("CIT-10-2020")
  expect_true("topics_development_of_theories_of_change" %in% colnames(applications))
  expect_true("topics_development_of_indicators" %in% colnames(applications))
  expect_true("topics_research_design" %in% colnames(applications))
  expect_true("topics_survey_design" %in% colnames(applications))
  expect_true("topics_data_protection" %in% colnames(applications))
  expect_true("topics_data_security" %in% colnames(applications))
  expect_true("topics_working_agile" %in% colnames(applications))
  expect_true("topics_project_management" %in% colnames(applications))
})


test_that("renaming other variables works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  mockery::stub(load_applications, 'get_surveymonkey', test_data)
  applications <- load_applications("CIT-10-2020")
  expect_true("consent_privacy_policy" %in% colnames(applications))
  expect_true("motivation_skills" %in% colnames(applications))
  expect_true("motivation_why_involved" %in% colnames(applications))
  expect_true("project_role" %in% colnames(applications))
  expect_true("gender" %in% colnames(applications))
  expect_true("first_name" %in% colnames(applications))
  expect_true("email" %in% colnames(applications))
})

test_that("coalescing gender works", {
  test_data <- readr::read_csv("test_data/surveymonkey/applications_fake_en.csv")
  mockery::stub(load_applications, 'get_surveymonkey', test_data)
  applications <- load_applications("CIT-10-2020")
  print(applications$gender %>% unique())
  expect_setequal(c("Male", "Female", "Non-binary", "I do not want to disclose my gender", "agender"), unique(applications$gender))
})