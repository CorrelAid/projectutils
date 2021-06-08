test_that("converting ids works", {
  expect_equal(id_path("CIT-11-2020"), "2020-11-CIT")
  expect_equal(id_surveymonkey("2020-11-CIT"), "CIT-11-2020")
})

test_that("extracting project ids from kobo column names works for both project id formats", {
  test_cnames <- c("project_role_car_04_2021", "project_role_kob_04_2021", "project_role_2021_04_kob")
  expect_equal(extract_ids_from_kobo_columnnames(test_cnames), c('2021-04-CAR', '2021-04-KOB', '2021-04-KOB'))
})