test_that("converting ids works", {
  expect_equal(id_path("CIT-11-2020"), "2020-11-CIT")
  expect_equal(id_surveymonkey("2020-11-CIT"), "CIT-11-2020")
})
