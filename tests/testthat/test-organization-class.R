test_that("Organization class initialization errors when inputs are wrong", {
    expect_error(Organization$new(organization_id = 2020, "awesome Organization"), regexp = "Assertion on 'organization_id' failed")
    expect_error(Organization$new(organization_id = "FOOOOO", "awesome Organization"), regexp = "Assertion on 'organization_id' failed")
})


test_that("Organization class initialization works", {
  proj <- Organization$new(organization_id = "FOO", "an awesome Organization")
  expect_equal(class(proj), c("Organization", "R6"))
  expect_equal(proj$organization_name, "an awesome Organization")
  expect_equal(proj$organization_id, "FOO")
})

test_that("setting about works as expected", {
  proj <- Organization$new(organization_id = "FOO", "an awesome Organization")
  proj$about <- list(de = 'cool cool', en = 'nice')
  expect_mapequal(proj$about, list(de = 'cool cool', en = 'nice'))
})

test_that("tibble serializaton works", {
  proj <- Organization$new(organization_id = "FOO", "an awesome Organization")
  proj$about <- list(de = 'cool cool', en = 'nice')

  proj_df <- proj$to_tibble()
  expect_equal(nrow(proj_df), 1)
})
