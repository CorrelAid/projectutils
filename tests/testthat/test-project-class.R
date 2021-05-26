test_that("project class initialization errors when inputs are wrong", {
    expect_error(Project$new(start_ym = 2020, abbreviation = "FOO", "awesome project"), regexp = "Assertion on 'start_ym' failed: Must be of type 'character', not 'double'.")
    expect_error(Project$new(start_ym = "2020-1", abbreviation = "FOO", "awesome project"), regexp = "Invalid format: start_ym needs to be set to")
    expect_error(Project$new(start_ym = "202001", abbreviation = "FOO", "awesome project"), regexp = "Invalid format: start_ym needs to be set to")

    # abbreviation 
    expect_error(Project$new(start_ym = "2020-01", abbreviation = 0, "awesome project"), regexp = "abbreviation")
    expect_error(Project$new(start_ym = "2020-01", abbreviation = "foobar", name = "awesome project"), regexp = "Invalid abbreviation")
    expect_error(Project$new(start_ym = "2020-01", abbreviation = "foo", name = "awesome project"), regexp = "Invalid abbreviation")
    expect_error(Project$new(start_ym = "2020-01", abbreviation = "FOO", name = c("foo", "bar")), regexp = "name")

})


test_that("project class initialization works", {
  proj <- Project$new(start_ym = "2020-01", abbreviation = "FOO", name = "an awesome project")
  expect_equal(class(proj), c("Project", "R6"))
  expect_equal(proj$name, "an awesome project")
  expect_equal(proj$project_id, "2020-01-FOO")
})

test_that("setting end_ym and end_ym_predicted works as expected", {
  proj <- Project$new(start_ym = "2020-01", abbreviation = "FOO", name = "an awesome project")
  expect_error({
    proj$end_ym <- "202013"
  }, regexp = "Invalid format: end_ym needs to be set to a character in the format YYYY-mm")

  expect_error({
    proj$start_ym <- NA
  }, regexp = "You can't set start_ym to NA")
    
  proj$end_ym <- NA
  proj$end_ym <- "2021-04"
  

})

test_that("adding and retrieving tags works", {
  proj <- Project$new(start_ym = "2020-01", abbreviation = "FOO", name = "an awesome project")
  proj$add_tag("internal")
  proj$add_tag("data", "process")
  proj$add_tag("lc", "berlin")
  proj$add_tag("lc", "berlin")

  tags <- proj$tags
  expect_equal(nrow(tags), 3)
  expect_equal(tags$category, c("internal", "data", "lc"))
  expect_equal(tags$value, c(NA, "process", "berlin"))
})

test_that("setting status throws error when invalid", {
  proj <- Project$new(start_ym = "2020-01", abbreviation = "FOO", name = "an awesome project")

  expect_error(proj$set_status("project acquisition"), "Invalid status")
})

test_that("setting status works", {
  proj <- Project$new(start_ym = "2020-01", abbreviation = "FOO", name = "an awesome project")
  proj$set_status("Ideation")
  expect_equal(proj$status_id, 2)  
})


test_that("adding local chapters works as expected", {
  proj <- Project$new(start_ym = "2020-01", abbreviation = "FOO", name = "an awesome project")
  # can't add more than one chapter at a time
  expect_error(
    proj$add_local_chapter(c("berlin", "munich")), regexp = "Must have length 1"
  )
  # invalid chapters throw error
  expect_error(
    proj$add_local_chapter("does not exist"), "does not exist is not valid"
  )

  proj$add_local_chapter("berlin")
  expect_warning(proj$add_local_chapter("berlin"), regexp = "berlin is already added")
  proj$add_local_chapter("rhein-main")
  proj$add_local_chapter("paris")

  expect_equal(nrow(proj$local_chapters), 3)
  expect_equal(proj$get_sql_tables()$projectlocalchapters$lc_id, c(1, 13, 12))
})


test_that("data frame representation works", {
  proj <- Project$new(start_ym = "2020-01", abbreviation = "FOO", name = "an awesome project")
  proj_df <- proj$to_tibble()
  
  expect_equal(nrow(proj_df), 1)
  expect_equal(proj_df$project_id, "2020-01-FOO")
  expect_equal(proj_df$start_ym, "2020-01")
  expect_equal(nrow(proj_df$tags[[1]]), 0)
  expect_equal(nrow(proj_df$local_chapters[[1]]), 0)
  expect_true(is.na(proj$status_id))
})
