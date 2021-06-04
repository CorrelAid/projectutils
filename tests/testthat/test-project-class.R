test_that("project class initialization errors when inputs are wrong", {
    expect_error(Project$new(project_id = 2020, "awesome project"), regexp = "project_id needs to be a character vector of length 1.")
    expect_error(Project$new(project_id = "2020-1", "awesome project"), regexp = "^Invalid project id. It needs to be conform to the following format: YYYY-mm-ABB where ABB")
    expect_error(Project$new(project_id = "202001", "awesome project"), regexp = "^Invalid project id. It needs to be conform to the following format: YYYY-mm-ABB where ABB")
    expect_error(Project$new(project_id = "2020-01-FOO", name = c("foo", "bar")), regexp = "name")

})


test_that("project class initialization works", {
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")
  expect_equal(class(proj), c("Project", "R6"))
  expect_equal(proj$name, "an awesome project")
  expect_equal(proj$project_id, "2020-01-FOO")
})

test_that("setting end_ym and end_ym_predicted works as expected", {
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")
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
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")
  proj$add_tag("internal")
  proj$add_tag("data", "process")
  proj$add_tag("lc", "berlin")
  proj$add_tag("lc", "berlin")

  tags <- proj$tags
  expect_equal(nrow(tags), 3)
  expect_equal(tags$tag_category, c("internal", "data", "lc"))
  expect_equal(tags$tag_value, c(NA, "process", "berlin"))
})

test_that("setting status throws error when invalid", {
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")

  expect_error(proj$set_status("project acquisition"), "Invalid status")
})

test_that("setting status works", {
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")
  # empty 
  expect_true(is.na(proj$status))
  expect_true(is.na(proj$status_id))

  # tibble column 
  expect_true(is.na(proj$to_tibble()$status))
  expect_true(is.na(proj$to_tibble()$status_id))

  proj$set_status("Ideation")
  expect_equal(proj$status_id, 2)
  expect_equal(proj$status, "Ideation")
})


test_that("adding local chapters works as expected", {
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")
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
  expect_equal(proj$get_sql_tables()$projectlocalchapter$lc_id, c(1, 13, 12))
})


test_that("adding project members works as expected", {
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")
  
  expect_equal(nrow(proj$project_members), 0)

  # can't add more than one member at a time
  expect_error(
    proj$add_project_member(c(1, 2), c("team_trainee", "team_member")), 
    regexp = "Must have length 1"
  )
  # invalid roles throw error
  expect_error(
    proj$add_project_member(1, "rolenotexist"), 
    regexp = "rolenotexist is not a valid role"
  )

  proj$add_project_member(1, "team_member")
  proj$add_project_member(2, "team_trainee")
  proj$add_project_member(3, "team_lead")

  expect_equal(nrow(proj$project_members), 3)
  expect_equal(ncol(proj$project_members), 11)
  expect_equal(proj$get_sql_tables()$projectmember$volunteer_id, c(1, 2, 3))
  # get a team member 
  expect_equal(proj$get_team_member(1)$volunteer_id , 1)
})


test_that("adding project members via a data frame works as expected", {
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")
  members <- tibble::tribble(
      ~volunteer_id, ~role,
      2, "team_trainee",
      43, "team_member", 
      92, "team_member", 
      42, "team_lead"
  )
  members$end_active_ym <- "2021-02"
  members %>% 
    purrr::pmap(proj$add_project_member)

  expect_equal(nrow(proj$project_members), 4)
  expect_equal(ncol(proj$project_members), 11)
  expect_equal(proj$project_members$end_active_ym %>% unique(), "2021-02")
})

test_that("data frame representation works", {
  proj <- Project$new(project_id = "2020-01-FOO", name = "an awesome project")
  proj_df <- proj$to_tibble()
  
  expect_equal(nrow(proj_df), 1)
  expect_equal(proj_df$project_id, "2020-01-FOO")
  expect_equal(proj_df$start_ym, "2020-01")
  expect_equal(nrow(proj_df$tags[[1]]), 0)
  expect_equal(nrow(proj_df$local_chapters[[1]]), 0)
  expect_equal(nrow(proj_df$project_members[[1]]), 0)
  expect_true(is.na(proj$status_id))
})