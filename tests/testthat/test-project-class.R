test_that("project class initialization errors when inputs are wrong", {
    expect_error(Project$new(project_id = 2020), regexp = "Assertion on 'value' failed: Must be of type 'character', not 'double'.")
    expect_error(Project$new(project_id = "2020-1"), regexp = "Invalid format: it needs to be in the form")
    expect_error(Project$new(project_id = "202001"), regexp = "Invalid format: it needs to be in the form")

    # abbreviation 
    expect_error(Project$new(start_ym = "2020-01", abbreviation = 0), regexp = "abbreviation")
    expect_error(Project$new(start_ym = "2020-01", abbreviation = "foobar"), regexp = "Invalid abbreviation")
    expect_error(Project$new(start_ym = "2020-01", abbreviation = "foo"), regexp = "Invalid abbreviation")
    expect_error(Project$new(start_ym = "2020-01", abbreviation = "FOO", name = c("foo", "bar")), regexp = "name")

})


test_that("project class initialization works", {
  proj <- Project$new(start_ym = "2020-01", abbreviation = "FOO", name = "an awesome project")
  expect_equal(class(proj), c("Project", "R6"))
  expect_equal(proj$name, "an awesome project")
  expect_equal(proj$project_id, "2020-01-FOO")
})

