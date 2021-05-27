test_that("project member class initialization errors when inputs are not valid", {

    expect_error(ProjectMember$new(), regexp = "^argument \"volunteer_id\" is missing, with no default")

    # invalid project id
    expect_error(ProjectMember$new(project_id = "dlfjwelr", volunteer_id = 1, role = "team_trainee"), regexp = "^Invalid project id.")
    expect_error(ProjectMember$new(project_id = c("2019-01-FOO", "2019-01-FOO"), volunteer_id = 1, role = "team_trainee"), regexp = "project_id needs to be a character vector of length 1.")

    # invalid role
    expect_error(ProjectMember$new(project_id = "2019-01-FOO", volunteer_id = 1, role = "team clown"), regexp = "^team clown is not a valid role.")
   
    # volunteer id
    expect_error(ProjectMember$new(project_id = "2019-01-FOO", volunteer_id = "1", role = "team_trainee"), regexp = "number")


})

test_that("project member class initialization works", {
    pm <- ProjectMember$new(project_id = "2019-01-FOO", volunteer_id = 1, role = "team_trainee")
    expect_equal(pm$project_id, "2019-01-FOO")
    expect_equal(pm$volunteer_id, 1)
    expect_equal(pm$role_id, 1)
})

test_that("setting project member behaviour works as expected", {
    pm <- ProjectMember$new(project_id = "2019-01-FOO", volunteer_id = 1, role = "team_trainee")
    expect_error(
        pm$set_behaviour(flag = TRUE),
        regexp = "You need to pass a description"
    )
    pm$set_behaviour(flag = TRUE, description = "just left without telling someone")
    expect_true(pm$behaviour_flag)
    expect_match(pm$behaviour_description, "just left without")
    
})
