roles <- tibble::tribble(
    ~role_id, ~role,
    1, "team_trainee", 
    2, "team_member", 
    3, "team_lead"
)

usethis::use_data(roles, internal = FALSE, overwrite = TRUE)
