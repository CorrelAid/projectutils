library(magrittr)
library(projectutils)
PROJECT_ID <- "{{{project_id}}}"


#read from rds file
proj %>% readr::read_rds(glue::glue("{PROJECT_ID}.rds"))
```

# Project members --------

# Project team members
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

proj$project_members


# write to rds file
proj %>% readr::write_rds(glue::glue("{PROJECT_ID}.rds"))