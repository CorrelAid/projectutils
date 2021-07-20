library(magrittr)
library(projectutils)
PROJECT_ID <- "{{{project_id}}}"

# GET DATA FROM GITHUB ISSUE ----------
gh_data <- get_gh_issue_data(PROJECT_ID)


get_description_section <- function(section) {
    de <- readr::read_lines(here::here(PROJECT_ID, glue::glue("de/{section}.md"))) %>% paste(collapse = "\n")
    en <- readr::read_lines(here::here(PROJECT_ID, glue::glue("en/{section}.md"))) %>% paste(collapse = "\n")
    list(en = en, de = de)
}

# CREATE PROJECT OBJECT ------------
proj <- Project$new(PROJECT_ID, gh_data$name)
proj$num_gh_issue <- gh_data$num_gh_issue
# properties set through the constructor -----
# only uncomment if you want to change something
# you can also create the object again with correct parameters
# proj$name <- "an awesome new title" 
# proj$start_ym <- "2021-03"
# proj$project_id <- "2021-03-HAC"

# ORGANIZATION - delete if internal project
org <- Organization$new(organization_id = 'EXA',
                        organization_name = 'Example org',
                        website = 'https://example.org')
org$about <- get_description_section('00_about')
proj$organization <- org 
# basic properties ---------------
proj$end_ym <- NA
proj$end_ym_predicted <- NA
proj$team_size <- 4

# misc
proj$is_internal <- FALSE
proj$slack_channel <- "foochannel"
# urls
proj$url_git_repo <- "https://github.com/CorrelAid/foobar"

# pad links
print(gh_data$pad_links)
proj$url_ideation_pad <- "https://pad.correlaid.org/aaaaaaaaaaaaaaaaa"
proj$url_call_pad <- "https://pad.correlaid.org/aaaaaaaaaaaaaaaaa"

# publish to website
proj$is_public <- FALSE

# RELATED TABLES ----------------
# set status
proj$set_status(get_project_status(proj$num_gh_issue))
proj$status_id

# set local chapters
gh_data$lc
gh_data$lc %>% 
     purrr::walk(function(lc) proj$add_local_chapter(lc_name = lc))
proj$local_chapters

# add tags
gh_data$tags
gh_data$tags %>% 
     purrr::pmap(function(category, value) proj$add_tag(category, value))
proj$tags



# PROJECT DESCRIPTION ----------
desc <- Description$new(PROJECT_ID)
desc$title <- list(
     de = "",
     en = ""
)
desc$summary <- get_description_section("00_summary")
desc$problem <- get_description_section("01_problem")
desc$data <- get_description_section("02_data")
desc$approach <- get_description_section("03_approach")
desc$impact <- get_description_section("04_impact")
desc$further_links <- list(
     de = "",
     en = ""
)
proj$description <- desc


# TIBBLE AND TABLES -----
# tibble representation of a project
proj$to_tibble()$tags
proj$to_tibble() %>% dplyr::glimpse()

# tables
proj$get_sql_tables()


# SAVE 
# write to rds file
proj %>% readr::write_rds(here::here(PROJECT_ID, glue::glue("{PROJECT_ID}.rds")))

