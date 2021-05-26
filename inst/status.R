# get board columns from GitHub
# descriptions are not available there so we have to add those manually at somem point
projects_custom_accept_header <- "application/vnd.github.inertia-preview+json"
gh_project <- gh::gh("/repos/CorrelAid/projects/projects", .send_headers = c(
  Accept = projects_custom_accept_header
))

columns_url <- gh_project %>% 
  purrr::keep(function(proj) proj$name == "Projects") %>% 
  purrr::map_chr("columns_url")

columns <- gh::gh(columns_url, .send_headers = c(Accept = projects_custom_accept_header))


status <- tibble::tibble(
  status = columns %>% 
    purrr::map_chr("name")
) %>% 
  dplyr::mutate(status_id = dplyr::row_number(),
                status_description = "") 

# for manual lookup
status %>% 
  readr::write_csv("inst/status.csv")

# make available as tibble in the package
usethis::use_data(status, internal = FALSE, overwrite = TRUE)
