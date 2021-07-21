library(projectutils)
library(readr)
library(glue)
library(dplyr)

PROJECT_ID <- "{{{project_id}}}"
SELECTED <- c()
project_id_lower <- tolower(PROJECT_ID)

data_folder <- here::here() # change this if your data folder is somewhere else than in project root
project_folder <- fs::path(data_folder, PROJECT_ID, "team_selection")

mapping <- readr::read_csv(glue::glue("{project_folder}/data/mapping.csv"))

# get emails and copy to clipboard
(mapping %>% get_application_emails(SELECTED))
(mapping %>% get_application_emails(SELECTED, get_discarded = TRUE))
