library(projectutils)
library(readr)
library(glue)
library(dplyr)
# download applications
PROJECT_ID <- "{{{project_id}}}"
SELECTED <- c({{{selected_ids}}})
project_id_lower <- tolower(PROJECT_ID)

data_folder <- here::here() # change this if your data folder is somewhere else than in project root
project_folder <- fs::path(data_folder, id_path(PROJECT_ID))

mapping <- readr::read_csv(glue::glue("{project_folder}/{project_id_lower}_mapping.csv"))

# selected 
mapping %>% get_application_emails(SELECTED)
mapping %>% get_application_emails(SELECTED, get_discarded = TRUE)