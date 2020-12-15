library(projectutils)
library(readr)
library(glue)
library(dplyr)
# download applications
PROJECT_ID <- "{{{project_id}}}"
project_id_lower <- tolower(PROJECT_ID)

data_folder <- here::here() # change this if your data folder is somewhere else than in project root
project_folder <- fs::path(data_folder, id_path(PROJECT_ID), "team_selection")
appl <- load_applications(PROJECT_ID)

# mapping of ids to emails / names -> only for project coordinator / local
appl %>% 
  select(applicant_id, email, first_name) %>% 
  write_csv(glue::glue("{project_folder}/{project_id_lower}_mapping.csv"))

# motivation questions for team selection committee
appl %>% 
  extract_motivation_questions() %>% 
  write_lines(glue("{project_folder}/{project_id_lower}_applications_motivation.md"))

# anonymized version for team selection committee
appl %>% 
  anonymize_applications() %>% 
  select(-starts_with("motivation_")) %>% # drop long-text variables bc they blow up the csv
  write_csv(glue("{project_folder}/{project_id_lower}_applications_anonymized.csv"))

# quick analytics
table(appl$gender)
table(appl$project_role)
table(appl$gender, appl$project_role)
