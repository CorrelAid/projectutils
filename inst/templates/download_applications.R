library(projectutils)
library(readr)
library(glue)
# download applications
PROJECT_ID <- "{{{project_id}}}"
project_id_lower <- tolower(PROJECT_ID)

data_folder <- here::here() # change this if your data folder is somewhere else than in project root
project_folder <- fs::path(data_folder, id_path(PROJECT_ID))
appl <- load_applications(PROJECT_ID)

appl %>% 
  anonymize_applications() %>% 
  write_csv(glue("{project_folder}/{project_id_lower}_applications_anonymized.csv"))

appl %>% 
  extract_motivation_questions() %>% 
  write_lines(glue("{project_folder}/{project_id_lower}_applications_motivation.md"))

# quick analytics
table(appl$gender)
table(appl$project_role)
table(appl$gender, appl$project_role)
