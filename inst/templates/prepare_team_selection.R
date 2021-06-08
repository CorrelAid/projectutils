library(projectutils)
library(readr)
library(glue)
library(dplyr)
#set up 
PROJECT_ID <- "{{{project_id}}}"
data_folder <- here::here() # change this if your data folder is somewhere else than in project root
PROJECT_FOLDER <- fs::path(data_folder, PROJECT_ID, "team_selection")
ANON_PATH <- glue::glue("{PROJECT_FOLDER}/data/applications_anonymized.csv")

# download applications
appl <- load_applications(Sys.getenv("KOBO_PROJECT_ASSET_URL"), PROJECT_ID)

# mapping of ids to emails / names -> only for project coordinator / local
appl %>% 
  select(applicant_id, email, first_name) %>% 
  write_csv(glue::glue("{PROJECT_FOLDER}/data/mapping.csv"))

# anonymized data set (for report)
appl %>% 
  anonymize_applications() %>% 
  write_csv(ANON_PATH)

# knit report 
rmarkdown::render(glue::glue("{PROJECT_FOLDER}/{PROJECT_ID}_applications_report.Rmd"), 
                params = list(project_id = PROJECT_ID, anon_path = ANON_PATH))

# quick analytics
table(appl$gender)
table(appl$project_role)
table(appl$gender, appl$project_role)