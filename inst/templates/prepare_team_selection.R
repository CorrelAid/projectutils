library(projectutils)
library(readr)
library(glue)
library(dplyr)
# download applications
PROJECT_ID <- "{{{project_id}}}"
EXPORT_CSV_FILE <- "{{{export_csv_file}}}" # backwards compatability with surveymonkey, will be deprecated soon
LANG <- "{{{lang}}}" # backwards compatability with surveymonkey, will be deprecated soon
project_id_lower <- tolower(PROJECT_ID)
project_id_path <- id_path(PROJECT_ID)
data_folder <- here::here() # change this if your data folder is somewhere else than in project root
project_folder <- fs::path(data_folder, project_id_path, "team_selection")

if (EXPORT_CSV_FILE == "") { # use Kobo API
  appl <- load_applications(Sys.getenv("KOBO_PROJECT_ASSET_URL"), PROJECT_ID)
} else {
  # export for backwards compatability with surveymonkey exports
  # will be deprecated soon
  path <- fs::path(project_folder, EXPORT_CSV_FILE)
  appl <- load_applications_export(path, PROJECT_ID, lang = LANG)
}

# mapping of ids to emails / names -> only for project coordinator / local
appl %>% 
  select(applicant_id, email, first_name) %>% 
  write_csv(glue::glue("{project_folder}/data/mapping.csv"))

# anonymized data set (for report)
ANON_PATH <- glue::glue("{project_folder}/data/applications_anonymized.csv")

appl %>% 
  anonymize_applications() %>% 
  write_csv(ANON_PATH)

# knit report 
project_id_path_lower <- tolower(project_id_path)
rmarkdown::render(glue::glue("{project_folder}/{project_id_path_lower}_applications_report.Rmd"), 
                params = list(project_id = PROJECT_ID, anon_path = ANON_PATH))

# quick analytics
table(appl$gender)
table(appl$project_role)
table(appl$gender, appl$project_role)