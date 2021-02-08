utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

#' load project applications 
#' @description loads the applications for a given project
#' @param project_id character. ID of the project, e.g. ERL-03-2020. Defaults to NULL which means not to filter by project
#' @param lang character. Which language was used to collect the applications. Defaults to "en" for the "application for correlaid projects (en)" form. "de" corresponds to "Bewerbungsformular für Projektteams (de)" form.
#' @return data frame containing the responses to the questions
#' @export
#' @importFrom surveymonkey fetch_survey_obj parse_survey 
#' @importFrom dplyr filter
#' @importFrom rlang .data
load_applications <- function(project_id = NULL, lang = "en") {
  if (!lang %in% c("en", "de")) {
    usethis::ui_stop("lang must be either 'de' or 'en'.")
  }
  
  if (lang == "en") {
    survey_id <- 287163371 # application for correlaid projects (en)
  } else {
    survey_id <- 284359289 # Bewerbungsformular für Projektteams
  }
  survey_df <- survey_id %>% 
    get_surveymonkey() %>% 
    clean_application_colnames(lang = lang)
  
  if (!is.null(project_id)) {
    proj_id <- project_id
    survey_df <- survey_df %>% 
      dplyr::filter(.data$project_id == proj_id)
  }
  
  survey_df <- survey_df %>% 
    dplyr::mutate(applicant_id = 1:dplyr::n()) %>%  # give participant integer id
    dplyr::select(.data$applicant_id, .data$gender, dplyr::everything())

  return(survey_df)
}

#' load project applications from export
#' @description loads the applications for a given project from the csv export
#' @param export_csv_path character. path to csv export from surveymonkey containing the applications 
#' @param project_id character. ID of the project, e.g. ERL-03-2020. Defaults to NULL which means not to filter by project
#' @param lang character. Which language was used to collect the applications. Defaults to "en" for the "application for correlaid projects (en)" form. "de" corresponds to "Bewerbungsformular für Projektteams (de)" form.
#' @return data frame containing the responses to the questions
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
load_applications_export <- function(export_csv_path, project_id = NULL, lang = "en") {
  if (!lang %in% c("en", "de")) {
    usethis::ui_stop("lang must be either 'de' or 'en'.")
  }
  
  export_raw <- readr::read_csv(export_csv_path)
  export_raw <- export_raw %>% 
    dplyr::select(-.data$`First Name`, -.data$`Last Name`, -.data$`Email Address`)

  # the exported format has two rows for the column names :facepalm:
  # do some cleaning
  colnames_row_1 <- colnames(export_raw)
  # answer options for dropdowns and matrizes have X in the first row (except for the first option), the actual value in the second row
  # replace first row with NA
  colnames_row_1[stringr::str_detect(colnames_row_1, "^X\\d{1,2}$")] <- NA 
  colnames_row_1 <- data.frame(colnames_row_1 = colnames_row_1) %>% tidyr::fill(colnames_row_1) %>% dplyr::pull(colnames_row_1) # fill NA with previous question
  # add "em" to the technologies and tools question to emulate the API export, see rename_programming_en
  colnames_row_1 <- ifelse(stringr::str_detect(colnames_row_1, ".+?functions and packages\\.$"),
                            paste(colnames_row_1, "em", sep = " "), 
                            colnames_row_1)

  # promote second row to colnames and simultaneously drop it from the data
  survey_df <- export_raw %>% 
           dplyr::slice(2:dplyr::n())
          
  colnames_row_2 <- export_raw %>% 
           dplyr::slice(1) %>% 
           unlist(., use.names=FALSE)

  # combine
  # answer format not important
  colnames_row_2[is.na(colnames_row_2) | colnames_row_2 == "Response" | colnames_row_2 == "Open-Ended Response"] <- ""
  colnames_combined <- paste(colnames_row_1, colnames_row_2, sep = " ")
  colnames(survey_df) <- colnames_combined
  
  # cleaning with janitor (to snakecase, remove special chars) and our own custom function 
  survey_df <- survey_df %>% 
    janitor::clean_names() %>% 
    clean_application_colnames(lang = lang)
  
  if (!is.null(project_id)) {
    proj_id <- project_id
    survey_df <- survey_df %>% 
      dplyr::filter(.data$project_id == proj_id)
  }
  
  survey_df <- survey_df %>% 
    dplyr::mutate(applicant_id = 1:dplyr::n()) %>%  # give participant integer id
    dplyr::select(.data$applicant_id, .data$gender, dplyr::everything())
  
  return(survey_df)
}

#' anonymize_applications
#' @param survey_df tibble. Tibble with the applications. 
#' @importFrom rlang .data
#' @export
anonymize_applications <- function(survey_df) {
  survey_df <- survey_df %>% 
    dplyr::select(-.data$email, -.data$first_name, -.data$ip_address)
  return(survey_df)
}

#' extract_motivation_questions
#' @param survey_df tibble. Tibble with the applications. 
#' @importFrom rlang .data
#' @export
extract_motivation_questions <- function(survey_df) {

  motivation <- survey_df %>% 
    dplyr::select(.data$applicant_id, skills_text = .data$motivation_skills, 
           motivation_text = .data$motivation_why_involved)
  md_text <- glue::glue("## Applicant {motivation$applicant_id} \n ### What skills qualify you? \n {motivation$skills_text} \n ### Why do you want to get involved? \n {motivation$motivation_text}")
  md_text
}


#' get_application_emails
#' @param mapping_df tibble. Tibble with the mapping from applicant id to name & email
#' @param selected_ids numeric. vector with numeric ids of those who were selected from the team.
#' @param get_discarded boolean. whether to invert the selection in order to get the email addresses of those who didn't make the team. defaults to FALSE.
#' @return character. invisibly returns vector of email addresses.
#' @description if clipr is available writes ; separated string of email addresses to the clipboard ready to copy into outlook
#' @export
#' @importFrom rlang .data
get_application_emails <- function(mapping_df, selected_ids, get_discarded = FALSE) {
  if (get_discarded) {
    emails <- mapping_df %>% 
      dplyr::filter(!.data$applicant_id %in% selected_ids) %>% 
      dplyr::pull(.data$email)
  } else {
    emails <- mapping_df %>% 
      dplyr::filter(.data$applicant_id %in% selected_ids) %>% 
      dplyr::pull(.data$email)
  }
  
  if (clipr::clipr_available()) {
    emails_str <- emails %>% paste(collapse = ";")
    clipr::write_clip(emails_str)
    usethis::ui_done(glue::glue("Wrote to clipboard: \n{emails_str}"))
  }
  invisible(emails)
}

#'Use team selection workflow
#'@param project_id_path project id in path form, e.g. 2020-11-COR
#'@param data_folder character. path to data folder starting at root of the project. defaults to ".", i.e. root
#'@param export_csv_file character. file name of csv export file within the team_selection folder. Defaults to NULL, i.e. using the API to get the data. 
#'@export 
use_team_selection_workflow <- function(project_id_path, data_folder = ".", export_csv_file = NULL) {
  
  
  # create subfolder for team selection if not already there
  team_selection_folder <- fs::path(data_folder, project_id_path, "team_selection")
  if (!dir.exists(team_selection_folder)) {
    dir.create(team_selection_folder)
  }
  
  # use templates
  project_id <- id_surveymonkey(project_id_path) # needed to download from surveymonkey
  usethis::use_template(
    "01_prepare_team_selection.R",
    save_as = fs::path(team_selection_folder, "01_prepare_team_selection.R"),
    data = list(project_id = project_id, export_csv_file = export_csv_file),
    package = "projectutils",
    open = TRUE
  )
  
  usethis::use_template(
    "02_get_application_emails.R",
    save_as = fs::path(team_selection_folder, "02_get_application_emails.R"),
    data = list(project_id = project_id),
    package = "projectutils",
    open = TRUE
  )
}

rename_techniques_en <- function(col_name) {
  stringr::str_replace(col_name, ".+following_techniques_(.+?)$", "techniques_\\1")
}

rename_topics_en <- function(col_name) {
  stringr::str_replace(col_name, ".+following_topics_(.+?)$", "topics_\\1")
}

rename_programming_en <- function(col_name) {
  # this question has the explanation of the scale before the actual question
  # the explanation is in italic which appears as "em" in the variable name
  # after the last "em" there is the value of the row
  stringr::str_replace(col_name, ".+em_(.+?)$", "skills_\\1")
}

#' clean_application_colnames
#' @param survey_df tibble. Tibble with the applications. 
#' @param lang character. Which language was used to collect the applications. Either "en" or "de". Defaults to "en".
#' @return tibble with clean column names
#' @importFrom dplyr contains
#' @importFrom rlang .data
#' @description snakecases column names and splits the title into "project_id" and "project_title"
clean_application_colnames <- function(survey_df, lang = "en") {
  if (!lang %in% c("en", "de")) {
    usethis::ui_stop("lang must be either 'de' or 'en'.")
  }
  
  survey_df <- survey_df %>% 
    janitor::clean_names() %>% # initial clean up with janitor
    dplyr::mutate(dplyr::across(where(is.factor), as.character))
  
  if (lang == "en") {
    # clean english column names
    survey_df <- survey_df %>% 
      tidyr::separate(.data$which_project_would_you_like_to_apply_for, c("project_id", "project_title"), sep = ":") %>% # extract project id
      dplyr::mutate(gender = dplyr::coalesce(!!! dplyr::select(., dplyr::contains("what_is_your_gender"))))
    
    # drop original gender variables
    survey_df <- survey_df %>% 
      dplyr::select(-contains("what_is_your_gender"))
    
    # rename column names
    survey_df <- survey_df %>% 
      dplyr::rename_with(rename_programming_en, dplyr::contains("experience_with_the_following_technologies")) %>% 
      dplyr::rename_with(rename_techniques_en, dplyr::contains("experience_with_the_following_techniques")) %>% 
      dplyr::rename_with(rename_topics_en, dplyr::contains("experience_with_the_following_topics")) %>% 
      dplyr::rename_with(~ "consent_privacy_policy", dplyr::contains("consent_to_privacy_policy")) %>% 
      dplyr::rename_with(~ "motivation_why_involved", dplyr::contains("why_you_want_to_get_involved")) %>% 
      dplyr::rename_with(~ "motivation_skills", dplyr::contains("what_skills_you_would_bring")) %>% 
      dplyr::rename_with(~ "project_role", dplyr::contains("what_role_do_you_want_to_assume")) %>% 
      dplyr::rename_with(~ "first_name", dplyr::contains("first_name")) %>% 
      dplyr::rename_with(~ "email", dplyr::contains("which_email_address"))
  } else {
    usethis::ui_stop("German is currently not supported.")
    # TODO once we have collected some responses in german, fix the column name here.
  }
  
  # trim character variables
  survey_df <- survey_df %>% 
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim))
  return(survey_df)
}
