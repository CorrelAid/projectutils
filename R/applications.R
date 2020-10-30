#' load project applications 
#' @description loads the applications for a given project
#' @param project_id character. ID of the project, e.g. ERL-03-2020. 
#' @param lang. character. Which language was used to collect the applications. Defaults to "en" for the "application for correlaid projects (en)" form. "de" corresponds to "Bewerbungsformular für Projektteams (de)" form.
#' @return data frame containing the responses to the questions
#' @export
#' @importFrom surveymonkey fetch_survey_obj parse_survey 
#' @importFrom dplyr filter
load_project_applications <- function(project_id, lang = "en") {
  if (!lang %in% c("en", "de")) {
    usethis::ui_stop("lang must be either 'de' or 'en'.")
  }
  
  if (lang == "en") {
    survey_id <- 287163371 # application for correlaid projects (en)
  } else {
    survey_id <- 284359289 # Bewerbungsformular für Projektteams
  }
  
  survey_df <- survey_id %>% 
    surveymonkey::fetch_survey_obj() %>%
    surveymonkey::parse_survey() %>% 
    clean_application_colnames(lang = lang) %>% 
    dplyr::filter(project_id == project_id) %>% 
    dplyr::mutate(applicant_id = 1:dplyr::n()) %>%  # give participant integer id
    dplyr::select(applicant_id, dplyr::everything())
  return(survey_df)
}

# helper function
clean_application_colnames <- function(survey_df, lang) {
  survey_df <- survey_df %>% 
    janitor::clean_names() # initial clean up with janitor
  if (lang == "en") {
    # clean english column names
    survey_df <- survey_df %>% 
      tidyr::separate(which_project_would_you_like_to_apply_for, c("project_id", "project_title"), sep = ":") # extract project id

  } else {
    # TODO once we have collected some responses in german, fix the column name here.
  }
  return(survey_df)
}

#'
#'@export
anonymize_applications <- function(survey_df, lang) {
  if (lang == "en") {
    survey_df <- cit %>% 
      dplyr::select_if(stringr::str_detect(colnames(.), "email") == FALSE) %>% 
      dplyr::select_if(stringr::str_detect(colnames(.), "name") == FALSE) %>% 
      dplyr::select(-ip_address)
  } else {
    # TODO: implement for german
  }
  return(survey_df)
}

#' @export
extract_motivation <- function(survey_df, lang) {
  if (lang == "en") {
  motivation <- cit %>% 
    dplyr::select(applicant_id, skills_text = please_describe_here_which_of_your_skills_particularly_qualify_you_for_the_participation_in_this_project_max_5_sentences, 
           motivation_text = please_describe_here_why_you_want_to_get_involved_in_this_project_max_5_sentences)
  } else {
    # TODO
  }
  md_text <- glue::glue("## Applicant {motivation$applicant_id} \n ### What skills qualify you? \n {motivation$skills_text} \n ### Why do you want to get involved? \n {motivation$motivation_text}")
  md_text %>% readr::write_lines("2020-10-cit_motivation.md")
}