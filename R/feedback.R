#' get_feedback
#' get feedback for all projects or just a specific one
#' @param project_id character. ID of the project, e.g. ERL-03-2020. 
#' @param lang. character. Which language was used to collect the feedback. Defaults to "de".
#' @return responses to the feedback survey
#' @export
get_feedback <- function(project_id = NULL, lang = "de") {
  if (!lang %in% c("de", "en")) {
    usethis::ui_stop("Invalid language.")
  }
  if (lang == "en") {
    survey_id <- 292926300 # feedback survey for project teams
  } else {
    survey_id <- 283946752 # Feedback für Projektteams
  }
  
  survey_df <- survey_id %>% 
    surveymonkey::fetch_survey_obj() %>%
    surveymonkey::parse_survey() 
  survey_df <- survey_df %>% 
    janitor::clean_names() # initial clean up with janitor
  return(survey_df)
}
