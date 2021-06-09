utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

#' load project applications from Kobo
#' @description loads the applications for a given project
#' @param asset_url character. URL of the Kobo asset corresponding to the survey.
#' @param project_id character. ID of the project, e.g. 2020-04-ERL. Defaults to NULL which means not to filter by project
#' @return data frame containing the responses to the questions
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
load_applications <- function(asset_url, project_id = NULL) {

  survey_list <- get_kobo(asset_url)

  clean_kobo(survey_list, project_id = project_id)
}

#' convert applications into tibble
#' @description cleans the list of applications into a tibble in long format (one person-application row)
#' @param survey_list list list of applications returned by get_kobo
#' @param project_id character. ID of the project, e.g. 2020-04-ERL.
#' @return data frame containing the responses to the questions
#' @export
clean_kobo <- function(survey_list, project_id) {
  
  survey_df <- survey_list %>%
    purrr::map_dfr(function(sub) purrr::compact(sub) %>%  tibble::as_tibble()) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(applicant_id = .data$id,
                  motivation_why_involved = .data$motivation_why)
  
  # create self-id column if it does not exist (because nobody used it)
  if (!"gender_self_identification" %in% colnames(survey_df)) {
    survey_df$gender_self_identification <- NA
  }
  # DATA CLEANING - this could probably be done more efficiently
  # tibble with project ids the person applied to (1 row per person-project)
  project_ids_df <- survey_df %>% 
    dplyr::select(.data$applicant_id, .data$project_id) %>% 
    dplyr::mutate(applied_to = .data$project_id) %>% # store this before separating the rows
    tidyr::separate_rows(.data$project_id, sep = " ") %>% 
    dplyr::mutate(project_id = unify_project_id_formats(.data$project_id)) %>% 
    dplyr::distinct()
  
  # tibble with project roles (1 person-project row)
  project_roles_df <- survey_df %>% 
    dplyr::select(.data$applicant_id, dplyr::starts_with("project_role")) %>% 
    tidyr::pivot_longer(dplyr::starts_with("project_role"), names_to = "project_id", values_to = "project_role")%>% 
    dplyr::mutate(project_id = extract_ids_from_kobo_columnnames(.data$project_id)) %>% 
    dplyr::filter(.data$project_role != "DNA") %>% # TODO: proper checking that applicants did not mess up here. 
    dplyr::distinct()
  
  # 1 person row
  personal_info_df <- survey_df %>% 
    dplyr::select(.data$applicant_id, dplyr::starts_with("gender"), .data$first_name, .data$last_name, email = .data$email_address,
                  .data$german_skills, dplyr::starts_with("rating"), dplyr::starts_with("motivation"),
                  .data$consent_privacy_policy) %>% 
    dplyr::distinct() %>% 
    janitor::clean_names() %>% 
    dplyr::rename_with(~stringr::str_replace_all(.x, "rating_technologies_tools", "skills"), dplyr::starts_with("rating_technologies_tools")) %>% 
    dplyr::rename_with(~stringr::str_replace_all(.x, "rating_", ""), dplyr::starts_with("rating"))
  
  # coalesce gender columns into one
  personal_info_df <- personal_info_df %>% 
    dplyr::mutate(gender = dplyr::if_else(.data$gender == "self_identification", NA_character_, .data$gender))
  personal_info_df$gender <- dplyr::coalesce(personal_info_df$gender,
                                             personal_info_df$gender_self_identification)
  
  # join 
  cleaned_df <- project_ids_df %>% 
    dplyr::left_join(project_roles_df, by = c("applicant_id", "project_id")) %>% 
    dplyr::left_join(personal_info_df, by = "applicant_id")
  
  # filter if the user wants to filter 
  if (!is.null(project_id)) {
    proj_id <- project_id
    cleaned_df <- cleaned_df %>% 
      dplyr::filter(.data$project_id == proj_id)
  }
  
  if (nrow(cleaned_df) == 0) {
    usethis::ui_warn(glue::glue("No applicants present after filtering for {project_id}. Did you specify the ID in the correct format?"))
  }
  
  cleaned_df <- cleaned_df %>% 
    dplyr::select(.data$applicant_id, .data$gender, dplyr::everything())
  
  return(cleaned_df)
}
#' anonymize_applications
#' @param survey_df tibble. Tibble with the applications. 
#' @importFrom rlang .data
#' @export
anonymize_applications <- function(survey_df) {
  survey_df <- survey_df %>% 
    dplyr::select(-.data$email, -.data$first_name)
  
  # backwards compatability for surveymonkey 
  # last_name is only in Kobo data -> we can only remove it if it's there
  # otherwise select will throw an error 
  if ("last_name" %in% colnames(survey_df)) {
    survey_df$last_name <- NULL
  }

  if ("ip_address" %in% colnames(survey_df)) {
    survey_df$ip_address <- NULL
  }
  return(survey_df)
}

#' extract_motivation_questions
#' @param survey_df tibble. Tibble with the applications. 
#' @param lang character. Which language was used to collect the applications. Either "en" or "de". Defaults to "en".
#' @importFrom rlang .data
#' @export
extract_motivation_questions <- function(survey_df, lang = "en") {
  if (!lang %in% c("en", "de")) {
    usethis::ui_stop("lang must be either 'de' or 'en'.")
  }

  motivation <- survey_df %>% 
    dplyr::select(.data$applicant_id, skills_text = .data$motivation_skills, 
           motivation_text = .data$motivation_why_involved)
  if (lang == "en") {
    md_text <- glue::glue("## Applicant {motivation$applicant_id} \n ### What skills qualify you? \n {motivation$skills_text} \n ### Why do you want to get involved? \n {motivation$motivation_text}")
  } else {
    md_text <- glue::glue("## Bewerber:in {motivation$applicant_id} \n ### Bitte beschreibe hier, welche Deiner Faehigkeiten Dich besonders fuer die Teilnahme an diesem Projekt qualifizieren \n {motivation$skills_text} \n ### Bitte beschreibe hier, warum Du Dich fuer dieses Projekt engagieren moechtest  \n {motivation$motivation_text}")
  }
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
#'@param project_id project id in path form, e.g. 2020-11-COR
#'@param data_folder character. path to data folder starting at root of the project. defaults to ".", i.e. root
#'@export 
use_team_selection_workflow <- function(project_id, data_folder = ".") {
  
  
  # create subfolder for team selection if not already there
  team_selection_folder <- fs::path(data_folder, project_id, "team_selection")
  if (!dir.exists(team_selection_folder)) {
    dir.create(team_selection_folder)
    dir.create(fs::path(team_selection_folder, "data"))
  }
  
  # use templates
  # we add the three-letter prefix 
  # to make file names distinguishable in RStudio when more then one project is managed at the same time
  prefix <- tolower(stringr::str_extract(project_id, '[:alpha:]+')) 
  usethis::use_template(
    "send_confirmation_emails.R",
    save_as = fs::path(team_selection_folder, glue::glue("02_{prefix}_send_confirmation_emails.R")),
    data = list(project_id = project_id),
    package = "projectutils",
    open = FALSE
  )
  usethis::use_template(
    "template_application_single.Rmd",
    save_as = fs::path(team_selection_folder, glue::glue("zzz_template_application_single.Rmd")),
    package = "projectutils",
    open = FALSE
  )

  usethis::use_template(
    "template_applications_report.Rmd",
    save_as = fs::path(team_selection_folder, glue::glue("{project_id}_applications_report.Rmd")),
    data = list(project_id = project_id),
    package = "projectutils",
    open = FALSE
  )

  usethis::use_template(
    "prepare_team_selection.R",
    save_as = fs::path(team_selection_folder, glue::glue("01_{prefix}_prepare_team_selection.R")),
    data = list(project_id = project_id),
    package = "projectutils",
    open = TRUE
  )
}
