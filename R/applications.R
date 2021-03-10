utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

#' load project applications from Kobo
#' @description loads the applications for a given project
#' @param asset_url character. URL of the Kobo asset corresponding to the survey.
#' @param project_id character. ID of the project, e.g. ERL-03-2020. Defaults to NULL which means not to filter by project
#' @return data frame containing the responses to the questions
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
load_applications <- function(asset_url, project_id = NULL) {

  survey_list <- get_kobo(asset_url)

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
    tidyr::separate_rows(.data$project_id, sep = " ") %>% 
    dplyr::distinct()

  # tibble with project roles (1 person-project row)
  project_roles_df <- survey_df %>% 
    dplyr::select(.data$applicant_id, dplyr::starts_with("project_role")) %>% 
    tidyr::pivot_longer(dplyr::starts_with("project_role"), names_to = "project_id", values_to = "project_role")%>% 
    dplyr::mutate(project_id = .data$project_id %>% 
              stringr::str_extract("\\w{3}_\\d{2}_\\d{4}") %>% 
              stringr::str_replace_all("_", "-") %>% 
              stringr::str_to_upper() %>% stringr::str_trim()) %>% 
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
    dplyr::left_join(personal_info_df, by = "applicant_id") %>% 
    dplyr::mutate(applicant_id = 1:dplyr::n())  # give participant integer id from 1:n 



  if (!is.null(project_id)) {
    proj_id <- project_id
    cleaned_df <- cleaned_df %>% 
      dplyr::filter(.data$project_id == proj_id)
  }
  
  if (nrow(cleaned_df) == 0) {
    usethis::ui_stop(glue::glue("No applicants present after filtering for {project_id}. Did you specify the ID in the correct format?"))
  }
  cleaned_df <- cleaned_df %>% 
    dplyr::select(.data$applicant_id, .data$gender, dplyr::everything())

  return(cleaned_df)
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
  # remove columns which are added by surveymonkey automatically but are empty 
  if (lang == "en") {
    export_raw <- export_raw %>% 
      dplyr::select(-.data$`First Name`, -.data$`Last Name`, -.data$`Email Address`)
  } else {
    export_raw <- export_raw %>% 
      dplyr::select(-.data$email_address, -.data$last_name, -.data$first_name)
  }

  # the exported format has two rows for the column names :facepalm:
  # do some cleaning
  colnames_row_1 <- colnames(export_raw)
  # answer options for dropdowns and matrizes have X in the first row (except for the first option), the actual value in the second row
  # replace first row with NA
  colnames_row_1[stringr::str_detect(colnames_row_1, "^X\\d{1,2}$")] <- NA 
  colnames_row_1 <- data.frame(colnames_row_1 = colnames_row_1) %>% tidyr::fill(colnames_row_1) %>% dplyr::pull(colnames_row_1) # fill NA with previous question
  
  # add "em" to the technologies and tools question to emulate the API export, see rename_programming
  if (lang == "en") {
    colnames_row_1 <- ifelse(stringr::str_detect(colnames_row_1, ".+?functions and packages\\.$"),
                              paste(colnames_row_1, "em", sep = " "), 
                              colnames_row_1)
  } else {
    colnames_row_1 <- ifelse(stringr::str_detect(colnames_row_1, ".+?Funktionen und Packages\\.$"),
                             paste(colnames_row_1, "em", sep = " "), 
                             colnames_row_1)
  }

  # promote second row to colnames and drop it from the data
  survey_df <- export_raw %>% 
           dplyr::slice(2:dplyr::n())
          
  colnames_row_2 <- export_raw %>% 
           dplyr::slice(1) %>% 
           unlist(., use.names=FALSE)

  # answer format is not important
  colnames_row_2[is.na(colnames_row_2) | colnames_row_2 == "Response" | colnames_row_2 == "Open-Ended Response"] <- ""
  # combine
  colnames_combined <- paste(colnames_row_1, colnames_row_2, sep = " ")
  colnames(survey_df) <- colnames_combined
  
  # cleaning with janitor (to snakecase, remove special chars) and our own custom function 
  survey_df <- survey_df %>% 
    janitor::clean_names() %>% 
    clean_application_colnames(lang = lang)
  
  # filter out specific project if specified 
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
#'@param project_id_path project id in path form, e.g. 2020-11-COR
#'@param data_folder character. path to data folder starting at root of the project. defaults to ".", i.e. root
#'@param export_csv_file character. file name of csv export file within the team_selection folder. Defaults to NULL, i.e. using the API to get the data. 
#'@param lang character. Which language was used to collect the applications. Defaults to "en" for the "application for correlaid projects (en)" form. "de" corresponds to "Bewerbungsformular für Projektteams (de)" form.
#'@export 
use_team_selection_workflow <- function(project_id_path, data_folder = ".", export_csv_file = NULL, lang = "en") {
  
  
  # create subfolder for team selection if not already there
  team_selection_folder <- fs::path(data_folder, project_id_path, "team_selection")
  if (!dir.exists(team_selection_folder)) {
    dir.create(team_selection_folder)
    dir.create(fs::path(team_selection_folder, "data"))
  }
  
  # use templates
  project_id <- id_surveymonkey(project_id_path) # needed to download from surveymonkey
  # we add the three-letter prefix 
  # to make file names distinguishable in RStudio when more then one project is managed at the same time
  prefix <- tolower(stringr::str_sub(project_id, 1, 3)) 
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

  project_id_path_lower <- tolower(project_id_path)
  usethis::use_template(
    "template_applications_report.Rmd",
    save_as = fs::path(team_selection_folder, glue::glue("{project_id_path_lower}_applications_report.Rmd")),
    data = list(project_id = project_id, export_csv_file = export_csv_file, lang = lang),
    package = "projectutils",
    open = FALSE
  )

  usethis::use_template(
    "prepare_team_selection.R",
    save_as = fs::path(team_selection_folder, glue::glue("01_{prefix}_prepare_team_selection.R")),
    data = list(project_id = project_id, export_csv_file = export_csv_file, lang = lang),
    package = "projectutils",
    open = TRUE
  )
}

rename_techniques <- function(col_name, lang = "en") {
  if (lang == "en") {
    col_name_new <- stringr::str_replace(col_name, ".+following_techniques_(.+?)$", "techniques_\\1")
  } else {
    col_name_new <- stringr::str_replace(col_name, ".+folgenden_techniken_(.+?)$", "techniques_\\1")
  }
  col_name_new
}

rename_topics <- function(col_name, lang = "en") {
  if (lang == "en") {
    col_name_new <- stringr::str_replace(col_name, ".+following_topics_(.+?)$", "topics_\\1")
  } else {
    col_name_new <- stringr::str_replace(col_name, ".+folgenden_themen_(.+?)$", "topics_\\1")
  }
  col_name_new
}

rename_programming <- function(col_name) {
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
      dplyr::select(-dplyr::contains("what_is_your_gender"))
    
    # rename column names
    survey_df <- survey_df %>% 
      dplyr::rename_with(rename_programming, dplyr::contains("experience_with_the_following_technologies")) %>% 
      dplyr::rename_with(rename_techniques, dplyr::contains("experience_with_the_following_techniques"), lang = lang) %>% 
      dplyr::rename_with(rename_topics, dplyr::contains("experience_with_the_following_topics"), lang = lang) %>% 
      dplyr::rename_with(~ "consent_privacy_policy", dplyr::contains("consent_to_privacy_policy")) %>% 
      dplyr::rename_with(~ "motivation_why_involved", dplyr::contains("why_you_want_to_get_involved")) %>% 
      dplyr::rename_with(~ "motivation_skills", dplyr::contains("what_skills_you_would_bring")) %>% 
      dplyr::rename_with(~ "project_role", dplyr::contains("what_role_do_you_want_to_assume")) %>% 
      dplyr::rename_with(~ "first_name", dplyr::contains("first_name")) %>% 
      dplyr::rename_with(~ "email", dplyr::contains("which_email_address"))
  } else {
    survey_df <- survey_df %>% 
      tidyr::separate(.data$auf_welches_projekt_mochtest_du_dich_bewerben, c("project_id", "project_title"), sep = ":") %>% # extract project id
      dplyr::mutate(gender = dplyr::coalesce(!!! dplyr::select(., dplyr::contains("was_ist_dein_geschlecht"))))
    
    # drop original gender variables
    survey_df <- survey_df %>% 
      dplyr::select(-contains("was_ist_dein_geschlecht"))

    # rename column names
    survey_df <- survey_df %>% 
      dplyr::rename_with(rename_programming, dplyr::contains("erfahrung_mit_den_folgenden_technologien_und_tools")) %>% 
      dplyr::rename_with(rename_techniques, dplyr::contains("erfahrung_mit_den_folgenden_techniken"), lang = lang) %>% 
      dplyr::rename_with(rename_topics, dplyr::contains("erfahrung_mit_den_folgenden_themen"), lang = lang) %>% 
      dplyr::rename_with(~ "consent_privacy_policy", dplyr::contains("einwilligung_in_datenschutzerklarung")) %>% 
      dplyr::rename_with(~ "motivation_why_involved", dplyr::contains("warum_du_dich_fur_dieses_projekt")) %>% 
      dplyr::rename_with(~ "motivation_skills", dplyr::contains("welche_deiner_fahigkeiten_dich_besonders")) %>% 
      dplyr::rename_with(~ "project_role", dplyr::contains("welche_rolle_mochtest_du_im_projekt_einnehmen")) %>% 
      dplyr::rename_with(~ "first_name", dplyr::contains("wie_lautet_dein_vorname")) %>% 
      dplyr::rename_with(~ "email", dplyr::contains("welcher_e_mail_adresse"))
    
    
  }
  
  # trim character variables
  survey_df <- survey_df %>% 
    dplyr::mutate(dplyr::across(where(is.character), stringr::str_trim))
  return(survey_df)
}
