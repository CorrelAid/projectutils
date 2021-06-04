utils::globalVariables("where") # https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(".")

#' load project applications from export
#' @description loads the applications for a given project from the csv export
#' @param export_csv_path character. path to csv export from surveymonkey containing the applications 
#' @param project_id_surveymonkey character. ID of the project, e.g. ERL-03-2020. Defaults to NULL which means not to filter by project
#' @param lang character. Which language was used to collect the applications. Defaults to "en" for the "application for correlaid projects (en)" form. "de" corresponds to "Bewerbungsformular für Projektteams (de)" form.
#' @return data frame containing the responses to the questions
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
load_applications_export <- function(export_csv_path, project_id_surveymonkey = NULL, lang = "en") {
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
  if (!is.null(project_id_surveymonkey)) {
    proj_id <- project_id_surveymonkey
    survey_df <- survey_df %>% 
      dplyr::filter(.data$project_id == proj_id)
  }
  
  survey_df <- survey_df %>% 
    dplyr::mutate(applicant_id = 1:dplyr::n()) %>%  # give participant integer id
    dplyr::mutate(project_id_surveymonkey = project_id_surveymonkey) %>% 
    dplyr::mutate(project_id = id_path(project_id_surveymonkey)) %>% 
    dplyr::select(.data$applicant_id, .data$gender, dplyr::everything())
  
  return(survey_df)
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
#' @description cleans surveymonkey column names. snakecases column names and splits the title into "project_id" and "project_title"
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
