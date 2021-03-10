#' id_path. convert from surveymonkey id to path id
#' @param project_id_surveymonkey character. project id in surveymonkey form, e.g. CIT-11-2020
#' @return character. project id in path form, e.g. 2020-11-CIT
#' @export
id_path <- function(project_id_surveymonkey) {
  pattern <- "([:upper:]{3})-(\\d{2})-(\\d{4})"
  prefix <- stringr::str_replace(project_id_surveymonkey, pattern, "\\1")
  month <- stringr::str_replace(project_id_surveymonkey, pattern, "\\2")
  year <- stringr::str_replace(project_id_surveymonkey, pattern, "\\3")
  paste(year, month, prefix, sep = "-")
}


#' id_surveymonkey. convert from path id to surveymonkey id
#' @param project_id_path character. project id in path form, e.g. 2020-11-CIT
#' @return character. project id in surveymonkey form, e.g. CIT-11-2020
#' @export
id_surveymonkey <- function(project_id_path) {
  pattern <- "(\\d{4})-(\\d{2})-([:upper:]{3})"
  prefix <- stringr::str_replace(project_id_path, pattern, "\\3")
  month <- stringr::str_replace(project_id_path, pattern, "\\2")
  year <- stringr::str_replace(project_id_path, pattern, "\\1")
  paste(prefix, month, year, sep = "-")
}


#' get_surveymonkey
#' @param id character. internal surveymonkey id for the survey
#' @return tibble with answers to the survey
get_surveymonkey <- function(id) {
  id %>% 
    surveymonkey::fetch_survey_obj() %>%
    surveymonkey::parse_survey()
}

#' get_kobo
#' @param url character. URL to download. 
#' @param token character. API token. defaults to Sys.getenv("KOBO_CORRELAID_TOKEN")
#' @description load an asset by its url
get_kobo <- function(url, token = Sys.getenv("KOBO_CORRELAID_TOKEN")) {
  req <- httr::GET(url, httr::add_headers(Authorization = glue::glue("Token {token}")))
  httr::stop_for_status(req)
  httr::content(req)$results
}