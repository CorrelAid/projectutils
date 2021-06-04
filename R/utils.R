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

#' get_kobo
#' @param url character. URL to download. 
#' @param token character. API token. defaults to Sys.getenv("KOBO_CORRELAID_TOKEN")
#' @description load an asset by its url
get_kobo <- function(url, token = Sys.getenv("KOBO_CORRELAID_TOKEN")) {
  req <- httr::GET(url, httr::add_headers(Authorization = glue::glue("Token {token}")))
  httr::stop_for_status(req)
  httr::content(req)$results
}


assert_project_id = function(value) {
  if (!checkmate::test_character(value, len = 1)) {
    usethis::ui_stop("project_id needs to be a character vector of length 1.")
  }
  if (!stringr::str_detect(value, "^\\d{4}\\-\\d{2}\\-[:upper:]{3,3}$")) usethis::ui_stop("Invalid project id. It needs to be conform to the following format: YYYY-mm-ABB where ABB is a three-letter abbreviation of the partner organization.")

  year  <- as.numeric(stringr::str_sub(value, 1, 4))
  month  <- stringr::str_sub(value, 6,7)

  if(year < 2015 | year > 2030) {
    usethis::ui_stop("Invalid year in project id. Only years between 2015 and 2030 are valid.")
  }
  months  <- c(paste0('0', c(0:9)), 10, 11, 12)
  if (!month %in% months) {
    usethis::ui_stop("Invalid month in project id.")
  }
}