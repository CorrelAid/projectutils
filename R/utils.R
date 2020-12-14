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