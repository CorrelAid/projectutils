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
get_kobo <- function(url, token = Sys.getenv("KOBO_TOKEN")) {
  req <- httr::GET(url, httr::add_headers(Authorization = glue::glue("Token {token}")))
  httr::stop_for_status(req)
  httr::content(req)$results
}

assert_project_id = function(value) {
  if (!checkmate::test_character(value, len = 1)) {
    usethis::ui_stop("project_id needs to be a character vector of length 1.")
  }
  if (!stringr::str_detect(value, "^\\d{4}\\-\\d{2}\\-[:upper:]{3,3}$")) usethis::ui_stop("Invalid project id. It needs to be conform to the following format: YYYY-mm-ABB where ABB is a three-letter abbreviation of the partner organization.")
  year <- as.numeric(stringr::str_sub(value, 1, 4))
  month <- stringr::str_sub(value, 6, 7)

  if (year < 2015 | year > 2030) {
    usethis::ui_stop("Invalid year in project id. Only years between 2015 and 2030 are valid.")
  }
  months <- c(paste0("0", c(0:9)), 10, 11, 12)
  if (!month %in% months) {
    usethis::ui_stop("Invalid month in project id.")
  }
}

check_ym <- function(value) {
  checkmate::test_character(value, pattern = "^\\d{4}\\-\\d{2}$")
}


assert_lang_list = function(lang_list, max_len_each = 1) {
  checkmate::assert_list(lang_list, types = c("character"))
  checkmate::assert_set_equal(names(lang_list), c("en", "de"))

  checkmate::assert_character(lang_list$en, max.len = max_len_each)
  checkmate::assert_character(lang_list$de, max.len = max_len_each)
}

#' connect to MariaDB using environment variables
#' @param host character. Defaults to Sys.getenv("DBHOST")
#' @param dbname character. Defaults to Sys.getenv("DBNAME")
#' @param user character. Defaults to Sys.getenv("DBUSER")
#' @param password character. Defaults to Sys.getenv("DBPW")
#' @export
connect_to_mariadb <- function(host = Sys.getenv("DBHOST"),
                               dbname = Sys.getenv("DBNAME"),
                               user = Sys.getenv("DBUSER"),
                               password = Sys.getenv("DBPW")) {
  DBI::dbConnect(RMariaDB::MariaDB(),
    host = host,
    dbname = dbname,
    user = user,
    password = password
  )

}

extract_ids_from_kobo_columnnames <- function(columnnames) {
  # some older submissions to the kobo still have the old project id format
  # so we have to account for that..
  regex_old <- '\\w{3}_\\d{2}_\\d{4}'
  regex_new <- '\\d{4}_\\d{2}_\\w{3}'
  extracted <- columnnames %>% 
    stringr::str_extract(glue::glue('{regex_old}|{regex_new}')) %>% 
                stringr::str_replace_all("_", "-") %>% 
                stringr::str_to_upper() %>%
                stringr::str_trim()
  # unify the old format to the new format
  unify_project_id_formats(extracted)
}

unify_project_id_formats <- function(char_vec) {
    regex_old <- '\\w{3}-\\d{2}-\\d{4}'
    ifelse(stringr::str_detect(char_vec, regex_old), 
        id_path(char_vec), 
        char_vec)
}

assert_url <- function(value, arg_name, domain = NA) {
  # see https://cran.r-project.org/web/packages/rex/vignettes/url_parsing.html
  if (!checkmate::test_character(value, pattern = "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")) {
    usethis::ui_stop(glue::glue("Invalid URL for {arg_name}"))
  }

  if (!is.na(domain)) {
    if (!checkmate::test_character(value, pattern = domain)) {
      usethis::ui_stop(glue::glue("URL for {arg_name} must include domain {domain}."))
    }
  }
}