#'load all data for a project from the project files.
#'@param project_id_path character. the project id used for folder names, i.e. in the format YYYY-mm-prefix
#'@param data_folder character. path to data folder. starts at root of the project as defined by here::here. Defaults to "".
#'@return a list
#'@description loads all data related to a project from its folder.
#' @export
load_project <- function(project_id_path, data_folder = "") {
  # error if folder does not exist
  if (!dir.exists(here::here(data_folder, project_id_path))) {
    usethis::ui_stop(glue::glue("Folder {data_folder}/{project_id_path} does not exist."))
  }
  
  # load meta data
  meta_path <- here::here(data_folder, project_id_path, "meta.json")
  if (!file.exists(meta_path)) {
    usethis::ui_stop(glue::glue("{meta_path} does not exist."))
  }
  
  j <-
    jsonlite::read_json(here::here(data_folder, project_id_path, "meta.json"))
  
  # set up lists for description
  j$description <- list()
  j$description$de <- list()
  j$description$en <- list()
  
  # load description data
  set_description_data <- function(lang) {
    # if no files in language, skip
    if (length(list.files(here::here(data_folder, project_id_path, lang))) == 0) {
      usethis::ui_info("no files found.")
      return(j)
    }
    
    # set description of organization
    j$organization$about[[lang]][["text"]] <-
      readr::read_file(here::here(data_folder, project_id_path, lang, "00_about.md"))
    
    
    # hardcoded to avoid loading any "junk" and to get better error messages
    j$description[[lang]][["summary"]] <-
      readr::read_file(here::here(data_folder, project_id_path, lang, "00_summary.md"))
    j$description[[lang]][["problem"]] <-
      readr::read_file(here::here(data_folder, project_id_path, lang, "01_problem.md"))
    j$description[[lang]][["data"]] <-
      readr::read_file(here::here(data_folder, project_id_path, lang, "02_data.md"))
    j$description[[lang]][["approach"]] <-
      readr::read_file(here::here(data_folder, project_id_path, lang, "03_approach.md"))
    j$description[[lang]][["impact"]] <-
      readr::read_file(here::here(data_folder, project_id_path, lang, "04_impact.md"))
    return(j)
  }
  
  j <- set_description_data("de")
  j <- set_description_data("en")
  usethis::ui_done(glue::glue("loaded data from {data_folder}/{project_id_path}"))
  
  return(j)
}

#'load all data for all projects in the data subfolder
#'@return a list of lists where each element represents a project.
#'@param data_folder character. path to data folder. starts at root of the project as defined by here::here. Defaults to "".
#'@description load all data for all projects in the data subfolder
#'@export
load_projects <- function(data_folder = "") {
  project_id_paths <-
    list.dirs(here::here(data_folder),
              recursive = FALSE,
              full.names = FALSE)
  project_id_paths <- stringr::str_subset(project_id_paths, pattern = "\\d{4}-\\d{2}-[:upper:]{3}")
  projects <- purrr::map(project_id_paths, load_project, data_folder = data_folder)
  return(projects)
}
