#'load all data for a project from the project files.
#'@param project_id_path character. the project id used for folder names, i.e. in the format YYYY-mm-prefix
#'@param data_folder character. path to data folder. starts at root of the project as defined by here::here. Defaults to here::here().
#'@return a list
#'@description loads all data related to a project from its folder.
#' @export
load_project <- function(project_id_path, data_folder = here::here()) {
  # error if folder does not exist
  full_path <- fs::path(data_folder, project_id_path)
  if (!dir.exists(full_path)) {
    wd <- getwd()
    dirs <- list.dirs(wd, full.names = TRUE, recursive = TRUE)
    usethis::ui_info(glue::glue("Current working directory: {wd}"))
    usethis::ui_info(glue::glue("Directories: {dirs}"))
    usethis::ui_stop(glue::glue("Folder {full_path} does not exist."))
  }
  
  # load meta data
  meta_path <- fs::path(data_folder, project_id_path, "meta.json")
  if (!file.exists(meta_path)) {
    usethis::ui_stop(glue::glue("{meta_path} does not exist."))
  }
  
  j <-
    jsonlite::read_json(fs::path(data_folder, project_id_path, "meta.json"))
  
  # set up lists for description
  j$description <- list()
  j$description$de <- list()
  j$description$en <- list()
  
  # load description data
  set_description_data <- function(lang) {
    # if no files in language, skip
    if (length(list.files(fs::path(data_folder, project_id_path, lang))) == 0) {
      usethis::ui_info("no files found.")
      return(j)
    }
    
    # set description of organization
    j$organization$about[[lang]][["text"]] <-
      readr::read_file(fs::path(data_folder, project_id_path, lang, "00_about.md"))
    
    
    # hardcoded to avoid loading any "junk" and to get better error messages
    j$description[[lang]][["summary"]] <-
      readr::read_file(fs::path(data_folder, project_id_path, lang, "00_summary.md"))
    j$description[[lang]][["problem"]] <-
      readr::read_file(fs::path(data_folder, project_id_path, lang, "01_problem.md"))
    j$description[[lang]][["data"]] <-
      readr::read_file(fs::path(data_folder, project_id_path, lang, "02_data.md"))
    j$description[[lang]][["approach"]] <-
      readr::read_file(fs::path(data_folder, project_id_path, lang, "03_approach.md"))
    j$description[[lang]][["impact"]] <-
      readr::read_file(fs::path(data_folder, project_id_path, lang, "04_impact.md"))
    return(j)
  }
  
  j <- set_description_data("de")
  j <- set_description_data("en")
  usethis::ui_done(glue::glue("loaded data from {data_folder}/{project_id_path}"))
  
  return(j)
}

#'load all data for all projects in the data subfolder
#'@return a list of lists where each element represents a project.
#'@param data_folder character. path to data folder. starts at root of the project as defined by here::here. Defaults to here::here().
#'@description load all data for all projects in the data subfolder
#'@export
load_projects <- function(data_folder = here::here()) {
  project_id_paths <-
    list.dirs(data_folder,
              recursive = FALSE,
              full.names = TRUE)
  project_id_paths <- stringr::str_subset(project_id_paths, pattern = "\\d{4}-\\d{2}-[:upper:]{3}")
  projects <- purrr::map(project_id_paths, load_project, data_folder = data_folder)
  return(projects)
}

#'updates a top level field of a project
#' @param project list. the project to be updated.
#' @param field character. the field that needs to be updated. only top-level fields work currently 
#' @param value the new value.
#' @return list. the project.
#' @export
update_project <- function(project, field, value) {
  if (is.null(project[[field]])) {
    usethis::ui_stop(glue::glue("Field {field} does not exist."))
  }
  project[[field]] <- value
  return(project)
}


#'update the list of projects
#'@param data_folder character. path to data folder starting at root of the project. defaults to here::here()
#'@description loads the data for all projects and writes the list as json to the data subfolder
#'@export 
update_projects_json <- function(data_folder = here::here()) {
  projects <- load_projects()
  projects %>% 
    jsonlite::toJSON(auto_unbox = TRUE) %>% 
    jsonlite::prettify() %T>% 
    readr::write_lines(fs::path(data_folder, "projects.json"))
  usethis::ui_done("updated projects.json")
}

#' writes back meta.json for a project.
#' @param project the project.
#' @param data_folder data folder path. defaults to here::here()
#' @return the project (invisibly) 
write_project <- function(project, data_folder = here::here()) {
  project_id_path <- project$project_id_path 
  pretty_json <- project %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>% 
    jsonlite::prettify()
  readr::write_lines(pretty_json, fs::path(data_folder, project_id_path, "meta.json"))
  usethis::ui_done(glue::glue("saved to {data_folder}/{project_id_path}/meta.json"))
  
  return(invisible(project))
}