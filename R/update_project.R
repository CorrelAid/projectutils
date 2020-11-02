
#'updates a top level field of a project
#'@param project list. the project to be updated.
#'@param field character. the field that needs to be updated. only top-level fields work currently
#'@param value the new value.
#'@return list. the project.
#'@description load all data for all projects in the data subfolder
#'@export
update_project <- function(project, field, value) {
  if (is.null(project[[field]])) {
    usethis::ui_stop(glue::glue("Field {field} does not exist."))
  }
  project[[field]] <- value
  return(project)
}


#'update the list of projects
#'@param data_folder character. path to data folder where the project folder should be created. starts at root of the project as defined by here::here. Defaults to "".
#'@description loads the data for all projects and writes the list as json to the data subfolder
#'and the docs subfolder (the latter to "publish" it).
#'@importFrom magrittr %T>%
#'@export
update_projects_json <- function(data_folder = "") {
  projects <- load_projects()
  projects %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>%
    jsonlite::prettify() %T>%
    readr::write_lines(here::here(data_folder, "projects.json"))
  usethis::ui_done("updated projects.json")
}

#' writes back meta.json for a project.
#'@param project the project.
#'@param data_folder character. path to data folder where the project folder is. starts at root of the project as defined by here::here. Defaults to "".
#'@return the project (invisibly)
#'@export
write_project <- function(project, data_folder = "") {
  project_id_path <- project$project_id_path
  pretty_json <- project %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>%
    jsonlite::prettify()
  readr::write_lines(pretty_json,
                     here::here(data_folder, project_id_path, "meta.json"))
  usethis::ui_done(glue::glue("saved to {data_folder}/{project_id_path}/meta.json"))
  
  return(invisible(project))
}