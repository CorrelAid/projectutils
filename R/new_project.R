#'creates folder for a new project and adds templates for all the necessary files.
#'@param project_id character. ID of the project, e.g. 2021-03-COR. see description for details.
#'@param data_folder character. path to data folder starting at root of the project. defaults to here::here()
#'@details the project id is composed by the year-month of when the project was started (~when the kickoff took place) and a three-letter, uppercase
#' abbreviation for the organization. If there are two projects with the same organization starting in the same month, 
#' two letters of the abbreviation should be used for the organization, and one for the content of the project. e.g. 2021-03-COV for a visualization project
#' with CorrelAid and 2021-03-COA for an automation project with CorrelAid.
#'@export  
new_project <- function(project_id, data_folder = here::here()) {
  assert_project_id(project_id)
  usethis::ui_info(glue::glue("processing {project_id}"))
  
  dir.create(fs::path(data_folder, project_id), showWarnings = FALSE)
  
  # meta data file (meta.json)
  template_meta <- get_meta_template()
  
  # populate with data 
  template_meta$project_id <- project_id
  template_meta$year <- stringr::str_sub(project_id, 1, 4)
  template_meta$start <- stringr::str_sub(project_id, 1, 7)
  
  meta_path <- fs::path(data_folder, project_id, "meta.json")

  answer <- TRUE
  if (file.exists(meta_path)) {
    usethis::ui_warn("meta.json already exists in {meta_path}")
    answer <- usethis::ui_yeah("Do you want to overwrite it?", yes = "Yes", no = "No", shuffle = FALSE)
    
  }
  if (answer) {
    readr::write_lines(jsonlite::prettify(jsonlite::toJSON(template_meta, auto_unbox = TRUE)), meta_path)
    usethis::ui_done(glue::glue("created meta.json at {meta_path}."))
  }
  
  # markdown files
  markdown_files <- c("00_about.md", "00_summary.md", "01_problem.md", "02_data.md", "03_approach.md", "04_impact.md")
  for (lang in c("de", "en")) {
    dir.create(fs::path(data_folder, project_id, lang), showWarnings = FALSE)

    purrr::walk(markdown_files, function(x) {
        file_path <- fs::path(data_folder, project_id, lang, x)
        
        answer <- TRUE
        if (file.exists(file_path)) {
          usethis::ui_warn(glue::glue("{file_path} already exists."))
          answer <- usethis::ui_yeah("Do you want to overwrite it?", yes = "Yes", no = "No", shuffle = FALSE)
        }
        if (answer) file.create(file_path)
        usethis::ui_done(glue::glue("created {x} at {file_path}"))
      }
    )
  }
}


#'Use fill project script template
#'@param project_id project id in path form, e.g. 2020-11-COR
#'@param data_folder character. path to data folder starting at root of the project. defaults to "", i.e. root
#'@export 
use_fill_project <- function(project_id, data_folder = "") {
  
  usethis::use_template(
    "fill_project.R",
    save_as = fs::path(data_folder, project_id, "fill_project.R"),
    data = list(project_id = project_id),
    package = "projectutils",
    open = TRUE
  )
}

get_meta_template <- function() {
  path <- tryCatch(
    fs::path_package(package = "projectutils", "templates", "template_meta.json"), # installed in library
    error = function(e) fs::path_package(package = "projectutils", "inst", "templates", "template_meta.json") # development mode
  )
  
  j <- jsonlite::read_json(path)
  return(j)
}
