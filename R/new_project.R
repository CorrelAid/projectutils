#'creates folder for a new project and adds templates for all the necessary files.
#'@param prefix character. 3 character, alphabetic uppercase prefix to be used for the project. Usually the first three letters of the organization name.
#'@param year numeric. year the project was started in, e.g. 2019.
#'@param month numeric. month the project was started in 
#'@param data_folder character. path to data folder starting at root of the project. defaults to here::here()
new_project <- function(prefix, year, month, data_folder = here::here()) {
  
  usethis::ui_info(glue::glue("processing {prefix} {year} {month}"))
  # check validity of inputs
  if (!is.numeric(year) | nchar(as.character(year)) != 4) {
    usethis::ui_stop("Invalid year.")
  }
  if (!is.numeric(month) || month > 12 || month < 1) {
    usethis::ui_stop("Invalid month argument. It must be an integer between 1 and 12.")
  }

  # check validity of prefix 
  if (!stringr::str_detect(prefix, "^[:upper:]{3,3}$")) usethis::ui_stop("Invalid prefix. It be 3 alphabetic characters long and it must be uppercase.")

  # add leading zero 
  if (month < 10) {
    month <- paste0("0", month)
  }
  
  # project id path 
  project_id_path <- glue::glue("{year}-{month}-{prefix}")
  dir.create(fs::path(data_folder, project_id_path), showWarnings = FALSE)
             ?fs::path()
  
  # meta data file (meta.json)
  template_meta <- get_meta_template()
  
  # populate with data 
  template_meta$project_id_path <- project_id_path
  template_meta$project_id <- glue::glue("{prefix}-{month}-{year}")
  template_meta$year <- year
  template_meta$start <- glue::glue("{year}-{month}")
  
  meta_path <- fs::path(data_folder, project_id_path, "meta.json")
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
    dir.create(fs::path(data_folder, project_id_path, lang), showWarnings = FALSE)
    purrr::walk(markdown_files, function(x) {
        file_path <- fs::path(data_folder, project_id_path, lang, x)
        
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

get_meta_template <- function() {
  path <- tryCatch(
    fs::path_package(package = "projectutils", "templates", "template_meta.json"), # installed in library
    error = function(e) fs::path_package(package = "projectutils", "inst", "templates", "template_meta.json") # development mode
  )
  
  j <- jsonlite::read_json(path)
  return(j)
}
