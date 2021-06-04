#'creates folder for a new project and adds templates for all the necessary files.
#'@param project_id character. ID of the project, e.g. 2021-03-COR. see description for details.
#'@param name character. short name of the project. defaults to "".
#'@param data_folder character. path to data folder starting at root of the project. defaults to "."
#'@details the project id is composed by the year-month of when the project was started (~when the kickoff took place) and a three-letter, uppercase
#' abbreviation for the organization. If there are two projects with the same organization starting in the same month, 
#' two letters of the abbreviation should be used for the organization, and one for the content of the project. e.g. 2021-03-COV for a visualization project
#' with CorrelAid and 2021-03-COA for an automation project with CorrelAid.
#'@export  
 new_project <- function(project_id, name = "", data_folder = ".") {
  assert_project_id(project_id)
  usethis::ui_info(glue::glue("processing {project_id}"))

  dir.create(fs::path(data_folder, project_id), showWarnings = FALSE)
  
  proj <- Project$new(project_id, name)
  proj_obj_path <- fs::path(data_folder, project_id, glue::glue("{project_id}.rds"))

  answer <- TRUE
  if (file.exists(proj_obj_path)) {
    usethis::ui_warn(glue::glue("{project_id}.rds already exists in {proj_obj_path}"))
    answer <- usethis::ui_yeah("Do you want to overwrite it?", yes = "Yes", no = "No", shuffle = FALSE)
  }
  if (answer) {
    readr::write_rds(proj, proj_obj_path)
    usethis::ui_done(glue::glue("created {project_id}.rds at {proj_obj_path}."))
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
  use_team_selection_workflow(project_id, data_folder)
  use_r6_templates(project_id, data_folder)
}

#'Use R6 templates
#'@param project_id project id in path form, e.g. 2020-11-COR
#'@param data_folder character. path to data folder starting at root of the project. defaults to ".", i.e. root
#'@export 
use_r6_templates <- function(project_id, data_folder = ".") {
  usethis::use_template(
    "project_data_public.Rmd",
    save_as = fs::path(data_folder, project_id, glue::glue("{project_id}_project_data_public.Rmd")),
    data = list(project_id = project_id),
    package = "projectutils",
    open = TRUE
  )
  usethis::use_template(
    "project_data_sensitive.Rmd",
    save_as = fs::path(data_folder, project_id, glue::glue("{project_id}_project_data_sensitive.Rmd")),
    data = list(project_id = project_id),
    package = "projectutils",
    open = FALSE
  )
}


#'Use team selection workflow
#'@param project_id project id in path form, e.g. 2020-11-COR
#'@param data_folder character. path to data folder starting at root of the project. defaults to ".", i.e. root
#'@export 
use_team_selection_workflow <- function(project_id, data_folder = ".") {
  
  project_folder <- fs::path(data_folder, project_id)
  
  # create subfolder for team selection if not already there

  team_selection_folder <- fs::path(data_folder, project_id, "team_selection")
  if (!dir.exists(team_selection_folder)) {
    dir.create(team_selection_folder)
    dir.create(fs::path(team_selection_folder, "data"))
  }
  
  # use templates
  # we add the three-letter prefix 
  # to make file names distinguishable in RStudio when more then one project is managed at the same time
  prefix <- tolower(stringr::str_extract(project_id, '[:alpha:]+')) 
  usethis::use_template(
    "send_confirmation_emails.R",
    save_as = fs::path(team_selection_folder, glue::glue("02_{project_id}_send_confirmation_emails.R")),
    data = list(project_id = project_id),
    package = "projectutils",
    open = FALSE
  )
  usethis::use_template(
    "template_application_single.Rmd",
    save_as = fs::path(team_selection_folder, glue::glue("zzz_template_application_single.Rmd")),
    package = "projectutils",
    open = FALSE
  )

  usethis::use_template(
    "template_applications_report.Rmd",
    save_as = fs::path(team_selection_folder, glue::glue("{project_id}_applications_report.Rmd")),
    data = list(project_id = project_id),
    package = "projectutils",
    open = FALSE
  )

  usethis::use_template(
    "prepare_team_selection.R",
    save_as = fs::path(team_selection_folder, glue::glue("01_{project_id}_prepare_team_selection.R")),
    data = list(project_id = project_id),
    package = "projectutils",
    open = TRUE
  )
}
