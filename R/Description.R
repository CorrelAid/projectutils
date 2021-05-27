#' Description
#' class representing the Description of a CorrelAid project
#' @export
Description <- R6::R6Class("Description",
  private = list(
    .project_id = NA_character_,
    .summary = list(en = "", de = ""),
    .problem = list(en = "", de = ""),
    .data = list(en = "", de = ""),
    .approach = list(en = "", de = ""),
    .impact = list(en = "", de = ""),
    .title = list(en = "", de = ""),
    .further_links = list(en = "", de = "")
  ),
  active = list(
    #' @field summary
    #' tibble. Returns the summary list of the project. read-only.
    summary = function(value) {
      if (missing(value)) {
        return(private$.summary)
      } else {
        usethis::ui_stop("Can't set summary. Please use the set_json_list method.")
      }
      invisible(self)
    },
    #' @field problem
    #' tibble. Returns the problem list of the project. read-only.
    problem = function(value) {
      if (missing(value)) {
        return(private$.problem)
      } else {
        usethis::ui_stop("Can't set problem. Please use the set_json_list method.")
      }
      invisible(self)
    },
    #' @field data
    #' tibble. Returns the data list of the project. read-only.
    data = function(value) {
      if (missing(value)) {
        return(private$.data)
      } else {
        usethis::ui_stop("Can't set data. Please use the set_json_list method.")
      }
      invisible(self)
    },
        #' @field approach
    #' tibble. Returns the approach list of the project. read-only.
    approach = function(value) {
      if (missing(value)) {
        return(private$.approach)
      } else {
        usethis::ui_stop("Can't set approach. Please use the set_json_list method.")
      }
      invisible(self)
    },
    #' @field impact
    #' tibble. Returns the impact list of the project. read-only.
    impact = function(value) {
      if (missing(value)) {
        return(private$.impact)
      } else {
        usethis::ui_stop("Can't set impact. Please use the set_json_list method.")
      }
      invisible(self)
    },

    #' @field further_links
    #' tibble. Returns or sets the further links list of the project.
    further_links = function(value) {
      if (missing(value)) {
        return(private$.further_links)
      } else {
          #TODO 
      }
      invisible(self)
    }
  ),
  public = list(
    #' create a description of a project
    #' @param project_id character. id of the project
    initialize = function(project_id) {
      # check validity of inputs
      # check start_ym
      checkmate::assert_character(start_ym)
      if (!check_ym(start_ym)) {
        usethis::ui_stop(usethis::ui_stop(glue::glue("Invalid format: start_ym needs to be set to a character in the format YYYY-mm")))
      }

      # check validity of abbreviation
      checkmate::assert_character(start_ym)
      if (!stringr::str_detect(abbreviation, "^[:upper:]{3,3}$")) usethis::ui_stop("Invalid abbreviation. It needs to be 3 alphabetic characters long and it must be uppercase.")

      # name needs to be character
      checkmate::assert_character(name, min.len = 1, max.len = 1)

      private$.start_ym <- start_ym
      private$.project_id <- glue::glue("{start_ym}-{abbreviation}")
      private$.name <- name
      invisible(self)
    },

    #' add a new tag to the project
    #' @param category character. the tag category. see projectutils::tags for available values.
    #' @param value character. the tag value. see projectutils::tags for available values and combinations. Defaults to NA.
    add_tag = function(category, value = NA) {
      tag <- Tag$new(category, value)
      private$.tags <- c(private$.tags, tag)
      invisible(self)
    },

    #' add a local chapter to the project
    #' @param lc_name character. local chapter name. see projectutils::local_chapters for available options.
    add_local_chapter = function(lc_name) {
      checkmate::assert_character(lc_name, len = 1)
      if (lc_name %in% self$local_chapters$lc_name) {
        usethis::ui_warn(glue::glue("{lc_name} is already added as a local chapter. Skipping."))
        return(invisible(self))
      }
      lc <- LocalChapter$new(lc_name)
      private$.local_chapters <- c(private$.local_chapters, lc)
      invisible(self)
    },

    #' set status of the project
    #' @param status character. the status. See projectutils::status for options.
    set_status = function(status) {
      status_print <- paste(projectutils::status$status, collapse = "\n")
      if (!status %in% projectutils::status$status) {
        usethis::ui_stop(glue::glue("Invalid status. The following status are available: \n{status_print}"))
      }
      status_set <- projectutils::status %>%
        dplyr::filter(status == .env$status)
      usethis::ui_done(glue::glue("Setting status of project {private$project_id} to {status_set$status}"))
      private$.status_id <- status_set$status_id
      private$.status <- status
      invisible(self)
    },
    #' to_tibble
    #' @description returns a one row tibble representation of the Project object.
    to_tibble = function() {
      tibble::tibble(
        project_id = private$.project_id,
        name = private$.name,
        start_ym = private$.start_ym,
        end_ym_predicted = private$.end_ym_predicted,
        end_ym = private$.end_ym,
        is_public = private$.is_public,
        is_internal = private$.is_internal,
        url_git_repo = private$.url_git_repo,
        url_gh_issue = private$url_gh_issue,
        url_pad = private$.url_pad,
        slack_channel = private$.slack_channel,
        organization_id = private$.organization_id,
        tags = list(self$tags),
        local_chapters = list(self$local_chapters),
        status_id = private$.status_id,
        status = private$.status
      )
    },

    #' get_sql_tables
    #' @description function that returns a tibble for each table
    get_sql_tables = function() {
      list(
        projecttag = tibble::tibble(
          project_id = private$.project_id,
          tag_id = self$tags$tag_id
        ), 
        tag = projectutils::tags,
        projectlocalchapters = tibble::tibble(
          project_id = private$.project_id,
          lc_id = self$local_chapters$lc_id
        )
      )
    }
  )
)