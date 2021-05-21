#' Project
#' class representing the basic properties of a CorrelAid Project
#'
Project <- R6::R6Class("Project",
  private = list(
    .project_id = "",
    .name = "",
    .slug = NA,
    .is_public = FALSE,
    .start_ym = NA,
    .end_ym = NA,
    .end_ym_predicted = NA,
    .is_internal = FALSE,
    .url_git_repo = NA,
    .url_gh_issue = NA,
    .url_pad = NA,
    .slack_channel = NA,
    .status_id = NA,
    .organisation_id = NA,
    check_name = function(value) {
      checkmate::assert_character(value, min.len = 1, max.len = 1)
    },
    check_project_id = function(value) {
      if (!stringr::str_detect(value, "^\\d{4}\\-\\d{2}\\-[:upper:]{3,3}$")) usethis::ui_stop("Invalid project id. It needs to be conform to the following format: YYYY-mm-{ABB} where {ABB} is uppercase")
    },
    check_ym = function(value) {
      checkmate::assert_character(value)
      if (!stringr::str_detect(value, "^\\d{4}\\-\\d{2}$")) usethis::ui_stop("Invalid format: it needs to be in the form YYYY-mm")
    }
  ),
  active = list(
    #' @field project_id
    #' character. id of the project, in the form YYYY-mm-ABB where ABB is any three-character, uppercase abbreviation
    project_id = function(value) {
      if (missing(value)) {
        return(private$.project_id)
      } else {
        private$check_project_id(value)
        private$.project_id <- value
      }
    },
    #' @field name
    #' character. title of the project.
    name = function(value) {
      if (missing(value)) {
        return(private$.name)
      } else {
        private$check_name(value)
        private$.name <- value
      }
    },
    #' @field slug
    #' character. Returns or sets the slug (the part of the URL after correlaid.org/projects) for the website. Must not contain spaces and only - as special characters.
    slug = function(value) {
      if (missing(value)) {
        return(private$.slug)
      } else {
        # TODO: Check
        # slug should not contain any spaces and only - as special chars
        # should not be too long either
        private$.slug <- value
      }
    },
    #' @field is_public
    #' boolean. Returns or sets whether the project is made public on the CorrelAid website. Use with caution!
    is_public = function(value) {
      if (missing(value)) {
        return(private$.is_public)
      } else {
        checkmate::check_logical(value)
        if (value) {
          answer <- usethis::ui_yeah("Setting is_public to TRUE will publish the project to the CorrelAid website under correlaid.org/projects once you push this. Are you sure you want to continue?")
          if (answer) {
            private$.is_public <- value
            usethis::ui_done("Set is_public to TRUE.")
          } else {
            usethis::ui_info("Not setting is_public to TRUE.")
          }
        }
      }
    },
    #' @field start_ym
    #' character. Return or set the start of the project, in YYYY-mm form. Does not change the project id.
    start_ym = function(value) {
      if (missing(value)) {
        private$check_ym(value)
        return(private$.start_ym)
      } else {
        private$.start_ym <- value
      }
    },
    #' @field end_ym
    #' character. Return or set the actual end of the project, in YYYY-mm form.
    end_ym = function(value) {
      if (missing(value)) {
        private$check_ym(value)
        return(private$.end_ym)
      } else {
        private$.end_ym <- value
      }
    },
    #' @field end_ym_predicted
    #' character. Return or set the predicted end of the project, in YYYY-mm form.
    end_ym_predicted = function(value) {
      if (missing(value)) {
        private$check_ym(value)
        return(private$.end_ym_predicted)
      } else {
        private$.end_ym_predicted <- value
      }
    },
    #' @field is_internal
    #' boolean. Return or set whether or not the project is an internal project.
    is_internal = function(value) {
      if (missing(value)) {
        return(private$.is_internal)
      } else {
        checkmate::check_logical(value)
        private$.is_internal <- value
      }
    },
    #' @field url_git_repo
    #' character. Return or set the full URL of the git repository, either GitHub or Gitlab.
    url_git_repo = function(value) {
      if (missing(value)) {
        return(private$.url_git_repo)
      } else {
        # TODO: Check
        # https://github.com/{owner}/{repo} or
        # https://gitlab.com/{owner}/{repo}
        private$.url_git_repo <- value
      }
    },
    #' @field url_gh_issue
    #' character. Returns the full URL to the GitHub issue in the projects repository. To set, pass the issue number as an integer.
    url_gh_issue = function(value) {
      if (missing(value)) {
        return(private$.url_gh_issue)
      } else {
        checkmate::check_integer(value, lower = 1)
        issue_url <- glue::glue("https://github.com/CorrelAid/projects/{value}")
        usethis::ui_info(glue::glue("Setting url_gh_issue to {issue_url}"))
        private$.url_gh_issue <- issue_url
      }
    },
    #' @field url_pad
    #' character. Returns or sets the full URL to the CodiMD Pad for the project.
    url_pad = function(value) {
      if (missing(value)) {
        return(private$.url_pad)
      } else {
        checkmate::check_character(value, pattern = "^https://pad.correlaid.org/.+?$")
        private$.url_pad <- value
      }
    },
    #' @field slack_channel
    #' character. Returns or sets the name of the Slack channel for the project.
    slack_channel = function(value) {
      if (missing(value)) {
        return(private$.slack_channel)
      } else {
        # TODO : url? with leading # or not?
        checkmate::check_character(value)
        private$.slack_channel <- value
      }
    },
    #' @field status_id
    #' integer. Returns or sets the integer corresponding to the status of the project.
    status_id = function(value) {
      if (missing(value)) {
        return(private$.status_id)
      } else {
        # TODO: Make dependent on internal data on status
        checkmate::check_integer(value, lower = 1, upper = 10)
        private$.status_id <- value
      }
    },
    #' @field organisation_id
    #' integer. Returns or sets the integer corresponding to the partner organization of the project.
    organisation_id = function(value) {
      if (missing(value)) {
        return(private$.organisation_id)
      } else {
        checkmate::check_integer(value, lower = 1)
        private$.organisation_id <- value
      }
    }
  ),
  public = list(
    #' create a project
    #' @param start_ym character. year and month of the predicted start of the project (~kickoff), in the form YYYY-mm
    #' @param abbreviation character. Three character, uppercase abbreviation, usually corresponding to the organization, e.g. COR for CorrelAid.
    #' @param name character. Short title of the project. 
    initialize = function(start_ym, abbreviation, name) {
      # check validity of inputs
      # check start_ym
      private$check_ym(start_ym)

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
    to_tibble = function() {
      print(self$active)
      tibble::tibble(
        
      )
    }
  )
)