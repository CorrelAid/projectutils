#' Project
#' class representing the basic properties of a CorrelAid Project
#' @export
Project <- R6::R6Class("Project",
  private = list(
    .tags = list(),
    .local_chapters = list(),
    .project_id = "",
    .name = "",
    .slug = NA_character_,
    .is_public = FALSE,
    .start_ym = NA_character_,
    .end_ym = NA_character_,
    .end_ym_predicted = NA_character_,
    .is_internal = FALSE,
    .url_git_repo = NA_character_,
    .url_gh_issue = NA_character_,
    .url_pad = NA_character_,
    .slack_channel = NA_character_,
    .status_id = NA_integer_,
    .status = NA_character_,
    .organization_id = NA_integer_,
    assert_name = function(value) {
      checkmate::assert_character(value, min.len = 1, max.len = 1)
    },
    assert_project_id = function(value) {
      if (!stringr::str_detect(value, "^\\d{4}\\-\\d{2}\\-[:upper:]{3,3}$")) usethis::ui_stop("Invalid project id. It needs to be conform to the following format: YYYY-mm-{ABB} where {ABB} is uppercase")
    },
    check_ym = function(value) {
      checkmate::test_character(value, pattern = "^\\d{4}\\-\\d{2}$")
    }
  ),
  active = list(
    #' @field tags
    #' tibble. Returns the tags of the project as a tibble. Read-only.
    tags = function(value) {
      if (missing(value)) {
        if (length(private$.tags) == 0) {
          # return empty tibble if no tags have been assigned
          return(tibble::tibble(tag_id = character(), category = character(), value = character()))
        }
        tags_df <- private$.tags %>%
          purrr::map_dfr(function(tag) {
            tag$to_tibble()
          }) %>%
          dplyr::distinct()
        return(tags_df)
      } else {
        usethis::ui_stop("Can't set tags. Please use the add_tag function to add tags to the project.")
      }
      invisible(self)
    },

    #' @field local_chapters
    #' tibble. Returns the local_chapters of the project as a tibble. Read-only.
    local_chapters = function(value) {
      if (missing(value)) {
        if (length(private$.local_chapters) == 0) {
          return(tibble::tibble(lc_id = character(), lc_name = character(), lc_name_full = character()))
        }
        local_chapters_df <- private$.local_chapters %>%
          purrr::map_dfr(function(lc) {
            lc$to_tibble()
          })
        return(local_chapters_df)
      } else {
        usethis::ui_stop("Can't set local_chapters. Please use the add_local_chapter function to add local chapters to the project.")
      }
      invisible(self)
    },

    #' @field project_id
    #' character. id of the project, in the form YYYY-mm-ABB where ABB is any three-character, uppercase abbreviation
    project_id = function(value) {
      if (missing(value)) {
        return(private$.project_id)
      } else {
        private$assert_project_id(value)
        private$.project_id <- value
      }
      invisible(self)
    },
    #' @field name
    #' character. title of the project.
    name = function(value) {
      if (missing(value)) {
        return(private$.name)
      } else {
        private$assert_name(value)
        private$.name <- value
      }
      invisible(self)
    },


    #' @field status_id
    #' integer. Returns integer corresponding to the status of the project. Check out
    #' projectutils::status to see the available status and the corresponding ids.
    status_id = function(value) {
      if (missing(value)) {
        return(private$.status_id)
      } else {
        usethis::ui_stop("Can't set status_id. Please use the set_status function to change the status of the project.")
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
      invisible(self)
    },
    #' @field is_public
    #' boolean. Returns or sets whether the project is made public on the CorrelAid website. Use with caution!
    is_public = function(value) {
      if (missing(value)) {
        return(private$.is_public)
      } else {
        checkmate::assert_logical(value)
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
      invisible(self)
    },

    #' @field start_ym
    #' character. Return or set the start of the project, in YYYY-mm form. Does not change the project id.
    start_ym = function(value) {
      # getter
      if (missing(value)) {
        return(private$.start_ym)
      } else {
        if (checkmate::test_scalar_na(value)) {
          usethis::ui_stop("You can't set start_ym to NA")
        }
        if (!private$check_ym(value)) {
          usethis::ui_stop(usethis::ui_stop(glue::glue("Invalid format: start_ym needs to be set to a character in the format YYYY-mm")))
        }

        private$.start_ym <- value
        usethis::ui_warn("You changed the start_ym of the project. You might want to change the project_id as well.")
        return(invisible(self))
      }
    },
    #' @field end_ym
    #' character. Return or set the actual end of the project, in YYYY-mm form.
    end_ym = function(value) {
      if (missing(value)) {
        return(private$.end_ym)
      } else {
        # setter
        # we can set to NA
        if (is.na(value)) {
          private$.end_ym <- value
          return(invisible(self))
        }

        # character
        checkmate::assert_character(value)
        if (!private$check_ym(value)) {
          usethis::ui_stop(usethis::ui_stop(glue::glue("Invalid format: end_ym needs to be set to a character in the format YYYY-mm")))
        }
        private$.end_ym <- value
      }
      invisible(self)
    },
    #' @field end_ym_predicted
    #' character. Return or set the predicted end of the project, in YYYY-mm form.
    end_ym_predicted = function(value) {
      if (missing(value)) {
        return(private$.end_ym_predicted)
      } else {
        # setter
        # we can set to NA
        if (is.na(value)) {
          private$.end_ym_predicted <- value
          return(invisible(self))
        }

        # character
        checkmate::assert_character(value)
        if (!private$check_ym(value)) {
          usethis::ui_stop(usethis::ui_stop(glue::glue("Invalid format: end_ym_predicted needs to be set to a character in the format YYYY-mm")))
        }
        private$.end_ym_predicted <- value
      }
      invisible(self)
    },
    #' @field is_internal
    #' boolean. Return or set whether or not the project is an internal project.
    is_internal = function(value) {
      if (missing(value)) {
        return(private$.is_internal)
      } else {
        checkmate::assert_logical(value)
        private$.is_internal <- value
      }
      invisible(self)
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
        value <- as.integer(value)
        checkmate::assert_integer(value, lower = 1)
        issue_url <- glue::glue("https://github.com/CorrelAid/projects/issues/{value}")
        private$.url_gh_issue <- issue_url
        usethis::ui_done(glue::glue("Set url_gh_issue to {issue_url}"))
      }
      invisible(self)
    },
    #' @field url_pad
    #' character. Returns or sets the full URL to the CodiMD Pad for the project.
    url_pad = function(value) {
      if (missing(value)) {
        return(private$.url_pad)
      } else {
        checkmate::assert_character(value, pattern = "^https://pad.correlaid.org/.+?$")
        private$.url_pad <- value
      }
      invisible(self)
    },
    #' @field slack_channel
    #' character. Returns or sets the name of the Slack channel for the project.
    slack_channel = function(value) {
      if (missing(value)) {
        return(private$.slack_channel)
      } else {
        # TODO : url? with leading # or not?
        checkmate::assert_character(value)
        private$.slack_channel <- value
      }
      invisible(self)
    },

    #' @field organization_id
    #' integer. Returns or sets the integer corresponding to the partner organization of the project.
    organization_id = function(value) {
      if (missing(value)) {
        return(private$.organization_id)
      } else {
        checkmate::assert_number(value, lower = 1)
        private$.organization_id <- value
      }
      invisible(self)
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
      checkmate::assert_character(start_ym)
      if (!private$check_ym(start_ym)) {
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