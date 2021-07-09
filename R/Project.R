#' Project
#' class representing the basic properties of a CorrelAid Project
#' @export
Project <- R6::R6Class("Project",
  private = list(
    .tags = list(),
    .local_chapters = list(),
    .project_members = list(),
    .description = NULL,
    .organization = NULL,
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
    .num_gh_issue = NA_integer_,
    .url_ideation_pad = NA_character_,
    .url_call_pad = NA_character_,
    .slack_channel = NA_character_,
    .status_id = NA_integer_,
    .status = NA_character_,
    .organization_id = NA_character_,
    assert_name = function(value) {
      checkmate::assert_character(value, min.len = 1, max.len = 1)
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
          return(tibble::tibble(lc_id = numeric(), lc_name = character(), lc_name_full = character()))
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
    
    #' @field description
    #' tibble. Returns the description of the project as a Description object.
    description = function(value) {
      if (missing(value)) {
        return(private$.description)
      } else {
        description <- value
        checkmate::assert_class(description, c("Description", "R6"))
        private$.description <- description
      }
      invisible(self)
    },

    #' @field organization
    #' tibble. Returns the organization of the project as a Organization object.
    organization = function(value) {
      if (missing(value)) {
        return(private$.organization)
      } else {
        organization <- value
        checkmate::assert_class(organization, c("Organization", "R6"))
        private$.organization <- organization
        private$.organization_id <- organization$organization_id
      }
      invisible(self)
    },
    
    #' @field project_members
    #' tibble. Returns the project_members of the project as a tibble. Read-only.
    project_members = function(value) {
      if (missing(value)) {
        if (length(private$.project_members) == 0) {
          return(tibble::tibble())
        }
        project_members_df <- private$.project_members %>%
          purrr::map_dfr(function(pm) {
            pm$to_tibble()
        })
        return(project_members_df)
      } else {
        usethis::ui_stop("Can't set project_members. Please use the add_project_member function to add a project member to the project.")
      }
      invisible(self)
    },
    
    #' @field project_id
    #' character. id of the project, in the form YYYY-mm-ABB where ABB is any three-character, uppercase abbreviation
    project_id = function(value) {
      if (missing(value)) {
        return(private$.project_id)
      } else {
        assert_project_id(value)
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

    #' @field status
    #' integer. Returns the status of the project. Check out
    #' projectutils::status to see the available status and the corresponding ids.
    status = function(value) {
      if (missing(value)) {
        return(private$.status)
      } else {
        usethis::ui_stop("Can't set status. Please use the set_status function to change the status of the project.")
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
        if (!check_ym(value)) {
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
        if (!check_ym(value)) {
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
        if (!check_ym(value)) {
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
    #' character. Returns the full URL to the GitHub issue in the projects repository. read-only. to set use num_gh_issue.
    url_gh_issue = function(value) {
      if (missing(value)) {
        return(private$.url_gh_issue)
      } else {
        usethis::ui_stop("url_gh_issue is read-only. You can set it via the num_gh_issue field.")
      }
      invisible(self)
    },

    #' @field num_gh_issue
    #' integer. Returns the number of the issue in the projects repository. read-only (use u)
    num_gh_issue = function(value) {
      if (missing(value)) {
        return(private$.num_gh_issue)
      } else {
        value <- as.integer(value)
        checkmate::assert_integer(value, lower = 1)
        private$.num_gh_issue <- value
        issue_url <- glue::glue("https://github.com/CorrelAid/projects/issues/{value}")
        private$.url_gh_issue <- issue_url
      }
      invisible(self)
    },
    #' @field url_ideation_pad
    #' character. Returns or sets the full URL to the CodiMD Pad for the ideation phase of the project.
    url_ideation_pad = function(value) {
      if (missing(value)) {
        return(private$.url_ideation_pad)
      } else {
        checkmate::assert_character(value, pattern = "^https://pad.correlaid.org/.+?$")
        private$.url_ideation_pad <- value
      }
      invisible(self)
    },
    #' @field url_call_pad
    #' character. Returns or sets the full URL to the CodiMD Pad for the call for applications for the project.
    url_call_pad = function(value) {
      if (missing(value)) {
        return(private$.url_call_pad)
      } else {
        checkmate::assert_character(value, pattern = "^https://pad.correlaid.org/.+?$")
        private$.url_call_pad <- value
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
    #' integer. Returns the three letter, uppercase ID corresponding to the partner organization of the project. e.g. COR for CorrelAid
    organization_id = function(value) {
      if (missing(value)) {
        return(private$.organization_id)
      } else {
        usethis::ui_stop('read only. set by setting organization field.')
      }
      invisible(self)
    }
  ),
  public = list(
    #' create a project
    #' @param project_id character. id of the project, in the form YYYY-mm-ABB where ABB is any three-character, uppercase abbreviation
    #' @param name character. Short title of the project.
    initialize = function(project_id, name) {
      # check validity of project_id
      assert_project_id(project_id)
      private$.project_id <- project_id

      # initialize description
      private$.description <- Description$new(project_id)
      # start_ym can be derived from project id
      private$.start_ym <- stringr::str_extract(project_id, "\\d{4}\\-\\d{2}")
      
      # name needs to be character
      checkmate::assert_character(name, min.len = 1, max.len = 1)
      private$.name <- name

      invisible(self)
    },

    #' print 
    #' @description print the project
    print = function() {
      self$to_tibble() %>% dplyr::glimpse()
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

    #' add a local chapter to the project
    #' @param volunteer_id integer. id of the volunteer.
    #' @param role character. role of the volunteer. see projectutils::roles for available options.
    #' @param ... optional arguments for ProjectMember constructor
    add_project_member = function(volunteer_id, role, ...) {
      pm <- ProjectMember$new(self$project_id, volunteer_id, role, ...)
      private$.project_members <- c(private$.project_members, pm)
      invisible(self)
    },

    #' get team member by volunteer id
    #' @param volunteer_id integer. numeric id of the volunteer.
    get_team_member = function(volunteer_id) { 

      tm <- private$.project_members %>% 
            purrr::keep(function(pm) pm$volunteer_id == volunteer_id) 
       if (length(tm) == 0) {
         usethis::ui_stop(glue::glue("Volunteer {volunteer_id} is not part of the project."))
       }
       tm[[1]]
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
        name = self$name,
        start_ym = self$start_ym,
        end_ym_predicted = self$end_ym_predicted,
        end_ym = self$end_ym,
        is_public = self$is_public,
        is_internal = self$is_internal,
        url_git_repo = self$url_git_repo,
        url_gh_issue = self$url_gh_issue,
        url_ideation_pad = self$url_ideation_pad, 
        url_call_pad = self$url_call_pad,
        slack_channel = self$slack_channel,
        organization_id = self$organization_id,
        tags = list(self$tags),
        local_chapters = list(self$local_chapters),
        project_members = list(self$project_members),
        status_id = self$status_id,
        status = self$status,
        description = list(self$description$to_tibble()),
        organization = list(self$organization$to_tibble())
      )
    },

    #' get_sql_tables
    #' @description function that returns a tibble for each table
    get_sql_tables = function() {
      list( 
        project = self$to_tibble() %>% 
            dplyr::select(-tags, -local_chapters, -project_members, -status, -description),
        projecttag = tibble::tibble(
          project_id = private$.project_id,
          tag_id = self$tags$tag_id
        ), 
        tag = projectutils::tags,
        projectlocalchapter = tibble::tibble(
          project_id = private$.project_id,
          lc_id = self$local_chapters$lc_id
        ),
        projectmember = self$project_members,
        description = self$description$to_tibble()
      )
    }
  )
)
