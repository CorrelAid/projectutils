#' ProjectMember
#' class representing a CorrelAid Project Member
#' @export
ProjectMember <- R6::R6Class("ProjectMember",
  private = list(
    .project_member_id = NA_integer_,
    .project_id = NA_character_,
    .volunteer_id = NA_character_,
    .team_id = 1,
    .start_active_ym = NA_character_,
    .end_active_ym = NA_character_,
    .behaviour_flag = FALSE,
    .behaviour_description = NA_character_,
    .is_public = NA,
    .role_id = NA_integer_,
    .role = NA_character_
  ),
  active = list(
    #' @field project_id
    #' character. id of the project in the form YYYY-mm-{ABB}.
    project_id = function(value) {
      if (missing(value)) {
        return(private$.project_id)
      } else {
        usethis::ui_stop("Can't set project_id. Recreate object.")
        invisible(self)
      }
    },

    #' @field volunteer_id
    #' character. id of the volunteer.
    volunteer_id = function(value) {
      if (missing(value)) {
        return(private$.volunteer_id)
      } else {
        usethis::ui_stop("Can't set volunteer_id. Recreate object.")
        invisible(self)
      }
    },

    #' @field team_id
    #' character. id of the team. Defaults to 1.
    team_id = function(value) {
      if (missing(value)) {
        return(private$.team_id)
      } else {
        usethis::ui_stop("Can't set team_id. So far we only assume one team per project.")
        invisible(self)
      }
    },

    #' @field start_active_ym
    #' character. start_active_ym when the project member became active
    start_active_ym = function(value) {
      if (missing(value)) {
        return(private$.start_active_ym)
      } else {
        if (!check_ym(value)) {
            usethis::ui_stop("Invalid value for start_active_ym. Must conform to %Y-%m.")
        }
        private$.start_active_ym <- value
        invisible(self)
      }
    },

    #' @field end_active_ym
    #' character. end_active_ym when the project member became active
    end_active_ym = function(value) {
      if (missing(value)) {
        return(private$.end_active_ym)
      } else {
        if (!check_ym(value)) {
            usethis::ui_stop("Invalid value for end_active_ym. Must conform to %Y-%m.")
        }
        private$.end_active_ym <- value
        invisible(self)
      }
    },

    #' @field behaviour_flag
    #' boolean. whether or not there was noteworthy behaviour of the volunteer
    behaviour_flag = function(value) {
      if (missing(value)) {
        return(private$.behaviour_flag)
      } else {
        usethis::ui_stop("Can't set behaviour_flag directly. Please use the set_behaviour method.")
      }
    },

    #' @field behaviour_description
    #' character. description of the behaviour.
    behaviour_description = function(value) {
      if (missing(value)) {
        return(private$.behaviour_description)
      } else {
        usethis::ui_stop("Can't set behaviour_flag directly. Please use the set_behaviour method.")
      }
    },

    #' @field is_public
    #' boolean. should the member be included in the project description on the website.
    is_public = function(value) {
      if (missing(value)) {
        return(private$.is_public)
      } else {
        checkmate::assert_logical(value, len = 1)
        private$.is_public <- value
        invisible(self)
      }
    },

    #' @field role_id
    #' integer. id of the role of the project member.
    role_id = function(value) {
      if (missing(value)) {
        return(private$.role_id)
      } else {
        usethis::ui_stop("Can't set role_id. Please use the set_role function to change the role of the project member.")
      }
    },
    #' @field role
    #' integer. role of the project member.
    role = function(value) {
      if (missing(value)) {
        return(private$.role)
      } else {
        usethis::ui_stop("Can't set role. Please use the set_role function to change the role of the project member.")
      }
    }
  ),
  public = list(
    #' create a volunteer
    #' @param project_id character. ID of the project.
    #' @param volunteer_id integer. ID of the volunteer
    #' @param role character. name of the project role the volunteer assumes. Check projectutils::roles for available options.
    initialize = function(project_id, volunteer_id, role) {
      # TODO: Check against existing projects
      # TODO: check against existing volunteers

      checkmate::assert_number(volunteer_id)
      private$.volunteer_id <- volunteer_id
      assert_project_id(project_id)
      private$.project_id <- project_id
      # set role using method
      self$set_role(role)
      self
    },

    
    #' to_tibble
    #' @description returns a one row tibble representation of the Volunteer object.
    to_tibble = function() {
      tibble::tibble(
        project_member_id = private$.project_member_id,
        project_id = private$.project_id,
        volunteer_id = private$.volunteer_id,
        team_id = private$.team_id,
        start_active_ym = private$.start_active_ym,
        end_active_ym = private$.end_active_ym,
        behaviour_flag = private$.behaviour_flag,
        behaviour_description = private$.behaviour_description,
        is_public = private$.is_public,
        role_id = private$.role_id,
        role = private$.role
      )
    },

    #' set_role
    #' @param role character. role of the volunteer. see projectutils::roles for available options.
    set_role = function(role) {
      roles <- projectutils::roles
      checkmate::assert_character(role, len = 1)
      if (!checkmate::test_choice(role, roles$role)) {
          role_options <- paste(roles$role, collapse = ", ")
          usethis::ui_stop(glue::glue("{role} is not a valid role. The options are: {role_options}"))
      }

      private$.role_id <- roles  %>% 
            dplyr::filter(role == .env$role) %>% 
            dplyr::pull(role_id)
      invisible(self)
    }, 

    #' set_behaviour
    #' @param flag boolean. value of the flag for offending behaviour.
    #' @param description character. description of the offending behaviour. Defaults to NA_character_ , needs to be specified if setting the flag to TRUE.
    set_behaviour = function(flag, description = NA) {
        # flag needs to be logical 
        checkmate::assert_logical(flag, len = 1)

        if (flag) {
          # flag = TRUE, something happened :eyes: 
          # need to pass description
          if (is.na(description) | !checkmate::test_character(description, len = 1)) {
            usethis::ui_stop("You need to pass a description of the behaviour when setting flag to TRUE.")
          }
        } else {
          # no offending behaviour, flag = FALSE
          # no description when no offending behaviour!
          if (!checkmate::test_scalar_na(description)) {
            usethis::ui_stop("You can't pass a description when flag is FALSE.")
          }

          usethis::ui_done("Setting behaviour_flag to FALSE and behaviour_description to NA.")
        }
        private$.behaviour_description <- description
        private$.behaviour_flag <- flag
        invisible(self)
    }, 
    #' get_sql_tables
    #' @description a list function that returns tibble for each table
    get_sql_tables = function() {
      list(
        projectmember = self$as_tibble() %>% 
          dplyr::select(-role)
      )
    }
  )
)