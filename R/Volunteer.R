#' Volunteer
#' class representing a CorrelAid Volunteer
#' @export
Volunteer <- R6::R6Class("Volunteer",
  private = list(
    .volunteer_id = NA,
    .projects = list(),
    .first_name = "",
    .last_name = "",
    .user_gh = NA,
    .user_gl = NA,
    .user_twitter = NA,
    .url_website = NA,
    .url_linkedin = NA,
    .url_xing = NA,
    .email = NA,
    .lc_id = NA,
    assert_name = function(value) {
      checkmate::assert_character(value, min.len = 1, max.len = 40)
    },

    assert_email = function(value) {
        regex <- "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>"
        if(!checkmate::test_character(value, pattern = regex)) {
            usethis::ui_stop("Invalid email address.")
        }
    },
    assert_url = function(value, domain = NA) {
        # see https://cran.r-project.org/web/packages/rex/vignettes/url_parsing.html
        if(!checkmate::test_character(value, pattern = "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")) {
            usethis::ui_stop("Invalid URL.")
        }

        if(!is.na(domain)) {
            if(!checkmate::test_character(value, pattern = domain)) {
                usethis::ui_stop(glue::glue("URL must include domain {domain}."))
            }
        }
    }

  ),
  active = list(
    #' @field first_name
    #' character. first_name of the volunteer
    first_name = function(value) {
      if (missing(value)) {
        return(private$.first_name)
      } else {
        private$assert_name(value)
        private$.first_name <- value
      }
    },
        #' @field last_name
    #' character. last_name of the volunteer
    last_name = function(value) {
      if (missing(value)) {
        return(private$.last_name)
      } else {
        private$assert_name(value)
        private$.last_name <- value
      }
    },


    #' @field email
    #' character. Email of the volunteer
    email = function(value) {
      if (missing(value)) {
        return(private$.email)
      } else {
        private$assert_email(value)
        private$.email <- value
      }
    },

    #' @field user_gh
    #' character. github username of the volunteer.
    user_gh = function(value) {
      if (missing(value)) {
        return(private$.user_gh)
      } else {
        checkmate::assert_character(value)
        private$.user_gh <- value
      }
    },

    #' @field user_gl
    #' character. gitlab username of the volunteer.
    user_gl = function(value) {
      if (missing(value)) {
        return(private$.user_gl)
      } else {
        checkmate::assert_character(value)
        private$.user_gl <- value
      }
    },

    #' @field user_twitter
    #' character. twitter username of the volunteer.
    user_twitter = function(value) {
      if (missing(value)) {
        return(private$.user_twitter)
      } else {
        checkmate::assert_character(value)
        private$.user_twitter <- value
      }
    },

    #' @field url_website
    #' character. personal website of the volunteer.
    url_website = function(value) {
      if (missing(value)) {
        return(private$.url_website)
      } else {
        private$assert_url(value)
        private$.url_website <- value
      }
    },
    #' @field url_linkedin
    #' character. url to the linkedin profil of the volunteer
    url_linkedin = function(value) {
      if (missing(value)) {
        return(private$.url_linkedin)
      } else {
        private$assert_url(value, domain = "linkedin.com")
        private$.url_linkedin <- value
      }
    },
    #' @field url_xing
    #' character. url to the xing profil of the volunteer
    url_xing = function(value) {
      if (missing(value)) {
        return(private$.url_xing)
      } else {
        private$assert_url(value, domain = "linkedin.com")
        private$.url_xing <- value
      }
    },

    #' @field lc_id
    #' integer. Returns the integer corresponding to the local chapter of the volunteer.
    lc_id = function(value) {

    if (missing(value)) {
        return(private$.lc_id)
      } else {
        usethis::ui_stop("Can't set lc_id. Please use the set_local_chapter function to change the status of the project.")
      }
    }
  ),
  public = list(
    #' create a volunteer
    #' @param first_name character. first name of the volunteer
    #' @param last_name character. last name of the volunteer
    #' @param email character. email of the volunteers
    initialize = function(first_name, last_name, email) {
      private$assert_name(first_name)
      private$assert_name(last_name)
      private$assert_email(email)

      private$.first_name <- first_name
      private$.last_name <- last_name
      private$.email <- email
      invisible(self)
    },

    
    #' as_tibble
    #' @description returns a one row tibble representation of the Volunteer object.
    as_tibble = function() {
      tibble::tibble(
        volunteer_id = private$.project_id,
        first_name = private$.first_name,
        last_name = private$.start_ym,
        email = private$.end_ym_predicted,
        user_gh = private$.user_gh,
        user_gl = private$.user_gl,
        user_twitter = private$.user_twitter,
        url_linkedin = private$.url_linkedin,
        url_xing = private$url_xing,
        url_website = private$.url_website,
        lc_id = private$.lc_id,
        projects = self$projects
      )
    },

    #' sql_tables
    #' @description a list function that returns tibble for each table
    sql_tables = function() {
      list(
        volunteer = self$as_tibble() %>% 
            dplyr::select(-projects)
      )
    }
  )
)