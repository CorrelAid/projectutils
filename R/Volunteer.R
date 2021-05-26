#' Volunteer
#' class representing a CorrelAid Volunteer
#' @export
Volunteer <- R6::R6Class("Volunteer",
  private = list(
    .volunteer_id = NA_integer_,
    .projects = list(),
    .first_name = "",
    .last_name = "",
    .user_gh = NA_character_,
    .user_gl = NA_character_,
    .user_twitter = NA_character_,
    .url_website = NA_character_,
    .url_linkedin = NA_character_,
    .url_xing = NA_character_,
    .email = NA_character_,
    .lc_id = NA_integer_,
    assert_name = function(value) {
      checkmate::assert_character(value, min.len = 1, max.len = 40)
    },
    assert_not_url = function(value, arg_name) {
        if (stringr::str_detect(value, "^http(s)?://")) {
          usethis::ui_stop(glue::glue("Only specify the user name for {arg_name}, not the complete URL."))
        }
    },
    assert_email = function(email) {
        regex <- "([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))"
        if(!checkmate::test_character(email, pattern = regex)) {
            usethis::ui_stop("Invalid email address.")
        }
    },
    assert_url = function(value, arg_name, domain = NA) {
        # see https://cran.r-project.org/web/packages/rex/vignettes/url_parsing.html
        if(!checkmate::test_character(value, pattern = "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$")) {
            usethis::ui_stop(glue::glue("Invalid URL for {arg_name}"))
        }

        if(!is.na(domain)) {
            if(!checkmate::test_character(value, pattern = domain)) {
                usethis::ui_stop(glue::glue("URL for {arg_name} must include domain {domain}."))
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
        invisible(self)
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
        invisible(self)
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
        invisible(self)
      }
    },

    #' @field user_gh
    #' character. github username of the volunteer.
    user_gh = function(value) {
      if (missing(value)) {
        return(private$.user_gh)
      } else {
        checkmate::assert_character(value, len = 1)
        private$assert_not_url(value, "user_gh")
        private$.user_gh <- value
        invisible(self)
      }
    },

    #' @field user_gl
    #' character. gitlab username of the volunteer.
    user_gl = function(value) {
      if (missing(value)) {
        return(private$.user_gl)
      } else {
        checkmate::assert_character(value, len = 1)
        private$assert_not_url(value, "user_gl")
        private$.user_gl <- value
        invisible(self)

      }
    },

    #' @field user_twitter
    #' character. twitter username of the volunteer.
    user_twitter = function(value) {
      if (missing(value)) {
        return(private$.user_twitter)
      } else {
        checkmate::assert_character(value, len = 1)
        private$assert_not_url(value, "user_twitter")
        private$.user_twitter <- value
        invisible(self)
      }
    },

    #' @field url_website
    #' character. personal website of the volunteer.
    url_website = function(value) {
      if (missing(value)) {
        return(private$.url_website)
      } else {
        private$assert_url(value, "url_website")
        private$.url_website <- value
        invisible(self)
      }
    },
    #' @field url_linkedin
    #' character. url to the linkedin profil of the volunteer
    url_linkedin = function(value) {
      if (missing(value)) {
        return(private$.url_linkedin)
      } else {
        private$assert_url(value, "url_linkedin", domain = "linkedin.com")
        private$.url_linkedin <- value
        invisible(self)
      }
    },
    #' @field url_xing
    #' character. url to the xing profil of the volunteer
    url_xing = function(value) {
      if (missing(value)) {
        return(private$.url_xing)
      } else {
        private$assert_url(value, "url_xing", domain = "xing.com")
        private$.url_xing <- value
        invisible(self)
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
      checkmate::assert_character(first_name, len = 1)
      checkmate::assert_character(last_name, len = 1)
      checkmate::assert_character(email, len = 1)

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
        url_xing = private$.url_xing,
        url_website = private$.url_website,
        lc_id = private$.lc_id,
        projects = list(self$projects)
      )
    },

    #' set_local_chapter
    #' @param lc_name name of the local chapter. see projectutils::local_chapters for options.
    #' @description sets the local chapter of the volunteer
    set_local_chapter = function(lc_name) {

      if (!checkmate::test_character(lc_name, len = 1)) {
        usethis::ui_stop("You can't assign a volunteer to more than one chapter.")
      }

      lc_id <- projectutils::local_chapters %>% 
          dplyr::filter(lc_name == .env$lc_name) %>% 
          dplyr::pull(lc_id)

      if (length(lc_id) == 0) {
        lc_options <- paste(projectutils::local_chapters$lc_name, collapse = ", ")
        usethis::ui_stop(glue::glue("{lc_name} is not a valid chapter. Valid options are: {lc_options}"))
      }
      private$.lc_id <- lc_id
      invisible(self)
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