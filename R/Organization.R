#' Organization
#' class representing a CorrelAid Partner Organization
#' @export
Organization <- R6::R6Class("Organization",
  private = list(
    .organization_id = NA_integer_,
    .organization_name = NA_character_,
    .website = NA_character_,
    .about = list(de = '', en = '')
  ),
  active = list(
    #' @field organization_id
    #' character. organization_id name of the organization
    organization_id = function(value) {
      if (missing(value)) {
        return(private$.organization_id)
      } else {
        usethis::ui_stop('read only.')
      }
    },
    #' @field organization_name
    #' character. organization_name name of the organization
    organization_name = function(value) {
      if (missing(value)) {
        return(private$.organization_name)
      } else {
        checkmate::assert_character(value)
        private$.organization_name <- value
        invisible(self)
      }
    },

    #' @field website
    #' character. website of the organization
    website = function(value) {
      if (missing(value)) {
        return(private$.website)
      } else {
        assert_url(value, 'website')
        private$.website <- value
        invisible(self)
      }
    },

    #' @field about
    #' tibble. Returns or sets the about paragraph of the organization.
    about = function(value) {
      if (missing(value)) {
        return(private$.about)
      } else {
        assert_lang_list(value)
        private$.about <- value
      }
      invisible(self)
    }
  ),
  public = list(
    #' create a organization
    #' @param organization_id character. ID of the organization. three letter abbreviation, uppercase. e.g. COR for CorrelAid
    #' @param organization_name character. name of organization
    #' @param website character. url of the website. defaults to NA
    initialize = function(organization_id, organization_name, website = NA) {
      checkmate::assert_character(organization_id, len = 1, pattern = '^[[:upper:]]{3}$')
      private$.organization_id <- organization_id
      
      self$organization_name <- organization_name
      self$website <- website
      self
    },

    
    #' to_tibble
    #' @description returns a one row tibble representation of the Volunteer object.
    to_tibble = function() {
      tibble::tibble(
        organization_id = private$.organization_id,
        organization_name = private$.organization_name,
        website = private$.website,
        about = list(private$.about)
      )
    },

    #' get_sql_tables
    #' @description a list function that returns tibble for each table
    get_sql_tables = function() {
      list(
        organization = self$as_tibble()
      )
    }
  )
)