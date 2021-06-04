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
    .further_links = list(en = "", de = ""),
    
    assert_lang_list = function(lang_list, max_len_each = 1) {
      checkmate::assert_list(lang_list, types = c("character"))
      checkmate::assert_set_equal(names(lang_list), c("en", "de"))

      checkmate::assert_character(lang_list$en, len = max_len_each)
      checkmate::assert_character(lang_list$de, len = max_len_each)
    }
  ),
  active = list(
    #' @field summary
    #' tibble. Returns the summary list of the project. read-only.
    summary = function(value) {
      if (missing(value)) {
        return(private$.summary)
      } else {
        assert_lang_list(value)
        private$.summary <- value
      }
      invisible(self)
    },
    #' @field problem
    #' tibble. Returns the problem list of the project. read-only.
    problem = function(value) {
      if (missing(value)) {
        return(private$.problem)
      } else {
        assert_lang_list(value)
        private$.problem <- value
      }
      invisible(self)
    },
    #' @field data
    #' tibble. Returns the data list of the project. read-only.
    data = function(value) {
      if (missing(value)) {
        return(private$.data)
      } else {
        assert_lang_list(value)
        private$.data <- value
      }
      invisible(self)
    },
    #' @field approach
    #' tibble. Returns the approach list of the project. read-only.
    approach = function(value) {
      if (missing(value)) {
        return(private$.approach)
      } else {
        assert_lang_list(value)
        private$.approach <- value
      }
      invisible(self)
    },
    #' @field impact
    #' tibble. Returns the impact list of the project. read-only.
    impact = function(value) {
      if (missing(value)) {
        return(private$.impact)
      } else {
        assert_lang_list(value)
        private$.impact <- value
      }
      invisible(self)
    },

    #' @field further_links
    #' tibble. Returns or sets the further links list of the project.
    further_links = function(value) {
      if (missing(value)) {
        return(private$.further_links)
      } else {
        assert_lang_list(value, max_len_each = 100)
        private$.further_links <- value
      }
      invisible(self)
    }
  ),
  public = list(
    #' create a description of a project
    #' @param project_id character. id of the project
    initialize = function(project_id) {
      # assert project id
      assert_project_id(project_id)
      private$.project_id <- project_id
      invisible(self)
    },
    #' to_tibble
    #' @description returns a one row tibble representation of the Description object.
    to_tibble = function() {
      tibble::tibble(
        project_id = private$.project_id,
        title = list(private$.title),
        summary = list(private$.summary),
        problem = list(private$.problem),
        data = list(private$.data),
        approach = list(private$.approach),
        impact = list(private$.impact),
        further_links = list(private$.further_links)
      )
    },
    #' get_sql_tables
    #' @description function that returns a tibble for each table
    get_sql_tables = function() {
      list(
        description = self$to_tibble()
      )
    }
  )
)