#' Tag
#' class for a project tag. internal.
Tag <- R6::R6Class(
    private = list(
        .tag_id = NA,
        .category = NA,
        .value = NA
    ),
    public = list(
        #' creates a tag object
        #' @param category character. the tag category. see projectutils::tags for available values.
        #' @param value character. the tag value. see projectutils::tags for available values and combinations. Defaults to NA.
        initialize = function(category, value = NA) {
            category_options <- projectutils::tags$category
            if (!category %in% category_options) {
                usethis::ui_stop(glue::glue("Category {category} is not valid. Valid options are: \n{category_options}"))
            }
            private$.category <- category

            value_options <- projectutils::tags %>%
                dplyr::filter(category == .env$category) %>%
                dplyr::pull(value)
            if (!value %in% value_options) {
                value_options_chr <- paste(value_options, collapse = ", ")
                usethis::ui_stop(glue::glue("Value {value} is not valid for category {category}. Valid options are: \n{value_options_chr}"))
            }
            private$.value <- value

            if (is.na(value)) {
                private$.tag_id <- projectutils::tags %>%
                    dplyr::filter(category == .env$category) %>%
                    dplyr::pull(tag_id)
            } else {
                private$.tag_id <- projectutils::tags %>%
                    dplyr::filter(category == .env$category & value == .env$value) %>%
                    dplyr::pull(tag_id)
            }
        },
        #' to_tibble
        #' @description representation of tag as a one-row tibble
        to_tibble = function() {
            tibble::tibble_row(
                tag_id = private$.tag_id,
                category = private$.category,
                value = private$.value
            )
        }
    )
)