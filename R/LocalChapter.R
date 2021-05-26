#' LocalChapter
#' class for a Local Chapter. internal.
LocalChapter <- R6::R6Class(
    private = list(
        .lc_id = NA,
        .lc_name = NA,
        .lc_name_full = NA
    ),
    public = list(
        #' creates a local chapter object
        #' @param lc_name character. the lc_name. see projectutils::local_chapters for available values.
        initialize = function(lc_name) {
            lc_name_options <- projectutils::local_chapters$lc_name
            if (!lc_name %in% lc_name_options) {
                lc_name_options_print <- paste(lc_name_options, collapse = ", ")
                usethis::ui_stop(glue::glue("lc_name {lc_name} is not valid. Valid options are: \n{lc_name_options_print}"))
            }
            private$.lc_name <- lc_name
            private$.lc_id <- projectutils::local_chapters %>%
                dplyr::filter(lc_name == .env$lc_name) %>%
                dplyr::pull(lc_id)

            private$.lc_name_full <- projectutils::local_chapters %>%
                dplyr::filter(lc_name == .env$lc_name) %>%
                dplyr::pull(lc_name_full)

        },
        #' to_tibble
        #' @description representation of local chapter as a one-row tibble
        to_tibble = function() {
            tibble::tibble_row(
                lc_id = private$.lc_id,
                lc_name = private$.lc_name,
                lc_name_full = private$.lc_name_full
            )
        }
    )
)