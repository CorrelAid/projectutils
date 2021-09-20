#' get_project_status
#' get status of project from the GitHub projects kanban board
#' @param gh_issue_num numeric. number of the github issue.
#' @param github_board_id character. internal id of the Project in the GitHub API. Get via the API. Defaults to the ID for the CorrelAid projects board.
#' @return character. status / column of the project in the kanban board
#' @export
#' @importFrom rlang .data
get_project_status <- function(gh_issue_num, github_board_id = '3771798') {
    columns <- gh::gh("/projects/{project_id}/columns",
        project_id = github_board_id,
        .accept = "application/vnd.github.inertia-preview+json"
    ) %>%
        purrr::map(`[`, c("id", "name", "cards_url"))

    cards <- columns %>%
        purrr::map_dfr(function(column) {
            issue_urls <- gh::gh(column$cards_url, .accept = "application/vnd.github.inertia-preview+json", .limit = 100) %>%
                purrr::map_chr("content_url", .default = NA)
            tibble::tibble(name = column$name, issue_url = issue_urls) %>%
                dplyr::mutate(issue_num = stringr::str_replace_all(.data$issue_url, ".+?/(\\d{1,})$", "\\1"))
        })
    if (!gh_issue_num %in% cards$issue_num) {
        usethis::ui_warn(glue::glue("{gh_issue_num} is not part of the board. Returning NA.\n"))
        return(NA)
    }
    cards %>%
        dplyr::filter(.data$issue_num == gh_issue_num) %>%
        dplyr::pull(.data$name)
}

#' get_gh_issue_data
#' get data from github issue that matches a given project_id
#' @param project_id character. id of the project, in the form YYYY-mm-ABB where ABB is any three-character, uppercase abbreviation
#' @return list. list with title, number, body and labels sub lists
#' @export
#' @importFrom rlang .data
get_gh_issue_data <- function(project_id) {
    gh_issues <- gh::gh("GET /repos/{owner}/{repo}/issues", repo = "projects", owner = "correlaid", state = "all", .limit = Inf,
    .accept = 'application/vnd.github.starfox-preview')
    gh_issue <- gh_issues %>%
        purrr::keep(function(issue) stringr::str_detect(issue$title, project_id))
    if (length(gh_issue) == 0) {
        usethis::ui_stop(glue::glue("no issue found that matches {project_id}."))
    }
    if (length(gh_issue) > 1) {
        usethis::ui_stop(glue::glue("more than one issue found that matches {project_id}."))
    }

    # data wrangling
    # extract project id and name
    gh_issue <- gh_issue[[1]]
    title_split <- stringr::str_split(gh_issue$title, ":", n = 2)
    name <- title_split[[1]][2] %>% stringr::str_trim()

    # tags and local chapter
    labels_df <- gh_issue$labels %>%
        purrr::map_chr("name") %>%
        tibble::tibble(name = .) %>%
        tidyr::separate(.data$name, into = c("category", "value"), sep = ":", fill = "right")
    # local chapters based on tags
    lcs <- labels_df %>%
        dplyr::filter(.data$category == "lc") %>%
        dplyr::pull(.data$value)

    return(
        list(
            name = name, 
            tags = labels_df %>% dplyr::filter(.data$category != 'lc'), 
            lc = lcs,
            num_gh_issue = gh_issue$number,
            text = gh_issue$body, 
            pad_links = extract_pad_links(gh_issue$body), 
            urls = extract_urls(gh_issue$body)
        )
    )
}

#' extract codimd pad links from text 
#' @param txt character. text of the github issue.
#' @return character. vector of pad links
extract_pad_links <- function(txt) {
    unlist(stringr::str_extract_all(txt, "https://pad.correlaid.org/.+"))
}

#' extract urls
#' @param txt character. text of the github issue.
#' @return character. vector of urls
extract_urls <- function(txt) {
    unlist(stringr::str_extract_all(txt, "(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"))
}