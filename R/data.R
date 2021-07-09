#' Possible tags for CorrelAid projects
#'
#' A dataset containing possible project tags, derived from the GitHub projects repo
#'
#' @format A data frame with 56 rows and 3 variables:
#' \describe{
#'   \item{tag_category}{category of the tag}
#'   \item{tag_value}{value of the tag}
#'   \item{tag_id}{numeric id}
#' }
#' @source derived from the category:value labels of the GitHub projects repo \url{https://github.com/CorrelAid/projects/labels}
"tags"


#' Possible roles for CorrelAid projects
#'
#' A dataset containing possible roles in CorrelAid projects
#'
#' @format A data frame with 3 rows and 2 variables:
#' \describe{
#'   \item{role_id}{numeric id}
#'   \item{role}{name of the role, in snakecase}
#' }
#' @source \url{https://docs.correlaid.org/project-manual/data4good-projects}
"roles"

#' CorrelAid local chapters
#'
#' A dataset containing all CorrelAid local chapters, derived from 
#' the `lc` labels in the CorrelAid projects GitHub repo.
#'
#' @format A data frame with 16 rows and 3 variables:
#' \describe{
#'   \item{lc_id}{numeric id}
#'   \item{lc_name}{name of the local chapter, in snakecase}
#'   \item{lc_name_full}{human readable of the local chapter}
#' }
#' @source \url{https://github.com/CorrelAid/projects/labels}
"local_chapters"


#' Possible status for CorrelAid projects
#'
#' A dataset containing possible status/phases for CorrelAid projects
#' taken from the Kanban board in the CorrelAid projects GitHub repo
#' 
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#'   \item{status_id}{numeric id}
#'   \item{status}{human readable name of the status}
#'   \item{status_description}{description of the status, currently empty}
#' }
#' @source \url{https://github.com/CorrelAid/projects/projects/1}
"status"