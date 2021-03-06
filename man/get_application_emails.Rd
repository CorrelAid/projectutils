% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applications.R
\name{get_application_emails}
\alias{get_application_emails}
\title{get_application_emails}
\usage{
get_application_emails(mapping_df, selected_ids, get_discarded = FALSE)
}
\arguments{
\item{mapping_df}{tibble. Tibble with the mapping from applicant id to name & email}

\item{selected_ids}{numeric. vector with numeric ids of those who were selected from the team.}

\item{get_discarded}{boolean. whether to invert the selection in order to get the email addresses of those who didn't make the team. defaults to FALSE.}
}
\value{
character. invisibly returns vector of email addresses.
}
\description{
if clipr is available writes ; separated string of email addresses to the clipboard ready to copy into outlook
}
