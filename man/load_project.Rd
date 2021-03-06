% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_utils.R
\name{load_project}
\alias{load_project}
\title{load all data for a project from the project files.}
\usage{
load_project(project_id_path, data_folder = here::here())
}
\arguments{
\item{project_id_path}{character. the project id used for folder names, i.e. in the format YYYY-mm-prefix}

\item{data_folder}{character. path to data folder. starts at root of the project as defined by here::here. Defaults to here::here().}
}
\value{
a list
}
\description{
loads all data related to a project from its folder.
}
