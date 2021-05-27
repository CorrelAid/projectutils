library(projectutils)
PROJECT_ID <- "{{{project_id}}}"

proj <- load_project(PROJECT_ID)

proj
proj$year <- 2020
proj$team_size <- 0

proj$title$de <- ""
proj$title$en <- ""

# organization
proj$organization$name <- "Example Org"
proj$organization$website <- "https://example.org/"
proj$organization$about$en$source <- "[Source](https://example.org)"
proj$organization$about$de$source <- "übersetzt von [hier](https://example.org)"

proj$start <- "2020-04"
proj$end <- "2020-10"
proj$repo$url <- "https://github.com/CorrelAid/example_project"  
proj$repo$public <- FALSE

proj$links$en <- list("a [markdown link](https://alink.org)", "[another markdown link](https://alink.org)") 
proj$links$de <- list("a [markdown link](https://alink.org)", "[another markdown link](https://alink.org)") 

# team    
proj$team <- list(
  list(first_name = "foo", last_name = "bar", twitter = "foobar", github = "foobar", linkedin = "https://www.linkedin.com/in/foobar", website = "https://mywebsite.org", xing = "completxingurl"),
  list(first_name = "foo2", last_name = "bar2", twitter = "foobar2", github = "foobar2", linkedin = "https://www.linkedin.com/in/foobar2", website = "https://mywebsite.org", xing = "completxingurl")
)

proj$published <- FALSE # set to true always before making a PR.

write_project(proj)