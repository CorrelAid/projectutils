library(projectutils)
devtools::load_all()
PROJECT_ID <- "{{{project_id}}}"

PROJECT_ID <- "2021-02-HAC"
proj <- Project$new("2021-02", "HAC", "Hacklab Foundation")

# properties set through the constructor -----
# only uncomment if you want to change something
# you can also create the object again with correct parameters
# proj$name <- "an awesome new title" 
# proj$start_ym <- "2021-03"
# proj$project_id <- "2021-03-HAC"

# basic properties ---------------
proj$end_ym <- "1970-02"
proj$end_ym_predicted <- "1970-01"

# misc
proj$is_internal <- FALSE
proj$slack_channel <- "foochannel"
# urls
proj$url_gh_issue <- 10000
proj$url_git_repo <- "https://github.com/CorrelAid/foobar"
proj$url_pad <- "https://pad.correlaid.org/aaaaaaaaaaaaaaaaa"

# publish to website
proj$is_public <- FALSE
proj$slug <- "a-slug-for-the-website"



# RELATED TABLES ----------------
# set status
projectutils::status
proj$set_status(status = "Team selection / Onboarding  / Kickoff")
proj$status_id

# set local chapters
projectutils::local_chapters
proj$add_local_chapter(lc_name = "munich")
proj$add_local_chapter(lc_name = "paris")
proj$add_local_chapter("berlin")
proj$local_chapters

# add tags
available_tags <- projectutils::tags
available_tags
proj$add_tag("data", "administrative")
proj$add_tag("lc", "berlin")
proj$tags

# volunteers
# fill out once team selection has been done 
vol <- Volunteer$new(first_name = "Foo", last_name = "Bar", email = "test@gmail.com")

# OLD

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