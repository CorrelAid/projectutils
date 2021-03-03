library(httr)
library(tidyverse)


# https://kf.kobotoolbox.org/api/v2/ : we can use this api for downloading data, uploading forms etc. 
kobo_get <- function(url, token = Sys.getenv("KOBO_TOKEN")) {
  req <- GET(url, add_headers(Authorization=glue::glue("Token {token}")))
  stop_for_status(req)
  content(req)
}


kobo_download_file <- function(url, outfile, token = Sys.getenv("KOBO_TOKEN")) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, authorization = glue::glue("Token {token}"))
  curl::curl_download(url, outfile, handle = h)
  
}


# https://kc.kobotoolbox.org/api/v1/ : only use the old API for uploading submissions 
# TODO: implement POST for submissions 
# TODO: password and user should not be stored in file -> transfer to .Renviron 
kobo_get_v1 <- function(url, user = "", password = "") {
  req <- GET(url,  authenticate(user, password))
  stop_for_status(req)
  content(req)
}


### use the functions 
# get all assets 
assets <- kobo_get("https://kobo.correlaid.org/api/v2/assets.json")

# extract the test survey asset
s <- assets$results %>% 
  keep(function(x) x$asset_type == "survey" & stringr::str_detect(x$name, "Project"))
test_survey <- s[[1]]

# we can extract the data in two ways: ourselves as json or download the xml export 
# TODO: xls should also work but somehow doesn't right now?

# JSON
data_url <- test_survey$data # url for data as json 
data_list <- kobo_get(data_url)$results 



# Data cleaning attempts
# this does not work yet - it gives 4 rows for 2 people... it is tricky with the questions that are matrix questions
df <- data_list %>%
  purrr::map_dfr(function(sub) purrr::compact(sub) %>%  as.tibble())

# only variables related to project and gender
df_proj <- df %>% 
  select(`_id`, starts_with("gender"), first_name, last_name, email_address, project_id, starts_with("project")) %>% 
  distinct()
# this makes one row into n when people have applied to more than one project
df_proj %>% 
  separate_rows(project_id, sep = " ")


# XML EXPORT
download_xml <- test_survey$downloads %>% keep(function(x) x$format == "xml")
download_url <- download_xml[[1]]$url

kobo_download_file(data_url, "xml_export.xml")

# Explore the v1 api 
kobo_get_v1("https://kobo.correlaid.org/api/v1/submissions") # 405 -> does not exist 
# only post exists: see https://kc.kobotoolbox.org/api/v1/submissions 
