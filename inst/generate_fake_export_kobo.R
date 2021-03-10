# generate fake data for applications
library(charlatan)
library(tidyverse)

set.seed(123)
fraud <- fraudster()
int_prov <- InternetProvider
names(data_list[[1]]) %>% dput()
# column names
list_names <-
  c(
    "__version__",
    "_attachments",
    "_geolocation",
    "_id",
    "_notes",
    "_status",
    "_submission_time",
    "_submitted_by",
    "_tags",
    "_uuid",
    "_validation_status",
    "_xform_id_string",
    "consent_privacy_policy",
    "email_address",
    "end",
    "first_name",
    "formhub/uuid",
    "gender",
    "german_skills",
    "last_name",
    "meta/instanceID",
    "motivation_skills",
    "motivation_why",
    "project_id",
    "project_role/car_04_2021",
    "project_role/fbb_04_2021",
    "project_role/kob_04_2021",
    "project_role/oce_04_2021",
    "project_role/sog_04_2021",
    "project_role/sos_04_2021",
    "rating_techniques/audio_data_processing",
    "rating_techniques/classification",
    "rating_techniques/clustering",
    "rating_techniques/data_anonymization",
    "rating_techniques/data_cleaning",
    "rating_techniques/data_visualization",
    "rating_techniques/database_management",
    "rating_techniques/descriptive_statistics",
    "rating_techniques/image_data_processing",
    "rating_techniques/inferential_statistics",
    "rating_techniques/neural_networks_and_deep_learn",
    "rating_techniques/nlp",
    "rating_techniques/regressions_and_modelling",
    "rating_technologies_tools/git",
    "rating_technologies_tools/html_css",
    "rating_technologies_tools/javascript_frontend",
    "rating_technologies_tools/nodejs",
    "rating_technologies_tools/python",
    "rating_technologies_tools/rstats",
    "rating_technologies_tools/sql",
    "rating_topics/data_protection",
    "rating_topics/data_security",
    "rating_topics/indicator_development",
    "rating_topics/project_management",
    "rating_topics/research_design",
    "rating_topics/survey_design",
    "rating_topics/theories_of_change_development",
    "rating_topics/working_agile",
    "start"
  )

NROW = 60
li <- vector("list", length(list_names)) %>% setNames(list_names)
ll <- vector("list", NROW)

appl_ll <- map(ll, function(l) {
  l <- li
  return(l)
})

# _id
appl_ll <- appl_ll %>%
  map2(1:length(appl_ll), function(appl_l, i) {
    appl_l$`_id` <- i
    appl_l
  })

# PROJECT ID AND PROJECT ROLE
base_prov <- BaseProvider$new()
fake_ids <-
  function(x)
    base_prov$random_element(
      c(
        "SOG-04-2021 CAR-04-2021 SOS-04-2021",
        "SOG-04-2021",
        "CAR-04-2021",
        "CAR-04-2021 SOS-04-2021"
      )
    )

appl_ll <- appl_ll %>%
  map(function(l) {
    l$project_id <- fake_ids()
    l
  })

appl_ll <- appl_ll %>%
  map(function(appl_l) {
    # map over name and value of each element of the applicant list
    map2(appl_l, names(appl_l), function(value, name) {
      if (str_detect(name, "^project_role")) {
        # extract project id from list name
        proj_id <-
          str_to_upper(str_extract(name, "\\w{3}_\\d{2}_\\d{4}"))
        proj_id <- str_replace_all(proj_id, "_", "-")
        if (str_detect(appl_l$project_id, proj_id)) {
          return (sample(
            c("team_trainee", "team_member", "team_lead"),
            size = 1,
            prob = c(0.2, 0.65, 0.2)
          ))
        } else {
          return ("DNA")
        }
      }
      value
    })
  })

# RATINGS
fake_rating <-
  function(x)
    base_prov$random_element(c("advanced", "beginner", "user", "expert"))

appl_ll <- appl_ll %>%
  map(function(appl_l) {
    # map over name and value of each element of the applicant list
    map2(appl_l, names(appl_l), function(value, name) {
      if (str_detect(name, "^rating")) {
        return(fake_rating())
      }
      value
    })
  })

# PERSONAL DETAILS AND OPEN Qs
fake_gender <-
  function(x)
    base_prov$random_element(c("female", "male", "non_binary", "not_disclosed"))
text_prov <- charlatan::LoremProvider$new()
name_prov <- charlatan::PersonProvider$new()
dt_prov <- DateTimeProvider$new()
fake_german <-
  function(x)
    base_prov$random_element(c("A1", "A2", "B1", "B2", "C1", "C2", "native"))
appl_ll <- appl_ll %>%
  map(function(appl_l) {
    appl_l$start <-
      dt_prov$date_time_between(start_date = "2021-02-03", end_date = "2021-02-05")
    appl_l$end <-
      dt_prov$date_time_between(start_date = "2021-02-06", end_date = "2021-02-06")
    appl_l$`_submission_time` <-
      dt_prov$date_time_between(start_date = "2021-02-07", end_date = "2021-02-07")
    appl_l$gender <- fake_gender()
    appl_l$last_name <- name_prov$last_name()
    appl_l$first_name <- name_prov$first_name()
    appl_l$email_address <- int_prov$new()$free_email()
    appl_l$gender_self_identification <- NA
    appl_l$motivation_skills <- text_prov$text()
    appl_l$motivation_why <- text_prov$text()
    appl_l$german_skills <- fake_german()
    appl_l$consent_privacy_policy <- "Yes"
    appl_l
  })

# add some self-identified genders
appl_ll[[12]]$gender_self_identification <- "agender"
appl_ll[[12]]$gender <- "self_identification"

# make sure all categories exist
appl_ll[[13]]$gender <- "non_binary"
appl_ll[[14]]$gender <- "not_disclosed"

appl_ll %>%
  jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
  readr::write_lines("tests/testthat/test_data/kobo/kobo_export.json")
