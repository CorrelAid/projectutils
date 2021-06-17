# generate fake data for applications
library(charlatan)
library(dplyr)
set.seed(123)
fraud <- fraudster()
int_prov <- InternetProvider


survey_id <- 287163371
# appl_en <- survey_id %>% 
#  surveymonkey::fetch_survey_obj() %>%
#  surveymonkey::parse_survey() 

# only c("COR-11-2020", "CIT-10-2020")
appl_en <- appl_en %>% 
  filter(`Which project would you like to apply for?` %in% c("COR-11-2020: Developing a Project Database + Frontend for CorrelAid", "CIT-10-2020: Citizen Science Project")) 


n <- nrow(appl_en)
appl_en$survey_id <- 2123912391
appl_en$respondent_id <- fraud$integer(n)
dt_prov <- DateTimeProvider$new()
appl_en$date_created <- purrr::map_chr(1:n, ~dt_prov$date_time() %>% as.character())
appl_en$date_modified <- purrr::map_chr(1:n, ~dt_prov$date_time() %>% as.character())
appl_en$`What is your first name?` <- fraud$name(n)

# gender 
base_prov <- BaseProvider$new()

fake_gender <- function(x) base_prov$random_element(c("Female", "Male", "Non-binary", "I do not want to disclose my gender"))
appl_en$`What is your gender?<br><em>Note: we work according to the principle of gender equality. This is why this question is particularly important to us.</em> - My gender is (please specify):` <- purrr::map_chr(1:n, fake_role)
# add some more genders
appl_en$`What is your gender?<br><em>Note: we work according to the principle of gender equality. This is why this question is particularly important to us.</em> - My gender is (please specify):`[12] <- "agender"
appl_en$`What is your gender?<br><em>Note: we work according to the principle of gender equality. This is why this question is particularly important to us.</em>`[12] <- NA
appl_en$`What is your gender?<br><em>Note: we work according to the principle of gender equality. This is why this question is particularly important to us.</em>`[13] <- "Non-binary"
appl_en$`What is your gender?<br><em>Note: we work according to the principle of gender equality. This is why this question is particularly important to us.</em>`[14] <- "I do not want to disclose my gender"

# fake emails 
appl_en$`Which email address can we use to contact you?` <-  purrr::map_chr(1:n, ~int_prov$new()$free_email())

# fake motivation statements 
text_prov <- charlatan::LoremProvider$new()
appl_en$`Please describe here why you want to get involved in this project (max. 5 sentences)` <- purrr::map_chr(1:n, ~text_prov$text())
appl_en$`Please describe here what skills you would bring to the project (max. 5 sentences).` <- purrr::map_chr(1:n, ~text_prov$text())

# fake ips
appl_en$ip_address <-  purrr::map_chr(1:n, ~int_prov$new()$ipv4())

# rating questions
base_prov <- BaseProvider$new()
fake_rating <- function(x) base_prov$random_element(c("Advanced", "Beginner", "User", "Expert"))
rating_cols <- stringr::str_subset(colnames(appl_en), "rate")

for (col in rating_cols) {
  appl_en[, col] <- purrr::map_chr(1:n, fake_rating)
}

# fake role
fake_role <- function(x) base_prov$random_element(c("Team trainee", "Team member", "Team lead"))
appl_en$`What role do you want to assume in the project?` <- purrr::map_chr(1:n, fake_role)

appl_en %>% readr::write_csv("tests/testthat/test_data/surveymonkey/applications_fake_en.csv")
