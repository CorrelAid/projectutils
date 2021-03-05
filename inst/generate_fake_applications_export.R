# generate fake data for applications
library(charlatan)
library(tidyverse)
library(dplyr)

set.seed(123)
fraud <- fraudster()
int_prov <- InternetProvider

# column names
cnames_row_1 <- c("Respondent ID", "Collector ID", "Start Date", "End Date", 
                  "IP Address", "Email Address", "First Name", "Last Name", "Custom Data 1", 
                  "Which project would you like to apply for?", "What is your first name?", 
                  "Which email address can we use to contact you?", "What is your gender?Note: we work according to the principle of gender equality. This is why this question is particularly important to us.", 
                  "X14", "Please rate your experience with the following technologies and tools:Beginner = I have never done this before / I have never written a single line of code.User = I have gained some first experience in this field / I have written code on my own.Advanced = I have gained some experience / I have written complex scriptsExpert =  I know my way around very well / I write my own functions and packages.", 
                  "X16", "X17", "X18", "X19", "X20", "X21", "Please rate your experience with the following techniques:", 
                  "X23", "X24", "X25", "X26", "X27", "X28", "X29", "X30", "X31", 
                  "X32", "X33", "X34", "Please rate your experience with the following topics:", 
                  "X36", "X37", "X38", "X39", "X40", "X41", "X42", "What role do you want to assume in the project? Team Lead: Leads the implementation of the project as part and primus inter pares of the project team. Has some additional coordination responsibilities.Team member: Volunteer of the CorrelAid network who is involved in the implementation of the projectTeam trainee: a team member who is a beginner (there is typically one \"team trainee\" position per team)", 
                  "Please describe here what skills you would bring to the project (max. 5 sentences).", 
                  "Please describe here why you want to get involved in this project (max. 5 sentences)", 
                  "Consent to privacy policyI have read the disclaimer and hereby give my consent to the collection, processing and use of my personal data (gender and personal skills) for the team selection process and for internal and external reporting as well as presentation purposes. In addition, I expressly grant permission to use my contact details (name and e-mail) to contact me and to share them with the project team if the application is successful."
)
cnames_row_2 <- c("", "", "", "", "", "", "", "", "", "Response", 
                  "Open-Ended Response", "Open-Ended Response", "Response", "My gender is (please specify):", 
                  "R", "Python", "SQL", "Git", "Javascript (Frontend)", "HTML/CSS", 
                  "Node.js (Javascript Backend)", "Data cleaning", "Data anonymization", 
                  "Database management", "Descriptive statistics", "Data visualization", 
                  "Inferential statistics", "Regressions and modelling", "Classification", 
                  "Clustering", "Natural Language Processing", "Neural networks / Deep learning", 
                  "Processing of image data", "Processing of audio data", "Development of Theories of Change", 
                  "Development of indicators", "Research Design", "Survey Design", 
                  "Data protection", "Data security", "Working agile", "Project management", 
                  "Response", "Open-Ended Response", "Open-Ended Response", "Response"
)

NROW = 60
appl_export <- map(1:length(cnames_row_1), function(x) rep(NA, NROW)) %>% 
  setNames(cnames_row_1) %>% 
  as.tibble()

# project id
appl_export$`Which project would you like to apply for?` <- rep(c("COR-11-2020: Developing a Project Database + Frontend for CorrelAid", "CIT-10-2020: Citizen Science Project"), each = NROW / 2)

n <- nrow(appl_export)
appl_export$`Respondent ID` <- fraud$integer(n)
dt_prov <- DateTimeProvider$new()
appl_export$`What is your first name?` <- fraud$name(n)

# gender 
base_prov <- BaseProvider$new()

fake_gender <- function(x) base_prov$random_element(c("Female", "Male", "Non-binary", "I do not want to disclose my gender"))
appl_export$`What is your gender?Note: we work according to the principle of gender equality. This is why this question is particularly important to us.` <- purrr::map_chr(1:n, fake_gender)
# add some more genders
appl_export$`What is your gender?Note: we work according to the principle of gender equality. This is why this question is particularly important to us.`[16] <- NA
appl_export$X14[16] <- "Genderqueer"
appl_export$`What is your gender?Note: we work according to the principle of gender equality. This is why this question is particularly important to us.`[13] <- "Non-binary"
appl_export$`What is your gender?Note: we work according to the principle of gender equality. This is why this question is particularly important to us.`[14] <- "I do not want to disclose my gender"

# fake emails 
appl_export$`Which email address can we use to contact you?` <-  purrr::map_chr(1:n, ~int_prov$new()$free_email())

# fake motivation statements 
text_prov <- charlatan::LoremProvider$new()
appl_export$`Please describe here why you want to get involved in this project (max. 5 sentences)` <- purrr::map_chr(1:n, ~text_prov$text())
appl_export$`Please describe here what skills you would bring to the project (max. 5 sentences).` <- purrr::map_chr(1:n, ~text_prov$text())

# fake ips
appl_export$`IP Address` <-  purrr::map_chr(1:n, ~int_prov$new()$ipv4())

# rating questions
base_prov <- BaseProvider$new()
fake_rating <- function(x) base_prov$random_element(c("Advanced", "Beginner", "User", "Expert"))
rating_cols <- stringr::str_subset(colnames(appl_export), "rate|^X\\d{1,2}")
rating_cols <- rating_cols[rating_cols != "X14"] # gender column

for (col in rating_cols) {
  appl_export[, col] <- purrr::map_chr(1:n, fake_rating)
}

# fake role
fake_role <- function(x) base_prov$random_element(c("Team trainee", "Team member", "Team lead"))
appl_export$`What role do you want to assume in the project? Team Lead: Leads the implementation of the project as part and primus inter pares of the project team. Has some additional coordination responsibilities.Team member: Volunteer of the CorrelAid network who is involved in the implementation of the projectTeam trainee: a team member who is a beginner (there is typically one "team trainee" position per team)` <- purrr::map_chr(1:n, fake_role)


# inject second row of column names
appl_export <- rbind(cnames_row_2, appl_export)
appl_export %>% readr::write_csv("tests/testthat/test_data/surveymonkey/applications_fake_export.csv")

