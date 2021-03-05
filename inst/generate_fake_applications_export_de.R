# generate fake data for applications
library(charlatan)
library(tidyverse)

set.seed(123)
fraud <- fraudster()
int_prov <- InternetProvider

t <- readr::read_csv("tests/testthat/test_data/surveymonkey/Bewerbungsformular für Projektteams dt..csv")

# column names
cnames_row_1 <- c("respondent_id", "collector_id", "date_created", "date_modified", 
                  "ip_address", "email_address", "first_name", "last_name", "custom_1", 
                  "Auf welches Projekt möchtest Du Dich bewerben?", "Wie lautet Dein Vorname?", 
                  "Unter welcher E-Mail-Adresse können wir Dich erreichen?", "Was ist dein Geschlecht?Hinweis: Da wir bei CorrelAid nach dem Grundprinzip von Geschlechtergleichberechtigung arbeiten, ist diese Frage für uns besonders wichtig.", 
                  "X14", "Bitte bewerte Deine Erfahrung mit den folgenden Technologien und Tools:Anfänger:in = Das habe ich noch nie gemacht./Ich habe noch nie eine einzige Zeile Code geschrieben.Anwender:in = Ich habe dazu erste Erfahrung gesammelt./Ich habe eigenständig Code geschrieben.Fortgeschrittene:r = Ich habe einige Erfahrung gesammelt./Ich habe schon komplexe Skripte geschrieben.Expert:in = Ich kenne mich mich sehr gut aus./Ich schreibe eigene Funktionen und Packages.", 
                  "X16", "X17", "X18", "X19", "Bitte bewerte Deine Erfahrung mit den folgenden Techniken.", 
                  "X21", "X22", "X23", "X24", "X25", "X26", "X27", "X28", "X29", 
                  "X30", "X31", "X32", "Bitte bewerte Deine Erfahrung mit den folgenden Themen:", 
                  "X34", "X35", "X36", "X37", "X38", "X39", "X40", "Welche Rolle möchtest Du im Projekt einnehmen?", 
                  "Bitte beschreibe hier, welche Deiner Fähigkeiten Dich besonders für die Teilnahme an diesem Projekt qualifizieren (max. 5 Sätze).", 
                  "Bitte beschreibe hier, warum Du Dich für dieses Projekt engagieren möchtest (max. 5 Sätze).", 
                  "Einwilligung in DatenschutzerklärungIch habe den Disclaimer gelesen und erteile hiermit meine Zustimmung zur Erhebung, Verarbeitung und Nutzung meiner personenbezogener Daten (Geschlecht und persönliche Fähigkeiten) zu internen und externen Reporting- und Präsentationszwecken. Daneben erteile ich ausdrücklich die Erlaubnis meine Kontaktdaten (Name und E-Mail) zur Kontaktaufnahme zu nutzen und diese bei erfolgreicher Bewerbung mit dem Projektteam zu teilen."
)

cnames_row_2 <- c("", "", "", "", "" , "", "", "", "", "Response", 
                  "Open-Ended Response", "Open-Ended Response", "Response", "Mein Geschlecht ist:", 
                  "R", "Python", "SQL", "Git", "Javascript", "Datenbereinigung", 
                  "Datenanonymisierung", "Datenbankmanagement", "Deskriptive Statistik", 
                  "Datenvisualisierung", "Inferenzstatistik", "Regressionen und Modellierung", 
                  "Klassifizierung", "Clustering", "Natural Language Processing", 
                  "Neuronale Netze/Deep Learning", "Verarbeitung von Bilddateien", 
                  "Verarbeitungen von Audiodateien", "Entwicklung von Wirkungsketten", 
                  "Entwicklung von Indikatoren", "Research Design", "Survey Design", 
                  "Datenschutz", "Datensicherheit", "Agiles Arbeiten", "Projektmanagement", 
                  "Response", "Open-Ended Response", "Open-Ended Response", "Response"
)

NROW = 60
appl_export <- map(1:length(cnames_row_1), function(x) rep(NA, NROW)) %>% 
  setNames(cnames_row_1) %>% 
  as_tibble()

# project id
appl_export$`Auf welches Projekt möchtest Du Dich bewerben?` <- rep(c("COR-11-2020: Developing a Project Database + Frontend for CorrelAid", "CIT-10-2020: Citizen Science Project"), each = NROW / 2)

n <- nrow(appl_export)
appl_export$respondent_id <- fraud$integer(n)
dt_prov <- DateTimeProvider$new()
appl_export$`Wie lautet Dein Vorname?` <- fraud$name(n)

# gender 
base_prov <- BaseProvider$new()

fake_gender <- function(x) base_prov$random_element(c("Weiblich", "Männlich", "Non-binary", "Das möchte ich nicht angeben"))
appl_export$`Was ist dein Geschlecht?Hinweis: Da wir bei CorrelAid nach dem Grundprinzip von Geschlechtergleichberechtigung arbeiten, ist diese Frage für uns besonders wichtig.` <- purrr::map_chr(1:n, fake_gender)
# add some more genders
appl_export$`Was ist dein Geschlecht?Hinweis: Da wir bei CorrelAid nach dem Grundprinzip von Geschlechtergleichberechtigung arbeiten, ist diese Frage für uns besonders wichtig.`[16] <- NA
appl_export$X14[16] <- "Genderqueer"
appl_export$`Was ist dein Geschlecht?Hinweis: Da wir bei CorrelAid nach dem Grundprinzip von Geschlechtergleichberechtigung arbeiten, ist diese Frage für uns besonders wichtig.`[13] <- "Non-binary"
appl_export$`Was ist dein Geschlecht?Hinweis: Da wir bei CorrelAid nach dem Grundprinzip von Geschlechtergleichberechtigung arbeiten, ist diese Frage für uns besonders wichtig.`[14] <- "I do not want to disclose my gender"

# fake emails 
appl_export$`Unter welcher E-Mail-Adresse können wir Dich erreichen?` <-  purrr::map_chr(1:n, ~int_prov$new()$free_email())

# fake motivation statements 
text_prov <- charlatan::LoremProvider$new()
appl_export$`Bitte beschreibe hier, warum Du Dich für dieses Projekt engagieren möchtest (max. 5 Sätze).` <- purrr::map_chr(1:n, ~text_prov$text())
appl_export$`Bitte beschreibe hier, welche Deiner Fähigkeiten Dich besonders für die Teilnahme an diesem Projekt qualifizieren (max. 5 Sätze).` <- purrr::map_chr(1:n, ~text_prov$text())

# fake ips
appl_export$ip_address <-  purrr::map_chr(1:n, ~int_prov$new()$ipv4())

# rating questions
base_prov <- BaseProvider$new()
fake_rating <- function(x) base_prov$random_element(c("Anfänger:in", "Anwender:in", "Fortgeschrittene:r", "Expert:in"))
rating_cols <- stringr::str_subset(colnames(appl_export), "rate|^X\\d{1,2}")
rating_cols <- rating_cols[rating_cols != "X14"] # gender column

for (col in rating_cols) {
  appl_export[, col] <- purrr::map_chr(1:n, fake_rating)
}

# fake role
fake_role <- function(x) base_prov$random_element(c("Teamtrainee", "Teammitglied", "Teamleiter:in"))
appl_export$`Welche Rolle möchtest Du im Projekt einnehmen?` <- purrr::map_chr(1:n, fake_role)


# inject second row of column names
appl_export <- rbind(cnames_row_2, appl_export)
appl_export %>% readr::write_csv("tests/testthat/test_data/surveymonkey/applications_fake_export_de.csv")

