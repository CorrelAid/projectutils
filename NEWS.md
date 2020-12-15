# projectutils 0.1.1
* several bug fixes and small improvments
* no usage of `here::here` within functions, only as default arguments
* new `usethis`-like function `use_fill_project` that creates a script that contains all the necessary workflow steps to fill a `meta.json` of a project.
* new function `load_applications` to download project applications from surveymonkey for all projects for a specific project 
* new function `anonymize_applications` to anonymize applications by dropping name, email and ip address
* new function `extract_motivation_questions` to extract the open-ended questions into a markdown string that can be then stored as a markdown file for easier reading
* new function `get_application_emails` to quickly extract the email addresses of those applicants who were (not) selected for the team. Writes a ; separated string to the clipboard for easy copy-pasting to email clients.
* new `usethis`-like function `use_team_selection_workflow` that creates a folder `team_selection` and two scripts that contains all the necessary workflow steps when preparing team selection and afterwards getting the email addresses of accepted / declined applicants.
* new utility functions `id_path` and `id_surveymonkey` to quickly convert project ids from the PREFIX-mm-yyyy format to yyyy-mm-PREFIX and vice versa. 