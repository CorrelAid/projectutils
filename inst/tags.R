# get list of tags from GitHub
# we should clean up there 
# alternatively single source of truth in the package and push the labels 
# to github from time to time
tags_list <- gh::gh("/repos/CorrelAid/projects/labels", per_page = 100)

tags <- tags_list %>% 
  purrr::map_chr("name") %>% 
  tibble::as_tibble()

tags <- tags %>% 
  tidyr::separate(value, into = c("category", "value"), sep = ":", fill = "right") %>% 
  dplyr::mutate(tag_id = dplyr::row_number())

# for manual lookup
tags %>% 
  readr::write_csv("inst/tags.csv")

# add / delete tags

# make tags available as tibble in the package
usethis::use_data(tags, internal = FALSE, overwrite = TRUE)
