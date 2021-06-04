# get list of tags from GitHub
tags_list <- gh::gh("/repos/CorrelAid/projects/labels", per_page = 100)

tags <- tags_list %>% 
  purrr::map(`[`, c("name", "description")) %>% 
  purrr::transpose() %>% 
  tibble::as_tibble() %>% 
  tidyr::separate(name, into = c("category", "name"), sep = ":", fill = "right")

lcs <- tags %>% 
  dplyr::filter(category == "lc") %>% 
  dplyr::arrange(name) %>% 
  dplyr::mutate(lc_id = dplyr::row_number(),
                lc_name_full = description %>% purrr::map_chr(function (x) x)) %>% 
  dplyr::select(lc_id, lc_name = name, lc_name_full)

lcs %>% 
  readr::write_csv("inst/local_chapters.csv")

local_chapters <- lcs
# make tags available as tibble in the package
usethis::use_data(local_chapters, internal = FALSE, overwrite = TRUE)
