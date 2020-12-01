data_raw <- jsonlite::read_json("https://connect.medrxiv.org/relate/collection_json.php?grp=181", simplifyVector = TRUE)

data_tibble <- data_raw$rels %>%
  dplyr::tibble() %>%
  dplyr::select(title = rel_title,
                abstract = rel_abs,
                links = rel_link)

results_by_abstract <- data_tibble %>% 
  dplyr::filter(stringr::str_detect(.$abstract, "(covid)|(coronavirus)|(SARS-CoV-2)") == "TRUE" & 
                  stringr::str_detect(.$abstract, "(travel restriction)|(travel ban)|(traveller)|(airline)|(border)|(flight)") == "TRUE")

results_by_title <- data_tibble %>% 
  dplyr::filter(stringr::str_detect(.$title, "(covid)|(coronavirus)|(SARS-CoV-2)") == "TRUE" & 
                  stringr::str_detect(.$title, "(travel restriction)|(travel ban)|(traveller)|(airline)|(border)|(flight)") == "TRUE")


results <- dplyr::full_join(results_by_abstract, results_by_title)

urls <- results$links

for(i in 81:87)
{
  browseURL(as.character(urls[i]))  
}
