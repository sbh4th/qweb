# pull data from Google Scholar
gsp <- scholar::get_publications("Ipf8idcAAAAJ&hl") %>%
  mutate(title = ifelse(title == "Methods in social epidemiology", 
    "Fixed effects and difference-in-differences", title),
    mtitle = str_to_lower(title),
    id_scholar = pubid)

# write to file
write_rds(gsp, here("data", "gspubs.rds"))
    