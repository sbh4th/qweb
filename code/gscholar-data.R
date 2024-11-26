# pull data from Google Scholar
gsp <- scholar::get_publications("Ipf8idcAAAAJ&hl") %>%
  mutate(title = ifelse(title == "Fixed effects and difference in differences", 
    "Fixed effects and difference-in-differences", title),
    mtitle = str_to_lower(title),
    twords = str_count(mtitle, "\\w+"),
    stitle = word(string = mtitle, start = 1, end = 4, 
      sep = fixed(" ")),
    id_scholar = pubid)

# write to file
write_rds(gsp, here("data", "gspubs.rds"))
    