# function to clean and standardize titles before joining:

normalize_title <- function(title) {
  title %>%
    str_to_lower() %>%
    # str_replace_all("[[:punct:]]", "") %>%  # remove punctuation
    str_replace_all("[–—−‐‑]", "-")     # normalize dash variants
    # str_squish()                            # collapse whitespace
}

# pull data from Google Scholar
gsp <- scholar::get_publications("Ipf8idcAAAAJ&hl") %>%
  mutate(title = ifelse(title == "Fixed effects and difference in differences", 
    "Fixed effects and difference-in-differences", title),
    mtitle = normalize_title(title),
    twords = str_count(mtitle, "\\w+"),
    stitle = word(string = mtitle, start = 1, end = 4, 
      sep = fixed(" ")),
    id_scholar = pubid)

# write to file
write_rds(gsp, here("data", "gspubs.rds"))
    