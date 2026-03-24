# refresh_meta.R — syncs Google Scholar data → _meta.csv
# Run whenever you add papers or want fresh citation counts, then:
#   quarto render papers/index.qmd
#
# For papers with a title mismatch between Zotero and Scholar
# (e.g. "SARS-CoV-2" vs "COVID-19"), add the Scholar cid manually:
#   MANUAL_IDS <- c(anato2023a = "abc123CID")
MANUAL_IDS <- c()

library(here); library(scholar); library(bib2df)
library(dplyr); library(readr); library(stringdist)

SCHOLAR_ID <- "Ipf8idcAAAAJ"

# Lowercase FIRST, then strip — reversing this order removes uppercase letters
# from Title Case BibTeX titles before they are lowercased (causing match failures).
# sub="" replaces any un-transliteratable characters with nothing rather than "?".
# Strip common letter/editorial prefixes that Scholar often omits from its title index.
STRIP_RE <- paste0("^(editorial|commentary|comment|letter|reply|response|",
                   "erratum|correction|authors?'?\\s+reply|re):?\\s+")
norm <- function(x) {
  x <- gsub(STRIP_RE, "", gsub("[{}]", "", x), ignore.case = TRUE, perl = TRUE)
  gsub("[^a-z0-9]", "", tolower(iconv(x, to = "ASCII//TRANSLIT", sub = "")))
}

match_scholar <- function(bib_nt, gsp_nt) {
  d <- stringdist(bib_nt, gsp_nt, method = "jw")
  i <- which.min(d)
  if (!is.na(d[i]) && d[i] <= 0.15) return(i)
  # Prefix fallback for truncated titles — check both directions:
  #   (a) Scholar title truncated: bib starts with Scholar title
  #   (b) Bib title is shorter:   Scholar title starts with bib title
  pfx <- which(nchar(gsp_nt) >= 40 &
               (startsWith(bib_nt, gsp_nt) | startsWith(gsp_nt, bib_nt)))
  if (length(pfx)) pfx[1] else NA_integer_
}

bib <- bib2df(here("papers", "website.bib"), separate_names = FALSE) |>
  rename_with(tolower) |>
  transmute(key = bibtexkey, ntitle = norm(gsub("[{}]", "", title)))

gsp <- get_publications(SCHOLAR_ID) |>
  mutate(ntitle = norm(title))

meta <- bib |>
  mutate(
    idx        = sapply(ntitle, match_scholar, gsp_nt = gsp$ntitle),
    cites      = gsp$cites[idx],
    # pubid  → used in citation_for_view URL on Scholar profile page
    # cid    → used in scholar?cites= URL to list citing articles
    id_scholar = as.character(gsp$pubid[idx]),
    cid        = as.character(gsp$cid[idx])
  ) |>
  select(key, cites, id_scholar, cid)

# Apply any manual overrides (MANUAL_IDS values are pubids)
for (k in names(MANUAL_IDS)) {
  pubid <- MANUAL_IDS[[k]]
  meta$id_scholar[meta$key == k] <- pubid
  row <- which(as.character(gsp$pubid) == pubid)
  if (length(row)) {
    meta$cites[meta$key == k] <- gsp$cites[row[1]]
    meta$cid[meta$key == k]   <- as.character(gsp$cid[row[1]])
  }
}

write_csv(meta, here("papers", "_meta.csv"), na = "")
message(sum(!is.na(meta$id_scholar)), "/", nrow(meta), " papers matched. _meta.csv updated.")

unmatched <- meta$key[is.na(meta$id_scholar)]
if (length(unmatched)) message("Unmatched: ", paste(unmatched, collapse = ", "))
