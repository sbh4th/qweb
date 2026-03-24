# papers/update.R
#
# Run this script manually (not at render time) to regenerate _papers.yml.
# It does all the heavy work: BibTeX parsing, Google Scholar citation join,
# author formatting, and citation string building.
#
# Usage:
#   Rscript papers/update.R          # from project root in terminal
#   source("papers/update.R")        # from R console at project root
#
# Output: papers/_papers.yml  (commit this file; it is read at render time)

library(here)
library(bib2df)
library(dplyr)
library(stringr)
library(readr)
library(yaml)
library(glue)

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

BIB_FILE <- here("papers", "website.bib")
GSP_FILE <- here("data", "gspubs.rds")
OUT_FILE <- here("papers", "_papers.yml")

# Author name replacements: bold self, asterisk trainees.
# Keys are regex patterns; \\b ensures we don't match partial initials
# (e.g., "Socha P" must not match inside "Socha PM").
NAME_REPLACEMENTS <- c(
  "\\bHarper S\\b"       = "**Harper S**",
  "\\bSocha PM\\b"       = "Socha PM*",
  "\\bSocha P\\b"        = "Socha P*",
  "\\bHetherington E\\b" = "Hetherington E*",
  "\\bAl-Soneidar WA\\b" = "Al-Soneidar WA*",
  "\\bAl-Soneidar W\\b"  = "Al-Soneidar W*",
  "\\bRiddell CA\\b"     = "Riddell CA*",
  "\\bMajid F\\b"        = "Majid F*",
  "\\bHajizadeh M\\b"    = "Hajizadeh M*",
  "\\bVoigt K\\b"        = "Voigt K*",
  "\\bCarroll S\\b"      = "Carroll S*",
  "\\bYuan W\\b"         = "Yuan W*",
  "\\bSternbach TS\\b"   = "Sternbach TS*",
  "\\bFarhat I\\b"       = "Farhat I*",
  "\\bVargas Lopez F\\b" = "Vargas Lopez F*",
  "\\bCapurro DA\\b"     = "Capurro DA*",
  "\\bAustin NA\\b"      = "Austin NA*",
  "\\bArsenault C\\b"    = "Arsenault C*",
  "\\bRichardson RA\\b"  = "Richardson RA*",
  "\\bManivong P\\b"     = "Manivong P*",
  "\\bMcKinnon BA\\b"    = "McKinnon BA*",
  "\\bGray AP\\b"        = "Gray AP*",
  "\\bMah SM\\b"         = "Mah SM*"
)

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

normalize_title <- function(title) {
  title |>
    str_to_lower() |>
    str_replace_all("[–—−‐‑]", "-")
}

# Convert "LastName, FirstName Middle" -> "LastName FMInitials"
# Handles curly-brace-wrapped last names like "{Al-Soneidar}, Walid A."
format_author_vancouver <- function(author_name) {
  # Strip curly braces (used in BibTeX for special last names)
  author_name <- gsub("[{}]", "", author_name)
  parts <- strsplit(author_name, ", ")[[1]]
  if (length(parts) < 2) return(trimws(author_name))
  last     <- trimws(parts[1])
  first    <- trimws(parts[2])
  initials <- paste0(substr(unlist(strsplit(first, " ")), 1, 1), collapse = "")
  paste0(last, " ", initials)
}

# Format a list of author strings to a single Vancouver-style string
format_author_list <- function(authors) {
  if (is.null(authors) || length(authors) == 0) return(NA_character_)
  formatted <- sapply(authors, format_author_vancouver)
  str_replace_all(paste(formatted, collapse = ", "), NAME_REPLACEMENTS)
}

build_citation <- function(pub) {
  cites_str <- if (!is.na(pub$cites) && pub$cites != "") glue("Citations: {pub$cites}") else ""

  if (!is.na(pub$booktitle) && nchar(trimws(pub$booktitle)) > 0) {
    # Book chapter
    editor    <- if (!is.na(pub$editor_fmt)  && nchar(trimws(pub$editor_fmt)) > 0)  glue("{pub$editor_fmt} (Eds.).") else ""
    book      <- if (!is.na(pub$booktitle)   && nchar(trimws(pub$booktitle)) > 0)   glue("_{pub$booktitle}_.") else ""
    edition   <- if (!is.na(pub$edition)     && nchar(trimws(pub$edition)) > 0)     glue("{pub$edition}nd ed.") else ""
    publisher <- if (!is.na(pub$publisher)   && nchar(trimws(pub$publisher)) > 0)   glue("{pub$publisher}.") else ""
    pages     <- if (!is.na(pub$pages)       && nchar(trimws(pub$pages)) > 0)       glue("pp. {pub$pages}.") else ""
    paste(pub$author_fmt, glue("({pub$year})."),
          glue('"{pub$title}"'), "In:", editor, book,
          edition, publisher, pages, cites_str) |> str_squish()
  } else {
    # Journal article
    journal  <- if (!is.na(pub$journal) && nchar(trimws(pub$journal)) > 0) glue("_{pub$journal}_.") else ""
    volume   <- if (!is.na(pub$volume)  && nchar(trimws(pub$volume))  > 0) glue("{pub$volume}:") else ""
    pages    <- if (!is.na(pub$pages)   && nchar(trimws(pub$pages))   > 0) glue("{pub$pages}.") else ""
    doi_link <- if (!is.na(pub$doi)     && nchar(trimws(pub$doi))     > 0) glue("DOI: [{pub$doi}](https://doi.org/{pub$doi})") else ""
    paste(glue("{pub$author_fmt}."),
          glue('"{pub$title}"'),
          journal, glue("{pub$year};"), volume, pages, doi_link, cites_str) |> str_squish()
  }
}

# ---------------------------------------------------------------------------
# Main pipeline
# ---------------------------------------------------------------------------

message("Reading ", BIB_FILE)
pubs_raw <- bib2df(BIB_FILE) |> rename_with(tolower)

# Format list columns (author, editor) into plain strings
pubs_raw$author_fmt <- sapply(pubs_raw$author, format_author_list)
pubs_raw$editor_fmt <- sapply(pubs_raw$editor, function(e) {
  if (is.null(e) || length(e) == 0 || all(is.na(e))) return(NA_character_)
  formatted <- sapply(e, format_author_vancouver)
  paste(formatted, collapse = ", ")
})

pubs <- pubs_raw |>
  mutate(
    title     = gsub("[{}]", "", title),
    booktitle = gsub("[{}]", "", if ("booktitle" %in% names(pubs_raw)) booktitle else NA_character_),
    mtitle    = normalize_title(title),
    # bib2df doesn't parse unquoted year integers; extract from bibtexkey
    year      = str_extract(bibtexkey, "[0-9]{4}")
  ) |>
  arrange(desc(year))

# Join Google Scholar metrics (matched on normalized title)
if (file.exists(GSP_FILE)) {
  message("Joining Google Scholar metrics from ", GSP_FILE)
  gsp  <- read_rds(GSP_FILE) |> select(mtitle, cites, id_scholar)
  pubs <- pubs |> left_join(gsp, by = "mtitle")
} else {
  message("No gspubs.rds found — skipping Scholar metrics")
  pubs$cites      <- NA_integer_
  pubs$id_scholar <- NA_character_
}

# Scholar URLs
pubs$url_scholar <- ifelse(
  is.na(pubs$id_scholar),
  NA_character_,
  glue("https://scholar.google.com/citations?view_op=view_citation&hl=en&user=Ipf8idcAAAAJ&citation_for_view=Ipf8idcAAAAJ:{pubs$id_scholar}")
)

# DOI-based view URL
pubs$url <- ifelse(
  !is.na(pubs$doi) & pubs$doi != "",
  paste0("https://doi.org/", pubs$doi),
  NA_character_
)

# Which keys have a summary page directory on disk
summary_dirs    <- list.dirs(here("papers"), recursive = FALSE, full.names = FALSE)
pubs$has_summary <- pubs$bibtexkey %in% summary_dirs

# Build pre-formatted citation strings
pubs$citation <- sapply(seq_len(nrow(pubs)), function(i) {
  build_citation(as.list(pubs[i, ]))
})

# ---------------------------------------------------------------------------
# Write output YAML
# ---------------------------------------------------------------------------

message("Writing ", OUT_FILE)

out <- pubs |>
  select(
    key         = bibtexkey,
    category,
    title,
    citation,
    doi,
    url,
    url_scholar,
    cites,
    abstract,
    has_summary,
    year
  ) |>
  mutate(
    across(where(is.character), ~ ifelse(is.na(.) | . == "NA", NA_character_, .)),
    cites       = suppressWarnings(as.integer(cites)),
    has_summary = as.logical(has_summary)
  )

out_list <- lapply(seq_len(nrow(out)), function(i) {
  row <- as.list(out[i, ])
  # Drop NAs to keep YAML clean
  row[!sapply(row, function(v) length(v) == 1 && is.na(v))]
})

write_yaml(out_list, OUT_FILE)
message("Done — ", nrow(out), " publications written to ", OUT_FILE)
