# papers/_render.R
#
# Reads website.bib + _meta.csv at render time and outputs year-grouped HTML.
# No pre-computation step. Source this from papers/index.qmd.
#
# To add a paper:
#   1. Add entry to website.bib (Zotero export)
#   2. Add a row to _meta.csv (key, cites, id_scholar) — cites/id_scholar optional
#   3. quarto render papers/index.qmd
#
# load_papers() is kept for backward compatibility with individual paper pages.
load_papers <- function() yaml::read_yaml(here::here("papers", "_papers.yml"))

suppressPackageStartupMessages({
  library(here)
  library(bib2df)
  library(dplyr)
  library(stringr)
  library(readr)
  library(htmltools)
  library(markdown)
})

# ---------------------------------------------------------------------------
# Author name decorations: bold self, asterisk trainees
# Keys are regex patterns with word boundaries to avoid partial matches
# ---------------------------------------------------------------------------

NAME_REPLACEMENTS <- c(
  "\\bHarper S\\b"       = "**Harper S**",
  "\\bSocha PM\\b"       = "Socha PM\\*",
  "\\bSocha P\\b"        = "Socha P\\*",
  "\\bHetherington E\\b" = "Hetherington E\\*",
  "\\bAl-Soneidar WA\\b" = "Al-Soneidar WA\\*",
  "\\bAl-Soneidar W\\b"  = "Al-Soneidar W\\*",
  "\\bRiddell CA\\b"     = "Riddell CA\\*",
  "\\bMajid F\\b"        = "Majid F\\*",
  "\\bHajizadeh M\\b"    = "Hajizadeh M\\*",
  "\\bVoigt K\\b"        = "Voigt K\\*",
  "\\bCarroll S\\b"      = "Carroll S\\*",
  "\\bYuan W\\b"         = "Yuan W\\*",
  "\\bSternbach TS\\b"   = "Sternbach TS\\*",
  "\\bFarhat I\\b"       = "Farhat I\\*",
  "\\bVargas Lopez F\\b" = "Vargas Lopez F\\*",
  "\\bCapurro DA\\b"     = "Capurro DA\\*",
  "\\bAustin NA\\b"      = "Austin NA\\*",
  "\\bArsenault C\\b"    = "Arsenault C\\*",
  "\\bRichardson RA\\b"  = "Richardson RA\\*",
  "\\bManivong P\\b"     = "Manivong P\\*",
  "\\bMcKinnon BA\\b"    = "McKinnon BA\\*",
  "\\bGray AP\\b"        = "Gray AP\\*",
  "\\bMah SM\\b"         = "Mah SM\\*"
)

# ---------------------------------------------------------------------------
# Author formatting
# ---------------------------------------------------------------------------

# "LastName, First Middle" -> "LastName FMInitials"
# Strips BibTeX curly braces (e.g. "{Al-Soneidar}, Walid A." -> "Al-Soneidar WA")
fmt_author <- function(name) {
  name  <- gsub("[{}]", "", name)
  parts <- strsplit(name, ", ")[[1]]
  if (length(parts) < 2) return(trimws(name))
  last     <- trimws(parts[1])
  initials <- paste0(substr(strsplit(trimws(parts[2]), " ")[[1]], 1, 1), collapse = "")
  paste0(last, " ", initials)
}

fmt_author_list <- function(author_vec) {
  if (is.null(author_vec) || length(author_vec) == 0) return("")
  raw <- paste(sapply(author_vec, fmt_author), collapse = ", ")
  str_replace_all(raw, NAME_REPLACEMENTS)
}

# ---------------------------------------------------------------------------
# Citation builder
# ---------------------------------------------------------------------------

# Null/NA-safe field formatter: returns sprintf(fmt, x) or "" if x is blank
f <- function(x, fmt) {
  x <- as.character(x)
  if (length(x) == 1 && !is.na(x) && nchar(trimws(x)) > 0) sprintf(fmt, trimws(x)) else ""
}

build_citation <- function(p) {
  if (!is.na(p$booktitle) && nchar(trimws(p$booktitle)) > 0) {
    # Book chapter
    paste0(
      p$authors,                           ". ",
      f(p$title,      '"%s." '),
      "In: ",
      f(p$editor_fmt, "%s (Eds.). "),
      f(p$booktitle,  "_%s_. "),
      f(p$publisher,  "%s; "),
      p$year,                              ". ",
      f(p$pages,      "pp. %s.")
    )
  } else {
    # Journal article
    paste0(
      p$authors,                         ". ",
      f(p$title,   '"%s." '),
      f(p$journal, "_%s_. "),
      p$year, ";",
      f(p$volume,  "%s"),
      f(p$number,  "(%s)"),
      f(p$pages,   ":%s")
    )
  }
}

# ---------------------------------------------------------------------------
# HTML for a single publication entry
# ---------------------------------------------------------------------------

render_pub <- function(p) {
  # Render markdown in citation (bold, italics, asterisks) to HTML
  cite_html <- renderMarkdown(text = p$citation)

  # --- Buttons ---
  btns <- tagList()

  # View (DOI link)
  if (!is.na(p$url))
    btns <- tagAppendChild(btns,
      tags$a(href = p$url, target = "_blank", class = "pub-btn btn-view", "View"))

  # Google Scholar
  if (!is.na(p$url_scholar))
    btns <- tagAppendChild(btns,
      tags$a(href = p$url_scholar, target = "_blank", class = "pub-btn btn-scholar", "Scholar"))

  # Citations badge
  if (!is.na(p$cites) && as.integer(p$cites) > 0)
    btns <- tagAppendChild(btns,
      tags$span(class = "pub-btn btn-cites", paste0("Cited: ", p$cites)))

  # Abstract toggle (Bootstrap collapse, available in Quarto/litera theme)
  abs_block <- NULL
  if (!is.na(p$abstract) && nchar(trimws(p$abstract)) > 0) {
    abs_id <- paste0("abs-", p$key)
    btns <- tagAppendChild(btns,
      tags$button(
        class = "pub-btn btn-abstract",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", abs_id),
        "Abstract"
      ))
    abs_block <- tags$div(
      id = abs_id, class = "collapse pub-abstract-body",
      tags$p(p$abstract)
    )
  }

  # Altmetric donut badge
  altmetric <- NULL
  if (!is.na(p$doi))
    altmetric <- tags$div(
      class = "altmetric-embed pub-altmetric",
      `data-badge-type` = "donut",
      `data-doi` = p$doi,
      `data-hide-no-mentions` = "true"
    )

  # Assemble
  tags$div(class = "pub-entry",
    HTML(cite_html),
    tags$div(class = "pub-buttons", btns),
    abs_block,
    altmetric
  )
}

# ---------------------------------------------------------------------------
# Year-grouped publication list
# ---------------------------------------------------------------------------

render_pub_list <- function(category = "ARTICLE") {

  # --- Load bib ---
  pubs <- bib2df(here("papers", "website.bib")) |>
    rename_with(tolower) |>
    filter(toupper(category) == toupper(.env$category)) |>
    mutate(
      title     = gsub("[{}]", "", title),
      booktitle = gsub("[{}]", "", if ("booktitle" %in% names(pick(everything()))) booktitle else NA_character_),
      # Zotero exports BibLaTeX which uses `journaltitle`; fall back to `journal`
      journal   = dplyr::coalesce(
                    if ("journaltitle" %in% names(pick(everything()))) journaltitle else NA_character_,
                    if ("journal"      %in% names(pick(everything()))) journal      else NA_character_
                  ),
      year      = str_extract(bibtexkey, "[0-9]{4}"),
      key       = bibtexkey
    ) |>
    arrange(desc(year))

  # Format authors and editors
  pubs$authors    <- sapply(pubs$author, fmt_author_list)
  pubs$editor_fmt <- sapply(pubs$editor, function(e) {
    if (is.null(e) || length(e) == 0 || all(is.na(e))) return(NA_character_)
    paste(sapply(e, fmt_author), collapse = ", ")
  })

  # --- Load metadata ---
  meta_path <- here("papers", "_meta.csv")
  if (file.exists(meta_path)) {
    meta <- read_csv(meta_path, show_col_types = FALSE)
    pubs <- left_join(pubs, meta, by = "key")
  } else {
    pubs$cites      <- NA_integer_
    pubs$id_scholar <- NA_character_
  }

  # Derived URLs
  pubs$url <- ifelse(
    !is.na(pubs$doi) & pubs$doi != "",
    paste0("https://doi.org/", pubs$doi),
    NA_character_
  )
  pubs$url_scholar <- ifelse(
    !is.na(pubs$id_scholar),
    paste0("https://scholar.google.com/citations?view_op=view_citation",
           "&hl=en&user=Ipf8idcAAAAJ&citation_for_view=Ipf8idcAAAAJ:",
           pubs$id_scholar),
    NA_character_
  )

  # Build citation strings
  pubs$citation <- sapply(seq_len(nrow(pubs)), function(i) build_citation(as.list(pubs[i, ])))

  # --- Render year-grouped HTML ---
  years <- unique(pubs$year)

  rows <- lapply(years, function(yr) {
    grp <- pubs[pubs$year == yr, ]
    entries <- lapply(seq_len(nrow(grp)), function(i) render_pub(as.list(grp[i, ])))
    tagList(
      tags$div(class = "pub-year-label", yr),
      tags$div(class = "pub-year-entries", tagList(entries))
    )
  })

  HTML(as.character(tags$div(class = "pub-list", tagList(rows))))
}
