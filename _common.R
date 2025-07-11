library(here)
library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(fontawesome)
library(bib2df)
library(rcrossref)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

# Vancouver style author formatting
format_author_vancouver <- function(author_name) {
  # Split the author name by comma
  name_parts <- strsplit(author_name, ", ")[[1]]
  
  # Extract last name and first name(s)
  last_name <- name_parts[1]
  first_names <- name_parts[2]
  
  # Extract the first letter of each first name (and middle name if available)
  initials <- paste0(substr(unlist(strsplit(first_names, " ")), 1, 1), collapse = "")
  
  # Combine last name and initials (with no space between initials)
  return(paste0(last_name, " ", initials))
}

# Function to reformat a list of authors
reformat_authors_vancouver <- function(authors) {
  # Apply the format function to each author in the vector
  formatted_authors <- sapply(authors, format_author_vancouver)
  
  # Collapse the authors into a single string separated by commas
  return(paste(formatted_authors, collapse = ", "))
}


library(bib2df)
library(dplyr)
library(stringr)
library(rcrossref)
library(here)
library(readr)

# Helper: fallback URL resolver
get_crossref_url <- function(doi) {
  if (is.na(doi) || doi == "") return(NA)

  tryCatch({
    res <- cr_works(doi)
    if (!is.null(res$data$link) && "URL" %in% names(res$data$link[[1]])) {
      return(res$data$link[[1]]$URL[1])
    }
    paste0("https://doi.org/", doi)
  }, error = function(e) {
    NA
  })
}

# Define name replacements
name_replacements <- c(
      'Harper S' = '**Harper S**',
      'Socha PM' = 'Socha PM*',
      'Socha P' = 'Socha P*',
      'Hetherington E' = 'Hetherington E*',
      'Al-Soneidar WA' = 'Al-Soneidar WA*',
      'Al-Soneidar W' = 'Al-Soneidar W*',
      'Riddell CA' = 'Riddell CA*',
      'Majid F' = 'Majid F*',
      'Hajizadeh M' = 'Hajizadeh M*',
      'Voigt K' = 'Voigt K*',
      'Carroll S' = 'Carroll S*', 
      'Yuan W' = 'Yuan W*',
      'Sternbach TS' = 'Sternbach TS*',
      'Farhat I' = 'Farhat I*',
      'Vargas Lopez F' = 'Vargas Lopez F*',
      'Capurro DA' = 'Capurro DA*',
      'Austin NA' = 'Austin NA*',
      'Arsenault C' = 'Arsenault C*',
      'Richardson RA' = 'Richardson RA*',
      'Manivong P' = 'Manivong P*',
      'McKinnon BA' = 'McKinnon BA*',
      'Gray AP' = 'Gray AP*',
      'Mah SM' = 'Mah SM*')
      # Add more names as needed

# function to clean and standardize titles before joining:

normalize_title <- function(title) {
  title %>%
    str_to_lower() %>%
    #str_replace_all("[[:punct:]]", "") %>%  # remove punctuation
    str_replace_all("[–—−‐‑]", "-")     # normalize dash variants
    #str_squish()                            # collapse whitespace
}

# Main function
get_pubs <- function() {
  # Load bib and standardize fields
  pubs <- here("papers", "website.bib") %>%
    bib2df() %>%
    rename_with(tolower) %>%
    arrange(desc(date)) %>%
    mutate(
      title     = gsub("[{}]", "", title),
      booktitle = gsub("[{}]", "", booktitle),
      mtitle    = normalize_title(title),
      stitle    = word(mtitle, 1, 4, sep = fixed(" ")),
      year      = str_sub(date, 1, 4)
    )

  # Resolve missing URLs
  missing <- pubs %>%
    filter((is.na(url) | url == "") & !is.na(doi) & doi != "")

  if (nrow(missing) > 0) {
    message(glue::glue("Looking up {nrow(missing)} missing URLs from CrossRef..."))
    resolved <- sapply(missing$doi, get_crossref_url)
    pubs <- pubs %>%
      left_join(tibble(doi = missing$doi, url_resolved = resolved), by = "doi") %>%
      mutate(
        url = ifelse(is.na(url) | url == "", url_resolved, url),
        url = gsub("\\.pdf|\\?(.*)", "", url)
      ) %>%
      select(-url_resolved)
  }

  # Merge in Google Scholar metrics if available
  gsp_path <- here("data", "gspubs.rds")
  if (file.exists(gsp_path)) {
    gsp <- read_rds(gsp_path) %>%
      select(mtitle, cites, id_scholar)
    pubs <- pubs %>% 
      left_join(gsp, by = "mtitle") %>%
      select(-mtitle, -stitle)
    pubs$author <- sapply(pubs$author, 
      reformat_authors_vancouver)
    pubs$author <- str_replace_all(pubs$author, 
      name_replacements)
    pubs$editor <- sapply(pubs$editor,
      reformat_authors_vancouver)
    pubs <- make_citations(pubs)
    pubs$summary <- ifelse(is.na(pubs$bibtexkey), FALSE, TRUE)
    # pubs$stub <- make_stubs(pubs)
    pubs$url_summary <- file.path(pubs$bibtexkey, "index.html")
    pubs$url_scholar <- ifelse(
      is.na(pubs$id_scholar), NA, 
      glue::glue('https://scholar.google.com/citations?view_op=view_citation&hl=en&user#=Ipf8idcAAAAJ&citation_for_view=Ipf8idcAAAAJJ:{pubs$id_scholar}')
    )
  }

  return(pubs)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  return(pubs)
}

make_citation <- function(pub) {
  # Check if it's a book chapter (presence of booktitle indicates it's a book chapter)
  if (!is.na(pub$booktitle)) {
    # For book chapters
    if (!is.na(pub$editor)) {
      pub$editor <- glue::glue('{pub$editor} (Eds.).')
    }
    if (!is.na(pub$booktitle)) {
      pub$book <- glue::glue('_{pub$booktitle}_.')
    }
    if (!is.na(pub$edition)) {
      pub$edition <- glue::glue('{pub$edition}nd ed.')
    }
    if (!is.na(pub$publisher)) {
      pub$publisher <- glue::glue('{pub$publisher}.')
    }
    if (!is.na(pub$pages)) {
      pub$pages <- glue::glue('pp. {pub$pages}.')
    }
    
    pub$year <- glue::glue("({pub$year}).")
    pub$title <- glue::glue('"{pub$title}"')
    
    pub[,which(is.na(pub))] <- ''
    
    # Format for book chapters
    return(paste(
      pub$author, pub$year, pub$title, "In:", pub$editor, pub$book,
      pub$edition, pub$publisher, pub$pages,
      "Citations:", pub$cites
    ))
    
  } else {
    # For regular journal articles
    if (!is.na(pub$journaltitle)) {
      pub$journal <- glue::glue('_{pub$journaltitle}_.')
    }
    if (!is.na(pub$volume)) {
      pub$volume <- glue::glue('{pub$volume}:')
    }
    if (!is.na(pub$pages)) {
      pub$pages <- glue::glue('{pub$pages}.')
    }
    if (!is.na(pub$doi)) {
      pub$doi <- make_doi(pub$doi)
    }
    pub$author <- glue::glue("{pub$author}. ")
    pub$year <- glue::glue("{pub$year};")
    pub$title <- glue::glue('"{pub$title}"')
    pub[,which(is.na(pub))] <- ''
    
    # Format the citation for journal articles
    return(paste(
      pub$author, pub$title, pub$journal, 
      pub$year, pub$volume, pub$pages, pub$doi, 
      "Citations:", pub$cites
    ))
  }
}

make_doi <- function(doi) {
  return(glue::glue('DOI: [{doi}](https://doi.org/{doi})'))
}

make_stubs <- function(pubs) {
    journal <- str_to_lower(pubs$journal)
    journal <- str_replace_all(journal, ':', '')
    journal <- str_replace_all(journal, '`', '')
    journal <- str_replace_all(journal, "'", '')
    journal <- str_replace_all(journal, "\\.", '')
    journal <- str_replace_all(journal, "&", '')
    journal <- str_replace_all(journal, ',', '')
    journal <- str_replace_all(journal, '  ', '-')
    journal <- str_replace_all(journal, ' ', '-')
    return(paste0(pubs$date, '-', journal))
}

make_pub_list <- function(pubs, category) {
    x <- pubs[which(pubs$category == category),]
    pub_list <- list()
    for (i in 1:nrow(x)) {
      pub_list[[i]] <- make_pub(x[i,], index = i)
    }
    return(htmltools::HTML(paste(unlist(pub_list), collapse = "")))
}

make_pub <- function(pub, index = NULL) {
  header <- FALSE
  altmetric <- make_altmetric(pub)
  if (is.null(index)) {
    cite <- pub$citation
    icons <- make_icons(pub)
  } else {
    cite <- glue::glue('{index}) {pub$citation}')
    icons <- glue::glue('<ul style="list-style: none;"><li>{make_icons(pub)}</li></ul>')
    if (index == 1) { header <- TRUE }
  }
  # return(markdown_to_html(cite))
  return(htmltools::HTML(glue::glue(
    '<div class="pub">
    <div class="grid">
    <div class="g-col-11"> {markdown_to_html(cite)} </div>
    <div class="g-col-1"> {altmetric} </div>
    </div>
    {icons}'
  )))
}

make_altmetric <- function(pub) {
  altmetric <- ""
  #if (pub$category == 'ARTICLE') {
  if (isTRUE(pub$category == "ARTICLE")) {
    altmetric <- glue::glue('<div data-badge-type="donut" data-doi="{pub$doi}" data-hide-no-mentions="true" class="altmetric-embed"></div>')
  }
  return(altmetric)
}

aside <- function(text) {
  return(htmltools::tag("aside", list(text)))
}

center <- function(text) {
  return(htmltools::tag("center", list(text)))
}

aside_center <- function(text) {
  return(aside(center(list(text))))
}

aside_center_b <- function(text) {
  return(aside(center(list(htmltools::tag("b", text)))))
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  
  # Replace the author names with underlined last names
  text <- gsub(
    pattern = "\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\1</u>, \\2", 
    text
  )
  text <- gsub(
    pattern = "\\\\\\*\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\\\*\\1</u>, \\2", 
    text
  )
  
  # Render the text as HTML
  return(htmltools::HTML(markdown::renderMarkdown(text = text)))
}

make_icons <- function(pub) {
  html <- c()
  #if (pub$summary) {
  if (isTRUE(pub$summary)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "Summary",
      url  = pub$url_summary, 
      class = "icon-link-summary", 
      target = "_self"
    )))      
  }
  #if (!is.na(pub$url)) {
  if (!is.null(pub$url) && length(pub$url) == 1 && !is.na(pub$url)) {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt",
      text = "View",
      url  = pub$url
    )))
  }
#  if (!is.na(pub$url_pdf)) {
#    html <- c(html, as.character(icon_link(
#      icon = "fa fa-file-pdf",
#      text = "PDF",
#      url  = pub$url_pdf
#    )))
#  }
#  if (!is.na(pub$url_repo)) {
#    html <- c(html, as.character(icon_link(
#      icon = "fab fa-github",
#      text = "Code & Data",
#      url  = pub$url_repo
#    )))
#  }
#  if (!is.na(pub$url_other)) {
#    html <- c(html, as.character(icon_link(
#      icon = "fas fa-external-link-alt",
#      text = pub$other_label,
#      url  = pub$url_other
#    )))
#  }
#  if (!is.na(pub$url_rg)) {
#    html <- c(html, as.character(icon_link(
#      icon = "ai ai-researchgate",
#      # text = "&nbsp;",
#      text = "RG",
#      url  = pub$url_rg
#    )))
#  }
  # if (!is.na(pub$url_scholar)) {
  if (!is.null(pub$url_scholar) && 
      length(pub$url_scholar) == 1 && 
      !is.na(pub$url_scholar)) {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-google-scholar",
      # text = "&nbsp;",
      text = "Scholar",
      url  = pub$url_scholar
    )))
  }
  return(paste(html, collapse = ""))
}

# The icon_link() function is in {distilltools}, but I've modified this
# one to include  a custom class to be able to have more control over the
# CSS and an optional target argument

icon_link <- function(
  icon = NULL,
  text = NULL,
  url = NULL,
  class = "icon-link",
  target = "_blank"
) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(htmltools::a(
    href = url, text, class = class, target = target, rel = "noopener"
  ))
}

make_icon_text <- function(icon, text) {
  return(htmltools::HTML(paste0(make_icon(icon), " ", text)))
}

make_icon <- function(icon) {
  return(htmltools::tag("i", list(class = icon)))
}

last_updated <- function() {
  return(span(
    paste0(
      'Last updated on ',
      format(Sys.Date(), format="%B %d, %Y")
    ),
    style = "font-size:0.8rem;")
  )
}

