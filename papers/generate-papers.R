library(here)
library(tidyverse)
library(bib2df)

papers <- bib2df(here("papers", "website.bib"))

## 1: Import the .bib file as a dataframe
# Specify .bib file path
bib_file <- here("papers", "website.bib") 
bib_data <- bib2df(bib_file)

## 2: Extract the BIBTEXKEY column
bib_keys <- bib_data$BIBTEXKEY

## 3: Helper function to write the 'index.qmd' file for each publication
write_qmd_file <- function(folder_name, title, authors, year, keywords, doi) {
  qmd_content <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "author: ", authors, "\n",
    "date: ", year, "\n",
    "categories: [", keywords, "]\n",
    if (!is.na(doi)) paste0("doi: \"", doi, "\"\n") else "",
    "---\n"
  )
  
  # Write the content to an 'index.qmd' file within the folder
  writeLines(qmd_content, con = file.path(folder_name, "index.qmd"))
}

# Step 4: Create a folder for each publication based on BIBTEXKEY and generate 'index.qmd'
output_folder <- here("papers") 

# Check if the base output folder exists, if not, create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Loop through each publication in the dataframe
for (i in 1:nrow(bib_data)) {
  key <- bib_data$BIBTEXKEY[i]
  title <- bib_data$TITLE[i]
  authors <- paste(bib_data$AUTHOR[[i]], collapse = ", ")
  year <- bib_data$YEAR[i]
  keywords <- paste(bib_data$KEYWORDS[[i]], collapse = ", ")
  doi <- if ("DOI" %in% names(bib_data)) bib_data$DOI[i] else NA
  
  # Create folder for each BIBTEXKEY if it doesn't already exist
  folder_name <- file.path(output_folder, key)
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
    cat("Created folder for BIBTEXKEY '", key, "'.\n")
  } else {
    cat("Folder for BIBTEXKEY '", key, "' already exists. Skipping creation.\n")
  }
  
  # Step 5: Create the 'index.qmd' file in the respective folder
  write_qmd_file(folder_name, title, authors, year, keywords, doi)
  cat("Created 'index.qmd' file for BIBTEXKEY '", key, "'.\n")
}

# Print a message confirming the completion of the process
cat("Folder creation and 'index.qmd' generation process complete.\n")


# Step 3: Create a folder for each publication based on BIBTEXKEY
output_folder <- here("papers") 

# Check if the base output folder exists, if not, create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Create subfolders for each BIBTEXKEY, checking if they already exist
for (key in bib_keys) {
  folder_name <- file.path(output_folder, key)
  
  if (dir.exists(folder_name)) {
    cat("Folder for BIBTEXKEY '", key, 
        "' already exists. Skipping creation.\n")
  } else {
    dir.create(folder_name)
    cat("Created folder for BIBTEXKEY '", key, "'.\n")
  }
}

# Print a message confirming the completion of the process
cat("Folder creation/check process complete.\n")
