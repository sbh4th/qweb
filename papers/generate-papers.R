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

## 3: Define a helper function to generate the 'index.qmd'
generate_qmd_content <- function(title, authors, year, keywords, doi) {
  # Ensure the date is in YYYY-MM-DD format
  formatted_date <- paste0(year, "-01-01")
  
  qmd_content <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "author: ", authors, "\n",
    "date: ", formatted_date, "\n",
    "categories: [", keywords, "]\n",
    if (!is.na(doi)) paste0("doi: \"", doi, "\"\n") else "",
    "---\n"
  )
  return(qmd_content)
}

# Step 4: Define a function to check for changes and update the 'index.qmd' file if needed
write_or_update_qmd_file <- function(folder_name, qmd_content) {
  qmd_file_path <- file.path(folder_name, "index.qmd")
  
  # Check if 'index.qmd' file already exists
  if (file.exists(qmd_file_path)) {
    # Read the existing file content
    existing_content <- readLines(qmd_file_path)
    existing_content <- paste(existing_content, collapse = "\n")
    
    # Compare existing content with new content
    if (existing_content == qmd_content) {
      cat("No changes detected for 'index.qmd' in folder '", 
          folder_name, "'.\n")
      return(FALSE)  # No update needed
    } else {
      cat("Changes detected in 'index.qmd' for folder '", 
          folder_name, "'. Updating file.\n")
    }
  } else {
    cat("Creating new 'index.qmd' in folder '", folder_name, "'.\n")
  }
  
  # Write the new content to 'index.qmd'
  writeLines(qmd_content, con = qmd_file_path)
  return(TRUE)  # Update performed
}

# Step 5: Create a folder for each publication based on BIBTEXKEY and generate/update 'index.qmd'
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
  }
  
  # Step 6: Generate the 'index.qmd' content with YYYY-MM-DD date format
  qmd_content <- generate_qmd_content(title, authors, year, keywords, doi)
  
  # Step 7: Write or update 'index.qmd' file if there are changes
  write_or_update_qmd_file(folder_name, qmd_content)
}

# Print a message confirming the completion of the process
cat("Folder creation and 'index.qmd' generation/update process complete.\n")
