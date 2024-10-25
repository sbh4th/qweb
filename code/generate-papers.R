library(here)
library(tidyverse)
library(bib2df)

## 1: Import the .bib file as a dataframe
# Specify .bib file path
bib_file <- here("papers", "website.bib") 
bib_data <- bib2df(bib_file)

## 2: Extract the BIBTEXKEY column
bib_keys <- bib_data$BIBTEXKEY

## 3: Define a helper function to generate the 'index.qmd'
# Step 3: Define a helper function to generate the 'index.qmd' content with only BIBTEXKEY
generate_qmd_content <- function(bib_key) {
  qmd_content <- paste0(
    "---\n",
    "format: html\n",
    "lightbox: true\n",
    "execute:\n",
    "  echo: false\n",
    "---\n\n",
    "```{r setup, include=FALSE}\n",
    "source(here::here(\"_common.R\"))\n",
    "pub <- get_pubs() %>% \n",
    "    filter(bibtexkey == '", bib_key, "')\n",
    "pub$summary <- FALSE\n",
    "```\n\n",
    "# `r pub$title` \n\n",
    "```{r}\n",
    "make_pub(pub)\n",
    "```\n\n",
    "**Abstract**:\n\n",
    "`r pub$abstract`\n\n"
  )
  
  return(qmd_content)
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
  
  # Create folder for each BIBTEXKEY if it doesn't already exist
  folder_name <- file.path(output_folder, key)
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
    cat("Created folder for BIBTEXKEY '", key, "'.\n")
  }
  
  # Step 5: Generate the 'index.qmd' content
  qmd_content <- generate_qmd_content(key)
  
  # Write the 'index.qmd' file to the folder
  qmd_file_path <- file.path(folder_name, "index.qmd")
  writeLines(qmd_content, con = qmd_file_path)
}

# Print a message confirming the completion of the process
cat("Folder creation and 'index.qmd' generation process complete.\n")
