# RDS to Word Tables Script
# Run this script to convert RDS files to Word documents with formatted tables

# Load required libraries
library(flextable)
library(officer)

# Function to convert HTML tags to flextable formatting
format_html_text <- function(ft, table_data, col_name) {
  if (any(grepl("<[^>]+>", table_data[[col_name]], ignore.case = TRUE))) {
    # First, clean the HTML tags and store the text
    clean_text <- sapply(table_data[[col_name]], function(x) {
      gsub("<[^>]+>", "", x)
    })

    # Create a new column with the clean text
    ft <- ft |>
      compose(j = col_name, value = as_paragraph(as_chunk(clean_text)))

    # Find rows with bold text and apply bold formatting
    bold_rows <- which(grepl("<(?:b|strong)>", table_data[[col_name]], ignore.case = TRUE))
    if (length(bold_rows) > 0) {
      ft <- ft |>
        bold(i = bold_rows, j = col_name, bold = TRUE)
    }

    # Find rows with italic text and apply italic formatting
    italic_rows <- which(grepl("<(?:em|i)>", table_data[[col_name]], ignore.case = TRUE))
    if (length(italic_rows) > 0) {
      ft <- ft |>
        italic(i = italic_rows, j = col_name, italic = TRUE)
    }
  }
  return(ft)
}

# Create output directory if it doesn't exist
output_dir <- "output/docs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created directory:", output_dir, "\n")
}

# Get list of RDS files
rds_files <- list.files("output", pattern = "\\.rds$", full.names = TRUE)

if (length(rds_files) == 0) {
  cat("No RDS files found in 'output' directory.\n")
  quit()
}

cat("Found", length(rds_files), "RDS files to process...\n")

# Process each RDS file
for (file in rds_files) {
  # Extract table name from filename
  table_name <- tools::file_path_sans_ext(basename(file))

  cat("Processing:", table_name, "\n")

  # Create a new Word document for this table
  doc <- read_docx()

  # Add title
  doc <- doc |>
    body_add_par(table_name, style = "heading 1")

  # Read the RDS file
  table_data <- readRDS(file)

  # Process the table
  if (is.data.frame(table_data)) {
    # Convert data frame to character to handle mixed types
    table_data[] <- lapply(table_data, as.character)

    # Create flextable with proper text wrapping
    ft <- flextable(table_data) |>
      theme_vanilla() |>
      bold(part = "header") |>
      autofit() |> # Automatically fit columns to content
      width(width = 1.2) |> # Increase column width to 120% of available space
      valign(valign = "top") |>
      padding(padding = 3) |>
      fix_border_issues() |>
      fontsize(size = 10, part = "all") |> # Set a reasonable font size
      line_spacing(space = 1.2, part = "all") |> # Add some line spacing for readability
      set_table_properties(layout = "autofit", width = 1) |> # Ensure table fits page width
      hrule(rule = "exact") # Exact height rules for better wrapping

    # Apply HTML formatting for each column
    for (col_name in names(table_data)) {
      ft <- format_html_text(ft, table_data, col_name)
    }

    # Add table to document
    doc <- doc |>
      body_add_flextable(ft)
  } else {
    # Handle non-data.frame objects
    doc <- doc |>
      body_add_par("Raw output:") |>
      body_add_par(paste(capture.output(print(table_data)), collapse = "\n"))
  }

  # Save the document to output/docs/ with the same name as the RDS file
  output_filename <- file.path(output_dir, paste0(table_name, ".docx"))
  print(doc, target = output_filename)

  cat("Word document saved as:", output_filename, "\n")
}

cat("Done!\n")
