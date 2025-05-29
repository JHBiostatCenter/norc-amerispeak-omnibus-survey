# Function to render a single file
render_file <- function(file) {
  message(sprintf("\nRendering %s...", file))
  start_time <- Sys.time()
  quarto::quarto_render(file, output_format = "html")
  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)
  message(sprintf("✓ Completed %s in %s seconds", file, duration))
}

# List of Quarto notebooks to render
files <- c(
  "1_norc-data-tabulation.qmd",
  "2_norc-report-generation.qmd",
  "3_paper-artifact-creation.qmd",
  "4_statistical-tests.qmd",
  "5_table-generation.qmd"
)

# Render each file
message("Starting Quarto document compilation...")
for (file in files) {
  if (file.exists(file)) {
    render_file(file)
  } else {
    warning(sprintf("⚠️ File %s not found", file))
  }
}

message("\n✨ Compilation complete! All documents have been rendered to HTML.")
