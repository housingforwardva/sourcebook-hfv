# Run all write_manifest.R files in the shiny/ directory structure
# This version runs from the main directory, just like your working example

# Set the base directory
base_dir <- "shiny"

# Find all write_manifest.R files recursively
manifest_files <- list.files(
  path = base_dir,
  pattern = "write_manifest\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

# Print what files were found
cat("Found", length(manifest_files), "write_manifest.R files:\n")
cat(paste("-", manifest_files), sep = "\n")
cat("\n")

# Run each file from the current directory (don't change directories)
for (file_path in manifest_files) {
  cat("\nRunning:", file_path, "\n")
  
  # Try to run the script and catch any errors
  tryCatch({
    source(file_path)
    cat("  ✓ Success\n")
  }, error = function(e) {
    cat("  ✗ Error:", e$message, "\n")
  })
}

cat("\nAll scripts processed.\n")