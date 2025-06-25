#!/usr/bin/env Rscript

# AgentsMCP Package Build and Check Script
# Run this script to build, check, and prepare the package for publication

cat("=====================================\n")
cat("AgentsMCP Package Build Script\n")
cat("=====================================\n\n")

# Load required packages
required_packages <- c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Set working directory to package root
if (basename(getwd()) != "AgentsMCP") {
  if (dir.exists("AgentsMCP")) {
    setwd("AgentsMCP")
  } else {
    stop("Please run this script from the AgentsMCP directory or its parent directory")
  }
}

cat("Working directory:", getwd(), "\n\n")

# 1. Generate documentation
cat("1. Generating package documentation...\n")
tryCatch({
  roxygen2::roxygenise()
  cat("✓ Documentation generated successfully\n\n")
}, error = function(e) {
  cat("✗ Documentation generation failed:", e$message, "\n\n")
})

# 2. Run tests
cat("2. Running package tests...\n")
tryCatch({
  test_results <- testthat::test_local()
  if (any(test_results$failed > 0)) {
    cat("✗ Some tests failed\n")
  } else {
    cat("✓ All tests passed\n")
  }
  cat("\n")
}, error = function(e) {
  cat("✗ Test execution failed:", e$message, "\n\n")
})

# 3. Check package
cat("3. Running R CMD check...\n")
tryCatch({
  check_results <- devtools::check(
    document = FALSE,  # Already documented above
    args = c("--no-manual", "--as-cran"),
    quiet = FALSE
  )
  
  if (length(check_results$errors) > 0) {
    cat("✗ Package check failed with errors:\n")
    cat(paste(check_results$errors, collapse = "\n"), "\n")
  } else if (length(check_results$warnings) > 0) {
    cat("⚠ Package check completed with warnings:\n")
    cat(paste(check_results$warnings, collapse = "\n"), "\n")
  } else {
    cat("✓ Package check completed successfully\n")
  }
  cat("\n")
}, error = function(e) {
  cat("✗ Package check failed:", e$message, "\n\n")
})

# 4. Build vignettes
cat("4. Building vignettes...\n")
tryCatch({
  devtools::build_vignettes()
  cat("✓ Vignettes built successfully\n\n")
}, error = function(e) {
  cat("✗ Vignette build failed:", e$message, "\n\n")
})

# 5. Build package
cat("5. Building package...\n")
tryCatch({
  pkg_file <- devtools::build(
    path = "..",
    binary = FALSE,
    quiet = FALSE
  )
  cat("✓ Package built successfully:", basename(pkg_file), "\n\n")
}, error = function(e) {
  cat("✗ Package build failed:", e$message, "\n\n")
})

# 6. Install package locally
cat("6. Installing package locally...\n")
tryCatch({
  devtools::install(upgrade = "never", quiet = FALSE)
  cat("✓ Package installed successfully\n\n")
}, error = function(e) {
  cat("✗ Package installation failed:", e$message, "\n\n")
})

# 7. Quick functionality test
cat("7. Testing basic functionality...\n")
tryCatch({
  library(AgentsMCP)
  
  # Test agent creation
  agent <- create_annem_agent("test", "neural_momentum", 1000000)
  cat("✓ Agent creation works\n")
  
  # Test configuration
  config <- get_annem_config()
  cat("✓ Configuration retrieval works\n")
  
  # Test utility functions
  gini <- calculate_gini_coefficient(c(1000, 2000, 3000, 4000, 5000))
  cat("✓ Utility functions work\n")
  
  cat("✓ Basic functionality test passed\n\n")
}, error = function(e) {
  cat("✗ Functionality test failed:", e$message, "\n\n")
})

# 8. Package summary
cat("8. Package Summary\n")
cat("==================\n")

# Get package info
desc <- read.dcf("DESCRIPTION")
cat("Package:", desc[,"Package"], "\n")
cat("Version:", desc[,"Version"], "\n")
cat("Title:", desc[,"Title"], "\n")
cat("Author:", desc[,"Authors@R"], "\n")

# Count files
r_files <- length(list.files("R", pattern = "\\.R$"))
test_files <- length(list.files("tests/testthat", pattern = "\\.R$", recursive = TRUE))
vignette_files <- length(list.files("vignettes", pattern = "\\.(Rmd|md)$"))

cat("\nPackage contents:\n")
cat("- R files:", r_files, "\n")
cat("- Test files:", test_files, "\n") 
cat("- Vignettes:", vignette_files, "\n")

cat("\nNext steps:\n")
cat("1. Review any warnings or errors above\n")
cat("2. Push to GitHub: git add . && git commit -m 'Package v", desc[,"Version"], "' && git push\n")
cat("3. Create GitHub release with built package\n")
cat("4. Submit to CRAN (optional)\n\n")

cat("=====================================\n")
cat("AgentsMCP Package Build Complete!\n")
cat("=====================================\n")