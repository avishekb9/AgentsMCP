# AgentsMCP Installation Guide

## Prerequisites

### R Environment
- **R Version**: 4.0.0 or higher
- **RStudio**: Recommended for development
- **Memory**: Minimum 8GB RAM for full simulations
- **Internet**: Required for market data download

### System Dependencies (Linux/Ubuntu)
```bash
# Install system libraries
sudo apt-get update
sudo apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libgit2-dev \
  libharfbuzz-dev \
  libfribidi-dev
```

## Installation Methods

### Method 1: From GitHub (Recommended)

```r
# Install devtools if not already installed
if (!require(devtools)) {
  install.packages("devtools", dependencies = TRUE)
}

# Install AgentsMCP from GitHub
devtools::install_github("avishekb9/AgentsMCP", 
                         dependencies = TRUE,
                         build_vignettes = TRUE)
```

### Method 2: Local Installation

If you have the source code locally:

```r
# Navigate to parent directory of AgentsMCP
setwd("/path/to/parent/directory")

# Install from local source
devtools::install("AgentsMCP", 
                  dependencies = TRUE,
                  build_vignettes = TRUE)
```

### Method 3: From Built Package

```r
# If you have the .tar.gz file
install.packages("AgentsMCP_1.0.0.tar.gz", 
                repos = NULL, 
                type = "source")
```

## Dependency Installation

### Core Dependencies
```r
# Install required packages
core_packages <- c(
  "quantmod",           # Financial data
  "igraph",             # Network analysis  
  "nnet",               # Neural networks
  "forecast",           # Time series
  "rugarch",            # GARCH models
  "PerformanceAnalytics", # Performance metrics
  "xts",                # Time series objects
  "dplyr",              # Data manipulation
  "ggplot2",            # Visualization
  "corrplot"            # Correlation plots
)

install.packages(core_packages, dependencies = TRUE)
```

### Visualization Dependencies
```r
# Install visualization packages
viz_packages <- c(
  "plotly",             # Interactive plots
  "networkD3",          # Interactive networks
  "gridExtra",          # Plot arrangements
  "viridis",            # Color schemes
  "tidyr",              # Data tidying
  "scales"              # Scale functions
)

install.packages(viz_packages, dependencies = TRUE)
```

### Development Dependencies (Optional)
```r
# For package development and testing
dev_packages <- c(
  "testthat",           # Testing framework
  "knitr",              # Documentation
  "rmarkdown",          # R Markdown
  "roxygen2",           # Documentation generation
  "pkgdown"             # Package websites
)

install.packages(dev_packages, dependencies = TRUE)
```

## Verification

### Basic Installation Check
```r
# Load the package
library(AgentsMCP)

# Check package information
packageVersion("AgentsMCP")
help(package = "AgentsMCP")

# Test basic functionality
config <- get_annem_config()
print(config$package_version)
```

### Quick Functionality Test
```r
# Set seed for reproducibility
set_annem_seed(42)

# Create a simple agent
agent <- create_annem_agent("test_001", "neural_momentum", 1000000)
cat("Agent created:", agent$id, "of type", agent$type, "\n")

# Test utility functions
wealth_vector <- c(800000, 1000000, 1200000, 1500000, 2000000)
gini <- calculate_gini_coefficient(wealth_vector)
cat("Gini coefficient calculation:", round(gini, 4), "\n")

# Test data generation
synthetic_data <- generate_synthetic_data(n_days = 50, n_assets = 2)
cat("Synthetic data generated:", nrow(synthetic_data$prices), "days\n")
```

### Full System Test
```r
# Run a small ANNEM analysis (may take 2-3 minutes)
tryCatch({
  results <- run_annem_analysis(
    symbols = c("AAPL"),
    n_agents = 50,
    n_steps = 25,
    save_results = FALSE,
    verbose = TRUE
  )
  
  cat("✓ Full system test completed successfully\n")
  annem_summary(results)
  
}, error = function(e) {
  cat("✗ System test failed:", e$message, "\n")
  cat("This may be due to internet connectivity or missing dependencies\n")
})
```

## Troubleshooting

### Common Issues

#### 1. Internet Connectivity
If market data download fails:
```r
# Use synthetic data instead
synthetic_results <- run_annem_analysis(
  symbols = c("SYNTHETIC_ASSET"),
  n_agents = 100,
  n_steps = 50
)
```

#### 2. Memory Issues
For systems with limited RAM:
```r
# Reduce simulation parameters
small_results <- run_annem_analysis(
  symbols = c("AAPL"),
  n_agents = 100,    # Instead of 1000
  n_steps = 50,      # Instead of 250
  save_results = TRUE
)
```

#### 3. Package Dependencies
If specific packages fail to install:
```r
# Install one by one with error checking
problem_packages <- c("rugarch", "PerformanceAnalytics")

for (pkg in problem_packages) {
  tryCatch({
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    cat("✓", pkg, "installed successfully\n")
  }, error = function(e) {
    cat("✗", pkg, "installation failed:", e$message, "\n")
  })
}
```

#### 4. Compilation Issues (Windows)
Install Rtools for package compilation:
```r
# Check if Rtools is needed
if (.Platform$OS.type == "windows") {
  if (!require(devtools)) install.packages("devtools")
  if (!devtools::find_rtools()) {
    cat("Please install Rtools from: https://cran.r-project.org/bin/windows/Rtools/\n")
  }
}
```

### Performance Optimization

#### For Large Simulations
```r
# Increase memory limit (Windows)
if (.Platform$OS.type == "windows") {
  memory.limit(size = 16000)  # 16GB
}

# Use parallel processing for some operations
library(parallel)
num_cores <- max(1, detectCores() - 1)
cat("Available cores for parallel processing:", num_cores, "\n")
```

#### Monitor Resource Usage
```r
# Check memory usage during simulation
check_memory <- function() {
  if (.Platform$OS.type == "windows") {
    cat("Memory usage:", round(memory.size()/1024, 2), "GB\n")
  } else {
    system("free -h", intern = FALSE)
  }
}

# Call this function periodically during long simulations
check_memory()
```

## Package Development

### Building from Source
```r
# Clone and build the package
setwd("/path/to/AgentsMCP")

# Build documentation
roxygen2::roxygenise()

# Run tests
testthat::test_local()

# Check package
devtools::check()

# Build package
devtools::build()

# Install locally
devtools::install()
```

### Contributing
1. Fork the repository on GitHub
2. Create a feature branch
3. Make changes and add tests
4. Run `R CMD check` 
5. Submit a pull request

## Support

- **Documentation**: `vignette("getting-started", package = "AgentsMCP")`
- **GitHub Issues**: https://github.com/avishekb9/AgentsMCP/issues
- **Email**: bavisek@gmail.com

## System Requirements Summary

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| R Version | 4.0.0 | 4.3.0+ |
| RAM | 4GB | 16GB |
| Storage | 1GB | 5GB |
| CPU | 2 cores | 8+ cores |
| Internet | Required | Broadband |

---

For additional help, please refer to the package documentation or submit an issue on GitHub.