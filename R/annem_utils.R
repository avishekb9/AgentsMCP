#' Utility Functions for ANNEM Package
#'
#' @description
#' Collection of utility functions for network analysis, performance calculations,
#' and general ANNEM operations.

#' Evolve Network Connections
#'
#' Update network topology based on agent decision similarity and performance.
#'
#' @param network igraph network object
#' @param agent_decisions Numeric vector of agent decisions
#' @param similarity_threshold Threshold for connecting agents (default: 0.8)
#' @param evolution_rate Rate of network evolution (default: 0.01)
#'
#' @return Updated igraph network object
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' net <- sample_smallworld(1, 100, 4, 0.1)
#' decisions <- rnorm(100)
#' new_net <- evolve_network(net, decisions)
#' }
#'
#' @export
evolve_network <- function(network, agent_decisions, similarity_threshold = 0.8, evolution_rate = 0.01) {
  
  adj_matrix <- as_adjacency_matrix(network, sparse = FALSE)
  n_agents <- length(agent_decisions)
  
  # Calculate decision similarity matrix
  similarity_matrix <- outer(agent_decisions, agent_decisions, function(x, y) 1 - abs(x - y))
  
  # Evolve connections based on similarity
  for (i in 1:(n_agents-1)) {
    for (j in (i+1):n_agents) {
      # Add connections for similar agents
      if (similarity_matrix[i, j] > similarity_threshold && runif(1) < evolution_rate) {
        adj_matrix[i, j] <- adj_matrix[j, i] <- 1
      }
      # Remove connections for dissimilar agents  
      else if (similarity_matrix[i, j] < (1 - similarity_threshold) && runif(1) < evolution_rate/2) {
        adj_matrix[i, j] <- adj_matrix[j, i] <- 0
      }
    }
  }
  
  return(graph_from_adjacency_matrix(adj_matrix, mode = "undirected"))
}

#' Calculate Network Metrics
#'
#' Compute comprehensive network topology metrics.
#'
#' @param network igraph network object
#' @param weighted Logical, whether to consider edge weights (default: FALSE)
#'
#' @return List containing network metrics
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' net <- sample_smallworld(1, 100, 4, 0.1)
#' metrics <- calculate_network_metrics(net)
#' print(metrics)
#' }
#'
#' @export
calculate_network_metrics <- function(network, weighted = FALSE) {
  
  metrics <- list()
  
  # Basic metrics
  metrics$n_nodes <- vcount(network)
  metrics$n_edges <- ecount(network)
  metrics$density <- edge_density(network)
  
  # Clustering and path metrics
  metrics$global_clustering <- transitivity(network, type = "global")
  metrics$local_clustering_avg <- mean(transitivity(network, type = "local"), na.rm = TRUE)
  
  if (is_connected(network)) {
    metrics$avg_path_length <- mean_distance(network, directed = FALSE, unconnected = TRUE)
    metrics$diameter <- diameter(network, directed = FALSE, unconnected = TRUE)
  } else {
    # For disconnected networks
    components <- igraph::decompose(network)
    largest_component <- components[[which.max(sapply(components, vcount))]]
    metrics$avg_path_length <- mean_distance(largest_component, directed = FALSE)
    metrics$diameter <- diameter(largest_component, directed = FALSE)
    metrics$n_components <- length(components)
  }
  
  # Centrality measures
  metrics$degree_centrality <- degree(network)
  metrics$betweenness_centrality <- betweenness(network)
  metrics$closeness_centrality <- closeness(network)
  metrics$eigenvector_centrality <- eigen_centrality(network)$vector
  
  # Summary statistics for centralities
  metrics$degree_stats <- list(
    mean = mean(metrics$degree_centrality),
    sd = sd(metrics$degree_centrality),
    max = max(metrics$degree_centrality)
  )
  
  metrics$betweenness_stats <- list(
    mean = mean(metrics$betweenness_centrality),
    sd = sd(metrics$betweenness_centrality), 
    max = max(metrics$betweenness_centrality)
  )
  
  # Small-world properties
  # Compare to random network with same n_nodes and density
  random_net <- sample_gnp(metrics$n_nodes, metrics$density)
  random_clustering <- transitivity(random_net, type = "global")
  random_path_length <- mean_distance(random_net)
  
  metrics$small_world_sigma <- (metrics$global_clustering / random_clustering) / 
                              (metrics$avg_path_length / random_path_length)
  
  # Modularity (community structure)
  communities <- cluster_louvain(network)
  metrics$modularity <- modularity(communities)
  metrics$n_communities <- length(communities)
  
  return(metrics)
}

#' Calculate Gini Coefficient
#'
#' Compute Gini coefficient for wealth or performance distribution analysis.
#'
#' @param x Numeric vector of values (e.g., wealth, returns)
#'
#' @return Numeric Gini coefficient [0, 1]
#'
#' @examples
#' wealth <- c(1000000, 1500000, 800000, 2000000, 1200000)
#' gini <- calculate_gini_coefficient(wealth)
#' print(gini)
#'
#' @export
calculate_gini_coefficient <- function(x) {
  
  # Remove missing values
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA)
  }
  
  # Sort values
  x <- sort(x)
  n <- length(x)
  
  # Calculate Gini coefficient
  gini <- (2 * sum((1:n) * x)) / (n * sum(x)) - (n + 1) / n
  
  return(gini)
}

#' Calculate Sharpe Ratio
#'
#' Compute annualized Sharpe ratio for return series.
#'
#' @param returns Numeric vector of returns
#' @param risk_free_rate Annual risk-free rate (default: 0.02)
#' @param periods_per_year Number of periods per year (default: 252 for daily)
#'
#' @return Numeric Sharpe ratio
#'
#' @examples
#' returns <- rnorm(252, 0.08/252, 0.2/sqrt(252))
#' sharpe <- calculate_sharpe_ratio(returns)
#' print(sharpe)
#'
#' @export
calculate_sharpe_ratio <- function(returns, risk_free_rate = 0.02, periods_per_year = 252) {
  
  # Calculate excess returns
  excess_returns <- returns - risk_free_rate / periods_per_year
  
  # Annualized Sharpe ratio
  sharpe <- mean(excess_returns) / sd(excess_returns) * sqrt(periods_per_year)
  
  return(sharpe)
}

#' Compare with Benchmark Models
#'
#' Compare ANNEM predictions with traditional econometric models.
#'
#' @param annem_predictions Numeric vector of ANNEM predictions
#' @param actual_returns Numeric vector of actual returns
#' @param benchmark_models Character vector of benchmark model names
#'
#' @return Data frame with comparison metrics
#'
#' @examples
#' \dontrun{
#' predictions <- rnorm(100)
#' actual <- rnorm(100)
#' comparison <- compare_with_benchmarks(predictions, actual)
#' }
#'
#' @export
compare_with_benchmarks <- function(annem_predictions, actual_returns, 
                                   benchmark_models = c("VAR", "Random_Walk")) {
  
  n_obs <- length(actual_returns)
  
  # Generate benchmark predictions
  benchmark_predictions <- list()
  
  if ("VAR" %in% benchmark_models) {
    # Simple VAR model (AR(1) for univariate case)
    if (n_obs > 10) {
      var_model <- lm(actual_returns[-1] ~ actual_returns[-n_obs])
      var_predictions <- c(0, stats::fitted(var_model))  # Add zero for first observation
    } else {
      var_predictions <- rep(mean(actual_returns), n_obs)
    }
    benchmark_predictions[["VAR"]] <- var_predictions
  }
  
  if ("Random_Walk" %in% benchmark_models) {
    # Random walk (previous period return)
    rw_predictions <- c(0, actual_returns[-n_obs])
    benchmark_predictions[["Random_Walk"]] <- rw_predictions
  }
  
  if ("ARIMA" %in% benchmark_models) {
    # ARIMA model
    tryCatch({
      arima_model <- forecast::auto.arima(actual_returns)
      arima_predictions <- stats::fitted(arima_model)
      benchmark_predictions[["ARIMA"]] <- as.numeric(arima_predictions)
    }, error = function(e) {
      benchmark_predictions[["ARIMA"]] <<- rep(mean(actual_returns), n_obs)
    })
  }
  
  # Combine all models
  all_predictions <- list(ANNEM = annem_predictions)
  all_predictions <- c(all_predictions, benchmark_predictions)
  
  # Calculate performance metrics for each model
  results <- data.frame(
    Model = names(all_predictions),
    MSE = numeric(length(all_predictions)),
    MAE = numeric(length(all_predictions)),
    RMSE = numeric(length(all_predictions)),
    Directional_Accuracy = numeric(length(all_predictions)),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(all_predictions)) {
    pred <- all_predictions[[i]]
    
    # Ensure same length
    min_length <- min(length(pred), length(actual_returns))
    pred <- pred[1:min_length]
    actual <- actual_returns[1:min_length]
    
    # Calculate metrics
    results$MSE[i] <- mean((pred - actual)^2, na.rm = TRUE)
    results$MAE[i] <- mean(abs(pred - actual), na.rm = TRUE)
    results$RMSE[i] <- sqrt(results$MSE[i])
    
    # Directional accuracy (exclude first observation)
    if (min_length > 1) {
      pred_direction <- sign(pred[-1])
      actual_direction <- sign(actual[-1])
      results$Directional_Accuracy[i] <- mean(pred_direction == actual_direction, na.rm = TRUE)
    } else {
      results$Directional_Accuracy[i] <- NA
    }
  }
  
  return(results)
}

#' Fit VAR Model
#'
#' Fit Vector Autoregression model for benchmark comparison.
#'
#' @param returns_data Matrix of return series
#' @param lag_order Order of VAR model (default: 1)
#'
#' @return Fitted values from VAR model
#'
#' @export
fit_var_model <- function(returns_data, lag_order = 1) {
  
  if (is.vector(returns_data)) {
    # Univariate case - use AR model
    n_obs <- length(returns_data)
    
    if (n_obs > (lag_order + 10)) {
      # Fit AR model
      ar_model <- lm(returns_data[(lag_order+1):n_obs] ~ 
                     returns_data[lag_order:(n_obs-1)])
      fitted_values <- c(rep(0, lag_order), stats::fitted(ar_model))
    } else {
      # Not enough data - return mean
      fitted_values <- rep(mean(returns_data), n_obs)
    }
    
  } else {
    # Multivariate case - use first column for simplicity
    fitted_values <- fit_var_model(returns_data[, 1], lag_order)
  }
  
  return(fitted_values)
}

# Performance metric helper functions
calculate_mse <- function(predictions, actual) {
  mean((predictions - actual)^2, na.rm = TRUE)
}

calculate_mae <- function(predictions, actual) {
  mean(abs(predictions - actual), na.rm = TRUE)
}

calculate_directional_accuracy <- function(predictions, actual) {
  if (length(predictions) != length(actual)) {
    min_length <- min(length(predictions), length(actual))
    predictions <- predictions[1:min_length]
    actual <- actual[1:min_length]
  }
  
  if (length(predictions) <= 1) {
    return(NA)
  }
  
  pred_direction <- sign(predictions[-1])
  actual_direction <- sign(actual[-1])
  mean(pred_direction == actual_direction, na.rm = TRUE)
}

#' Set Random Seed for ANNEM
#'
#' Set random seed for reproducible ANNEM simulations.
#'
#' @param seed Integer random seed
#'
#' @examples
#' set_annem_seed(42)
#'
#' @export
set_annem_seed <- function(seed = 42) {
  set.seed(seed)
  cat("Random seed set to:", seed, "\n")
}

#' Get ANNEM Configuration
#'
#' Return default configuration parameters for ANNEM simulations.
#'
#' @return List of configuration parameters
#'
#' @examples
#' config <- get_annem_config()
#' print(config)
#'
#' @export
get_annem_config <- function() {
  
  config <- list(
    # Agent parameters
    agent_types = c("neural_momentum", "contrarian_ai", "fundamentalist_ml", 
                   "adaptive_noise", "social_network", "meta_learning"),
    agent_distribution = c(0.20, 0.15, 0.18, 0.12, 0.25, 0.10),
    
    # Simulation parameters
    default_n_agents = 1000,
    default_n_steps = 250,
    default_symbols = c("AAPL", "GOOGL", "MSFT", "TSLA", "NVDA"),
    
    # Network parameters
    network_type = "small_world",
    initial_degree = 6,
    rewiring_prob = 0.1,
    evolution_rate = 0.01,
    
    # Economic parameters
    risk_free_rate = 0.02,
    trading_cost = 0.001,
    
    # Neural network parameters
    input_dim = 50,
    hidden_dims = c(128, 64),
    learning_rate = 0.001,
    
    # Package info
    package_version = "1.0.0",
    framework_version = "ANNEM-2025"
  )
  
  return(config)
}

#' ANNEM Summary
#'
#' Print summary information about ANNEM analysis results.
#'
#' @param analysis_results Results from run_annem_analysis()
#'
#' @examples
#' \dontrun{
#' results <- run_annem_analysis(n_agents = 100, n_steps = 50)
#' annem_summary(results)
#' }
#'
#' @export
annem_summary <- function(analysis_results) {
  
  cat("ANNEM Analysis Summary\n")
  cat("=====================\n\n")
  
  # Basic info
  config <- analysis_results$config
  cat("Configuration:\n")
  cat("- Symbols:", paste(config$symbols, collapse = ", "), "\n")
  cat("- Agents:", config$n_agents, "\n")
  cat("- Steps:", config$n_steps, "\n\n")
  
  # Performance
  performance <- analysis_results$agent_performance
  avg_return <- mean(performance$total_return) * 100
  cat("Performance:\n")
  cat("- Average return:", round(avg_return, 2), "%\n")
  cat("- Best agent return:", round(max(performance$total_return) * 100, 2), "%\n")
  cat("- Worst agent return:", round(min(performance$total_return) * 100, 2), "%\n\n")
  
  # Best performing agent type
  best_type <- performance %>%
    group_by(agent_type) %>%
    summarise(avg_ret = mean(total_return), .groups = 'drop') %>%
    arrange(desc(avg_ret)) %>%
    slice(1)
  
  cat("Best performing agent type:", best_type$agent_type, 
      "(", round(best_type$avg_ret * 100, 2), "% avg return)\n")
  
  # Model comparison
  comparison <- analysis_results$benchmark_comparison
  annem_row <- comparison[comparison$Model == "ANNEM", ]
  
  cat("\nModel Performance:\n")
  cat("- ANNEM MSE:", sprintf("%.6f", annem_row$MSE), "\n")
  cat("- ANNEM Directional Accuracy:", sprintf("%.3f", annem_row$Directional_Accuracy), "\n")
  
  # Validation
  validation <- analysis_results$validation_results
  passed <- sum(validation$Validation_Status == "PASS")
  total <- nrow(validation)
  
  cat("\nValidation:", passed, "out of", total, "tests passed\n")
  
  cat("\n=====================\n")
}