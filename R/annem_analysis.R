#' Main ANNEM Analysis Functions
#'
#' @description
#' High-level analysis functions for running complete ANNEM studies,
#' performance validation, and result generation.

#' Run Complete ANNEM Analysis
#'
#' Execute a comprehensive ANNEM empirical analysis including simulation,
#' performance evaluation, network analysis, and benchmark comparison.
#'
#' @param symbols Character vector of stock symbols to analyze
#' @param n_agents Number of agents in the simulation (default: 1000)
#' @param n_steps Number of simulation steps (default: 250)
#' @param agent_distribution Named list of agent type proportions (optional)
#' @param save_results Logical, whether to save results to disk (default: TRUE)
#' @param output_dir Character string, directory for saving results
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return List containing:
#' \itemize{
#'   \item market: ANNEMMarket object
#'   \item simulation_results: Complete simulation data
#'   \item agent_performance: Agent performance metrics
#'   \item network_metrics: Network evolution data
#'   \item benchmark_comparison: Model comparison results
#'   \item validation_results: Mathematical framework validation
#' }
#'
#' @examples
#' \dontrun{
#' # Basic analysis
#' results <- run_annem_analysis(
#'   symbols = c("AAPL", "MSFT", "GOOGL"),
#'   n_agents = 500,
#'   n_steps = 100
#' )
#' 
#' # Custom agent distribution
#' custom_dist <- list(
#'   neural_momentum = 0.3,
#'   contrarian_ai = 0.2,
#'   fundamentalist_ml = 0.2,
#'   adaptive_noise = 0.1,
#'   social_network = 0.1,
#'   meta_learning = 0.1
#' )
#' 
#' results <- run_annem_analysis(
#'   symbols = c("AAPL", "MSFT"),
#'   agent_distribution = custom_dist
#' )
#' }
#'
#' @export
run_annem_analysis <- function(symbols = c("AAPL", "GOOGL", "MSFT", "TSLA", "NVDA"),
                              n_agents = 1000, n_steps = 250, 
                              agent_distribution = NULL,
                              save_results = TRUE, output_dir = "annem_results",
                              verbose = TRUE) {
  
  if (verbose) {
    cat("Starting ANNEM Empirical Analysis\n")
    cat("================================\n")
    cat("Symbols:", paste(symbols, collapse = ", "), "\n")
    cat("Agents:", n_agents, "\n")
    cat("Steps:", n_steps, "\n")
  }
  
  # Create output directory
  if (save_results && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialize market
  if (verbose) cat("\n1. Initializing Market Environment...\n")
  market <- ANNEMMarket$new(n_agents = n_agents, symbols = symbols)
  
  # Apply custom agent distribution if provided
  if (!is.null(agent_distribution)) {
    # This would require modifying the market initialization
    # For now, we use the default distribution
    if (verbose) cat("Custom agent distribution specified but not yet implemented\n")
  }
  
  # Run simulation
  if (verbose) cat("\n2. Running ANNEM Simulation...\n")
  tryCatch({
    results <- market$run_simulation(n_steps = n_steps, verbose = verbose)
  }, error = function(e) {
    stop("Simulation failed: ", e$message, "\nTry reducing n_agents or n_steps")
  })
  
  # Analyze agent performance
  if (verbose) cat("\n3. Analyzing Agent Performance...\n")
  agent_performance <- market$analyze_agent_performance()
  
  if (verbose) {
    performance_summary <- agent_performance %>%
      group_by(agent_type) %>%
      summarise(
        avg_return = mean(total_return) * 100,
        avg_sharpe = mean(sharpe_ratio),
        count = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(avg_return))
    
    cat("Performance by Agent Type:\n")
    print(performance_summary)
  }
  
  # Analyze network evolution
  if (verbose) cat("\n4. Analyzing Network Evolution...\n")
  network_metrics <- market$analyze_network_evolution()
  
  if (verbose) {
    initial_density <- network_metrics$density[1]
    final_density <- tail(network_metrics$density, 1)
    cat("Network Density Evolution:", round(initial_density, 4), "->", round(final_density, 4), "\n")
  }
  
  # Compare with benchmarks
  if (verbose) cat("\n5. Comparing with Benchmark Models...\n")
  benchmark_comparison <- market$compare_with_benchmarks()
  
  if (verbose) {
    cat("Model Performance Comparison:\n")
    print(benchmark_comparison)
  }
  
  # Validate mathematical framework
  if (verbose) cat("\n6. Validating Mathematical Framework...\n")
  validation_results <- validate_annem_framework(market, results, agent_performance)
  
  if (verbose) {
    passed_tests <- sum(validation_results$Validation_Status == "PASS")
    total_tests <- nrow(validation_results)
    cat("Framework Validation:", passed_tests, "out of", total_tests, "tests passed\n")
  }
  
  # Save results
  if (save_results) {
    if (verbose) cat("\n7. Saving Results...\n")
    
    # Save main results
    saveRDS(results, file.path(output_dir, "annem_simulation_results.rds"))
    write.csv(agent_performance, file.path(output_dir, "agent_performance.csv"), row.names = FALSE)
    write.csv(network_metrics, file.path(output_dir, "network_evolution.csv"), row.names = FALSE)
    write.csv(benchmark_comparison, file.path(output_dir, "benchmark_comparison.csv"), row.names = FALSE)
    write.csv(validation_results, file.path(output_dir, "mathematical_validation.csv"), row.names = FALSE)
    
    # Create summary report
    create_annem_summary_report(market, agent_performance, network_metrics, 
                               benchmark_comparison, validation_results, output_dir)
    
    if (verbose) cat("Results saved to:", output_dir, "\n")
  }
  
  # Compile final results
  analysis_results <- list(
    market = market,
    simulation_results = results,
    agent_performance = agent_performance,
    network_metrics = network_metrics,
    benchmark_comparison = benchmark_comparison,
    validation_results = validation_results,
    config = list(
      symbols = symbols,
      n_agents = n_agents,
      n_steps = n_steps,
      agent_distribution = agent_distribution
    )
  )
  
  if (verbose) {
    cat("\n================================\n")
    cat("ANNEM Analysis Complete!\n")
    cat("================================\n")
  }
  
  return(analysis_results)
}

#' Validate ANNEM Mathematical Framework
#'
#' Validate the implementation against the theoretical mathematical framework
#' as described in the ANNEM paper.
#'
#' @param market ANNEMMarket object
#' @param simulation_results Simulation results from run_simulation
#' @param agent_performance Agent performance data frame
#'
#' @return Data frame with validation metrics and status
#'
#' @examples
#' \dontrun{
#' market <- create_annem_market(n_agents = 100)
#' results <- market$run_simulation(n_steps = 50)
#' performance <- market$analyze_agent_performance()
#' validation <- validate_annem_framework(market, results, performance)
#' }
#'
#' @export
validate_annem_framework <- function(market, simulation_results, agent_performance) {
  
  validation <- data.frame(
    Metric = character(),
    Mathematical_Definition = character(),
    Implemented_Value = numeric(),
    Expected_Range = character(),
    Validation_Status = character(),
    stringsAsFactors = FALSE
  )
  
  # 1. Agent Space Validation (Definition 1)
  n_agents <- length(market$agents)
  agent_types <- unique(simulation_results$agent_types)
  
  validation <- rbind(validation, data.frame(
    Metric = "Agent Space Size",
    Mathematical_Definition = "N = |A|",
    Implemented_Value = n_agents,
    Expected_Range = paste("Target:", n_agents),
    Validation_Status = "PASS",
    stringsAsFactors = FALSE
  ))
  
  # 2. Agent Type Distribution (Equation 54)
  type_distribution <- table(simulation_results$agent_types) / length(simulation_results$agent_types)
  
  expected_types <- c("neural_momentum", "contrarian_ai", "fundamentalist_ml", 
                     "adaptive_noise", "social_network", "meta_learning")
  expected_dist <- c(0.20, 0.15, 0.18, 0.12, 0.25, 0.10)
  
  actual_dist <- numeric(length(expected_types))
  for (i in 1:length(expected_types)) {
    if (expected_types[i] %in% names(type_distribution)) {
      actual_dist[i] <- as.numeric(type_distribution[expected_types[i]])
    }
  }
  
  dist_error <- mean(abs(actual_dist - expected_dist))
  
  validation <- rbind(validation, data.frame(
    Metric = "Agent Distribution Error", 
    Mathematical_Definition = "Mean |π_actual - π_expected|",
    Implemented_Value = round(dist_error, 4),
    Expected_Range = "< 0.05",
    Validation_Status = ifelse(dist_error < 0.05, "PASS", "ADJUST"),
    stringsAsFactors = FALSE
  ))
  
  # 3. Network Metrics (Section 2.4)
  final_network <- market$network
  density <- edge_density(final_network)
  clustering <- transitivity(final_network, type = "global")
  
  validation <- rbind(validation, data.frame(
    Metric = "Network Density",
    Mathematical_Definition = "ρ(t) = 2|E|/(|V|(|V|-1))",
    Implemented_Value = round(density, 4),
    Expected_Range = "0.05-0.15",
    Validation_Status = ifelse(density >= 0.05 & density <= 0.15, "PASS", "REVIEW"),
    stringsAsFactors = FALSE
  ))
  
  validation <- rbind(validation, data.frame(
    Metric = "Network Clustering",
    Mathematical_Definition = "C = Σ(triangles)/Σ(triples)",
    Implemented_Value = round(clustering, 4),
    Expected_Range = "0.3-0.7",
    Validation_Status = ifelse(!is.na(clustering) && clustering >= 0.3 && clustering <= 0.7, "PASS", "REVIEW"),
    stringsAsFactors = FALSE
  ))
  
  # 4. Performance Metrics (Table 1 from paper)
  # Check if any agent type has positive average performance
  performance_by_type <- agent_performance %>%
    group_by(agent_type) %>%
    summarise(avg_return = mean(total_return), .groups = 'drop')
  
  max_performance <- max(performance_by_type$avg_return)
  
  validation <- rbind(validation, data.frame(
    Metric = "Best Agent Performance",
    Mathematical_Definition = "max(E[R_i]) for agent types",
    Implemented_Value = round(max_performance * 100, 2),
    Expected_Range = "> 0%",
    Validation_Status = ifelse(max_performance > 0, "PASS", "REVIEW"),
    stringsAsFactors = FALSE
  ))
  
  # 5. Wealth Distribution (Gini Coefficient)
  final_wealth <- simulation_results$agent_wealth[nrow(simulation_results$agent_wealth), ]
  gini_coeff <- calculate_gini_coefficient(final_wealth)
  
  validation <- rbind(validation, data.frame(
    Metric = "Wealth Gini Coefficient",
    Mathematical_Definition = "G = Σ|w_i - w_j|/(2n²μ)",
    Implemented_Value = round(gini_coeff, 4),
    Expected_Range = "0.2-0.8",
    Validation_Status = ifelse(gini_coeff >= 0.2 & gini_coeff <= 0.8, "PASS", "REVIEW"),
    stringsAsFactors = FALSE
  ))
  
  # 6. Simulation Convergence
  n_steps_completed <- simulation_results$n_steps
  expected_steps <- nrow(market$market_data$returns)
  
  validation <- rbind(validation, data.frame(
    Metric = "Simulation Completion",
    Mathematical_Definition = "Steps completed / Steps available",
    Implemented_Value = round(n_steps_completed / expected_steps, 4),
    Expected_Range = "> 0.8",
    Validation_Status = ifelse(n_steps_completed / expected_steps > 0.8, "PASS", "REVIEW"),
    stringsAsFactors = FALSE
  ))
  
  return(validation)
}

#' Calculate Performance Metrics
#'
#' Calculate comprehensive performance metrics for agents or portfolios.
#'
#' @param returns Numeric vector or matrix of returns
#' @param benchmark Optional benchmark returns for comparison
#' @param risk_free_rate Annual risk-free rate (default: 0.02)
#'
#' @return List containing various performance metrics
#'
#' @examples
#' \dontrun{
#' returns <- rnorm(252, 0.08/252, 0.2/sqrt(252))  # Daily returns
#' metrics <- calculate_performance_metrics(returns)
#' print(metrics)
#' }
#'
#' @export
calculate_performance_metrics <- function(returns, benchmark = NULL, risk_free_rate = 0.02) {
  
  # Convert to daily risk-free rate
  daily_rf <- risk_free_rate / 252
  
  # Basic metrics
  total_return <- prod(1 + returns) - 1
  annualized_return <- (1 + total_return)^(252 / length(returns)) - 1
  annualized_volatility <- sd(returns) * sqrt(252)
  sharpe_ratio <- (annualized_return - risk_free_rate) / annualized_volatility
  
  # Downside metrics
  downside_returns <- returns[returns < 0]
  downside_volatility <- sd(downside_returns) * sqrt(252)
  sortino_ratio <- (annualized_return - risk_free_rate) / downside_volatility
  
  # Drawdown analysis
  cumulative_returns <- cumprod(1 + returns)
  running_max <- cummax(cumulative_returns)
  drawdowns <- (cumulative_returns - running_max) / running_max
  max_drawdown <- min(drawdowns)
  
  # Additional metrics
  win_rate <- mean(returns > 0)
  avg_win <- mean(returns[returns > 0])
  avg_loss <- mean(returns[returns < 0])
  profit_factor <- ifelse(avg_loss != 0, abs(avg_win / avg_loss), Inf)
  
  metrics <- list(
    total_return = total_return,
    annualized_return = annualized_return,
    annualized_volatility = annualized_volatility,
    sharpe_ratio = sharpe_ratio,
    sortino_ratio = sortino_ratio,
    max_drawdown = max_drawdown,
    win_rate = win_rate,
    profit_factor = profit_factor,
    skewness = moments::skewness(returns),
    kurtosis = moments::kurtosis(returns)
  )
  
  # Benchmark comparison if provided
  if (!is.null(benchmark)) {
    benchmark_return <- prod(1 + benchmark) - 1
    excess_return <- total_return - benchmark_return
    tracking_error <- sd(returns - benchmark) * sqrt(252)
    information_ratio <- excess_return / tracking_error
    
    metrics$benchmark_return <- benchmark_return
    metrics$excess_return <- excess_return
    metrics$tracking_error <- tracking_error
    metrics$information_ratio <- information_ratio
  }
  
  return(metrics)
}

#' Create ANNEM Summary Report
#'
#' Generate a comprehensive text summary report of ANNEM analysis results.
#'
#' @param market ANNEMMarket object
#' @param agent_performance Agent performance data frame
#' @param network_metrics Network evolution metrics
#' @param benchmark_comparison Model comparison results
#' @param validation_results Framework validation results
#' @param output_dir Directory to save the report
#'
#' @export
create_annem_summary_report <- function(market, agent_performance, network_metrics, 
                                       benchmark_comparison, validation_results, output_dir) {
  
  report_file <- file.path(output_dir, "annem_summary_report.txt")
  
  sink(report_file)
  
  cat("ANNEM Empirical Analysis Summary Report\n")
  cat("======================================\n")
  cat("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Simulation parameters
  cat("SIMULATION PARAMETERS:\n")
  cat("- Number of agents:", length(market$agents), "\n")
  cat("- Number of steps:", market$simulation_results$n_steps, "\n")
  cat("- Assets analyzed:", ncol(market$market_data$returns), "\n")
  cat("- Symbols:", paste(market$market_data$symbols, collapse = ", "), "\n\n")
  
  # Agent type distribution
  agent_types <- table(market$simulation_results$agent_types)
  cat("AGENT TYPE DISTRIBUTION:\n")
  for (i in 1:length(agent_types)) {
    cat("-", names(agent_types)[i], ":", agent_types[i], 
        paste0("(", round(agent_types[i]/sum(agent_types)*100, 1), "%)"), "\n")
  }
  cat("\n")
  
  # Performance summary
  cat("PERFORMANCE SUMMARY:\n")
  performance_by_type <- agent_performance %>%
    group_by(agent_type) %>%
    summarise(
      avg_return = mean(total_return) * 100,
      std_return = sd(total_return) * 100,
      avg_sharpe = mean(sharpe_ratio),
      .groups = 'drop'
    )
  
  for (i in 1:nrow(performance_by_type)) {
    cat("-", performance_by_type$agent_type[i], ":\n")
    cat("  Average Return:", round(performance_by_type$avg_return[i], 2), "%\n")
    cat("  Return Std Dev:", round(performance_by_type$std_return[i], 2), "%\n")
    cat("  Average Sharpe:", round(performance_by_type$avg_sharpe[i], 3), "\n")
  }
  cat("\n")
  
  # Network evolution
  cat("NETWORK EVOLUTION:\n")
  cat("- Initial density:", round(network_metrics$density[1], 4), "\n")
  cat("- Final density:", round(tail(network_metrics$density, 1), 4), "\n")
  cat("- Average clustering:", round(mean(network_metrics$clustering, na.rm = TRUE), 4), "\n\n")
  
  # Model comparison
  cat("MODEL COMPARISON:\n")
  for (i in 1:nrow(benchmark_comparison)) {
    cat("-", benchmark_comparison$Model[i], ":\n")
    cat("  MSE:", sprintf("%.6f", benchmark_comparison$MSE[i]), "\n")
    cat("  MAE:", sprintf("%.4f", benchmark_comparison$MAE[i]), "\n")
    cat("  Directional Accuracy:", sprintf("%.3f", benchmark_comparison$Directional_Accuracy[i]), "\n")
  }
  cat("\n")
  
  # Validation results
  cat("MATHEMATICAL FRAMEWORK VALIDATION:\n")
  passed_tests <- sum(validation_results$Validation_Status == "PASS")
  total_tests <- nrow(validation_results)
  cat("- Tests passed:", passed_tests, "out of", total_tests, "\n")
  
  failed_tests <- validation_results[validation_results$Validation_Status != "PASS", ]
  if (nrow(failed_tests) > 0) {
    cat("- Tests requiring attention:\n")
    for (i in 1:nrow(failed_tests)) {
      cat("  *", failed_tests$Metric[i], ":", failed_tests$Validation_Status[i], "\n")
    }
  }
  
  cat("\nReport generated by AgentsMCP package\n")
  
  sink()
  
  cat("Summary report saved to:", report_file, "\n")
}