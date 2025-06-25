#' ANNEM Visualization Functions
#'
#' @description
#' Comprehensive visualization suite for ANNEM empirical results including
#' agent performance plots, network evolution, wealth dynamics, and model comparisons.

#' Plot Agent Performance by Type
#'
#' Create comprehensive visualizations of agent performance across different types.
#'
#' @param agent_performance Data frame with agent performance metrics
#' @param plot_type Character string: "boxplot", "scatter", or "both" (default: "both")
#'
#' @return List containing ggplot objects
#'
#' @examples
#' \dontrun{
#' market <- create_annem_market(n_agents = 100)
#' results <- market$run_simulation(n_steps = 50)
#' performance <- market$analyze_agent_performance()
#' plots <- plot_agent_performance(performance)
#' print(plots$performance_dist)
#' }
#'
#' @export
plot_agent_performance <- function(agent_performance, plot_type = "both") {
  
  plots <- list()
  
  # Performance distribution boxplot
  if (plot_type %in% c("boxplot", "both")) {
    plots$performance_dist <- ggplot(agent_performance, aes(x = agent_type, y = total_return * 100, fill = agent_type)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
      scale_fill_viridis_d(name = "Agent Type") +
      labs(
        title = "Agent Performance Distribution by Type",
        subtitle = "Total returns across different agent types",
        x = "Agent Type",
        y = "Total Return (%)",
        caption = "AgentsMCP Package - ANNEM Analysis"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray60")
      )
  }
  
  # Risk-return scatter plot
  if (plot_type %in% c("scatter", "both")) {
    performance_summary <- agent_performance %>%
      group_by(agent_type) %>%
      summarise(
        avg_return = mean(total_return) * 100,
        avg_sharpe = mean(sharpe_ratio),
        count = n(),
        .groups = 'drop'
      )
    
    plots$risk_return <- ggplot(performance_summary, aes(x = avg_return, y = avg_sharpe, size = count, color = agent_type)) +
      geom_point(alpha = 0.8) +
      scale_color_viridis_d(name = "Agent Type") +
      scale_size_continuous(name = "Count", range = c(3, 10)) +
      labs(
        title = "Risk-Return Profile by Agent Type",
        subtitle = "Average return vs Sharpe ratio",
        x = "Average Return (%)",
        y = "Average Sharpe Ratio"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray60")
      )
  }
  
  return(plots)
}

#' Plot Network Evolution
#'
#' Visualize the evolution of network topology metrics over time.
#'
#' @param network_metrics Data frame with network evolution metrics
#' @param metrics_to_plot Character vector of metrics to plot (default: all)
#'
#' @return List containing ggplot objects
#'
#' @examples
#' \dontrun{
#' market <- create_annem_market(n_agents = 100)
#' results <- market$run_simulation(n_steps = 50)
#' network_metrics <- market$analyze_network_evolution()
#' plots <- plot_network_evolution(network_metrics)
#' }
#'
#' @export
plot_network_evolution <- function(network_metrics, metrics_to_plot = c("density", "clustering", "avg_path_length")) {
  
  plots <- list()
  
  # Prepare data for plotting
  available_metrics <- intersect(metrics_to_plot, colnames(network_metrics))
  
  if (length(available_metrics) == 0) {
    stop("No valid metrics found in network_metrics data frame")
  }
  
  # Reshape data for plotting multiple metrics
  network_long <- network_metrics %>%
    select(step, all_of(available_metrics)) %>%
    pivot_longer(cols = -step, names_to = "metric", values_to = "value") %>%
    filter(!is.na(value))
  
  # Normalize metrics for comparison
  network_long <- network_long %>%
    group_by(metric) %>%
    mutate(
      normalized_value = if(max(value, na.rm = TRUE) > min(value, na.rm = TRUE)) {
        (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
      } else {
        rep(0.5, length(value))
      }
    ) %>%
    ungroup()
  
  # Combined evolution plot
  plots$evolution <- ggplot(network_long, aes(x = step, y = normalized_value, color = metric)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_smooth(method = "loess", se = FALSE, alpha = 0.3) +
    scale_color_viridis_d(name = "Network Metric") +
    labs(
      title = "Network Evolution Over Time",
      subtitle = "Normalized network metrics (0-1 scale)",
      x = "Simulation Step",
      y = "Normalized Value",
      caption = "All metrics normalized for comparison"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      legend.position = "bottom"
    )
  
  # Individual metric plots
  if ("density" %in% available_metrics) {
    plots$density <- ggplot(network_metrics, aes(x = step, y = density)) +
      geom_line(color = "#440154", size = 1) +
      geom_smooth(method = "loess", color = "#FDE725", fill = "#FDE725", alpha = 0.2) +
      labs(
        title = "Network Density Evolution",
        x = "Simulation Step",
        y = "Network Density"
      ) +
      theme_minimal()
  }
  
  if ("clustering" %in% available_metrics) {
    plots$clustering <- ggplot(network_metrics, aes(x = step, y = clustering)) +
      geom_line(color = "#31688e", size = 1) +
      geom_smooth(method = "loess", color = "#35b779", fill = "#35b779", alpha = 0.2) +
      labs(
        title = "Network Clustering Evolution", 
        x = "Simulation Step",
        y = "Global Clustering Coefficient"
      ) +
      theme_minimal()
  }
  
  return(plots)
}

#' Plot Wealth Dynamics
#'
#' Visualize agent wealth evolution over simulation time.
#'
#' @param simulation_results ANNEM simulation results
#' @param n_sample Number of agents to sample for individual trajectories (default: 50)
#'
#' @return List containing ggplot objects
#'
#' @examples
#' \dontrun{
#' market <- create_annem_market(n_agents = 100)
#' results <- market$run_simulation(n_steps = 50)
#' plots <- plot_wealth_dynamics(results)
#' }
#'
#' @export
plot_wealth_dynamics <- function(simulation_results, n_sample = 50) {
  
  wealth_data <- simulation_results$agent_wealth
  agent_types <- simulation_results$agent_types
  
  # Sample agents for visualization
  n_sample <- min(n_sample, ncol(wealth_data))
  sample_agents <- sample(1:ncol(wealth_data), n_sample)
  
  # Create data frame for plotting
  wealth_df <- data.frame(
    step = rep(1:nrow(wealth_data), n_sample),
    wealth = as.vector(wealth_data[, sample_agents]),
    agent_id = rep(sample_agents, each = nrow(wealth_data)),
    agent_type = rep(agent_types[sample_agents], each = nrow(wealth_data))
  )
  
  plots <- list()
  
  # Individual wealth trajectories
  plots$individual <- ggplot(wealth_df, aes(x = step, y = wealth, color = agent_type, group = agent_id)) +
    geom_line(alpha = 0.6, size = 0.5) +
    scale_color_viridis_d(name = "Agent Type") +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    labs(
      title = "Individual Agent Wealth Evolution",
      subtitle = paste("Sample of", n_sample, "agents"),
      x = "Simulation Step",
      y = "Wealth (Millions USD)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      legend.position = "bottom"
    )
  
  # Average wealth by type
  wealth_by_type <- wealth_df %>%
    group_by(step, agent_type) %>%
    summarise(
      avg_wealth = mean(wealth),
      median_wealth = median(wealth),
      q25 = quantile(wealth, 0.25),
      q75 = quantile(wealth, 0.75),
      .groups = 'drop'
    )
  
  plots$by_type <- ggplot(wealth_by_type, aes(x = step, y = avg_wealth, color = agent_type, fill = agent_type)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2, color = NA) +
    geom_line(size = 1.2) +
    scale_color_viridis_d(name = "Agent Type") +
    scale_fill_viridis_d(name = "Agent Type") +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
    labs(
      title = "Average Wealth Evolution by Agent Type",
      subtitle = "Lines show average, ribbons show IQR",
      x = "Simulation Step",
      y = "Average Wealth (Millions USD)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      legend.position = "bottom"
    )
  
  return(plots)
}

#' Plot Decision Heatmap
#'
#' Create a heatmap visualization of agent decisions over time.
#'
#' @param simulation_results ANNEM simulation results
#' @param n_sample_agents Number of agents to sample (default: 100)
#' @param n_sample_steps Number of time steps to sample (default: 100)
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' market <- create_annem_market(n_agents = 100)
#' results <- market$run_simulation(n_steps = 50)
#' heatmap_plot <- plot_decision_heatmap(results)
#' print(heatmap_plot)
#' }
#'
#' @export
plot_decision_heatmap <- function(simulation_results, n_sample_agents = 100, n_sample_steps = 100) {
  
  decisions <- simulation_results$agent_decisions
  agent_types <- simulation_results$agent_types
  
  # Sample for visualization
  n_sample_agents <- min(n_sample_agents, ncol(decisions))
  n_sample_steps <- min(n_sample_steps, nrow(decisions))
  
  sample_agents <- sample(1:ncol(decisions), n_sample_agents)
  sample_steps <- seq(1, nrow(decisions), length.out = n_sample_steps)
  sample_steps <- round(sample_steps)
  
  # Create heatmap data
  heatmap_data <- decisions[sample_steps, sample_agents]
  
  # Convert to long format
  heatmap_df <- expand.grid(
    step = sample_steps,
    agent = sample_agents
  )
  heatmap_df$decision <- as.vector(heatmap_data)
  heatmap_df$agent_type <- rep(agent_types[sample_agents], each = length(sample_steps))
  
  # Order by agent type for better visualization
  heatmap_df <- heatmap_df[order(heatmap_df$agent_type, heatmap_df$agent), ]
  heatmap_df$agent_ordered <- factor(heatmap_df$agent, levels = unique(heatmap_df$agent))
  
  p1 <- ggplot(heatmap_df, aes(x = step, y = agent_ordered, fill = decision)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue", midpoint = 0,
      name = "Decision\n(Sell/Hold/Buy)"
    ) +
    labs(
      title = "Agent Decision Heatmap",
      subtitle = paste("Sample of", n_sample_agents, "agents over", n_sample_steps, "steps"),
      x = "Simulation Step",
      y = "Agent ID (grouped by type)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  return(p1)
}

#' Plot Model Comparison
#'
#' Visualize performance comparison between ANNEM and benchmark models.
#'
#' @param benchmark_comparison Data frame with model comparison results
#'
#' @return List containing ggplot objects
#'
#' @examples
#' \dontrun{
#' market <- create_annem_market(n_agents = 100)
#' results <- market$run_simulation(n_steps = 50)
#' comparison <- market$compare_with_benchmarks()
#' plots <- plot_model_comparison(comparison)
#' }
#'
#' @export
plot_model_comparison <- function(benchmark_comparison) {
  
  plots <- list()
  
  # Reshape for plotting
  comparison_long <- benchmark_comparison %>%
    pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")
  
  # Performance metrics comparison
  plots$comparison <- ggplot(comparison_long, aes(x = Model, y = Value, fill = Model)) +
    geom_col(alpha = 0.8) +
    facet_wrap(~Metric, scales = "free_y", ncol = 3) +
    scale_fill_viridis_d(name = "Model") +
    labs(
      title = "Model Performance Comparison",
      subtitle = "ANNEM vs Benchmark Models",
      x = "Model",
      y = "Metric Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
  
  # Model ranking plot
  # Create ranking based on multiple criteria (lower MSE, higher DA)
  ranking_df <- benchmark_comparison %>%
    mutate(
      MSE_rank = rank(MSE),  # Lower is better
      MAE_rank = rank(MAE),  # Lower is better  
      DA_rank = rank(-Directional_Accuracy),  # Higher is better, so negate
      overall_rank = (MSE_rank + MAE_rank + DA_rank) / 3
    ) %>%
    arrange(overall_rank)
  
  plots$ranking <- ggplot(ranking_df, aes(x = reorder(Model, -overall_rank), y = overall_rank, fill = Model)) +
    geom_col(alpha = 0.8) +
    scale_fill_viridis_d(name = "Model") +
    labs(
      title = "Overall Model Ranking",
      subtitle = "Based on MSE, MAE, and Directional Accuracy",
      x = "Model",
      y = "Average Rank (lower is better)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60"),
      legend.position = "none"
    )
  
  return(plots)
}

#' Create Interactive Network Visualization
#'
#' Generate an interactive network visualization using networkD3.
#'
#' @param network igraph network object
#' @param agent_types Character vector of agent types
#' @param width Width of the visualization (default: 800)
#' @param height Height of the visualization (default: 600)
#'
#' @return networkD3 forceNetwork object
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' net <- sample_smallworld(1, 50, 4, 0.1)
#' agent_types <- sample(c("type1", "type2", "type3"), 50, replace = TRUE)
#' interactive_net <- create_interactive_network(net, agent_types)
#' }
#'
#' @export
create_interactive_network <- function(network, agent_types, width = 800, height = 600) {
  
  # Convert to networkD3 format
  network_d3 <- igraph_to_networkD3(network)
  
  # Add agent type information
  network_d3$nodes$group <- agent_types[1:nrow(network_d3$nodes)]
  network_d3$nodes$size <- degree(network)
  
  # Create color scale for agent types
  agent_type_levels <- unique(agent_types)
  color_scale <- paste0('d3.scaleOrdinal().domain(["', 
                       paste(agent_type_levels, collapse = '","'), 
                       '"]).range(["#440154","#31688e","#35b779","#fde725","#e55c30","#c73e4c"])')
  
  # Create interactive network
  interactive_net <- forceNetwork(
    Links = network_d3$links,
    Nodes = network_d3$nodes,
    Source = "source",
    Target = "target",
    NodeID = "name",
    Group = "group",
    Nodesize = "size",
    opacity = 0.8,
    zoom = TRUE,
    legend = TRUE,
    colourScale = JS(color_scale),
    fontSize = 12,
    width = width,
    height = height
  )
  
  return(interactive_net)
}

#' Generate ANNEM Report
#'
#' Create a comprehensive visualization report with all ANNEM plots.
#'
#' @param analysis_results Complete ANNEM analysis results
#' @param output_dir Directory to save plots (default: "annem_plots")
#' @param save_plots Logical, whether to save plots to disk (default: TRUE)
#'
#' @return List of all generated plots
#'
#' @examples
#' \dontrun{
#' results <- run_annem_analysis(n_agents = 100, n_steps = 50)
#' plots <- generate_annem_report(results)
#' }
#'
#' @export
generate_annem_report <- function(analysis_results, output_dir = "annem_plots", save_plots = TRUE) {
  
  cat("Generating ANNEM Visualization Report...\n")
  
  # Create output directory
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  plots <- list()
  
  # 1. Agent Performance Plots
  cat("Creating agent performance plots...\n")
  perf_plots <- plot_agent_performance(analysis_results$agent_performance)
  plots <- c(plots, perf_plots)
  
  if (save_plots) {
    ggsave(file.path(output_dir, "agent_performance_distribution.png"), 
           perf_plots$performance_dist, width = 12, height = 8, dpi = 300)
    if (!is.null(perf_plots$risk_return)) {
      ggsave(file.path(output_dir, "agent_risk_return.png"), 
             perf_plots$risk_return, width = 10, height = 8, dpi = 300)
    }
  }
  
  # 2. Network Evolution Plots
  cat("Creating network evolution plots...\n")
  network_plots <- plot_network_evolution(analysis_results$network_metrics)
  plots <- c(plots, network_plots)
  
  if (save_plots) {
    ggsave(file.path(output_dir, "network_evolution.png"), 
           network_plots$evolution, width = 12, height = 8, dpi = 300)
    if (!is.null(network_plots$density)) {
      ggsave(file.path(output_dir, "network_density.png"), 
             network_plots$density, width = 10, height = 6, dpi = 300)
    }
  }
  
  # 3. Model Comparison Plots
  cat("Creating model comparison plots...\n")
  comparison_plots <- plot_model_comparison(analysis_results$benchmark_comparison)
  plots <- c(plots, comparison_plots)
  
  if (save_plots) {
    ggsave(file.path(output_dir, "model_comparison.png"), 
           comparison_plots$comparison, width = 14, height = 8, dpi = 300)
    ggsave(file.path(output_dir, "model_ranking.png"),
           comparison_plots$ranking, width = 10, height = 6, dpi = 300)
  }
  
  # 4. Wealth Evolution Plots
  cat("Creating wealth evolution plots...\n")
  wealth_plots <- plot_wealth_dynamics(analysis_results$simulation_results)
  plots <- c(plots, wealth_plots)
  
  if (save_plots) {
    ggsave(file.path(output_dir, "wealth_evolution_individual.png"), 
           wealth_plots$individual, width = 12, height = 8, dpi = 300)
    ggsave(file.path(output_dir, "wealth_evolution_by_type.png"), 
           wealth_plots$by_type, width = 12, height = 8, dpi = 300)
  }
  
  # 5. Decision Heatmap
  cat("Creating decision heatmap...\n")
  decision_heatmap <- plot_decision_heatmap(analysis_results$simulation_results)
  plots$decision_heatmap <- decision_heatmap
  
  if (save_plots) {
    ggsave(file.path(output_dir, "agent_decision_heatmap.png"), 
           decision_heatmap, width = 14, height = 10, dpi = 300)
  }
  
  # 6. Interactive Network (save as HTML)
  if (save_plots) {
    cat("Creating interactive network visualization...\n")
    tryCatch({
      final_network <- analysis_results$market$network
      agent_types <- analysis_results$simulation_results$agent_types
      
      interactive_network <- create_interactive_network(final_network, agent_types)
      saveNetwork(interactive_network, file.path(output_dir, "interactive_network.html"))
    }, error = function(e) {
      cat("Warning: Could not create interactive network visualization:", e$message, "\n")
    })
  }
  
  cat("Visualization report completed!\n")
  if (save_plots) {
    cat("Plots saved to:", output_dir, "\n")
  }
  
  return(plots)
}