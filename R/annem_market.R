#' ANNEM Market Environment Class
#'
#' @description
#' Main class for running ANNEM simulations. Manages agents, networks, market data,
#' and simulation execution. Provides comprehensive analysis and validation capabilities.
#'
#' @details
#' The ANNEMMarket class orchestrates the entire simulation environment including:
#' \itemize{
#'   \item Agent population management
#'   \item Dynamic network evolution
#'   \item Market data integration
#'   \item Sentiment analysis
#'   \item Performance tracking and validation
#' }
#'
#' @field agents List of ANNEMAgent objects
#' @field market_data List containing prices, returns, volatility data
#' @field network igraph network object representing agent connections
#' @field sentiment_data Numeric vector of sentiment scores
#' @field simulation_results List containing simulation outputs
#'
#' @examples
#' \dontrun{
#' # Create market with 100 agents
#' market <- ANNEMMarket$new(n_agents = 100, symbols = c("AAPL", "MSFT"))
#' 
#' # Run simulation
#' results <- market$run_simulation(n_steps = 50)
#' 
#' # Analyze performance
#' performance <- market$analyze_agent_performance()
#' }
#'
#' @export
ANNEMMarket <- setRefClass("ANNEMMarket",
  fields = list(
    agents = "list",
    market_data = "list", 
    network = "ANY",
    sentiment_data = "numeric",
    simulation_results = "list"
  ),
  
  methods = list(
    #' Initialize Market Environment
    #' 
    #' @param n_agents Number of agents (default: 1000)
    #' @param symbols Character vector of stock symbols
    initialize = function(n_agents = 1000, symbols = c("AAPL", "GOOGL", "MSFT", "TSLA", "NVDA")) {
      "Initialize ANNEM market environment with agents and data"
      
      cat("Initializing ANNEM Market Environment...\n")
      
      # Initialize agents with different types
      agent_types <- c("neural_momentum", "contrarian_ai", "fundamentalist_ml", 
                      "adaptive_noise", "social_network", "meta_learning")
      type_probs <- c(0.20, 0.15, 0.18, 0.12, 0.25, 0.10)
      
      agents <<- list()
      for (i in 1:n_agents) {
        agent_type <- sample(agent_types, 1, prob = type_probs)
        agent_id <- paste0("agent_", i)
        agents[[i]] <<- ANNEMAgent$new(agent_id, agent_type)
      }
      
      # Initialize network (small-world network)
      network <<- sample_smallworld(1, n_agents, 6, 0.1)
      
      # Load market data
      market_data <<- load_market_data(symbols)
      
      # Initialize sentiment (random for now, could be news-based)
      sentiment_data <<- rnorm(nrow(market_data$returns), 0, 0.1)
      
      simulation_results <<- list()
      
      cat("ANNEM Market initialized with", n_agents, "agents\n")
    },
    
    #' Run ANNEM Simulation
    #' 
    #' @param n_steps Number of simulation steps (default: 250)
    #' @param verbose Logical, whether to print progress (default: TRUE)
    #' @return List containing simulation results
    run_simulation = function(n_steps = 250, verbose = TRUE) {
      "Execute complete ANNEM simulation with agents, networks, and markets"
      
      if (verbose) cat("Running ANNEM simulation for", n_steps, "steps...\n")
      
      n_agents <- length(agents)
      n_assets <- ncol(market_data$returns)
      
      # Initialize tracking variables
      agent_decisions <- matrix(0, n_steps, n_agents)
      agent_wealth <- matrix(0, n_steps, n_agents)
      market_prices <- matrix(0, n_steps, n_assets)
      network_evolution <- array(0, c(n_steps, n_agents, n_agents))
      
      # Get initial wealth
      for (i in 1:n_agents) {
        agent_wealth[1, i] <- agents[[i]]$wealth
      }
      
      for (t in 2:min(n_steps, nrow(market_data$returns))) {
        if (verbose && t %% 50 == 0) cat("Step", t, "of", n_steps, "\n")
        
        # Get current market state
        current_data <- list(
          prices = market_data$prices[1:t, ],
          returns = market_data$returns[1:t, ],
          volatility = market_data$volatility[1:t]
        )
        
        # Calculate network signals for each agent
        adj_matrix <- as_adjacency_matrix(network, sparse = FALSE)
        network_signals <- numeric(n_agents)
        
        for (i in 1:n_agents) {
          neighbors <- which(adj_matrix[i, ] == 1)
          if (length(neighbors) > 0) {
            # Average decision of neighbors (from previous step)
            if (t > 2) {
              network_signals[i] <- mean(agent_decisions[t-1, neighbors])
            }
          }
        }
        
        # Get agent decisions
        for (i in 1:n_agents) {
          agent_decisions[t, i] <- agents[[i]]$make_decision(
            current_data, 
            network_signals[i], 
            sentiment_data[t]
          )
          
          # Calculate portfolio return (simplified)
          portfolio_return <- sum(agent_decisions[t, i] * market_data$returns[t, ]) / n_assets
          agents[[i]]$update_wealth(portfolio_return)
          agent_wealth[t, i] <- agents[[i]]$wealth
        }
        
        # Update market prices (influenced by agent decisions)
        market_impact <- colMeans(matrix(agent_decisions[t, ], n_agents, n_assets))
        market_prices[t, ] <- market_data$prices[t, ] * (1 + market_impact * 0.01)
        
        # Evolve network (simplified)
        if (t %% 10 == 0) {
          network <<- evolve_network(network, agent_decisions[t, ])
        }
        network_evolution[t, , ] <- as_adjacency_matrix(network, sparse = FALSE)
      }
      
      # Store results
      simulation_results <<- list(
        agent_decisions = agent_decisions[1:t, ],
        agent_wealth = agent_wealth[1:t, ],
        market_prices = market_prices[1:t, ],
        network_evolution = network_evolution[1:t, , ],
        actual_returns = market_data$returns[1:t, ],
        n_steps = t,
        agent_types = sapply(agents, function(a) a$type)
      )
      
      if (verbose) cat("Simulation completed!\n")
      return(simulation_results)
    },
    
    #' Analyze Agent Performance
    #' 
    #' @return Data frame with agent performance metrics
    analyze_agent_performance = function() {
      "Calculate comprehensive performance metrics for all agents"
      
      if (is.null(simulation_results)) {
        stop("Run simulation first!")
      }
      
      agent_types <- simulation_results$agent_types
      final_wealth <- simulation_results$agent_wealth[nrow(simulation_results$agent_wealth), ]
      initial_wealth <- simulation_results$agent_wealth[1, ]
      total_returns <- (final_wealth - initial_wealth) / initial_wealth
      
      # Performance by type
      performance_by_type <- data.frame(
        agent_type = agent_types,
        total_return = total_returns,
        final_wealth = final_wealth,
        sharpe_ratio = calculate_sharpe_ratios(),
        stringsAsFactors = FALSE
      )
      
      return(performance_by_type)
    },
    
    #' Calculate Sharpe Ratios
    #' 
    #' @return Numeric vector of Sharpe ratios for each agent
    calculate_sharpe_ratios = function() {
      "Calculate annualized Sharpe ratios for all agents"
      
      wealth_data <- simulation_results$agent_wealth
      returns_matrix <- apply(wealth_data, 2, function(x) diff(log(x)))
      
      sharpe_ratios <- apply(returns_matrix, 2, function(x) {
        if (sd(x) > 0) {
          return(mean(x) / sd(x) * sqrt(252))  # Annualized
        } else {
          return(0)
        }
      })
      
      return(sharpe_ratios)
    },
    
    #' Analyze Network Evolution
    #' 
    #' @return Data frame with network evolution metrics
    analyze_network_evolution = function() {
      "Calculate network topology metrics over simulation time"
      
      if (is.null(simulation_results)) {
        stop("Run simulation first!")
      }
      
      n_steps <- simulation_results$n_steps
      network_metrics <- data.frame(
        step = 1:n_steps,
        density = numeric(n_steps),
        clustering = numeric(n_steps),
        avg_path_length = numeric(n_steps)
      )
      
      for (t in 1:n_steps) {
        g <- graph_from_adjacency_matrix(simulation_results$network_evolution[t, , ], mode = "undirected")
        
        network_metrics$density[t] <- edge_density(g)
        network_metrics$clustering[t] <- transitivity(g, type = "global")
        
        if (is_connected(g)) {
          network_metrics$avg_path_length[t] <- average.path.length(g)
        } else {
          network_metrics$avg_path_length[t] <- NA
        }
      }
      
      return(network_metrics)
    },
    
    #' Compare with Benchmark Models
    #' 
    #' @return Data frame with model comparison results
    compare_with_benchmarks = function() {
      "Compare ANNEM performance with VAR and Random Walk models"
      
      if (is.null(simulation_results)) {
        stop("Run simulation first!")
      }
      
      actual_returns <- simulation_results$actual_returns
      
      # ANNEM predictions (aggregate agent decisions)
      annem_predictions <- rowMeans(simulation_results$agent_decisions)
      
      # Benchmark models
      var_model <- fit_var_model(actual_returns)
      random_walk <- rnorm(nrow(actual_returns), 0, sd(actual_returns[, 1]))
      
      # Calculate performance metrics
      results <- data.frame(
        Model = c("ANNEM", "VAR", "Random_Walk"),
        MSE = c(
          calculate_mse(annem_predictions, actual_returns[, 1]),
          calculate_mse(var_model, actual_returns[, 1]),
          calculate_mse(random_walk, actual_returns[, 1])
        ),
        MAE = c(
          calculate_mae(annem_predictions, actual_returns[, 1]),
          calculate_mae(var_model, actual_returns[, 1]),
          calculate_mae(random_walk, actual_returns[, 1])
        ),
        Directional_Accuracy = c(
          calculate_directional_accuracy(annem_predictions, actual_returns[, 1]),
          calculate_directional_accuracy(var_model, actual_returns[, 1]),
          calculate_directional_accuracy(random_walk, actual_returns[, 1])
        ),
        stringsAsFactors = FALSE
      )
      
      return(results)
    }
  )
)

#' Create ANNEM Market
#' 
#' Convenience function to create a new ANNEM market environment.
#' 
#' @param n_agents Number of agents to create (default: 1000)
#' @param symbols Character vector of stock symbols to analyze
#' 
#' @return ANNEMMarket reference class object
#' 
#' @examples
#' \dontrun{
#' market <- create_annem_market(n_agents = 500, symbols = c("AAPL", "MSFT"))
#' }
#' 
#' @export
create_annem_market <- function(n_agents = 1000, symbols = c("AAPL", "GOOGL", "MSFT", "TSLA", "NVDA")) {
  return(ANNEMMarket$new(n_agents = n_agents, symbols = symbols))
}