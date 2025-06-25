#' Agentic Neural Network Economic Model Agent Class
#'
#' @description
#' Implementation of heterogeneous agents with neural decision-making capabilities
#' for the ANNEM framework. Each agent has a specific type, neural network weights,
#' memory, and performance tracking.
#'
#' @details
#' The ANNEMAgent class implements six different agent types as described in the
#' mathematical framework:
#' \itemize{
#'   \item neural_momentum: Trend-following with neural enhancement
#'   \item contrarian_ai: Mean-reversion with AI signals
#'   \item fundamentalist_ml: Technical analysis with machine learning
#'   \item adaptive_noise: Random strategy with adaptive learning
#'   \item social_network: Peer influence and herding behavior
#'   \item meta_learning: MAML-inspired strategy adaptation
#' }
#'
#' @field id Character string identifying the agent
#' @field type Agent type (one of six types)
#' @field wealth Current wealth of the agent
#' @field risk_tolerance Risk tolerance parameter (0,1)
#' @field nn_weights List containing neural network weights
#' @field memory List containing agent's memory of past observations
#' @field connections Numeric vector of connected agents
#' @field performance_history Numeric vector of past performance
#'
#' @examples
#' \dontrun{
#' # Create a neural momentum agent
#' agent <- ANNEMAgent$new("agent_001", "neural_momentum", 1000000)
#' 
#' # Make a decision based on market data
#' decision <- agent$make_decision(market_data, network_signals = 0.1, sentiment = 0.05)
#' 
#' # Update wealth based on portfolio return
#' agent$update_wealth(0.02)  # 2% return
#' }
#'
#' @export
ANNEMAgent <- setRefClass("ANNEMAgent",
  fields = list(
    id = "character",
    type = "character", 
    wealth = "numeric",
    risk_tolerance = "numeric",
    nn_weights = "list",
    memory = "list",
    connections = "numeric",
    performance_history = "numeric"
  ),
  
  methods = list(
    #' Initialize Agent
    #' 
    #' @param agent_id Character string for agent ID
    #' @param agent_type Agent type (neural_momentum, contrarian_ai, etc.)
    #' @param initial_wealth Initial wealth amount (default: 1,000,000)
    initialize = function(agent_id, agent_type, initial_wealth = 1000000) {
      "Initialize a new ANNEM agent with specified parameters"
      
      id <<- agent_id
      type <<- agent_type
      wealth <<- initial_wealth
      risk_tolerance <<- runif(1, 0.1, 0.9)
      
      # Initialize neural network weights (simplified 3-layer)
      nn_weights <<- list(
        W1 = matrix(rnorm(50 * 128, 0, 0.1), 50, 128),  # Input to hidden
        b1 = rnorm(128, 0, 0.1),
        W2 = matrix(rnorm(128 * 64, 0, 0.1), 128, 64),  # Hidden to hidden  
        b2 = rnorm(64, 0, 0.1),
        W3 = matrix(rnorm(64 * 1, 0, 0.1), 64, 1),      # Hidden to output
        b3 = rnorm(1, 0, 0.1)
      )
      
      memory <<- list(
        prices = numeric(0),
        returns = numeric(0),
        volatility = numeric(0),
        sentiment = numeric(0),
        network_signals = numeric(0)
      )
      
      connections <<- numeric(0)
      performance_history <<- numeric(0)
    },
    
    #' Neural Network Forward Pass
    #' 
    #' @param state_vector Numeric vector of input features
    #' @return Numeric decision value in range (-1, 1)
    neural_decision = function(state_vector) {
      "Forward propagation through neural network with ReLU activation"
      
      # Forward propagation with ReLU activation
      h1 <- pmax(0, state_vector %*% nn_weights$W1 + nn_weights$b1)
      h2 <- pmax(0, h1 %*% nn_weights$W2 + nn_weights$b2)
      output <- tanh(h2 %*% nn_weights$W3 + nn_weights$b3)  # Output in [-1, 1]
      
      return(as.numeric(output))
    },
    
    #' Make Trading Decision
    #' 
    #' @param market_data List containing prices, returns, volatility
    #' @param network_signals Numeric, signals from connected agents
    #' @param sentiment Numeric, market sentiment score
    #' @return Numeric decision value (negative = sell, positive = buy)
    make_decision = function(market_data, network_signals = 0, sentiment = 0) {
      "Generate trading decision based on agent type and neural network"
      
      # Prepare state vector
      prices <- tail(market_data$prices, 10)
      returns <- tail(market_data$returns, 10)
      volatility <- tail(market_data$volatility, 5)
      
      # Pad with zeros if not enough data
      if (length(prices) < 10) prices <- c(rep(0, 10 - length(prices)), prices)
      if (length(returns) < 10) returns <- c(rep(0, 10 - length(returns)), returns)
      if (length(volatility) < 5) volatility <- c(rep(0, 5 - length(volatility)), volatility)
      
      state_vector <- c(
        scale(prices)[1:10],           # Normalized prices
        returns,                       # Returns  
        volatility,                    # Volatility measures
        network_signals,               # Network connectivity features
        sentiment,                     # Market sentiment
        risk_tolerance,                # Agent risk tolerance
        rep(0, 22)                     # Padding to reach 50 dimensions
      )
      
      # Base neural decision
      base_decision <- neural_decision(matrix(state_vector, nrow = 1))
      
      # Type-specific modifications
      final_decision <- switch(type,
        "neural_momentum" = apply_momentum_strategy(base_decision, returns),
        "contrarian_ai" = apply_contrarian_strategy(base_decision, returns),
        "fundamentalist_ml" = apply_fundamental_strategy(base_decision, prices),
        "adaptive_noise" = apply_noise_strategy(base_decision),
        "social_network" = apply_social_strategy(base_decision, network_signals),
        "meta_learning" = apply_meta_strategy(base_decision, performance_history),
        base_decision
      )
      
      # Update memory
      memory$prices <<- c(tail(memory$prices, 99), tail(prices, 1))
      memory$returns <<- c(tail(memory$returns, 99), tail(returns, 1))
      memory$network_signals <<- c(tail(memory$network_signals, 99), network_signals)
      
      return(final_decision)
    },
    
    #' Update Agent Wealth
    #' 
    #' @param portfolio_return Numeric, portfolio return for this period
    update_wealth = function(portfolio_return) {
      "Update agent wealth based on portfolio return and perform learning"
      
      old_wealth <- wealth
      wealth <<- wealth * (1 + portfolio_return)
      performance_history <<- c(tail(performance_history, 99), portfolio_return)
      
      # Simple learning: adjust weights based on performance
      if (length(performance_history) > 1) {
        performance_gradient <- tail(performance_history, 1)
        learning_rate <- 0.001
        
        # Update neural network weights (simplified)
        nn_weights$W3 <<- nn_weights$W3 + learning_rate * performance_gradient * 
                          rnorm(length(nn_weights$W3), 0, 0.01)
        nn_weights$b3 <<- nn_weights$b3 + learning_rate * performance_gradient * 
                          rnorm(length(nn_weights$b3), 0, 0.01)
      }
    }
  )
)

#' Create ANNEM Agent
#' 
#' Convenience function to create a new ANNEM agent with specified parameters.
#' 
#' @param agent_id Character string for agent identifier
#' @param agent_type Agent type (neural_momentum, contrarian_ai, fundamentalist_ml, 
#'   adaptive_noise, social_network, meta_learning)
#' @param initial_wealth Initial wealth amount (default: 1,000,000)
#' 
#' @return ANNEMAgent reference class object
#' 
#' @examples
#' \dontrun{
#' agent <- create_annem_agent("agent_001", "neural_momentum", 1500000)
#' }
#' 
#' @export
create_annem_agent <- function(agent_id, agent_type, initial_wealth = 1000000) {
  return(ANNEMAgent$new(agent_id, agent_type, initial_wealth))
}

# Agent strategy implementations
apply_momentum_strategy <- function(base_decision, returns) {
  if (length(returns) >= 3) {
    momentum <- mean(tail(returns, 3))
    return(base_decision * 0.7 + tanh(momentum * 10) * 0.3)
  }
  return(base_decision)
}

apply_contrarian_strategy <- function(base_decision, returns) {
  if (length(returns) >= 5) {
    mean_return <- mean(tail(returns, 5))
    return(base_decision * 0.6 - tanh(mean_return * 10) * 0.4)
  }
  return(base_decision)
}

apply_fundamental_strategy <- function(base_decision, prices) {
  if (length(prices) >= 20) {
    sma_20 <- mean(tail(prices, 20))
    current_price <- tail(prices, 1)
    deviation <- (current_price - sma_20) / sma_20
    return(base_decision * 0.8 + tanh(-deviation * 5) * 0.2)
  }
  return(base_decision)
}

apply_noise_strategy <- function(base_decision) {
  noise <- rnorm(1, 0, 0.1)
  return(base_decision + noise)
}

apply_social_strategy <- function(base_decision, network_signals) {
  social_influence <- 0.3
  return(base_decision * (1 - social_influence) + network_signals * social_influence)
}

apply_meta_strategy <- function(base_decision, performance_history) {
  if (length(performance_history) >= 10) {
    recent_performance <- mean(tail(performance_history, 10))
    confidence <- pmax(0.1, pmin(0.9, 0.5 + recent_performance * 2))
    return(base_decision * confidence)
  }
  return(base_decision)
}