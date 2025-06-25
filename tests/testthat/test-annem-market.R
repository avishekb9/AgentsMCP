# Test ANNEM Market functionality

test_that("ANNEMMarket can be created", {
  skip_on_cran()  # Skip on CRAN due to internet dependency
  
  # Use synthetic data to avoid internet dependency in testing
  market <- ANNEMMarket$new(n_agents = 10, symbols = c("TEST"))
  
  expect_equal(length(market$agents), 10)
  expect_true(is.list(market$agents))
  expect_s4_class(market$agents[[1]], "ANNEMAgent")
  expect_true(!is.null(market$network))
})

test_that("Market simulation can run", {
  skip_on_cran()
  
  # Create small market for testing
  market <- ANNEMMarket$new(n_agents = 5, symbols = c("TEST"))
  
  # Mock market data to avoid internet dependency
  market$market_data <- list(
    prices = matrix(100 * cumprod(1 + rnorm(50, 0, 0.02)), ncol = 1),
    returns = matrix(rnorm(49, 0, 0.02), ncol = 1),
    volatility = abs(rnorm(49, 0, 0.02)),
    symbols = "TEST"
  )
  market$sentiment_data <- rnorm(49, 0, 0.1)
  
  results <- market$run_simulation(n_steps = 10, verbose = FALSE)
  
  expect_true(is.list(results))
  expect_true("agent_decisions" %in% names(results))
  expect_true("agent_wealth" %in% names(results))
  expect_true("n_steps" %in% names(results))
  expect_true(results$n_steps <= 10)
})

test_that("Agent performance analysis works", {
  skip_on_cran()
  
  market <- ANNEMMarket$new(n_agents = 5, symbols = c("TEST"))
  
  # Mock data and run simulation
  market$market_data <- list(
    prices = matrix(100 * cumprod(1 + rnorm(30, 0, 0.02)), ncol = 1),
    returns = matrix(rnorm(29, 0, 0.02), ncol = 1),
    volatility = abs(rnorm(29, 0, 0.02)),
    symbols = "TEST"
  )
  market$sentiment_data <- rnorm(29, 0, 0.1)
  
  results <- market$run_simulation(n_steps = 10, verbose = FALSE)
  performance <- market$analyze_agent_performance()
  
  expect_true(is.data.frame(performance))
  expect_true("agent_type" %in% colnames(performance))
  expect_true("total_return" %in% colnames(performance))
  expect_true("final_wealth" %in% colnames(performance))
  expect_true("sharpe_ratio" %in% colnames(performance))
  expect_equal(nrow(performance), 5)  # One row per agent
})

test_that("Network evolution analysis works", {
  skip_on_cran()
  
  market <- ANNEMMarket$new(n_agents = 5, symbols = c("TEST"))
  
  # Mock data and run simulation
  market$market_data <- list(
    prices = matrix(100 * cumprod(1 + rnorm(30, 0, 0.02)), ncol = 1),
    returns = matrix(rnorm(29, 0, 0.02), ncol = 1),
    volatility = abs(rnorm(29, 0, 0.02)),
    symbols = "TEST"
  )
  market$sentiment_data <- rnorm(29, 0, 0.1)
  
  results <- market$run_simulation(n_steps = 10, verbose = FALSE)
  network_metrics <- market$analyze_network_evolution()
  
  expect_true(is.data.frame(network_metrics))
  expect_true("step" %in% colnames(network_metrics))
  expect_true("density" %in% colnames(network_metrics))
  expect_true("clustering" %in% colnames(network_metrics))
  expect_true(nrow(network_metrics) <= 10)
})

test_that("Benchmark comparison works", {
  skip_on_cran()
  
  market <- ANNEMMarket$new(n_agents = 5, symbols = c("TEST"))
  
  # Mock data and run simulation
  market$market_data <- list(
    prices = matrix(100 * cumprod(1 + rnorm(50, 0, 0.02)), ncol = 1),
    returns = matrix(rnorm(49, 0, 0.02), ncol = 1),
    volatility = abs(rnorm(49, 0, 0.02)),
    symbols = "TEST"
  )
  market$sentiment_data <- rnorm(49, 0, 0.1)
  
  results <- market$run_simulation(n_steps = 20, verbose = FALSE)
  comparison <- market$compare_with_benchmarks()
  
  expect_true(is.data.frame(comparison))
  expect_true("Model" %in% colnames(comparison))
  expect_true("MSE" %in% colnames(comparison))
  expect_true("MAE" %in% colnames(comparison))
  expect_true("Directional_Accuracy" %in% colnames(comparison))
  expect_true("ANNEM" %in% comparison$Model)
})

test_that("create_annem_market convenience function works", {
  market <- create_annem_market(n_agents = 3, symbols = c("TEST"))
  
  expect_equal(length(market$agents), 3)
  expect_s4_class(market, "ANNEMMarket")
})

test_that("Market handles different agent type distributions", {
  skip_on_cran()
  
  market <- ANNEMMarket$new(n_agents = 60, symbols = c("TEST"))  # Divisible by 6 for even distribution
  
  agent_types <- sapply(market$agents, function(a) a$type)
  type_counts <- table(agent_types)
  
  # Should have all 6 agent types
  expected_types <- c("neural_momentum", "contrarian_ai", "fundamentalist_ml", 
                     "adaptive_noise", "social_network", "meta_learning")
  
  expect_true(all(expected_types %in% names(type_counts)))
  
  # No type should be completely absent (with 60 agents, very unlikely)
  expect_true(all(type_counts > 0))
})

test_that("Market simulation produces sensible wealth evolution", {
  skip_on_cran()
  
  market <- ANNEMMarket$new(n_agents = 5, symbols = c("TEST"))
  
  # Create market data with known characteristics
  returns <- rnorm(30, 0.01, 0.02)  # Positive expected return
  prices <- matrix(100 * cumprod(1 + returns), ncol = 1)
  
  market$market_data <- list(
    prices = prices,
    returns = matrix(returns[-1], ncol = 1),
    volatility = abs(returns[-1]),
    symbols = "TEST"
  )
  market$sentiment_data <- rnorm(length(returns)-1, 0, 0.1)
  
  results <- market$run_simulation(n_steps = 15, verbose = FALSE)
  
  # Check that wealth matrix has correct dimensions
  expect_equal(nrow(results$agent_wealth), results$n_steps)
  expect_equal(ncol(results$agent_wealth), 5)  # Number of agents
  
  # Initial wealth should be around 1M for all agents
  initial_wealth <- results$agent_wealth[1, ]
  expect_true(all(initial_wealth >= 900000 & initial_wealth <= 1100000))
})