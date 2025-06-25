# Test ANNEM Market functionality

test_that("ANNEMMarket components can be tested individually", {
  skip_on_cran()  # Skip on CRAN due to internet dependency
  
  # Test should expect error when no market data is available
  expect_error(ANNEMMarket$new(n_agents = 10, symbols = c("TEST")), 
               "No market data could be loaded")
  
  # Test agent creation function separately 
  agent <- create_annem_agent("test_001", "neural_momentum", 1000000)
  expect_s4_class(agent, "ANNEMAgent")
  expect_equal(agent$id, "test_001")
  expect_equal(agent$type, "neural_momentum")
})

test_that("Synthetic data generation works", {
  skip_on_cran()
  
  # Test synthetic data generation
  synthetic_data <- generate_synthetic_data(n_days = 50, n_assets = 2)
  
  expect_true(is.list(synthetic_data))
  expect_true("prices" %in% names(synthetic_data))
  expect_true("returns" %in% names(synthetic_data))
  expect_equal(nrow(synthetic_data$prices), 50)
  expect_equal(ncol(synthetic_data$prices), 2)
})

test_that("Agent performance analysis works without market", {
  skip_on_cran()
  
  # Create some agents manually
  agents <- list()
  for(i in 1:5) {
    agents[[i]] <- create_annem_agent(paste0("agent_", i), "neural_momentum", 1000000)
  }
  
  expect_equal(length(agents), 5)
  expect_true(all(sapply(agents, function(x) class(x)[1]) == "ANNEMAgent"))
})

test_that("Network evolution works independently", {
  skip_on_cran()
  
  # Create a simple test network
  test_network <- sample_smallworld(1, 10, 4, 0.1)
  
  # Test network metrics
  metrics <- calculate_network_metrics(test_network)
  
  expect_true(is.list(metrics))
  expect_true("n_nodes" %in% names(metrics))
  expect_true("n_edges" %in% names(metrics))
  expect_equal(metrics$n_nodes, 10)
})

test_that("Benchmark comparison works with synthetic data", {
  skip_on_cran()
  
  # Generate synthetic returns
  actual_returns <- rnorm(100, 0, 0.02)
  annem_predictions <- actual_returns + rnorm(100, 0, 0.01)
  
  # Test benchmark comparison
  comparison <- compare_with_benchmarks(annem_predictions, actual_returns)
  
  expect_true(is.list(comparison))
  expect_true("performance_summary" %in% names(comparison))
})

test_that("create_annem_market convenience function handles errors", {
  skip_on_cran()
  
  # Should fail gracefully with invalid symbols
  expect_error(create_annem_market(n_agents = 3, symbols = c("TEST")), 
               "No market data could be loaded")
})

test_that("Market handles different agent type distributions", {
  skip_on_cran()
  
  # Test agent type distribution without full market
  agent_types <- c("neural_momentum", "contrarian_ai", "fundamentalist_ml", 
                   "adaptive_noise", "social_network", "meta_learning")
  
  # Create agents with different types
  agents <- list()
  for(i in 1:6) {
    agents[[i]] <- create_annem_agent(paste0("agent_", i), agent_types[i], 1000000)
  }
  
  types <- sapply(agents, function(x) x$type)
  expect_equal(length(unique(types)), 6)
  expect_true(all(agent_types %in% types))
})

test_that("Wealth calculation works independently", {
  skip_on_cran()
  
  # Test wealth-related functions
  wealth_vector <- c(800000, 1000000, 1200000, 1500000, 2000000)
  gini <- calculate_gini_coefficient(wealth_vector)
  
  expect_true(is.numeric(gini))
  expect_true(gini >= 0 && gini <= 1)
  expect_true(gini > 0)  # Should have some inequality
})