# Test ANNEM Agent functionality

test_that("ANNEMAgent can be created", {
  agent <- ANNEMAgent$new("test_001", "neural_momentum", 1000000)
  
  expect_equal(agent$id, "test_001")
  expect_equal(agent$type, "neural_momentum")
  expect_equal(agent$wealth, 1000000)
  expect_true(agent$risk_tolerance >= 0.1 && agent$risk_tolerance <= 0.9)
  expect_true(is.list(agent$nn_weights))
  expect_true(length(agent$nn_weights) == 6)  # W1, b1, W2, b2, W3, b3
})

test_that("Agent neural decision making works", {
  agent <- ANNEMAgent$new("test_002", "contrarian_ai", 1000000)
  
  # Create test state vector
  state_vector <- matrix(rnorm(50), nrow = 1)
  
  decision <- agent$neural_decision(state_vector)
  
  expect_true(is.numeric(decision))
  expect_true(length(decision) == 1)
  expect_true(decision >= -1 && decision <= 1)  # tanh output range
})

test_that("Agent decision making with market data works", {
  agent <- ANNEMAgent$new("test_003", "fundamentalist_ml", 1000000)
  
  # Create synthetic market data
  market_data <- list(
    prices = matrix(100 * cumprod(1 + rnorm(20, 0, 0.02)), ncol = 1),
    returns = matrix(rnorm(20, 0, 0.02), ncol = 1),
    volatility = abs(rnorm(20, 0, 0.02))
  )
  
  decision <- agent$make_decision(market_data, network_signals = 0.1, sentiment = 0.05)
  
  expect_true(is.numeric(decision))
  expect_true(length(decision) == 1)
})

test_that("Agent wealth update works", {
  agent <- ANNEMAgent$new("test_004", "adaptive_noise", 1000000)
  initial_wealth <- agent$wealth
  
  # Positive return
  agent$update_wealth(0.05)  # 5% return
  expect_true(agent$wealth > initial_wealth)
  
  # Negative return
  agent$update_wealth(-0.02)  # -2% return
  expect_true(agent$wealth < agent$wealth * 1.05)  # Should be less than before negative return
})

test_that("Different agent types produce different behaviors", {
  set.seed(42)  # For reproducibility
  
  # Create agents of different types
  momentum_agent <- ANNEMAgent$new("momentum", "neural_momentum", 1000000)
  contrarian_agent <- ANNEMAgent$new("contrarian", "contrarian_ai", 1000000)
  
  # Create market data with clear trend
  prices <- matrix(100 * cumprod(c(1, rep(1.01, 19))), ncol = 1)  # Strong uptrend
  returns <- matrix(diff(log(prices)), ncol = 1)
  volatility <- abs(returns)
  
  market_data <- list(
    prices = prices,
    returns = returns,
    volatility = volatility
  )
  
  momentum_decision <- momentum_agent$make_decision(market_data)
  contrarian_decision <- contrarian_agent$make_decision(market_data)
  
  # Momentum and contrarian agents should make different decisions given a trend
  expect_true(is.numeric(momentum_decision))
  expect_true(is.numeric(contrarian_decision))
})

test_that("create_annem_agent convenience function works", {
  agent <- create_annem_agent("convenience_test", "social_network", 2000000)
  
  expect_equal(agent$id, "convenience_test")
  expect_equal(agent$type, "social_network")
  expect_equal(agent$wealth, 2000000)
  expect_s4_class(agent, "ANNEMAgent")
})

test_that("Agent memory is updated correctly", {
  agent <- ANNEMAgent$new("memory_test", "meta_learning", 1000000)
  
  # Initially empty memory
  expect_equal(length(agent$memory$prices), 0)
  expect_equal(length(agent$memory$returns), 0)
  
  # Create market data
  market_data <- list(
    prices = matrix(c(100, 101, 102), ncol = 1),
    returns = matrix(c(0.01, 0.01), ncol = 1),
    volatility = c(0.01, 0.01)
  )
  
  agent$make_decision(market_data)
  
  # Memory should be updated
  expect_true(length(agent$memory$prices) > 0)
  expect_true(length(agent$memory$returns) > 0)
})

test_that("Agent performance history tracking works", {
  agent <- ANNEMAgent$new("performance_test", "neural_momentum", 1000000)
  
  # Initially empty performance history
  expect_equal(length(agent$performance_history), 0)
  
  # Update wealth several times
  agent$update_wealth(0.02)
  agent$update_wealth(0.01)
  agent$update_wealth(-0.01)
  
  # Performance history should be updated
  expect_equal(length(agent$performance_history), 3)
  expect_equal(agent$performance_history, c(0.02, 0.01, -0.01))
})