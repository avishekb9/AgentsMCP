#' Market Data Functions for ANNEM
#'
#' @description
#' Functions for loading, processing, and managing financial market data
#' for ANNEM simulations. Includes data retrieval from various sources,
#' return calculations, and market sentiment processing.

#' Load Market Data
#'
#' Downloads and processes daily stock market data for ANNEM analysis.
#' Uses quantmod to retrieve data from Yahoo Finance.
#'
#' @param symbols Character vector of stock symbols (e.g., c("AAPL", "MSFT"))
#' @param period Character string specifying time period (default: "2y")
#' @param from Date to start data collection (optional)
#' @param to Date to end data collection (optional)
#'
#' @return List containing:
#' \itemize{
#'   \item prices: Matrix of adjusted close prices
#'   \item returns: Matrix of log returns
#'   \item volatility: Vector of rolling volatility measures
#' }
#'
#' @examples
#' \dontrun{
#' data <- load_market_data(c("AAPL", "MSFT", "GOOGL"))
#' head(data$prices)
#' head(data$returns)
#' }
#'
#' @export
load_market_data <- function(symbols, period = "2y", from = NULL, to = NULL) {
  
  # Set default date range if not provided
  if (is.null(from)) from <- Sys.Date() - 730  # 2 years ago
  if (is.null(to)) to <- Sys.Date()
  
  cat("Loading market data for symbols:", paste(symbols, collapse = ", "), "\n")
  
  data_list <- list()
  failed_symbols <- character()
  
  for (symbol in symbols) {
    tryCatch({
      data_list[[symbol]] <- getSymbols(symbol, src = "yahoo", 
                                       from = from, 
                                       to = to, 
                                       auto.assign = FALSE)
    }, error = function(e) {
      cat("Warning: Could not load data for", symbol, "\n")
      failed_symbols <<- c(failed_symbols, symbol)
    })
  }
  
  if (length(data_list) == 0) {
    stop("No market data could be loaded! Check internet connection and symbol validity.")
  }
  
  if (length(failed_symbols) > 0) {
    cat("Failed to load data for:", paste(failed_symbols, collapse = ", "), "\n")
  }
  
  # Extract adjusted close prices
  prices <- do.call(cbind, lapply(data_list, function(x) Ad(x)))
  colnames(prices) <- names(data_list)
  
  # Remove NA values
  prices <- na.omit(prices)
  
  if (nrow(prices) < 50) {
    warning("Limited data available (", nrow(prices), " days). Results may be unreliable.")
  }
  
  # Calculate returns
  returns <- diff(log(prices))
  returns <- na.omit(returns)
  
  # Calculate rolling volatility
  volatility <- apply(returns, 1, function(x) sqrt(sum(x^2)))
  
  cat("Loaded", nrow(prices), "days of data for", ncol(prices), "assets\n")
  
  return(list(
    prices = as.matrix(prices),
    returns = as.matrix(returns),
    volatility = as.numeric(volatility),
    symbols = names(data_list),
    date_range = c(min(index(prices)), max(index(prices)))
  ))
}

#' Process Market Returns
#'
#' Advanced processing of market return data including outlier detection,
#' volatility clustering analysis, and regime identification.
#'
#' @param returns Matrix of asset returns
#' @param method Character string specifying processing method ("standard", "robust", "garch")
#' @param window_size Integer, rolling window size for calculations (default: 20)
#'
#' @return List containing processed returns and additional metrics
#'
#' @examples
#' \dontrun{
#' data <- load_market_data(c("AAPL", "MSFT"))
#' processed <- process_market_returns(data$returns, method = "robust")
#' }
#'
#' @export
process_market_returns <- function(returns, method = "standard", window_size = 20) {
  
  if (method == "robust") {
    # Robust processing with outlier detection
    outliers <- apply(returns, 2, function(x) {
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      return(x < lower | x > upper)
    })
    
    # Winsorize outliers
    processed_returns <- returns
    for (i in 1:ncol(returns)) {
      outlier_idx <- outliers[, i]
      if (any(outlier_idx)) {
        Q1 <- quantile(returns[, i], 0.05, na.rm = TRUE)
        Q99 <- quantile(returns[, i], 0.95, na.rm = TRUE)
        processed_returns[outlier_idx, i] <- pmax(Q1, pmin(Q99, returns[outlier_idx, i]))
      }
    }
    
  } else if (method == "garch") {
    # GARCH-based processing
    processed_returns <- returns
    volatility_forecasts <- matrix(0, nrow(returns), ncol(returns))
    
    for (i in 1:ncol(returns)) {
      tryCatch({
        spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0)))
        fit <- ugarchfit(spec, returns[, i])
        volatility_forecasts[, i] <- sigma(fit)
      }, error = function(e) {
        # Fallback to rolling standard deviation
        volatility_forecasts[, i] <<- rollapply(returns[, i], window_size, sd, fill = sd(returns[, i]))
      })
    }
    
  } else {
    # Standard processing
    processed_returns <- returns
    volatility_forecasts <- NULL
  }
  
  # Calculate additional metrics
  rolling_correlation <- array(0, c(nrow(returns) - window_size + 1, ncol(returns), ncol(returns)))
  
  for (t in window_size:nrow(returns)) {
    window_data <- returns[(t - window_size + 1):t, ]
    rolling_correlation[t - window_size + 1, , ] <- cor(window_data, use = "complete.obs")
  }
  
  return(list(
    returns = processed_returns,
    volatility_forecasts = volatility_forecasts,
    rolling_correlation = rolling_correlation,
    outlier_proportion = if (exists("outliers")) colMeans(outliers) else NULL,
    processing_method = method
  ))
}

#' Calculate Market Sentiment
#'
#' Compute market sentiment scores based on return patterns, volatility,
#' and optional news data integration.
#'
#' @param returns Matrix of asset returns
#' @param volatility Vector of volatility measures
#' @param news_data Optional data frame with news sentiment (default: NULL)
#' @param method Character string for sentiment calculation method ("technical", "news", "hybrid")
#'
#' @return Numeric vector of sentiment scores [-1, 1]
#'
#' @examples
#' \dontrun{
#' data <- load_market_data(c("AAPL", "MSFT"))
#' sentiment <- calculate_market_sentiment(data$returns, data$volatility)
#' plot(sentiment, type = "l", main = "Market Sentiment Over Time")
#' }
#'
#' @export
calculate_market_sentiment <- function(returns, volatility, news_data = NULL, method = "technical") {
  
  n_periods <- nrow(returns)
  sentiment_scores <- numeric(n_periods)
  
  if (method == "technical" || method == "hybrid") {
    # Technical sentiment based on returns and volatility
    
    # Price momentum component
    momentum_window <- 5
    momentum_scores <- numeric(n_periods)
    
    for (t in momentum_window:n_periods) {
      recent_returns <- returns[(t - momentum_window + 1):t, ]
      avg_return <- mean(recent_returns)
      momentum_scores[t] <- tanh(avg_return * 50)  # Scale to [-1, 1]
    }
    
    # Volatility component (high volatility = negative sentiment)
    vol_scores <- -tanh((volatility - median(volatility)) / mad(volatility))
    
    # Combine components
    technical_sentiment <- 0.7 * momentum_scores + 0.3 * vol_scores
    sentiment_scores <- technical_sentiment
  }
  
  if (method == "news" && !is.null(news_data)) {
    # News-based sentiment (placeholder implementation)
    # In practice, this would integrate with news sentiment APIs
    news_sentiment <- rnorm(n_periods, 0, 0.2)
    sentiment_scores <- news_sentiment
  }
  
  if (method == "hybrid" && !is.null(news_data)) {
    # Combine technical and news sentiment
    news_sentiment <- rnorm(n_periods, 0, 0.2)
    sentiment_scores <- 0.6 * technical_sentiment + 0.4 * news_sentiment
  }
  
  # Ensure scores are in [-1, 1] range
  sentiment_scores <- pmax(-1, pmin(1, sentiment_scores))
  
  return(sentiment_scores)
}

#' Generate Synthetic Market Data
#'
#' Create synthetic market data for testing and simulation purposes.
#' Useful when real market data is unavailable or for controlled experiments.
#'
#' @param n_days Number of trading days to simulate
#' @param n_assets Number of assets to simulate
#' @param initial_price Initial price for all assets (default: 100)
#' @param annual_return Expected annual return (default: 0.08)
#' @param annual_volatility Annual volatility (default: 0.20)
#' @param correlation_matrix Optional correlation matrix between assets
#'
#' @return List with same structure as load_market_data()
#'
#' @examples
#' \dontrun{
#' synthetic_data <- generate_synthetic_data(n_days = 252, n_assets = 3)
#' plot(synthetic_data$prices[, 1], type = "l", main = "Synthetic Price Series")
#' }
#'
#' @export
generate_synthetic_data <- function(n_days = 252, n_assets = 1, initial_price = 100,
                                   annual_return = 0.08, annual_volatility = 0.20,
                                   correlation_matrix = NULL) {
  
  # Convert annual parameters to daily
  daily_return <- annual_return / 252
  daily_volatility <- annual_volatility / sqrt(252)
  
  # Generate correlated returns if correlation matrix provided
  if (!is.null(correlation_matrix) && n_assets > 1) {
    if (nrow(correlation_matrix) != n_assets || ncol(correlation_matrix) != n_assets) {
      stop("Correlation matrix dimensions must match number of assets")
    }
    
    # Cholesky decomposition for correlated random variables
    L <- chol(correlation_matrix)
    random_matrix <- matrix(rnorm(n_days * n_assets), n_days, n_assets)
    correlated_returns <- random_matrix %*% L * daily_volatility + daily_return
    
  } else {
    # Independent returns
    correlated_returns <- matrix(rnorm(n_days * n_assets, daily_return, daily_volatility),
                                n_days, n_assets)
  }
  
  # Generate price paths
  prices <- matrix(0, n_days + 1, n_assets)
  prices[1, ] <- initial_price
  
  for (t in 2:(n_days + 1)) {
    prices[t, ] <- prices[t-1, ] * exp(correlated_returns[t-1, ])
  }
  
  # Remove first row (initial prices)
  prices <- prices[-1, ]
  
  # Calculate returns and volatility
  returns <- diff(log(rbind(initial_price, prices)))
  volatility <- apply(returns, 1, function(x) sqrt(sum(x^2)))
  
  # Add column names
  colnames(prices) <- paste0("Asset_", 1:n_assets)
  colnames(returns) <- paste0("Asset_", 1:n_assets)
  
  cat("Generated", n_days, "days of synthetic data for", n_assets, "assets\n")
  
  return(list(
    prices = prices,
    returns = returns,
    volatility = volatility,
    symbols = colnames(prices),
    date_range = c(Sys.Date() - n_days, Sys.Date()),
    generation_parameters = list(
      annual_return = annual_return,
      annual_volatility = annual_volatility,
      correlation_matrix = correlation_matrix
    )
  ))
}