#' AgentsMCP: Agentic Neural Network Economic Model with MCP Communication
#'
#' @description
#' A comprehensive implementation of the Agentic Neural Network Economic Model (ANNEM), 
#' featuring heterogeneous AI agents with neural decision-making capabilities, Model Context Protocol 
#' (MCP) communication, dynamic network formation, and empirical validation using daily stock market data.
#'
#' @details
#' The AgentsMCP package provides tools for agent-based economic modeling, financial network analysis, 
#' and performance comparison with traditional econometric models including DSGE and VAR frameworks.
#'
#' ## Key Features
#'
#' ### Agent Types
#' The package implements six heterogeneous agent types:
#' \itemize{
#'   \item \strong{Neural Momentum}: Trend-following agents with neural network enhancement
#'   \item \strong{Contrarian AI}: Mean-reversion agents with AI-based signals
#'   \item \strong{Fundamentalist ML}: Technical analysis agents with machine learning
#'   \item \strong{Adaptive Noise}: Random strategy agents with adaptive learning
#'   \item \strong{Social Network}: Peer influence and herding behavior agents
#'   \item \strong{Meta Learning}: MAML-inspired strategy adaptation agents
#' }
#'
#' ### Core Functionality
#' \itemize{
#'   \item Agent-based market simulation with neural decision making
#'   \item Dynamic network evolution and MCP communication protocols
#'   \item Real-time market data integration from Yahoo Finance
#'   \item Comprehensive performance analysis and benchmarking
#'   \item Advanced visualization suite with interactive networks
#'   \item Mathematical framework validation and empirical testing
#' }
#'
#' ### Mathematical Framework
#' The implementation is based on the mathematical framework described in:
#' "ANNEM: A Mathematical Framework for AI-MCP-Network Economic Hybrids"
#'
#' Key mathematical components include:
#' \itemize{
#'   \item Agent space definition with heterogeneous types
#'   \item Neural policy networks with attention mechanisms
#'   \item MCP communication protocol for inter-agent messaging
#'   \item Dynamic network formation based on decision similarity
#'   \item Market dynamics with price formation and spillover effects
#' }
#'
#' ## Quick Start
#'
#' ```r
#' # Load the package
#' library(AgentsMCP)
#'
#' # Run a basic ANNEM analysis
#' results <- run_annem_analysis(
#'   symbols = c("AAPL", "MSFT", "GOOGL"),
#'   n_agents = 500,
#'   n_steps = 100
#' )
#'
#' # Generate visualizations
#' plots <- generate_annem_report(results)
#'
#' # View summary
#' annem_summary(results)
#' ```
#'
#' ## Advanced Usage
#'
#' ```r
#' # Create custom market environment
#' market <- create_annem_market(
#'   n_agents = 1000,
#'   symbols = c("AAPL", "MSFT", "GOOGL", "TSLA", "NVDA")
#' )
#'
#' # Run simulation with custom parameters
#' simulation_results <- market$run_simulation(n_steps = 250, verbose = TRUE)
#'
#' # Analyze performance
#' agent_performance <- market$analyze_agent_performance()
#' network_metrics <- market$analyze_network_evolution()
#' benchmark_comparison <- market$compare_with_benchmarks()
#'
#' # Create specific visualizations
#' perf_plots <- plot_agent_performance(agent_performance)
#' network_plots <- plot_network_evolution(network_metrics)
#' wealth_plots <- plot_wealth_dynamics(simulation_results)
#' ```
#'
#' ## Data Requirements
#'
#' The package automatically downloads financial data using the quantmod package.
#' For optimal results, ensure:
#' \itemize{
#'   \item Internet connection for data retrieval
#'   \item Valid stock symbols (Yahoo Finance format)
#'   \item Sufficient memory for large simulations (8GB+ recommended for 1000 agents)
#' }
#'
#' ## Performance Considerations
#'
#' For large-scale simulations:
#' \itemize{
#'   \item Use \code{set_annem_seed()} for reproducible results
#'   \item Consider reducing \code{n_agents} or \code{n_steps} for testing
#'   \item Monitor system resources during execution
#'   \item Use \code{save_results = TRUE} to preserve results
#' }
#'
#' @references
#' \itemize{
#'   \item Farmer, J. D., & Foley, D. (2009). The economy needs agent-based modelling. Nature, 460(7256), 685-686.
#'   \item Jackson, M. O. (2008). Social and economic networks. Princeton university press.
#'   \item Billio, M., Getmansky, M., Lo, A. W., & Pelizzon, L. (2012). Econometric measures of connectedness and systemic risk in the finance and insurance sectors. Journal of financial economics, 104(3), 535-559.
#' }
#'
#' @author Avishek Bhandari \email{bavisek@@gmail.com}
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{run_annem_analysis}} for main analysis function
#'   \item \code{\link{ANNEMAgent}} for agent class documentation
#'   \item \code{\link{ANNEMMarket}} for market environment documentation
#'   \item \code{\link{plot_agent_performance}} for visualization functions
#' }
#'
#' @keywords package agent-based-modeling neural-networks economics finance
#'
#' @docType package
#' @name AgentsMCP-package
#' @aliases AgentsMCP
"_PACKAGE"