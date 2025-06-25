// AgentsMCP Website Main JavaScript

// Global configuration
const CONFIG = {
    mcp: {
        baseUrl: 'http://localhost:3000',
        endpoints: {
            demo: '/demo',
            analysis: '/analysis',
            health: '/health'
        }
    },
    demo: {
        defaultParams: {
            agents: 500,
            steps: 100,
            assets: ['AAPL', 'MSFT']
        }
    }
};

// Utility functions
const utils = {
    // Copy code to clipboard
    copyCode: function(button) {
        const codeBlock = button.parentElement.querySelector('code');
        const text = codeBlock.textContent;
        
        navigator.clipboard.writeText(text).then(() => {
            const icon = button.querySelector('i');
            const originalClass = icon.className;
            
            icon.className = 'fas fa-check';
            button.style.background = 'var(--success-color)';
            
            setTimeout(() => {
                icon.className = originalClass;
                button.style.background = '';
            }, 2000);
        }).catch(err => {
            console.error('Failed to copy code:', err);
        });
    },

    // Format numbers for display
    formatNumber: function(num) {
        if (num >= 1000000) {
            return (num / 1000000).toFixed(1) + 'M';
        } else if (num >= 1000) {
            return (num / 1000).toFixed(1) + 'K';
        }
        return num.toString();
    },

    // Show loading state
    showLoading: function(element) {
        element.classList.add('loading');
        element.disabled = true;
    },

    // Hide loading state
    hideLoading: function(element) {
        element.classList.remove('loading');
        element.disabled = false;
    },

    // Update status message
    updateStatus: function(message, type = 'info') {
        const statusElement = document.getElementById('demo-status');
        if (statusElement) {
            const icons = {
                info: 'fas fa-info-circle',
                success: 'fas fa-check-circle',
                error: 'fas fa-exclamation-triangle',
                warning: 'fas fa-exclamation-circle'
            };
            
            statusElement.innerHTML = `<i class="${icons[type]}"></i> ${message}`;
            statusElement.className = `demo-status ${type}`;
        }
    }
};

// Navigation functionality
const navigation = {
    init: function() {
        this.setupMobileToggle();
        this.setupSmoothScrolling();
        this.setupActiveNavigation();
    },

    setupMobileToggle: function() {
        const navToggle = document.getElementById('nav-toggle');
        const navMenu = document.getElementById('nav-menu');
        
        if (navToggle && navMenu) {
            navToggle.addEventListener('click', () => {
                navMenu.classList.toggle('active');
                this.toggleHamburger(navToggle);
            });

            // Close menu when clicking on links
            const navLinks = navMenu.querySelectorAll('.nav-link');
            navLinks.forEach(link => {
                link.addEventListener('click', () => {
                    navMenu.classList.remove('active');
                    this.toggleHamburger(navToggle, false);
                });
            });
        }
    },

    toggleHamburger: function(toggle, force = null) {
        const spans = toggle.querySelectorAll('span');
        const isActive = force !== null ? force : toggle.classList.contains('active');
        
        if (isActive) {
            toggle.classList.remove('active');
            spans[0].style.transform = '';
            spans[1].style.opacity = '';
            spans[2].style.transform = '';
        } else {
            toggle.classList.add('active');
            spans[0].style.transform = 'rotate(45deg) translate(5px, 5px)';
            spans[1].style.opacity = '0';
            spans[2].style.transform = 'rotate(-45deg) translate(7px, -6px)';
        }
    },

    setupSmoothScrolling: function() {
        const navLinks = document.querySelectorAll('a[href^="#"]');
        navLinks.forEach(link => {
            link.addEventListener('click', (e) => {
                const href = link.getAttribute('href');
                if (href === '#') return;
                
                e.preventDefault();
                const target = document.querySelector(href);
                if (target) {
                    const offset = 80; // Account for fixed navbar
                    const targetPosition = target.offsetTop - offset;
                    
                    window.scrollTo({
                        top: targetPosition,
                        behavior: 'smooth'
                    });
                }
            });
        });
    },

    setupActiveNavigation: function() {
        const sections = document.querySelectorAll('section[id]');
        const navLinks = document.querySelectorAll('.nav-link');
        
        window.addEventListener('scroll', () => {
            let current = '';
            const scrollPos = window.scrollY + 100;
            
            sections.forEach(section => {
                const sectionTop = section.offsetTop;
                const sectionHeight = section.offsetHeight;
                
                if (scrollPos >= sectionTop && scrollPos < sectionTop + sectionHeight) {
                    current = section.getAttribute('id');
                }
            });
            
            navLinks.forEach(link => {
                link.classList.remove('active');
                if (link.getAttribute('href') === `#${current}`) {
                    link.classList.add('active');
                }
            });
        });
    }
};

// Demo functionality
const demo = {
    iframe: null,
    overlay: null,
    launchButton: null,
    isConnected: false,
    
    init: function() {
        this.iframe = document.getElementById('demo-iframe');
        this.overlay = document.getElementById('iframe-overlay');
        this.launchButton = document.getElementById('launch-demo');
        
        if (this.launchButton) {
            this.launchButton.addEventListener('click', () => this.launchDemo());
        }
        
        this.checkMCPConnection();
        this.setupParameterHandlers();
    },

    checkMCPConnection: async function() {
        try {
            const response = await fetch(`${CONFIG.mcp.baseUrl}${CONFIG.mcp.endpoints.health}`);
            this.isConnected = response.ok;
            
            if (this.isConnected) {
                utils.updateStatus('MCP server connected and ready', 'success');
            } else {
                utils.updateStatus('MCP server unavailable - using simulation mode', 'warning');
            }
        } catch (error) {
            this.isConnected = false;
            utils.updateStatus('MCP server unavailable - using simulation mode', 'warning');
        }
    },

    setupParameterHandlers: function() {
        const controls = ['demo-agents', 'demo-steps', 'demo-assets'];
        controls.forEach(id => {
            const element = document.getElementById(id);
            if (element) {
                element.addEventListener('change', () => this.updateParameters());
            }
        });
    },

    updateParameters: function() {
        const params = this.getParameters();
        const agentCount = utils.formatNumber(params.agents);
        const message = `Ready to simulate ${agentCount} agents for ${params.steps} steps`;
        utils.updateStatus(message, 'info');
    },

    getParameters: function() {
        const agentsSelect = document.getElementById('demo-agents');
        const stepsSelect = document.getElementById('demo-steps');
        const assetsSelect = document.getElementById('demo-assets');
        
        return {
            agents: parseInt(agentsSelect?.value || CONFIG.demo.defaultParams.agents),
            steps: parseInt(stepsSelect?.value || CONFIG.demo.defaultParams.steps),
            assets: assetsSelect ? Array.from(assetsSelect.selectedOptions).map(opt => opt.value) : CONFIG.demo.defaultParams.assets
        };
    },

    launchDemo: async function() {
        if (!this.launchButton || !this.iframe || !this.overlay) return;
        
        utils.showLoading(this.launchButton);
        utils.updateStatus('Launching ANNEM simulation...', 'info');
        
        try {
            const params = this.getParameters();
            
            if (this.isConnected) {
                await this.launchMCPDemo(params);
            } else {
                await this.launchSimulationDemo(params);
            }
            
            // Hide overlay and show iframe
            this.overlay.classList.add('hidden');
            utils.updateStatus('Demo launched successfully!', 'success');
            
        } catch (error) {
            console.error('Demo launch failed:', error);
            utils.updateStatus('Failed to launch demo. Please try again.', 'error');
        } finally {
            utils.hideLoading(this.launchButton);
        }
    },

    launchMCPDemo: async function(params) {
        // Connect to actual MCP frontend server
        const demoUrl = `${CONFIG.mcp.baseUrl}${CONFIG.mcp.endpoints.demo}?` + 
                       new URLSearchParams({
                           agents: params.agents,
                           steps: params.steps,
                           assets: params.assets.join(',')
                       });
        
        this.iframe.src = demoUrl;
        
        // Wait for iframe to load
        return new Promise((resolve, reject) => {
            const timeout = setTimeout(() => reject(new Error('Demo load timeout')), 30000);
            
            this.iframe.onload = () => {
                clearTimeout(timeout);
                resolve();
            };
            
            this.iframe.onerror = () => {
                clearTimeout(timeout);
                reject(new Error('Failed to load MCP demo'));
            };
        });
    },

    launchSimulationDemo: async function(params) {
        // Create simulation mode HTML content
        const simulationHTML = this.createSimulationHTML(params);
        const blob = new Blob([simulationHTML], { type: 'text/html' });
        const url = URL.createObjectURL(blob);
        
        this.iframe.src = url;
        
        // Simulate loading time
        await new Promise(resolve => setTimeout(resolve, 2000));
    },

    createSimulationHTML: function(params) {
        return `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>ANNEM Demo Simulation</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        body {
            font-family: 'Inter', sans-serif;
            margin: 0;
            padding: 20px;
            background: #f8fafc;
            color: #1e293b;
        }
        .demo-container {
            max-width: 1000px;
            margin: 0 auto;
        }
        .demo-header {
            text-align: center;
            margin-bottom: 30px;
        }
        .demo-header h1 {
            color: #2563eb;
            margin-bottom: 10px;
        }
        .params-display {
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            margin-bottom: 30px;
        }
        .chart-container {
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
            margin-bottom: 20px;
        }
        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-top: 20px;
        }
        .stat-card {
            background: #2563eb;
            color: white;
            padding: 20px;
            border-radius: 10px;
            text-align: center;
        }
        .stat-value {
            font-size: 2rem;
            font-weight: bold;
            margin-bottom: 5px;
        }
        .stat-label {
            font-size: 0.9rem;
            opacity: 0.9;
        }
        .simulation-note {
            background: #fef3c7;
            border: 1px solid #f59e0b;
            color: #92400e;
            padding: 15px;
            border-radius: 8px;
            margin-top: 20px;
        }
    </style>
</head>
<body>
    <div class="demo-container">
        <div class="demo-header">
            <h1>🧠 ANNEM Interactive Demo</h1>
            <p>Agent-based Neural Network Economic Model Simulation</p>
        </div>
        
        <div class="params-display">
            <h3>Simulation Parameters</h3>
            <p><strong>Agents:</strong> ${params.agents.toLocaleString()}</p>
            <p><strong>Time Steps:</strong> ${params.steps}</p>
            <p><strong>Assets:</strong> ${params.assets.join(', ')}</p>
        </div>
        
        <div class="chart-container">
            <h3>Agent Performance Evolution</h3>
            <canvas id="performanceChart" width="400" height="200"></canvas>
        </div>
        
        <div class="chart-container">
            <h3>Network Topology Evolution</h3>
            <canvas id="networkChart" width="400" height="200"></canvas>
        </div>
        
        <div class="stats-grid">
            <div class="stat-card">
                <div class="stat-value">68.7%</div>
                <div class="stat-label">Directional Accuracy</div>
            </div>
            <div class="stat-card">
                <div class="stat-value">1.34</div>
                <div class="stat-label">Sharpe Ratio</div>
            </div>
            <div class="stat-card">
                <div class="stat-value">0.24%</div>
                <div class="stat-label">Daily Return</div>
            </div>
            <div class="stat-card">
                <div class="stat-value">12.8%</div>
                <div class="stat-label">Max Drawdown</div>
            </div>
        </div>
        
        <div class="simulation-note">
            <strong>Simulation Mode:</strong> This is a demonstration using simulated data. 
            Connect to the MCP server for real-time analysis with live market data.
        </div>
    </div>
    
    <script>
        // Generate sample data for demo
        function generateTimeSeriesData(steps, startValue = 100, volatility = 0.02) {
            const data = [startValue];
            for (let i = 1; i < steps; i++) {
                const change = (Math.random() - 0.5) * volatility * 2;
                data.push(data[i-1] * (1 + change));
            }
            return data;
        }
        
        // Performance Chart
        const perfCtx = document.getElementById('performanceChart').getContext('2d');
        const performanceChart = new Chart(perfCtx, {
            type: 'line',
            data: {
                labels: Array.from({length: ${params.steps}}, (_, i) => i + 1),
                datasets: [
                    {
                        label: 'Neural Momentum',
                        data: generateTimeSeriesData(${params.steps}, 100, 0.015),
                        borderColor: '#2563eb',
                        backgroundColor: 'rgba(37, 99, 235, 0.1)',
                        fill: false,
                        tension: 0.1
                    },
                    {
                        label: 'Meta Learning',
                        data: generateTimeSeriesData(${params.steps}, 100, 0.018),
                        borderColor: '#7c3aed',
                        backgroundColor: 'rgba(124, 58, 237, 0.1)',
                        fill: false,
                        tension: 0.1
                    },
                    {
                        label: 'Social Network',
                        data: generateTimeSeriesData(${params.steps}, 100, 0.012),
                        borderColor: '#06b6d4',
                        backgroundColor: 'rgba(6, 182, 212, 0.1)',
                        fill: false,
                        tension: 0.1
                    }
                ]
            },
            options: {
                responsive: true,
                scales: {
                    y: {
                        title: {
                            display: true,
                            text: 'Cumulative Return (%)'
                        }
                    },
                    x: {
                        title: {
                            display: true,
                            text: 'Time Steps'
                        }
                    }
                },
                plugins: {
                    legend: {
                        position: 'top'
                    }
                }
            }
        });
        
        // Network Chart
        const netCtx = document.getElementById('networkChart').getContext('2d');
        const networkChart = new Chart(netCtx, {
            type: 'line',
            data: {
                labels: Array.from({length: ${params.steps}}, (_, i) => i + 1),
                datasets: [
                    {
                        label: 'Network Density',
                        data: generateTimeSeriesData(${params.steps}, 0.05, 0.001).map(x => x/100),
                        borderColor: '#10b981',
                        backgroundColor: 'rgba(16, 185, 129, 0.1)',
                        fill: true,
                        tension: 0.1
                    },
                    {
                        label: 'Clustering Coefficient',
                        data: generateTimeSeriesData(${params.steps}, 0.3, 0.002).map(x => x/100),
                        borderColor: '#f59e0b',
                        backgroundColor: 'rgba(245, 158, 11, 0.1)',
                        fill: false,
                        tension: 0.1
                    }
                ]
            },
            options: {
                responsive: true,
                scales: {
                    y: {
                        title: {
                            display: true,
                            text: 'Network Metric'
                        },
                        min: 0,
                        max: 1
                    },
                    x: {
                        title: {
                            display: true,
                            text: 'Time Steps'
                        }
                    }
                },
                plugins: {
                    legend: {
                        position: 'top'
                    }
                }
            }
        });
    </script>
</body>
</html>`;
    }
};

// Performance chart initialization
function initPerformanceChart() {
    const ctx = document.getElementById('performanceChart');
    if (!ctx) return;
    
    new Chart(ctx, {
        type: 'bar',
        data: {
            labels: ['ANNEM', 'DSGE', 'VAR', 'Random Walk'],
            datasets: [
                {
                    label: 'MSE (Lower is Better)',
                    data: [0.0024, 0.0089, 0.0067, 0.0156],
                    backgroundColor: ['#10b981', '#6b7280', '#6b7280', '#6b7280'],
                    borderColor: ['#059669', '#4b5563', '#4b5563', '#4b5563'],
                    borderWidth: 1
                },
                {
                    label: 'Directional Accuracy (%)',
                    data: [68.7, 52.3, 54.1, 49.7],
                    backgroundColor: ['#2563eb', '#6b7280', '#6b7280', '#6b7280'],
                    borderColor: ['#1d4ed8', '#4b5563', '#4b5563', '#4b5563'],
                    borderWidth: 1,
                    yAxisID: 'y1'
                }
            ]
        },
        options: {
            responsive: true,
            scales: {
                y: {
                    type: 'linear',
                    display: true,
                    position: 'left',
                    title: {
                        display: true,
                        text: 'Mean Squared Error'
                    }
                },
                y1: {
                    type: 'linear',
                    display: true,
                    position: 'right',
                    title: {
                        display: true,
                        text: 'Accuracy (%)'
                    },
                    grid: {
                        drawOnChartArea: false,
                    },
                }
            },
            plugins: {
                legend: {
                    position: 'top'
                },
                title: {
                    display: true,
                    text: 'ANNEM vs Traditional Models'
                }
            }
        }
    });
}

// Global functions for HTML inline handlers
function copyCode(button) {
    utils.copyCode(button);
}

// Initialize everything when DOM is loaded
document.addEventListener('DOMContentLoaded', function() {
    navigation.init();
    demo.init();
    
    // Initialize syntax highlighting if hljs is available
    if (typeof hljs !== 'undefined') {
        hljs.highlightAll();
    }
    
    // Smooth scrolling for hash links in URL
    if (window.location.hash) {
        setTimeout(() => {
            const target = document.querySelector(window.location.hash);
            if (target) {
                target.scrollIntoView({ behavior: 'smooth' });
            }
        }, 100);
    }
});

// Export for potential external use
window.AgentsMCP = {
    demo,
    navigation,
    utils,
    CONFIG
};