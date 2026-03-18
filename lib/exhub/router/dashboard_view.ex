defmodule Exhub.Router.DashboardView do
  @moduledoc """
  HTML view for the Token Usage Dashboard.

  This module contains the HTML template and rendering logic for the
  Exhub token usage dashboard page.
  """

  @doc """
  Returns the complete HTML content for the dashboard page.
  """
  @spec render() :: String.t()
  def render do
    """
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Exhub Token Usage Dashboard</title>
      <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
          background: #0d1117;
          color: #c9d1d9;
          line-height: 1.6;
        }
        .container {
          max-width: 1400px;
          margin: 0 auto;
          padding: 20px;
        }
        header {
          background: #161b22;
          border-bottom: 1px solid #30363d;
          padding: 20px 0;
          margin-bottom: 30px;
        }
        h1 {
          color: #58a6ff;
          font-size: 28px;
        }
        .subtitle {
          color: #8b949e;
          font-size: 14px;
        }
        .stats-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
          gap: 20px;
          margin-bottom: 30px;
        }
        .stat-card {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 12px;
          padding: 24px;
          transition: transform 0.2s, box-shadow 0.2s;
        }
        .stat-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 24px rgba(0,0,0,0.3);
        }
        .stat-label {
          font-size: 12px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          color: #8b949e;
          margin-bottom: 8px;
        }
        .stat-value {
          font-size: 32px;
          font-weight: 700;
          color: #58a6ff;
        }
        .stat-value.cost {
          color: #238636;
        }
        .section {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 12px;
          padding: 24px;
          margin-bottom: 24px;
        }
        .section h2 {
          font-size: 18px;
          margin-bottom: 20px;
          color: #f0f6fc;
        }
        table {
          width: 100%;
          border-collapse: collapse;
        }
        th, td {
          text-align: left;
          padding: 12px;
          border-bottom: 1px solid #30363d;
        }
        th {
          font-weight: 600;
          color: #8b949e;
          font-size: 12px;
          text-transform: uppercase;
        }
        tr:hover {
          background: #21262d;
        }
        .loading {
          text-align: center;
          padding: 40px;
          color: #8b949e;
        }
        .error {
          background: #da3633;
          color: white;
          padding: 16px;
          border-radius: 8px;
          margin-bottom: 20px;
        }
        .refresh-btn {
          background: #238636;
          color: white;
          border: none;
          padding: 8px 16px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 14px;
          margin-bottom: 20px;
        }
        .refresh-btn:hover {
          background: #2ea043;
        }
        .chart-container {
          height: 300px;
          position: relative;
        }
        .bar-chart {
          display: flex;
          align-items: flex-end;
          justify-content: space-around;
          height: 250px;
          padding: 20px 0;
          border-bottom: 2px solid #30363d;
        }
        .bar {
          background: linear-gradient(to top, #58a6ff, #79c0ff);
          border-radius: 4px 4px 0 0;
          min-width: 30px;
          position: relative;
          transition: opacity 0.2s;
        }
        .bar:hover {
          opacity: 0.8;
        }
        .bar-label {
          position: absolute;
          bottom: -25px;
          left: 50%;
          transform: translateX(-50%);
          font-size: 11px;
          color: #8b949e;
          white-space: nowrap;
        }
        .bar-value {
          position: absolute;
          top: -20px;
          left: 50%;
          transform: translateX(-50%);
          font-size: 11px;
          color: #c9d1d9;
        }
      </style>
    </head>
    <body>
      <header>
        <div class="container">
          <h1>Token Usage Dashboard</h1>
          <p class="subtitle">Monitor your LLM API usage and costs</p>
        </div>
      </header>

      <div class="container">
        <button class="refresh-btn" onclick="loadDashboard()">Refresh Data</button>
        <div id="error"></div>

        <div class="stats-grid" id="stats-grid">
          <div class="stat-card">
            <div class="stat-label">Total Requests</div>
            <div class="stat-value" id="total-requests">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Total Tokens</div>
            <div class="stat-value" id="total-tokens">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Input Tokens</div>
            <div class="stat-value" id="input-tokens">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Output Tokens</div>
            <div class="stat-value" id="output-tokens">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Estimated Cost</div>
            <div class="stat-value cost" id="total-cost">-</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Unique Models</div>
            <div class="stat-value" id="unique-models">-</div>
          </div>
        </div>

        <div class="section">
          <h2>Usage Trends (Last 30 Days)</h2>
          <div class="chart-container">
            <div class="bar-chart" id="trends-chart">
              <div class="loading">Loading...</div>
            </div>
          </div>
        </div>

        <div class="section">
          <h2>Top Models</h2>
          <table>
            <thead>
              <tr>
                <th>Model</th>
                <th>Requests</th>
                <th>Input Tokens</th>
                <th>Output Tokens</th>
                <th>Total Tokens</th>
                <th>Cost (USD)</th>
                <th>Percentage</th>
              </tr>
            </thead>
            <tbody id="top-models">
              <tr><td colspan="7" class="loading">Loading...</td></tr>
            </tbody>
          </table>
        </div>

        <div class="section">
          <h2>Recent Usage</h2>
          <table>
            <thead>
              <tr>
                <th>Timestamp</th>
                <th>Model</th>
                <th>Provider</th>
                <th>Input Tokens</th>
                <th>Output Tokens</th>
                <th>Total</th>
                <th>Cost (USD)</th>
              </tr>
            </thead>
            <tbody id="recent-usage">
              <tr><td colspan="7" class="loading">Loading...</td></tr>
            </tbody>
          </table>
        </div>
      </div>

      <script>
        function formatNumber(num) {
          if (num >= 1000000) return (num / 1000000).toFixed(1) + 'M';
          if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
          return num.toString();
        }

        function formatCurrency(num) {
          return '$' + num.toFixed(4);
        }

        function formatDate(isoString) {
          const date = new Date(isoString);
          return date.toLocaleString();
        }

        async function loadDashboard() {
          try {
            document.getElementById('error').innerHTML = '';

            const response = await fetch('/dashboard/data?days=30');
            const result = await response.json();

            if (!result.success) {
              throw new Error(result.error || 'Failed to load data');
            }

            const data = result.data;

            // Update summary stats
            const summary = data.summary;
            document.getElementById('total-requests').textContent = formatNumber(summary.total_requests);
            document.getElementById('total-tokens').textContent = formatNumber(summary.total_tokens);
            document.getElementById('input-tokens').textContent = formatNumber(summary.total_input_tokens);
            document.getElementById('output-tokens').textContent = formatNumber(summary.total_output_tokens);
            document.getElementById('total-cost').textContent = formatCurrency(summary.total_cost);
            document.getElementById('unique-models').textContent = summary.unique_models_count;

            // Update trends chart
            const trendsHtml = data.trends.map(day => {
              const maxTokens = Math.max(...data.trends.map(d => d.total_tokens), 1);
              const height = (day.total_tokens / maxTokens * 200);
              return `
                <div class="bar" style="height: ${height}px">
                  <div class="bar-value">${formatNumber(day.total_tokens)}</div>
                  <div class="bar-label">${day.date.slice(5)}</div>
                </div>
              `;
            }).join('');
            document.getElementById('trends-chart').innerHTML = trendsHtml || '<div class="loading">No data</div>';

            // Update top models table
            const modelsHtml = data.top_models.map(model => `
              <tr>
                <td>${model.model}</td>
                <td>${formatNumber(model.request_count)}</td>
                <td>${formatNumber(model.total_input)}</td>
                <td>${formatNumber(model.total_output)}</td>
                <td>${formatNumber(model.total_tokens)}</td>
                <td>${formatCurrency(model.total_cost)}</td>
                <td>${model.percentage}%</td>
              </tr>
            `).join('');
            document.getElementById('top-models').innerHTML = modelsHtml || '<tr><td colspan="7" class="loading">No data</td></tr>';

            // Update recent usage table
            const recentHtml = data.recent_usage.map(usage => `
              <tr>
                <td>${formatDate(usage.timestamp)}</td>
                <td>${usage.model}</td>
                <td>${usage.provider}</td>
                <td>${formatNumber(usage.input_tokens)}</td>
                <td>${formatNumber(usage.output_tokens)}</td>
                <td>${formatNumber(usage.total_tokens)}</td>
                <td>${formatCurrency(usage.estimated_cost)}</td>
              </tr>
            `).join('');
            document.getElementById('recent-usage').innerHTML = recentHtml || '<tr><td colspan="7" class="loading">No data</td></tr>';

          } catch (err) {
            document.getElementById('error').innerHTML = `<div class="error">Error: ${err.message}</div>`;
            console.error('Dashboard error:', err);
          }
        }

        // Load dashboard on page load
        loadDashboard();

        // Auto-refresh every 60 seconds
        setInterval(loadDashboard, 60000);
      </script>
    </body>
    </html>
    """
  end
end
