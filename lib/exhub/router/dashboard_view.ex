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
          padding: 16px;
        }
        header {
          background: #161b22;
          border-bottom: 1px solid #30363d;
          padding: 16px 0;
          margin-bottom: 20px;
        }
        header .container {
          display: flex;
          align-items: center;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 8px;
        }
        h1 {
          color: #58a6ff;
          font-size: 22px;
        }
        .subtitle {
          color: #8b949e;
          font-size: 13px;
        }
        .header-right {
          display: flex;
          align-items: center;
          gap: 8px;
          flex-wrap: wrap;
        }
        .stats-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
          gap: 12px;
          margin-bottom: 20px;
        }
        .stat-card {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 10px;
          padding: 16px;
          transition: transform 0.2s, box-shadow 0.2s;
        }
        .stat-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 24px rgba(0,0,0,0.3);
        }
        .stat-label {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          color: #8b949e;
          margin-bottom: 6px;
        }
        .stat-value {
          font-size: 26px;
          font-weight: 700;
          color: #58a6ff;
          word-break: break-all;
        }
        .stat-value.cost {
          color: #3fb950;
        }
        .section {
          background: #161b22;
          border: 1px solid #30363d;
          border-radius: 10px;
          padding: 16px;
          margin-bottom: 16px;
        }
        .section h2 {
          font-size: 16px;
          margin-bottom: 16px;
          color: #f0f6fc;
        }
        .table-wrapper {
          overflow-x: auto;
          -webkit-overflow-scrolling: touch;
        }
        table {
          width: 100%;
          border-collapse: collapse;
          min-width: 480px;
        }
        th, td {
          text-align: left;
          padding: 10px 12px;
          border-bottom: 1px solid #30363d;
          white-space: nowrap;
        }
        th {
          font-weight: 600;
          color: #8b949e;
          font-size: 11px;
          text-transform: uppercase;
        }
        tr:hover {
          background: #21262d;
        }
        .loading {
          text-align: center;
          padding: 30px;
          color: #8b949e;
        }
        .error {
          background: #da3633;
          color: white;
          padding: 12px;
          border-radius: 8px;
          margin-bottom: 16px;
          font-size: 14px;
        }
        .filter-bar {
          display: flex;
          align-items: center;
          gap: 8px;
          margin-bottom: 16px;
          flex-wrap: wrap;
        }
        .filter-label {
          color: #8b949e;
          font-size: 13px;
          white-space: nowrap;
        }
        .filter-btn {
          background: #21262d;
          color: #c9d1d9;
          border: 1px solid #30363d;
          padding: 6px 14px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 13px;
          transition: background 0.15s, border-color 0.15s;
          white-space: nowrap;
        }
        .filter-btn:hover {
          background: #30363d;
        }
        .filter-btn.active {
          background: #1f6feb;
          border-color: #388bfd;
          color: white;
        }
        .refresh-btn {
          background: #238636;
          color: white;
          border: none;
          padding: 6px 14px;
          border-radius: 6px;
          cursor: pointer;
          font-size: 13px;
          white-space: nowrap;
        }
        .refresh-btn:hover {
          background: #2ea043;
        }
        .chart-container {
          height: 260px;
          position: relative;
          overflow-x: auto;
          -webkit-overflow-scrolling: touch;
        }
        .bar-chart {
          display: flex;
          align-items: flex-end;
          justify-content: flex-start;
          gap: 4px;
          height: 200px;
          padding: 20px 0 30px;
          border-bottom: 2px solid #30363d;
          min-width: max-content;
        }
        .bar-item {
          display: flex;
          flex-direction: column;
          align-items: center;
          min-width: 36px;
        }
        .bar {
          background: linear-gradient(to top, #1f6feb, #58a6ff);
          border-radius: 4px 4px 0 0;
          width: 28px;
          position: relative;
          transition: opacity 0.2s;
          cursor: pointer;
        }
        .bar:hover {
          opacity: 0.8;
        }
        .bar-value {
          font-size: 10px;
          color: #c9d1d9;
          margin-bottom: 4px;
          white-space: nowrap;
        }
        .bar-label {
          font-size: 10px;
          color: #8b949e;
          margin-top: 4px;
          white-space: nowrap;
        }
        .current-month-badge {
          display: inline-block;
          background: #1f6feb22;
          border: 1px solid #1f6feb55;
          color: #58a6ff;
          font-size: 11px;
          padding: 2px 8px;
          border-radius: 10px;
          margin-left: 8px;
        }

        @media (max-width: 768px) {
          h1 { font-size: 18px; }
          .stats-grid {
            grid-template-columns: repeat(2, 1fr);
            gap: 10px;
          }
          .stat-value { font-size: 20px; }
          .section { padding: 12px; }
          th, td { padding: 8px 10px; font-size: 13px; }
        }

        @media (max-width: 480px) {
          .container { padding: 10px; }
          .stats-grid {
            grid-template-columns: repeat(2, 1fr);
            gap: 8px;
          }
          .stat-card { padding: 12px; }
          .stat-value { font-size: 18px; }
          .stat-label { font-size: 10px; }
          .filter-bar { gap: 6px; }
          .filter-btn, .refresh-btn { padding: 5px 10px; font-size: 12px; }
        }
      </style>
    </head>
    <body>
      <header>
        <div class="container">
          <div>
            <h1>Token Usage Dashboard</h1>
            <p class="subtitle">Monitor your LLM API usage and costs</p>
          </div>
          <div class="header-right">
            <button class="refresh-btn" onclick="loadDashboard()">↻ Refresh</button>
          </div>
        </div>
      </header>

      <div class="container">
        <div class="filter-bar">
          <span class="filter-label">Period:</span>
          <button class="filter-btn" id="filter-this-month" onclick="setFilter('this_month')">This Month</button>
          <button class="filter-btn active" id="filter-30d" onclick="setFilter('30d')">Last 30 Days</button>
          <button class="filter-btn" id="filter-7d" onclick="setFilter('7d')">Last 7 Days</button>
          <button class="filter-btn" id="filter-all" onclick="setFilter('all')">All Time</button>
        </div>

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
          <h2 id="trends-title">Usage Trends</h2>
          <div class="chart-container">
            <div class="bar-chart" id="trends-chart">
              <div class="loading">Loading...</div>
            </div>
          </div>
        </div>

        <div class="section">
          <h2>Top Models</h2>
          <div class="table-wrapper">
            <table>
              <thead>
                <tr>
                  <th>Model</th>
                  <th>Requests</th>
                  <th>Input</th>
                  <th>Output</th>
                  <th>Total</th>
                  <th>Cost (USD)</th>
                  <th>%</th>
                </tr>
              </thead>
              <tbody id="top-models">
                <tr><td colspan="7" class="loading">Loading...</td></tr>
              </tbody>
            </table>
          </div>
        </div>

        <div class="section">
          <h2>Recent Usage</h2>
          <div class="table-wrapper">
            <table>
              <thead>
                <tr>
                  <th>Time</th>
                  <th>Model</th>
                  <th>Provider</th>
                  <th>Input</th>
                  <th>Output</th>
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
      </div>

      <script>
        var currentFilter = '30d';

        function formatNumber(num) {
          if (num >= 1000000) return (num / 1000000).toFixed(1) + 'M';
          if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
          return num.toString();
        }

        function formatCurrency(num) {
          return '$' + num.toFixed(4);
        }

        function formatDate(isoString) {
          var date = new Date(isoString);
          var now = new Date();
          var isToday = date.toDateString() === now.toDateString();
          if (isToday) {
            return date.toLocaleTimeString([], {hour: '2-digit', minute: '2-digit'});
          }
          return date.toLocaleDateString([], {month: 'short', day: 'numeric'}) + ' ' +
                 date.toLocaleTimeString([], {hour: '2-digit', minute: '2-digit'});
        }

        function getFilterParams(filter) {
          var today = new Date();
          var params = {};
          if (filter === 'this_month') {
            var y = today.getFullYear();
            var m = String(today.getMonth() + 1).padStart(2, '0');
            params.start_date = y + '-' + m + '-01';
            var lastDay = new Date(y, today.getMonth() + 1, 0).getDate();
            params.end_date = y + '-' + m + '-' + String(lastDay).padStart(2, '0');
          } else if (filter === '7d') {
            params.days = 7;
          } else if (filter === '30d') {
            params.days = 30;
          }
          return params;
        }

        function setFilter(filter) {
          currentFilter = filter;
          document.querySelectorAll('.filter-btn').forEach(function(btn) {
            btn.classList.remove('active');
          });
          document.getElementById('filter-' + filter.replace('_', '-')).classList.add('active');

          var titles = {
            'this_month': 'Usage Trends (This Month)',
            '30d': 'Usage Trends (Last 30 Days)',
            '7d': 'Usage Trends (Last 7 Days)',
            'all': 'Usage Trends (All Time)'
          };
          document.getElementById('trends-title').textContent = titles[filter] || 'Usage Trends';

          loadDashboard();
        }

        async function loadDashboard() {
          try {
            document.getElementById('error').innerHTML = '';

            var params = getFilterParams(currentFilter);
            var query = Object.keys(params).map(function(k) {
              return encodeURIComponent(k) + '=' + encodeURIComponent(params[k]);
            }).join('&');

            var url = '/dashboard/data' + (query ? '?' + query : '');
            var response = await fetch(url);
            var result = await response.json();

            if (!result.success) {
              throw new Error(result.error || 'Failed to load data');
            }

            var data = result.data;

            var summary = data.summary;
            document.getElementById('total-requests').textContent = formatNumber(summary.total_requests);
            document.getElementById('total-tokens').textContent = formatNumber(summary.total_tokens);
            document.getElementById('input-tokens').textContent = formatNumber(summary.total_input_tokens);
            document.getElementById('output-tokens').textContent = formatNumber(summary.total_output_tokens);
            document.getElementById('total-cost').textContent = formatCurrency(summary.total_cost);
            document.getElementById('unique-models').textContent = summary.unique_models_count;

            var trends = data.trends || [];
            if (trends.length === 0) {
              document.getElementById('trends-chart').innerHTML = '<div class="loading">No data</div>';
            } else {
              var maxTokens = Math.max.apply(null, trends.map(function(d) { return d.total_tokens; }));
              maxTokens = maxTokens || 1;
              var trendsHtml = trends.map(function(day) {
                var height = Math.max(4, (day.total_tokens / maxTokens * 160));
                return '<div class="bar-item">' +
                  '<div class="bar-value">' + formatNumber(day.total_tokens) + '</div>' +
                  '<div class="bar" style="height:' + height + 'px" title="' + day.date + ': ' + formatNumber(day.total_tokens) + ' tokens"></div>' +
                  '<div class="bar-label">' + day.date.slice(5) + '</div>' +
                  '</div>';
              }).join('');
              document.getElementById('trends-chart').innerHTML = trendsHtml;
            }

            var modelsHtml = (data.top_models || []).map(function(model) {
              return '<tr>' +
                '<td>' + model.model + '</td>' +
                '<td>' + formatNumber(model.request_count) + '</td>' +
                '<td>' + formatNumber(model.total_input) + '</td>' +
                '<td>' + formatNumber(model.total_output) + '</td>' +
                '<td>' + formatNumber(model.total_tokens) + '</td>' +
                '<td>' + formatCurrency(model.total_cost) + '</td>' +
                '<td>' + model.percentage + '%</td>' +
                '</tr>';
            }).join('');
            document.getElementById('top-models').innerHTML = modelsHtml || '<tr><td colspan="7" class="loading">No data</td></tr>';

            var recentHtml = (data.recent_usage || []).map(function(usage) {
              return '<tr>' +
                '<td>' + formatDate(usage.timestamp) + '</td>' +
                '<td>' + usage.model + '</td>' +
                '<td>' + usage.provider + '</td>' +
                '<td>' + formatNumber(usage.input_tokens) + '</td>' +
                '<td>' + formatNumber(usage.output_tokens) + '</td>' +
                '<td>' + formatNumber(usage.total_tokens) + '</td>' +
                '<td>' + formatCurrency(usage.estimated_cost) + '</td>' +
                '</tr>';
            }).join('');
            document.getElementById('recent-usage').innerHTML = recentHtml || '<tr><td colspan="7" class="loading">No data</td></tr>';

          } catch (err) {
            document.getElementById('error').innerHTML = '<div class="error">Error: ' + err.message + '</div>';
            console.error('Dashboard error:', err);
          }
        }

        loadDashboard();
        setInterval(loadDashboard, 60000);
      </script>
    </body>
    </html>
    """
  end
end
