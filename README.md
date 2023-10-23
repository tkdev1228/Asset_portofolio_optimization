# Asset_portofolio_optimization

# Objective
- Created a financial data analysis and visualization system that acquires price data for financial assets, calculates asset returns and risks, performs portfolio optimization, and compares market sentiment and stock price trends.

# Processing details
- Asset Information Retrieval: Uses the Yahoo Finance API to retrieve price data for different assets and stores each data in a separate variable.
- Data Joining: combines the retrieved price data into a single data frame, creating a joined_prices data frame that includes OHLC data and adjusted price data.
- Extract Adjusted Prices: Extracts only the adjusted price data from joined_prices to create a joined_prices_only data frame.
- Asset Return Calculation: Calculates the most recent 12-, 18-, and 24-month returns for each asset and evaluates correlations and covariances among assets.
- ARIMA Modeling and Forecasting: Fits an ARIMA model to each asset, generates forecasts, and updates the correlation matrix between assets.
- Correlation Calculation of Recent Data: Calculate the correlation matrix between assets using the last 12 months of data.
- Portfolio Performance Calculator: creates portfolios combining multiple assets and calculates portfolio returns.
- Risk Calculation and Sharpe Ratio: Evaluate the risk of a portfolio and calculate the Sharpe Ratio to show the return taking into account the risk.
- Market Sentiment and Stock Trend Comparison using Hidden Markov Models: Fit Hidden Markov Models using price data for specific assets to estimate and visualize the probability of market sentiment.
- Portfolio optimization and efficient frontier visualization: Generate different portfolios with different risk/return combinations and visualize them to provide choices for different risk/return profiles.
