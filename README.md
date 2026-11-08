# Portfolio Optimization and Asset Pricing in R

**Advanced Corporate Finance – University of Deusto**

## About the Project

This project was developed individually as part of my Advanced Corporate Finance course at the University of Deusto. The objective was to construct and evaluate an investment portfolio for a three-year horizon, considering an investor with moderate risk aversion.

I analyzed four European companies from different sectors: Banco Santander (SAN.MC), TotalEnergies (TTE.PA), Inditex (ITX.MC), and Unilever (ULVR.L). I selected the STOXX Europe 600 as the market benchmark and used the German 10-year government bond yield as a proxy for the risk-free rate.

The analysis combines Markowitz mean-variance optimization, the Capital Asset Pricing Model (CAPM), Arbitrage Pricing Theory (APT), and the Fama-French three-factor model. My aim was not only to identify an optimal portfolio allocation, but also to understand how different approaches to risk and expected returns could influence the final investment decision.

## Methodology

I conducted the analysis in R using monthly financial data from September 2020 to April 2026.

I started with **Markowitz mean-variance optimization**, calculating historical returns, volatilities, and correlations between the four assets. I then constructed the efficient frontier under no-short-selling constraints and identified the minimum-variance and tangency portfolios. This allowed me to evaluate how diversification could improve the portfolio's risk-return relationship.

Next, I applied the **Capital Asset Pricing Model (CAPM)** to estimate each stock's alpha and beta relative to the STOXX Europe 600. I performed statistical tests to assess the significance of these coefficients and compared observed returns with those implied by the model.

I then extended the analysis using **Arbitrage Pricing Theory (APT)**, incorporating Brent crude oil prices and EUR/USD exchange-rate movements alongside market returns. This helped me examine whether the companies were exposed to additional macroeconomic risks that could not be fully captured by the CAPM.

Finally, I used the **Fama-French three-factor model** to assess the influence of market, size, and value factors on stock returns and compare its explanatory power with the previous models.

## Results

The analysis showed that the different models provide complementary perspectives on portfolio risk and expected returns.

Markowitz optimization helped identify the benefits of diversification and the allocation that maximized the historical Sharpe ratio under the selected constraints. The CAPM and multifactor regressions provided further insight into the systematic and macroeconomic risks affecting each company.

Based on the results and the investor's moderate risk aversion, my final recommendation was:

| Asset | Portfolio Weight |
|---|---:|
| German government bond proxy | 50.00% |
| Banco Santander | 34.38% |
| TotalEnergies | 15.62% |

I chose to allocate half of the portfolio to the government bond proxy to reduce overall risk, while concentrating the equity allocation in Santander and TotalEnergies.

One of the main conclusions of the project was that an optimal portfolio cannot be determined solely by historical returns and volatility. The results also depend on the assumptions behind each model, the selected time horizon, and the investor's risk preferences.

## Files

- `scripts/Markowitz.R`: Mean-variance optimization, efficient frontier, tangency portfolio, and Capital Market Line.
- `scripts/CAPM.R`: Estimation of alpha and beta, hypothesis testing, and Security Market Line.
- `scripts/APT.R`: Multifactor regressions using market, oil, and exchange-rate factors, including the Fama-French extension.
- `report/Portfolio_Recommendation_Report.pdf`: Complete academic report in Spanish, including the economic rationale, methodology, empirical results, and final recommendation.
- `data/README.md`: Description of the datasets required to reproduce the analysis.

## Tools and Libraries

The project was developed in **R**, using RStudio and Posit Cloud.

The main libraries used were:

- `tidyverse` and `tidyquant` for financial data processing and time-series analysis.
- `quadprog` for portfolio optimization.
- `ggplot2`, `ggrepel`, and `scales` for data visualization.
- `readxl` and `writexl` for working with Excel files.
- `lubridate` for date manipulation.
- `broom` for organizing regression results.

Historical market data were obtained from Bloomberg and Yahoo Finance, while the Fama-French factors were used for the multifactor extension.

## How to Run

To reproduce the analysis, download the repository and open it in RStudio.

Install the required packages:

```r
install.packages(c(
  "tidyverse", "tidyquant", "quadprog",
  "scales", "ggrepel", "writexl",
  "future", "readxl", "lubridate", "broom"
))
```

The scripts require three Excel datasets containing historical stock prices, German government bond yields, and Fama-French factors. These should be placed in the repository's root directory using the filenames specified in `data/README.md`.

Once the datasets are available, the scripts can be executed independently:

```r
source("scripts/Markowitz.R")
source("scripts/CAPM.R")
source("scripts/APT.R")
```

An internet connection is required to retrieve additional financial data from Yahoo Finance.

The original Bloomberg datasets are not publicly included due to potential redistribution restrictions.

## Limitations

The analysis is based on historical financial data, and the estimated relationships may change over time. Portfolio weights and expected returns are sensitive to the observation period, market conditions, and the assumptions of each model.

The German 10-year government bond yield is used as a risk-free rate proxy, although it does not perfectly match the portfolio's three-year investment horizon.

This project was completed for academic purposes and does not constitute investment advice.
