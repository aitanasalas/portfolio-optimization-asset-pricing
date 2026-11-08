# Portfolio Optimization and Asset Pricing in R

### Markowitz Mean-Variance Optimization | CAPM | APT | Fama-French Three-Factor Model

**Author:** Aitana Salas  
**Course:** Advanced Corporate Finance  
**Institution:** University of Deusto  
**Programming Language:** R

## Project Overview

This individual academic project develops and evaluates an investment portfolio for a three-year investment horizon, combining portfolio optimization techniques with empirical asset pricing models.

The analysis examines four European equities from different sectors and evaluates their risk-return characteristics, exposure to systematic risk, and sensitivity to macroeconomic factors.

The objective is to identify a suitable portfolio allocation for an investor with moderate risk aversion and assess whether different financial models lead to consistent investment recommendations.

## Assets and Market Benchmark

The portfolio considers four publicly traded European companies:

| Company | Ticker | Sector |
|---|---|---|
| Banco Santander | SAN.MC | Financial Services |
| TotalEnergies | TTE.PA | Energy |
| Inditex | ITX.MC | Consumer Discretionary |
| Unilever | ULVR.L | Consumer Staples |

The STOXX Europe 600 is used as the market benchmark, while the German 10-year government bond yield serves as the risk-free rate proxy.

The analysis uses monthly financial observations covering the period from September 2020 to April 2026.

## Methodology

### 1. Markowitz Mean-Variance Optimization

- Estimate historical monthly log returns and the covariance matrix.
- Construct the minimum-variance portfolio.
- Generate the efficient frontier under no-short-selling constraints.
- Identify the tangency portfolio by maximizing the Sharpe ratio.
- Analyze portfolio weights, risk contributions, and the Capital Market Line.

### 2. Capital Asset Pricing Model (CAPM)

- Estimate asset-specific alpha and beta coefficients through linear regression.
- Evaluate exposure to systematic market risk.
- Perform statistical hypothesis tests on estimated coefficients.
- Compare observed returns with CAPM-implied expected returns.
- Visualize the Security Market Line.

### 3. Arbitrage Pricing Theory (APT)

- Estimate multifactor regressions using market returns, Brent crude oil prices, and EUR/USD exchange-rate movements.
- Examine the statistical significance and magnitude of factor exposures.
- Evaluate whether additional factors improve the explanation of asset returns.
- Apply the multifactor framework to the selected portfolio.

### 4. Fama-French Three-Factor Extension

- Extend the analysis using European market, size (SMB), and value (HML) factors.
- Estimate factor sensitivities and model fit for the selected equities.
- Compare the explanatory power of the multifactor approach with the CAPM.

## Portfolio Recommendation

The final recommendation adopts a balanced allocation between the German government bond risk-free proxy and a diversified equity portfolio, reflecting the investor's moderate risk aversion.

| Asset | Portfolio Weight |
|---|---:|
| German government bond proxy | 50.000% |
| Banco Santander | 34.383% |
| TotalEnergies | 15.617% |
| **Total** | **100.000%** |

The project also discusses model assumptions, estimation uncertainty, diversification benefits, and the limitations of historical financial data when making investment decisions.

## Technologies and Tools

- **R:** Statistical analysis, portfolio optimization, and financial modeling
- **tidyverse / tidyquant:** Financial data manipulation and time-series processing
- **quadprog:** Quadratic optimization
- **ggplot2 / ggrepel:** Data visualization
- **readxl / writexl:** Spreadsheet import and export
- **lubridate:** Date processing
- **broom:** Regression output analysis

## Repository Structure

```text
portfolio-optimization-asset-pricing/
├── scripts/
│   ├── Markowitz.R
│   ├── CAPM.R
│   └── APT.R
├── data/
│   └── README.md
├── report/
│   └── Portfolio_Recommendation_Report.pdf
├── .gitignore
└── README.md
```

## How to Run

1. Install R and RStudio.
2. Clone or download this repository.
3. Install the required R packages:

```r
install.packages(c(
  "tidyverse", "tidyquant", "quadprog",
  "scales", "ggrepel", "writexl",
  "future", "readxl", "lubridate", "broom"
))
```

4. Obtain the required input datasets and place them in the repository's root directory, using the exact filenames referenced in the scripts.
5. Open RStudio and set the working directory to the repository's root directory.
6. Execute the scripts individually:

```r
source("scripts/Markowitz.R")
source("scripts/CAPM.R")
source("scripts/APT.R")
```

The scripts can be run independently. Internet access is required for market data downloaded through Yahoo Finance.

**Data availability:** Some original datasets were obtained from Bloomberg and are not included in this public repository pending redistribution permission. Users must supply appropriately licensed data with the expected structure to reproduce the analysis.

## Report

The full academic report, written in Spanish, is available in the `report/` directory.

It provides the economic rationale, data treatment, empirical methodology, statistical results, portfolio recommendation, and discussion of model limitations.

## Limitations

The analysis relies on historical returns and estimated relationships, which may not remain stable over time. Results are sensitive to the sample period, data source, benchmark selection, and underlying model assumptions.

The German 10-year government bond yield is used as a proxy for the risk-free rate and does not represent a perfectly risk-free three-year investment.

This project was developed for academic purposes and does not constitute investment advice.
