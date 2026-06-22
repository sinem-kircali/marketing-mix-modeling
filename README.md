# Marketing Mix Modeling: Media ROI & Budget Optimization

Quantifies the sales contribution and return on investment of three digital media channels — TikTok, Facebook, and Google Ads — using weekly time series data. Built for the Strategic Marketing course at Erasmus University Rotterdam.

## Methods

### 1. Baseline Regression
Linear model: `Sales ~ TikTok + Facebook + Google Ads`

### 2. Dynamic Effects (Lagged Sales)
Adds 1- and 2-week lagged sales to capture carryover effects:
`Sales ~ L1.Sales + L2.Sales + TikTok + Facebook + Google Ads`

### 3. Log-Log Model (Diminishing Returns)
Log-transforms all variables to model diminishing returns to advertising spend:
`ln(Sales) ~ ln(L1.Sales) + ln(TikTok) + ln(Facebook) + ln(Google Ads)`

### 4. Interaction Effects
Tests synergies between channels (e.g. TikTok × Facebook) within the log-log framework.

## Outputs
- **Sales contribution** per channel (derived from elasticities × average spend)
- **SROMI** (Sales Return on Marketing Investment) bar chart per channel
- **Actual vs. optimal budget allocation** pie charts — optimal shares derived from log-log elasticities
- **Model fit plot** — predicted vs. actual log sales

## Tech Stack
- R
- Base R (`ts`, `lm`) · `ggplot2`

## Data
Weekly marketing spend (TikTok, Facebook, Google Ads) and sales data from 2018 — not included in this repository. Place `marketing_mix_assignment.csv` in your working directory and update the path in the script.
