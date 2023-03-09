
<!-- README.md is generated from README.Rmd. Please edit that file -->

# peRformance

<!-- badges: start -->
<!-- badges: end -->

The goal of peRformance is to provide a range of functions to study and
visualize market or economic data and analyize and benchmark asset
returns.

## Installation

You can install the development version of peRformance from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fscheler/peRformance")
```

## Example application monthly performance overview table

Create a monthly and annual performance overview table in plotly and a
printable version that can be used in markdown documents with the kable
package. Monthly tables are a popular tool in analysis and comparison of
financial time series.

``` r
  library(peRformance)

  # Generate an integrated monthly and annual performance overview using plotly
  date=seq(as.Date("2007-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-rnorm(length(date))/100
  benchmark_ret<-rnorm(length(date))/100
  da<-data.frame(date,asset_ret,benchmark_ret)

  # Including Benchmark Returns
  df<-mperf_table(da,ts_format="returns")  
#>   Year    Jan    Feb    Mar    Apr    May     Jun     Jul    Aug    Sep    Oct
#> 1 2007  -1.1%   0.3% -0.46%  -6.7%  7.97%   6.93% -10.94%  2.48%  4.07% -1.82%
#> 2 2008 -2.25% -5.11% -1.28%  6.62%  4.19%  -1.99%   3.24%  7.72% -1.84%  0.34%
#> 3 2009 10.66% -1.22% -1.29% -7.46% -9.78%   2.01%  11.34% -2.46% -4.61% -2.06%
#> 4 2010 -0.45% -3.11% -3.64% -6.31% -1.62%   0.71%   0.17%  3.65% -2.89%    10%
#> 5 2011 -0.84%  2.99% -3.98%  6.94%  6.76%   8.82%      4% -2.09% -4.52% -1.24%
#> 6 2012  4.44%  8.27% -8.95% -4.14% -4.33%   1.41%   7.55%  2.25% -1.06%  0.42%
#> 7 2013 -7.09%  8.05%  8.44%  5.58% -2.98%   0.46%  -6.52%  3.84%  4.71% -7.92%
#> 8 2014  0.76%  0.67% -0.25% -7.29%   2.4% -11.05%  -8.07% -2.82%  3.33%  0.68%
#> 9 2015 -1.42%                                                                 
#>      Nov    Dec       FY Benchmark
#> 1  4.43%  3.39%    7.09%   -23.61%
#> 2 -3.45%  0.64%    6.13%    -7.17%
#> 3 -2.56% -7.16%  -15.66%   -15.64%
#> 4  -4.8% -6.33%  -14.68%   -12.27%
#> 5  5.22%  6.52%   31.11%   -38.56%
#> 6  4.92%  2.96%   13.02%     6.78%
#> 7 -4.56% -5.93%   -5.88%    24.88%
#> 8  5.94% -3.66%  -18.94%    44.01%
#> 9                 -1.42%    -1.11%

  #Excluding Benchmark Returns  
  df<-mperf_table(da[,c("date","asset_ret")],ts_format="returns",print_output=F)
  
  # Using Index instead of Return Time Series
  date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-cumprod(1+rnorm(length(date))/100)
  benchmark_ret<-cumprod(1+rnorm(length(date))/100)
  da<-data.frame(date,asset_ret,benchmark_ret)
  
  df<-mperf_table(da,ts_format="index",print_output=F)  
  
  # You can also configure the export options and download the plotly graphic as a high resolution svg
  df<-mperf_table(da,ts_format="index",header_color="#3b5171",font_color="#04103b",export_format="svg",
                  chart_export_width=600,chart_export_height=150,print_output=F)  

  # Display Plotly Graphic
  
  
  htmlwidgets::saveWidget(df$fig, "index.html")

  htmltools::tags$iframe(
    src=file.path(getwd(), "index.html"),
    width="100%",
    height="600",
    scrolling="no",
    seamless="seamless",
    frameBorder="0"
  )
```

<iframe src="C:/FS/packageR/peRformance/index.html" width="100%" height="600" scrolling="no" seamless="seamless" frameBorder="0"></iframe>

## Example application recession shading with ggplot2

Apply NBER recession shading to any ggplot2 time series graphic,
directly loading the recession indicator from the St Louis Fed. Please
note, that this function requires an API key that can be generated for
free on the Fed’s website
(<https://fred.stlouisfed.org/docs/api/api_key.html>).

<img src="man/figures/README-function_recession_shading_plot-1.png" width="100%" />

## Example application dRawdowns

Analyse frequence, magnitude and length of drawdowns in a given time
series. Please note: If the time series ends in a drawdown, the last
peak2recovery and trough2recovery values display the number of periods
since the last trough and the date of the last observation even if this
does not mark a complete recovery.

``` r

# Generate a random return time series
date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
asset_ret<-rnorm(length(date))/100
da<-data.frame(date,asset_ret)

# Analyze the number, magnitude and length of drawdowns
df<-dRawdowns(da,ret_format='returns',graphics=F)

# Some example output
df$longest_drawdown
#> Time difference of 1636 days
df$longest_peak2trough
#> Time difference of 1631 days
# Count number of drawdowns with a trough below threshold value
df$n
#>   ranges observations
#> 1   0.00           11
#> 2  -0.05            4
#> 3  -0.10            2
#> 4  -0.20            1
#> 5  -0.30            1
#> 6  -0.40            1
#> 7  -0.50            1
#> 8   0.60            0
#> 9  -0.70            0
```

sequence of random returns \## Example application rrScat & rrScatEff
functions

The rrScat functions provide visual insights into the risk/return
profile of a given sample of assets. rrScat displays the annualized risk
(standard deviation) of returns and the annualized return in a scatter
plot. rrScatEff provides an interface to the ROI optimizer of the
PortfolioAnalytics package and plots the efficient frontier alongisde
the given assets.

``` r

# Generate a dataframe of random asset returns
date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
Asset1<-rnorm(length(date))/100+0.0005
Asset2<-rnorm(length(date))/100+0.0005
Asset3<-rnorm(length(date))/100+0.0005
Asset4<-rnorm(length(date))/100+0.0005
da<-data.frame(date,Asset1,Asset2,Asset3,Asset4)

# Plot the risk/return scatter
df<-rrScat(da,ret_format="returns",table_format='wide')
df$rr_ggplot
```

<img src="man/figures/README-function_risk_return_scatter-1.png" width="100%" />

``` r

# Plot the risk/return scatter and the efficient frontier
p<-rrScatEff(da,ret_format="returns",table_format='wide')

# The function can also handle data in long format. 
# For this, the data should be arranged as follows: date, id, values
long <- melt(setDT(da), id.vars = "date")
eff<-rrScatEff(long,ret_format="returns",table_format='long')



eff$eff_ggplot
```

<img src="man/figures/README-function_risk_return_scatter-2.png" width="100%" />

## Foreign Exchange Hedging

Investors are frequently exposed to the risk of foreign exchange rate
changes. The very good underlying performance of an investment,
demoninated in a foreign currency, can be (over)compensated easily by a
depreciation in the respective currency’s value compared to the
investor’s home currency. For this reason, investors regularly insure
themselves (hedge) against this risk using Swaps or Forward contracts.
However, the price of these hedges depends on the relative interest
rates level. As a rule of thumb, investor’s can’t escape their home
country’s interest rate environment without taking foreign exchange
risk. If interest rates in the foreign currency are higher, ceteris
paribus, hedging will cost money and vice versa if foreign interest
rates are lower. The cost of a hedge can, therefore, be simulated easily
using the interest rate differential. The FXhedgeR provides a generic
function that calculates the cost of carry of the hedge of any currency
pair based on two dataframes of historical FX rates and forwards points
(both can be obtained for instance from Bloomberg). For major
currencies, the function uses an example dataset hosted on GitHub by
default which, however, may not be up-to-date.

``` r

# Obtain the carry paid/earned by hedging USD exposure for a Singapore based investor
# A negative figure means, the hedge costs money, a postive figure indicates that the investor is earning carry.
df<-FXhedgeR(base_currency='SGD',exp_currency='USD')
tail(df$forwards_perc,1)
#>         Dates    Hedge
#> 1: 2023-03-07 0.013948

# Obtain the carry paid/earned by hedging GBP exposure for a Swiss investor with user-given data
df<-
FXhedgeR(base_currency='CHF',exp_currency='GBP',
         fxrates_gui=read_delim(url('https://raw.githubusercontent.com/fscheler/opendata/main/fx_rates.csv'),delim = ";",show_col_types = FALSE),
         fxforwards_gui=read_delim(url('https://raw.githubusercontent.com/fscheler/opendata/main/fx_forwards.csv'),delim = ";",show_col_types = FALSE)
)

head(df$hedge_perf)
#>         Dates     Hedge
#> 1: 1999-05-20 1.0001607
#> 2: 1999-05-21 1.0003214
#> 3: 1999-05-24 1.0060352
#> 4: 1999-05-25 1.0099296
#> 5: 1999-05-26 0.9983723
#> 6: 1999-05-27 0.9966876
tail(df$forwards_perc)
#>         Dates      Hedge
#> 1: 2023-02-28 0.03138301
#> 2: 2023-03-01 0.03035254
#> 3: 2023-03-02 0.02991483
#> 4: 2023-03-03 0.02987529
#> 5: 2023-03-06 0.02821794
#> 6: 2023-03-07 0.02795312
tail(df$cumulative_hedge_cost)
#>         Dates    Hedge
#> 1: 2023-02-28 1.650495
#> 2: 2023-03-01 1.650694
#> 3: 2023-03-02 1.650890
#> 4: 2023-03-03 1.651085
#> 5: 2023-03-06 1.651270
#> 6: 2023-03-07 1.651453
```

<img src="man/figures/README-function_synthetic_fx_hedging_plot-1.png" width="100%" />

## Dependencies

The package depends on the following packages:

lubridate, fredr, ecm, ggplot2, dplyr, scales, fredr, purrr,
PortfolioAnalytics, ecm, plotly, tidyverse, caTools, zoo, data.table,
tibble
