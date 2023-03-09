
<!-- README.md is generated from README.Rmd. Please edit that file -->

# peRformance

<!-- badges: start -->
<!-- badges: end -->

The goal of peRformance is to provide a range of functions to study and
visualize market or economic data and analyize and benchmark asset
returns.

## Installation

You can install and load the development version of peRformance from
[GitHub](https://github.com/) with the following commands (if not yet
installed, install devtools from CRAN first):

``` r
# Install devtools if required
# install.packages("devtools")

# Install the package
devtools::install_github("fscheler/peRformance")

# Load the package
library(peRformance)
```

## Example application monthly performance overview table

Create a monthly and annual performance overview table in plotly and a
printable version that can be used in markdown documents with the kable
package. Monthly tables are a popular tool in analysis and comparison of
financial time series.

``` r

  # Generate an integrated monthly and annual performance overview using plotly
  date=seq(as.Date("2007-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-rnorm(length(date))/100
  benchmark_ret<-rnorm(length(date))/100
  da<-data.frame(date,asset_ret,benchmark_ret)

  # Including Benchmark Returns
  df<-mperf_table(da,ts_format="returns")  
#>   Year     Jan    Feb    Mar    Apr    May    Jun    Jul     Aug    Sep    Oct
#> 1 2007  -7.35%  -2.1%  8.58%  0.69%  7.75%   1.7%  3.58%  -3.79%  -5.5%  9.43%
#> 2 2008  -0.67% -2.41%  -3.3%  6.72% -7.65% -5.86% -7.08%   2.78%   0.8% -0.56%
#> 3 2009 -16.79% -6.81%  1.59%  2.45%  2.24%  5.44%   3.2% -11.51% -4.13% -0.41%
#> 4 2010  -0.43% -2.62% -3.34%   4.5%  1.16% -1.48%  6.73%  -5.72% -1.61% -5.03%
#> 5 2011   1.21%   4.3%  4.35% -7.28%  -7.9% -2.62% -0.11%  -0.27%   0.1%  4.16%
#> 6 2012    3.8%  1.12% -2.58% -0.99% -4.79%  5.97% -9.38%  -2.07% -0.74% -1.11%
#> 7 2013  -3.31%  3.02% -3.38% -5.79% -1.95%  1.94% -2.97%   0.19% -3.45%  -3.4%
#> 8 2014   6.36%  1.65%  0.88%  3.76%   0.4%  -0.9% -2.35%  11.02%  2.21% -2.71%
#> 9 2015   0.57%                                                                
#>      Nov    Dec       FY Benchmark
#> 1  3.23% -2.38%   12.84%    -8.07%
#> 2  5.82%  0.56%  -11.42%   -15.75%
#> 3  2.37% -10.7%  -30.65%     6.31%
#> 4 -0.97% 10.22%    0.18%   -12.33%
#> 5 -1.67%  2.97%   -3.67%   -16.37%
#> 6  4.17%  7.01%   -0.82%   -28.12%
#> 7 -1.88%   6.3%   -14.3%    -9.33%
#> 8 -1.53%  3.49%   23.69%    16.64%
#> 9                  0.57%     -1.1%

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
                  chart_export_width=800,chart_export_height=150,print_output=F)  

  # Display Plotly Graphic
  #df$fig
```

<img src="./mtable.svg" width="100%" />

## Example application recession shading with ggplot2

Apply NBER recession shading to any ggplot2 time series graphic,
directly loading the recession indicator from the St Louis Fed. Please
note, that this function requires an API key that can be generated for
free on the Fed’s website
(<https://fred.stlouisfed.org/docs/api/api_key.html>).

``` r

# Generate a random return time series
date=seq(as.Date("2007-1-1"), as.Date("2015-1-1"), by = "days")
asset_ret<-cumprod(1+rnorm(length(date))/100)
da<-data.frame(date,asset_ret)

# Initiate a ggplot2 chart
  library(ggplot2)
  cols <- c("Time series with NBER recession shading" = "#04103b")
  p<-ggplot(data=da,aes(x=as.Date(date), y=asset_ret))
# Add recession shading using the function and your API Key  
# to obtain a key visit: https://fred.stlouisfed.org/docs/api/api_key.html
  tryCatch({
    p<-p + ggRec(as.Date(min(da$date)),as.Date(max(da$date)),fredr_key="your_api_key")
  }, error = function(e) {})
#> NULL
# add formatting  
  library(scales)
  p<-p +
# add a theme from the peRformance package
    theme_aq_black_default_font(base_size=16)+
#Add other elements
    geom_line(size=1,aes(y=asset_ret,color="Time series with NBER recession shading"))+
    scale_colour_manual(values = cols)+
    scale_x_date(labels = date_format("%m-%Y"))+
    xlab("")+
    ylab("")+
    theme(legend.position = "bottom",legend.title = element_blank())+
    theme(plot.margin=margin(l=5,r=10,b=5,t=5))
  
```

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
#> Time difference of 1675 days
df$longest_peak2trough
#> Time difference of 1494 days
# Count number of drawdowns with a trough below threshold value
df$n
#>   ranges observations
#> 1   0.00           10
#> 2  -0.05            3
#> 3  -0.10            1
#> 4  -0.20            1
#> 5  -0.30            1
#> 6  -0.40            1
#> 7  -0.50            0
#> 8   0.60            0
#> 9  -0.70            0
```

## Example application rrScat & rrScatEff functions

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
tail(df$hedge_cost_perc,1)
#>         Dates     Hedge
#> 1: 2023-03-07 -0.013948

# Obtain the carry paid/earned by hedging GBP exposure for a Swiss investor with user-given data
df<-
FXhedgeR(base_currency='CHF',exp_currency='GBP',
         fxrates_gui=read_delim(url('https://raw.githubusercontent.com/fscheler/opendata/main/fx_rates.csv'),delim = ";",show_col_types = FALSE),
         fxforwards_gui=read_delim(url('https://raw.githubusercontent.com/fscheler/opendata/main/fx_forwards.csv'),delim = ";",show_col_types = FALSE)
)

head(df$hedge_perf)
#>         Dates     Hedge
#> 1: 1999-05-20 0.9998394
#> 2: 1999-05-21 0.9996787
#> 3: 1999-05-24 1.0050651
#> 4: 1999-05-25 1.0086278
#> 5: 1999-05-26 0.9967644
#> 6: 1999-05-27 0.9947610
tail(df$hedge_cost_perc)
#>         Dates       Hedge
#> 1: 2023-02-28 -0.03138301
#> 2: 2023-03-01 -0.03035254
#> 3: 2023-03-02 -0.02991483
#> 4: 2023-03-03 -0.02987529
#> 5: 2023-03-06 -0.02821794
#> 6: 2023-03-07 -0.02795312
tail(df$cumulative_hedge_cost)
#>         Dates     Hedge
#> 1: 2023-02-28 0.6058789
#> 2: 2023-03-01 0.6058059
#> 3: 2023-03-02 0.6057340
#> 4: 2023-03-03 0.6056622
#> 5: 2023-03-06 0.6055944
#> 6: 2023-03-07 0.6055272
```

<img src="man/figures/README-function_synthetic_fx_hedging_plot-1.png" width="100%" />

## Dependencies

The package references the following packages:

lubridate, fredr, ecm, ggplot2, dplyr, scales, fredr, purrr,
PortfolioAnalytics, ecm, plotly, tidyverse, caTools, zoo, data.table,
tibble
