
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
#>   Year    Jan    Feb    Mar    Apr     May    Jun    Jul    Aug    Sep    Oct
#> 1 2007  1.15%  4.47%  5.31% -3.47% -11.82%  0.23%  2.32% -5.94%    -3%  4.68%
#> 2 2008 -0.97%  5.18% -5.34% -6.39%  -4.89%  8.75% -2.35%  6.83% -0.75% -6.19%
#> 3 2009  5.28%  3.23%  4.03%  0.29%  -4.94%  1.82% -6.81%  -0.5% 10.51% -1.96%
#> 4 2010  8.24% -6.77% -1.14% -8.09%  -2.84% -5.46%  0.19% -6.54%  1.55%  1.02%
#> 5 2011 -6.02%  3.05%  1.18% -4.66%   5.08% 12.12%  0.32%  3.64%  2.26% -4.07%
#> 6 2012   4.3% -2.19% -0.73%  12.6%  -4.71%  3.54%  2.16%  0.91%  2.22%  4.66%
#> 7 2013  1.42% -4.32% 12.15%  0.29%   1.15%  6.34% 10.31% -8.14% 12.14% -3.96%
#> 8 2014 -5.53%  7.92% -3.28%  -2.3%  -2.32%  0.22%  7.69% -4.15%  2.04%  6.92%
#> 9 2015  1.11%                                                                
#>      Nov    Dec       FY Benchmark
#> 1 14.19% -5.66%   -0.05%    31.19%
#> 2 -0.21%  1.81%   -5.79%   -21.16%
#> 3 -4.74%  1.77%    6.88%   -26.59%
#> 4   2.6%  0.25%  -16.78%   -15.09%
#> 5  1.07%  -9.5%    2.69%   -20.89%
#> 6  9.62% -6.13%   27.67%   -12.37%
#> 7   0.8% -2.02%   26.52%    -6.75%
#> 8  1.57%  1.51%    9.52%    -3.77%
#> 9                  1.11%     0.82%

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
  df$fig
```

<img src="man/figures/README-function_mperf_table-1.png" width="100%" />

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

  cols <- c("Time Series" = "#04103b")
  p<-ggplot(data=da,aes(x=as.Date(date), y=asset_ret))
# Add recession shading using the function and your API Key  
# to obtain a key visit: https://fred.stlouisfed.org/docs/api/api_key.html
  tryCatch({
    p<-p + add_rec_shade(as.Date(min(da$date)),as.Date(max(da$date)),fredr_key="your_api_key")
  }, error = function(e) {})
#> NULL
# add formatting  
  library(scales)
  p<-p +
    geom_line(size=1,aes(y=asset_ret,color="Time Series"))+
    scale_colour_manual(values = cols)+
    scale_x_date(labels = date_format("%m-%Y"))+
    xlab("")+
    ylab("")+
    theme(legend.position = "bottom",legend.title = element_blank())+
    theme(plot.margin=margin(l=5,r=10,b=5,t=5))
  p
```

<img src="man/figures/README-function_recession_shading-1.png" width="100%" />

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
#> Time difference of 1104 days
df$longest_peak2trough
#> Time difference of 1093 days
# Count number of drawdowns with a trough below threshold value
df$n
#>   ranges observations
#> 1   0.00            5
#> 2  -0.05            2
#> 3  -0.10            2
#> 4  -0.20            2
#> 5  -0.30            2
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
p<-rrScatEff(long,ret_format="returns",table_format='long')
```

## Dependencies

The package depends on the following packages:

lubridate fredr ecm ggplot2 dplyr scales fredr purrr PortfolioAnalytics
ecm plotly tidyverse caTools zoo data.table
