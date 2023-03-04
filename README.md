
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

  #Generate an integrated monthly and annual performance overview using plotly
  date=seq(as.Date("2007-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-rnorm(length(date))/100
  benchmark_ret<-rnorm(length(date))/100
  da<-data.frame(date,asset_ret,benchmark_ret)

  #Including Benchmark Returns
  df<-mperf_table(da,ts_format="returns")  
#>   Year    Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
#> 1 2007   1.9% -3.51%  6.93%  6.67% -1.21% -8.65% -7.93%  -0.2% -7.66% -1.49%
#> 2 2008 -0.56% -1.03% -5.14%  0.76% -2.88%  3.01%  6.13% -1.68% -8.81% -4.74%
#> 3 2009 -3.97% -0.89% -3.64% -3.18% -7.61% -5.76% -1.98%  8.73%  5.11%     7%
#> 4 2010 -7.19% -3.28% -4.97%  2.81%  0.74%  0.22% -5.34%  1.88% -6.48% -3.27%
#> 5 2011  7.68% 15.28%  13.8%  7.03%  -1.8% -4.45%  5.27% -0.65% -4.64%  1.04%
#> 6 2012  3.74% -2.41% -3.71%  4.92% -3.16% -2.62%  1.02%  0.54% -6.73%  2.46%
#> 7 2013  1.84%  4.43%  -2.3%  9.85% -3.16%  -3.2% -8.95% -5.74%   3.4%  2.38%
#> 8 2014 -0.44% -2.46%  4.96% -6.08% -0.53%  6.13%  -0.1% 10.25%  1.71%  -2.3%
#> 9 2015  -2.6%                                                               
#>      Nov    Dec       FY Benchmark
#> 1 -1.06%  -9.7%  -24.42%    -9.67%
#> 2   2.6% -6.42%   -18.1%   -28.13%
#> 3 -6.72% -1.43%   -14.8%   -18.09%
#> 4 -1.05%  8.93%  -16.73%     8.14%
#> 5 -10.8% -2.74%   24.02%   -27.59%
#> 6 -7.25% -4.89%  -17.41%   -18.13%
#> 7 -2.85% -2.22%   -7.64%     7.06%
#> 8 -0.17%  0.82%   11.32%    14.35%
#> 9                  -2.6%    -1.09%

  #Excluding Benchmark Returns  
  df<-mperf_table(da[,c("date","asset_ret")],ts_format="returns",print_output=F)
  
  #Using Index instead of Return Time Series
  date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-cumprod(1+rnorm(length(date))/100)
  benchmark_ret<-cumprod(1+rnorm(length(date))/100)
  da<-data.frame(date,asset_ret,benchmark_ret)
  
  df<-mperf_table(da,ts_format="index",print_output=F)  
  
  #You can also configure the export options and download the plotly graphic as a high resolution svg
  df<-mperf_table(da,ts_format="index",header_color="#3b5171",font_color="#04103b",export_format="svg",
                  chart_export_width=600,chart_export_height=150,print_output=F)  

  #Display Plotly Graphic
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

# Create a return time series

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
#add formatting  
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

#Study lenght, magnitude and frequency of drawdowns in a time series
date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
asset_ret<-rnorm(length(date))/100
da<-data.frame(date,asset_ret)

df<-dRawdowns(da,ret_format='returns',graphics=F)

#Some example output
df$longest_drawdown
#> Time difference of 1602 days
df$longest_peak2trough
#> Time difference of 596 days
#Count number of drawdowns with a trough below threshold value
df$n
#>   ranges observations
#> 1   0.00           23
#> 2  -0.05            3
#> 3  -0.10            2
#> 4  -0.20            1
#> 5  -0.30            1
#> 6  -0.40            0
#> 7  -0.50            0
#> 8   0.60            0
#> 9  -0.70            0
```
