
<!-- README.md is generated from README.Rmd. Please edit that file -->

# peRformance

<!-- badges: start -->
<!-- badges: end -->

The goal of peRformance is to provide a range of functions to visually
analyze and benchmark asset returns.

## Installation

You can install the development version of peRformance from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fscheler/peRformance")
```

## Example application

``` r
  library(peRformance)

  #Generate an integrated monthly and annual performance overview using plotly
  date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-rnorm(length(date))/100
  benchmark_ret<-rnorm(length(date))/100
  da<-data.frame(date,asset_ret,benchmark_ret)

  #Including Benchmark Returns
  df<-mperf_table(da,ts_format="returns")  
#>   Year    Jan    Feb    Mar    Apr    May    Jun    Jul    Aug     Sep    Oct
#> 1 2010  1.57%  6.23% 15.53% -2.81% -2.03%  5.52% -6.74% -3.06% -16.46%  8.06%
#> 2 2011  -5.7%  4.63% -0.84%   3.3%  0.01%  2.58% -3.63%  -1.9%  -3.09% -2.16%
#> 3 2012  1.83%  9.25%  7.87% -8.41%  2.07%    -3%  4.55% -7.59%   8.89% -7.58%
#> 4 2013  5.84% -1.47%   0.4% -9.29% -2.35% -4.15%  1.48%  2.94%  12.07%  2.82%
#> 5 2014 -4.15%  0.64%  2.68%  -5.1% -8.08%  9.74%  1.56% -1.74%  10.93%  9.33%
#> 6 2015  1.41%                                                                
#>      Nov     Dec      FY Benchmark
#> 1  3.25%  -4.38%   0.93%    19.28%
#> 2 10.48%  -0.85%   1.82%   -14.22%
#> 3 11.09% -11.59%   3.91%     -4.8%
#> 4  0.32%   2.89%  10.45%   -22.91%
#> 5   0.4%   6.62%  22.87%    -3.46%
#> 6                  1.41%     1.51%

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
                  chart_width=600,chart_height=150,print_output=F)  

  #Display Plotly Graphic
  df$fig
```

<img src="man/figures/README-function_mperf_table-1.png" width="100%" />
