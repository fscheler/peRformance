
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
#>   Year    Jan    Feb    Mar    Apr    May    Jun    Jul     Aug    Sep    Oct
#> 1 2010  -7.4% -5.43% -2.95%  7.03%  8.57% -0.91%  1.94%   -1.5%  -4.3%  7.36%
#> 2 2011 -4.56% -1.94% -4.44%  7.44% -0.97% -2.43% -8.19% -11.19%  1.83%  6.69%
#> 3 2012 -3.97% -6.15%  5.19% -2.25% -7.11%  2.94%  6.41%  -8.99%  3.55%  1.91%
#> 4 2013   0.4%   6.8% -7.59%  3.86% -0.11%  0.78% -1.07%   1.33% -0.45% -0.51%
#> 5 2014 -6.42%  3.39% -2.51%  -3.6% -1.22% -4.11%     0%   9.73%  0.36%  3.33%
#> 6 2015 -0.62%                                                                
#>      Nov    Dec       FY Benchmark
#> 1 -2.63% -2.33%   -3.99%    41.21%
#> 2 -0.35%  5.56%  -13.49%     4.52%
#> 3 -9.21% -3.49%  -20.64%    -17.3%
#> 4 -0.63% -3.34%    -1.2%   -18.93%
#> 5 -6.85% -3.16%   -11.6%    -1.79%
#> 6                 -0.62%     0.26%

  #Excluding Benchmark Returns  
  df<-mperf_table(da[,c("date","asset_ret")],ts_format="returns")
#>   Year    Jan    Feb    Mar    Apr    May    Jun    Jul     Aug    Sep    Oct
#> 1 2010  -7.4% -5.43% -2.95%  7.03%  8.57% -0.91%  1.94%   -1.5%  -4.3%  7.36%
#> 2 2011 -4.56% -1.94% -4.44%  7.44% -0.97% -2.43% -8.19% -11.19%  1.83%  6.69%
#> 3 2012 -3.97% -6.15%  5.19% -2.25% -7.11%  2.94%  6.41%  -8.99%  3.55%  1.91%
#> 4 2013   0.4%   6.8% -7.59%  3.86% -0.11%  0.78% -1.07%   1.33% -0.45% -0.51%
#> 5 2014 -6.42%  3.39% -2.51%  -3.6% -1.22% -4.11%     0%   9.73%  0.36%  3.33%
#> 6 2015 -0.62%                                                                
#>      Nov    Dec       FY
#> 1 -2.63% -2.33%   -3.99%
#> 2 -0.35%  5.56%  -13.49%
#> 3 -9.21% -3.49%  -20.64%
#> 4 -0.63% -3.34%    -1.2%
#> 5 -6.85% -3.16%   -11.6%
#> 6                 -0.62%
  
  #Using Index instead of Return Time Series
  date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-cumprod(1+rnorm(length(date))/100)
  benchmark_ret<-cumprod(1+rnorm(length(date))/100)
  da<-data.frame(date,asset_ret,benchmark_ret)
  
  df<-mperf_table(da,ts_format="index")  
#>   Year    Jan     Feb   Mar    Apr    May     Jun    Jul    Aug    Sep    Oct
#> 1 2010 -4.79%   5.44% 2.95% -0.26%  4.75% -11.17% 10.05% 13.09%    -4%  2.66%
#> 2 2011 12.76%  -8.38%   -8% -3.68%  6.22%  -6.78% -0.45%  0.72% -2.63%  1.78%
#> 3 2012   7.9%  -1.22% 5.65% -9.53%  4.92%  -2.64%  2.55%  1.95%   4.7% -5.58%
#> 4 2013  2.67% -10.11% 1.01%  -2.9%  0.75%  -2.27%  6.21%  1.06% -6.86% -9.52%
#> 5 2014 -5.39%  -6.71% 3.51% -3.07% -9.82%   2.42% -3.49% -1.67%  1.13% -2.51%
#> 6 2015  -1.1%                                                                
#>     Nov    Dec       FY Benchmark
#> 1 4.47%  1.32%   24.53%    13.56%
#> 2 5.75%  7.99%    2.86%    27.45%
#> 3 6.77% -3.75%   10.53%    34.45%
#> 4 1.24% -9.51%  -26.13%     9.07%
#> 5 2.55% 10.94%  -12.94%     5.78%
#> 6                 -1.1%    -0.68%
```
