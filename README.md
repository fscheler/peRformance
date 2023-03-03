
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
#>   Year    Jan    Feb     Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
#> 1 2010  9.24%  0.71%  -1.21%  -8.2% -2.67% -2.43%  9.19% -1.54%  0.66%  8.32%
#> 2 2011  0.14%  2.65%   0.06% -5.57%   0.6% -4.58%  3.74%  3.89% -5.72% -2.35%
#> 3 2012    -2%  1.01% -12.18%   4.6% -0.13%  0.94%  1.83%  -1.9%  3.26% -5.54%
#> 4 2013 -2.68% -6.66%   7.05%  5.78%  0.22% -5.28% -14.2%  3.21% -2.33% -4.65%
#> 5 2014 -0.69% -1.33%   2.07% -5.95%  3.35%     2% -7.32%  5.24% 10.81% -2.32%
#> 6 2015  0.22%                                                                
#>      Nov     Dec       FY Benchmark
#> 1 -1.82%   2.63%   11.93%     0.11%
#> 2 -9.19%  -7.17%  -22.01%     17.7%
#> 3  -5.8% -15.83%  -29.17%     21.2%
#> 4  0.64%  -2.89%  -21.29%     7.96%
#> 5 14.76%  11.22%   33.63%    -9.02%
#> 6                   0.22%     1.61%

  #Excluding Benchmark Returns  
  df<-mperf_table(da[,c("date","asset_ret")],ts_format="returns")
#>   Year    Jan    Feb     Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
#> 1 2010  9.24%  0.71%  -1.21%  -8.2% -2.67% -2.43%  9.19% -1.54%  0.66%  8.32%
#> 2 2011  0.14%  2.65%   0.06% -5.57%   0.6% -4.58%  3.74%  3.89% -5.72% -2.35%
#> 3 2012    -2%  1.01% -12.18%   4.6% -0.13%  0.94%  1.83%  -1.9%  3.26% -5.54%
#> 4 2013 -2.68% -6.66%   7.05%  5.78%  0.22% -5.28% -14.2%  3.21% -2.33% -4.65%
#> 5 2014 -0.69% -1.33%   2.07% -5.95%  3.35%     2% -7.32%  5.24% 10.81% -2.32%
#> 6 2015  0.22%                                                                
#>      Nov     Dec       FY
#> 1 -1.82%   2.63%   11.93%
#> 2 -9.19%  -7.17%  -22.01%
#> 3  -5.8% -15.83%  -29.17%
#> 4  0.64%  -2.89%  -21.29%
#> 5 14.76%  11.22%   33.63%
#> 6                   0.22%
  
  #Using Index instead of Return Time Series
  date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-cumprod(1+rnorm(length(date))/100)
  benchmark_ret<-cumprod(1+rnorm(length(date))/100)
  da<-data.frame(date,asset_ret,benchmark_ret)
  
  df<-mperf_table(da,ts_format="index")  
#>   Year    Jan     Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct
#> 1 2010  0.77% -12.42% -0.32%  1.66%  3.05% -1.83%  9.86%   4.4% -0.75%  6.83%
#> 2 2011 -6.69%  -1.79% -5.77% -0.02% -7.92%  8.54%  4.44%  3.05% -0.88%  1.36%
#> 3 2012 -9.81%  -5.76% -2.54%   2.2% -0.45%  -5.1% -3.12%  9.86% -2.32%  4.87%
#> 4 2013 -4.45%   2.99%  6.31% -3.55% -2.71%  0.81%  8.18%  5.07%  6.83% -2.27%
#> 5 2014  8.75%   4.55%  0.01% -2.95%  1.65%  7.26%  1.97% -0.28%  5.06%  3.37%
#> 6 2015 -1.04%                                                                
#>      Nov    Dec      FY Benchmark
#> 1 -7.35%  -0.4%   1.54%    -9.91%
#> 2 -2.92%  7.34%  -2.78%    -0.35%
#> 3  2.69% -8.11%  -17.7%   -12.89%
#> 4 -0.16% -0.88%  16.23%    -9.64%
#> 5  8.51% -4.02%  38.38%    -4.22%
#> 6                -1.04%     0.79%

  #Display Plotly Graphic
  df$fig
```

<img src="man/figures/README-function_mperf_table-1.png" width="100%" />
