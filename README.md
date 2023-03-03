
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

## Example application monthly performance overview table

``` r
  library(peRformance)

  #Generate an integrated monthly and annual performance overview using plotly
  date=seq(as.Date("2007-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-rnorm(length(date))/100
  benchmark_ret<-rnorm(length(date))/100
  da<-data.frame(date,asset_ret,benchmark_ret)

  #Including Benchmark Returns
  df<-mperf_table(da,ts_format="returns")  
#>   Year    Jan     Feb    Mar     Apr    May    Jun     Jul    Aug    Sep    Oct
#> 1 2007 -5.56%  -4.42%  1.87%  -2.39% -0.32%  6.42%   5.54% -2.74%  3.11%  3.83%
#> 2 2008  2.49%  -0.52%  -7.8%  -5.91% -6.07%  0.05% -10.73% -8.58% -7.67%  9.97%
#> 3 2009 -4.23%   4.26% -5.87%  -4.37%  5.24%  3.89%  -8.98% -5.73%  3.44% -1.52%
#> 4 2010   3.7%   2.83%  1.41% -16.49%  1.08% -3.21%  -2.18%  7.59%  7.52% -5.23%
#> 5 2011  4.07%  -1.37%  -1.7%  -1.03%   5.1% -4.73%  -1.72%  0.25% -1.03%  -4.4%
#> 6 2012  4.03% -10.44%  4.29%  -0.34%  0.33%  5.47%   7.18%  2.74% -9.37% -0.04%
#> 7 2013  0.44%   2.69% -4.54%  -9.93%  6.53% -4.54%   3.42% -1.41% -2.78% -1.12%
#> 8 2014  1.71%  -5.13%  8.03%   5.88% -6.42% -5.53%   8.11% -2.25% 14.36%   1.3%
#> 9 2015   1.3%                                                                  
#>      Nov    Dec       FY Benchmark
#> 1 -4.34% 13.25%   13.36%     5.91%
#> 2 -6.36% -6.22%  -39.51%     5.73%
#> 3  2.65% -7.35%  -18.32%     -6.1%
#> 4 -0.09%  0.05%   -5.31%      3.3%
#> 5  5.31%  -3.1%   -4.88%    -3.64%
#> 6 -3.99% -0.05%   -1.92%     -6.5%
#> 7 -5.47% -2.51%  -18.53%    -36.6%
#> 8  0.49%  3.51%   24.26%    22.22%
#> 9                   1.3%     0.08%

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
