
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
#>   Year    Jan    Feb    Mar    Apr    May    Jun     Jul     Aug    Sep    Oct
#> 1 2007 10.17%  5.06%  4.95% -4.15% -5.55%  1.13%     -2%   1.18%   9.6%  6.54%
#> 2 2008  5.95% -2.08%  8.76%  6.96%  0.21% -5.35%   0.43%  -0.06% -2.29% -8.05%
#> 3 2009 -1.25%  -1.6% -4.24% -6.28%    -3% -2.05%   6.43%  -2.24%  -0.3% -1.71%
#> 4 2010 -3.08% -8.12%  8.05%  2.72% -1.96% -0.79%  -1.05% -14.25% -3.08% -8.68%
#> 5 2011 -6.14%  2.27% -3.36%  1.04%  8.62%  3.03%   0.29%  -4.28% -0.94%  5.18%
#> 6 2012 10.71%  1.95%  0.73%  4.58% -3.37%  2.68%   6.64%   5.47% -1.59% -5.64%
#> 7 2013 -0.36% -5.43%  9.95% -2.08%  6.88%  0.02% -12.79%  -7.39% -0.26%  7.47%
#> 8 2014  2.28% -5.29% -1.52%     7% -4.14%  6.33%    3.2%  -4.21%   0.7%  -1.7%
#> 9 2015 -0.03%                                                                 
#>      Nov    Dec       FY Benchmark
#> 1  -2.6% -4.13%   20.23%   -21.02%
#> 2  1.17%  0.23%    4.67%    -1.28%
#> 3 -5.05% -1.98%  -21.38%     4.13%
#> 4 -2.47%  2.41%  -27.89%     4.09%
#> 5 10.23%   4.6%   20.98%   -30.81%
#> 6  1.06% -2.72%   21.14%    -0.33%
#> 7 -3.43%  1.95%   -7.56%    18.72%
#> 8  0.24%  0.54%    2.61%   -22.59%
#> 9                 -0.03%     1.01%

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
    p<-p%>%add_rec_shade(as.Date(min(da$date)),as.Date(max(da$date)),fredr_key="your_api_key")
  }, error = function(e) {})
#> NULL
#add formatting  
  p<-p+
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
