---
 date: "2023-03-09"
 output:
   html_document:
     keep_md: TRUE
---

<!-- README.md is generated from README.Rmd. Please edit that file -->





# peRformance

<!-- badges: start -->
<!-- badges: end -->

The goal of peRformance is to provide a range of functions to study and visualize market or economic data and analyize and benchmark asset returns.

## Installation

You can install the development version of peRformance from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fscheler/peRformance")
```

## Example application monthly performance overview table

Create a monthly and annual performance overview table in plotly and a printable version that can be used in markdown documents with the kable package.
Monthly tables are a popular tool in analysis and comparison of financial time series.


```r
  library(peRformance)

  # Generate an integrated monthly and annual performance overview using plotly
  date=seq(as.Date("2007-1-1"), as.Date("2015-1-1"), by = "days")
  asset_ret<-rnorm(length(date))/100
  benchmark_ret<-rnorm(length(date))/100
  da<-data.frame(date,asset_ret,benchmark_ret)

  # Including Benchmark Returns
  df<-mperf_table(da,ts_format="returns")  
#>   Year    Jan    Feb    Mar    Apr    May    Jun    Jul    Aug     Sep     Oct
#> 1 2007  5.61% -4.82% -1.83% -1.23%   8.4%  4.89%  0.18% -3.25%  -3.97%   -7.8%
#> 2 2008 -3.52%  1.34%  4.74% -0.84%  2.07%  8.92% -4.43% -4.14%  -2.73%  -9.59%
#> 3 2009  6.24% -0.44% -2.63% -4.62%  4.31%  4.57%  7.65% 10.37%   4.04%   2.32%
#> 4 2010 -5.54% -4.38%  2.53% -4.78% -0.73% -8.78%  2.57%  5.17% -10.63%   3.26%
#> 5 2011 -4.09%  9.21%   2.5%  7.49%     2%  8.38% -4.17%  6.96%   0.35%  -5.69%
#> 6 2012  3.72% -5.03% 10.02% -0.97% -3.96% 13.49%  9.05%  1.66%  10.23%   0.32%
#> 7 2013   7.9% -4.82% -5.06% -2.47% -2.24% -0.58%  4.74% -4.53%  -3.54% -10.25%
#> 8 2014  6.35%  2.02% -0.93%  5.71% -0.79% -6.02%  7.39%  1.09%  -6.84%  -6.13%
#> 9 2015  -0.9%                                                                 
#>      Nov    Dec       FY Benchmark
#> 1 -1.51% -2.31%   -8.49%    -4.43%
#> 2 -1.39% -2.66%  -12.69%    23.11%
#> 3 -5.59% -3.34%   23.67%    32.82%
#> 4 -1.87% -6.07%  -26.72%    -3.56%
#> 5  5.52%  1.29%   32.27%     1.65%
#> 6 -2.02% 10.44%   55.16%    21.51%
#> 7 -5.93% -0.03%  -24.75%    -5.82%
#> 8 -1.76%  0.31%   -0.87%   -20.63%
#> 9                  -0.9%     0.94%

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
  
  htmltools::tagList(list(df$fig))
```

```{=html}
<div id="htmlwidget-ea7a9602d42db54d4a82" style="width:100%;height:400px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-ea7a9602d42db54d4a82">{"x":{"visdat":{"4283c6266a":["function () ","plotlyVisDat"]},"cur_data":"4283c6266a","attrs":{"4283c6266a":{"columnwidth":[60,80,80,80,80,80,80,80,80,80,80,80,80],"header":{"values":["<b>Year<\/b>","<b>Jan<\/b>","<b>Feb<\/b>","<b>Mar<\/b>","<b>Apr<\/b>","<b>May<\/b>","<b>Jun<\/b>","<b>Jul<\/b>","<b>Aug<\/b>","<b>Sep<\/b>","<b>Oct<\/b>","<b>Nov<\/b>","<b>Dec<\/b>","<b><\/b>","<b>FY<\/b>","<b>Benchmark<\/b>"],"line":{"color":"white"},"fill":{"color":"#3b5171"},"font":{"color":"white","size":7}},"cells":{"height":16,"values":[["2010","2011","2012","2013","2014","2015"],["-8.04%","-14.04%","-5.54%","5.83%","-6.09%","-0.11%"],["-9.42%","12.99%","-2.92%","0.4%","1.74%",""],["2.2%","-3.79%","-1%","-0.94%","0.98%",""],["-1.36%","6.56%","-3.42%","-4.53%","-3.56%",""],["10.79%","-3.77%","1.56%","5.88%","0.64%",""],["7.68%","0.76%","1.31%","8.93%","-3.11%",""],["2.74%","2.71%","-3.24%","-1.62%","-0.66%",""],["5.61%","-4.03%","-5.28%","13.97%","-0.37%",""],["-5.26%","-0.85%","1.61%","-7.34%","9.5%",""],["10.91%","0.44%","-0.25%","3.37%","2.14%",""],["-4.88%","0.22%","3.18%","5.1%","4.92%",""],["-4%","3.15%","3.73%","3.14%","-11.29%",""],["","","","","",""],["4.31%","-2.03%","-10.3%","34.94%","-6.52%","-0.11%"],["-16.1%","-30.19%","-4.45%","37.7%","18.96%","0.68%"]],"line":{"color":"white"},"fill":{"color":[["white","lightgrey","white","lightgrey","white","lightgrey","white","lightgrey"]]},"align":["center","center","center","center","center","center","center","center"],"font":{"color":"#04103b","size":6}},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"table"}},"layout":{"margin":{"b":0,"l":0,"t":0,"r":0,"par":4},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"toImageButtonOptions":{"format":"svg","filename":"monthly_returns_table","width":600,"height":150}},"data":[{"columnwidth":[60,80,80,80,80,80,80,80,80,80,80,80,80],"header":{"values":["<b>Year<\/b>","<b>Jan<\/b>","<b>Feb<\/b>","<b>Mar<\/b>","<b>Apr<\/b>","<b>May<\/b>","<b>Jun<\/b>","<b>Jul<\/b>","<b>Aug<\/b>","<b>Sep<\/b>","<b>Oct<\/b>","<b>Nov<\/b>","<b>Dec<\/b>","<b><\/b>","<b>FY<\/b>","<b>Benchmark<\/b>"],"line":{"color":"white"},"fill":{"color":"#3b5171"},"font":{"color":"white","size":7}},"cells":{"height":16,"values":[["2010","2011","2012","2013","2014","2015"],["-8.04%","-14.04%","-5.54%","5.83%","-6.09%","-0.11%"],["-9.42%","12.99%","-2.92%","0.4%","1.74%",""],["2.2%","-3.79%","-1%","-0.94%","0.98%",""],["-1.36%","6.56%","-3.42%","-4.53%","-3.56%",""],["10.79%","-3.77%","1.56%","5.88%","0.64%",""],["7.68%","0.76%","1.31%","8.93%","-3.11%",""],["2.74%","2.71%","-3.24%","-1.62%","-0.66%",""],["5.61%","-4.03%","-5.28%","13.97%","-0.37%",""],["-5.26%","-0.85%","1.61%","-7.34%","9.5%",""],["10.91%","0.44%","-0.25%","3.37%","2.14%",""],["-4.88%","0.22%","3.18%","5.1%","4.92%",""],["-4%","3.15%","3.73%","3.14%","-11.29%",""],["","","","","",""],["4.31%","-2.03%","-10.3%","34.94%","-6.52%","-0.11%"],["-16.1%","-30.19%","-4.45%","37.7%","18.96%","0.68%"]],"line":{"color":"white"},"fill":{"color":[["white","lightgrey","white","lightgrey","white","lightgrey","white","lightgrey"]]},"align":["center","center","center","center","center","center","center","center"],"font":{"color":"#04103b","size":6}},"type":"table","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```



##  Example application recession shading with ggplot2

Apply NBER recession shading to any ggplot2 time series graphic, directly loading the recession indicator from the St Louis Fed.
Please note, that this function requires an API key that can be generated for free on the Fed's website (https://fred.stlouisfed.org/docs/api/api_key.html).



<img src="man/figures/README-function_recession_shading_plot-1.png" width="100%" />


##  Example application dRawdowns

Analyse frequence, magnitude and length of drawdowns in a given time series.
Please note: If the time series ends in a drawdown, the last peak2recovery and trough2recovery values display the number of periods since the last trough and the date of the last observation even if this does not mark a complete recovery.


```r

# Generate a random return time series
date=seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "days")
asset_ret<-rnorm(length(date))/100
da<-data.frame(date,asset_ret)

# Analyze the number, magnitude and length of drawdowns
df<-dRawdowns(da,ret_format='returns',graphics=F)

# Some example output
df$longest_drawdown
#> Time difference of 595 days
df$longest_peak2trough
#> Time difference of 320 days
# Count number of drawdowns with a trough below threshold value
df$n
#>   ranges observations
#> 1   0.00           47
#> 2  -0.05           11
#> 3  -0.10            5
#> 4  -0.20            1
#> 5  -0.30            1
#> 6  -0.40            0
#> 7  -0.50            0
#> 8   0.60            0
#> 9  -0.70            0
```
sequence of random returns
##    Example application rrScat & rrScatEff functions

The rrScat functions provide visual insights into the risk/return profile of a given sample of assets.
rrScat displays the annualized risk (standard deviation) of returns and the annualized return in a scatter plot.
rrScatEff provides an interface to the ROI optimizer of the PortfolioAnalytics package and plots the efficient frontier alongisde the given assets.


```r

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

```r

# Plot the risk/return scatter and the efficient frontier
p<-rrScatEff(da,ret_format="returns",table_format='wide')

# The function can also handle data in long format. 
# For this, the data should be arranged as follows: date, id, values
long <- melt(setDT(da), id.vars = "date")
eff<-rrScatEff(long,ret_format="returns",table_format='long')



eff$eff_ggplot
```

<img src="man/figures/README-function_risk_return_scatter-2.png" width="100%" />

##  Foreign Exchange Hedging

Investors are frequently exposed to the risk of foreign exchange rate changes. The very good underlying performance of an investment, demoninated in a foreign currency, can be (over)compensated easily by a depreciation in the respective currency's value compared to the investor's home currency.
For this reason, investors regularly insure themselves (hedge) against this risk using Swaps or Forward contracts. However, the price of these hedges depends on the relative interest rates level. As a rule of thumb, investor's can't escape their home country's interest rate environment without taking foreign exchange risk. If interest rates in the foreign currency are higher, ceteris paribus, hedging will cost money and vice versa if foreign interest rates are lower.
The cost of a hedge can, therefore, be simulated easily using the interest rate differential. The FXhedgeR provides a generic function that calculates the cost of carry of the hedge of any currency pair based on two dataframes of historical FX rates and forwards points (both can be obtained for instance from Bloomberg). For major currencies, the function uses an example dataset hosted on GitHub by default which, however, may not be up-to-date.


```r

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

##  Dependencies

The package depends on the following packages:

  lubridate,
  fredr,
  ecm,
  ggplot2,
  dplyr,
  scales,
  fredr,
  purrr,
  PortfolioAnalytics,
  ecm,
  plotly,
  tidyverse,
  caTools,
  zoo,
  data.table,
  tibble

