\name{mperfT}
\alias{mperfT}
\title{Monthly performance table}
\usage{
mperfT(da,ts_format="returns",header_color="#3b5171",font_color="#04103b",
                      export_format="svg",chart_export_width=800,chart_export_height=400,print_output=T)
}
\description{
Returns a Plotly graphic and a data.frame showing monthly and annual returns of an asset and optionally a benchmark
}
\examples{
# Generate an integrated monthly and annual performance overview using plotly
date=seq(as.Date("2007-1-1"), as.Date("2015-1-1"), by = "days")
asset_ret<-rnorm(length(date))/100
benchmark_ret<-rnorm(length(date))/100
da<-data.frame(date,asset_ret,benchmark_ret)

dlist<-mperfT(da,ts_format="returns",header_color="#3b5171",font_color="#04103b",
                      export_format="svg",chart_export_width=800,chart_export_height=400,print_output=T)

dlist$fig
}
