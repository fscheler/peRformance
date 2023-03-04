



dRawdowns<-function(da,graphics=F)
{
  if (!require("ecm")) install.packages("ecm")
  if (!require("plotly")) install.packages("ggplot2")
  if (!require("dplyr")) install.packages("dplyr")


  library(ecm)
  library(plotly)
  library(dplyr)

  names(da)<-c("date","asset_ret")
  da$asset_idx<-cumprod(1+da$asset_ret)
  da$asset_idx[1]<-1

  da$md<-da$asset_idx/cummax(da$asset_idx)-1

  da$mdst<-ifelse(da$md<0 & lagpad(da$md,k=1)==0,1,0)
  da$mdn<-cumsum(da$mdst)

  ds<-da[da$md<0,]
  ds<-ds%>%group_by(mdn)%>%mutate(trough=min(md))
  ds$through_date<-ifelse(ds$md==ds$trough,ds$date,0)
  ds<-ds%>%group_by(mdn)%>%summarise(st_date=head(date,1),ed_date=tail(date,1),trough=min(md),through_date=as.Date(max(through_date),origin="1970-01-01"))
  ds$through_date

  ds$peak2through<-ds$through_date-ds$st_date
  ds$peak2recovery<-ds$ed_date-ds$st_date
  ds$through2recovery<-ds$ed_date-ds$through_date

  pos_observations<-nrow(da[da$asset_ret>=0,])
  neg_observations<-nrow(da[da$asset_ret<0,])


  longest_drawdown<-max(ds$peak2recovery)
  longest_peak2through<-max(ds$peak2through)
  longest_recovery<-max(ds$through2recovery)
  max_drawdown<-min(ds$trough)
  ann_return<-((tail(da$asset_idx,1)/head(da$asset_idx,1))^(365.25/(as.numeric(tail(da$date,1)-head(da$date,1))))-1)

  #library(lubridate)
  #da%>%group_by(year(date))%>%summarise(n=n())

  sharpe_ratio<-ann_return/(sd(da$asset_ret)*252^0.5)
  calmar_ratio<-ann_return/max_drawdown


  ranges<-c(-0.0,-0.05,-0.1,-0.2,-0.3,-0.4,-0.5,0.6,-0.7)
  observations<-c(
  nrow(ds[ds$trough<(-0.0),]),
  nrow(ds[ds$trough<(-0.05),]),
  nrow(ds[ds$trough<(-0.1),]),
  nrow(ds[ds$trough<(-0.2),]),
  nrow(ds[ds$trough<(-0.3),]),
  nrow(ds[ds$trough<(-0.4),]),
  nrow(ds[ds$trough<(-0.5),]),
  nrow(ds[ds$trough<(-0.6),]),
  nrow(ds[ds$trough<(-0.7),])
  )
  n<-data.frame(ranges,observations)


  res_list<-list(
    'longest_drawdown'=longest_drawdown,
    'longest_peak2through'=longest_peak2through,
    'longest_recovery'=longest_recovery,
    'max_drawdown'=max_drawdown,
    'ann_return'=ann_return,
    'sharpe_ratio'=sharpe_ratio,
    'calmar_ratio'=calmar_ratio,
    'pos_observations'=pos_observations,
    'neg_observations'=neg_observations,
    'all_drawdowns'=ds,
    'n'=n
  )


  if(graphics==T)
  {

    #Histogram
    col_aq2<-as.character(c("#04103b","#dd0400","#3b5171"))

    dd_dist <- plot_ly(
      type='histogram',histnorm = "probability",
      x=~ds$trough,
      marker = list(color = col_aq2[1]))
    dd_dist <- dd_dist %>% layout(
      barmode="stack",
      bargap=0.1)
    #fig<-fig%>%add_segments(x = 1, xend = 1, y = 0.3, yend = 0,type="scatter",mode="line",line=list(color=col_aq2[2]),marker=list(color="white"))
    dd_dist<-dd_dist%>%layout(showlegend=F,xaxis=list(title="Drawdowns",tickformat="0%"),yaxis = list(title="Probability",tickformat="0%"))
    dd_dist


    dd_ts<-plot_ly(da,x=~date,y=~asset_idx/cummax(asset_idx),type="scatter",mode="line",line=list(color=col_aq2[1]))%>%
      layout(title="Maximum Drawdown %",xaxis = list(title=""), yaxis = list(title="",tickformat=".0%"),showlegend=F)

    perf_ts<-plot_ly(da,x=~date,y=~asset_idx,type="scatter",mode="line",line=list(color=col_aq2[1]))%>%
      layout(title="Performance %",xaxis = list(title=""), yaxis = list(title="",tickformat=".0%"),showlegend=F)

    res_list<-list(
      'longest_drawdown'=longest_drawdown,
      'longest_peak2through'=longest_peak2through,
      'longest_recovery'=longest_recovery,
      'max_drawdown'=max_drawdown,
      'ann_return'=ann_return,
      'sharpe_ratio'=sharpe_ratio,
      'calmar_ratio'=calmar_ratio,
      'pos_observations'=pos_observations,
      'neg_observations'=neg_observations,
      'all_drawdowns'=ds,
      'n'=n,
      'dd_dist'=dd_dist,
      'dd_ts'=dd_ts,
      'perf_ts'=perf_ts
    )

  }



  return(res_list)

}

