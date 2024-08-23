

dRawdowns<-function(da,ret_format='returns',graphics=F,runmaxwindow=2000,chart_export_width=600,chart_export_height=450,m=list(r=0,l=0,b=0,t=50,par=4))
{
  if (!require("ecm")) install.packages("ecm")
  if (!require("plotly")) install.packages("ggplot2")
  if (!require("dplyr")) install.packages("dplyr")


  library(ecm)
  library(plotly)
  library(dplyr)
  library(caTools)

  if(ret_format=='returns')
  {
    names(da)<-c("date","asset_ret")
    da$asset_idx<-cumprod(1+da$asset_ret)
  }else{
    names(da)<-c("date","asset_idx")
    da$asset_ret<-da$asset_idx/lagpad(da$asset_idx,k=1)-1
    da$asset_ret[1]<-0
  }
  da$date<-as.Date(da$date)
  da<-da[order(da$date),]

  if(runmaxwindow=="")
  {
    da$md<-da$asset_idx/cummax(da$asset_idx)-1
  }else{
    da$md<-da$asset_idx/runmax(da$asset_idx,k=runmaxwindow,endrule="max",align="right")-1
  }


  da$mdst<-ifelse(da$md<0 & lagpad(da$md,k=1)==0,1,0)
  da$mdn<-cumsum(da$mdst)

  ds<-da[da$md<0,]
  ds<-ds%>%group_by(mdn)%>%mutate(trough=min(md))

  ds$trough_date<-as.Date(ifelse(ds$md==ds$trough,ds$date,NA))

  #dtorigin_adj="1970-01-01"
  ds<-ds%>%group_by(mdn)%>%summarise(st_date=head(date,1),ed_date=tail(date,1),trough=min(md),trough_date=as.Date(max(trough_date,na.rm=T)))

  ds$trough_date

  ds$peak2trough<-ds$trough_date-ds$st_date
  ds$peak2recovery<-ds$ed_date-ds$st_date
  ds$trough2recovery<-ds$ed_date-ds$trough_date

  pos_observations<-nrow(da[da$asset_ret>=0,])
  neg_observations<-nrow(da[da$asset_ret<0,])


  longest_drawdown<-max(ds$peak2recovery)
  longest_peak2trough<-max(ds$peak2trough)
  longest_recovery<-max(ds$trough2recovery)
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
    'longest_peak2trough'=longest_peak2trough,
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
    dd_dist<-dd_dist %>% config(toImageButtonOptions = list( format = "svg",filename = "drawdown_distribution",width = chart_export_width,height = chart_export_height))


    dd_ts<-plot_ly(da,x=~date,y=~asset_idx/cummax(asset_idx),type="scatter",mode="line",line=list(color=col_aq2[1]))%>%
      layout(title="Maximum Drawdown %",xaxis = list(title=""), yaxis = list(title="",tickformat=".0%"),showlegend=F)
    dd_ts<-dd_ts %>% config(toImageButtonOptions = list( format = "svg",filename = "drawdowns",width = chart_export_width,height = chart_export_height))

    perf_ts<-plot_ly(da,x=~date,y=~asset_idx/head(asset_idx,1),type="scatter",mode="line",line=list(color=col_aq2[1]))%>%
      layout(title="Performance %",xaxis = list(title=""), yaxis = list(title="",tickformat=".0%"),showlegend=F)
    perf_ts<-perf_ts %>% config(toImageButtonOptions = list( format = "svg",filename = "indexed_returns",width = chart_export_width,height = chart_export_height))

    res_list<-list(
      'longest_drawdown'=longest_drawdown,
      'longest_peak2trough'=longest_peak2trough,
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



