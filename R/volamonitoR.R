lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

#Volatility Monitoring
#-----------------------------------------------------------------------------------------------------------------------
volamonitoR<-function(tr,ret_format="returns",vola_monitoring_slider=90,svg_export_width=800,svg_export_height=450, chart_title1="Volatility Z-Scores",chart_title2="Performance & Signals",threshold1=3,threshold2=4,threshold3=5)
{

  lagpad <- function(x, k) {
    if (k>0) {
      return (c(rep(NA, k), x)[1 : length(x)] );
    }
    else {
      return (c(x[(-k+1) : length(x)], rep(NA, -k)));
    }
  }
  
  #if (!require("ecm")) install.packages("ecm")
  if (!require("plotly")) install.packages("plotly")
  if (!require("caTools")) install.packages("caTools")

  #library(ecm)
  library(caTools)
  library(plotly)

  col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#969696"))

  if(ret_format=='returns')
  {
    names(tr)<-c("date","ret")
    tr$idx<-cumprod(1+tr$ret)
  }else{
    names(tr)<-c("date","idx")
    tr$ret<-tr$idx/lagpad(tr$idx,k=1)-1
    tr$ret[1]<-0
  }
  tr$date<-as.Date(tr$date)

  tr$sd<-caTools::runsd(tr$ret,vola_monitoring_slider,align="right",endrule="NA")
  tr$sd_ann<-tr$sd*252^0.5

  tr$sd_ann_mean<-runmean(tr$sd_ann,500,align="right",endrule="NA")
  tr$sd_ann_sd<-runsd(tr$sd_ann,500,align="right",endrule="NA")
  tr$z_score<-(tr$sd_ann-tr$sd_ann_mean)/tr$sd_ann_sd

  tr$threshold1<-threshold1
  tr$threshold2<-threshold2
  tr$threshold3<-threshold3


  scores<-plot_ly(tr,x=~date,y=~z_score,name="Volatility Z-Score",type="scatter",mode="lines",line=list(color=col_aq2[1]))%>%
    add_trace(x=~date,y=~threshold1,name="Threshold 1",line=list(color=col_aq2[2]))%>%
    add_trace(x=~date,y=~threshold2,name="Threshold 2",line=list(color=col_aq2[2]))%>%
    add_trace(x=~date,y=~threshold3,name="Threshold 3",line=list(color=col_aq2[2]))%>%
    layout(title=chart_title1,xaxis = list(title="",range=c(min(as.Date(tr$date)),max(as.Date(tr$date)))), yaxis = list(title=""),legend = list(orientation = "h",xanchor = "center",x = 0.5))

  scores<-scores %>% config(toImageButtonOptions = list( format = "svg",filename = "volatility_deviation.svg",width = svg_export_width,height = svg_export_height))

  tr$trigger1<-ifelse(tr$z_score>=tr$threshold1 & lagpad(tr$z_score,k=1) < tr$threshold1,tr$idx/head(tr$idx,1),NA)
  tr$trigger2<-ifelse(tr$z_score>=tr$threshold2 & lagpad(tr$z_score,k=1) < tr$threshold2,tr$idx/head(tr$idx,1),NA)
  tr$trigger3<-ifelse(tr$z_score>=tr$threshold3 & lagpad(tr$z_score,k=1) < tr$threshold3,tr$idx/head(tr$idx,1),NA)


  perf<-plot_ly(tr,x=~date,y=~idx/head(idx,1),name="Performance",type="scatter",mode="lines",line=list(color=col_aq2[1]))%>%
    add_trace(x=~date,y=~trigger1,name="Threshold 1",marker=list(color="#969696",size=9))%>%
    add_trace(x=~date,y=~trigger2,name="Threshold 2",marker=list(color=col_aq2[3],size=9))%>%
    add_trace(x=~date,y=~trigger3,name="Threshold 3",marker=list(color=col_aq2[2],size=9))%>%
    layout(title=chart_title2,xaxis = list(title=""), yaxis = list(title="",tickformat="0%"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
  perf<-perf %>% config(toImageButtonOptions = list( format = "svg",filename = "returns_and_signals.svg",width = svg_export_width,height = svg_export_height))

  plist<-list("scores"=scores,"perf"=perf,"tr"=tr)

  return(plist)
}

