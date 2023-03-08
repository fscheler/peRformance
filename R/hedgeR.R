



FXhedge<-function(base_currency='USD',exp_currency='EUR',just_convert=T)
{

  options(warn=-1)

  if (!require("tibble")) install.packages("tibble")
  if (!require("data.table")) install.packages("data.table")
  if (!require("readr")) install.packages("readr")
  if (!require("tibble")) install.packages("tibble")

  library(tibble)
  library(data.table)
  library (readr)
  library(tibble)

  #fxrates_gui<-dbGetQuery(pool,paste0("select * from gaa.gaa_fx_rates"))
  fxrates_gui=read_delim(url('https://raw.githubusercontent.com/fscheler/opendata/main/fx_rates.csv'),delim = ";",show_col_types = FALSE)
  fxrates_gui<- as_tibble(fxrates_gui[duplicated(fxrates_gui$Dates)==FALSE,])

  #fxforwards_gui<-  dbGetQuery(pool,"select * from gaa.gaa_fx_fowards")
  fxforwards_gui=read_delim(url('https://raw.githubusercontent.com/fscheler/opendata/main/fx_forwards.csv'),delim = ";",show_col_types = FALSE)
  fxforwards_gui<- as_tibble(fxforwards_gui[duplicated(fxforwards_gui$Dates)==FALSE,])

  gui<-list(
    "fxrates_gui"=fxrates_gui,
    "fxforwards_gui"=fxforwards_gui
  )

  options(warn = -1)
  `%ni%` <- Negate(`%in%`)
  #Calculate FX Rates Depending on Base Currency

  #Calculate FX Forward Rates in %
  forwards_perc<-cbind(gui$fxforwards_gui[names(gui$fxforwards_gui) %in% c("Dates","EURJPY")],
                       gui$fxforwards_gui[names(gui$fxforwards_gui) %ni% c("EURJPY","Dates")]/10000)
  forwards_perc[,"EURJPY"]<-forwards_perc[,"EURJPY"]/100
  forwards_perc<-forwards_perc[,names(gui$fxrates_gui)]
  forwards_perc<-cbind(forwards_perc[,"Dates"],
                       as.data.table(forwards_perc)[,-"Dates"]/as.data.table(gui$fxrates_gui)[,-"Dates"])
  names(forwards_perc)<-names(gui$fxrates_gui)


  #base_currency<-"CHF"
  if(base_currency!="EUR")
  {
    #Translate FX Rates
    selcol<-as.character(paste0("EUR",base_currency))

    gui$fxrates_gui<-as.data.table(gui$fxrates_gui)
    gui$fxrates_gui<-cbind(gui$fxrates_gui[,"Dates"],gui$fxrates_gui[,-"Dates"]/as.matrix(subset(gui$fxrates_gui,select=(selcol))))
    names(gui$fxrates_gui)<-gsub("EUR",base_currency,names(gui$fxrates_gui))
    names(gui$fxrates_gui)[names(gui$fxrates_gui)==paste0(base_currency)]<-paste0(base_currency,"EUR")
    names(gui$fxrates_gui)[names(gui$fxrates_gui)==paste0(base_currency,base_currency)]<-base_currency

    #Translate FX Forward Rates
    forwards_perc<-as.data.table(forwards_perc)
    forwards_perc<-cbind(forwards_perc[,"Dates"],-forwards_perc[,-"Dates"]+as.matrix(subset(forwards_perc,select=(selcol))))
    names(forwards_perc)<-gsub("EUR",base_currency,names(gui$fxforwards_gui))
    names(forwards_perc)[names(forwards_perc)==paste0(base_currency)]<-paste0(base_currency,"EUR")
    names(forwards_perc)[names(forwards_perc)==paste0(base_currency,base_currency)]<-base_currency

    #Hedge Performance (Information only as hedging costs are deducted later)
    divide_head<-function(x)
    { x<-x/head(x,1) }
    cumprod_apply<-function(x)
    { x<-cumprod(1+x) }

    fx_performance<-apply(as.data.table(gui$fxrates_gui)[,-"Dates"],2,divide_head)
    #plot(fx_performance[,1],type="l")

    hedge_cost<-apply(as.data.table(forwards_perc[,-"Dates"]/252),2,cumprod_apply)
    #plot(hedge_cost[,1],type="l")

    if(just_convert==T)
    {
      hedge_perf<-fx_performance
    }else{
      hedge_perf<-fx_performance*hedge_cost
    }
    hedge_perf<-cbind(gui$fxrates_gui[,"Dates"],hedge_perf)
    #plot(hedge_perf$CHFEUR,type="l")
    #plot(hedge_perf$CHFUSD,type="l")

  }else{

    divide_head<-function(x)
    { x<-x/head(x,1) }
    cumprod_apply<-function(x)
    { x<-cumprod(1+x) }

    fx_performance<-apply(as.data.table(gui$fxrates_gui)[,-"Dates"],2,divide_head)
    hedge_cost<-apply(as.data.table(forwards_perc[,-"Dates"])/252,2,cumprod_apply)
    if(just_convert==T)
    {
      hedge_perf<-fx_performance
    }else{
      hedge_perf<-fx_performance/hedge_cost
    }
    hedge_perf<-cbind(gui$fxrates_gui[,"Dates"],as.data.frame(hedge_perf))
    colnames(hedge_perf)[1]<-"Dates"
  }

  hedge_perf<-subset(hedge_perf,select=c('Dates',paste0(base_currency,exp_currency)))
  names(hedge_perf)[names(hedge_perf)==paste0(base_currency,exp_currency)]<-"Hedge"
  hedge_perf<-hedge_perf[,c("Dates","Hedge")]

  hedge_cost_perc<-subset(forwards_perc,select=c("Dates",paste0(base_currency,exp_currency)))
  names(hedge_cost_perc)<-c("Dates","Hedge")
  hl<-list('hedge_perf'=hedge_perf,'forwards_perc'=hedge_cost_perc)

  options(warn=0)

  return(hl)

}

#df<-FXhedge()
#tail(df$forwards_perc,1)


