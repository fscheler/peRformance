

ggRec<-function(st_date="2007-01-01",ed_date="2015-01-01",fredr_key,shade_color="darkgray")
{
  if (!require("fredr")) install.packages("fredr")
  if (!require("ecm")) install.packages("ecm")
  if (!require("ggplot2")) install.packages("ggplot2")
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("scales")) install.packages("scales")

  library(fredr)
  library(ecm)
  library(ggplot2)
  library(dplyr)
  library(scales)

  fredr_set_key(fredr_key)

  #st_date<-as.Date("2000-12-31")
  #ed_date<-as.Date(Sys.Date())
  #shade_color<-"darkgray"

  recession<-fredr(series_id = "USRECD",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))

  recession$diff<-recession$value-lagpad(recession$value,k=1)
  recession<-recession[!is.na(recession$diff),]
  recession.start<-recession[recession$diff==1,]$date
  recession.end<-recession[recession$diff==(-1),]$date

  if(length(recession.start)>length(recession.end))
  {recession.end<-c(recession.end,Sys.Date())}
  if(length(recession.end)>length(recession.start))
  {recession.start<-c(min(recession$date),recession.start)}

  recs<-as.data.frame(cbind(recession.start,recession.end))
  recs$recession.start<-as.Date(as.numeric(recs$recession.start),origin=as.Date("1970-01-01"))
  recs$recession.end<-as.Date(recs$recession.end,origin=as.Date("1970-01-01"))

  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), fill=shade_color, alpha=0.5)

    return(rec_shade)
  }
}


#----------------------------------------------------------------------------------------------------------------------------
ggBear<-function(mb,st_date="2001-01-01",ed_date="2020-01-01",shade_color="darkgray",threshold=0.1,mode="cummax",days=252)
{
  #st_date<-"2000-01-01"
  #ed_date<-Sys.Date()
  library(fredr)
  library(ecm)
  library(ggplot2)
  library(dplyr)
  library(tidyverse)
  library(caTools)
  library(zoo)
  library(data.table)


  #mb <- read_csv(paste0("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/","market_breadth.csv"))
  mb<-mb[,1:2]
  names(mb)<-c("date","PX_LAST")

  mb<-mb[mb$date>st_date & mb$date<ed_date,]

  if(mode=="runmax")
  {
    mb$max_drawdown<-mb$PX_LAST/runmax(mb$PX_LAST,days,align="right")-1
  }else{
    mb$max_drawdown<-mb$PX_LAST/cummax(mb$PX_LAST)-1
  }
  #plot(mb$PX_LAST/runmax(mb$PX_LAST,250)-1)
  #plot(mb$PX_LAST/cummax(mb$PX_LAST)-1)

  mb$dddummy<-ifelse(mb$max_drawdown<0,1,0)
  mb$ddcount<-ifelse(mb$dddummy==1 & lagpad(mb$dddummy,k=1)==0,1,0)
  mb$ddcount[1]<-0
  mb$dds<-cumsum(mb$ddcount)
  mb$dds<-ifelse(mb$dddummy==0,0,mb$dds)

  mb<-mb %>% group_by(dds) %>% mutate(through=min(max_drawdown))

  mb$dd10<-ifelse(mb$through<(-threshold),1,0)
  mb$regime<-ifelse(mb$dddummy==1 & mb$max_drawdown==mb$through,"recovery",NA)
  mb$regime<-ifelse(mb$dddummy==1 & lagpad(mb$dddummy,k=1)==0,"bear",mb$regime)
  mb<-mb%>%group_by(dds)%>%mutate(regime=na.locf(regime,na.rm=F))
  mb$dd10_bear<-ifelse(mb$through<(-threshold) & mb$regime=="bear",1,0)

  mb$bear_start<-ifelse(mb$dd10_bear==1 & lagpad(mb$dd10_bear,k=1)==0,1,NA)
  mb$bear_end<-ifelse(mb$dd10_bear==0 & lagpad(mb$dd10_bear,k=1)==1,1,NA)
  mb<-as.data.table(mb)

  bear_starts<-(mb[bear_start==1,]$date)
  bear_ends<-(mb[bear_end==1,]$date)
  if(length(bear_starts)>length(bear_starts))
  {
    bear_ends<-c(bear_ends,Sys.Date())
  }
  if(length(bear_starts)>length(bear_starts))
  {
    bear_starts<-tail(bear_starts,length(bear_starts)-1)
  }
  recs<-as.data.frame(cbind(bear_starts,bear_ends),stringsAsFactors=F)
  names(recs)<-c("recession.start","recession.end")
  recs$recession.start<-as.Date(recs$recession.start)
  recs$recession.end<-as.Date(recs$recession.end)

  if(nrow(recs)>0)
  {
    rec_shade<-geom_rect(data=recs, inherit.aes=F, aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), fill=shade_color, alpha=0.5)
    rec_shade<-list("bear_shade"=rec_shade,"mb"=mb)
    return(rec_shade)
  }
}





ggFRED<-function(mnemonic="T10YIE",chart_name,subline,save_name,observation_start_dt="1990-01-01",recession_shading="TRUE",chart_width=10,chart_height=6,base_size_font=22,
                                 target_path="",api_key=NULL)
{
  if(!is.null(api_key))
  {
    

  #install.packages("fredr")
  library(fredr)
  library(dplyr)
  library(xts)
  library(plotly)
  library(ggplot2)
  library(ecm) 
    
  
  #Sys.getenv('FRED_API_KEY')
  fredr_set_key(api_key)
  

  de<-
    fredr(
      series_id = mnemonic,
      observation_start = as.Date(observation_start_dt)
    )
  
  
   
  #p<-plot_ly(de,x=~as.Date(date),y=~value,type='scatter',mode='lines+markers')
  de<-na.locf(de)
  #source("C:/FS/Systems/ggplot_functions.R")
  #Create Chart

  cols <- c("TS" = col_aq2[1])
  p<-
    ggplot(data=de,aes(x=as.Date(date), y=value))
  if(recession_shading=="TRUE")
  {
    p<-p+    
      add_rec_shade(as.Date(min(de$date)),as.Date(Sys.Date()))
  }
  p<-p+
    geom_line(size=1,aes(y=value,color="TS"))+
    scale_colour_manual(values = cols)+
    theme_aq_black(base_size=base_size_font)+
    #size 22 for overleaf
    labs(color='')+
    labs(title=chart_name,subtitle=subline,x ="")+
    labs(caption = 'Source: FRED, Amadeus')+
    guides(colour = guide_legend(nrow = 5))+
    scale_x_date(labels = date_format("%m-%Y"))+
    theme(legend.position = "none")+
    ylab("")+
    #ylim(45,55)+
    theme(plot.margin=margin(5,5,5,5))
  p
  
  if(target_path!="")
  {
    ggsave(paste0(target_path,save_name,".png"),plot = p,width=chart_width,height=chart_height)
    ggsave(paste0(target_path,save_name,".svg"),plot = p,width=chart_width,height=chart_height)    
  }

  }
  return(p)
}
