

add_rec_shade<-function(st_date="2007-01-01",ed_date="2015-01-01",fredr_key,shade_color="darkgray")
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


