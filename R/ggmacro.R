lagpad <- function(x, k=1) {
  i<-is.vector(x)
  if(is.vector(x)) x<-matrix(x) else x<-matrix(x,nrow(x))
  if(k>0) {
    x <- rbind(matrix(rep(NA, k*ncol(x)),ncol=ncol(x)), matrix(x[1:(nrow(x)-k),], ncol=ncol(x)))
  }else {
    x <- rbind(matrix(x[(-k+1):(nrow(x)),], ncol=ncol(x)),matrix(rep(NA, -k*ncol(x)),ncol=ncol(x)))
  }
  if(i) x[1:length(x)] else x
}

ggRec<-
  function (st_date = "2007-01-01", ed_date = "2015-01-01", fredr_key,
            shade_color = "darkgray",yminmanual=-Inf,ymaxmanual=+Inf)
  {
    if (!require("fredr"))
      install.packages("fredr")
    if (!require("ggplot2"))
      install.packages("ggplot2")
    if (!require("dplyr"))
      install.packages("dplyr")
    if (!require("scales"))
      install.packages("scales")
    library(fredr)
    library(ggplot2)
    library(dplyr)
    library(scales)
    fredr_set_key(fredr_key)
    recession <- fredr(series_id = "USRECD", observation_start = as.Date(st_date),
                       observation_end = as.Date(ed_date))
    recession$diff <- recession$value - lagpad(recession$value,
                                               k = 1)
    recession <- recession[!is.na(recession$diff), ]
    recession.start <- recession[recession$diff == 1, ]$date
    recession.end <- recession[recession$diff == (-1), ]$date
    if (length(recession.start) > length(recession.end)) {
      recession.end <- c(recession.end, ed_date)
    }
    if (length(recession.end) > length(recession.start)) {
      recession.start <- c(min(recession$date), recession.start)
    }
    recs <- as.data.frame(cbind(recession.start, recession.end))
    recs$recession.start <- as.Date(as.numeric(recs$recession.start),
                                    origin = as.Date("1970-01-01"))
    recs$recession.end <- as.Date(recs$recession.end, origin = as.Date("1970-01-01"))
    if (nrow(recs) > 0) {
      rec_shade <- geom_rect(data = recs, inherit.aes = F,
                             aes(xmin = recession.start, xmax = recession.end,
                                 ymin =yminmanual, ymax = ymaxmanual), fill = shade_color,
                             alpha = 0.5)
      return(rec_shade)
    }
  }


plotlyRec<-function(p,st_date="2007-01-01",ed_date="2015-01-01",fredr_key,shade_color="lightgrey")
{
  #if (!require("fredr")) install.packages("fredr")
  #if (!require("plotly")) install.packages("plotly")

  library(fredr)
  library(ggplot2)

  fredr_set_key(fredr_key)

  recession<-fredr(series_id = "USRECD",observation_start = as.Date(st_date),observation_end = as.Date(ed_date))
  Noax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    overlaying = "y",
    side = "right",
    range=c(0,1)
  )
  if(nrow(recession)>0)
  {
    p<-p%>%
      add_trace(x=~recession$date,y=~recession$value,type="scatter",alpha = 1,mode="none", opacity=1, stackgroup = 'one',showlegend = FALSE, yaxis = "y2",line=list(width = 0.1,color=shade_color),marker = list(size = 0.1,color = shade_color))%>%
      layout(yaxis2 = Noax)

    return(p)
  }
}



#----------------------------------------------------------------------------------------------------------------------------
ggBear<-
  function (mb, st_date = "2001-01-01", ed_date = "2020-05-01",
            shade_color = "grey", threshold = 0.1, mode = "cummax", days = 252)
  {
    library(fredr)
    library(ggplot2)
    library(dplyr)
    library(caTools)
    library(zoo)
    library(data.table)
    mb <- mb[, 1:2]
    names(mb) <- c("date", "PX_LAST")
    mb$date <- as.Date(mb$date)
    mb <- mb[mb$date > st_date & mb$date < ed_date, ]
    if (mode == "runmax") {
      mb$max_drawdown <- mb$PX_LAST/runmax(mb$PX_LAST, days,
                                           align = "right") - 1
    }else {
      mb$max_drawdown <- mb$PX_LAST/cummax(mb$PX_LAST) - 1
    }
    mb$dddummy <- ifelse(mb$max_drawdown < 0, 1, 0)
    mb$ddcount <- ifelse(mb$dddummy == 1 & lagpad(mb$dddummy,
                                                  k = 1) == 0, 1, 0)
    mb$ddcount[1] <- 0
    mb$dds <- cumsum(mb$ddcount)
    mb$dds <- ifelse(mb$dddummy == 0, 0, mb$dds)
    mb <- mb %>% group_by(dds) %>% mutate(through = min(max_drawdown))
    mb$dd10 <- ifelse(mb$through < (-threshold), 1, 0)
    mb$regime <- ifelse(mb$dddummy == 1 & mb$max_drawdown ==
                          mb$through, "recovery", NA)
    mb$regime <- ifelse(mb$dddummy == 1 & lagpad(mb$dddummy,
                                                 k = 1) == 0, "bear", mb$regime)
    mb <- mb %>% group_by(dds) %>% mutate(regime = na.locf(regime,
                                                           na.rm = F))
    mb$dd10_bear <- ifelse(mb$through < (-threshold) & mb$regime ==
                             "bear", 1, 0)
    mb$bear_start <- ifelse(mb$dd10_bear == 1 & lagpad(mb$dd10_bear,
                                                       k = 1) == 0, 1, NA)
    mb$bear_end <- ifelse(mb$dd10_bear == 0 & lagpad(mb$dd10_bear,
                                                     k = 1) == 1, 1, NA)
    mb <- as.data.table(mb)
    bear_starts <- (mb[mb$bear_start == 1, ]$date)
    bear_ends <- (mb[mb$bear_end == 1, ]$date)
    if (length(bear_starts) > length(bear_ends)) {
      bear_ends <- c(bear_ends, ed_date)
    }
    if (length(bear_ends) > length(bear_starts)) {
      bear_ends <- tail(bear_ends, length(bear_ends) - 1)
    }
    bear_starts <- bear_starts[!is.na(bear_starts)]
    bear_ends <- bear_ends[!is.na(bear_ends)]
    recs <- as.data.frame(cbind(bear_starts, bear_ends), stringsAsFactors = F)
    names(recs) <- c("recession.start", "recession.end")
    recs$recession.start <- as.Date(recs$recession.start, origin = "1970-01-01")
    recs$recession.end <- as.Date(recs$recession.end, origin = "1970-01-01")
    print(recs)
    #rec_shade <- geom_rect(data = recs, inherit.aes = F, aes(xmin = recession.start,
    #                                                         xmax = recession.end, ymin = -Inf, ymax = +Inf), fill = shade_color,
    #                       alpha = 0.5)
    if(nrow(recs)>0)
    {
      rec_shade<-geom_rect(data=recs, inherit.aes=F, aes(xmin=recession.start, xmax=recession.end, ymin=-Inf, ymax=+Inf), fill=shade_color, alpha=0.5)
      rec_shade<-list("rec_shade"=rec_shade,"mb"=mb)
      return(rec_shade)
    }
  }




plotlyBear<-function(p,mb,st_date="2001-01-01",ed_date="2023-01-01",shade_color="lightgrey",threshold=0.1,mode="cummax",days=252)
{
  library(fredr)
  library(ggplot2)
  library(dplyr)
  #library(tidyverse)
  library(caTools)
  library(zoo)
  library(data.table)

  mb<-mb[,1:2]
  names(mb)<-c("date","PX_LAST")

  Noax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    overlaying = "y",
    side = "right",
    range=c(0,1),
    layer="below"
  )

  mb<-mb[mb$date>st_date & mb$date<ed_date,]

  if(mode=="runmax")
  {
    mb$max_drawdown<-mb$PX_LAST/runmax(mb$PX_LAST,days,align="right")-1
  }else{
    mb$max_drawdown<-mb$PX_LAST/cummax(mb$PX_LAST)-1
  }

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

  bear_starts <- (mb[mb$bear_start == 1, ]$date)
  bear_ends <- (mb[mb$bear_end == 1, ]$date)
  if (length(bear_starts) > length(bear_ends)) {
    bear_ends <- c(bear_ends, ed_date)
  }
  if (length(bear_ends) > length(bear_starts)) {
    bear_ends <- tail(bear_ends, length(bear_ends) - 1)
  }

  recs <- as.data.frame(cbind(bear_starts, bear_ends), stringsAsFactors = F)
  names(recs) <- c("recession.start", "recession.end")
  recs$recession.start <- as.Date(recs$recession.start,origin="1970-01-01")
  recs$recession.end <- as.Date(recs$recession.end,origin="1970-01-01")


  if(nrow(recs)>0)
  {
    p<-p%>%
      add_trace(x=~mb$date,y=~mb$dd10_bear,type="scatter",alpha = 1,mode="none", opacity=1, stackgroup = 'one',showlegend = FALSE, yaxis = "y2",line=list(width = 0.1,color=shade_color),marker = list(size = 0.1,color = shade_color))%>%
      layout(yaxis2 = Noax)
    return(list("p"=p,"bear_markets"=recs,"mb"=mb))
  }
}


ggFRED<-function(mnemonic="T10YIE",chart_name="10 Year Treasury",subtitle="Yield",save_name="Treasury Yield",observation_start_dt="1990-01-01",recession_shading="TRUE",chart_width=10,chart_height=6,base_size_font=22,
                                 target_path="",cols="#04103b",source='Produced with the peRformance package',fredr_key=NULL)
{
  if(!is.null(fredr_key))
  {


  #install.packages("fredr")
  library(fredr)
  library(dplyr)
  library(xts)
  library(plotly)
  library(ggplot2)
  #library(ecm)


  #Sys.getenv('FRED_API_KEY')
  fredr_set_key(fredr_key)


  de<-
    fredr(
      series_id = mnemonic,
      observation_start = as.Date(observation_start_dt)
    )



  #p<-plot_ly(de,x=~as.Date(date),y=~value,type='scatter',mode='lines+markers')
  de<-na.locf(de)
  #source("C:/FS/Systems/ggplot_functions.R")
  #Create Chart

  cols <- c("TS" = cols[1])
  p<-
    ggplot(data=de,aes(x=as.Date(date), y=value))
  if(recession_shading=="TRUE")
  {
    p<-p+
      ggRec(as.Date(min(de$date)),as.Date(Sys.Date()),fredr_key=fredr_key)
  }
  p<-p+
    geom_line(size=1,aes(y=value,color="TS"))+
    scale_colour_manual(values = cols)+
    theme_aq_black(base_size=base_size_font)+
    #size 22 for overleaf
    labs(color='')+
    labs(title=chart_name,subtitle=subtitle,x ="")+
    labs(caption = source)+
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

  return(p)

  }else{
    print("Missing API Key")
  }
}






ggPretty<-function(dw,chart_name="10 Year Treasury",subtitle="Yield",save_name="Treasury Yield",chart_width=10,chart_height=6,base_size_font=22,
                 target_path="",cols="#04103b",source='Produced with the peRformance package',fredr_key=NULL,colorarea=T)
{



  #install.packages("fredr")
  #library(fredr)
  library(dplyr)
  library(xts)
  library(plotly)
  library(ggplot2)
  #library(ecm)

  names(dw)<-c('date','sums')
  dw$date<-as.Date(dw$date)
  dw<-na.locf(dw)
  # Initiate a ggplot2 chart
  library(ggplot2)
  cols <- c("TS" = "#04103b")
  p<-ggplot(data=dw,aes(x=as.Date(date), y=sums))
  # Add recession shading using the function and your API Key
  # to obtain a key visit: https://fred.stlouisfed.org/docs/api/api_key.html
  tryCatch({
    if(!is.null(fredr_key))
    {
      p<-p + ggRec(as.Date(min(dw$date)),as.Date(max(dw$date)),fredr_key=fredr_key)
    }
  }, error = function(e) {})
  #> NULL
  # add formatting
  library(scales)
  p<-p +
    # add a theme from the peRformance package
    theme_aq_black(base_size=20)+
    #Add other elements
    geom_line(size=1,aes(y=sums,color="TS"))

  if(colorarea==T)
  {
    p<-p+
      geom_area( fill="#3b5171", alpha=1)
  }
  p<-p+
    scale_colour_manual(values = cols)+
    scale_x_date(labels = date_format("%Y"),limits = c(min(dw$date), max(dw$date)), expand = c(0, 0))+
    #scale_x_continuous(limits = c(1986,2014), expand = c(0, 0)) +
    scale_y_continuous( expand = c(0, 0))+
    labs(color='')+
    labs(title="London Metal Exchange Opening Stock",subtitle="Total for Copper, Nickel, Aluminium, Lead, Tin & Zinc",x ="")+
    xlab("")+
    ylab("")+
    theme(legend.position = "none",legend.title = element_blank())+
    theme(plot.margin=margin(l=5,r=10,b=5,t=5))

  return(p)

}





bdhw<-
  function (sec, mnem, st = "2000-01-01", ed = NULL, crncy="",periodicity="")
  {
    library(Rblpapi)
    library(data.table)
    blpConnect()

    opt=NULL
    if(crncy !="")
    {
      #opt <- c("periodicitySelection"="MONTHLY")
      opt <- c("currency"=crncy)
      #df<-bdh(mnemonics,"PX_LAST",start.date = as.Date("2000-07-01"),NULL,options=opt)
    }
    if(periodicity !="")
    {
      #opt <- c("periodicitySelection"="MONTHLY")
      opt <- c("periodicitySelection"=periodicity)
      #df<-bdh(mnemonics,"PX_LAST",start.date = as.Date("2000-07-01"),NULL,options=opt)
    }
    if(periodicity !="" & crncy !="")
    {
      #opt <- c("periodicitySelection"="MONTHLY")
      opt <- c("currency"=crncy,"periodicitySelection"=periodicity)
      #df<-bdh(mnemonics,"PX_LAST",start.date = as.Date("2000-07-01"),NULL,options=opt)
    }


    if (is.null(ed)) {
      df <- bdh(sec, mnem, start.date = as.Date(st), end.date = NULL,options=opt)
    }
    else {
      df <- bdh(sec, mnem, start.date = as.Date(st), end.date = as.Date(ed),options=opt)
    }
    df <- rbindlist(df, id = "id")
    df<-unique(df)
    dw <- data.table::dcast(as.data.table(df), date ~ id, value.var = mnem)
    dw<-dw[,c("date",sec)]
    return(dw)
  }


ggbaRfix<-
  function (df, title = "Title", subtitle = "Subtitle", xcap = "",
            ycap = "", name1 = "Portfolio", perc = T, col_aq2 = c("#04103b",
                                                                  "#dd0400"), fredr_key = NULL)
  {
    library(ggplot2)
    names(df) <- c("date", "asset")
    #df$date <- as.Date(df$date)
    cols <- setNames(c(col_aq2[1]), c(name1))
    p <- ggplot(data = df, aes(x = df$date, y = df$asset))

    p <- p + geom_col(size = 0.8, fill = col_aq2[1]) + theme_aq_black(base_size = 24) +
      labs(color = "") + labs(title = title, subtitle = subtitle,
                              x = xcap) + labs(caption = "") + theme(legend.position = "none",
                                                                     legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(0,
                                                                                                                                            0, 0, 0)) + guides(colour = guide_legend(nrow = 1))
    if (perc == TRUE) {
      p <- p + scale_y_continuous(labels = scales::percent,
                                  expand = c(0, 0))
    }
    p <- p + ylab(ycap) + theme(plot.margin = margin(5, 5, 5,
                                                     5))
    return(p)
  }
