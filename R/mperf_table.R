
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

mperf_table<-function(da,chart_height=500,ts_format="returns",header_color="#3b5171",font_color="#04103b",export_format="svg",chart_width=600,chart_height=400)
{

  if (!require("plotly")) install.packages("plotly")
  if (!require("ecm")) install.packages("ecm")
  if (!require("caTools")) install.packages("caTools")
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("lubridate")) install.packages("lubridate")
  library(plotly)
  library(ecm)
  library(caTools)
  library(dplyr)
  library(lubridate)
  options(warn = -1)


  bm_dummy<-ncol(da)

  if(bm_dummy==2)
  {
    da<-da[,1:2]
    names(da)<-c("date","asset_ret")
  }else{
    da<-da[,1:3]
    names(da)<-c("date","asset_ret","benchmark_ret")
  }


  if(ts_format=="index")
  {
    da$asset_ret<-da$asset_ret/lagpad(da$asset_ret,k=1)-1
    da$asset_ret[1]<-0
    da$benchmark_ret<-da$benchmark_ret/lagpad(da$benchmark_ret,k=1)-1
    da$benchmark_ret[1]<-0
  }

  da$asset_ret<-da$asset_ret+1

  da$yearmon<-substr(da$date,1,7)
  da<-da%>%group_by(yearmon)%>%mutate(mtd=cumprod(asset_ret))
  das <- da %>% group_by(yearmon) %>% do(tail(., n=1))
  da<-da%>%group_by(year(date))%>%mutate(ytd=cumprod(asset_ret))

  if(bm_dummy==3)
  {
    da$benchmark_ret<-da$benchmark_ret+1
    da<-da%>%group_by(year(date))%>%mutate(ytd_bm=cumprod(benchmark_ret))
  }
  dy <- da %>% group_by(year(date)) %>% do(tail(., n=1))


  das$month<-substr(das$date,6,7)
  das$years<-substr(das$date,1,4)


  jan<-das[das$month=="01",c("mtd","years")]
  feb<-das[das$month=="02",c("mtd","years")]
  mar<-das[das$month=="03",c("mtd","years")]
  apr<-das[das$month=="04",c("mtd","years")]
  may<-das[das$month=="05",c("mtd","years")]
  jun<-das[das$month=="06",c("mtd","years")]
  jul<-das[das$month=="07",c("mtd","years")]
  aug<-das[das$month=="08",c("mtd","years")]
  sep<-das[das$month=="09",c("mtd","years")]
  oct<-das[das$month=="10",c("mtd","years")]
  nov<-das[das$month=="11",c("mtd","years")]
  dec<-das[das$month=="12",c("mtd","years")]


  years<-unique(year(das$date))

  ds<-as.data.frame(years,stringsAsFactors=F)
  ds<-merge(ds,jan,by="years",all.x=T)
  ds<-merge(ds,feb,by="years",all.x=T)
  ds<-merge(ds,mar,by="years",all.x=T)
  ds<-merge(ds,apr,by="years",all.x=T)
  ds<-merge(ds,may,by="years",all.x=T)
  ds<-merge(ds,jun,by="years",all.x=T)
  ds<-merge(ds,jul,by="years",all.x=T)
  ds<-merge(ds,aug,by="years",all.x=T)
  ds<-merge(ds,sep,by="years",all.x=T)
  ds<-merge(ds,oct,by="years",all.x=T)
  ds<-merge(ds,nov,by="years",all.x=T)
  ds<-merge(ds,dec,by="years",all.x=T)

  names(ds)<-c("Year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  ds$Ann<-dy$ytd
  if(bm_dummy==3)
  {
    ds$Ann_bm<-dy$ytd_bm
  }



  replace_function<-function(x)
  {

    x<-as.numeric(x)
    x<-ifelse(is.na(x),"",paste0(round(x-1,4)*100,"%"))
    #x<-ifelse(x<0,paste0("<b>",x,"</b>"),x)
    return(x)

  }

  ds$Jan<-replace_function(ds$Jan)
  ds$Feb<-replace_function(ds$Feb)
  ds$Mar<-replace_function(ds$Mar)
  ds$Apr<-replace_function(ds$Apr)
  ds$May<-replace_function(ds$May)
  ds$Jun<-replace_function(ds$Jun)
  ds$Jul<-replace_function(ds$Jul)
  ds$Aug<-replace_function(ds$Aug)
  ds$Sep<-replace_function(ds$Sep)
  ds$Oct<-replace_function(ds$Oct)
  ds$Nov<-replace_function(ds$Nov)
  ds$Dec<-replace_function(ds$Dec)
  ds$Ann<-replace_function(ds$Ann)
  tryCatch({
    ds$Ann_bm<-replace_function(ds$Ann_bm)
  }, error=function(e){})

  headerColor<-header_color
  rowOddColor<-"white"
  rowEvenColor<-"lightgrey"



  if(bm_dummy==2)
  {

  fig <- plot_ly(
    height=chart_height,
    type = 'table',
    columnwidth  = c(60,rep(80,12)),
    header = list(
      values = c('<b>Year</b>',
                 '<b>Jan</b>',
                 '<b>Feb</b>',
                 '<b>Mar</b>',
                 '<b>Apr</b>',
                 '<b>May</b>',
                 '<b>Jun</b>',
                 '<b>Jul</b>',
                 '<b>Aug</b>',
                 '<b>Sep</b>',
                 '<b>Oct</b>',
                 '<b>Nov</b>',
                 '<b>Dec</b>',
                 '<b></b>',
                 '<b>FY</b>'
      ),
      line = list(color = 'white'),
      fill = list(color = headerColor),
      #align = c(rep('left',3),rep('center',7)),
      font = list(color = "white", size = 7)
    ),
    cells = list(
      height = 16,
      values = rbind(
        ds$Year,
        ds$Jan,
        ds$Feb,
        ds$Mar,
        ds$Apr,
        ds$May,
        ds$Jun,
        ds$Jul,
        ds$Aug,
        ds$Sep,
        ds$Oct,
        ds$Nov,
        ds$Dec,
        rep("",nrow(ds)),
        ds$Ann
      ),
      line = list(color = 'white'),
      fill = list(color = list(rep(c(rowOddColor,rowEvenColor),length(ds$Year)/2+1))),
      align = c('center', 'center','center','center','center', 'center','center', 'center'),
      font = list(color = c(font_color), size = 6)
      #font = list(color = list(list(c("red","green")),), size = 6.5),

    ))

  printtable =
    as.data.frame(cbind(
      ds$Year,
      ds$Jan,
      ds$Feb,
      ds$Mar,
      ds$Apr,
      ds$May,
      ds$Jun,
      ds$Jul,
      ds$Aug,
      ds$Sep,
      ds$Oct,
      ds$Nov,
      ds$Dec,
      rep("",nrow(ds)),
      ds$Ann
    ),stringsAsFactors = F)
  names(printtable)<-
    c('Year',
      'Jan',
      'Feb',
      'Mar',
      'Apr',
      'May',
      'Jun',
      'Jul',
      'Aug',
      'Sep',
      'Oct',
      'Nov',
      'Dec',
      '',
      'FY'
    )

  }else{
    fig <- plot_ly(
      height=chart_height,
      type = 'table',
      columnwidth  = c(60,rep(80,12)),
      header = list(
        values = c('<b>Year</b>',
                   '<b>Jan</b>',
                   '<b>Feb</b>',
                   '<b>Mar</b>',
                   '<b>Apr</b>',
                   '<b>May</b>',
                   '<b>Jun</b>',
                   '<b>Jul</b>',
                   '<b>Aug</b>',
                   '<b>Sep</b>',
                   '<b>Oct</b>',
                   '<b>Nov</b>',
                   '<b>Dec</b>',
                   '<b></b>',
                   '<b>FY</b>',
                   '<b>Benchmark</b>'
        ),
        line = list(color = 'white'),
        fill = list(color = headerColor),
        #align = c(rep('left',3),rep('center',7)),
        font = list(color = "white", size = 7)
      ),
      cells = list(
        height = 16,
        values = rbind(
          ds$Year,
          ds$Jan,
          ds$Feb,
          ds$Mar,
          ds$Apr,
          ds$May,
          ds$Jun,
          ds$Jul,
          ds$Aug,
          ds$Sep,
          ds$Oct,
          ds$Nov,
          ds$Dec,
          rep("",nrow(ds)),
          ds$Ann,
          ds$Ann_bm
        ),
        line = list(color = 'white'),
        fill = list(color = list(rep(c(rowOddColor,rowEvenColor),length(ds$Year)/2+1))),
        align = c('center', 'center','center','center','center', 'center','center', 'center'),
        font = list(color = c(font_color), size = 6)
        #font = list(color = list(list(c("red","green")),), size = 6.5),

      ))


    printtable =
      as.data.frame(cbind(
        ds$Year,
        ds$Jan,
        ds$Feb,
        ds$Mar,
        ds$Apr,
        ds$May,
        ds$Jun,
        ds$Jul,
        ds$Aug,
        ds$Sep,
        ds$Oct,
        ds$Nov,
        ds$Dec,
        rep("",nrow(ds)),
        ds$Ann,
        ds$Ann_bm
      ),stringsAsFactors = F)
    names(printtable)<-
      c('Year',
        'Jan',
        'Feb',
        'Mar',
        'Apr',
        'May',
        'Jun',
        'Jul',
        'Aug',
        'Sep',
        'Oct',
        'Nov',
        'Dec',
        '',
        'FY',
        'Benchmark'
      )

  }
  m<-list(r=0,b=0,t=0,l=0,par=4)
  fig<-fig%>%layout(margin=m)
  fig <- fig %>% config(toImageButtonOptions = list( format = export_format,filename = "monthly_returns_table",width = chart_width,height = chart_height))

  print(printtable)

  fig_list<-list("fig"=fig,"printtable"=printtable)

}
