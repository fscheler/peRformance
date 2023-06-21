
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



mperfT<-function(da,ts_format="returns",header_color="#3b5171",header_font="white",font_color="#04103b",
                      export_format="svg",chart_export_width=800,chart_export_height=400,print_output=T,bm_name="Benchmark",
                 rowOddColor="white",
                 rowEvenColor="lightgrey"
                 )
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
    tryCatch({
      da$benchmark_ret<-da$benchmark_ret/lagpad(da$benchmark_ret,k=1)-1
      da$benchmark_ret[1]<-0
    }, error=function(e){})
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




  if(bm_dummy==2)
  {

  fig <- plot_ly(
    #height=chart_height,
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
      font = list(color = header_font, size = 7)
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
      #height=chart_height,
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
                   paste0('<b>',bm_name,'</b>')
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
  fig <- fig %>% config(toImageButtonOptions = list( format = export_format,filename = "monthly_returns_table",width = chart_export_width,height = chart_export_height))

  if(print_output==T)
  {
    print(printtable)
  }


  fig_list<-list("fig"=fig,"printtable"=printtable)

}



#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



mperfTa<-function(df,ts_format="returns",rounding=2,header_color="#3b5171",header_font="white",font_color="#04103b",
                 export_format="svg",chart_export_width=800,chart_export_height=400,print_output=T,
                 rowOddColor="white",
                 rowEvenColor="lightgrey",
                 orders=c("Year","Amadeus Very Defensive USD","Amadeus Defensive USD","Amadeus Balanced USD","Amadeus Dynamic USD","Amadeus Very Dynamic USD")
)
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


  
  if(ts_format=="index")
  {
    names(df)<-c("id","date","nav")
    df<-df%>%group_by(id)%>%mutate(ret=nav/lagpad(nav,k=1)-1)    
  }else{
    names(df)<-c("id","date","ret")
  }

  df$ret<-ifelse(is.na(df$ret),0,df$ret)
  df$Year<-year(as.Date(df$date))
  df<-df%>%group_by(id,Year)%>%mutate(aret=cumprod(1+ret))
  df<-df%>%group_by(id,Year)%>%summarise(aret=tail(aret,1)-1)
  dw<-dcast(df,Year~id,value.var="aret")
  
  formating<-function(x)
  {
    x<-paste0(round(as.numeric(x)*100,rounding),"%")
  }
  
  
  #orders<-c("Year","Amadeus Very Defensive USD","Amadeus Defensive USD","Amadeus Balanced USD","Amadeus Dynamic USD","Amadeus Very Dynamic USD")
  dw<-dw[,c(orders)]
  
  dv<-cbind(dw$Year,apply(dw[,2:ncol(dw)],2,formating))
  names(dv)<-orders
  headerColor<-header_color

    
    fig <- plot_ly(
      #height=chart_height,
      type = 'table',
      columnwidth  = c(60,rep(80,12)),
      header = list(
        values = paste0("<b>",names(dw),"</b>"),
        line = list(color = 'white'),
        fill = list(color = headerColor),
        #align = c(rep('left',3),rep('center',7)),
        font = list(color = header_font, size = 7)
      ),
      cells = list(
        height = 16,
        values = t(dv),
        line = list(color = 'white'),
        fill = list(color = list(rep(c(rowOddColor,rowEvenColor),length(dw$Year)/2+1))),
        align = c('center', 'center','center','center','center', 'center','center', 'center'),
        font = list(color = c(font_color), size = 6)
        #font = list(color = list(list(c("red","green")),), size = 6.5),
        
      ))

  
  m<-list(r=0,b=0,t=0,l=0,par=4)
  fig<-fig%>%layout(margin=m)
  fig <- fig %>% config(toImageButtonOptions = list( format = export_format,filename = "annual_returns_table",width = chart_export_width,height = chart_export_height))
  

  fig_list<-list("fig"=fig)
  
}




createValueBoxes <- function(df, h = 4, w = 6, padding=0.5, rows = 2,cols){
  # required packages
  library(ggplot2)
  library(emojifont)
  # verify our inputs
  if (!is.data.frame(df)) {
    stop(paste("Argument", deparse(substitute(df)), "must be a data.frame."))
  }
  if(!all(i <- rlang::has_name(df,c("values", "infos", "icons")))){
    stop(sprintf(
      "%s does not contain: %s",
      deparse(substitute(df)),
      paste(columns[!i], collapse=", ")))
  }
  
  boxes = nrow(df) # number of items passed
  # calculate the grid
  cols = boxes/rows
  plotdf <- data.frame(
    x = rep(seq(0, (w+padding)*cols-1, w+padding), times=rows),
    y = rep(seq(0, (h+padding)*rows-1, h+padding), each=cols),
    h = rep(h, boxes),
    w = rep(w, boxes),
    value = df$values,
    info = df$infos,
    icon = fontawesome(df$icons),
    font_family = c(rep("fontawesome-webfont", boxes)),
    color = factor(1:boxes)
  )
  print(plotdf)
  ggplot(plotdf, aes(x, y, height = h, width = w, label = info)) +
    ## Create the tiles using the `color` column
    geom_tile(aes(fill = color)) +
    ## Add the numeric values as text in `value` column
    geom_text(color = "white", fontface = "bold", size = 10,
              aes(label = value, x = x - w/2.2, y = y + h/4), hjust = 0) +
    ## Add the labels for each box stored in the `info` column
    geom_text(color = "white", fontface = "bold",
              aes(label = info, x = x - w/2.2, y = y-h/4), hjust = 0) +
    coord_fixed() +
    scale_colour_manual(values = cols)+
    #scale_fill_brewer(type = "qual",palette = "Dark2") +
    ## Use `geom_text()` to add the icons by specifying the unicode symbol.
    geom_text(size = 20, aes(label = icon, family = font_family,
                             x = x + w/4, y = y + h/8), alpha = 0.25) +
    theme_void() +
    guides(fill = FALSE)
  
} 
