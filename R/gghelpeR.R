

library(svglite)

#----------------------------------------------------------------------------------------------------------------------------
get_db_connection<-function(dbname)
{
  library(pool)
  # pool connection
  pool <<- dbPool(
    drv = RMySQL::MySQL(),
    dbname = dbname,
    #host = "167.99.95.195",
    host = "172.40.0.17",
    username = "root",
    #password = "Amad3usCap1tal2018",
    password = "AmCap2019*",
    port=3306
  )
}

col_w_light<-function(number_cols)
{
  library("RColorBrewer")
  #display.brewer.pal(n = 8, name = 'RdBu')
  cols<-brewer.pal(n = number_cols, name = 'RdBu')
  cols[cols=="#2166AC"]<-"#297EBF"
  cols[cols=="#67001F"]<-"#7e1206"
  return(cols)
}

#----------------------------------------------------------------------------------------------------------------------------
add_bear_shade<-function(st_date,ed_date,shade_color="darkgray",threshold=0.1,mode="runmax",days=252)
{
  #st_date<-"2000-01-01"
  #ed_date<-Sys.Date()
  library(fredr)
  library(ecm)
  library(ggplot2)
  library(dplyr)
  library(tidyverse)
  #mb <- read_csv(paste0("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/","market_breadth.csv"))
  mb <- read_csv("https://cloud.amadeusquantamental.lu/owncloud/index.php/s/gka11HDskZE2rPA/download")
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
    rec_shade<-list("rec_shade"=rec_shade,"mb"=mb)
    return(rec_shade)
  }
}

#----------------------------------------------------------------------------------------------------------------------------
add_rec_shade<-function(st_date,ed_date,shade_color="darkgray")
{
  library(fredr)
  library(ecm)
  library(ggplot2)
  library(dplyr)
  fredr_set_key("267ec659142c1bbeb7637d657488acaa")

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



theme_w_small<-
  function (base_size = 12, base_family = "")
  {
    library(ggthemes)
    library(scales)
    library(extrafont)
    theme_hc(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(color = "grey20", size = 9,family="Calibri"),
        axis.text.y = element_text(color = "grey20", size = 9,family="Calibri"),
        axis.title.x = element_text(color = "grey20", size = 10,family="Calibri"),
        axis.title.y = element_text(color = "grey20", size = 10,family="Calibri"),
        plot.title = element_text(color="#297EBF", size=10, face="bold",family="Calibri"),
        legend.text = element_text(color = "grey20", size = 9,family="Calibri")
      )
  }


#----------------------------------------------------------------------------------------------------------------------------
theme_w_emma<-
  function (base_size = 12, base_family = "",font_type="Calibri",cols_emma)
  {
    library(ggplot2)
    library(ggthemes)
    library(scales)



    theme_hc(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(color = "grey20", size = base_size-1,family=font_type),
        axis.text.y.left = element_text(color = cols_emma[1], size = base_size-1,family=font_type),
        axis.text.y.right = element_text(color = cols_emma[2], size = base_size-1,family=font_type),
        axis.title.x = element_text(color = "grey20", size = base_size,family=font_type),
        axis.title.y.left = element_text(color = cols_emma[1],size = base_size-1,family=font_type),
        axis.title.y.right = element_text(color = cols_emma[2], size = base_size-1,family=font_type),
        plot.title = element_text(hjust=0,color=cols_emma[1], size=base_size+1, face="bold",family=font_type),
        legend.text = element_text(color = "grey20", size = base_size,family=font_type),
        plot.caption = element_text(color = cols_emma[1], family=font_type, face = 'bold',hjust=1),
        plot.subtitle = element_text(hjust=0,color=cols_emma[2], size=base_size-2,family=font_type),

        panel.background = element_rect(fill = "#ecf7fa", color = "#ecf7fa"), # bg of the panel
        plot.background = element_rect(fill = "#ecf7fa", color = "#ecf7fa"), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        panel.grid.major.y = element_line(colour = "white",linetype="dashed",size=1),
        legend.background = element_rect(fill = "transparent",color = 'white'), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent",color="transparent")
      )
  }

#----------------------------------------------------------------------------------------------------------------------------
theme_w<-
  function (base_size = 12, base_family = "",font_type="Calibri")
  {
    library(ggplot2)
    library(ggthemes)
    library(scales)
    col_aq2<-as.character(c("#297EBF","#7D1206","#1e1e1e","#3b5171","#5777a7","#04103b","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
    theme_hc(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(color = "grey20", size = base_size-1,family=font_type),
        axis.text.y.left = element_text(color = col_aq2[1], size = base_size-1,family=font_type),
        axis.text.y.right = element_text(color = col_aq2[2], size = base_size-1,family=font_type),
        axis.title.x = element_text(color = "grey20", size = base_size,family=font_type),
        axis.title.y.left = element_text(color = col_aq2[1],size = base_size-1,family=font_type),
        axis.title.y.right = element_text(color = col_aq2[2], size = base_size-1,family=font_type),
        plot.title = element_text(hjust=0,color=col_aq2[1], size=base_size+1, face="bold",family=font_type),
        legend.text = element_text(color = "grey20", size = base_size,family=font_type),
        plot.caption = element_text(color = col_aq2[1], family=font_type, face = 'bold',hjust=1),
        plot.subtitle = element_text(hjust=0,color=col_aq2[2], size=base_size-2,family=font_type),

        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent",color = 'white'), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent",color="transparent")
      )
  }

theme_w_black<-
  function (base_size = 12, base_family = "",font_type="Calibri")
  {
    library(ggplot2)
    library(ggthemes)
    library(scales)
    col_aq2<-as.character(c("#297EBF","#7D1206","#1e1e1e","#3b5171","#5777a7","#04103b","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
    theme_hc(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.text.y.left = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.text.y.right = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.title.x = element_text(color = col_aq2[3], size = base_size,family=font_type),
        axis.title.y.left = element_text(color = col_aq2[3],size = base_size-1,family=font_type),
        axis.title.y.right = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        plot.title = element_text(hjust=0,color=col_aq2[1], size=base_size+1, face="bold",family=font_type),
        legend.text = element_text(color = col_aq2[3], size = base_size,family=font_type),
        plot.caption = element_text(color = col_aq2[1], family=font_type, face = 'bold',hjust=1),
        plot.subtitle = element_text(hjust=0,color=col_aq2[2], size=base_size-2,family=font_type),

        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent",color = 'white'), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent",color="transparent"),
        rect = element_rect(fill = "transparent")
      )
  }

theme_aq_black<-
  function (base_size = 12, base_family = "",font_type="Calibri")
  {
    library(ggplot2)
    library(ggthemes)
    library(scales)
    #col_aq2<-as.character(c("#297EBF","#7D1206","#1e1e1e","#3b5171","#5777a7","#04103b","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
    col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))

    theme_hc(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.text.y.left = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.text.y.right = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.title.x = element_text(color = col_aq2[3], size = base_size,family=font_type),
        axis.title.y.left = element_text(color = col_aq2[3],size = base_size-1,family=font_type),
        axis.title.y.right = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        plot.title = element_text(hjust=0,color=col_aq2[1], size=base_size+1, face="bold",family=font_type),
        legend.text = element_text(color = col_aq2[3], size = base_size,family=font_type),
        plot.caption = element_text(color = col_aq2[1], family=font_type, face = 'bold',hjust=1),
        plot.subtitle = element_text(hjust=0,color=col_aq2[2], size=base_size-2,family=font_type),

        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent",color = 'white'), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent",color="transparent")
      )
  }

theme_aq_black_default_font<-
  function (base_size = 12, base_family = "")
  {
    library(ggplot2)
    library(ggthemes)
    library(scales)
    #col_aq2<-as.character(c("#297EBF","#7D1206","#1e1e1e","#3b5171","#5777a7","#04103b","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
    col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))

    theme_hc(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(color = col_aq2[3], size = base_size-1),
        axis.text.y.left = element_text(color = col_aq2[3], size = base_size-1),
        axis.text.y.right = element_text(color = col_aq2[3], size = base_size-1),
        axis.title.x = element_text(color = col_aq2[3], size = base_size),
        axis.title.y.left = element_text(color = col_aq2[3],size = base_size-1),
        axis.title.y.right = element_text(color = col_aq2[3], size = base_size-1),
        plot.title = element_text(hjust=0,color=col_aq2[1], size=base_size+1, face="bold"),
        legend.text = element_text(color = col_aq2[3], size = base_size),
        plot.caption = element_text(color = col_aq2[1], face = 'bold',hjust=1,size=base_size/2),
        #plot.caption = element_text(color = col_aq2[1],hjust=1),
        plot.subtitle = element_text(hjust=0,color=col_aq2[2], size=base_size-2),

        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent",color = 'white'), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent",color="transparent"),
        rect = element_rect(fill = "transparent")
      )
  }

theme_aq_black_default_font_no_ticks<-
  function (base_size = 12, base_family = "")
  {
    library(ggplot2)
    library(ggthemes)
    library(scales)
    #col_aq2<-as.character(c("#297EBF","#7D1206","#1e1e1e","#3b5171","#5777a7","#04103b","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
    col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))

    theme_hc(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.title.x = element_text(color = col_aq2[3], size = base_size),
        axis.title.y.left = element_text(color = col_aq2[3],size = base_size-1),
        axis.title.y.right = element_text(color = col_aq2[3], size = base_size-1),
        plot.title = element_text(hjust=0,color=col_aq2[1], size=base_size+1, face="bold"),
        legend.text = element_text(color = col_aq2[3], size = base_size),
        plot.caption = element_text(color = col_aq2[1], face = 'bold',hjust=1),
        plot.subtitle = element_text(hjust=0,color=col_aq2[2], size=base_size-2),

        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent",color = 'white'), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent",color="transparent")
      )
  }



theme_aq_black_pie<-
  function (base_size = 12, base_family = "",font_type="Calibri")
  {
    library(ggplot2)
    library(ggthemes)
    library(scales)
    #col_aq2<-as.character(c("#297EBF","#7D1206","#1e1e1e","#3b5171","#5777a7","#04103b","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
    col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))

    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid  = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      plot.title = element_text(hjust=0,color=col_aq2[1], size=base_size+1, face="bold"),
      plot.caption = element_text(color = col_aq2[1], face = 'bold',hjust=1),
      plot.subtitle = element_text(hjust=0,color=col_aq2[2], size=base_size-2),
      legend.text = element_text(color = col_aq2[3], size = base_size)
    )

  }



add_w_logo<-function()
{
  library(png)
  library(gridExtra)
  library(grid)
  logo = readPNG("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/wlogo1.png")
  grid::grid.raster(logo, x = 0.005, y = 0.01, just = c('left', 'bottom'), width = unit(0.8, 'inches'))
}

add_aq_logo<-function()
{
  library(png)
  library(gridExtra)
  library(grid)
  logo = readPNG("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/AMADEUS_logo_quantamental.png")
  grid::grid.raster(logo, x = 0.005, y = 0.01, just = c('left', 'bottom'), width = unit(1.4, 'inches'))
}

add_ac_logo_old<-function()
{
  library(png)
  library(gridExtra)
  library(grid)
  logo = readPNG("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/amadeus_logo.png")
  grid::grid.raster(logo, x = 0.005, y = 0.01, just = c('left', 'bottom'), width = unit(1.1, 'inches'),name="with_logo")
}

add_ac_logo<-function(p1)
{
  library(png)
  library(gridExtra)
  library(grid)
  logo = readPNG("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/amadeus_logo.png")
  p2<-grid.arrange(p1,grid::rasterGrob(logo, x = 0.005, y = 1.1, just = c('left', 'bottom'), width = unit(1.2, 'inches'),name="with_logo"),heights=c(4,0))
}
#----------------------------------------------------------------------------------------------------------------------------
get_w_color<-function()
{
  col_w<-as.character(c("#297EBF","#7D1206","#1e1e1e","#3b5171","#5777a7","#04103b","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
  return(col_w)
}

get_aq_color<-function()
{
  col_aq<-as.character(c("#04103b","#dd0400","#3b5171","#D6604D","#4393C3","#BDBDBD","#D9D9D9","#F0F0F0"))
  return(col_aq)
}
#----------------------------------------------------------------------------------------------------------------------------
prepare_for_chart<-function(df,mnemonic_vector,id_var)
{
  library(ggthemes)
  library(ggplot2)
  library(reshape2)
  library(data.table)

  df_s<-df[,colnames(df) %in% mnemonic_vector]

  long <- melt(setDT(df_s), id.vars = id_var,stringsAsFactors=F)
  names(long)<-c("date","id","value")
  long$date<-as.Date(long$date)
  long$id<-as.character(long$id)
  long$value<-as.numeric(long$value)
  long$value_adapt<-as.numeric(long$value)
  return(long)
}

#Move to secondary axis
#----------------------------------------------------------------------------------------------------------------------------
move_to_secondary_axis<-function(long,id,scaleFactor)
{
  #long<-move_to_secondary_axis(long,"p_price_open",1)
  long$value_adapt<-ifelse(long$id==id,long$value*scaleFactor,long$value)
  return(long)
}



#Add Secondary Axis to Chart
#----------------------------------------------------------------------------------------------------------------------------
add_sec_axis<-function(axis_title_prim,percent_prim=F,axis_title_sec,percent_sec=F,scaleFactor)
{
  library(scales)
  if(percent_prim==T & percent_sec==T)
  { sec_y<-scale_y_continuous(name=axis_title_prim,labels = scales::percent_format(accuracy = 1),sec.axis=sec_axis(~./scaleFactor, name=axis_title_sec,labels = scales::percent_format(accuracy = 1))) }

  if(percent_prim==F & percent_sec==T)
  { sec_y<-scale_y_continuous(name=axis_title_prim,sec.axis=sec_axis(~./scaleFactor, name=axis_title_sec,labels = scales::percent_format(accuracy = 1))) }

  if(percent_prim==T & percent_sec==F)
  { sec_y<-scale_y_continuous(name=axis_title_prim,labels = scales::percent_format(accuracy = 1),sec.axis=sec_axis(~./scaleFactor, name=axis_title_sec)) }

  if(percent_prim==F & percent_sec==F)
  { sec_y<-scale_y_continuous(name=axis_title_prim,sec.axis=sec_axis(~./scaleFactor, name=axis_title_sec)) }

  return(sec_y)
}


#Move to PPT
#----------------------------------------------------------------------------------------------------------------------------
move_to_ppt<-function(my_plot,path="",ppt_layout="Title and Content",ppt_master="Office Theme",left=0.8,top=1.7,width=8,height=4.5)
{
  library(dplyr)
  library(officer)
  library(rvg)
  #https://davidgohel.github.io/officer/articles/offcran/powerpoint.html

  #1) Convert your chart
  editable_graph <- dml(ggobj = my_plot)
  #2) Create a power point
  if(path=="")
  {
    doc <- read_pptx()
  }else{
    doc <- read_pptx(path)
  }
  #If you want to use an existing presentation
  #doc <- read_pptx(target_ppt)
  #3) Add a new slide
  doc <- add_slide(doc, layout = ppt_layout, master = ppt_master)%>%
    #4) Add your plot
    ph_with(dml(ggobj = my_plot),location = ph_location(left = left, top = top, width = width, height = height) )
  #5) Save the new presentation
  print(doc, target = "officer_ppt.pptx")
}



rbind_bbg<-function(df)
{
  for(i in 1:length(df))
  {
    if(i ==1)
    {
      long<-df[[i]]
      long$id<-names(df)[i]
    }else{
      tmp<-df[[i]]
      tmp$id<-names(df)[i]
      long<-rbind(long,tmp)
    }
  }
  return(long)
}

calc_ret<-function(df,id,mnemonic)
{
  library(ecm)
  ret<-ifelse(df[[id]]!=lagpad(df[[id]],k=1),0,df[[mnemonic]]/lagpad(df[[mnemonic]],k=1)-1)
  ret[is.na(ret)]<-0
  return(ret)
}

calc_idx<-function(df,id,mnemonic)
{
  names(df)[names(df)==id]<-"id"
  names(df)[names(df)==mnemonic]<-"mnemonic"
  library(dplyr)
  df<- df %>% group_by(id) %>% mutate(idx = cumprod(1+mnemonic))
  idx<-df$idx
  return(idx)
}


lm_eqn <- function(m){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}



#Export as SVG
#library(svglite)
#For Latex Publication
#ggsave(file="P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/test.svg", plot=p1, width=10, height=6)

#For PowerPoint
#ggsave(file="P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/test.svg", plot=p1, width=10, height=10)



plotly_gauge_charting_function<-function(chart_value=30,chart_title="Economic Situation")
{
  #************Charting Function********************
  #Create Chart
  #file.edit("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/ggplot_functions.R")
  source("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/ggplot_functions.R")
  #file.edit("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/ggplot_functions.R")
  col_w<-get_w_color()
  col_aq<-get_aq_color()

  cols_gr<-c("#632523", "#953735", "#D99694", "#E6B9B8", "#D7E4BD", "#C3D69B", "#77933C", "#4F6228")
  #cols_gr = colorRampPalette(col_gr)(10)

  library(plotly)

  fig <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = chart_value,
    title = list(text = chart_title, font = list(size = 50)),
    delta = list(reference = 1, increasing = list(color = "white")),
    gauge = list(
      axis = list(range = list(NULL, 100), tickwidth = 1, tickcolor = "white",font=list(color="white")),
      bar = list(color = col_aq[1]),
      bgcolor = "white",
      borderwidth = 2,
      bordercolor = "white",
      steps = list(
        list(range = c(0, 12.5), color = cols_gr[1]),
        list(range = c(12.5, 25), color = cols_gr[2]),
        list(range = c(25, 37.5), color = cols_gr[3]),
        list(range = c(37.5, 50), color = cols_gr[4]),
        list(range = c(50, 62.5), color = cols_gr[5]),
        list(range = c(62.5, 75), color = cols_gr[6]),
        list(range = c(75, 87.5), color = cols_gr[7]),
        list(range = c(87.5, 100), color = cols_gr[8])),
      threshold = list(
        line = list(color = col_aq[1], width = 4),
        thickness = 0.75,
        value = 50)))
  fig <- fig %>%
    layout(
      margin = list(l=20,r=30,t=50,b=0),
      paper_bgcolor = "white",
      font = list(color = col_aq[1], family = "Arial"))

  return(fig)

}

save_gauges<-function(es,gauge_name="gauge_economic_situation.svg",target_folder="P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/Market_Balance_Sheet/Output/Charts/")
{
  setwd(target_folder)
  #es<-plotly_gauge_charting_function(chart_value=75,chart_title="Economic Situation")
  orca(es,gauge_name)
}
