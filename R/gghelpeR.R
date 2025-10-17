



library(svglite)

#----------------------------------------------------------------------------------------------------------------------------

col_w_light<-function(number_cols)
{
  library("RColorBrewer")
  #display.brewer.pal(n = 8, name = 'RdBu')
  cols<-brewer.pal(n = number_cols, name = 'RdBu')
  cols[cols=="#2166AC"]<-"#297EBF"
  cols[cols=="#67001F"]<-"#7e1206"
  return(cols)
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
  function (base_size = 12, base_family = "",font_type="Calibri",titlepos="adjusted")
  {
    library(ggplot2)
    library(ggthemes)
    library(scales)
    #col_aq2<-as.character(c("#297EBF","#7D1206","#1e1e1e","#3b5171","#5777a7","#04103b","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
    col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))


    if(titlepos!="left")
    {
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

    }else{
      theme(
        axis.text.x = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.text.y.left = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.text.y.right = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        axis.title.x = element_text(color = col_aq2[3], size = base_size,family=font_type),
        axis.title.y.left = element_text(color = col_aq2[3],size = base_size-1,family=font_type),
        axis.title.y.right = element_text(color = col_aq2[3], size = base_size-1,family=font_type),
        plot.title = element_text(hjust=0,color=col_aq2[1], size=base_size+1, face="bold",family=font_type),
        legend.text = element_text(color = col_aq2[3], size = base_size,family=font_type),

        plot.caption = element_text(hjust = 0.2), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot", #NEW parameter
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent",color = 'white'), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent",color="transparent")
      )
    }


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



scoreGauge<-function(score_value=30,min_value=0,max_value=100,chart_title="Economic Situation",chart_export_width=600,chart_export_height=400)
{
  #************Charting Function********************
  #Create Chart
  col_aq<-as.character(c("#04103b","#dd0400","#3b5171"))
  cols_gr<-c("#632523", "#953735", "#D99694", "#E6B9B8", "#D7E4BD", "#C3D69B", "#77933C", "#4F6228")
  #cols_gr = colorRampPalette(col_gr)(10)

  library(plotly)

  steps<-(max_value-min_value)/8

  fig <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = score_value,
    title = list(text = chart_title, font = list(size = 50)),
    delta = list(reference = 1, increasing = list(color = "white")),
    gauge = list(
      axis = list(range = list(min_value, max_value), tickwidth = 1, tickcolor = "white",font=list(color="white")),
      bar = list(color = col_aq[1]),
      bgcolor = "white",
      borderwidth = 2,
      bordercolor = "white",
      steps = list(
        list(range = c(min_value, min_value+steps), color = cols_gr[1]),
        list(range = c(min_value+steps, min_value+steps*2), color = cols_gr[2]),
        list(range = c(min_value+steps*2,min_value+steps*3), color = cols_gr[3]),
        list(range = c(min_value+steps*3, min_value+steps*4), color = cols_gr[4]),
        list(range = c(min_value+steps*4, min_value+steps*5), color = cols_gr[5]),
        list(range = c(min_value+steps*5, min_value+steps*6), color = cols_gr[6]),
        list(range = c(min_value+steps*6,min_value+steps*7), color = cols_gr[7]),
        list(range = c(min_value+steps*7,min_value+steps*8), color = cols_gr[8])),
      threshold = list(
        line = list(color = col_aq[1], width = 4),
        thickness = 0.75,
        value = (max_value-min_value)/2)))
  fig <- fig %>%
    layout(
      margin = list(l=20,r=30,t=50,b=0),
      paper_bgcolor = "white",
      font = list(color = col_aq[1], family = "Arial"))

  fig<-fig %>% config(toImageButtonOptions = list( format = "svg",filename = "scoring_gauge",width = chart_export_width,height = chart_export_height))


  return(fig)

}


indexR<-function(df,normalization="index",first_value_adj=0,rolln=NULL,annualization=NULL)
{
  library(caTools)
  lagpad<-function (x, k)
  {
    if (k > 0) {
      return(c(rep(NA, k), x)[1:length(x)])
    }
    else {
      return(c(x[(-k + 1):length(x)], rep(NA, -k)))
    }
  }

  if(normalization=="index")
  {
    indexfunction<-function(x)
    {
      x<-x/head(x,1)
    }
  }
  if(normalization=="index*100")
  {
    indexfunction<-function(x)
    {
      x<-(x/head(x,1))*100
    }
  }
  if(normalization=="drawdown")
  {
    indexfunction<-function(x)
    {
      x<-(x/cummax(x))
    }
  }
  if(normalization=="drawdown-1")
  {
    indexfunction<-function(x)
    {
      x<-(x/cummax(x))-1
    }
  }
  if(normalization=="rundrawdown" & !is.null(rolln))
  {
    indexfunction<-function(x)
    {
      x<-(x/runmax(x,k=rolln,align="right",endrule="NA"))
    }
  }
  if(normalization=="rundrawdown-1" & !is.null(rolln))
  {
    indexfunction<-function(x)
    {
      x<-(x/runmax(x,k=rolln,align="right",endrule="NA"))-1
    }
  }
  if(normalization=="index-1")
  {
    indexfunction<-function(x)
    {
      x<-x/head(x,1)-1
    }
  }
  if(normalization=="1+cumprod")
  {
    indexfunction<-function(x)
    {
      x<-cumprod(1+x)
    }
  }
  if(normalization=="cumprod")
  {
    indexfunction<-function(x)
    {
      x<-cumprod(x)
    }
  }
  if(normalization=="returns")
  {
    indexfunction<-function(x)
    {
      x<-x/lagpad(x,k=1)-1
    }
  }
  if(normalization=="1+returns")
  {
    indexfunction<-function(x)
    {
      x<-x/lagpad(x,k=1)
    }
  }
  if(normalization=="runreturns")
  {
    indexfunction<-function(x)
    {
      x<-x/lagpad(x,k=rolln)-1
    }
  }
  if(normalization=="ln")
  {
    indexfunction<-function(x)
    {
      x<-log(x/lagpad(x,k=1))
    }
  }
  if(normalization=="runsd" & !is.null(rolln)  & !is.null(annualization))
  {
    indexfunction<-function(x)
    {
      x<-x/lagpad(x,k=1)-1
      x<-runsd(x,k=rolln,endrule = "NA",align="right")*annualization^0.5
    }
  }

  if(normalization=="convertnumeric")
  {
    indexfunction<-function(x)
    {
      x<-as.numeric(x)
    }
  }


  dfs<-df[,2:ncol(df)]
  dfs<-as.data.frame(dfs)
  dfs<-apply(dfs,2,indexfunction)

  if(first_value_adj==0 & normalization %in% c("ln","returns"))
  {
    dfs[1,]<-0
  }
  dates<-df[,1]
  values<-dfs
  dfr<-data.frame(dates,values,stringsAsFactors = F)
  names(dfr)<-names(df)
  return(dfr)
}



'%ni%' <- function(x,y)!('%in%'(x,y))


umbRuch<-function(x,l)
{
  repl<-paste0('(.{1,',l,'})(\\s|$)')
  x<-gsub(repl, '\\1\n', x)
  x<-ifelse(substr(x,nchar(x)-0,nchar(x))=="\n",substr(x,0,nchar(x)-1),x)
  return(x)
}

#get_root_path<-function(search_variable="OneDrive")
#{
#  require(rstudioapi,lib="C:/Rlib")
#  require(rstudioapi)
#
#  full_path <- dirname(rstudioapi::getSourceEditorContext()$path)
#  onedrive_path <- sub(paste0("(/",search_variable,"[^/]*).*"), "\\1", full_path)
#  root_path <- sub(paste0("(^.*", onedrive_path, ").*"), "\\1", full_path)
#  return(root_path)
#}

get_root_path <- function(search_variable = "OneDrive", file_path = NULL) {
  # Determine the full path
  full_path <- NULL

  # Option 1: User provides a file path explicitly
  if (!is.null(file_path)) {
    full_path <- normalizePath(file_path, winslash = "/", mustWork = TRUE)
  } else {
    # Option 2: RStudio session
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      context_path <- rstudioapi::getSourceEditorContext()$path
      if (nzchar(context_path)) {
        full_path <- normalizePath(dirname(context_path), winslash = "/", mustWork = TRUE)
      }
    }

    # Option 3: Fallback to working directory
    if (is.null(full_path)) {
      full_path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    }
  }

  # Extract root path based on search_variable
  onedrive_path <- sub(paste0("(/", search_variable, "[^/]*).*"), "\\1", full_path)
  root_path <- sub(paste0("(^.*", onedrive_path, ").*"), "\\1", full_path)

  return(root_path)
}



read_xlsb<-function(wb,sheet)
{
  library(RODBC)
  con2 <- odbcConnectExcel2007(wb)
  data <- sqlFetch(con2, sheet) # Provide name of sheet
}



#install.packages("blastula")
send_conf_email <- function(
    server = "",
    receiver = "",
    sender = "Quantamental Platform Alerts <your_email@example.com>",
    smtp_user = "",
    smtp_password = "",
    smtp_port = 0,
    body_text="",
    subject_text="",
    count_files_path=""
) {
  library(blastula)
  # --- Find the most recent file ---
  folder <- count_files_path
  files <- list.files(folder, full.names = TRUE)

  if (length(files) == 0) {
    arr <- character(0)
  } else {
    latest_file <- files[which.max(file.info(files)$mtime)]
    arr <- basename(latest_file)
  }

  file_count <- length(arr)

  # --- Build message ---
  body <- glue::glue(
    body_text,"\n\n",
    "Data as of: ",as.character(Sys.time())
  )

  Sys.setenv(SMTP_USER =smtp_user)
  Sys.setenv(SMTP_PASSWORD =smtp_password)
  email <- compose_email(
    body = md(body)
  )
  email %>%
    smtp_send(
      from = sender,
      to = receiver,
      subject = subject_text,
      credentials = creds_envvar(
        user = smtp_user,
        pass_envvar = "SMTP_PASSWORD",
        host = server,
        port = smtp_port,
        use_ssl = TRUE
      )
    )


  message("Email sent successfully with subject line!")
}


get_clean_root <- function(search_variable = "OneDrive", file_path = NULL) {
  full_path <- NULL

  # 1. Decide which path to normalize
  if (!is.null(file_path)) {
    full_path <- normalizePath(file_path, winslash = "/", mustWork = TRUE)
  } else {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      context_path <- rstudioapi::getSourceEditorContext()$path
      if (nzchar(context_path)) {
        full_path <- normalizePath(dirname(context_path), winslash = "/", mustWork = TRUE)
      }
    }
    if (is.null(full_path)) {
      full_path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    }
  }

  # 2. Strip out the OneDrive portion (case-insensitive, literal match)
  root_path <- sub(paste0("/", search_variable, "[^/]*"), "", full_path, ignore.case = TRUE)

  # 3. Return only up to the user directory (C:/Users/Name)
  # e.g. remove everything after "C:/Users/<username>"
  root_path <- sub("^((?:.*/Users/[^/]+)).*$", "\\1", root_path)

  return(root_path)
}





sftpZIPReader<-function(sftp_file_path="",sftp_user="",sftp_password="",clean_up="Yes")
{
  library(curl)

  # Remote SFTP path to the ZIP
  sftp_file = paste0("sftp://",sftp_user,":",sftp_password,"@",sftp_file_path)

  # Local temporary path
  local_zip <- tempfile(fileext = ".zip")

  # Download the ZIP
  curl_download(sftp_file, local_zip)

  # List contents of the ZIP
  files_in_zip <- unzip(local_zip, list = TRUE)$Name

  # Keep only CSVs
  csv_files <- files_in_zip[grepl("\\.csv$", files_in_zip, ignore.case = TRUE)]

  # Extract and read A1 from each CSV inside the ZIP
  results <- lapply(csv_files, function(f) {
    val <- read.csv(unz(local_zip, f), header = FALSE, nrows = 1)[1,1]
    data.frame(file = f, A1 = val, stringsAsFactors = FALSE)
  })

  # Combine results
  final_df <- do.call(rbind, results)

  # Clean up temporary ZIP
  if(clean_up=="Yes")
  {
    unlink(local_zip)
    resl<-list("contents"=final_df)
  }else{
    resl<-list("contents"=final_df,local_zip=local_zip)
  }



  return(resl)

}


write_with_varchar_margin <- function(con, table_name, df, overwrite = TRUE, varchar_margin = 50) {
  # Compute maximum observed string length per character/factor column
  max_len <- sapply(df, function(x) if (is.character(x)) max(nchar(x), na.rm = TRUE) else NA)

  # Build SQL type definitions
  field.types <- sapply(names(df), function(col) {
    x <- df[[col]]
    if (is.character(x)) {
      base_len <- ifelse(is.finite(max_len[col]), max_len[col], 0)
      paste0("VARCHAR(", base_len + varchar_margin, ")")
    } else if (is.factor(x)) {
      levels_len <- max(nchar(levels(x)), na.rm = TRUE)
      paste0("VARCHAR(", levels_len + varchar_margin, ")")
    } else if (is.numeric(x)) {
      "DOUBLE"
    } else if (inherits(x, "Date")) {
      "DATE"
    } else if (inherits(x, "POSIXt")) {
      "DATETIME"
    } else if (is.logical(x)) {
      "TINYINT(1)"
    } else {
      "TEXT"  # fallback
    }
  }, USE.NAMES = TRUE)

  # Ensure it's a named character vector
  field.types <- as.character(field.types)
  names(field.types) <- names(df)

  # Write the table
  DBI::dbWriteTable(con, table_name, df, overwrite = overwrite, field.types = field.types)

  message(sprintf("Table '%s' written successfully with +%d character margin for VARCHAR columns.",
                  table_name, varchar_margin))
}
