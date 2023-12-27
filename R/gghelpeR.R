

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


indexR<-function(df,normalization="index",first_value_adj=0)
{
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
  if(normalization=="ln")
  {
    indexfunction<-function(x)
    {
      x<-log(x/lagpad(x,k=1))
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
