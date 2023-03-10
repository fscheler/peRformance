


#assets<-c("EUR","USD","CHF","GBP","JPY")
#gross<-c(0.3,0.4,0.1,0.05,0.15)
#net<-c(0.45,0.4,0.1,0.05,0.00)
#da<-data.frame(assets,gross,net)
#FXallocBar(da)
#allocBar(da)

#Export table without quotes
noQuoteCSV<-function(pf,path)
{
  write.table(pf,path,sep = ";",row.names = F,col.names=T, quote = FALSE)
}

#NAV
trLine<-function(da,ret_format="returns",chart_title="Performance",chart_height=400,chart_export_width=600,chart_export_height=450,m=list(r=0,l=0,b=0,t=50,par=4))
{
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("plotly")) install.packages("plotly")
  if (!require("lubridate")) install.packages("lubridate")

  library(dplyr)
  library(plotly)
  library(lubridate)

  da<-da[,1:2]

  if(ret_format=="returns")
  {
    names(da)<-c("date","ret")
    da$idx<-cumprod(1+da$ret)
  }else{
    names(da)<-c("date","idx")
  }

  da$date<-as.Date(da$date)
  da<-da%>%group_by(year(date))%>%mutate(ytd=idx/head(idx,1)-1)

  da$ytd_pos<-ifelse(da$ytd>=0,da$ytd,0)
  da$ytd_neg<-ifelse(da$ytd<0,da$ytd,0)


  p<-
    plot_ly(da,x=~date,y=~idx/head(idx,1),type="scatter",mode="line",line=list(color="#04103b"),height=chart_height)%>%
    add_trace(x=~date,y=~ytd_neg,fill="tozeroy", mode = 'none',fillcolor ="#dd0400",line = list(width = 0.01))%>%
    add_trace(x=~date,y=~ytd_pos,fill="tozeroy", mode = 'none',fillcolor = "darkgreen",line = list(width = 0.01))%>%
    layout(margin=m,title=chart_title,xaxis = list(title=""), yaxis = list(title="",tickformat=".0%"),showlegend=F)
  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "allocation_pie",width = chart_export_width,height = chart_export_height))

  return(p)
}



allocBar<-function(da,chart_title="Portfolio Allocation",chart_height=400,chart_font_size=11,chart_export_width=600,chart_export_height=450,m=list(r=0,l=0,b=0,t=50,par=4))
{
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("plotly")) install.packages("plotly")
  if (!require("lubridate")) install.packages("lubridate")

  library(dplyr)
  library(plotly)
  library(lubridate)

  da<-as.data.frame(da)
  da<-da[,1:2]

  names(da)<-c("assets","weight")
  da$weight<-as.numeric(da$weight)
  col_aq2<-as.character(c("#04103b","#dd0400","#3b5171"))
  y_axis_caption<-""
  #chart_font_size<-16

  da<-da%>%group_by(assets)%>%summarize(weight=sum(as.numeric(weight)))

  da$assets <- factor(da$assets, levels = unique(da$assets)[order(as.numeric(da$weight), decreasing = F)])

  p <- plot_ly(da, x = as.numeric(da$weight), y =da$assets ,height=chart_height, type = 'bar', name = 'Portfolio',marker = list(color = col_aq2[1]))
  p <- p %>% layout(margin = m,font=list(size=chart_font_size),title=chart_title, xaxis = list(title=y_axis_caption,tickformat =".0%"), yaxis = list(title=y_axis_caption), barmode = 'group')
  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "allocation_pie",width = chart_export_width,height = chart_export_height))

  return(p)
}


FXallocBar<-function(da,ret_format="returns",chart_title="Portfolio Allocation",chart_height=400,chart_font_size=11,chart_export_width=600,chart_export_height=450,m=list(r=0,l=0,b=0,t=50,par=4))
{

  if (!require("plotly")) install.packages("plotly")
  if (!require("reshape2")) install.packages("reshape2")

  library(plotly)
  library(reshape2)

  da<-da[,1:3]
  names(da)<-c("assets","gross","net")


  col_aq2<-as.character(c("#04103b","#dd0400","#3b5171"))
  y_axis_caption<-""
  #chart_font_size<-16

  da<-da%>%group_by(assets)%>%summarize(gross=sum(as.numeric(gross)),net=sum(as.numeric(net)))

  da$assets <- factor(da$assets, levels = unique(da$assets)[order(as.numeric(da$gross), decreasing = F)])

  p <- da %>% plot_ly()
  p <- p %>% add_trace(x = ~net, y = ~assets, type = 'bar',
                       text = paste0(round(da$net*100),"%"), textposition = 'auto',
                       marker = list(color = '#3b5171',
                                     line = list(color = 'white', width = 1.5)),name="Net")
  p <- p %>% add_trace(x = ~gross, y = ~assets, type = 'bar',
                       text = paste0(round(da$gross*100),"%"), textposition = 'auto',
                       marker = list(color = '#04103b',
                                     line = list(color = 'white', width = 1.5)),name="Gross")
  p <- p %>% layout(margin = m,font=list(size=chart_font_size),title=chart_title, xaxis = list(title=y_axis_caption,tickformat =".0%"), yaxis = list(title=y_axis_caption), barmode = 'group')
  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "allocation_pie",width = chart_export_width,height = chart_export_height))

  return(p)
}


allocPie<-function(df,chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 50,t = 50,pad = 4))
{

  if (!require("scales")) install.packages("scales")
  if (!require("plotly")) install.packages("plotly")
  if (!require("dplyr")) install.packages("dplyr")

  library(scales)
  library(plotly)
  library(dplyr)
  #library(grDevices)

  names(df)<-c("assets","weights")
  df<-df%>%group_by(assets)%>%summarize(weights=sum(weights))
  df$weights<-df$weights/sum(df$weights)

  col_aq2<-as.character(c("#04103b","#5777a7","#D1E2EC","#dd0400"))
  #col_aq2<-as.character(c("#04103b","#D1E2EC","#dd0400"))
  cols = colorRampPalette(col_aq2)(nrow(df))
  #show_col(cols)


  p <- plot_ly(df, labels = ~assets, values = ~weights, type = 'pie',showlegend = FALSE,
               textposition = 'outside',textinfo = 'text',
               hoverinfo = 'text',source = "subset",
               text=~paste(sub(" ","<br>",df$assets),":","<br>",paste0(round(df$weights,2)*100,"%") ),
               insidetextfont = list(color = '#FFFFFF'),
               sort = FALSE
               ,marker = list(colors = cols)) %>%
    layout(title = list(text ='Portfolio', y = 1), margin = m,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "allocation_pie",width = chart_export_width,height = chart_export_height))

  return(p)

}


allocTree<-function(df,parent_label="Portfolio",chart_export_width=600,chart_export_height=450,m = list(l = 0,r = 0,b = 0,t = 0,pad = 4))
{
  if (!require("scales")) install.packages("scales")
  if (!require("plotly")) install.packages("plotly")
  if (!require("dplyr")) install.packages("dplyr")

  library(scales)
  library(plotly)
  library(dplyr)
  #library(grDevices)

  names(df)<-c("assets","weights")
  df<-df%>%group_by(assets)%>%summarize(weights=sum(weights))
  df$weights<-df$weights/sum(df$weights)

  wmp<-df
  wmp$Parent_Label<-parent_label

  regions<-wmp%>%group_by(assets)%>%summarize(sum= sum(weights,na.rm=T))

    library(plotly)

    regions$perc<-regions$sum/sum(regions$sum)
    regions$Parent_Label<-parent_label
    regions<-rbind(regions,c(parent_label,sum(regions$sum),1,""))
    regions<-as.data.frame(regions,stringsAsFactors=F)
    regions$perc<-as.numeric(regions$perc)

    #m<-list(r=0,l=0,b=0,t=0,par=4)

    col_aq2<-as.character(c("#04103b","#5777a7","#D1E2EC","#dd0400"))
    #col_aq2<-as.character(c("#04103b","#D1E2EC","#dd0400"))
    cols = colorRampPalette(col_aq2)(length(unique(regions$assets))-1)
    #show_col(cols)

    tree <- plot_ly(height=250,
                   type="treemap",
                   labels=regions$assets,
                   parents=regions$Parent_Label,
                   values= as.numeric(regions$sum),
                   marker=list(colors=c(cols,'#020b2b')),
                   #width=1100,
                   #height=600,
                   #hovertemplate  = paste(gp$Child_Label, "<br>", gp$IssueValue,"%"),
                   hovertemplate = paste(regions$assets,"<br>",
                                         as.numeric(regions$sum)*100,"%",
                                         '<extra></extra>'),
                   branchvalues="total",
                   outsidetextfont=list(size=22, color= "white"),
                   #insidetextfont=list(size=22),
                   #textfont=list(size=22),
                   text=paste0("",round(regions$perc*100,1),"% <br>")
    )%>%
      layout(margin=m)

    tree<-tree %>% config(toImageButtonOptions = list( format = "svg",filename = "allocation_tree",width = chart_export_width,height = chart_export_height))

    return(tree)

}


rrScat<-function(da,ret_format="returns",table_format='wide',graphics=T,ann_factor=252,chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 80,t = 50,pad = 4))
{

  if (!require("data.table")) install.packages("data.table")
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("ecm")) install.packages("ecm")
  if (!require("lubridate")) install.packages("lubridate")
  if (!require("plotly")) install.packages("plotly")
  if (!require("scales")) install.packages("scales")
  if (!require("reshape2")) install.packages("reshape2")

  library(data.table)
  library(dplyr)
  library(ecm)
  library(lubridate)
  library(plotly)
  library(scales)

  names(da)[1]<-"date"
  da$date<-as.Date(da$date)
  if(table_format=='wide')
  {
    dl <- reshape2::melt(setDT(da), id.vars = "date")
  }else{
    dl<-da
  }

  if(ret_format=="returns")
  {
    names(dl)<-c("date","variable","ret")
    dl<-dl%>%group_by(variable)%>%mutate(idx=cumprod(1+ret))
  }else{
    names(dl)<-c("date","variable","idx")
    dl<-dl%>%group_by(variable)%>%mutate(ret=idx/lagpad(idx,k=1)-1)
    dl$ret<-ifelse(is.na(dl$ret),0,dl$ret)
  }

  #dls <- dl %>% group_by(variable) %>%  do(tail(., n=1))
  dls<-dl%>%group_by(variable)%>%summarise(sd=head(date,1),ld=tail(date,1),std=sd(ret),first=head(idx,1),last=tail(idx,1))

  dls$sd_ann<-dls$std*ann_factor^0.5
  dls$ret_ann<-(dls$last/dls$first)^(1/time_length(difftime(as.Date(dls$ld), as.Date(dls$sd)), "years"))-1


  col_aq2<-as.character(c("#04103b","#5777a7","#D1E2EC","#dd0400"))
  #col_aq2<-as.character(c("#04103b","#D1E2EC","#dd0400"))
  cols = colorRampPalette(col_aq2)(nrow(dls))
  #show_col(cols)


  if(graphics==T)
  {


  rr_plotly<-
    plot_ly(dls, x=~sd_ann, y=~ret_ann,type='scatter',mode='markers',color = ~variable, colors = cols,marker=list(size=12)) %>%
    layout(margin = m,title="Risk & Return",xaxis = list(title="Risk",tickformat =".1%",range = list(0, max(dls$sd_ann*1.2))), yaxis = list(title="Return",tickformat =".1%"),legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.2))
  rr_plotly<-rr_plotly %>% config(toImageButtonOptions = list( format = "svg",filename = "risk_return_scatter",width = chart_export_width,height = chart_export_height))

  rr_ggplot<<-
    #GGplot scatter
    ggplot(data=dls, aes(x=sd_ann,y=ret_ann)) +
    geom_point(cex=5,aes(color = factor(variable)))+
    #geom_bar(stat="identity",width=0.7, position=position_dodge(width=0.8))+
    #geom_text(aes(label=names), vjust=-0.5, color=retsd$colors,
    #          position = position_dodge(0.8), size=3.5)+
    #scale_color_brewer(palette = "PuOr")+
    scale_color_manual(values=cols)+
    theme_aq_black_default_font(base_size=14)+
    #size 22 for overleaf
    labs(color='')+
    labs(title="Risk & Return",subtitle="Annualized values in %",x ="")+
    labs(caption = 'Source: Created with the peRformance package')+
    xlab("Risk")+
    ylab("Excess Return")+
    theme(legend.position = "bottom",legend.margin=margin(-20,-20,-20,-20),legend.box.margin=margin(15,0,30,0))+
    guides(colour = guide_legend(nrow = 2))+
    theme(plot.margin=margin(l=5,r=10,b=5,t=5))+
    scale_x_continuous(labels = scales::percent_format(accuracy=.1))+
    scale_y_continuous(labels = scales::percent_format(accuracy=.1))+
    theme(panel.grid.major.x = element_line(colour = "#D8D8D8"))

    res_list<-list("dls"=dls,"rr_plotly"=rr_plotly,"rr_ggplot"=rr_ggplot)

  }else{
    res_list<-list("dls"=dls)
  }

  return(res_list)
}


rrScatEff<-function(da,ret_format="returns",table_format='wide',ann_factor=252,chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 80,t = 50,pad = 4),n.portfolios=30,box_constraint_list=NULL)
{
  

  if (!require("purrr")) install.packages("data.table")
  if (!require("PortfolioAnalytics")) install.packages("dplyr")
  if (!require("ecm")) install.packages("ecm")
  if (!require("plotly")) install.packages("lubridate")
  if (!require("scales")) install.packages("scales")

  library(purrr)
  library(PortfolioAnalytics)
  library(ecm)
  library(plotly)
  library(scales)

  calc_ret<-function(x)
  {
    x<-x/lagpad(x,k=1)-1
    x[1]<-0
    return(x)
  }
  calc_log_ret<-function(x)
  {
    x<-log(x/lagpad(x,k=1))
    x[1]<-0
    return(x)
  }

  dls<-rrScat(da,ret_format=ret_format,table_format=table_format,graphics=F)$dls

  if(table_format!='wide')
  {
    names(da)<-c("date",'id','value')
    da$date<-as.Date(da$date)
    da<-data.table::dcast(as.data.table(da), date ~ id, value.var = "value")
  }else{
    names(da)[1]<-"date"
    da$date<-as.Date(da$date)
  }

  dz<-read.zoo(da,index.column=1)
  R<-as.xts(dz)

  if(ret_format!="returns")
  {
    R<-apply(R,2,calc_ret)
  }

  funds <- colnames(R)

  #' Construct initial portfolio with basic constraints.
  init.portf <- portfolio.spec(assets=funds)
  #init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
  init.portf <- add.constraint(portfolio = init.portf, type="weight_sum", min_sum = 0.99999, max_sum = 1.0001)
  init.portf <- add.constraint(portfolio=init.portf, type="long_only")
  init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
  init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")

  # Use random portfolios to run the optimization.
  #maxSR.lo.RP <- optimize.portfolio(R=R, portfolio=init.portf,
  #                                  optimize_method="random",
  #                                  search_size=2000,
  #                                  trace=TRUE)

  #Add Box Constraints      
  if(is.null(box_constraint_list))
  {
    box_constraint_list<-list(
      min=c(rep(0,length(funds))),
      max=c(rep(1,length(funds)))
    )        
  }
  init.portf <- add.constraint(portfolio=init.portf,
                               type="box",
                               min=box_constraint_list$min[1:length(funds)],
                               max=box_constraint_list$max[1:length(funds)])
  
  
  #ef<-create.EfficientFrontier(R=R, portfolio=init.portf, type="mean-var", n.portfolios = n.portfolios,risk_aversion = NULL, match.col = "ES", search_size = 500)

  #  define moment function
  annualized.moments <- function(R, scale=ann_factor, portfolio=NULL){
    out <- list()
    out$mu <-   (1+dls$ret_ann)^(1/ann_factor)-1
    out$sigma <- cov(R)
    return(out)
  }
  ef <- create.EfficientFrontier(R=R, portfolio=init.portf, type="mean-StdDev", n.portfolios = n.portfolios, match.col = "StdDev", search_size = 500, 
                                      momentFUN="annualized.moments", scale=ann_factor)
  
  eff<-as.data.frame(cbind(ef$frontier[,4:ncol(ef$frontier)]))

  eff<-eff/rowSums(eff)

  eff$ret_ann<-rowSums(map2_dfc(eff, dls$ret_ann, `*`))

  eff$sd_ann<-ef$frontier[,2]*252^0.5

  col_aq2<-as.character(c("#04103b","#5777a7","#D1E2EC","#dd0400"))
  #col_aq2<-as.character(c("#04103b","#D1E2EC","#dd0400"))
  cols = colorRampPalette(col_aq2)(nrow(dls))
  #show_col(cols)

  p<-plot_ly(eff, x=~sd_ann, y=~ret_ann,type='scatter',mode='line', colors = cols,line=list(color='grey'),name='Efficient Frontier')%>%
    add_trace( x=dls$sd_ann,y=dls$ret_ann,color = dls$variable,marker=list(size=12),mode='markers',name=dls$variable)%>%
    layout(margin = m,title="Risk & Return",
           xaxis = list(title="Risk",tickformat =".1%",range = list(min(eff$sd_ann/1.1), max(dls$sd_ann*1.1))),
           yaxis = list(title="Return",tickformat =".1%"),legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.2))
  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "efficient_frontier",width = chart_export_width,height = chart_export_height))
  

  rr_ggplot<<-
    #GGplot scatter
    ggplot(NULL, aes(sd_ann, ret_ann)) +
    geom_point(cex=5,data = dls,aes(color=factor(variable))) +
    geom_line(data = eff)+
    scale_color_manual(values=cols)+
    theme_aq_black_default_font(base_size=14)+
    #size 22 for overleaf
    labs(color='')+
    labs(title="Risk & Return",subtitle="Annualized values in % including efficient frontier",x ="")+
    labs(caption = 'Source: Created with the peRformance package')+
    xlab("Risk")+
    ylab("Excess Return")+
    theme(legend.position = "bottom",legend.margin=margin(-20,-20,-20,-20),legend.box.margin=margin(15,0,30,0))+
    guides(colour = guide_legend(nrow = 2))+
    theme(plot.margin=margin(l=5,r=10,b=5,t=5))+
    scale_x_continuous(labels = scales::percent_format(accuracy=.1))+
    scale_y_continuous(labels = scales::percent_format(accuracy=.1))+
    theme(panel.grid.major.x = element_line(colour = "#D8D8D8"))

  reslist<-list("dls"=dls,"eff"=eff,"eff_plotly"=p,"eff_ggplot"=rr_ggplot)

  return(reslist)
}








styleBox<-function(
    number_categories_horizontal=5,
    number_categories_vertical=3,
    highlight_category=4,
    main_color="#5777a7",
    highlight_color="#dd0400",
    chart_font_size=14,
    chart_caption_1="Risk",
    chart_caption_2="Return",
    opacity_scale_factor=0.5,
    chart_export_width=500,
    chart_export_height=150
)
{
  if (!require("plotly")) install.packages("plotly")
  if (!require("dplyr")) install.packages("dplyr")

  library(plotly)
  library(dplyr)

  number_categories<-number_categories_horizontal*number_categories_vertical
  if(highlight_category>number_categories)
  { highlight_category<-number_categories }

  cols<-rep(main_color,number_categories)
  cols[highlight_category]<-highlight_color



  st_list<-list(
    "fillcolor" = cols[1],
    "line" = list("color"="rgb(0,0,0)", "width"=2),
    "opacity" = (1/number_categories*1),
    "type" = "rect",
    "x0" = 0,
    "x1" = (1/number_categories*1),
    "xref" = "paper",
    "y0" = 0,
    "y1" = (1/number_categories*1),
    "yref" = "paper"
  )
  dups <- list(st_list)[rep(1,number_categories)]
  for(i in 1:number_categories_horizontal)
  {
    for(j in 1:number_categories_vertical)
    {
      dups[[i+(j-1)*number_categories_horizontal]]$fillcolor<-cols[i+(j-1)*number_categories_horizontal]
      dups[[i+(j-1)*number_categories_horizontal]]$x0<-(1/number_categories_horizontal)*(i-1)
      dups[[i+(j-1)*number_categories_horizontal]]$x1<-(1/number_categories_horizontal)*(i)
      dups[[i+(j-1)*number_categories_horizontal]]$y0<-(1/number_categories_vertical)*(j-1)
      dups[[i+(j-1)*number_categories_horizontal]]$y1<-(1/number_categories_vertical)*(j)
      dups[[i+(j-1)*number_categories_horizontal]]$opacity<-1/(number_categories_horizontal-i+number_categories_vertical-j)^opacity_scale_factor
    }
  }



  p <- plot_ly(x = list("1"), y = list("1"), hoverinfo = "none",
                               marker = list("opacity" = 0), mode = "markers", name = "B", type = "scatter",
                               width = 700,
                               height = 280) %>%
    layout(title = "",
           annotations = list(
             list(
               "x" = 0.990130093458,
               "y" = 1.00181709504,
               "align" = "left",
               "font" = list("size" = chart_font_size,color="#04103b"),
               "showarrow" = FALSE,
               "text" = paste0("<b>",chart_caption_1,"</b>"),
               "xref" = "x",
               "yref" = "y"
             ),
             list(
               "x"= 1.00001816013,
               "y"= 1.35907755794e-16,
               "font" = list("size" = chart_font_size,color="#04103b"),
               "showarrow" = FALSE,
               "text" = paste0("<b>",chart_caption_2,"</b>"),
               "xref" = "x",
               "yref" = "y"
             )
           ),

           hovermode = "closest",
           margin = list("r" = 30, "t" = 20, "b" = 0, "l" = 30),


           shapes = dups,
           xaxis = list(
             "autorange" = TRUE,
             "range" = list(0.989694747864, 1.00064057995),
             "showgrid" = FALSE,
             "showline" = FALSE,
             "showticklabels" = FALSE,
             "title"  = "<br>",
             "type" = "linear",
             "zeroline" = FALSE
           ),
           yaxis = list(
             "autorange" = TRUE,
             "range" = list(-0.0358637178721, 1.06395696354),
             "showgrid" = FALSE,
             "showline" = FALSE,
             "showticklabels" = FALSE,
             "title"  = "<br>",
             "type" = "linear",
             "zeroline" = FALSE
           )

    )
  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "stylebox",width = chart_export_width,height = chart_export_height))

  return(p)

}


#p<-style_box_flex(number_categories_horizontal=6,number_categories_vertical=6,highlight_category=1,chart_font_size=25,chart_caption_1="Risk",chart_caption_2="Return",opacity_scale_factor=0.2)
#p
#orca(p,"style_box_plot.svg",width = 700,height = 250)



