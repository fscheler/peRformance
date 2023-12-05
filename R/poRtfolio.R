
library(ggplot2)


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
  #if (!require("dplyr")) install.packages("dplyr")
  #if (!require("plotly")) install.packages("plotly")
  #if (!require("lubridate")) install.packages("lubridate")

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

allocBar<-function (da, chart_title = "Portfolio Allocation", chart_subtitle="Optional",chart_height = 400,
                    chart_font_size = 11, chart_export_width = 600, chart_export_height = 450,
                    m = list(r = 0, l = 0, b = 0, t = 50, par = 4),plotly=T,title_pos="center")
{


  #if (!require("dplyr"))
  #  install.packages("dplyr")
  #if (!require("plotly"))
  #  install.packages("plotly")
  #if (!require("lubridate"))
  #  install.packages("lubridate")
  library(dplyr)
  library(plotly)
  library(lubridate)
  da <- as.data.frame(da)
  da <- da[, 1:2]
  names(da) <- c("assets", "weight")
  da$weight <- as.numeric(da$weight)
  col_aq2 <- as.character(c("#04103b", "#dd0400", "#3b5171"))
  y_axis_caption <- ""
  da <- da %>% group_by(assets) %>% summarize(weight = sum(as.numeric(weight)))
  da$assets <- factor(da$assets, levels = unique(da$assets)[order(as.numeric(da$weight),
                                                                  decreasing = F)])
  p <- plot_ly(da, x = as.numeric(da$weight), y = da$assets,
               height = chart_height, type = "bar", name = "Portfolio",
               marker = list(color = col_aq2[1]))
  p <- p %>% layout(margin = m, font = list(size = chart_font_size),
                    title = chart_title, xaxis = list(title = y_axis_caption,
                                                      tickformat = ".0%"), yaxis = list(title = y_axis_caption),
                    barmode = "group")
  p <- p %>% config(toImageButtonOptions = list(format = "svg",
                                                filename = "allocation_pie", width = chart_export_width,
                                                height = chart_export_height))

  if(plotly!=T)
  {
    p<-
      ggplot(da, aes(x=as.numeric(weight), y=assets,fill="#04103b")) +
      geom_bar(stat='identity') +
      #coord_flip()+
      scale_fill_manual(values="#04103b") +
      theme_aq_black_default_font(base_size = 20) +
      # scale_x_discrete(limits =names(df)[2:length(names(df))])+
      labs(title = chart_title, subtitle = chart_subtitle,
           x = "") + labs(caption = "") + xlab("") +
      theme(plot.margin = margin(l = 5, r = 10, b = 5, t = 5)) + xlab("")+ylab("")+
      #geom_text(size = 5, col="lightgrey",position = position_stack(vjust = 0.5))+
      theme(legend.position = "none",
            legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(15,0, 30, 0)) + guides(colour = guide_legend(nrow = 3)) +
      theme(panel.grid.major.x = element_blank()) + theme(panel.grid.major.x = element_blank()) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(legend.title = element_blank()) + guides(fill = guide_legend(nrow = 1,  byrow = TRUE))
    #theme(axis.text.x = element_text(angle = 45, hjust=1))+
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1))  +
    #xlim(min(mm$year),max(mm$year))
    if(title_pos=="left")
    {
      p<-p+
        theme(plot.caption = element_text(hjust = 0.2), #Default is hjust=1
              plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
              plot.caption.position =  "plot") #NEW parameter
    }

  }


  return(p)
}

allocBar2<-function(da,chart_title="Portfolio Allocation",chart_height=400,chart_font_size=11,chart_export_width=600,chart_export_height=450,m=list(r=0,l=0,b=0,t=50,par=4),barcol="#f9f9f9",barborder="#3b5171")
{
  #if (!require("dplyr")) install.packages("dplyr")
  #if (!require("plotly")) install.packages("plotly")
  #if (!require("lubridate")) install.packages("lubridate")

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

  p <- plot_ly(da, type='bar', x = ~as.numeric(da$weight), y = ~da$assets, text = ~paste0(" ",da$assets), name = 'Portfolio',height=chart_height,marker = list(color = barcol,line = list(width = 0.5,color = barborder)),
                 texttemplate = paste0("<b>",da$assets,"</b>"), textfont=20,textposition = 'inside',insidetextanchor = "start")

  #p <- p %>% layout(uniformtext=list(minsize=50))
  p<-p%>%layout(yaxis= list(showticklabels = FALSE))
  p <- p %>% layout(margin = m,font=list(size=chart_font_size),title=chart_title, xaxis = list(title=y_axis_caption,tickformat =".0%"), yaxis = list(title=y_axis_caption), barmode = 'group')
  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "allocation_pie",width = chart_export_width,height = chart_export_height))

  return(p)
}

FXallocBar<-function(da,ret_format="returns",chart_title="Portfolio Allocation",legends=c("Gross","Net"),chart_height=400,chart_font_size=11,chart_export_width=600,chart_export_height=450,m=list(r=0,l=0,b=0,t=50,par=4))
{

  #if (!require("plotly")) install.packages("plotly")
  #if (!require("reshape2")) install.packages("reshape2")

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
                                     line = list(color = 'white', width = 1.5)),name=legends[2])
  p <- p %>% add_trace(x = ~gross, y = ~assets, type = 'bar',
                       text = paste0(round(da$gross*100),"%"), textposition = 'auto',
                       marker = list(color = '#04103b',
                                     line = list(color = 'white', width = 1.5)),name=legends[1])
  p <- p %>% layout(margin = m,font=list(size=chart_font_size),title=chart_title, xaxis = list(title=y_axis_caption,tickformat =".0%"), yaxis = list(title=y_axis_caption), barmode = 'group')
  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "allocation_pie",width = chart_export_width,height = chart_export_height))

  return(p)
}


allocPie<-function(df,chart_title='Portfolio',subtitle="Allocation in %",chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 50,t = 50,pad = 4),title_pos="center",legend_row=2,base_size = 14,plotly=T,rotation_angle=90)
{

  #if (!require("scales")) install.packages("scales")
  #if (!require("plotly")) install.packages("plotly")
  #if (!require("dplyr")) install.packages("dplyr")

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

  if(plotly==T)
  {
    p <- plot_ly(df, labels = ~assets, values = ~weights, type = 'pie',showlegend = FALSE, rotation = rotation_angle,
                 textposition = 'outside',textinfo = 'text',
                 hoverinfo = 'text',source = "subset",
                 text=~paste(sub(" ","<br>",df$assets),":","<br>",paste0(round(df$weights,2)*100,"%") ),
                 insidetextfont = list(color = '#FFFFFF'),
                 sort = FALSE
                 ,marker = list(colors = cols)) %>%
      layout(title = list(text =chart_title, y = 1), margin = m,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "allocation_pie",width = chart_export_width,height = chart_export_height))
  }else{
    library(ggplot2)
    p<-
    ggplot(df, aes(x="", y=weights, fill=assets)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      scale_fill_manual("",values=cols)+
      theme_aq_black_pie(base_size = 14)+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank()
      )+
      labs(title=chart_title,subtitle=subtitle)


    if(title_pos=="left")
    {
      p<-p+
        theme(plot.caption = element_text(hjust = 0.2), #Default is hjust=1
              plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
              plot.caption.position =  "plot") #NEW parameter
    }

  }



  return(p)

}


allocTree<-function(df,parent_label="Portfolio",chart_export_width=600,chart_export_height=450,m = list(l = 0,r = 0,b = 0,t = 0,pad = 4))
{
  #if (!require("scales")) install.packages("scales")
  #if (!require("plotly")) install.packages("plotly")
  #if (!require("dplyr")) install.packages("dplyr")

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


rrScat<-function(da,ret_format="returns",table_format='wide',graphics=T,ann_factor=252,chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 80,t = 50,pad = 4),source_text='Source: Created with the peRformance package')
{

  #if (!require("data.table")) install.packages("data.table")
  #if (!require("dplyr")) install.packages("dplyr")
  #if (!require("ecm")) install.packages("ecm")
  #if (!require("lubridate")) install.packages("lubridate")
  #if (!require("plotly")) install.packages("plotly")
  #if (!require("scales")) install.packages("scales")
  #if (!require("reshape2")) install.packages("reshape2")

  library(data.table)
  library(dplyr)
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
    dl<-dl%>%dplyr::group_by(variable)%>%dplyr::mutate(idx=cumprod(1+ret))
  }else{
    names(dl)<-c("date","variable","idx")
    dl<-dl%>%dplyr::group_by(variable)%>%dplyr::mutate(ret=idx/lagpad(idx,k=1)-1)
    dl$ret<-ifelse(is.na(dl$ret),0,dl$ret)
  }

  #dls <- dl %>% group_by(variable) %>%  do(tail(., n=1))
  dls<-dl%>%dplyr::group_by(variable)%>%dplyr::summarise(sd=head(date,1),ld=tail(date,1),std=sd(ret),first=head(idx,1),last=tail(idx,1))

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
    labs(caption = source_text)+
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



rrScatEff<-function(da,ret_format="returns",table_format='wide',
                    ann_factor=252,chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 80,t = 50,pad = 4),n.portfolios=30,box_constraint_list=NULL)
{


  #if (!require("purrr")) install.packages("data.table")
  #if (!require("PortfolioAnalytics")) install.packages("dplyr")
  #if (!require("ecm")) install.packages("ecm")
  #if (!require("plotly")) install.packages("lubridate")
  #if (!require("scales")) install.packages("scales")

  library(purrr)
  library(PortfolioAnalytics)
  #library(ecm)
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


  annualized.moments <- function(R, scale=ann_factor, portfolio=NULL){
    out <- list()
    out$mu <-   (1+dls$ret_ann)^(1/ann_factor)-1
    out$sigma <- cov(R)
    #out$siga <- corpcor::cov.shrink(R, shrinkage_lambda, shrinkage_lambda)[1:ncol(R),1:ncol(R)]
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
  #if (!require("plotly")) install.packages("plotly")
  #if (!require("dplyr")) install.packages("dplyr")

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
  p<-p%>%layout(plot_bgcolor='white') %>% layout(paper_bgcolor='#252a4f')
  p<-p %>% config(toImageButtonOptions = list( format = "svg",filename = "stylebox",width = chart_export_width,height = chart_export_height))

  return(p)

}



valueboxeR<-function(  values = c(avg_dvd_yield,
                                          avg_fcf_yield,
                                          avg_pe,
                                          avg_rev_cagr,
                                          avg_ebitda_margin,
                                          avg_net_debt_ebitda),
                               info = c("P/E Ratio",
                                        "FCF Yield",
                                        "P/E/ Ratio",
                                        "5Yr Rev. Growth",
                                        "EBITDA Margin",
                                        "Net Debt/EBITDA"),
                               symbols=c("fa-area-chart","fa-percent","fa-bar-chart-o","fa-line-chart","fa-pie-chart","fa-money"),
                               col_aq2 = c("#04103b", "#5777a7", "#D1E2EC", "#dd0400")
)
{



  col_aq2 = as.character(col_aq2)
  cols = colorRampPalette(col_aq2)(6)

  library(ggplot2)
  library(emojifont)
  ds <- data.frame(
    x = rep(seq(2, 15, 6.5), 2),
    y = c(rep(6.5, 3), rep(2,3)),
    h = rep(4.25, 6),
    w = rep(6.25, 6),
    value = values,
    info = info,
    icon = c(fontawesome(as.character(symbols))),
    font_family = c(rep("fontawesome-webfont", 6)),
    color = factor(1:6)
  )

  p<-ggplot(ds, aes(x, y, height = h, width = w, label = info)) +
    ## Create the tiles using the `color` column
    geom_tile(aes(fill = color)) +
    ## Add the numeric values as text in `value` column
    geom_text(color = "white", fontface = "bold", size = 10,
              aes(label = value, x = x - 2.9, y = y + 1), hjust = 0) +
    ## Add the labels for each box stored in the `info` column
    geom_text(color = "white", fontface = "bold",
              aes(label = info, x = x - 2.9, y = y - 1), hjust = 0) +
    coord_fixed() +
    scale_fill_manual(values =cols)+
    ## Use `geom_text()` to add the icons by specifying the unicode symbol.
    geom_text(size = 20, aes(label = icon, family = font_family,
                             x = x + 1.5, y = y + 0.5), alpha = 0.25) +
    theme_void() +
    guides(fill = FALSE)

  return(p)


}


boxeR<-function()
{

  library(plotly)

  y <- c('Dividend Yield %')
  MXWO <- c(2.2)
  Portfolio <- c(3.9)
  data <- data.frame(y, MXWO, Portfolio)
  m <- list(l = 0,r = 50,b = 10,t = 10,pad = 4)
  fig <- plot_ly(data, x = ~MXWO, y = ~y, type = 'bar', orientation = 'h', name = 'SF Zoo',height=200,
                 text = paste0("Market<br>",MXWO), textposition = 'inside',
                 marker = list(color = 'white',
                               line = list(color = 'white',
                                           width = 3)))
  fig <- fig %>% add_trace(x = ~Portfolio, name = 'LA Zoo',
                           text = paste0("Portfolio<br>",Portfolio), textposition = 'outside',
                           marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                         line = list(color = 'rgba(58, 71, 80, 1.0)',
                                                     width = 0.5)))
  fig <- fig %>% layout(margin=m,barmode = 'stack',showlegend=F,
                        xaxis = list(title = "",showgrid=FALSE,showticklabels = FALSE,range=c(0,(data$MXWO+data$Portfolio)*1.5)),
                        yaxis = list(title ="",showgrid=FALSE))

  fig<-fig %>%layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",paper_bgcolor = "rgba(0, 0, 0, 0)",yaxis = list(zerolinecolor = '#f9f9f9',showgrid = FALSE),xaxis = list(zerolinecolor = '#f9f9f9',showgrid = FALSE))
  fig
  orca(fig,"comparison_dividend_yield.svg", width = 580,height=70)


}

rangeR<-function(y=c('Dividend Yield %'),benchmark=2.2,portfolio=3.9,caption_bm=paste0(2.2,"%"),caption_po=paste0(3.9,"%"),chart_height=200,m=list(r=0,l=0,b=0,t=0,par=4))
{
  library(plotly)

  #,range=c(0,(data$MXWO+data$Portfolio)*1.5)
  #y <- c('Dividend Yield %')
  #benchmark <- c(2.2)
  #portfolio <- c(3.9)
  s <- data.frame(y, benchmark, portfolio)
  library(plotly)
  fig <- plot_ly(s, color = I("#BDBDBD"),height=chart_height)
  fig <- fig %>% add_segments(x = ~benchmark, xend = ~portfolio, y = ~y, yend = ~y, showlegend = FALSE)
  fig <- fig %>% add_markers(x = ~benchmark, y = ~y, name = "Benchmark", color = I("#dd0400"),text = paste0("Market<br>",benchmark))
  fig <- fig %>% add_markers(x = ~portfolio, y = ~y, name = "Portfolio", color = I("#5777a7"))
  fig <- fig %>% layout(
    title = "",
    xaxis = list(title = ""),
    margin = list(l = 65,t=65)
  )

  #fig <- fig %>% add_text(x = ~benchmark, y = ~y,text = ~caption_bm,
  #                        textposition = "bottom right",color = I("#dd0400"), showlegend = FALSE)

  #fig <- fig %>% add_text(x = ~portfolio, y = ~y,text = ~caption_po,
  #                        textposition = "bottom left",color = I("#5777a7"), showlegend = FALSE)

  fig <- fig %>% add_annotations(x = s$benchmark,
                                 y = s$y,
                                 text = caption_bm,
                                 xref = "x",
                                 yref = "y",
                                 showarrow = TRUE,
                                 arrowhead = 0,
                                 arrowsize = 0,
                                 ax = 0,
                                 ay = -10)

  fig
  fig <- fig %>% add_annotations(x = s$portfolio,
                                 y = s$y,
                                 text = caption_po,
                                 xref = "x",
                                 yref = "y",
                                 showarrow = TRUE,
                                 arrowhead = 0,
                                 arrowsize = 0,
                                 ax = 0,
                                 ay = -10)

  fig

  fig <- fig %>% layout(margin=m,barmode = 'stack',showlegend=F,
                        xaxis = list(title = "",showgrid=FALSE,showticklabels = FALSE),
                        yaxis = list(title ="",showgrid=FALSE))
  fig<-fig %>%layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",paper_bgcolor = "rgba(0, 0, 0, 0)",yaxis = list(zerolinecolor = '#f9f9f9',showgrid = FALSE),xaxis = list(zerolinecolor = '#f9f9f9',showgrid = FALSE))

  return(fig)

}


#Life Performance
gglineR<-function(df,title="Title",subtitle="Subtitle",xcap="",ycap="",name1="Portfolio",perc=T,col_aq2 = c("#04103b", "#dd0400","#5777a7", "#D1E2EC"),fredr_key=NULL)
{
  library(ggplot2)
  names(df)<-c("date","asset")
  df$date<-as.Date(df$date)
  cols <- setNames(c(col_aq2[1]), c(name1))

  p<-
    ggplot(data=df,aes(x=as.Date(df$date), y=df$asset))

    if(!is.null(fredr_key))
    {
      p<-p+ggRec(as.Date(min(df$date)),as.Date(max(df$date)),fredr_key=fredr_key)
    }
  p<-p+
    geom_line(size=0.8,aes(y=df$asset,color=name1))+
    scale_colour_manual(values = cols)+
    theme_aq_black(base_size=24)+
    #size 22 for overleaf
    labs(color='')+
    labs(title=title,subtitle=subtitle,x =xcap)+
    labs(caption = '')+
    theme(legend.position = "none",legend.margin=margin(-20,-20,-20,-20),legend.box.margin=margin(0,0,0,0))+
    guides(colour = guide_legend(nrow = 1))+
    scale_x_date(labels = date_format("%Y"))
  if(perc==TRUE)
  {
    p<-p+scale_y_continuous(labels = scales::percent)
  }
  p<-p+
    ylab(ycap)+
    theme(plot.margin=margin(5,5,5,5))

  return(p)
}



#Life Performance
gglineRt<-function(df,title="Title",subtitle="Subtitle",xcap="",ycap="",name1="asset",name2="benchmark",perc=F,col_aq2 = c("#04103b", "#dd0400","#5777a7", "#D1E2EC"),fredr_key=NULL,secaxis=1,legend_rows=1)
{
  library(ggplot2)
  names(df)<-c("date","asset","benchmark")
  df$date<-as.Date(df$date)
  cols <- setNames(c(col_aq2[1], col_aq2[2]), c(name1, name2))
  secaxisfactor<-2
  p<-
    ggplot(data=df,aes(x=as.Date(df$date), y=df$asset))

  if(!is.null(fredr_key))
  {
  p<-p+ggRec(as.Date(min(df$date)),as.Date(max(df$date)),fredr_key=fredr_key)
  }
  p<-p+
    geom_line(size=0.8,aes(y=df$asset,color=name1))+
    geom_line(size=0.8,aes(y=df$benchmark/secaxis,color=name2))+
    scale_colour_manual(values = cols)+
    theme_aq_black(base_size=24)+
    #size 22 for overleaf
    labs(color='')+
    labs(title=title,subtitle=subtitle,x =xcap)+
    labs(caption = '')+
    theme(legend.position = "bottom",legend.margin=margin(-20,-20,-20,-20),legend.box.margin=margin(0,0,0,0))+
    guides(colour = guide_legend(nrow = legend_rows))+
    scale_x_date(labels = date_format("%Y"))
  if(perc==TRUE)
  {
    p<-p+scale_y_continuous(labels = scales::percent)
  }
  p<-p+
    ylab(ycap)+
    theme(plot.margin=margin(5,5,5,5))

  if(secaxis!=1)
  {
    # Custom the Y scales:
    p<-p+scale_y_continuous(
      # Features of the first axis
      name = "",
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*secaxis, name="")
    )
  }


  return(p)
}



ggStacked<-function(long,title="Title",subtitle="Subtitle",perc=T,rel=T,lrow=2,col_aq2 = as.character(c("#04103b", "#5777a7", "#D1E2EC", "#dd0400")),minmaxx=F)
{
  library(ggplot2)
  #long <- melt(setDT(df), id.vars = "date")
  #names need to be date, variable, value
  cols = colorRampPalette(col_aq2)(length(unique(long$variable)))

  # Stacked + percent
  p<-
    ggplot(long, aes(fill=variable, y=value, x=date))

  if(rel==T)
  {
    p<-p+geom_bar(position="fill", stat="identity")
  }else{
    p<-p+geom_bar(stat="identity")
  }

  p<-p+
    scale_fill_manual(values=cols)+
    theme_aq_black_default_font(base_size = 20) +
    labs(color = "") + labs(title = title,
                            subtitle = subtitle, x = "") +
    labs(caption = "") + xlab("") +
    ylab("") + theme(legend.position = "bottom",
                     legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(15,  0, 30, 0)) +
    guides(colour = guide_legend(nrow = 3)) +
    theme(plot.margin = margin(l = 5, r = 10, b = 5, t = 5)) +
    theme(legend.title=element_blank())+
    guides(fill=guide_legend(nrow=lrow,byrow=TRUE))

  if(minmaxx==T)
  {
    p<-p+scale_x_date(limits=c(min(long$date),max(long$date)), expand = c(0, 0))
  }

    if(perc==T)
    {
      p<-p  +  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    }

  p<-p+
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    coord_cartesian(xlim = c(min(long$date),max(long$date)))

  return(p)
}

#p<-style_box_flex(number_categories_horizontal=6,number_categories_vertical=6,highlight_category=1,chart_font_size=25,chart_caption_1="Risk",chart_caption_2="Return",opacity_scale_factor=0.2)
#p
#orca(p,"style_box_plot.svg",width = 700,height = 250)


gglineRtr<-function (df, title = "Title", subtitle = "Subtitle", xcap = "",
                     ycap = "", name1 = "asset", name2 = "benchmark",name3="benchmark1", perc = F,
                     col_aq2 = c("#04103b", "#dd0400", "#5777a7", "#D1E2EC"),
                     fredr_key = NULL, secaxis = 1,base_size=24,legend_rows=1)
{
  library(ggplot2)
  names(df) <- c("date", "asset", "benchmark","benchmark1")
  df$date <- as.Date(df$date)
  cols <- setNames(c(col_aq2[1], col_aq2[2], col_aq2[3]), c(name1, name2,name3))
  secaxisfactor <- 2
  p <- ggplot(data = df, aes(x = as.Date(df$date), y = df$asset))
  if (!is.null(fredr_key)) {
    p <- p + ggRec(as.Date(min(df$date)), as.Date(max(df$date)),
                   fredr_key = fredr_key)
  }
  p <- p + geom_line(size = 0.8, aes(y = df$asset, color = name1)) +
    geom_line(size = 0.8, aes(y = df$benchmark/secaxis, color = name2)) +
    geom_line(size = 0.8, aes(y = df$benchmark1/secaxis, color = name3)) +
    scale_colour_manual(values = cols) +
    theme_aq_black(base_size = base_size ) + labs(color = "") +
    labs(title = title, subtitle = subtitle, x = xcap) +
    labs(caption = "") + theme(legend.position = "bottom",
                               legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(0,
                                                                                                      0, 0, 0)) + guides(colour = guide_legend(nrow = legend_rows)) +
    scale_x_date(labels = date_format("%Y"))
  if (perc == TRUE) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  p <- p + ylab(ycap) + theme(plot.margin = margin(5, 5, 5,
                                                   5))
  if (secaxis != 1) {
    p <- p + scale_y_continuous(name = "", sec.axis = sec_axis(trans = ~. *
                                                                 secaxis, name = ""))
  }
  return(p)
}


gglineRfr<-function (df, title = "Title", subtitle = "Subtitle", xcap = "",
                     ycap = "", name1 = "asset", name2 = "benchmark",name3="benchmark1",name4="benchmark2", perc = F,
                     col_aq2 = c("#04103b", "#dd0400", "#5777a7", "#D1E2EC"),
                     fredr_key = NULL, secaxis = 1,base_size=24,legend_rows=2)
{
  library(ggplot2)
  names(df) <- c("date", "asset", "benchmark","benchmark1","benchmark2")
  df$date <- as.Date(df$date)
  cols <- setNames(c(col_aq2[1], col_aq2[2], col_aq2[3], col_aq2[4]), c(name1, name2,name3,name4))
  secaxisfactor <- 2
  p <- ggplot(data = df, aes(x = as.Date(df$date), y = df$asset))
  if (!is.null(fredr_key)) {
    p <- p + ggRec(as.Date(min(df$date)), as.Date(max(df$date)),
                   fredr_key = fredr_key)
  }
  p <- p + geom_line(size = 0.8, aes(y = df$asset, color = name1)) +
    geom_line(size = 0.8, aes(y = df$benchmark/secaxis, color = name2)) +
    geom_line(size = 0.8, aes(y = df$benchmark1/secaxis, color = name3)) +
    geom_line(size = 0.8, aes(y = df$benchmark2/secaxis, color = name4)) +
    scale_colour_manual(values = cols) +
    theme_aq_black(base_size = base_size ) + labs(color = "") +
    labs(title = title, subtitle = subtitle, x = xcap) +
    labs(caption = "") + theme(legend.position = "bottom",
                               legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(0,
                                                                                                      0, 0, 0)) + guides(colour = guide_legend(nrow = legend_rows)) +
    scale_x_date(labels = date_format("%Y"))
  if (perc == TRUE) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  p <- p + ylab(ycap) + theme(plot.margin = margin(5, 5, 5,
                                                   5))
  if (secaxis != 1) {
    p <- p + scale_y_continuous(name = "", sec.axis = sec_axis(trans = ~. *
                                                                 secaxis, name = ""))
  }
  return(p)
}


gglineRff<-function (df, title = "Title", subtitle = "Subtitle", xcap = "",
                     ycap = "", name1 = "asset", name2 = "benchmark",name3="benchmark1",name4="benchmark2",name5="benchmark3", perc = F,
                     col_aq2 = c("#04103b", "#dd0400", "#5777a7", "#D1E2EC","grey"),
                     fredr_key = NULL, secaxis = 1,base_size=24,legend_rows=2)
{
  library(ggplot2)
  names(df) <- c("date", "asset", "benchmark","benchmark1","benchmark2","benchmark3")
  df$date <- as.Date(df$date)
  cols <- setNames(c(col_aq2[1], col_aq2[2], col_aq2[3], col_aq2[4], col_aq2[5]), c(name1, name2,name3,name4,name5))
  secaxisfactor <- 2
  p <- ggplot(data = df, aes(x = as.Date(df$date), y = df$asset))
  if (!is.null(fredr_key)) {
    p <- p + ggRec(as.Date(min(df$date)), as.Date(max(df$date)),
                   fredr_key = fredr_key)
  }
  p <- p + geom_line(size = 0.8, aes(y = df$asset, color = name1)) +
    geom_line(size = 0.8, aes(y = df$benchmark/secaxis, color = name2)) +
    geom_line(size = 0.8, aes(y = df$benchmark1/secaxis, color = name3)) +
    geom_line(size = 0.8, aes(y = df$benchmark2/secaxis, color = name4)) +
    geom_line(size = 0.8, aes(y = df$benchmark3/secaxis, color = name5)) +
    scale_colour_manual(values = cols) +
    theme_aq_black(base_size = base_size ) + labs(color = "") +
    labs(title = title, subtitle = subtitle, x = xcap) +
    labs(caption = "") + theme(legend.position = "bottom",
                               legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(0,
                                                                                                      0, 0, 0)) + guides(colour = guide_legend(nrow = legend_rows)) +
    scale_x_date(labels = date_format("%Y"))
  if (perc == TRUE) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  p <- p + ylab(ycap) + theme(plot.margin = margin(5, 5, 5,
                                                   5))
  if (secaxis != 1) {
    p <- p + scale_y_continuous(name = "", sec.axis = sec_axis(trans = ~. *
                                                                 secaxis, name = ""))
  }
  return(p)
}




statleR<-function(returns_matrix_z,periodicity_adjustment=252,
                  chart_height=220,
                  custom_caption_asset="Asset",
                  custom_caption_benchmark="Benchmark"
                  #Portfolio_type="Defensive",
                  #main_currency="EUR"
)
{
  #returns_matrix_z<-dm

  custom_caption_asset<-""
  custom_caption_benchmark<-""
  library(PerformanceAnalytics)
  library(PortfolioAnalytics)
  library(zoo)
  periodicity_adjustment<-252
  #returns_matrix_z<-df[,c("date","ret")]
  names(returns_matrix_z)<-c("date","asset_ret")
  returns_matrix_z$benchmark_ret<-0
  returns_matrix_z$asset_exc_ret<-returns_matrix_z$asset_ret
  returns_matrix_z$benchmark_exc_ret<-returns_matrix_z$benchmark_ret
  returns_matrix_z$asset<-cumprod(1+returns_matrix_z$asset_ret)
  returns_matrix_z$benchmark<-cumprod(1+returns_matrix_z$benchmark_ret)
  #da<-returns_matrix_z
  col<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))


  #da<-returns_matrix_z
  col<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))

  #Total Return
  tot_ret_asset<-tail(cumprod(1+returns_matrix_z$asset_ret),1)-1
  tot_ret_benchmark<-tail(cumprod(1+returns_matrix_z$benchmark_ret),1)-1

  #Annualized Return
  number_years<-as.numeric(tail(returns_matrix_z$date,1)-head(returns_matrix_z$date,1))/365.25
  ann_ret_asset<-tail(cumprod(1+returns_matrix_z$asset_ret),1)^(1/number_years)-1
  ann_ret_benchmark<-tail(cumprod(1+returns_matrix_z$benchmark_ret),1)^(1/number_years)-1

  #Annualized Volatility
  risk_asset<-sd(returns_matrix_z$asset_ret)*periodicity_adjustment^0.5
  risk_benchmark<-sd(returns_matrix_z$benchmark_ret)*periodicity_adjustment^0.5

  #Drawdowns
  worst_drawdown_asset<-min(returns_matrix_z$asset/cummax(returns_matrix_z$asset),na.rm=T)-1
  worst_drawdown_benchmark<-min(returns_matrix_z$benchmark/cummax(returns_matrix_z$benchmark),na.rm=T)-1

  #Sharpe Ratio
  number_years<-as.numeric(tail(returns_matrix_z$date,1)-head(returns_matrix_z$date,1))/365.25
  excess_ret_asset<-tail(cumprod(1+returns_matrix_z$asset_exc_ret),1)^(1/number_years)-1
  excess_ret_benchmark<-tail(cumprod(1+returns_matrix_z$benchmark_exc_ret),1)^(1/number_years)-1
  risk_asset<-sd(returns_matrix_z$asset_ret)*periodicity_adjustment^0.5
  risk_benchmark<-sd(returns_matrix_z$benchmark_ret)*periodicity_adjustment^0.5
  sharpe_ratio_asset<-  excess_ret_asset/risk_asset
  sharpe_ratio_benchmark<-  excess_ret_benchmark/risk_benchmark

  #Calmar Ratio
  calmar_ratio_asset<-excess_ret_asset/(-(min(returns_matrix_z$asset/cummax(returns_matrix_z$asset),na.rm=T)-1))
  calmar_ratio_benchmark<-excess_ret_asset/(-(min(returns_matrix_z$benchmark/cummax(returns_matrix_z$benchmark),na.rm=T)-1))

  returns_matrix_zoo<-read.zoo(returns_matrix_z,index.column=1)

  #Information Ratio
  information_ratio<-InformationRatio(returns_matrix_zoo$asset_ret, returns_matrix_zoo$benchmark_ret)

  #Treynor Ratio Portfolio
  treynor_ratio<-TreynorRatio(returns_matrix_zoo$asset_exc_ret, returns_matrix_zoo$benchmark_exc_ret, Rf = 0, scale = NA, modified = FALSE)

  #Pooled correlation
  pooled_correlation<-cor(returns_matrix_zoo$asset_exc_ret, returns_matrix_zoo$benchmark_exc_ret)

  #Longest Drawdown
  returns_matrix_z$drawdown_asset<-returns_matrix_z$asset/cummax(returns_matrix_z$asset)-1
  returns_matrix_z$drawdown_asset_binary<-ifelse(returns_matrix_z$drawdown_asset<0,1,0)
  returns_matrix_z$seq_asset<-sequence(rle(as.character(returns_matrix_z$drawdown_asset_binary))$lengths)
  rec_index<-index(returns_matrix_z$date)[returns_matrix_z$seq_asset==max(returns_matrix_z$seq_asset)]
  longest_drawdown_asset<-as.numeric(returns_matrix_z$date[rec_index]-returns_matrix_z$date[rec_index-max(returns_matrix_z$seq_asset)])/365.26

  returns_matrix_z$drawdown_benchmark<-returns_matrix_z$benchmark/cummax(returns_matrix_z$benchmark)-1
  returns_matrix_z$drawdown_benchmark_binary<-ifelse(returns_matrix_z$drawdown_benchmark<0,1,0)
  returns_matrix_z$seq_benchmark<-sequence(rle(as.character(returns_matrix_z$drawdown_benchmark_binary))$lengths)
  rec_index<-index(returns_matrix_z$date)[returns_matrix_z$seq_benchmark==max(returns_matrix_z$seq_benchmark)]
  longest_drawdown_benchmark<-as.numeric(returns_matrix_z$date[rec_index]-returns_matrix_z$date[rec_index-max(returns_matrix_z$seq_benchmark)])/365.26

  headerColor<-"darkgrey"
  rowOddColor<-"white"
  rowEvenColor<-"lightgrey"

  chart_height<-500

  fig <- plot_ly(
    height=chart_height,
    type = 'table',
    columnwidth  = c(80,60),
    header = list(
      values = c('<b>Performance</b>',paste0('<b>',custom_caption_asset,'</b> ')
      ),
      line = list(color = 'white'),
      fill = list(color = headerColor),
      align = c(rep('left',1),rep('center',1)),
      font = list(color = "white", size = 12)
      #align="left"
    ),
    cells = list(
      values = rbind(
        c(
          "Total Return",
          "Annualized Return",
          "Annualized Volatility",
          "Sharpe Ratio",
          "Calmar Ratio",
          "Longest Drawdown (Yrs)"
        ),
        c(
          paste0(round(tot_ret_asset*100,2),"%"),
          paste0(round(ann_ret_asset*100,2),"%"),
          paste0(round(risk_asset*100,2),"%"),
          paste0(round(sharpe_ratio_asset,4),""),
          paste0(round(calmar_ratio_asset,4),""),
          paste0(round(longest_drawdown_asset,4),"")
        )
      ),
      line = list(color = 'white'),
      fill = list(color = list(rep(c(rowOddColor,rowEvenColor),10/2+1))),
      align = c('left', 'center','center'),
      font = list(color = c("#04103b"), size = 10),
      height = 20
    ))

  m<-list(r=0,b=0,t=0,l=0,par=4)
  fig<-fig%>%layout(margin=m)

  fig_list<-list("fig"=fig)

  return(fig_list)

}





#Life Performance
ggReg<-function(df,title="Title",subtitle="Subtitle",xcap="",ycap="",markersize=1,percx=T,percy=T,col_aq2 = c("#04103b", "#dd0400","#5777a7", "#D1E2EC"),fredr_key=NULL)
{
  library(ggplot2)
  names(df)<-c("assetx","assety")

  #cols <- setNames(c(col_aq2[1]), c("assetx","assety"))

  p<-
    ggplot(df, aes(x=df$assetx, y=df$assety))
  p<-p+
    geom_point(size=markersize,col=col_aq2[1])+
    geom_smooth(method=lm, se=FALSE,col=col_aq2[2])+
    theme_aq_black(base_size=24)+
    #size 22 for overleaf
    labs(color='')+
    labs(title=title,subtitle=subtitle,x =xcap)+
    labs(caption = '')+
    theme(legend.position = "none",legend.margin=margin(-20,-20,-20,-20),legend.box.margin=margin(0,0,0,0))+
    guides(colour = guide_legend(nrow = 1))
    #scale_x_date(labels = date_format("%Y"))
  if(percx==TRUE)
  {
    p<-p+scale_x_continuous(labels = scales::percent)
  }
  if(percy==TRUE)
  {
    p<-p+scale_y_continuous(labels = scales::percent)
  }
  p<-p+
    ylab(ycap)+
    theme(plot.margin=margin(5,5,5,5))

  reg<-lm(assety~assetx,data=df)
  plist=list("p"=p,"reg"=reg)
  print(summary(reg))
  return(plist)
}




allocSun2<-function(sb,sb_title="")
{
  library(scales)
  library(colorspace)
  col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
  names(sb)<-c("parents","ids","labels","values")
  sb_center<-sb %>%group_by(parents) %>%summarise(sum = sum(values))
  sb_center$ids<-sb_center$parents
  sb_center$labels<-sb_center$parents
  sb_center$parents<-""
  sb_center$values<-sb_center$sum
  sb_center<-sb_center[,c("parents","ids","labels","values")]
  sb_center<-as.data.frame(sb_center)

  sb_center1<-sb %>%group_by(parents,ids) %>%summarise(sum = sum(values))
  sb_center1$labels<-sb_center1$ids
  sb_center1$ids<-paste(sb_center1$parents,sb_center1$ids,sep=" - ")
  sb_center1$values<-sb_center1$sum
  sb_center1<-sb_center1[,c("parents","ids","labels","values")]
  sb_center1<-as.data.frame(sb_center1)

  sb$labels<-ifelse(is.na(sb$labels),"All Sectors",sb$labels)

  sb$parents<-paste(sb$parents,sb$ids,sep=" - ")
  sb$ids<-ifelse(sb$parents==sb$ids& sb$parents!="",sb$ids,sb$labels)
  sb$ids<-ifelse(sb$parents==sb$ids& sb$parents!="",paste(sb$parents,sb$labels,sep=" - "),sb$ids)

  #Customized Color Scheme
  library(colorspace)
  sb_center$colors<-1
  sb_center1$colors<-1
  sb$colors<-1
  sb_center<-sb_center[order(sb_center$values),]

  for(color_i in 1:nrow(sb_center))
  {
    sel_id<-substr(sb_center$ids[color_i],1,4)
    sb_center$colors<-ifelse(substr(sb_center$ids,1,4)==sel_id,col_aq2[color_i],sb_center$colors)
    sb_center1$colors<-ifelse(substr(sb_center1$parents,1,4)==sel_id,lighten(col_aq2[color_i],amount=0.1,method="relative"),sb_center1$colors)
    sb$colors<-ifelse(substr(sb$parents,1,4)==sel_id,lighten(col_aq2[color_i],amount=0.2,method="relative"),sb$colors)
  }
  sb_tmp<-rbind(sb_center,sb_center1,sb)

  p <- plot_ly(sb_tmp, ids = ~ids, labels = ~factor(labels), parents = ~parents,values=~round(100*values,2), type = 'sunburst')%>%
    layout(colorway  = ~colors,title =sb_title)%>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
    layout(title = list(text = chart_title, y = 1))


  plist=list("p"=p,"sel"=sb)

  return(plist)

}

allocTree2<-function(sb,sb_title="Allocation Tree")
{
  library(scales)
  library(colorspace)
  col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
  names(sb)<-c("parents","ids","labels","values")
  sb_center<-sb %>%group_by(parents) %>%summarise(sum = sum(values))
  sb_center$ids<-sb_center$parents
  sb_center$labels<-sb_center$parents
  sb_center$parents<-""
  sb_center$values<-sb_center$sum
  sb_center<-sb_center[,c("parents","ids","labels","values")]
  sb_center<-as.data.frame(sb_center)

  sb_center1<-sb %>%group_by(parents,ids) %>%summarise(sum = sum(values))
  sb_center1$labels<-sb_center1$ids
  sb_center1$ids<-paste(sb_center1$parents,sb_center1$ids,sep=" - ")
  sb_center1$values<-sb_center1$sum
  sb_center1<-sb_center1[,c("parents","ids","labels","values")]
  sb_center1<-as.data.frame(sb_center1)

  sb$labels<-ifelse(is.na(sb$labels),"All Sectors",sb$labels)

  sb$parents<-paste(sb$parents,sb$ids,sep=" - ")
  sb$ids<-ifelse(sb$parents==sb$ids& sb$parents!="",sb$ids,sb$labels)
  sb$ids<-ifelse(sb$parents==sb$ids& sb$parents!="",paste(sb$parents,sb$labels,sep=" - "),sb$ids)

  #Customized Color Scheme
  library(colorspace)
  sb_center$colors<-1
  sb_center1$colors<-1
  sb$colors<-1
  sb_center<-sb_center[order(sb_center$values),]

  for(color_i in 1:nrow(sb_center))
  {
    sel_id<-substr(sb_center$ids[color_i],1,4)
    sb_center$colors<-ifelse(substr(sb_center$ids,1,4)==sel_id,col_aq2[color_i],sb_center$colors)
    sb_center1$colors<-ifelse(substr(sb_center1$parents,1,4)==sel_id,lighten(col_aq2[color_i],amount=0.1,method="relative"),sb_center1$colors)
    sb$colors<-ifelse(substr(sb$parents,1,4)==sel_id,lighten(col_aq2[color_i],amount=0.2,method="relative"),sb$colors)
  }
  sb_tmp<-rbind(sb_center,sb_center1,sb)

  p <- plot_ly(sb_tmp, ids = ~ids, labels = ~factor(labels), parents = ~parents,values=~round(100*values,2),branchvalues="total", type = 'treemap')%>%
    layout(colorway  = ~colors,title =sb_title)%>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


  plist<-list("p"=p,"sel"=sb)

  return(plist)

}


allocSun2<-function(sb,sb_title="")
{
  library(scales)
  library(colorspace)
  col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
  names(sb)<-c("parents","ids","labels","values")
  sb_center<-sb %>%group_by(parents) %>%summarise(sum = sum(values))
  sb_center$ids<-sb_center$parents
  sb_center$labels<-sb_center$parents
  sb_center$parents<-""
  sb_center$values<-sb_center$sum
  sb_center<-sb_center[,c("parents","ids","labels","values")]
  sb_center<-as.data.frame(sb_center)

  sb_center1<-sb %>%group_by(parents,ids) %>%summarise(sum = sum(values))
  sb_center1$labels<-sb_center1$ids
  sb_center1$ids<-paste(sb_center1$parents,sb_center1$ids,sep=" - ")
  sb_center1$values<-sb_center1$sum
  sb_center1<-sb_center1[,c("parents","ids","labels","values")]
  sb_center1<-as.data.frame(sb_center1)

  sb$labels<-ifelse(is.na(sb$labels),"All Sectors",sb$labels)

  sb$parents<-paste(sb$parents,sb$ids,sep=" - ")
  sb$ids<-ifelse(sb$parents==sb$ids& sb$parents!="",sb$ids,sb$labels)
  sb$ids<-ifelse(sb$parents==sb$ids& sb$parents!="",paste(sb$parents,sb$labels,sep=" - "),sb$ids)

  #Customized Color Scheme
  library(colorspace)
  sb_center$colors<-1
  sb_center1$colors<-1
  sb$colors<-1
  sb_center<-sb_center[order(sb_center$values),]

  for(color_i in 1:nrow(sb_center))
  {
    sel_id<-substr(sb_center$ids[color_i],1,4)
    sb_center$colors<-ifelse(substr(sb_center$ids,1,4)==sel_id,col_aq2[color_i],sb_center$colors)
    sb_center1$colors<-ifelse(substr(sb_center1$parents,1,4)==sel_id,lighten(col_aq2[color_i],amount=0.1,method="relative"),sb_center1$colors)
    sb$colors<-ifelse(substr(sb$parents,1,4)==sel_id,lighten(col_aq2[color_i],amount=0.2,method="relative"),sb$colors)
  }
  sb_tmp<-rbind(sb_center,sb_center1,sb)

  p <- plot_ly(sb_tmp, ids = ~ids, labels = ~factor(labels), parents = ~parents,values=~round(100*values,2), type = 'sunburst')%>%
    layout(colorway  = ~colors,title =sb_title)%>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


  plist<-list("p"=p,"sel"=sb)

  return(plist)

}

allocTree2<-function(sb,sb_title="")
{
  library(scales)
  library(colorspace)
  col_aq2<-as.character(c("#04103b","#dd0400","#3b5171","#5777a7","#969696","#BDBDBD","#D9D9D9","#F0F0F0"))
  names(sb)<-c("parents","ids","labels","values")
  sb_center<-sb %>%group_by(parents) %>%summarise(sum = sum(values))
  sb_center$ids<-sb_center$parents
  sb_center$labels<-sb_center$parents
  sb_center$parents<-""
  sb_center$values<-sb_center$sum
  sb_center<-sb_center[,c("parents","ids","labels","values")]
  sb_center<-as.data.frame(sb_center)

  sb_center1<-sb %>%group_by(parents,ids) %>%summarise(sum = sum(values))
  sb_center1$labels<-sb_center1$ids
  sb_center1$ids<-paste(sb_center1$parents,sb_center1$ids,sep=" - ")
  sb_center1$values<-sb_center1$sum
  sb_center1<-sb_center1[,c("parents","ids","labels","values")]
  sb_center1<-as.data.frame(sb_center1)

  sb$labels<-ifelse(is.na(sb$labels),"All Sectors",sb$labels)

  sb$parents<-paste(sb$parents,sb$ids,sep=" - ")
  sb$ids<-ifelse(sb$parents==sb$ids& sb$parents!="",sb$ids,sb$labels)
  sb$ids<-ifelse(sb$parents==sb$ids& sb$parents!="",paste(sb$parents,sb$labels,sep=" - "),sb$ids)

  #Customized Color Scheme
  library(colorspace)
  sb_center$colors<-1
  sb_center1$colors<-1
  sb$colors<-1
  sb_center<-sb_center[order(sb_center$values),]

  for(color_i in 1:nrow(sb_center))
  {
    sel_id<-substr(sb_center$ids[color_i],1,4)
    sb_center$colors<-ifelse(substr(sb_center$ids,1,4)==sel_id,col_aq2[color_i],sb_center$colors)
    sb_center1$colors<-ifelse(substr(sb_center1$parents,1,4)==sel_id,lighten(col_aq2[color_i],amount=0.1,method="relative"),sb_center1$colors)
    sb$colors<-ifelse(substr(sb$parents,1,4)==sel_id,lighten(col_aq2[color_i],amount=0.2,method="relative"),sb$colors)
  }
  sb_tmp<-rbind(sb_center,sb_center1,sb)

  p <- plot_ly(sb_tmp, ids = ~ids, labels = ~factor(labels), parents = ~parents,values=~round(100*values,2),branchvalues="total", type = 'treemap')%>%
    layout(colorway  = ~colors,title =sb_title)%>%
    layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


  plist<-list("p"=p,"sel"=sb)

  return(plist)

}


gaugeR<-function(value=6,min=0,max=10,text="Score",chart_export_width_uploader,chart_export_height_uploader)
{
  col_aq_esg<-as.character(c("#dd0400","#D1E2EC","#5777a7","#04103b"))
  stepsize<-10
  cols_esg = colorRampPalette(col_aq_esg)(stepsize)
  #show_col(cols_esg)
  fig <-
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = value,
      title = list(text = text),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        bar = list(color = "#04103b"),
        axis =list(range = list(min, max)),
        steps = list(
          list(range = c(min, (max/stepsize)), color = cols_esg[1]),
          list(range = c((max/stepsize)*1, (max/stepsize)*2), color = cols_esg[2]),
          list(range = c((max/stepsize)*2, (max/stepsize)*3), color = cols_esg[3]),
          list(range = c((max/stepsize)*3, (max/stepsize)*4), color = cols_esg[4]),
          list(range = c((max/stepsize)*4, (max/stepsize)*5), color = cols_esg[5]),
          list(range = c((max/stepsize)*5, (max/stepsize)*6), color = cols_esg[6]),
          list(range = c((max/stepsize)*6, (max/stepsize)*7), color = cols_esg[7]),
          list(range = c((max/stepsize)*7, (max/stepsize)*8), color = cols_esg[8]),
          list(range = c((max/stepsize)*8, (max/stepsize)*9), color = cols_esg[9]),
          list(range = c((max/stepsize)*9, (max/stepsize)*10), color = cols_esg[10])
        ),
        threshold = list(
          line = list(color = "#f4f4f4", width = 3),thickness = 1,value = value)
      )
    )
  fig <- fig %>%
    layout(margin = list(l=20,r=30))
  fig <- fig %>% config(toImageButtonOptions = list( format = "svg",filename = "gauge",width = chart_export_width_uploader,height = chart_export_height_uploader))
  return(fig)
}



allocBarh<-function (da, chart_title = "Portfolio Allocation", chart_height = 400,
                     chart_font_size = 11, chart_export_width = 600, chart_export_height = 450,
                     m = list(r = 0, l = 0, b = 0, t = 50, par = 4), barcol = "#f9f9f9",
                     barborder = "#3b5171")
{


  #if (!require("dplyr"))
  #  install.packages("dplyr")
  #if (!require("plotly"))
  #  install.packages("plotly")
  #if (!require("lubridate"))
  #  install.packages("lubridate")
  library(dplyr)
  library(plotly)
  library(lubridate)
  da <- as.data.frame(da)
  da <- da[, 1:2]
  names(da) <- c("assets", "weight")
  da$weight <- as.numeric(da$weight)
  col_aq2 <- as.character(c("#04103b", "#dd0400",
                            "#3b5171"))
  y_axis_caption <- ""
  da <- da %>% group_by(assets) %>% summarize(weight = sum(as.numeric(weight)))
  da$assets <- factor(da$assets, levels = unique(da$assets)[order(as.numeric(da$weight),  decreasing = T)])
  p <- plot_ly(da, type = "bar", y = ~as.numeric(da$weight), textangle=0,
               x = ~da$assets, text = ~paste0(" ", da$assets),
               name = "Portfolio", height = chart_height, marker = list(color = barcol,
                                                                        line = list(width = 0.5, color = barborder)), texttemplate = paste0("<b>",
                                                                                                                                            round(as.numeric(da$weight),3)*100,"%", "</b>"), textfont = 20, textposition = "outside",
               insidetextanchor = "start")
  p <- p %>% layout(yaxis = list(showticklabels = FALSE))
  p <- p %>% layout(margin = m, font = list(size = chart_font_size),
                    title = chart_title, xaxis = list(title = y_axis_caption,tickangle=-45), yaxis = list(title = y_axis_caption),
                    barmode = "group")
  p <- p %>% config(toImageButtonOptions = list(format = "svg",
                                                filename = "allocation_pie", width = chart_export_width,
                                                height = chart_export_height))
  return(p)
}
