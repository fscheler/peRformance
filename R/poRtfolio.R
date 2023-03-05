


alloc_pie<-function(df,chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 50,t = 50,pad = 4))
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


alloc_tree<-function(df,parent_label="Portfolio",chart_export_width=600,chart_export_height=450,m = list(l = 0,r = 0,b = 0,t = 0,pad = 4))
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
    dl <- melt(setDT(da), id.vars = "date")
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
    layout(margin = m,title="Risk & Return",xaxis = list(title="Risk",tickformat =".1%",range = list(0, max(dls$sd_ann*1.2))), yaxis = list(title="Return",tickformat =".1%",range = list(0, max(dls$ret_ann*1.2))),legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.2))


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
    scale_x_continuous(labels = scales::percent_format(accuracy=.1),limits=c(0, max(dls$sd_ann*1.2)))+
    scale_y_continuous(labels = scales::percent_format(accuracy=.1),limits=c(0, max(dls$ret_ann*1.2)))+
    theme(panel.grid.major.x = element_line(colour = "#D8D8D8"))

    res_list<-list("dls"=dls,"rr_plotly"=rr_plotly,"rr_ggplot"=rr_ggplot)

  }else{
    res_list<-list("dls"=dls)
  }

  return(res_list)
}


rrScatEff<-function(da,ret_format="returns",table_format='wide',ann_factor=252,chart_export_width=600,chart_export_height=450,m = list(l = 50,r = 50,b = 80,t = 50,pad = 4))
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

  dls<-risk_return_scatter(da,ret_format=ret_format,table_format=table_format,graphics=F)$dls

  if(table_format!='wide')
  {
    names(da)<-c("date",'id','value')
    da$date<-as.Date(da$date)
    da<-dcast(as.data.table(da), date ~ id, value.var = "value")
  }else{
    names(da)[1]<-"date"
    da$date<-as.Date(da$date)
  }

  dz<-read.zoo(da,index.column=1)
  R<-as.xts(dz)

  if(ret_format!="returns")
  {
    R<-apply(R,2,calc_log_ret)
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

  ef<-create.EfficientFrontier(R=R, portfolio=init.portf, type="mean-var", n.portfolios = 20,risk_aversion = NULL, match.col = "ES", search_size = 500)



  #as.numeric(ef$frontier[,4:ncol(ef$frontier)])
  #global_wd<<-"C:/FS/quantamental_platform/Docker_ShinyPortfolioOptimizer/app3/"
  #source(paste0(global_wd,"public_optimizer_functions/custom_moments_optimizer_hacked.R"))
  #opt_qu <- PortfolioAnalytics::optimize.portfolio(R, portfolio=init.portf, optimize_method="ROI")

  eff<-as.data.frame(cbind(ef$frontier[,4:ncol(ef$frontier)]))


  eff$ret_ann<-rowSums(map2_dfc(eff, dls$ret_ann, `*`))
  eff$sd_ann<-ef$frontier[,2]*252^0.5

  p<-plot_ly(eff, x=~sd_ann, y=~ret_ann,type='scatter',mode='line', colors = cols,line=list(color='grey'))%>%
    add_trace( x=dls$sd_ann,y=dls$ret_ann,color = dls$variable,marker=list(size=12),mode='markers')%>%
    layout(margin = m,title="Risk & Return",
           xaxis = list(title="Risk",tickformat =".1%",range = list(min(eff$sd_ann/1.1), max(dls$sd_ann*1.1))),
           yaxis = list(title="Return",tickformat =".1%",range = list(min(dls$ret_ann/1.1,eff$ret_ann/1.2), max(dls$ret_ann*1.1))),legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.2))

  return(p)
}
