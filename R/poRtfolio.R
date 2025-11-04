
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
                    m = list(r = 0, l = 0, b = 0, t = 50, par = 4),plotly=T,title_pos="center",perc=".0%")
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
  library(dplyr)
  da <- as.data.frame(da)
  da <- da[, 1:2]
  names(da) <- c("assets", "weight")
  da$weight <- as.numeric(da$weight)
  col_aq2 <- as.character(c("#04103b", "#dd0400", "#3b5171"))
  y_axis_caption <- ""
  da <- da %>% dplyr::group_by(assets) %>% dplyr::summarize(weight = sum(as.numeric(weight)))
  da$assets <- factor(da$assets, levels = unique(da$assets)[order(as.numeric(da$weight),
                                                                  decreasing = F)])
  p <- plot_ly(da, x = as.numeric(da$weight), y = da$assets,
               height = chart_height, type = "bar", name = "Portfolio",
               marker = list(color = col_aq2[1]))

  p <- p %>% layout(margin = m, font = list(size = chart_font_size),
                    title = chart_title, xaxis = list(title = y_axis_caption,
                                                      tickformat = perc), yaxis = list(title = y_axis_caption),
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
    if(perc==".0%")
    {
      p<-p+scale_x_continuous(labels = scales::percent_format(accuracy = 1))
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

  da<-da%>%dplyr::group_by(assets)%>%dplyr::summarize(weight=sum(as.numeric(weight)))

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
  library(dplyr)

  da<-da[,1:3]
  names(da)<-c("assets","gross","net")


  col_aq2<-as.character(c("#04103b","#dd0400","#3b5171"))
  y_axis_caption<-""
  #chart_font_size<-16

  da<-da%>%dplyr::group_by(assets)%>%dplyr::summarize(gross=sum(as.numeric(gross)),net=sum(as.numeric(net)))

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
  #if (!require("scales")) install.packages("scales")
  #if (!require("plotly")) install.packages("plotly")
  #if (!require("dplyr")) install.packages("dplyr")

  library(scales)
  library(plotly)
  library(dplyr)
  #library(grDevices)

  names(df)<-c("assets","weights")
  df<-df%>%dplyr::group_by(assets)%>%dplyr::summarize(weights=sum(weights))
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
      theme_aq_black_pie(base_size = base_size)+
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


allocTree<-function(df,parent_label="Portfolio",chart_export_width=600,chart_export_height=450,m = list(l = 0,r = 0,b = 0,t = 0,pad = 4),col_aq2=as.character(c("#04103b","#5777a7","#D1E2EC","#dd0400")))
{
  #if (!require("scales")) install.packages("scales")
  #if (!require("plotly")) install.packages("plotly")
  #if (!require("dplyr")) install.packages("dplyr")

  library(scales)
  library(plotly)
  library(dplyr)
  #library(grDevices)

  names(df)<-c("assets","weights")
  df<-df%>%dplyr::group_by(assets)%>%dplyr::summarize(weights=sum(weights))
  df$weights<-df$weights/sum(df$weights)

  wmp<-df
  wmp$Parent_Label<-parent_label

  regions<-wmp%>%dplyr::group_by(assets)%>%dplyr::summarize(sum= sum(weights,na.rm=T))

    library(plotly)

    regions$perc<-regions$sum/sum(regions$sum)
    regions$Parent_Label<-parent_label
    regions<-rbind(regions,c(parent_label,sum(regions$sum),1,""))
    regions<-as.data.frame(regions,stringsAsFactors=F)
    regions$perc<-as.numeric(regions$perc)

    #m<-list(r=0,l=0,b=0,t=0,par=4)


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




rrScatEff<-
  function (da, ret_format = "returns", table_format = "wide",
            ann_factor = 252, chart_export_width = 600, chart_export_height = 450,
            m = list(l = 50, r = 50, b = 80, t = 50, pad = 4), n.portfolios = 30,
            box_constraint_list = NULL,ggbase_size=24,flex_caption="Source: Created with the peRformance package")
  {

    library(purrr)
    library(PortfolioAnalytics)
    library(plotly)
    library(scales)
    calc_ret <- function(x) {
      x <- x/lagpad(x, k = 1) - 1
      x[1] <- 0
      return(x)
    }
    calc_log_ret <- function(x) {
      x <- log(x/lagpad(x, k = 1))
      x[1] <- 0
      return(x)
    }
    dls <- rrScat(da, ret_format = ret_format, table_format = table_format,
                  graphics = F)$dls
    if (table_format != "wide") {
      names(da) <- c("date", "id", "value")
      da$date <- as.Date(da$date)
      da <- data.table::dcast(as.data.table(da), date ~ id,
                              value.var = "value")
    }else {
      names(da)[1] <- "date"
      da$date <- as.Date(da$date)
    }
    dz <- read.zoo(da, index.column = 1)
    R <- as.xts(dz)
    if (ret_format != "returns") {
      R <- apply(R, 2, calc_ret)
    }
    funds <- colnames(R)
    init.portf <- portfolio.spec(assets = funds)
    init.portf <- add.constraint(portfolio = init.portf, type = "weight_sum",
                                 min_sum = 0.99999, max_sum = 1.0001)
    init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
    init.portf <- add.objective(portfolio = init.portf, type = "return",
                                name = "mean")
    init.portf <- add.objective(portfolio = init.portf, type = "risk",
                                name = "StdDev")
    if (is.null(box_constraint_list)) {
      box_constraint_list <- list(min = c(rep(0, length(funds))),
                                  max = c(rep(1, length(funds))))
    }
    init.portf <- add.constraint(portfolio = init.portf, type = "box",
                                 min = box_constraint_list$min[1:length(funds)], max = box_constraint_list$max[1:length(funds)])
    annualized.moments <- function(R, scale = ann_factor, portfolio = NULL) {
      out <- list()
      out$mu <- (1 + dls$ret_ann)^(1/ann_factor) - 1
      out$sigma <- cov(R)
      return(out)
    }
    ef <- create.EfficientFrontier(R = R, portfolio = init.portf,
                                   type = "mean-StdDev", n.portfolios = n.portfolios, match.col = "StdDev",
                                   search_size = 500, momentFUN = "annualized.moments",
                                   scale = ann_factor)
    eff <- as.data.frame(cbind(ef$frontier[, 4:ncol(ef$frontier)]))
    eff <- eff/rowSums(eff)
    eff$ret_ann <- rowSums(map2_dfc(eff, dls$ret_ann, `*`))
    eff$sd_ann <- ef$frontier[, 2] * 252^0.5
    col_aq2 <- as.character(c("#04103b", "#5777a7", "#D1E2EC",
                              "#dd0400"))
    cols = colorRampPalette(col_aq2)(nrow(dls))
    p <- plot_ly(eff, x = ~sd_ann, y = ~ret_ann, type = "scatter",
                 mode = "line", colors = cols, line = list(color = "grey"),
                 name = "Efficient Frontier") %>% add_trace(x = dls$sd_ann,
                                                            y = dls$ret_ann, color = dls$variable, marker = list(size = 12),
                                                            mode = "markers", name = dls$variable) %>% layout(margin = m,
                                                                                                              title = "Risk & Return", xaxis = list(title = "Risk",
                                                                                                                                                    tickformat = ".1%", range = list(min(eff$sd_ann/1.1),
                                                                                                                                                                                     max(dls$sd_ann * 1.1))), yaxis = list(title = "Return",
                                                                                                                                                                                                                           tickformat = ".1%"), legend = list(orientation = "h",
                                                                                                                                                                                                                                                              xanchor = "center", x = 0.5, y = -0.2))
    p <- p %>% config(toImageButtonOptions = list(format = "svg",
                                                  filename = "efficient_frontier", width = chart_export_width,
                                                  height = chart_export_height))
    rr_ggplot <<- ggplot(NULL, aes(sd_ann, ret_ann)) + geom_point(cex = 5,
                                                                  data = dls, aes(color = factor(variable))) + geom_line(data = eff) +
      scale_color_manual(values = cols) + theme_aq_black_default_font(base_size = ggbase_size) +
      labs(color = "") + labs(title = "Risk & Return", subtitle = "Annualized values in % including efficient frontier",
                              x = "") + labs(caption = flex_caption) +
      xlab("Risk") + ylab("Excess Return") + theme(legend.position = "bottom",
                                                   legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(15,
                                                                                                                          0, 30, 0)) + guides(colour = guide_legend(nrow = 2)) +
      theme(plot.margin = margin(l = 5, r = 10, b = 5, t = 5)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      theme(panel.grid.major.x = element_line(colour = "#D8D8D8"))
    reslist <- list(dls = dls, eff = eff, eff_plotly = p, eff_ggplot = rr_ggplot)
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
    chart_export_height=150,
    bgcolors="#04103b"
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
  p<-p%>%layout(plot_bgcolor='white') %>% layout(paper_bgcolor=bgcolors)
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

rangeR<-
  rangeR <- function (y = c("Dividend Yield %"), benchmark = 2.2, portfolio = 3.9,
                      caption_bm = paste0(2.2, "%"), caption_po = paste0(3.9, "%"),
                      chart_height = 200, m = list(r = 0, l = 0, b = 0, t = 0, par = 4),minoffset=25,specialoffsetthreshold=0.5,specialoffset=70) {

    library(plotly)
    s <- data.frame(y, benchmark, portfolio)

    fig <- plot_ly(s, color = I("lightgrey"), height = chart_height)
    fig <- fig %>% add_segments(x = ~benchmark, xend = ~portfolio,
                                y = ~y, yend = ~y, showlegend = FALSE,line = list(color = "lightgrey", width = 1))
    fig <- fig %>% add_markers(x = ~benchmark, y = ~y, name = "Benchmark",
                               color = I("#dd0400"), text = paste0("Market<br>", benchmark))
    fig <- fig %>% add_markers(x = ~portfolio, y = ~y, name = "Portfolio",
                               color = I("#5777a7"))

    # ---- Avoid overlapping annotations (left & right offset) ----
    dx <- abs(s$benchmark - s$portfolio)
    offset <- ifelse(dx < specialoffsetthreshold, specialoffset, minoffset)   # increase horizontal offset if too close

    fig <- fig %>% add_annotations(x = s$benchmark, y = s$y,
                                   text = caption_bm, xref = "x", yref = "y",
                                   showarrow = TRUE, arrowhead = 0, arrowsize = 0,
                                   ax = -offset, ay = 0,
                                   arrowwidth = 0.5)   # thinner arrow line

    fig <- fig %>% add_annotations(x = s$portfolio, y = s$y,
                                   text = caption_po, xref = "x", yref = "y",
                                   showarrow = TRUE, arrowhead = 0, arrowsize = 0,
                                   ax = offset, ay = 0,
                                   arrowwidth = 0.5)    # thinner arrow line
    # -------------------------------------------------------------

    fig <- fig %>% layout(margin = m, barmode = "stack", showlegend = F,
                          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE),
                          yaxis = list(title = "", showgrid = FALSE)) %>%
      layout(plot_bgcolor = "rgba(0, 0, 0, 0)",
             paper_bgcolor = "rgba(0, 0, 0, 0)",
             yaxis = list(zerolinecolor = "#f9f9f9", showgrid = FALSE),
             xaxis = list(zerolinecolor = "#f9f9f9", showgrid = FALSE))

    return(fig)
  }

gglineRoct<-
  function (df, title = "Title", subtitle = "Subtitle", xcap = "",
            ycap = "", name1 = "asset", name2 = "benchmark", name3 = "benchmark1",
            name4 = "benchmark2", name5 = "benchmark3", name6 = "benchmark4", name7 = "benchmark5", name8 = "benchmark6", perc = F, col_aq2 = c("#04103b",
                                                                                                                                                "#dd0400", "#5777a7", "#D1E2EC", "grey", "green", "lightgrey", "darkgreen"), fredr_key = NULL,
            secaxis = 1, base_size = 24, legend_rows = 2)
  {
    library(ggplot2)
    names(df) <- c("date", "asset", "benchmark", "benchmark1",
                   "benchmark2", "benchmark3", "benchmark4", "benchmark5", "benchmark6")
    df$date <- as.Date(df$date)
    cols <- setNames(c(col_aq2[1], col_aq2[2], col_aq2[3], col_aq2[4],
                       col_aq2[5],col_aq2[6],col_aq2[7],col_aq2[8]), c(name1, name2, name3, name4, name5, name6, name7, name8))
    secaxisfactor <- 2
    p <- ggplot(data = df, aes(x = as.Date(df$date), y = df$asset))
    if (!is.null(fredr_key)) {
      p <- p + ggRec(as.Date(min(df$date)), as.Date(max(df$date)),
                     fredr_key = fredr_key)
    }
    p <- p + geom_line(size = 0.8, aes(y = df$asset, color = name1)) +
      geom_line(size = 0.8, aes(y = df$benchmark/secaxis, color = name2)) +
      geom_line(size = 0.8, aes(y = df$benchmark1/secaxis, color = name3)) +
      geom_line(size = 0.8, aes(y = df$benchmark2/secaxis,   color = name4)) +
      geom_line(size = 0.8, aes(y = df$benchmark3/secaxis, color = name5))+
      geom_line(size = 0.8, aes(y = df$benchmark4/secaxis, color = name6))+
      geom_line(size = 0.8, aes(y = df$benchmark5/secaxis, color = name7))+
      geom_line(size = 0.8, aes(y = df$benchmark6/secaxis, color = name8))+
      scale_colour_manual(values = cols) +
      theme_aq_black(base_size = base_size) + labs(color = "") +
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

#Life Performance
gglineR<-function(df,title="Title",subtitle="Subtitle",xcap="",ycap="",name1="Portfolio",perc=T,col_aq2 = c("#04103b", "#dd0400","#5777a7", "#D1E2EC"),fredr_key=NULL,base_size=24)
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
    theme_aq_black(base_size=base_size)+
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


ggbaR<-
  function (df, title = "Title", subtitle = "Subtitle", xcap = "",
            ycap = "", name1 = "Portfolio", perc = T, col_aq2 = c("#04103b",
                                                                  "#dd0400"), fredr_key = NULL)
  {
    library(ggplot2)
    names(df) <- c("date", "asset")
    df$date <- as.Date(df$date)
    cols <- setNames(c(col_aq2[1]), c(name1))
    p <- ggplot(data = df, aes(x = as.Date(df$date), y = df$asset))
    if (!is.null(fredr_key)) {
      p <- p + ggRec(as.Date(min(df$date)), as.Date(max(df$date)),
                     fredr_key = fredr_key)
    }
    p <- p + geom_col(size = 0.8,fill = col_aq2[1])+
    #p <- p + geom_line(size = 0.8,color = col_aq2[2]) +
      #scale_colour_manual(values = cols) +
      theme_aq_black(base_size = 24) +
      labs(color = "") + labs(title = title, subtitle = subtitle,
                              x = xcap) + labs(caption = "") + theme(legend.position = "none",
                                                                     legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(0,
                                                                                                                                            0, 0, 0)) + guides(colour = guide_legend(nrow = 1)) +
      scale_x_date(labels = date_format("%Y"), expand = c(0, 0))
    if (perc == TRUE) {
      p <- p + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
    }
    p <- p + ylab(ycap) + theme(plot.margin = margin(5, 5, 5,
                                                     5))
    return(p)
  }


ggaRea<-
  function (df, title = "Title", subtitle = "Subtitle", xcap = "",
            ycap = "", name1 = "Portfolio", perc = T, col_aq2 = c("#04103b",
                                                                  "#dd0400"), fredr_key = NULL)
  {
    library(ggplot2)
    names(df) <- c("date", "asset")
    df$date <- as.Date(df$date)
    cols <- setNames(c(col_aq2[1]), c(name1))
    p <- ggplot(data = df, aes(x = as.Date(df$date), y = df$asset))
    if (!is.null(fredr_key)) {
      p <- p + ggRec(as.Date(min(df$date)), as.Date(max(df$date)),
                     fredr_key = fredr_key)
    }
    p <- p + geom_area(size = 0.8,fill = col_aq2[1])
    p <- p + geom_line(size = 0.8,color = col_aq2[2]) +
      #scale_colour_manual(values = cols) +
      theme_aq_black(base_size = 24) +
      labs(color = "") + labs(title = title, subtitle = subtitle,
                              x = xcap) + labs(caption = "") + theme(legend.position = "none",
                                                                     legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(0,
                                                                                                                                            0, 0, 0)) + guides(colour = guide_legend(nrow = 1)) +
      scale_x_date(labels = date_format("%Y"), expand = c(0, 0))
    if (perc == TRUE) {
      p <- p + scale_y_continuous(labels = scales::percent, expand = c(0, 0))
    }
    p <- p + ylab(ycap) + theme(plot.margin = margin(5, 5, 5,
                                                     5))
    return(p)
  }

#Life Performance
gglineRt<-function(df,title="Title",subtitle="Subtitle",xcap="",ycap="",name1="asset",name2="benchmark",perc=F,col_aq2 = c("#04103b", "#dd0400","#5777a7", "#D1E2EC"),fredr_key=NULL,secaxis=1,secaxisadd=0,legend_rows=1,base_size=24)
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
    geom_line(size=0.8,aes(y=df$benchmark/secaxis-secaxisadd,color=name2))+
    scale_colour_manual(values = cols)+
    theme_aq_black(base_size=base_size)+
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
      sec.axis = sec_axis( trans=~.*secaxis+secaxisadd*secaxis, name="")
    )
  }


  return(p)
}

gglineRsx<-
  function (df, title = "Title", subtitle = "Subtitle", xcap = "",
            ycap = "", name1 = "asset", name2 = "benchmark", name3 = "benchmark1",
            name4 = "benchmark2", name5 = "benchmark3", name6 = "benchmark4",
            name7 = "benchmark5", name8 = "benchmark6", perc = F, col_aq2 = c("#04103b",
                                                                              "#dd0400", "#5777a7", "#D1E2EC", "grey", "green", "lightgrey",
                                                                              "darkgreen"), fredr_key = NULL, secaxis = 1, base_size = 24,
            legend_rows = 2)
  {
    library(ggplot2)
    names(df) <- c("date", "asset", "benchmark", "benchmark1",
                   "benchmark2", "benchmark3", "benchmark4")
    df$date <- as.Date(df$date)
    cols <- setNames(c(col_aq2[1], col_aq2[2], col_aq2[3], col_aq2[4],
                       col_aq2[5], col_aq2[6]), c(name1,
                                                  name2, name3, name4, name5, name6))
    secaxisfactor <- 2
    p <- ggplot(data = df, aes(x = as.Date(df$date), y = df$asset))
    if (!is.null(fredr_key)) {
      p <- p + ggRec(as.Date(min(df$date)), as.Date(max(df$date)),
                     fredr_key = fredr_key)
    }
    p <- p + geom_line(size = 0.8, aes(y = df$asset, color = name1)) +
      geom_line(size = 0.8, aes(y = df$benchmark/secaxis, color = name2)) +
      geom_line(size = 0.8, aes(y = df$benchmark1/secaxis,
                                color = name3)) + geom_line(size = 0.8, aes(y = df$benchmark2/secaxis,
                                                                            color = name4)) + geom_line(size = 0.8, aes(y = df$benchmark3/secaxis,
                                                                                                                        color = name5)) + geom_line(size = 0.8, aes(y = df$benchmark4/secaxis,
                                                                                                                                                                    color = name6))  +
      scale_colour_manual(values = cols) +
      theme_aq_black(base_size = base_size) + labs(color = "") +
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

ggStacked<-function(long,title="Title",subtitle="Subtitle",perc=T,rel=T,lrow=2,col_aq2 = as.character(c("#04103b", "#5777a7", "#D1E2EC", "#dd0400")),minmaxx=F,base_size=20)
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
    theme_aq_black_default_font(base_size = base_size) +
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




ggReg2<-function (df, title = "Title", subtitle = "Subtitle", xcap = "",
                  captions = TRUE, regression = "linear", ycap = "", markersize = 1,
                  percx = T, percy = T, col_aq2 = c("#04103b", "#dd0400", "#5777a7",
                                                    "#D1E2EC"), fredr_key = NULL, nudge_x = 0, nudge_y = 0,linetype="dotted",legendposition="bottom",
                  xintercept = NULL)
{
  #df<-agg[,c("Prim_Exch","Sector_Name","Median_P_E","Exp_Earnings_Growth")]
  library(ggplot2)
  names(df) <- c("group","caption","assetx", "assety")


  group_levels <- unique(df$group)  # Get unique group names
  custom_colors <- setNames(col_aq2[seq_along(group_levels)], group_levels)

  p <- ggplot(df, aes(x = df$assetx, y = df$assety, color = group))
  p <- p + geom_point(size = markersize)
  p <- p+scale_color_manual(values = custom_colors)
  p <- p+ guides(caption = "none")
  if (regression == "linear") {
    p<-p+ geom_smooth(method = "lm", se = FALSE, aes(group = group, color = group),show.legend=FALSE,linetype =linetype, size = 0.7) +
      scale_color_manual(values = custom_colors)
  }
  p <- p + theme_aq_black(base_size = 24) + labs(color = "") +
    labs(title = title, subtitle = subtitle, x = xcap) +
    labs(caption = "") + theme(legend.position = legendposition,
                               legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(20,20, 20, 20)) + guides(colour = guide_legend(nrow = 1))
  if (captions == TRUE) {
    p <- p + geom_text(label = df$caption, check_overlap = T,
                       nudge_x = nudge_x, nudge_y = nudge_y,show.legend=FALSE)
  }
  if (!is.null(xintercept)) {
    p <- p + geom_vline(xintercept = xintercept, colour = "lightgrey",
                        linetype = "longdash")
  }
  if (percx == TRUE) {
    p <- p + scale_x_continuous(labels = scales::percent)
  }
  if (percy == TRUE) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  p <- p + ylab(ycap) + theme(plot.margin = margin(5, 5, 5,  5))

  reg <- lm(assety ~ assetx, data = df)
  plist = list(p = p, reg = reg)
  print(summary(reg))
  return(plist)
}


#Life Performance
ggReg<-function (df, title = "Title", subtitle = "Subtitle", xcap = "", captions=TRUE,regression="linear",
                  ycap = "", markersize = 1, percx = T, percy = T, col_aq2 = c("#04103b", "#dd0400", "#5777a7", "#D1E2EC"), fredr_key = NULL,nudge_x = 0, nudge_y = 0,xintercept=NULL,regsize=0.5,reglinetype ="dashed")
{

  library(ggplot2)
  names(df) <- c("assetx", "assety")
  p <- ggplot(df, aes(x = df$assetx, y = df$assety))
  p <- p + geom_point(size = markersize, col = col_aq2[1])

  if(regression=="linear")
  {
    p<-p+geom_smooth(method = lm, se = FALSE, col = col_aq2[2],size=regsize,linetype =reglinetype)
  }


  p<-p+theme_aq_black(base_size = 24) + labs(color = "") + labs(title = title,
                                                                subtitle = subtitle, x = xcap) + labs(caption = "") +
    theme(legend.position = "none", legend.margin = margin(-20,
                                                           -20, -20, -20), legend.box.margin = margin(0, 0,
                                                                                                      0, 0)) + guides(colour = guide_legend(nrow = 1))
  if(captions==TRUE)
  {
    p<-p+geom_text(label=rownames(df),check_overlap = T,nudge_x = nudge_x, nudge_y = nudge_y)
  }
  if(!is.null(xintercept))
  {
    p<-p+geom_vline(xintercept = xintercept, colour="lightgrey", linetype = "longdash")
  }
  if (percx == TRUE) {
    p <- p + scale_x_continuous(labels = scales::percent)
  }
  if (percy == TRUE) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  p <- p + ylab(ycap) + theme(plot.margin = margin(5, 5, 5,
                                                   5))
  reg <- lm(assety ~ assetx, data = df)
  plist = list(p = p, reg = reg)
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

gaugeR<-function(value=6,min=0,max=10,text="Score",chart_export_width_uploader=200,chart_export_height_uploader=100,
                 stepsize = 10,col_aq_esg = as.character(c("#dd0400", "#D1E2EC", "#5777a7", "#04103b")))
{
  #col_aq_esg<-as.character(c("#dd0400","#D1E2EC","#5777a7","#04103b"))
  #stepsize<-10
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
  da <- da %>% dplyr::group_by(assets) %>% dplyr::summarize(weight = sum(as.numeric(weight)))
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



gggaugeR <- function(pos=10,caption="",title="",breaks=c(0,10,20,30,40,50,60,70,80,90,100))
{
  require(ggplot2)

  col_aq_esg = as.character(c("#04103b","#D1E2EC","#dd0400"))
  cols<-colorRampPalette(col_aq_esg)(10)

  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill=cols[1])+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill=cols[2])+
    geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill=cols[3])+
    geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill=cols[4])+
    geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill=cols[5])+
    geom_polygon(data=get.poly(breaks[6],breaks[7]),aes(x,y),fill=cols[6])+
    geom_polygon(data=get.poly(breaks[7],breaks[8]),aes(x,y),fill=cols[7])+
    geom_polygon(data=get.poly(breaks[8],breaks[9]),aes(x,y),fill=cols[8])+
    geom_polygon(data=get.poly(breaks[9],breaks[10]),aes(x,y),fill=cols[9])+
    geom_polygon(data=get.poly(breaks[10],breaks[11]),aes(x,y),fill=cols[10])+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    #geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
    #          aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0.98,label=caption,vjust=0,size=5,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(
          axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank())+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))

}



#Stylebox
#------------------------------------------------------------------------------------------------------------------------------------------------------------
styleBox2<-function(
    number_categories_horizontal=5,
    number_categories_vertical=5,
    highlight_category=4,
    main_color="#5777a7",
    highlight_color="#dd0400",
    font_color="#04103b",
    chart_font_size=14,
    chart_caption_1="Risk Category",
    chart_caption_2="Return Category",
    chart_caption_center="Not Available/Not Applicable",
    opacity_scale_factor=0.5,
    custom_width=300,
    custom_height=180,
    dups=""
)
{
  #Update

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

if(dups=="")
{
  dups<-""
}else{
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
}



  stock_style_graph <- plot_ly(x = list("1"), y = list("1"), hoverinfo = "none",
                               marker = list("opacity" = 0), mode = "markers", name = "B", type = "scatter",
                               width = custom_width,
                               height = custom_height) %>%
    layout(title = "",
           annotations = list(
             list(
               "x" = 0.990130093458,
               "y" = 1.00181709504,
               "align" = "left",
               "font" = list("size" = chart_font_size,color=font_color),
               "showarrow" = FALSE,
               "text" = paste0("<b>",chart_caption_1,"</b>"),
               "xref" = "x",
               "yref" = "y"
             ),
             list(
               "x" = 0.989694747864+(1.00064057995-0.989694747864)/2,
               "y" = 0.5,
               "align" = "left",
               "font" = list("size" = chart_font_size,color=font_color),
               "showarrow" = FALSE,
               "text" = paste0(chart_caption_center),
               "xref" = "x",
               "yref" = "y"
             ),
             list(
               "x"= 1.00001816013,
               "y"= 1.35907755794e-16,
               "font" = list("size" = chart_font_size,color=font_color),
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
  return(stock_style_graph)

}



flextableR<-function(df,customwidth=1,alignnumbers="",alignvalues="",padding_left=20,padding_right=30)
{
  library(officer)
  library(flextable)
  theme_zebra_fs<-
    function (x, odd_header = "#CFCFCF", odd_body = "#EFEFEF", even_header = "transparent",
              even_body = "transparent")
    {
      if (!inherits(x, "flextable")) {
        stop(sprintf("Function `%s` supports only flextable objects.",
                     "theme_zebra()"))
      }
      h_nrow <- nrow_part(x, "header")
      f_nrow <- nrow_part(x, "footer")
      b_nrow <- nrow_part(x, "body")
      x <- border_remove(x)
      x <- align(x = x, align = "center", part = "header")
      if (h_nrow > 0) {
        even <- seq_len(h_nrow)%%2 == 0
        odd <- !even
        x <- bg(x = x, i = odd, bg = odd_header, part = "header")
        x <- bg(x = x, i = even, bg = even_header, part = "header")
        x <- bold(x = x, bold = TRUE, part = "header")
      }
      if (f_nrow > 0) {
        even <- seq_len(f_nrow)%%2 == 0
        odd <- !even
        x <- bg(x = x, i = odd, bg = odd_header, part = "footer")
        x <- bg(x = x, i = even, bg = even_header, part = "footer")
        x <- bold(x = x, bold = TRUE, part = "footer")
      }
      if (b_nrow > 0) {
        even <- seq_len(b_nrow)%%2 == 0
        odd <- !even
        x <- bg(x = x, i = odd, bg = odd_body, part = "body")
        x <- bg(x = x, i = even, bg = even_body, part = "body")
      }
      #x <- align_text_col(x, align = "left", header = TRUE)
      #x <- align_nottext_col(x, align = "right", header = TRUE)
      x
    }

  #df<-tmp_save
  #alignnumbers=""

  #df %>% kbl() %>% kable_styling(bootstrap_options = c("striped", "hover"))
  std_border <- fp_border(color = "red")
  #df<-flextable(df) %>%theme_zebra() %>% autofit()

  if(alignnumbers=="")
  {
    ncol_align<-ncol(df)
    alignnumbers<-c(1,2:ncol_align)
    alignvalues<-c("left",rep("right",ncol_align-1))
  }

  df <- flextable(df)
  df<- padding(df, padding.left = padding_left)
  df<- padding(df, padding.right = padding_right)
  #df <- align_text_col(df, align = "left")
  #df <- align_nottext_col(df, align = "right")
  df <- align(df, j = alignnumbers, align = alignvalues, part = "all")
  df <- df%>%theme_zebra_fs() %>% autofit()
  df<-color(df,color = "#04103b", part = "header")
  df<-color(df,color = "#162653", part = "body")
  df<-bg(df, bg = "white", part = "header")
  df<-hline(df, part = "header", border = std_border)
  #df<-padding(df, padding.top = 10, part = "header")
  df<-set_table_properties(df, layout = "autofit", width = customwidth)
  df<-df%>%fontsize(size = 9, part = "all")

  return(df)
}


flextableD<-function(df,customwidth=1)
{
  library(officer)
  library(flextable)
  #df %>% kbl() %>% kable_styling(bootstrap_options = c("striped", "hover"))
  std_border <- fp_border(color = "#dd0400")
  std_border2 <- fp_border(color = "#D1E2EC")
  df<-flextable(df) %>%theme_zebra(odd_header="transparent",even_header="transparent",odd_body ="#04103b",even_body="#162653") %>% autofit()
  df<-color(df,color = "#dd0400", part = "header")
  df<-color(df,color = "#D1E2EC", part = "body")
  #df<-bg(df, bg = "white", part = "header")
  #df<-bg(df, bg = "#04103b", part = "body")
  df<-hline(df, part = "header", border = std_border)
  df<-hline(df, part = "body", border = std_border2)
  #df<-padding(df, padding.top = 10, part = "header")
  df<-set_table_properties(df, layout = "autofit", width = customwidth)
  df<-df%>%fontsize(size = 9, part = "all")
  return(df)
}


ggBar2<-function (da, chart_title = "Portfolio Allocation", chart_subtitle = "Optional", horizontal=T,
                  chart_height = 400, chart_font_size = 11, chart_export_width = 600,
                  chart_export_height = 450,col_aq2 = as.character(c("#04103b", "#D1E2EC","#dd0400", "#3b5171")), m = list(r = 0, l = 0, b = 0, t = 50, par = 4), plotly = F, title_pos = "center", perc = ".0%")
{

  library(dplyr)
  library(plotly)
  library(lubridate)
  library(dplyr)
  da <- as.data.frame(da)
  da <- da[, 1:3]
  names(da) <- c("group","valuex", "valuey")

  y_axis_caption <- ""

  #try(da <- da %>% dplyr::group_by(group,valuex) %>% dplyr::summarize(valuey = sum(as.numeric(valuey))),silent=T)
  da$valuex <- factor(da$valuex, levels = unique(da$valuex)[order(as.numeric(da$valuey), decreasing = F)])

  # Assign colors to each group dynamically
  da$color <- factor(da$group, levels = unique(da$group))
  color_mapping <- setNames(col_aq2, unique(da$group))
  if(plotly != T)
  {

    p<-ggplot(da, aes(x = valuex, y = valuey, fill = color)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = color_mapping)

    if(horizontal==T)
    {
      p<-p+coord_flip()  # Make the bar chart horizontal
      if(perc == ".0%")
      {
        p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
      }
    }else{
      if(perc == ".0%")
      {
        p <- p + scale_x_continuous(labels = scales::percent_format(accuracy = 1))
      }
    }


    p<-p+ theme_aq_black_default_font(base_size = 20) +
      labs(title = chart_title, subtitle = chart_subtitle,
           x = "") + labs(caption = "") + xlab("") + theme(plot.margin = margin(l = 5,
                                                                                r = 10, b = 5, t = 5)) + xlab("") + ylab("") + theme(legend.position = "bottom",
                                                                                                                                     legend.margin = margin(-20, -20, -20, -20), legend.box.margin = margin(15,
                                                                                                                                                                                                            0, 30, 0)) + guides(colour = guide_legend(nrow = 3)) +
      theme(panel.grid.major.x = element_blank()) + theme(panel.grid.major.x = element_blank()) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(legend.title = element_blank()) + guides(fill = guide_legend(nrow = 1,
                                                                         byrow = TRUE))
    if(title_pos == "left")
    {
      p <- p + theme(plot.caption = element_text(hjust = 0.2),
                     plot.title.position = "plot", plot.caption.position = "plot")
    }

  }
  return(p)
}


PLradaR<-function(categories = c("Speed", "Power", "Accuracy", "Endurance", "Agility"),series1 = c(80, 60, 90, 70, 85),series2 =c(65, 75, 80, 60, 70),selected1,selected2)
{




  # Define categories (5 points)
  #categories <- c("Speed", "Power", "Accuracy", "Endurance", "Agility")

  # First series (loop closed by repeating first value)
  #series1 <- c(80, 60, 90, 70, 85)
  series1 <- c(series1, series1[1])
  categories_closed <- c(categories, categories[1])

  # Optional second series (if NULL, no overlay)
  #series2 <- c(65, 75, 80, 60, 70)  # <-- Replace with NULL to hide

  # Create radar chart
  fig <- plot_ly(
    type = 'scatterpolar',
    r = series1,
    theta = categories_closed,
    fill = 'toself',
    name = selected1,
    line = list(color = '#99CCFF')
  )

  # Add second series if it exists
  if (!is.null(series2)) {
    series2 <- c(series2, series2[1])  # Close the loop
    fig <- fig %>% add_trace(
      r = series2,
      theta = categories_closed,
      fill = 'toself',
      name = selected2,
      line = list(color = '#dd0400')
    )
  }

  # Layout settings
  fig <- fig %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(0, 100)
      )
    ),
    showlegend = TRUE,
    legend = list(
      orientation = 'h',
      x = 0.5,
      xanchor = 'center',
      y = -0.1  # Adjust this if it cuts off in your output
    )
  )

  return(fig)


}


chatgpt_query <- function(prompt, model = "gpt-3.5-turbo",api_key="") {
  library(httr)
  library(jsonlite)

  gpt_response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = toJSON(list(
      model = model,
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.7
    ), auto_unbox = TRUE)
  )

  gpt_content <- content(gpt_response, as = "parsed", type = "application/json")
  try(print(gpt_content$error[[1]]),silent=T)
  try(detach("package:plotly", unload=TRUE),silent=T)
  try(detach("package:httr", unload=TRUE),silent=T)
  try(detach("package:jsonlite", unload=TRUE),silent=T)
  library(plotly)
  # Return the assistant's reply
  gpt_content$choices[[1]]$message$content
}



advanced_chat_gpt_query<-function(user_query, model = "gpt-3.5-turbo")
{


  library(httr)
  library(jsonlite)

  # 1. Set your API keys
  openai_api_key <- ""        # Replace with your OpenAI key
  serpapi_key <- "" # Replace with your SerpAPI key

  # 2. Function to search the web using SerpAPI
  search_web <- function(query) {
    res <- GET("https://serpapi.com/search",
               query = list(q = query,
                            api_key = serpapi_key,
                            engine = "google"))
    data <- content(res, "parsed")

    results <- data$organic_results
    summaries <- sapply(results[1:20], function(r) {
      paste0("- ", r$title, ": ", r$snippet)
    })

    paste(summaries, collapse = "\n")
  }

  # 3. Function to query OpenAI GPT-4-turbo with context
  query_openai <- function(prompt, context) {
    full_prompt <- paste0(
      "You are a smart assistant. Use the context below to answer the user's query.\n\n",
      "=== Context ===\n", context, "\n\n",
      "=== Query ===\n", prompt, "\n\n",
      "Answer:"
    )

    res <- POST("https://api.openai.com/v1/chat/completions",
                add_headers(Authorization = paste("Bearer", openai_api_key)),
                content_type_json(),
                encode = "json",
                timeout(180),
                body = toJSON(list(
                  model = model,
                  messages = list(
                    list(role = "system", content = "You are a helpful assistant."),
                    list(role = "user", content = full_prompt)
                  ),
                  temperature = 0.4,
                  max_tokens = 2000
                ), auto_unbox = TRUE))

    answer <- content(res, as = "parsed")
    answer$choices[[1]]$message$content
  }

  # 4. Use the functions together
  #user_query <- "What are the current business challenges facing Estée Lauder?"

  web_context <- search_web(user_query)
  gpt_response <- query_openai(user_query, web_context)

  try(print(gpt_content$error[[1]]),silent=T)
  try(detach("package:plotly", unload=TRUE),silent=T)
  try(detach("package:httr", unload=TRUE),silent=T)
  try(detach("package:jsonlite", unload=TRUE),silent=T)
  library(plotly)

  return(gpt_response)

}


plytableR<-function (dw, ts_format = "returns", rounding = 2, header_color = "#3b5171",
                     header_font = "white", font_color = "#04103b", export_format = "svg",
                     chart_export_width = 800, chart_export_height = 400, print_output = T,
                     rowOddColor = "white", rowEvenColor = "lightgrey", fontsize = 10,
                     cellsheight = 20, orders = c("Year", "Amadeus Very Defensive USD",
                                                  "Amadeus Defensive USD", "Amadeus Balanced USD", "Amadeus Dynamic USD",
                                                  "Amadeus Very Dynamic USD"))
{


  #dw<-display_ext


  fig <- plot_ly(type = "table", columnwidth = c(150, rep(80,
                                                          12)), header = list(values = paste0("<b>", names(dw),
                                                                                              "</b>"), line = list(color = "white"), fill = list(color = header_color),
                                                                              font = list(color = header_font, size = fontsize + 1)),
                 cells = list(height = cellsheight, values = t(dw), line = list(color = "white"),
                              fill = list(color = list(rep(c(rowOddColor, rowEvenColor),
                                                           length(dw[1,])/2 + 1))), align = c("left",
                                                                                              "center", "center", "center", "center", "center",
                                                                                              "center", "center"), font = list(color = c(font_color),
                                                                                                                               size = fontsize)))
  m <- list(r = 0, b = 0, t = 0, l = 0, par = 4)
  fig <- fig %>% layout(margin = m)
  fig <- fig %>% config(toImageButtonOptions = list(format = export_format,
                                                    filename = "annual_returns_table", width = chart_export_width,
                                                    height = chart_export_height))
  fig_list <- list(fig = fig)

  fig
}


allocTreeBold<-function (
    df,
    parent_label = "Portfolio",
    chart_export_width = 600,
    chart_export_height = 450,
    fontsize=26,
    fontsizeheader=28,
    m = list(l = 0, r = 0, b = 0, t = 0, pad = 4),
    col_aq2 = as.character(c("#04103b", "#5777a7", "#D1E2EC", "#dd0400"))
)
{
  library(scales)
  library(plotly)
  library(dplyr)

  names(df) <- c("assets", "weights")
  df <- df %>%
    group_by(assets) %>%
    summarize(weights = sum(weights), .groups = 'drop')

  df$weights <- df$weights / sum(df$weights)
  wmp <- df
  wmp$Parent_Label <- parent_label

  regions <- wmp %>%
    group_by(assets) %>%
    summarize(sum = sum(weights, na.rm = TRUE), .groups = 'drop')

  regions$perc <- regions$sum / sum(regions$sum)
  regions$Parent_Label <- parent_label

  # Add root node
  regions <- rbind(
    regions,
    data.frame(
      assets = parent_label,
      sum = sum(regions$sum),
      perc = 1,
      Parent_Label = ""
    )
  )

  regions <- as.data.frame(regions, stringsAsFactors = FALSE)
  regions$perc <- as.numeric(regions$perc)

  cols <- colorRampPalette(col_aq2)(length(unique(regions$assets)) - 1)

  tree <- plot_ly(
    height = 250,
    type = "treemap",
    labels = regions$assets,
    parents = regions$Parent_Label,
    values = as.numeric(regions$sum),
    marker = list(colors = c(cols, "#020b2b")),
    hovertemplate = paste(
      regions$assets, "<br>",
      round(as.numeric(regions$sum) * 100, 2), "%",
      "<extra></extra>"
    ),
    branchvalues = "total",
    text = paste0(round(regions$perc * 100, 1), "% <br>"),
    outsidetextfont = list(
      size = fontsizeheader,
      color = "white",
      family = "Arial Black"
    ),
    textfont = list(
      size = fontsize,
      color = "white",
      family = "Arial Black"
    )
  ) %>% layout(margin = m)

  tree <- tree %>% config(toImageButtonOptions = list(
    format = "svg",
    filename = "allocation_tree",
    width = chart_export_width,
    height = chart_export_height
  ))

  return(tree)
}



msci_translate<-function(average_score)
{
  average_score_l<-
    ifelse(average_score < 1.428, "CCC",
           ifelse(average_score<2.857,"B",
                  ifelse(average_score<4.286,"BB",
                         ifelse(average_score<5.714,"BBB",
                                ifelse(average_score<7.143,"A",
                                       ifelse(average_score<8.571,"AA","AAA"
                                       ))))))
  return(average_score_l)
}


msci_translate_qtr<-
  function (average_score)
  {
    average_score_l <- ifelse(average_score < 25, "4th Quartile", ifelse(average_score <
                                                                           50, "3rd Quartile", ifelse(average_score < 75, "2nd Quartile", ifelse(average_score <
                                                                                                                                                   100, "1st Quartile","Not Rated"))))
    return(average_score_l)
  }


msci_esg_donut<-function(df,chart_title="Positions by Aggregate MSCI ESG Rating",flex_height=250,chart_font_size=12)
{
  msci_translate<-function(average_score)
  {
    average_score_l<-
      ifelse(average_score < 1.428, "CCC",
             ifelse(average_score<2.857,"B",
                    ifelse(average_score<4.286,"BB",
                           ifelse(average_score<5.714,"BBB",
                                  ifelse(average_score<7.143,"A",
                                         ifelse(average_score<8.571,"AA","AAA"
                                         ))))))
    return(average_score_l)
  }

  msci_translate_qtr<-
    function (average_score)
    {
      average_score_l <- ifelse(average_score < 25, "4th Quartile", ifelse(average_score <
                                                                             50, "3rd Quartile", ifelse(average_score < 75, "2nd Quartile", ifelse(average_score <
                                                                                                                                                     100, "1st Quartile","Not Rated"))))
      return(average_score_l)
    }

  ss<-df[,c("isin_or_account","perc_of_portfolio","AssetClass","SubClass")]
  ss$ISIN<-ss$isin_or_account
  ss$WeightEFA<-ss$perc_of_portfolio
  all_isins<-paste0(df$isin_or_account,collapse="','")



  df0<-dbGetQuery(pool,paste0("select * from esg.FundRatings where FUND_ISIN in ('",all_isins,"')"))
  df1<-dbGetQuery(pool,paste0("select * from esg.FundRatings_ClimateChange where FUND_ISIN in ('",all_isins,"')"))
  df2<-dbGetQuery(pool,paste0("select * from esg.FundRatings_EUSustainableFinance where FUND_ISIN in ('",all_isins,"')"))
  df0a<-merge(df0,merge(as.data.table(df1)[,-c("FUND_NAME","FUND_CUSIP","FUND_TICKER","FUND_ID")],as.data.table(df2)[,-c("FUND_NAME","FUND_CUSIP","FUND_TICKER","FUND_ID")],by="FUND_ISIN"),by=c("FUND_ISIN"),all=T)
  pfrating<-merge(ss,df0a,by.x="ISIN",by.y="FUND_ISIN",all.x=T)
  pfrating<-pfrating[(pfrating$ISIN)!="-1" & pfrating$AssetClass!="LIQUIDITY",]



  #library(peRformance)
  #library(plotly)


  #Average Score
  score<-sum(pfrating$WeightEFA*pfrating$FUND_ESG_QUALITY_SCORE,na.rm=T)/sum(pfrating$WeightEFA[!is.na(pfrating$FUND_ESG_QUALITY_SCORE)],na.rm=T)
  rating<-msci_translate(as.numeric(score))



  fig0<-
    gaugeR(value=score,chart_export_width_uploader=300,min=0,max=10,chart_export_height_uploader=200,text=paste0("Average Score: ",rating))
  #orca(fig,file="avg_score.svg",width=380,height=280)


  #Percentile Peers
  score<-sum(pfrating$WeightEFA*pfrating$FUND_ESG_QUALITY_SCORE_PCTL_PEER,na.rm=T)/sum(pfrating$WeightEFA[!is.na(pfrating$FUND_ESG_QUALITY_SCORE_PCTL_PEER)],na.rm=T)
  fig1<-
    gaugeR(value=score,chart_export_width_uploader=300,min=0,max=100,chart_export_height_uploader=200,text="Percentile Peers")
  #orca(fig,file="perc_peers.svg",width=380,height=280)

  #Percentile Universe
  score<-sum(pfrating$WeightEFA*pfrating$FUND_ESG_QUALITY_SCORE_PCTL_GLOBAL,na.rm=T)/sum(pfrating$WeightEFA[!is.na(pfrating$FUND_ESG_QUALITY_SCORE_PCTL_PEER)],na.rm=T)
  fig2<-
    gaugeR(value=score,chart_export_width_uploader=300,min=0,max=100,chart_export_height_uploader=200,text="Percentile Global")
  #orca(fig,file="perc_universe.svg",width=380,height=280)

  #Coverage Look-through
  pfrating$FUND_ESG_COVERAGE_OVERALL<-ifelse(is.na(pfrating$FUND_ESG_COVERAGE_OVERALL),0,pfrating$FUND_ESG_COVERAGE_OVERALL)
  score<-sum(pfrating$WeightEFA*pfrating$FUND_ESG_COVERAGE_OVERALL,na.rm=T)
  fig3<-
    gaugeR(value=score,chart_export_width_uploader=300,min=0,max=10,chart_export_height_uploader=200,text="Position Coverage %")
  #orca(fig,file="coverage.svg",width=380,height=280)

  #sel_statistic<-"FUND_ESG_QUALITY_SCORE_PCTL_PEER"
  divide<-1
  #library(dplyr)
  dst<-pfrating
  dst$Perc<-dst$perc_of_portfolio
  names(dst)[names(dst)=="FUND_ESG_QUALITY_SCORE"]<-"FUND_ESG_QUALITY_SCORE"
  dst$Rating<-msci_translate(as.numeric(dst$FUND_ESG_QUALITY_SCORE)/divide)

  score_selected<-sum(dst$WeightEFA*dst$FUND_ESG_QUALITY_SCORE,na.rm=T)/sum(dst$WeightEFA[!is.na(dst$FUND_ESG_QUALITY_SCORE)],na.rm=T)
  average_score_l<-round(score_selected,1)

  pie_cols<-as.character(c(colorRampPalette(as.character(c("#04103b","#3b5171","#dd0400")))(7),"#D9D9D9"))
  dsta <- dst%>% group_by(Rating  ) %>% summarize(count = sum(Perc) )
  dsta$Rating<-ifelse(is.na(dsta$Rating),"Not Rated",dsta$Rating)
  dsta$count<-ifelse(is.na(dsta$count),1-sum(dsta$count,na.rm=T),dsta$count)
  mdst<-data.frame(Rating=c("AAA","AA","A","BBB","BB","B","CCC","Not Rated"),
                   color=c(pie_cols))
  mdsta<-merge(mdst,dsta,by="Rating",all.x=T)
  mdsta<-mdsta[match(mdst$Rating, rev(mdsta$Rating)), ]
  mdsta$Rating <- factor(mdsta$Rating, levels = mdsta$Rating)
  plot_ly(mdsta, values = ~factor(count), labels = ~(Rating), marker = list(colors = mdsta$color), type = 'pie',sort=F)

  m <- list(l = 0,r = 0,b = 10,t = 100,pad = 4)
  donut<-
    mdsta %>% plot_ly(labels = ~Rating, values = ~count, sort = FALSE, marker = list(colors = mdsta$color), rotation = 240,textfont = list(family = "Arial Black", size = 14)) %>%
    add_pie(hole = 0.6) %>%
    layout(margin=m,title = chart_title,  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           annotations=list(text=average_score_l, "showarrow"=F, font=list(size = 50)),
           legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.15)
    )


  #hms<-hms[order(hms$Wgt),]
  y_axis_caption<-""
  mdsta$Rating <- factor(mdsta$Rating, levels = c("AAA","AA","A","BBB","BB","B","CCC","Not Rated"))
  mdsta$count<-ifelse(is.na(mdsta$count),0,mdsta$count)
  m<-list(b=50,t=0,r=0,l=0,par=4)
  rating_distribution_plot <- plot_ly(mdsta, x =mdsta$Rating, y =mdsta$count ,height=flex_height, type = 'bar', name = 'Gross',marker = list(color = col_aq2[1]),
                                      text = paste0(round(mdsta$count*100,1),"%"), textposition = 'inside',textfont =list(color=toRGB(col_aq[2])))
  rating_distribution_plot <- rating_distribution_plot %>% layout(margin = m,font=list(size=chart_font_size),title=chart_title,yaxis=list(tickformat =".0%"), xaxis = list(title=y_axis_caption), barmode = 'group')
  rating_distribution_plot

  library(dplyr)
  dst<-pfrating
  dst$Perc<-dst$perc_of_portfolio
  names(dst)[names(dst)=="FUND_ESG_QUALITY_SCORE_PCTL_PEER"]<-"FUND_ESG_QUALITY_SCORE_PCTL_PEER"
  dst$Rating<-msci_translate_qtr(as.numeric(dst$FUND_ESG_QUALITY_SCORE_PCTL_PEER)/divide)

  score_selected<-sum(dst$WeightEFA*dst$FUND_ESG_QUALITY_SCORE_PCTL_PEER,na.rm=T)/sum(dst$WeightEFA[!is.na(dst$FUND_ESG_QUALITY_SCORE_PCTL_PEER)],na.rm=T)
  average_score_l<-round(score_selected,1)


  pie_cols<-as.character(c(colorRampPalette(as.character(c("#04103b","#3b5171","#dd0400")))(4),"#D9D9D9"))
  dsta <- dst%>% group_by(Rating  ) %>% summarize(count = sum(Perc) )
  dsta$Rating<-ifelse(is.na(dsta$Rating),"Not Rated",dsta$Rating)
  dsta$count<-ifelse(is.na(dsta$count),1-sum(dsta$count,na.rm=T),dsta$count)
  mdst<-data.frame(Rating=c("1st Quartile","2nd Quartile","3rd Quartile","4th Quartile","Not Rated"),
                   color=c(pie_cols))
  mdsta<-merge(mdst,dsta,by="Rating",all.x=T)
  mdsta<-mdsta[match(mdst$Rating, rev(mdsta$Rating)), ]
  mdsta$Rating <- factor(mdsta$Rating, levels = mdsta$Rating)
  plot_ly(mdsta, values = ~factor(count), labels = ~(Rating), marker = list(colors = mdsta$color), type = 'pie',sort=F)

  m <- list(l = 0,r = 0,b = 10,t = 100,pad = 4)
  donut_peers<-
    mdsta %>% plot_ly(labels = ~Rating, values = ~count, sort = FALSE, marker = list(colors = mdsta$color), rotation = 240,textfont = list(family = "Arial Black", size = 14)) %>%
    add_pie(hole = 0.6) %>%
    layout(margin=m,title = chart_title,  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           annotations=list(text=average_score_l, "showarrow"=F, font=list(size = 50)),
           legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.15)
    )






  rlist=list("donut"=donut,"donut_peers"=donut_peers,"avg_score"=score,"fig0"=fig0,"fig1"=fig1,"fig2"=fig2,"fig3"=fig3,"rating_distribution_plot"=rating_distribution_plot)

  return(rlist)

}

fxtRanslate<-function(dfe,fx_rates,base_currency="CHF",instrument_currency="USD")
{
  #Currency translation
  if(base_currency=="EUR")
  {
    eurref<-fx_rates[,c("Dates",paste0("EUR"))]
  }else{
    eurref<-fx_rates[,c("Dates",paste0("EUR",base_currency))]
  }
  names(eurref)<-c("dates","EURREF")
  eurusd<-fx_rates[,c("Dates",paste0("EUR",instrument_currency))]
  names(eurusd)<-c("dates",paste0("EUR",instrument_currency))
  dfe <- Reduce(function(x, y) merge(x, y, by = "dates"), list(dfe, eurref, eurusd))
  dfe$pf_idx<-dfe$pf_idx/dfe$EURUSD*dfe$EURREF
  dfe$pf_ret<-dfe$pf_idx/lagpad(dfe$pf_idx,k=1)-1
  dfe$pf_ret[1]<-0
  return(dfe)
}
fxTranslateIDs <- function(dfe, fx_rates, base_currency) {
  # Rename input columns to standard names
  names(dfe) <- c("ticker", "price", "instrument_currency")
  dfe$instrument_currency <- toupper(dfe$instrument_currency)

  # Check base_currency vector length
  if (length(base_currency) != nrow(dfe)) {
    stop("Length of base_currency must equal number of rows in dfe.")
  }

  # Preserve original order
  dfe$row_id <- seq_len(nrow(dfe))
  dfe$base_currency <- base_currency

  # --- Use the most recent FX rates ---
  latest_date <- max(fx_rates$Dates, na.rm = TRUE)
  fx_latest <- fx_rates[fx_rates$Dates == latest_date, , drop = FALSE]

  # Remove date column and convert to named numeric vector
  fx_vec <- as.list(fx_latest[1, -which(names(fx_latest) == "Dates")])

  # --- Helper function to convert one row ---
  convert_price <- function(price, inst_cur, base_cur, fx) {
    if (inst_cur == base_cur) return(price)

    # Handle EUR as base or instrument
    rate_base <- if (base_cur == "EUR") 1 else fx[[paste0("EUR", base_cur)]]
    rate_inst <- if (inst_cur == "EUR") 1 else fx[[paste0("EUR", inst_cur)]]

    if (is.null(rate_base) || is.null(rate_inst)) {
      warning(paste("Missing FX rate for", inst_cur, "or", base_cur))
      return(NA_real_)
    }

    price / rate_inst * rate_base
  }

  # --- Apply conversion row-wise (order preserved) ---
  dfe$price_translated <- mapply(
    convert_price,
    price = dfe$price,
    inst_cur = dfe$instrument_currency,
    base_cur = dfe$base_currency,
    MoreArgs = list(fx = fx_vec),
    SIMPLIFY = TRUE
  )

  # --- Ensure order is identical to input ---
  dfe <- dfe[order(dfe$row_id), ]

  # --- Return translated table ---
  dfe[, c("ticker", "price", "instrument_currency", "base_currency", "price_translated")]
}



allocBarHL <- function(da, chart_title = "Portfolio Allocation", chart_subtitle = "Optional",
                       chart_height = 400, chart_font_size = 11, chart_export_width = 600,
                       chart_export_height = 450, m = list(r = 0, l = 0, b = 0, t = 50, par = 4),
                       plotly = TRUE, title_pos = "center", perc = ".0%", highlight_assets = "Germany",
                       base_size = 20,col_aq2 = as.character(c("#04103b", "#dd0400", "#3b5171"))) {

  library(dplyr)
  library(plotly)
  library(lubridate)

  da <- as.data.frame(da)
  da <- da[, 1:2]
  names(da) <- c("assets", "weight")
  da$weight <- as.numeric(da$weight)



  y_axis_caption <- ""

  # Aggregate weights by asset
  da <- da %>% group_by(assets) %>% summarize(weight = sum(weight), .groups = "drop")

  # Order assets by weight
  da$assets <- factor(da$assets, levels = unique(da$assets)[order(da$weight, decreasing = FALSE)])

  # Define colors for Plotly bars
  da$color <- ifelse(da$assets == highlight_assets, col_aq2[2], col_aq2[1])

  if (plotly) {
    # Plotly version
    p <- plot_ly(
      da,
      x = as.numeric(da$weight),
      y = da$assets,
      height = chart_height,
      type = "bar",
      name = "Portfolio",
      marker = list(color = da$color)
    )

    p <- p %>% layout(
      margin = m,
      font = list(size = chart_font_size),
      title = chart_title,
      xaxis = list(title = y_axis_caption, tickformat = perc),
      yaxis = list(title = y_axis_caption),
      barmode = "group"
    )

    p <- p %>% config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "allocation_pie",
        width = chart_export_width,
        height = chart_export_height
      )
    )

  } else {
    # ggplot version
    library(ggplot2)

    p <- ggplot(da, aes(x = weight, y = assets,
                        fill = ifelse(assets == highlight_assets, "highlight", "normal"))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(normal = col_aq2[1], highlight = col_aq2[2])) +
      theme_minimal(base_size = base_size) +
      labs(title = chart_title, subtitle = chart_subtitle, x = "", y = "", caption = "") +
      theme(
        plot.margin = margin(l = 5, r = 10, b = 5, t = 5),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )

    if (title_pos == "left") {
      p <- p + theme(plot.title.position = "plot", plot.caption.position = "plot")
    }

    if (perc == ".0%") {
      p <- p + scale_x_continuous(labels = scales::percent_format(accuracy = 1))
    }
  }

  return(p)
}





plotly_line <- function(df, title = "Title", subtitle = "Subtitle",
                        xcap = "", ycap = "", name1 = "Portfolio",
                        perc = TRUE,
                        col_aq2 = c("#04103b", "#dd0400", "#5777a7", "#D1E2EC"),
                        base_size = 24,dateformat="%m/%Y") {
  library(plotly)

  names(df) <- c("date", "asset")
  df$date <- as.Date(df$date)

  # Format y-axis if perc = TRUE
  y_format <- if (perc) {
    list(tickformat = ".2%")  # percentage with 2 decimals
  } else {
    list(tickformat = ".2f")  # numeric with 2 decimals
  }

  # Build plotly object
  p <- plot_ly(
    data = df,
    x = ~date,
    y = ~asset,
    type = "scatter",
    mode = "lines",
    name = name1,
    line = list(color = col_aq2[1], width = 2)
  ) %>%
    layout(
      title = list(
        text = paste0(title)
        #font = list(size = base_size)
      ),
      xaxis = list(title = xcap, tickformat = dateformat),
      yaxis = c(list(title = ycap), y_format),
      showlegend = FALSE,
      margin = list(l = 50, r = 50, b = 50, t = 50, pad = 5)
    )

  return(p)
}








performance_attribution_plotly<-function(pandldaily,amc=NULL,chart_title,base_cols = c("#04103b", "#dd0400", "#5777a7", "#D1E2EC"))
{


  library(plotly)
  library(dplyr)
  library(tidyr)

  # ensure dates are Date
  pandldaily <- pandldaily %>% mutate(as_per = as.Date(as_per, format = "%d.%m.%Y"))
  if(!is.null(amc))
  {
    try(amc$Date <- as.Date(amc$Date),silent=T)
    try(amc_index <- indexR(amc[, c("Date", "NAV")], normalization = "index-1"),silent=T)
  }


  # ungroup first (important!)
  pandldaily <- pandldaily %>% ungroup()

  # create complete grid
  unique_dates <- sort(unique(pandldaily$as_per))

  pandldaily_complete <- pandldaily %>%
    tidyr::complete(
      as_per = unique_dates,
      strategy = unique(strategy),  # explicit
      fill = list(cumperc = 0)
    ) %>%
    arrange(strategy, as_per)

  # order strategies (optional): by total absolute cumperc so stacking order is stable
  strategy_order <- pandldaily_complete %>%
    group_by(strategy) %>%
    summarize(total_abs = sum(abs(cumperc), na.rm = TRUE)) %>%
    arrange(desc(total_abs)) %>%
    pull(strategy)

  # build a palette (you can replace with your own vector)
  nstrat <- length(strategy_order)

  cols <- if (nstrat <= length(base_cols)) {
    base_cols[1:nstrat]
  } else {
    colorRampPalette(base_cols)(nstrat)
  }
  color_map <- setNames(cols, strategy_order)

  # START building fig
  fig <- plot_ly()

  for (s in strategy_order) {
    dpos <- pandldaily_complete %>% filter(strategy == s, cumperc >= 0)
    dneg <- pandldaily_complete %>% filter(strategy == s, cumperc < 0)
    col <- color_map[[s]]
    fill_col <- grDevices::adjustcolor(col, alpha.f = 0.6)  # semi-transparent fill

    # positive trace (show legend here if exists)
    if (nrow(dpos) > 0) {
      fig <- fig %>%
        add_trace(
          data = dpos,
          x = ~as_per,
          y = ~cumperc,
          type = "scatter",
          mode = "none",
          stackgroup = "positive",    # all positives stack together
          name = s,
          legendgroup = s,
          showlegend = TRUE,
          fillcolor = fill_col,
          line = list(color = col, width = 0.5)
        )
    }

    # negative trace (hide legend if positive shown; otherwise show)
    if (nrow(dneg) > 0) {
      showleg <- ifelse(nrow(dpos) > 0, FALSE, TRUE)
      fig <- fig %>%
        add_trace(
          data = dneg,
          x = ~as_per,
          y = ~cumperc,
          type = "scatter",
          mode = "none",
          stackgroup = "negative",    # negatives stack downward
          name = s,
          legendgroup = s,
          showlegend = showleg,
          fillcolor = fill_col,
          line = list(color = col, width = 0.5)
        )
    }
  }

  if(!is.null(amc))
  {
    # add NAV line on top
    fig <- fig %>%
      add_trace(
        data = amc_index,
        x = ~Date,
        y = ~NAV,
        type = "scatter",
        mode = "lines",
        line = list(color = "black", width = 2),
        name = "Amadeus Decorrelated Strategies",
        inherit = FALSE
      )
  }
  fig<-fig%>%
    layout(
      title = list(text = chart_title),
      xaxis = list(title = ""),
      yaxis = list(
        title = "",
        tickformat = ".2%"  # percentage with 2 decimals
      ),
      legend = list(orientation = "h",h=0.5,y=-0.2)
    )
  return(fig)

}


allocBarhDendro <- function(da, chart_title = "Portfolio Allocation", chart_height = 600,
                            chart_font_size = 12, title_font_size = 16, bargap = 0.2,
                            barcol = "#5777a7", barborder = "#04103b",
                            line_color = "#5777a7", line_width = 0.5, connector_space = 0.05,
                            category_font_color = "#5777a7", min_label_spacing = 0.03) {
  library(dplyr)
  library(plotly)

  da <- da[, 1:3]
  names(da) <- c("asset", "weight", "strategy")
  da$weight <- as.numeric(da$weight)

  # Order descending by weight
  da <- da %>% arrange(desc(weight))
  da$asset_factor <- factor(da$asset, levels = da$asset)

  # Compute range for proportional spacing
  range_height <- max(da$weight) - min(da$weight)

  # Base bar chart (without text labels)
  p <- plot_ly() %>% add_bars(
    data = da,
    x = ~asset_factor,
    y = ~weight,
    marker = list(color = barcol, line = list(color = barborder, width = 0.5)),
    name = "Portfolio"
  )

  strategies <- unique(da$strategy)
  label_positions <- c()  # Track previous label y-positions

  for (strat in strategies) {
    da_strat <- da %>% filter(strategy == strat)
    if (nrow(da_strat) < 1) next

    x_start <- da_strat$asset_factor[1]
    x_end <- da_strat$asset_factor[nrow(da_strat)]

    # Initial connector height based on tallest bar
    y_connector <- max(da_strat$weight, 0) + connector_space * range_height

    # Strategy label position (bumped for min spacing)
    y_label <- y_connector + connector_space * range_height * 0.8
    if (length(label_positions) > 0) {
      for (prev_y in label_positions) {
        while (abs(y_label - prev_y) < min_label_spacing * range_height) {
          y_label <- y_label + min_label_spacing * range_height
        }
      }
    }

    # Horizontal connector line just below the label
    y_horiz <- y_label - connector_space * range_height / 8
    p <- p %>% add_segments(
      x = x_start, xend = x_end,
      y = y_horiz, yend = y_horiz,
      line = list(color = line_color, width = line_width),
      showlegend = FALSE
    )

    # Vertical lines from bar top (or 0) up to horizontal connector
    for (x_vert in c(x_start, x_end)) {
      y_val <- da_strat$weight[which(da_strat$asset_factor == x_vert)]
      y_bottom <- ifelse(y_val > 0, y_val, 0)
      p <- p %>% add_segments(
        x = x_vert, xend = x_vert,
        y = y_bottom, yend = y_horiz,
        line = list(color = line_color, width = line_width),
        showlegend = FALSE
      )
    }

    # Add strategy label
    mid_index <- ceiling(nrow(da_strat) / 2)
    x_center <- da_strat$asset_factor[mid_index]
    p <- p %>% add_text(
      x = x_center,
      y = y_label,
      text = strat,
      textposition = "top center",
      showlegend = FALSE,
      textfont = list(color = category_font_color)
    )

    # Store label position
    label_positions <- c(label_positions, y_label)
  }

  # Add numeric value annotations with white semi-transparent background (no border)
  for (i in 1:nrow(da)) {
    p <- p %>% add_annotations(
      x = da$asset_factor[i],
      y = da$weight[i],
      text = paste0(round(da$weight[i] * 100, 2), "%"),
      xanchor = "center",
      yanchor = ifelse(da$weight[i] >= 0, "bottom", "top"),
      showarrow = FALSE,
      font = list(size = chart_font_size),
      bgcolor = "rgba(255,255,255,0.7)",  # white, 70% opacity
      bordercolor = "rgba(0,0,0,0)",      # fully transparent border
      borderpad = 2
    )
  }

  # Layout
  p <- p %>% layout(
    title = list(text = chart_title, font = list(size = title_font_size)),
    xaxis = list(
      title = "",
      tickangle = -45,
      categoryorder = "array",
      categoryarray = da$asset_factor
    ),
    yaxis = list(
      title = "",
      showticklabels = TRUE,
      tickformat = ".2%",
      showgrid = FALSE   # remove horizontal gridlines
    ),
    barmode = "overlay",
    bargap = bargap,
    font = list(size = chart_font_size),
    height = chart_height,
    plot_bgcolor = "white"
  )

  return(p)
}
