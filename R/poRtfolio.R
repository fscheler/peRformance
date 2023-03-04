


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





