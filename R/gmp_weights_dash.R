
library(pool)
#library(RMySQL)

#pool1 <<- dbPool(drv = RMySQL::MySQL(),dbname = "gaa",host = "172.40.0.17",username = "root",password = "AmCap2019*",port=3306)
#source("P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/RStudio/ggplot_functions.R")

get_current_mkt_cap_share_pie<-function(path="P:/NEWTREE/Amadeus/Clients/Ulysses/MEETING/WORLD MARKET CAP AND NEW ETF SCREENER.xlsx",
                                        plotly_output="T")
{

  
  
  #library(Rblpapi)
  #library(readxl)
  library(data.table)
  library(dplyr)
  
  #col_w_light<-function(number_cols)
  #{
  #  col_aq2<-as.character(c("#04103b","#5777a7","#969696","#dd0400"))
  #  cols = colorRampPalette(col_aq2)(number_cols)
  #}
  
  
  #wmp <- read_excel("P:/NEWTREE/Amadeus/Clients/Ulysses/MEETING/WORLD MARKET CAP AND NEW ETF SCREENER.xlsx", sheet = "World Market Cap Static", skip = 1)
  
  #tickers<-wmp$REGION[!is.na(wmp$REGION) & wmp$REGION != 'WCAUWRLD Index' & wmp$REGION !="WCAUZIMB Index" & wmp$REGION!="WCAUVENE Index"]
  
  #df<-bdp(tickers,"px_last")
  #df$id<-row.names(df)
  
  
  wmp<-dbGetQuery(pool1,"select * from gaa.gmp_weights")

  wmp$Parent_Label<-"World"
  
  wmp$Region<-ifelse(wmp$region_gmp %in% c("Central Asia","East Asia","South Asia","South East Asia","Pacific"),"Asia & Pacific",wmp$region_gmp)
  wmp$Region<-ifelse(wmp$region_gmp %in% c("Africa","South America","Middle East"),"Others",wmp$region_gmp)
  
  
  regions<-wmp%>%group_by(Region)%>%summarize(sum= sum(px_last,na.rm=T))
  

  # Barplot
  if(plotly_output=="T")
  {
    library(plotly)
    
    regions$perc<-regions$sum/sum(regions$sum)
    regions$Parent_Label<-"World"
    regions<-rbind(regions,c("World",sum(regions$sum),1,""))
    regions<-as.data.frame(regions,stringsAsFactors=F)
    regions$perc<-as.numeric(regions$perc)
    
    m<-list(r=0,l=0,b=0,t=0,par=4)
    
    pie <- plot_ly(height=250,
      type="treemap",
      labels=regions$Region,
      parents=regions$Parent_Label,
      values= as.numeric(regions$perc),
      marker=list(colors=col_w_light(length(unique(regions$Region)))),
      #width=1100,
      #height=600,
      #hovertemplate  = paste(gp$Child_Label, "<br>", gp$IssueValue,"%"),
      hovertemplate = paste(regions$Region,"<br>",
                            as.numeric(regions$perc)*100,"%",
                            '<extra></extra>'),
      branchvalues="total",
      outsidetextfont=list(size=22, color= "white"),
      insidetextfont=list(size=14),
      #textfont=list(size=22),
      text=paste0("",round(regions$perc*100,1),"% <br>")
    )%>%
      layout(margin=m)
    pie

  }else{
    
    library(ggplot2)
    
    bp<- ggplot(regions, aes(x="", y=sum, fill=Region))+
      geom_bar(width = 1, stat = "identity")
    pie <- bp + coord_polar("y", start=0)
    pie <- pie + 
      scale_fill_manual(values=col_w_light(10))+
      theme_aq_black_pie()+
      ylab("")+
      xlab("")
  }  
  return(pie)
}

#get_current_mkt_cap_share_pie()


#Historical Development
#---------------------------------------------------------------------------------------------------------
get_hist_mkt_cap_share<-function(path="P:/NEWTREE/Amadeus/Clients/Ulysses/MEETING/WORLD MARKET CAP AND NEW ETF SCREENER.xlsx",plotly_output="T")
{
  #library(Rblpapi)
  library(readxl)
  library(data.table)
  library(dplyr)
  #blpConnect()
  #wmp <- read_excel(path, sheet = "World Market Cap Static", skip = 1)
  

  
  #col_w_light<-function(number_cols)
  #{
  #  col_aq2<-as.character(c("#04103b","#5777a7","#969696","#dd0400"))
  #  cols = colorRampPalette(col_aq2)(number_cols)
  #}
  
  #tickers<-wmp$REGION[!is.na(wmp$REGION) & wmp$REGION != 'WCAUWRLD Index' & wmp$REGION !="WCAUZIMB Index" & wmp$REGION!="WCAUVENE Index"]
  
  
  #hh<-bdh(tickers,"px_last",as.Date("2003-09-22"),NULL,include.non.trading.days = T)
  #hhl<-rbindlist(hh,id="id")
  
  #w<-bdh("WCAUWRLD Index","px_last",as.Date("2003-09-22"),NULL,include.non.trading.days = T)
  #hhl<-merge(hhl,w,by="date")
  
  hhl<-dbGetQuery(pool1,"select * from gaa.gmp_weights_history")
  
  #hhl<-hhl[order(id),]
  
  #hhl<-hhl %>% group_by(id) %>% mutate_at(vars(-group_cols()),funs(na.locf(., na.rm = FALSE)))
  #hhl<-merge(hhl,wmp[,c("REGION","Region")],by="id",by.y="REGION")
  #hhl$perc<-hhl$px_last.x/hhl$px_last.y

  
  hhl$Region<-ifelse(hhl$region_gmp %in% c("Central Asia","East Asia","South Asia","South East Asia","Pacific"),"Asia & Pacific",hhl$region_gmp)
  hhl$Region<-ifelse(hhl$region_gmp %in% c("Africa","South America","Middle East","NEW ZEALAND"),"Others",hhl$region_gmp)


  hhr<-hhl %>% group_by(Region,date)%>%summarize(sum=sum(perc,na.rm=T))
  
  condense=F
  if(condense==T)
  {
    hhr$Region<-ifelse(hhr$Region %in% c("East Asia","South Asia","Central Asia","South East Asia","Asia"),"Asia",hhr$Region)
    
    hhr$Region<-ifelse(hhr$Region %in% c("Pacific","Others"),"Rest of World",hhr$Region)
    hhr<-hhr %>% group_by(Region,date)%>%summarize(sum=sum(sum,na.rm=T))
    
    col_w_light<-function(number_cols)
    {
      col_aq2<-as.character(c("#04103b","#5777a7","#969696","#dd0400"))
      cols = colorRampPalette(col_aq2)(number_cols)
    }
    
    cols <- c("North America" = col_w_light(4)[1]
              ,"Europe" = col_w_light(4)[2]
              ,"Asia"=col_w_light(4)[3]
              ,"Rest of World"=col_w_light(4)[4])
    unique(hhr$Region)  
    
  }
  
  
  cols <- c("Africa" = col_w_light(10)[1]
            ,"Central Asia" = col_w_light(10)[2]
            ,"East Asia"=col_w_light(10)[3]
            ,"Europe"=col_w_light(10)[4]
            ,"Middle East"=col_w_light(10)[5]
            ,"North America"=col_w_light(10)[6]
            ,"Pacific"=col_w_light(10)[7]
            ,"South America"=col_w_light(10)[8]
            ,"South Asia"=col_w_light(10)[9]
            ,"South East Asia"=col_w_light(10)[10])
  
  #Correct Data Errors
  hhr$sum[hhr$Region=='Europe' & hhr$date=='2006-12-02']<-0.3193849


  hhr<-hhr%>%group_by(Region)%>%mutate(runmedian=runmed(sum,10))
  
  #hhr<-hhr[!is.na(hhr$runsum),]
  if(plotly_output=="T")
  {
    m <- list(l = 10,r = 0,b = 0,t = 5,pad = 4)
    p1<-
    plot_ly(hhr[hhr$Region!="Others",],x=~as.Date(date),y=~runmedian,name=~Region,color=~Region, colors=col_w_light(10),type="scatter",mode="line",height=250)%>%
      layout(margin = m,title="",xaxis = list(title=""), yaxis = list(title="",tickformat=".0%"),legend = list(font = list(size = 8),bgcolor = 'rgba(255,255,255,0)'))
    p1
    #orientation = "h",xanchor = "center",x = 0.5,y=-0.7, 
  }else{
    p1<-
    ggplot(data=hhr,aes(x=as.Date(date), y=sum,lu=Region,color=Region))+
      #add_rec_shade(as.Date(min(df$date)),as.Date(Sys.Date()))+
      geom_line(size=1)+
      scale_colour_manual(values = cols)+
      theme_aq_black(base_size=26)+
      #size 22 for overleaf
      labs(color='')+
      labs(title="The stunning decline of corporate Europe",subtitle="Share in worldwide market capitalization",x ="")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      labs(caption = 'Source: Bloomberg')+
      guides(colour = guide_legend(nrow = 2))+
      scale_x_date(labels = date_format("%m-%Y"))+
      theme(legend.position = c(x=0.4, y=0.95),legend.text=element_text(size=20))+
      ylab("")
    p1
      #ggsave(file="P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/iOSApp/MarlinMotion/ChartPosts/corporate_europe.png", plot=p1, width=15, height=10)

  }
  
  return(p1)
}



#get_hist_mkt_cap_share()