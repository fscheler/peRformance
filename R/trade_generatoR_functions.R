

top_bets_subclasstgR<-function(df,rf,asset_class="BONDS",chart_height=300)
{

  if(asset_class=="GMP")
  {
    df$AssetClass<-ifelse(grepl("GMP",df$SubClass)==T,"GMP",df$AssetClass)
    rf$AssetClass<-ifelse(grepl("GMP",rf$SubClass)==T,"GMP",rf$AssetClass)
  }

  df<-df%>%group_by(AssetClass,SubClass)%>%summarise(perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),TradeSimulation=sum(TradeSimulation,na.rm=T))
  rf<-rf%>%group_by(AssetClass,SubClass)%>%summarise(Weight=sum(Weight,na.rm=T))
  dfs<-merge(df[,c("AssetClass","SubClass","perc_of_portfolio","TradeSimulation")],rf[,c("AssetClass","SubClass","Weight")],by=c("AssetClass","SubClass"),all=T)
  dfs$Weight<-ifelse(is.na(dfs$Weight),0,dfs$Weight)
  dfs$perc_of_portfolio<-ifelse(is.na(dfs$perc_of_portfolio),0,dfs$perc_of_portfolio)
  dfs$TradeSimulation<-ifelse(is.na(dfs$TradeSimulation),0,dfs$TradeSimulation)

  dfs<-dfs[dfs$AssetClass==asset_class & !is.na(dfs$AssetClass),]
  dfs$WeightAdjPre<-dfs$Weight*sum(dfs$perc_of_portfolio,na.rm=T)/sum(dfs$Weight,na.rm=T)
  dfs$WeightAdjPost<-dfs$Weight*sum(dfs$TradeSimulation,na.rm=T)/sum(dfs$Weight,na.rm=T)
  dfs$ActiveWeightPreTrade<-round(dfs$perc_of_portfolio-dfs$WeightAdjPre,6)
  dfs$ActiveWeightPostTrade<-round(dfs$TradeSimulation-dfs$WeightAdjPost,6)

  llong <- melt(setDT(dfs), id.vars = c("SubClass"), variable.name = c("variable"))
  llong<- llong[llong$variable%in% c("ActiveWeightPreTrade","ActiveWeightPostTrade"),]
  llong<-llong[!is.na(llong$value) & !is.na(llong$SubClass),]
  llong$value<-as.numeric(llong$value)
  #llong<-llong%>%group_by(SubClass,variable)%>%summarise(value=sum(value))
  llong$captions=llong$variable
  custom_colors <- c("ActiveWeightPreTrade" = "#04103b", "ActiveWeightPostTrade" = "#99CCFF")
  custom_colors<-c(rep("#04103b",nrow(llong)/2),rep("#99CCFF",nrow(llong)/2))

  # Define the group to sort by
  sort_group <- "ActiveWeightPostTrade"

  # Get the order of categories based on the sorting group
  sorted_categories <- llong %>%
    filter(variable == sort_group) %>%
    arrange(-desc(value)) %>%
    pull(SubClass)  # Extract sorted category names

  # Apply this ordering to all categories
  llong$SubClass <- factor(llong$SubClass, levels = sorted_categories)

  p<-
    plot_ly(llong,y=~SubClass,x=~value,text = ~paste0(round(value,4)*100,"%"),name=~captions, height = chart_height,type = "bar",marker = list(color = c(custom_colors)))%>%
    layout(showlegend = F,title=paste0("Active bets within ",asset_class),xaxis=list(title="",tickformat =".1%"),yaxis = list(title = ''))

  return(p)
}

liquidity_charttgR<-function(df,rf,chart_height=300)
{
  sum(df$trades,na.rm=T)
  sum(df$effective_trades,na.rm=T)
  sum(df$perc_of_portfolio,na.rm=T)


  sum(df$perc_of_portfolio[df$AssetClass!="LIQUIDITY"],na.rm=T)
  sum(df$TradeSimulation[df$AssetClass!="LIQUIDITY"],na.rm=T)

  df$effective_trades<-ifelse(is.na(df$trade_value_adj) | df$trade_value_adj==0,0,df$trades)
  cash_impact_trades<-df%>%group_by(CRNCY)%>%summarise(sumcashimpact=sum(effective_trades,na.rm=T))

  l<-df[df$AssetClass=="LIQUIDITY",]
  l<-l%>%group_by(CRNCY)%>%summarise(perc_of_portfolio=sum(perc_of_portfolio,na.rm=T))
  l<-merge(l,cash_impact_trades,by="CRNCY",all=T)
  l$CashSimulation<-l$perc_of_portfolio-l$sumcashimpact

  llong <- melt(setDT(l), id.vars = c("CRNCY"), variable.name = c("variable"))
  llong<- llong[!llong$variable%in%"sumcashimpact",]
  llong<-llong[!is.na(llong$value) & !is.na(llong$CRNCY),]
  llong<-llong[order(llong$CRNCY),]
  llong$captions=ifelse(llong$variable=="perc_of_portfolio","Current","Trade Simulation")
  custom_colors <- c("Current" = "#04103b", "Trade Simulation" = "#99CCFF")
  custom_colors<-c(rep("#04103b",nrow(llong)/2),rep("#99CCFF",nrow(llong)/2))

  #llong<<-llong
  p<-
    plot_ly(llong,x=~CRNCY,y=~value,text = ~paste0(round(value,4)*100,"%"),name=~captions, height = chart_height,type = "bar",marker = list(color = c(custom_colors)))%>%
    layout(showlegend = F,title="Cash impact",xaxis=list(title=""),yaxis = list(title = '',tickformat =".1%"))

  #p<-plot_ly(l,x=~CRNCY,y=~perc_of_portfolio,text = paste0(round(l$perc_of_portfolio,4)*100,"%"), height = chart_height,type = "bar",marker = list(color = '#04103b'))%>%
  #  add_trace(x=~CRNCY,y=~CashSimulation,marker = list(color = '#99CCFF'))%>%
  #  layout(showlegend = F,title="Cash impact",xaxis=list(title=""),yaxis = list(title = '',tickformat =".1%"))

  cash_impact_list<-list("p"=p,"llong"=llong)
  return(cash_impact_list)
}


clean_prepare_update_pricestgR<-function(df,rlist_rebalancing)
{

  sec<-dbGetQuery(pool,"select * from gaa.gaa_instrumentsdb")
  sec<-sec[duplicated(sec$ID_ISIN)==F ,]
  sec$LAST_PRICE<-as.numeric(sec$LAST_PRICE)

  cry<-dbGetQuery(pool,"select * from gaa.gaa_currenciesdb")
  cry$cprice<-as.numeric(cry$cprice)
  cry<-cry[!is.na(cry$cprice),]
  cry<-cry[duplicated(cry$cpair)==F,]

  df<-subset(df,select=c(-cprice))
  df$crytr<-paste0(rlist_rebalancing$df$portfolio_ref_ccy[1],df$CRNCY," Curncy")

  df<-merge(df,sec[,c("ID_ISIN","LAST_PRICE")],by.x="isin_or_account",by.y="ID_ISIN",all.x=T)
  df<-merge(df,cry[,c("cpair","cprice")],by.x="crytr",by.y="cpair",all.x=T)

  df$cprice<-ifelse(df$CRNCY==rlist_rebalancing$df$portfolio_ref_ccy[1],1,df$cprice)
  df$PriceLC<-df$LAST_PRICE
  df$pl_total_value_last_price<-df$quantity*df$LAST_PRICE/df$cprice
  df$pl_total_value_last_price<-ifelse(is.na(df$pl_total_value_last_price),df$current_net_total_ref_ccy,df$pl_total_value_last_price)

  df$perc_of_portfolio_md<-df$perc_of_portfolio/100
  df$perc_of_portfolio<-df$pl_total_value_last_price/sum(df$pl_total_value_last_price)

  df<-df[order(df$AssetClass,df$SubClass),]
  df$SubClass<-toupper(df$SubClass)

  return(df)
}


#Clean and Prepare Data
clean_preparetgR<-function(bm_name="CHF_D",pf_name,exclude="LIABILITIES",aam_file,holdings_data_as_of,use_hist_database="No",dominating_currency="Excel",bank_database="No")
{

  #Data excel
  #read_path<-"C:/Users/fsche/OneDrive - Amadeus Capital SA/Documents - Clients/Trade_Generator/"
  #rf <- read_excel(paste0(read_path,"AAM_GUI.xlsm"), sheet = bm_name, skip = 33)

  #Data database
  rf<-dbGetQuery(pool,paste0("select * from gaa.gaa_allocations where AAM_File = '",aam_file,"' and Portfolio = '",bm_name,"' and Date = (select max(Date) from gaa.gaa_allocations where Portfolio = '",bm_name,"' and AAM_File = '",aam_file,"')"))

  rf$SubClass<-toupper(rf$SubClass)
  rf$AssetClass<-toupper(rf$AssetClass)
  rf$AssetName<-toupper(rf$AssetName)
  rf<-rf[rf$Weight!=0,]

  sec<-dbGetQuery(pool,"select * from gaa.gaa_instrumentsdb")
  sec<-sec[duplicated(sec$ID)==F ,]
  sec$LAST_PRICE<-as.numeric(sec$LAST_PRICE)
  sec$ASSET_CLASS_PF_PRES<-toupper(sec$ASSET_CLASS_PF_PRES)


  cry<-dbGetQuery(pool,"select * from gaa.gaa_currenciesdb")
  cry$cprice<-as.numeric(cry$cprice)
  cry<-cry[!is.na(cry$cprice),]
  cry<-cry[duplicated(cry$cpair)==F,]


  #bank_database<-"Yes"

  dfa<-dbGetQuery(pool,paste0("select * from portfolio_presentation.",bank_database," where portfolio_name in ('",paste0(pf_name,collapse="','"),"')"))

  #if(use_hist_database=="No")
  #{
  #  if(bank_database=="No")
  #  {
  #    dfa<-dbGetQuery(pool,paste0("select * from portfolio_presentation.pms_positions where portfolio_name in ('",paste0(pf_name,collapse="','"),"')"))
  #  }else{
  #    dfa<-dbGetQuery(pool,paste0("select * from portfolio_presentation.bank_positions where portfolio_name in ('",paste0(pf_name,collapse="','"),"')") )
  #  }
  #}else{
  #  dfa<-dbGetQuery(pool,paste0("select * from portfolio_presentation.pms_positions_hist where file_production_date = '",holdings_data_as_of,"' and portfolio_name in ('",paste0(pf_name,collapse="','"),"')"))
  #}

  dfa$tree_rule_display_name_1<-toupper(dfa$tree_rule_display_name_1)
  dfa$Description<-toupper(dfa$Description)
  dfa$position_type<-toupper(dfa$position_type)

  dfa<-dfa[!dfa$tree_rule_display_name_1 %in% exclude,]

  names(rf)[names(rf)=="ISIN"]<-"ID_ISIN_CHECK"
  rf$Now<-rf$Weight

  rf<-rf[!is.na(rf$AssetClass),]
  rf<-rf[order(rf$Date,rf$AssetClass),]

  #if(bank_database=="No")
  #{
  #  df<-dfa[dfa$portfolio_name %in% pf_name,]
  #}else{
  #  df<-dfa
  #}
  df<-dfa

  #Take MyDesq or Sec File Currency?
  if(dominating_currency=="ISIN & Currency")
  {
    df<-merge(df,sec,by.x=c("isin_or_account","Market_ccy"),by.y=c("ID_ISIN","CRNCY"),all.x=T)
    df$CRNCY<-df$Market_ccy
  }else{
    sec<-sec[duplicated(sec$ID_ISIN)==F ,]
    df<-merge(df,sec,by.x="isin_or_account",by.y="ID_ISIN",all.x=T)
  }


  df$tree_rule_display_name_1<-ifelse((is.na(df$tree_rule_display_name_1) | toupper(df$tree_rule_display_name_1)=="UNKNOWN") & !is.na(df$ASSET_CLASS_PF_PRES), df$ASSET_CLASS_PF_PRES,df$tree_rule_display_name_1)
  df$Custom_Name<-ifelse(is.na(df$Custom_Name) & "custom_name_bank" %in% names(df), df$custom_name_bank,df$Custom_Name)

  df<-df[,c("portfolio_ref_ccy","isin_or_account","Symbol","position_type","tree_rule_display_name_1","Custom_Name","Description","Market_ccy","CRNCY","quantity","current_net_total_ref_ccy","current_price_market_ccy","LAST_PRICE","perc_of_portfolio")]
  names(df)<-c("portfolio_ref_ccy","isin_or_account","Symbol","position_type","AssetClass","SubClass","Asset","Market_ccy","CRNCY","quantity","current_net_total_ref_ccy","current_price_market_ccy","LAST_PRICE","perc_of_portfolio")
  df$CRNCY<-ifelse(is.na(df$CRNCY),df$Market_ccy,df$CRNCY)


  df$crytr<-paste0(df$portfolio_ref_ccy,df$CRNCY," Curncy")


  df<-merge(df,cry[,c("cpair","cprice")],by.x="crytr",by.y="cpair",all.x=T)

  df$pl_total_value_last_price<-df$quantity*df$LAST_PRICE/df$cprice
  df$pl_total_value_last_price<-ifelse(is.na(df$pl_total_value_last_price),df$current_net_total_ref_ccy,df$pl_total_value_last_price)

  #Checks
  df$perc_of_portfolio_md<-df$perc_of_portfolio/100
  df$perc_of_portfolio<-df$pl_total_value_last_price/sum(df$pl_total_value_last_price)

  df<-df[order(df$AssetClass,df$SubClass),]
  df$pos_target<-NA
  df$SubClass<-toupper(df$SubClass)
  #df$isin_or_account<-ifelse(is.na(df$isin_or_account),df$Asset,df$isin_or_account)
  rlist<-list("df"=df,"rf"=rf,"sec"=sec,"dfa"=dfa,"cry"=cry)
  return(rlist)

}



comparison_chart_actgR<-function(df,rf,assetclass="Bonds",chart_height =300)
{

  new_cash<-sum(df$TradeSimulation[df$AssetClass=="LIQUIDITY"],na.rm=T)-sum(df$trades,na.rm=T)

  assetclass<-toupper(assetclass)



  dftemp<-data.frame(
    xv=c("Portfolio", "Benchmark"),
    yv=c(sum(df$perc_of_portfolio[df$AssetClass==assetclass],na.rm=T), sum(rf$Now[rf$AssetClass==assetclass],na.rm=T))
  )

  fig <- plot_ly(x = dftemp$xv,y = dftemp$yv,name = "SF Zoo",type = "bar")
  fig

  if("CASH" %in% assetclass)
  {
    df$pos_target<-ifelse(is.na(df$pos_target),df$perc_of_portfolio,df$pos_target)
    dftemp<-data.frame(
      xv=c("Portfolio","Portfolio Target","FV Forwards", "Benchmark"),
      yv=c(sum(df$perc_of_portfolio[df$AssetClass %in% assetclass],na.rm=T),sum(df$TradeSimulation[df$AssetClass=="LIQUIDITY"],na.rm=T)-sum(df$trades,na.rm=T),sum(df$perc_of_portfolio[df$AssetClass=="FORWARDS"]), sum(rf$Now[rf$AssetClass%in%assetclass],na.rm=T))
    )
    dftemp$color<-ifelse(dftemp$xv=="Benchmark","#5777a7","#04103b")
    dftemp$color<-ifelse(dftemp$xv=="FV Forwards","#E8CCDC",dftemp$color)

    dftemp$xv <- factor(dftemp$xv, levels = c("Portfolio","Portfolio Target","FV Forwards","Benchmark"))

    fig <- plot_ly(x = dftemp$xv,y = dftemp$yv,text = paste0(round(dftemp$yv,4)*100,"%"),name = "SF Zoo", height = chart_height,type = "bar",marker = list(color = dftemp$color))%>%
      layout(title=paste0("Allocation ",paste0(str_to_title(tolower(assetclass)),collapse= " / ")),xaxis = list(title=""), yaxis = list(title="",tickformat =".1%"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

    fig
  }else{
    df$pos_target<-ifelse(is.na(df$pos_target),df$perc_of_portfolio,df$pos_target)
    dftemp<-data.frame(
      xv=c("Portfolio","Portfolio Target", "Benchmark"),
      yv=c(sum(df$perc_of_portfolio[df$AssetClass %in% assetclass],na.rm=T),sum(df$pos_target[df$AssetClass %in% assetclass],na.rm=T), sum(rf$Now[rf$AssetClass%in%assetclass],na.rm=T))
    )
    dftemp$xv <- factor(dftemp$xv, levels = c("Portfolio","Portfolio Target","Benchmark"))
    dftemp$color<-ifelse(dftemp$xv=="Benchmark","#5777a7","#04103b")
    fig <- plot_ly(x = dftemp$xv,y = dftemp$yv,text = paste0(round(dftemp$yv,4)*100,"%"),name = "SF Zoo", height = chart_height,type = "bar",marker = list(color = dftemp$color))%>%
      layout(title=paste0("Allocation ",paste0(str_to_title(tolower(assetclass)),collapse= " / ")),xaxis = list(title=""), yaxis = list(title="",tickformat =".1%"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

    fig
  }


}


comparison_chart_sctgR<-function(df,rf,SubClass="GMP",chart_height=300)
{

  SubClass<-toupper(SubClass)

  dftemp<-data.frame(
    xv=c("Portfolio", "Benchmark"),
    yv=c(sum(df$perc_of_portfolio[grepl(SubClass,df$SubClass)==T]), sum(rf$Now[grepl(SubClass,rf$SubClass)==T]))
  )

  fig1 <- plot_ly(x = dftemp$xv,y = dftemp$yv,name = "SF Zoo",type = "bar")
  fig1


  df$pos_target<-ifelse(is.na(df$pos_target),df$perc_of_portfolio,df$pos_target)
  dftemp<-data.frame(
    xv=c("Portfolio","Portfolio Target", "Benchmark"),
    yv=c(sum(df$perc_of_portfolio[grepl(SubClass,df$SubClass)==T],na.rm=T),sum(df$pos_target[grepl(SubClass,df$SubClass)==T],na.rm=T), sum(rf$Now[grepl(SubClass,rf$SubClass)==T],na.rm=T))
  )
  dftemp$xv <- factor(dftemp$xv, levels = c("Portfolio","Portfolio Target","Benchmark"))
  dftemp$color<-ifelse(dftemp$xv=="Benchmark","#5777a7","#04103b")
  fig2 <- plot_ly(x = dftemp$xv,y = dftemp$yv,text = paste0(round(dftemp$yv,4)*100,"%"),name = "SF Zoo",type = "bar", height = chart_height,marker = list(color = dftemp$color)) %>%
    layout(title=paste0("Allocation ",paste0(str_to_title(tolower(SubClass)),collapse= " / ")),xaxis = list(title=""), yaxis = list(title="",tickformat =".1%"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
  fig2

}

comparison_chart_gmptgR<-function(df,rf,mnemonic="perc_of_portfolio",chart_height = 300,reb_algo_level="Level 1",assetclasssubclass="Bonds")
{

  assetclasssubclass<-toupper(assetclasssubclass)
  if(reb_algo_level=="Level 1" | (reb_algo_level=="Level 2" & assetclasssubclass=="GMP"))
  {
    dftemp_pf<-df[grepl("GMP",df$SubClass)==T,c("SubClass","perc_of_portfolio","pos_target")]
    dftemp_pf$pos_target<-ifelse(is.na(dftemp_pf$pos_target),dftemp_pf$perc_of_portfolio,dftemp_pf$pos_target)
    dftemp_pf<-dftemp_pf%>%group_by(SubClass )%>%summarise(perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),pos_target=sum(pos_target,na.rm=T))

    dftemp_bm<-rf[grepl("GMP",rf$SubClass)==T,c("SubClass","Now")]
  }

  if(reb_algo_level=="Level 2" & !is.na(assetclasssubclass) & assetclasssubclass!="GMP")
  {
    dftemp_pf<-df[grepl(assetclasssubclass,df$AssetClass)==T,c("SubClass","perc_of_portfolio","pos_target")]
    dftemp_pf$pos_target<-ifelse(is.na(dftemp_pf$pos_target),dftemp_pf$perc_of_portfolio,dftemp_pf$pos_target)
    dftemp_pf<-dftemp_pf%>%group_by(SubClass )%>%summarise(perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),pos_target=sum(pos_target,na.rm=T))

    dftemp_bm<-rf[grepl(assetclasssubclass,rf$AssetClass)==T  & rf$Now!=0,c("SubClass","Now")]
  }


  dftemp_bm<-dftemp_bm%>%group_by(SubClass)%>%summarise(Now=sum(Now))
  dftemp<-merge(dftemp_pf,dftemp_bm,by.x="SubClass",by="SubClass",all=T)
  dftemp$perc_of_portfolio[is.na(dftemp$perc_of_portfolio)] <- 0
  dftemp$pos_target[is.na(dftemp$pos_target)] <- 0
  dftemp$Now[is.na(dftemp$Now)] <- 0
  dftemp$Benchmark<-dftemp$Now/sum(dftemp$Now)
  dftemp$Portfolio<-dftemp$perc_of_portfolio/sum(dftemp$perc_of_portfolio)
  dftemp$PortfolioTarget<-dftemp$pos_target/sum(dftemp$pos_target)

  fig1<-
    data.table::melt(as.data.table(dftemp[,c("SubClass","Portfolio","Benchmark")]), id.vars='SubClass') %>%
    plot_ly(x = ~variable, y = ~value,text = ~paste0(round(value,4)*100,"%"), type = 'bar', height = chart_height,
            name = ~SubClass, color = ~SubClass) %>%
    layout(showlegend = F,title="GMP",xaxis=list(title=""),yaxis = list(title = '',tickformat =".1%"), barmode = 'stack')

  col_hc<-rev(c("#04103b", "#99CCFF", "#dd0400", "#8E0152"))
  cols = colorRampPalette(col_hc)(nrow(dftemp))


  toplot<-data.table::melt(as.data.table(dftemp[,c("SubClass","Portfolio","PortfolioTarget","Benchmark")]), id.vars='SubClass')
  toplot<-toplot%>%group_by(variable)%>%mutate(ypos=cumsum(value))
  toplot$caption<-ifelse(toplot$value<0.05,"",paste0(round(toplot$value,4)*100,"%"))
  fig2<-
    toplot%>%
    plot_ly(x = ~variable, y = ~value, type = 'bar',colors=cols, height = chart_height,
            name = ~SubClass, color = ~SubClass,text = ~value, textposition = 'inside') %>%
    layout(showlegend = F,title="Global Market Portfolio",xaxis=list(title=""),yaxis = list(title = '',tickformat =".1%"), barmode = 'stack')%>%
    add_annotations(font=list(color = "white", size = 10, family = "Arial"),text = toplot$caption, x = toplot$variable,
                    y = toplot$ypos-toplot$value/2,
                    showarrow = FALSE)
  fig2
  reslist<-list("dftemp"=dftemp,"fig1"=fig1,"fig2"=fig2)
  return(reslist)
}


reb_asset_alloc_pietgR<-function(df,rf,show_allocations="All",mnemonic="perc_of_portfolio",chart_height = 300,reb_algo_level="Level 1",assetclasssubclass="Bonds")
{

  dftemp_pf<-df
  dftemp_bm<-rf[!rf$AssetClass%in%"FXHEDGE",]

  dftemp_pf$pos_target<-ifelse(is.na(dftemp_pf$pos_target),dftemp_pf$perc_of_portfolio,dftemp_pf$pos_target)
  dftemp_pf<-dftemp_pf%>%group_by(AssetClass )%>%summarise(perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),pos_target=sum(pos_target,na.rm=T))
  dftemp_bm<-dftemp_bm%>%group_by(AssetClass )%>%summarise(Now=sum(Now,na.rm=T))

  dftemp<-merge(dftemp_pf,dftemp_bm,by.x="AssetClass",by="AssetClass",all=T)
  dftemp$perc_of_portfolio[is.na(dftemp$perc_of_portfolio)] <- 0
  dftemp$pos_target[is.na(dftemp$pos_target)] <- 0
  dftemp$Now[is.na(dftemp$Now)] <- 0
  dftemp$Benchmark<-dftemp$Now/sum(dftemp$Now)
  dftemp$Portfolio<-dftemp$perc_of_portfolio/sum(dftemp$perc_of_portfolio)
  dftemp$PortfolioTarget<-dftemp$pos_target/sum(dftemp$pos_target)

  col_hc<-rev(c("#04103b", "#99CCFF", "#dd0400", "#8E0152"))
  cols = colorRampPalette(col_hc)(nrow(dftemp))


  toplot<-data.table::melt(as.data.table(dftemp[,c("AssetClass","Portfolio","PortfolioTarget","Benchmark")]), id.vars='AssetClass')
  toplot<-toplot%>%group_by(variable)%>%mutate(ypos=cumsum(value))
  toplot$caption<-ifelse(toplot$value<0.05,"",paste0(round(toplot$value,4)*100,"%"))

  if(show_allocations=="All")
  {
    fig2<-
      toplot%>%
      plot_ly(x = ~variable, y = ~value, type = 'bar',colors=cols, height = chart_height,
              name = ~AssetClass, color = ~AssetClass,text = ~value, textposition = 'inside') %>%
      layout(showlegend = F,title="Asset Allocation",xaxis=list(title=""),yaxis = list(title = '',tickformat =".1%"), barmode = 'stack')%>%
      add_annotations(font=list(color = "white", size = 10, family = "Arial"),text = toplot$caption, x = toplot$variable,
                      y = toplot$ypos-toplot$value/2,
                      showarrow = FALSE)
    fig2
  }else{
    m <- list(l = 60,r = 60,b = 100,t = 30,pad = 4)
    col_hc<-c("#04103b", "#99CCFF", "#dd0400", "#8E0152")
    cols1 = colorRampPalette(col_hc)(nrow(toplot[toplot$variable==show_allocations,]))
    fig2 <- plot_ly(toplot[toplot$variable==show_allocations & toplot$value !=0 & !is.na(toplot$AssetClass),], labels = ~AssetClass,textinfo='label+percent', values = ~value,colors=cols1, type = 'pie',marker = list(colors = cols1,line = list(color = '#FFFFFF', width = 1)),
                    showlegend = F)
    fig2 <- fig2 %>% layout(title = 'Asset Allocation',margin = m,
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    fig2
  }


  fig2<-fig2 %>% config(toImageButtonOptions = list( format = "svg",filename = "asset_allocation.svg",width = 800,height=500))


  reslist<-list("dftemp"=dftemp,"fig2"=fig2)
  return(reslist)
}

comparison_chart_bondstgR<-function(df,rf,aggregation_level="None",mnemonic="perc_of_portfolio",chart_height = 300,reb_algo_level="Level 1",assetclasssubclass="Bonds")
{
  #aggregation_level<-"Gov vs Corp"

  assetclasssubclass<-toupper(assetclasssubclass)

  dftemp_pf<-df[grepl("BONDS",df$AssetClass)==T,c("SubClass","CRNCY","perc_of_portfolio","pos_target")]
  dftemp_bm<-rf[grepl("BONDS",rf$AssetClass)==T,c("SubClass","Currency","Now")]

  if(aggregation_level=="Gov vs Corp")
  {
    dftemp_pf$SubClass<-ifelse(grepl("GOV",dftemp_pf$SubClass)==T,"Government",
                               ifelse(grepl("TIPS",dftemp_pf$SubClass)==T,"TIPS","Corporate"))
    dftemp_bm$SubClass<-ifelse(grepl("GOV",dftemp_bm$SubClass)==T,"Government",
                               ifelse(grepl("TIPS",dftemp_bm$SubClass)==T,"TIPS","Corporate"))
  }
  if(aggregation_level=="Currency")
  {
    dftemp_pf$SubClass<-dftemp_pf$CRNCY
    dftemp_bm$SubClass<-dftemp_bm$Currency
  }
  if(aggregation_level=="None")
  {
    dftemp_pf$SubClass<-dftemp_pf$SubClass
    dftemp_bm$SubClass<-dftemp_bm$SubClass
  }


  dftemp_pf$pos_target<-ifelse(is.na(dftemp_pf$pos_target),dftemp_pf$perc_of_portfolio,dftemp_pf$pos_target)
  dftemp_pf<-dftemp_pf%>%group_by(SubClass )%>%summarise(perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),pos_target=sum(pos_target,na.rm=T))
  dftemp_bm<-dftemp_bm%>%group_by(SubClass )%>%summarise(Now=sum(Now,na.rm=T))

  dftemp<-merge(dftemp_pf,dftemp_bm,by.x="SubClass",by="SubClass",all=T)
  dftemp$perc_of_portfolio[is.na(dftemp$perc_of_portfolio)] <- 0
  dftemp$pos_target[is.na(dftemp$pos_target)] <- 0
  dftemp$Now[is.na(dftemp$Now)] <- 0
  dftemp$Benchmark<-dftemp$Now/sum(dftemp$Now)
  dftemp$Portfolio<-dftemp$perc_of_portfolio/sum(dftemp$perc_of_portfolio)
  dftemp$PortfolioTarget<-dftemp$pos_target/sum(dftemp$pos_target)

  col_hc<-rev(c("#04103b", "#99CCFF", "#dd0400", "#8E0152"))
  cols = colorRampPalette(col_hc)(nrow(dftemp))


  toplot<-data.table::melt(as.data.table(dftemp[,c("SubClass","Portfolio","PortfolioTarget","Benchmark")]), id.vars='SubClass')
  toplot<-toplot%>%group_by(variable)%>%mutate(ypos=cumsum(value))
  toplot$caption<-ifelse(toplot$value<0.05,"",paste0(round(toplot$value,4)*100,"%"))
  fig2<-
    toplot%>%
    plot_ly(x = ~variable, y = ~value, type = 'bar',colors=cols, height = chart_height,
            name = ~SubClass, color = ~SubClass,text = ~value, textposition = 'inside') %>%
    layout(showlegend = F,title="Fixed Income Allocation",xaxis=list(title=""),yaxis = list(title = '',tickformat =".1%"), barmode = 'stack')%>%
    add_annotations(font=list(color = "white", size = 10, family = "Arial"),text = toplot$caption, x = toplot$variable,
                    y = toplot$ypos-toplot$value/2,
                    showarrow = FALSE)
  fig2
  reslist<-list("dftemp"=dftemp,"fig2"=fig2)
  return(reslist)
}


#Rebalancing
rebalance_algotgR<-function(rlist_rebalancing,df,rf,alloc_matrix,i,remove_duplicates="Yes",level1_adjustment_algo="MaximumDistance",merge_variables=c("AssetClass","SubClass"),asset_level_identifier="ISIN & Currency")
{
  tryCatch({
    df<-subset(df,select=c(-Weight))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #Let the subclass dominate
  alloc_matrix$AssetClass<-ifelse(alloc_matrix$SubClass!="" & !is.na(alloc_matrix$SubClass),NA,alloc_matrix$AssetClass)

  protected_assets<-alloc_matrix$Asset[!is.na(alloc_matrix$Asset) & is.na(alloc_matrix$Weight)]

  tryCatch({
    reb_algo_level<-alloc_matrix$Reb_Algo_Level[i]
    reb_algo_level<-ifelse(is.na(reb_algo_level),"Level 1",reb_algo_level)
    gmp_algo<-alloc_matrix$Missing_Excess[i]
    gmp_algo<-ifelse(is.na(gmp_algo),"Ignore Missing",gmp_algo)

    df$AssetClass<-ifelse(df$AssetClass == "EQUITIES" & df$SubClass%in%c("AI EUROPE","AI USA"),"ALTERNATIVE INVESTMENTS",df$AssetClass)


    if(asset_level_identifier=="ISIN & Currency")
    {
      df$AssetCRNCY<-paste0(df$isin_or_account,"_",df$CRNCY)
    }
    if(asset_level_identifier=="ISIN Only")
    {
      df$AssetCRNCY<-paste0(df$isin_or_account)
    }
    if(asset_level_identifier=="Asset Name")
    {
      df$AssetCRNCY<-paste0(df$Asset)
    }


    if(reb_algo_level=="Ignore")
    {

    }else{


      if(reb_algo_level=="Level 3")
      {

        if(!is.na(alloc_matrix$AssetClass[i]))
        {
          sel<-alloc_matrix$AssetClass[i]
          selw<-alloc_matrix$Weight[i]
          if(is.na(selw))
          {
            selw<-sum(rf$Now[rf$AssetClass==sel])
          }
          rfs<-rf[rf$AssetClass==sel & rf$Weight>0,c("ID_ISIN_CHECK","SubClass","AssetName","Currency","Weight")]
          rfs<-rfs[!is.na(rfs$ID_ISIN_CHECK),]
          rfs$AssetClass<-sel
          rfs$Weight<-rfs$Weight/sum(rfs$Weight,na.rm=T)*selw


          #rfs<-merge(df,rfs,by.x=c("isin_or_account","AssetClass","SubClass"),by.y=c("ID_ISIN_CHECK","AssetClass","SubClass"),all=T)
          #merge_variables<-"AssetClass"
          if("CRNCY" %in% merge_variables)
          {
            rfs$CRNCY<-rfs$Currency
          }
          if(merge_variables[1]!="")
          {
            rfs<-merge(df,rfs[, !names(rfs) %in% setdiff(merge_variables, merge_variables)],by.x=c("isin_or_account",merge_variables),by.y=c("ID_ISIN_CHECK",merge_variables),all=T)
          }else{
            rfs<-merge(df,rfs[, !names(rfs) %in% setdiff(merge_variables, merge_variables)],by.x=c("isin_or_account"),by.y=c("ID_ISIN_CHECK"),all=T)
          }

          if(remove_duplicates=="No")
          {

            rfsu <- rfs %>%
              filter(!AssetClass %in% c(sel))
            rfs <- rfs %>%
              filter(AssetClass %in% c(sel)) %>%
              group_by(isin_or_account) %>%
              summarise(across(everything(), ~ first(na.omit(.)), .names = "{.col}"), .groups = "drop")
            rfs<-rbind(rfs,rfsu)
          }



          rfs$Asset<-ifelse(is.na(rfs$Asset),rfs$AssetName,rfs$Asset)
          rfs$CRNCY<-ifelse(is.na(rfs$CRNCY),rfs$Currency,rfs$CRNCY)
          rfs$pos_target<-ifelse(rfs$AssetClass==sel & !is.na(rfs$Weight),rfs$Weight,
                                 ifelse(rfs$AssetClass==sel & is.na(rfs$Weight),0,rfs$pos_target))
          rfs$perc_of_portfolio<-ifelse(rfs$AssetClass==sel & is.na(rfs$perc_of_portfolio),0,rfs$perc_of_portfolio)

          #View(rfs)
          #sum(rfs$pos_target,na.rm=T)
          #View(rfs[rfs$AssetClass=="Bonds",])

          rfs<-subset(rfs,select=c(-AssetName,-Weight,-Currency))
          df<-rfs
          sum(df$pos_target[df$AssetClass=="BONDS"],na.rm=T)
          #View(df[df$AssetClass=="Bonds",])
          df<-df[order(df$AssetClass,df$SubClass),]



          #Add missing prices and currency prices
          df$portfolio_ref_ccy<-rlist_rebalancing$df$portfolio_ref_ccy[1]
          newsec<-rlist_rebalancing$sec[,c("ID_ISIN","CRNCY","Symbol","LAST_PRICE")]
          names(newsec)<-c("ID_ISIN","CRNCY","Symbol_SEC","LAST_PRICE_SEC")
          df<-merge(df,newsec,by.x=c("isin_or_account","CRNCY"),by.y=c("ID_ISIN","CRNCY"),all.x=T)
          df$Symbol<-ifelse(is.na(df$Symbol),df$Symbol_SEC,df$Symbol)
          df$LAST_PRICE<-ifelse(is.na(df$LAST_PRICE),df$LAST_PRICE_SEC,df$LAST_PRICE)

          crytr<-rlist_rebalancing$cry[,c("cpair","cprice")]
          names(crytr)<-c("cpair","cpricesec")
          df$crytr<-paste0(df$portfolio_ref_ccy,df$CRNCY," Curncy")
          df<-merge(df,crytr,by.x="crytr",by.y="cpair",all.x=T)
          df$cprice<-ifelse(is.na(df$cprice),df$cpricesec,df$cprice)
          df<-subset(df,select=-c(cpricesec,Symbol_SEC,LAST_PRICE_SEC))

        }

        if(is.na(alloc_matrix$AssetClass[i]) & alloc_matrix$SubClass[i]=="GMP")
        {
          sel<-"GMP"
          selw<-alloc_matrix$Weight[i]
          if(is.na(selw))
          {
            selw<-sum(rf$Now[substr(rf$SubClass,1,3)==sel & rf$AssetClass=="EQUITIES"])
          }
          rfs<-rf[substr(rf$SubClass,1,3)==sel  & rf$AssetClass=="EQUITIES" & rf$Weight>0,c("ID_ISIN_CHECK","SubClass","AssetName","Currency","Weight")]
          rfs<-rfs[!is.na(rfs$ID_ISIN_CHECK),]
          rfs$AssetClass<-"EQUITIES"
          rfs$Weight<-rfs$Weight/sum(rfs$Weight,na.rm=T)*selw


          #rfs<-merge(df,rfs,by.x=c("isin_or_account","AssetClass","SubClass"),by.y=c("ID_ISIN_CHECK","AssetClass","SubClass"),all=T)
          #merge_variables<-"AssetClass"
          if("CRNCY" %in% merge_variables)
          {
            rfs$CRNCY<-rfs$Currency
          }
          if(merge_variables[1]!="")
          {
            rfs<-merge(df,rfs[, !names(rfs) %in% setdiff(merge_variables, merge_variables)],by.x=c("isin_or_account",merge_variables),by.y=c("ID_ISIN_CHECK",merge_variables),all=T)
          }else{
            rfs<-merge(df,rfs[, !names(rfs) %in% setdiff(merge_variables, merge_variables)],by.x=c("isin_or_account"),by.y=c("ID_ISIN_CHECK"),all=T)
          }

          if(remove_duplicates=="Yes")
          {

            rfsu <- rfs %>%
              filter(!AssetClass %in% c(sel))
            rfs <- rfs %>%
              filter(AssetClass %in% c(sel)) %>%
              group_by(isin_or_account) %>%
              summarise(across(everything(), ~ first(na.omit(.)), .names = "{.col}"), .groups = "drop")
            rfs<-rbind(rfs,rfsu)
          }

          rfs$Asset<-ifelse(is.na(rfs$Asset),rfs$AssetName,rfs$Asset)
          rfs$CRNCY<-ifelse(is.na(rfs$CRNCY),rfs$Currency,rfs$CRNCY)
          rfs$pos_target<-ifelse(substr(rfs$SubClass,1,3)==sel  & rfs$AssetClass=="EQUITIES" & !is.na(rfs$Weight),rfs$Weight,
                                 ifelse(substr(rfs$SubClass,1,3)==sel  & rfs$AssetClass=="EQUITIES" & is.na(rfs$Weight),0,rfs$pos_target))
          rfs$perc_of_portfolio<-ifelse(substr(rfs$SubClass,1,3)==sel  & rfs$AssetClass=="EQUITIES" & is.na(rfs$perc_of_portfolio),0,rfs$perc_of_portfolio)

          #View(rfs)
          #sum(rfs$pos_target,na.rm=T)
          #View(rfs[rfs$AssetClass=="Bonds",])

          rfs<-subset(rfs,select=c(-AssetName,-Weight,-Currency))
          df<-rfs

          #View(df[df$AssetClass=="Bonds",])
          df<-df[order(df$AssetClass,df$SubClass),]



        }
      }else{
        df<-merge(df,rf[,c("ID_ISIN_CHECK","Weight")],by.x="isin_or_account",by.y="ID_ISIN_CHECK",all.x=T)
        df$distance<-df$perc_of_portfolio-df$Weight

        #Asset
        #Asset Class Level 1
        if(!is.na(alloc_matrix$Asset[i]))
        {
          sel<-alloc_matrix$Asset[i]
          selw<-alloc_matrix$Weight[i]
          df$pos_target[df$AssetCRNCY==sel  & !is.na(df$AssetCRNCY)][1]<-selw
          #df[df$AssetCRNCY==sel  & !is.na(df$AssetCRNCY),][1]<-selw
        }

        #Asset Class
        if(!is.na(alloc_matrix$AssetClass[i]) & reb_algo_level=="Level 1")
        {

          sel<-alloc_matrix$AssetClass[i]
          selw<-alloc_matrix$Weight[i]
          if(is.na(selw))
          {
            selw<-sum(rf$Now[rf$AssetClass==sel])
          }
          if(nrow(df[df$AssetClass==sel & !is.na(df$AssetClass),])==1)
          {
            df$pos_target[df$AssetClass==sel  & !is.na(df$AssetClass)][1]<-selw
          }else{
            #Highest distance algo
            if(selw>sum(df$perc_of_portfolio[df$AssetClass==sel & !is.na(df$AssetClass)]))
            {
              if(level1_adjustment_algo=="MaximumDistance" & nrow(df[!is.na(df$distance)&df$AssetClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T),])>0)
              {
                df$pos_target[!is.na(df$distance)&df$AssetClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]<-
                  max(
                    df$perc_of_portfolio[!is.na(df$distance)&df$AssetClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]+
                      (selw-sum(df$perc_of_portfolio[df$AssetClass==sel & !is.na(df$AssetClass)],na.rm=T))
                    ,0)
              }
              if(level1_adjustment_algo=="AbsoluteWeight")
              {
                df$pos_target[df$AssetClass == sel & df$perc_of_portfolio==min(df$perc_of_portfolio[df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)][1]<-
                  max(
                    df$perc_of_portfolio[df$AssetClass == sel & df$perc_of_portfolio==min(df$perc_of_portfolio[df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)][1]+
                      (selw-sum(df$perc_of_portfolio[df$AssetClass==sel & !is.na(df$AssetClass)],na.rm=T)),
                    0)
              }
              if(level1_adjustment_algo=="EqualWeight")
              {
                df$pos_target[df$AssetClass == sel& !df$AssetCRNCY %in% protected_assets]<-
                  (selw-sum(df$perc_of_portfolio[df$AssetClass==sel& df$AssetCRNCY %in% protected_assets],na.rm=T))/length(df$pos_target[df$AssetClass == sel& !is.na(df$AssetClass)& !df$AssetCRNCY %in% protected_assets])
              }
              if(level1_adjustment_algo=="EqualTrades")
              {
                df$pos_target[df$AssetClass == sel& !df$AssetCRNCY %in% protected_assets]<-
                  df$perc_of_portfolio[df$AssetClass == sel& !df$AssetCRNCY %in% protected_assets]+
                  (selw-sum(df$perc_of_portfolio[df$AssetClass==sel],na.rm=T))/length(df$pos_target[df$AssetClass == sel& !is.na(df$AssetClass)& !df$AssetCRNCY %in% protected_assets])
              }
            }else{
              if(level1_adjustment_algo=="MaximumDistance" & nrow(df[!is.na(df$distance)&df$AssetClass == sel & df$distance==max(df$distance[!is.na(df$perc_of_portfolio)&df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T),])>0)
              {
                df$pos_target[!is.na(df$distance)&df$AssetClass == sel & df$distance==max(df$distance[!is.na(df$perc_of_portfolio)&df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]<-
                  max(
                    df$perc_of_portfolio[!is.na(df$distance)&df$AssetClass == sel & df$distance==max(df$distance[!is.na(df$perc_of_portfolio)&df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]+
                      (selw-sum(df$perc_of_portfolio[df$AssetClass==sel & !is.na(df$AssetClass)],na.rm=T))
                    ,0)
              }
              if(level1_adjustment_algo=="AbsoluteWeight")
              {
                df$pos_target[df$AssetClass == sel & df$perc_of_portfolio==max(df$perc_of_portfolio[df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)][1]<-
                  max(
                    df$perc_of_portfolio[df$AssetClass == sel & df$perc_of_portfolio==max(df$perc_of_portfolio[df$AssetClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)][1]+
                      selw-sum(df$perc_of_portfolio[df$AssetClass==sel],na.rm=T),
                    0)
              }
              if(level1_adjustment_algo=="EqualWeight")
              {
                df$pos_target[df$AssetClass == sel& !df$AssetCRNCY %in% protected_assets]<-
                  (selw-sum(df$pos_target[df$AssetClass == sel& df$AssetCRNCY %in% protected_assets],na.rm=T))/length(df$pos_target[df$AssetClass == sel& !is.na(df$AssetClass)& !df$AssetCRNCY %in% protected_assets])
              }
              if(level1_adjustment_algo=="EqualTrades")
              {
                df$pos_target[df$AssetClass == sel & !is.na(df$AssetClass)& !df$AssetCRNCY %in% protected_assets]<-
                  df$perc_of_portfolio[df$AssetClass == sel & !is.na(df$AssetClass)& !df$AssetCRNCY %in% protected_assets]+
                  (selw-sum(df$perc_of_portfolio[df$AssetClass==sel],na.rm=T))/length(df$pos_target[df$AssetClass == sel& !is.na(df$AssetClass)& !df$AssetCRNCY %in% protected_assets])
              }
            }
          }
        }


        #Sub Asset Class Level 1
        if(!is.na(alloc_matrix$SubClass[i]) & alloc_matrix$SubClass[i]!="GMP"  & reb_algo_level=="Level 1")
        {

          sel<-alloc_matrix$SubClass[i]
          selw<-alloc_matrix$Weight[i]
          if(is.na(selw))
          {
            selw<-sum(rf$Now[rf$SubClass==sel])
          }
          if(nrow(df[df$SubClass==sel & !is.na(df$SubClass),])==1)
          {
            df$pos_target[df$SubClass==sel  & !is.na(df$SubClass)][1]<-selw
          }else{
            if(selw>sum(df$perc_of_portfolio[df$SubClass==sel & !is.na(df$SubClass)]))
            {
              if(level1_adjustment_algo=="MaximumDistance" & nrow(df[!is.na(df$distance)&df$SubClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T),])>0)
              {
                df$pos_target[!is.na(df$distance)&df$SubClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]<-
                  max(
                    df$perc_of_portfolio[!is.na(df$distance)&df$SubClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]+
                      (selw-sum(df$perc_of_portfolio[df$SubClass==sel& !is.na(df$SubClass)],na.rm=T))
                    ,0)
              }
              if(level1_adjustment_algo=="AbsoluteWeight")
              {
                df$pos_target[df$SubClass == sel & df$perc_of_portfolio==min(df$perc_of_portfolio[df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)][1]<-
                  max(
                    df$perc_of_portfolio[df$SubClass == sel & df$perc_of_portfolio==min(df$perc_of_portfolio[df$SubClass==sel],na.rm=T)][1]+
                      selw-sum(df$perc_of_portfolio[df$SubClass==sel& !is.na(df$SubClass) ],na.rm=T)
                    ,0)
              }
              if(level1_adjustment_algo=="EqualWeight")
              {
                df$pos_target[df$SubClass == sel& !df$AssetCRNCY %in% protected_assets]<-
                  (selw-sum(df$perc_of_portfolio[df$SubClass==sel& df$AssetCRNCY %in% protected_assets],na.rm=T))/length(df$pos_target[df$SubClass == sel& !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets])
              }
              if(level1_adjustment_algo=="EqualTrades")
              {
                df$pos_target[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]<-
                  df$perc_of_portfolio[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]+
                  (selw-sum(df$perc_of_portfolio[df$SubClass==sel],na.rm=T))/length(df$pos_target[df$SubClass == sel& !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets])
              }
            }else{
              if(level1_adjustment_algo=="MaximumDistance" & nrow(df[!is.na(df$distance)&df$SubClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T),])>0)
              {
                df$pos_target[!is.na(df$distance)&df$SubClass == sel & df$distance==max(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]<-
                  max(
                    df$perc_of_portfolio[!is.na(df$distance)&df$SubClass == sel & df$distance==max(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]+
                      (selw-sum(df$perc_of_portfolio[df$SubClass==sel],na.rm=T))
                    ,0)
              }
              if(level1_adjustment_algo=="AbsoluteWeight")
              {
                df$pos_target[df$SubClass == sel & df$perc_of_portfolio==max(df$perc_of_portfolio[df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)][1]<-
                  max(
                    df$perc_of_portfolio[df$SubClass == sel & df$perc_of_portfolio==max(df$perc_of_portfolio[df$SubClass==sel],na.rm=T)][1]+
                      selw-sum(df$perc_of_portfolio[df$SubClass==sel],na.rm=T)
                    ,0)
              }
              if(level1_adjustment_algo=="EqualWeight")
              {
                df$pos_target[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]<-
                  (selw-sum(df$perc_of_portfolio[df$SubClass==sel& df$AssetCRNCY %in% protected_assets],na.rm=T))/length(df$pos_target[df$SubClass == sel& !is.na(df$SubClass)]& !df$AssetCRNCY %in% protected_assets)
              }
              if(level1_adjustment_algo=="EqualTrades")
              {
                df$pos_target[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]<-
                  df$perc_of_portfolio[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]+
                  (selw-sum(df$perc_of_portfolio[df$SubClass==sel],na.rm=T))/length(df$pos_target[df$SubClass == sel& !is.na(df$SubClass)]& !df$AssetCRNCY %in% protected_assets)
              }
            }
          }
        }


        #Global Market Portfolio
        if((!is.na(alloc_matrix$SubClass[i]) & alloc_matrix$SubClass[i]=="GMP") | (!is.na(alloc_matrix$AssetClass[i]) & reb_algo_level=="Level 2"))
        {

          selwgmp<-alloc_matrix$Weight[i]
          if(is.na(selwgmp[1]))
          {
            if(!is.na(alloc_matrix$SubClass[i])& alloc_matrix$SubClass[i]=="GMP")
            {
              selwgmp<-sum(rf$Now[grepl("GMP",rf$SubClass)==T])
            }else{
              selwgmp<-sum(rf$Now[grepl(alloc_matrix$AssetClass[i],rf$AssetClass)==T])
            }

            alloc_matrix$Weight[i]<-selwgmp
          }

          if(!is.na(alloc_matrix$AssetClass[i]))
          {
            gmp_cmp<-comparison_chart_gmp(df,rf,reb_algo_level=reb_algo_level,assetclasssubclass=alloc_matrix$AssetClass[i])
          }else{
            gmp_cmp<-comparison_chart_gmp(df,rf,reb_algo_level=reb_algo_level,assetclasssubclass=alloc_matrix$SubClass[i])
          }



          gmp_cmp<-gmp_cmp$dftemp
          if(gmp_algo=="Ignore Missing")
          {
            gmp_cmp<-gmp_cmp[gmp_cmp$Portfolio>0,]
          }
          if(gmp_algo=="Add Remove")
          {
            gmp_cmp<-gmp_cmp$PortfolioTarget[gmp_cmp$Now == 0 ]<-0
          }

          gmp_cmp$Target<-gmp_cmp$Benchmark/sum(gmp_cmp$Benchmark)*alloc_matrix$Weight[i]


          for(ij in 1:nrow(gmp_cmp))
          {
            sel<-gmp_cmp$SubClass[ij]
            selw<-gmp_cmp$Target[ij]

            if(sel %in% df$SubClass)
            {

              if(nrow(df[df$SubClass==sel & !is.na(df$SubClass),])==1)
              {
                df$pos_target[df$SubClass==sel & !is.na(df$SubClass)][1]<-selw
              }else{
                if(selw > sum(df$perc_of_portfolio[df$SubClass==sel & !is.na(df$SubClass)]))
                {
                  if(level1_adjustment_algo=="MaximumDistance" & nrow(df[!is.na(df$distance)&df$SubClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T),])>0)
                  {
                    df$pos_target[!is.na(df$distance)&df$SubClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]<-
                      max(
                        df$perc_of_portfolio[!is.na(df$distance)&df$SubClass == sel & df$distance==min(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]+
                          (selw-sum(df$perc_of_portfolio[df$SubClass==sel& !is.na(df$SubClass)],na.rm=T))
                        ,0)
                  }
                  if(level1_adjustment_algo=="AbsoluteWeight"){
                    df$pos_target[df$SubClass == sel & !is.na(df$SubClass) & df$perc_of_portfolio==min(df$perc_of_portfolio[df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)][1]<-
                      max(
                        df$perc_of_portfolio[df$SubClass == sel & !is.na(df$SubClass) & df$perc_of_portfolio==min(df$perc_of_portfolio[df$SubClass==sel],na.rm=T)][1]+
                          selw-sum(df$perc_of_portfolio[df$SubClass==sel& !is.na(df$SubClass)],na.rm=T)
                        ,0)
                  }
                  if(level1_adjustment_algo=="EqualWeight")
                  {
                    df$pos_target[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]<-
                      (selw-sum(df$perc_of_portfolio[df$SubClass==sel& df$AssetCRNCY %in% protected_assets],na.rm=T))/length(df$pos_target[df$SubClass == sel& !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets])
                  }
                  if(level1_adjustment_algo=="EqualTrades")
                  {
                    df$pos_target[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]<-
                      df$perc_of_portfolio[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]+
                      (selw-sum(df$perc_of_portfolio[df$SubClass==sel],na.rm=T))/length(df$pos_target[df$SubClass == sel& !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets])
                  }
                }else{
                  if(level1_adjustment_algo=="MaximumDistance" & nrow(df[!is.na(df$distance)&df$SubClass == sel & df$distance==max(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T),])>0)
                  {
                    df$pos_target[!is.na(df$distance)&df$SubClass == sel & df$distance==max(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]<-
                      max(
                        df$perc_of_portfolio[!is.na(df$distance)&df$SubClass == sel & df$distance==max(df$distance[!is.na(df$perc_of_portfolio)&df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)]+
                          (selw-sum(df$perc_of_portfolio[df$SubClass==sel& !is.na(df$SubClass)],na.rm=T))
                        ,0)
                  }
                  if(level1_adjustment_algo=="AbsoluteWeight")
                  {
                    df$pos_target[df$SubClass == sel & !is.na(df$SubClass) & df$perc_of_portfolio==max(df$perc_of_portfolio[df$SubClass==sel & !df$AssetCRNCY %in% protected_assets],na.rm=T)][1]<-
                      max(
                        df$perc_of_portfolio[df$SubClass == sel & !is.na(df$SubClass) & df$perc_of_portfolio==max(df$perc_of_portfolio[df$SubClass==sel],na.rm=T)][1]+
                          selw-sum(df$perc_of_portfolio[df$SubClass==sel& !is.na(df$SubClass)],na.rm=T)
                        ,0)
                  }
                  if(level1_adjustment_algo=="EqualWeight")
                  {
                    df$pos_target[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]<-
                      (selw-sum(df$perc_of_portfolio[df$SubClass == sel & !is.na(df$SubClass)& df$AssetCRNCY %in% protected_assets],na.rm=T))/length(df$pos_target[df$SubClass == sel& !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets])
                  }
                  if(level1_adjustment_algo=="EqualTrades")
                  {
                    df$pos_target[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]<-
                      df$perc_of_portfolio[df$SubClass == sel & !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets]+
                      (selw-sum(df$perc_of_portfolio[df$SubClass==sel& !is.na(df$SubClass)],na.rm=T))/length(df$pos_target[df$SubClass == sel& !is.na(df$SubClass)& !df$AssetCRNCY %in% protected_assets])
                  }
                }
              }

            }else{

              add<-head(df[grepl("GMP",df$SubClass)==T,],1)
              add[1,]<-NA
              add$SubClass<-sel
              add$Asset<-head(rf$AssetName[rf$SubClass==sel],1)
              add$current_net_total_ref_ccy<-0
              add$perc_of_portfolio<-0
              add$pos_target<-selw
              add$CRNCY<-head(rf$Currency[rf$SubClass==sel],1)
              add$AssetClass<-head(rf$AssetClass[rf$SubClass==sel],1)
              add$SubClass<-head(rf$SubClass[rf$SubClass==sel],1)
              add$isin_or_account <-head(rf$ID_ISIN_CHECK[rf$SubClass==sel],1)
              add$Symbol <-head(rf$Ticker[rf$SubClass==sel],1)
              add$portfolio_ref_ccy <- df$portfolio_ref_ccy[1]
              add$LAST_PRICE<-rlist_rebalancing$sec$LAST_PRICE[rlist_rebalancing$sec$Symbol==add$Symbol & !is.na(rlist_rebalancing$sec$Symbol)]

              cpricedf<-rlist_rebalancing$cry[!is.na(rlist_rebalancing$cry$cpair) & rlist_rebalancing$cry$cpair==paste0(rlist_rebalancing$df$portfolio_ref_ccy,add$CRNCY," Curncy"),]
              cpricedf<-cpricedf$cprice[!is.na(cpricedf$cprice)]
              add$cprice<-cpricedf

              df<-rbind(df,add)
              df<-df[order(df$AssetClass,df$SubClass),]

            }
          }

        }

        df<-subset(df,select=c(-distance,-Weight))
      }
    }

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  return(df)
}

currency_equalization_algotgR<-function(df, EqualizationClass="BONDS",df_nav)
{

  cash_impact_trades<-df%>%group_by(CRNCY)%>%summarise(sumcashimpact=sum(trades,na.rm=T))

  l<-df[df$AssetClass=="LIQUIDITY" & !is.na(df$Asset),]
  l<-l%>%group_by(CRNCY)%>%summarise(perc_of_portfolio=sum(perc_of_portfolio,na.rm=T))
  l<-merge(l,cash_impact_trades,by="CRNCY",all=T)
  l<-l[!is.na(l$perc_of_portfolio ),]
  l$CashSimulation<-l$perc_of_portfolio-l$sumcashimpact
  l<-l[order(l$CashSimulation),]

  disp<-df[df$AssetClass==EqualizationClass & !is.na(df$trade_value) & df$trade_value!=0,]

  margin_of_safety<-1.1
  reinvestir<-0
  ix<-1
  if(nrow(l[l$CashSimulation<0,]))
  {
    for(ix in 1:nrow(l[l$CashSimulation<0,]))
    {
      if(l$CashSimulation[ix]<0)
      {
        choose<-disp[disp$CRNCY==l$CRNCY[ix] & disp$trade_value!=0,]
        choose<-choose[choose$TradeSimulation==max(choose$TradeSimulation),]
        df$pos_target[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          choose$pos_target+(l$CashSimulation[ix]*margin_of_safety)

        df$TradeSimulation[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          df$pos_target[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]
        df$trades[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          df$pos_target[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]-
          df$perc_of_portfolio[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]
        df$trade_value[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          df$trades[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]*df_nav
        df$trade_value_adj[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          df$trade_value[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]

        reinvestir<-reinvestir+(l$CashSimulation[ix]*margin_of_safety)
      }
    }


    sum(df$trades,na.rm=T)
    l<-l[order(-l$CashSimulation),]
    for(iy in 1:nrow(l[l$CashSimulation>0]))
    {

      if(l$CashSimulation[iy]>0 & reinvestir<0)
      {
        sum(df$trades,na.rm=T)
        sum(df$TradeSimulation,na.rm=T)
        choose<-disp[disp$CRNCY==l$CRNCY[iy] & disp$trade_value!=0,]
        choose<-choose[choose$TradeSimulation==min(choose$TradeSimulation),]
        df$pos_target[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          choose$pos_target-max(reinvestir,-l$CashSimulation[iy]/margin_of_safety)

        df$TradeSimulation[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          df$pos_target[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]
        df$trades[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          df$pos_target[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]-
          df$perc_of_portfolio[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]
        df$trade_value[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          df$trades[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]*df_nav
        df$trade_value_adj[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]<-
          df$trade_value[df$isin_or_account == choose$isin_or_account[1] & !is.na(df$isin_or_account) & !is.na(df$pos_target)]

        reinvestir<-reinvestir-max(reinvestir,-(l$CashSimulation[iy]/margin_of_safety))
      }
    }
  }

  return(df)
}

equalize_algotgR<-function(df,rlist_rebalancing,alloc_matrix,MinCash,EqualizationLine,asset_level_identifier="ISIN_CRNCY")
{

  trade_value<-sum(df$TradeSimulation,na.rm=T)-1
  current_cash<-sum(df$perc_of_portfolio[df$AssetClass=="LIQUIDITY"],na.rm=T)
  toequalize<-(trade_value-current_cash+MinCash)
  EqualizationLine<-EqualizationLine[!is.na(EqualizationLine)]

  forbiddenassets<-alloc_matrix$Asset[is.na(alloc_matrix$Weight) | alloc_matrix$Weight==""]

  if(asset_level_identifier=="ISIN & Currency")
  {
    df$AssetCRNCY<-paste0(df$isin_or_account,"_",df$CRNCY)
  }
  if(asset_level_identifier=="ISIN Only")
  {
    df$AssetCRNCY<-paste0(df$isin_or_account)
  }
  if(asset_level_identifier=="Asset Name")
  {
    df$AssetCRNCY<-paste0(df$Asset)
  }


  if(length(EqualizationLine)==0)
  {
    equalizeperlin<-toequalize/1
    bonds<-as.data.frame(df[df$AssetClass=="BONDS" & !is.na(df$SubClass) & !paste0(df$isin_or_account,"_",df$CRNCY) %in% forbiddenassets,])%>%group_by(SubClass)%>%summarise(isin_or_account=head(isin_or_account,1),CRNCY=head(CRNCY,1),SumW=sum(as.numeric(TradeSimulation)))
    bondsbm<-rlist_rebalancing$rf%>%group_by(SubClass)%>%summarise(SumBM=sum(Weight))
    potential<-merge(bonds,bondsbm,by="SubClass")

    potential$active<-potential$SumW-potential$SumBM
    if(toequalize>0)
    {
      target_asset<-potential[potential$active==max(potential$active),]
    }else{
      target_asset<-potential[potential$active==min(potential$active),]
    }

    df$pos_target[df$isin_or_account==target_asset$isin_or_account & df$CRNCY==target_asset$CRNCY]<-ifelse(is.na(df$pos_target[df$isin_or_account==target_asset$isin_or_account & df$CRNCY==target_asset$CRNCY]),df$perc_of_portfolio[df$isin_or_account==target_asset$isin_or_account & df$CRNCY==target_asset$CRNCY],df$pos_target[df$isin_or_account==target_asset$isin_or_account & df$CRNCY==target_asset$CRNCY])
    df$pos_target[df$isin_or_account==target_asset$isin_or_account & df$CRNCY==target_asset$CRNCY]<-df$pos_target[df$isin_or_account==target_asset$isin_or_account & df$CRNCY==target_asset$CRNCY]-equalizeperlin
    df$TradeSimulation<-ifelse(is.na(df$pos_target),df$perc_of_portfolio,df$pos_target)
    sum(df$TradeSimulation)

  }else{
    EqualizationLine<-toupper(EqualizationLine)
    equalizeperlin<-toequalize/length(EqualizationLine)

    for(ij in 1:length(EqualizationLine))
    {
      print(ij)
      df$pos_target[df$AssetCRNCY==EqualizationLine[ij]]<-ifelse(is.na(df$pos_target[df$AssetCRNCY==EqualizationLine[ij]]),df$perc_of_portfolio[df$AssetCRNCY==EqualizationLine[ij]],df$pos_target[df$AssetCRNCY==EqualizationLine[ij]])
      df$pos_target[df$AssetCRNCY==EqualizationLine[ij]]<-df$pos_target[df$AssetCRNCY==EqualizationLine[ij]]-equalizeperlin
      df$TradeSimulation<-ifelse(is.na(df$pos_target),df$perc_of_portfolio,df$pos_target)
      sum(df$TradeSimulation)
    }
  }

  df$trades<-df$pos_target-df$perc_of_portfolio
  sum(df$trades,na.rm=T)
  sum(df$pos_target,na.rm=T)

  return(df)
}

calculate_hedgestgR<-function(df,rf,alloc_matrix,chart_height=350,pf_nav,aam_file)
{

  pf_ref_crncy<-substr(alloc_matrix$RefPF[1],1,3)
  ref_profile<-substr(alloc_matrix$RefPF[1],5,6)
  hedging_policy<-dbGetQuery(pool, paste0("select * from gaa.gaa_hedging_policy where AAM_File = '",aam_file,"'"))
  hedging_policy_sub<-dbGetQuery(pool, paste0("select * from gaa.gaa_hedging_policy_sub where AAM_File = '",aam_file,"'"))

  ref_profile<-ifelse(ref_profile=="D","Defensive",
                      ifelse(ref_profile =="VD","Very Defensive",
                             ifelse(ref_profile =="B","Balanced",
                                    ifelse(ref_profile =="DY","Dynamic",
                                           ifelse(ref_profile =="VDY","Very Dynamic",""
                                           )))))



  if(length(names(df)[names(df)=="Market_ccy"])==0)
  {
    #names(df)[names(df)=="CRNCY"]<-"Market_ccy"
  }

  hedging_policy<-hedging_policy[hedging_policy$Crncy == pf_ref_crncy & hedging_policy$Profile == ref_profile,]

  ss<-df%>%group_by(AssetClass,SubClass,CRNCY)%>%summarise(sumweight=sum(TradeSimulation))
  ss$SubClass<-toupper(ss$SubClass)
  hedging_policy_sub$SubClass<-toupper(hedging_policy_sub$SubClass)
  hedging_policy_sub$HedgingCategory<-toupper(hedging_policy_sub$HedgingCategory)

  Bonds_EUR<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("Bonds_EUR")])
  RiskAssets_EUR<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("RiskAssets_EUR")])
  RealEstate_EUR<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("RealEstate_EUR")])


  #Hedge Ratio Calculation
  Bonds_USD<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("Bonds_USD")])
  RiskAssets_USD<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("RiskAssets_USD")])
  RiskAssets_Others<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("RiskAssets_Others")])
  RealEstate_Others<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("RealEstate_Others")])
  RealEstate_USD<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("RealEstate_USD")])
  Commodities<-toupper(hedging_policy_sub$SubClass[hedging_policy_sub$HedgingCategory==toupper("Commodities")])


  RiskAssets_EURUSD<-"ALTERNATIVE HE"


  #EUR
  eur<-
    sum(ss$sumweight[ss$AssetClass%in%c(Bonds_EUR) & ss$CRNCY %in%c("EUR")])*as.numeric(hedging_policy$Bonds_EUR)+
    sum(ss$sumweight[ss$SubClass%in%c(RiskAssets_EUR)])*as.numeric(hedging_policy$RiskAssets_EUR)+
    sum(ss$sumweight[ss$SubClass%in%c(RealEstate_EUR)])*as.numeric(hedging_policy$RealEstate_EUR)

  #USD
  usd<-
    sum(ss$sumweight[ss$AssetClass%in%c(Bonds_USD) & ss$CRNCY %in%c("USD")])*as.numeric(hedging_policy$Bonds_USD)+
    sum(ss$sumweight[ss$SubClass%in%c(RiskAssets_USD)])*as.numeric(hedging_policy$RiskAssets_USD)+
    sum(ss$sumweight[ss$SubClass%in%c(RiskAssets_Others)])*as.numeric(hedging_policy$RiskAssets_Others)+
    sum(ss$sumweight[ss$SubClass%in%c(RealEstate_USD)])*as.numeric(hedging_policy$RealEstate_USD)

  chf<-0


  #Hedging Overview
  df$hedge_group<-NA
  df$hedge_ratio<-NA
  df$hedge_currency<-NA
  #Reference Currency
  df$hedge_group<-ifelse(df$CRNCY==pf_ref_crncy,toupper("Domestic_Investment"),df$hedge_group)
  df$hedge_currency<-ifelse(df$CRNCY==pf_ref_crncy,"None",df$hedge_currency)
  df$hedge_ratio<-ifelse(df$CRNCY==pf_ref_crncy,0,df$hedge_ratio)
  #
  df$hedge_group<-ifelse(df$AssetClass%in%c(Commodities),"Commodities",df$hedge_group )
  df$hedge_currency<-ifelse(df$AssetClass%in%c(Commodities),"None",df$hedge_group )
  df$hedge_ratio<-ifelse(df$AssetClass%in%Commodities,as.numeric(hedging_policy$Commodities),df$hedge_ratio)
  #EUR
  df$hedge_group<-ifelse(df$AssetClass%in%c(Bonds_EUR) & df$CRNCY %in%c("EUR"),toupper("Bonds_EUR"),df$hedge_group )
  df$hedge_currency<-ifelse(df$AssetClass%in%c(Bonds_EUR) & df$CRNCY %in%c("EUR"),"EUR",df$hedge_currency )
  df$hedge_ratio<-ifelse(df$AssetClass%in%c(Bonds_EUR) & df$CRNCY %in%c("EUR"),as.numeric(hedging_policy$Bonds_EUR),df$hedge_ratio )
  df$hedge_group<-ifelse(df$SubClass%in%c(RiskAssets_EUR),toupper("RiskAssets_EUR"),df$hedge_group )
  df$hedge_currency<-ifelse(df$SubClass%in%c(RiskAssets_EUR),"EUR",df$hedge_currency )
  df$hedge_ratio<-ifelse(df$SubClass%in%c(RiskAssets_EUR),as.numeric(hedging_policy$RiskAssets_EUR),df$hedge_ratio )
  df$hedge_group<-ifelse(df$SubClass%in%c(RealEstate_EUR),toupper("RealEstate_EUR"),df$hedge_group )
  df$hedge_currency<-ifelse(df$SubClass%in%c(RealEstate_EUR),"EUR",df$hedge_currency )
  df$hedge_ratio<-ifelse(df$SubClass%in%c(RealEstate_EUR),as.numeric(hedging_policy$RealEstate_EUR),df$hedge_ratio )
  #USD
  df$hedge_group<-ifelse(df$AssetClass%in%c(Bonds_USD) & df$CRNCY %in%c("USD"),toupper("Bonds_USD"),df$hedge_group )
  df$hedge_currency<-ifelse(df$AssetClass%in%c(Bonds_USD) & df$CRNCY %in%c("USD"),"USD",df$hedge_currency )
  df$hedge_ratio<-ifelse(df$AssetClass%in%c(Bonds_USD) & df$CRNCY %in%c("USD"),as.numeric(hedging_policy$Bonds_USD),df$hedge_ratio )
  df$hedge_group<-ifelse(df$SubClass%in%c(RiskAssets_USD),toupper("RiskAssets_USD"),df$hedge_group )
  df$hedge_currency<-ifelse(df$SubClass%in%c(RiskAssets_USD),"USD",df$hedge_currency )
  df$hedge_ratio<-ifelse(df$SubClass%in%c(RiskAssets_USD),as.numeric(hedging_policy$RiskAssets_USD),df$hedge_ratio )
  df$hedge_group<-ifelse(df$SubClass%in%c(RiskAssets_Others),toupper("RiskAssets_Others"),df$hedge_group )
  df$hedge_currency<-ifelse(df$SubClass%in%c(RiskAssets_Others),"USD",df$hedge_currency )
  df$hedge_ratio<-ifelse(df$SubClass%in%c(RiskAssets_Others),as.numeric(hedging_policy$RiskAssets_Others),df$hedge_ratio )
  df$hedge_group<-ifelse(df$SubClass%in%c(RealEstate_USD),"RealEstate_USD",df$hedge_group )
  df$hedge_currency<-ifelse(df$SubClass%in%c(RealEstate_USD),"USD",df$hedge_currency )
  df$hedge_ratio<-ifelse(df$SubClass%in%c(RealEstate_USD),as.numeric(hedging_policy$RealEstate_USD),df$hedge_ratio )
  #Cash
  df$hedge_group<-ifelse(df$AssetClass=="LIQUIDITY" & df$CRNCY!=pf_ref_crncy,toupper(paste0("Cash_", df$CRNCY)),df$hedge_group)
  df$hedge_currency<-ifelse(df$AssetClass=="LIQUIDITY" & df$CRNCY!=pf_ref_crncy,df$CRNCY,df$hedge_currency)
  df$hedge_ratio<-ifelse(df$AssetClass=="LIQUIDITY" & df$CRNCY!=pf_ref_crncy,1,df$hedge_ratio)


  #Hedge 50/50 EUR USD
  if(nrow(df[df$SubClass %in% RiskAssets_EURUSD,])>0)
  {

    dftmp<-df[df$SubClass %in% RiskAssets_EURUSD,]

    showNotification(paste0("Splitting ",nrow(dftmp)," position(s) for partial hedging vs EUR and USD."))
    dftmp$TradeSimulation <-dftmp$TradeSimulation /2
    dftmp$perc_of_portfolio  <-dftmp$perc_of_portfolio  /2
    dftmp$current_net_total_ref_ccy  <-dftmp$current_net_total_ref_ccy  /2
    dftmp$pl_total_value_last_price   <-dftmp$pl_total_value_last_price  /2
    dftmp$quantity   <-dftmp$quantity  /2
    dftmp$TQuantity            <-dftmp$TQuantity           /2
    dftmp$pos_target <-dftmp$pos_target/2
    dftmp$trades <-dftmp$trades /2
    dftmp$trade_value <-dftmp$trade_value/2
    dftmp$trade_value_adj <-dftmp$trade_value_adj/2

    dfeur<-dftmp
    dfusd<-dftmp

    dfeur$hedge_group<-"RiskAssets_EURUSD"
    dfeur$hedge_currency<-"EUR"
    dfeur$hedge_ratio<-as.numeric(hedging_policy$RiskAssets_Others)

    dfusd$hedge_group<-"RiskAssets_EURUSD"
    dfusd$hedge_currency<-"USD"
    dfusd$hedge_ratio<-as.numeric(hedging_policy$RiskAssets_Others)
    dfhedg<-rbind(df[!df$SubClass %in% RiskAssets_EURUSD,],dfeur,dfusd)
  }else{
    dfhedg<-df
  }



  if(pf_ref_crncy=="CHF")
  {
    hedges<-data.frame(
      Asset=c("FORWARD","FORWARD","FORWARD"),
      CRNCY=c("CHF","USD","EUR"),
      pos_target_hedge=c(usd+eur,-usd,-eur)
    )
  }
  if(pf_ref_crncy=="EUR")
  {
    hedges<-data.frame(
      Asset=c("FORWARD","FORWARD","FORWARD"),
      CRNCY=c("CHF","USD","EUR"),
      pos_target_hedge=c(-chf,-usd,usd+chf)
    )
  }
  if(pf_ref_crncy=="USD")
  {
    hedges<-data.frame(
      Asset=c("FORWARD","FORWARD","FORWARD"),
      CRNCY=c("CHF","USD","EUR"),
      pos_target_hedge=c(-chf,eur+chf,-eur)
    )
  }


  ss<-ss[!is.na(ss$AssetClass),]
  if(nrow(ss[ss$AssetClass=="FORWARDS",])>0)
  {
    fw<-ss[ss$AssetClass=="FORWARDS",]
    hedges<-merge(fw,hedges,by.x="CRNCY",by.y="CRNCY",all=T)

    llong <- melt(setDT(hedges), id.vars = c("CRNCY"), variable.name = c("variable"))
    llong<- llong[llong$variable%in%c("sumweight","pos_target_hedge"),]
    llong<-llong[!is.na(llong$value),]
    llong<-llong[order(llong$CRNCY),]
    llong$captions=ifelse(llong$variable=="sumweight","Current","Recommended")
    custom_colors <- c("Current" = "#04103b", "Recommended" = "#99CCFF")
    llong$colors<-ifelse(llong$variable=="sumweight","#04103b","#99CCFF")
    custom_colors<-c(rep("#04103b",nrow(llong)/2),rep("#99CCFF",nrow(llong)/2))
    #llong<<-llong
    fig<-
      plot_ly(llong,x=~CRNCY,y=~as.numeric(value),text = ~paste0(round(as.numeric(value),4)*100,"%"),name=~captions, height = chart_height,type = "bar",marker = list(color = c(custom_colors)))%>%
      layout(showlegend = F,title=paste0("FX Hedges (ex Liquidity)"),xaxis=list(title=""),yaxis = list(title = '',tickformat =".1%"))


    #fig <- plot_ly(x = hedges$CRNCY,y = hedges$sumweight,text = ~paste0(round(hedges$sumweight,3)*100,"%"),font = list(size = 9,textangle=0),name = "Current", height = chart_height,type = "bar",marker = list(color = '#04103b'))%>%
    #  add_trace(x=hedges$CRNCY,y=hedges$pos_target_hedge,marker=list(color="#99CCFF"),name="Recommended")%>%
    #  layout(title=paste0("FX Hedges (ex Liquidity)"),xaxis = list(title=""), yaxis = list(title="",tickformat =".1%"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
    #fig

    fig_abs <- plot_ly(x = hedges$CRNCY,y = hedges$pos_target_hedge*pf_nav,text = ~paste0(round(hedges$pos_target_hedge*pf_nav/1000,2),"k"),font = list(size = 9,textangle=0),name = "Recommended", height = chart_height,type = "bar",marker = list(color = '#99CCFF'))%>%
      layout(title=paste0("FX Hedges in PF Crncy (ex Liquidity)"),xaxis = list(title=""), yaxis = list(title=""),legend = list(orientation = "h",xanchor = "center",x = 0.5))
    fig_abs

  }else{
    fig <- plot_ly(x = hedges$currrency,y = hedges$pos_target_hedge,text = ~paste0(round(hedges$pos_target_hedge,3)*100,"%"),font = list(size = 9,textangle=0),name = "SF Zoo", height = chart_height,type = "bar",marker = list(color = '#99CCFF'))%>%
      layout(title=paste0("Recommended FX Hedges (ex Liquidity)"),xaxis = list(title=""), yaxis = list(title="",tickformat =".1%"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
    fig

    fig_abs <- plot_ly(x = hedges$currrency,y = hedges$pos_target_hedge*pf_nav,text = ~paste0(round(hedges$pos_target_hedge*pf_nav/1000,2),"k"),font = list(size = 9,textangle=0),name = "SF Zoo", height = chart_height,type = "bar",marker = list(color = '#99CCFF'))%>%
      layout(title=paste0("Recommended FX Hedges in PF Crncy (ex Liquidity)"),xaxis = list(title=""), yaxis = list(title=""),legend = list(orientation = "h",xanchor = "center",x = 0.5))
    fig_abs
  }

  dfadjustmentstgR<-function(df)
  {
    df<-merge(df,hedges[,c("Asset","CRNCY","pos_target_hedge")],by.x=c("Asset","CRNCY"),by.y=c("Asset","CRNCY"),all=T)
    df$pos_target<-ifelse(!is.na(df$pos_target_hedge) & duplicated(paste0(df$Asset,df$CRNCY))==F,df$pos_target_hedge,df$pos_target)


    df$pos_target_hedge<-ifelse(duplicated(paste0(df$AssetClass,df$CRNCY,df$isin_or_account))==T & df$AssetClass=="FORWARDS",NA,df$pos_target_hedge)
    df$pos_target<-ifelse(duplicated(paste0(df$AssetClass,df$CRNCY,df$isin_or_account))==T & df$AssetClass=="FORWARDS",NA,df$pos_target)

    #df<-merge(df,ss[!is.na(ss$hedge_group),c("AssetClass","SubClass","CRNCY","hedge_group","hedge_ratio")],by=c("AssetClass","SubClass","CRNCY"),all.x=T)


    df<-subset(df,select=c(-pos_target_hedge))
    df<-df[order(df$AssetClass,df$SubClass),]
    return(df)
  }
  df<-dfadjustments(df)
  dfhedg<-dfadjustments(dfhedg)

  rlist<-list("df"=df,"dfhedg"=dfhedg,"fig"=fig,"fig_abs"=fig_abs,"hedges"=hedges)
  return(rlist)
}


rhandstradesimulationtgR<-function(df,sort_vector)
{



  jsCode <- JS(
    "function(el, x) {",
    "  Handsontable.hooks.add('afterSelectionEnd', function(r, c, r2, c2) {",
    "    Shiny.setInputValue('selectedCellRange', {r: r, c: c, r2: r2, c2: c2}, {priority: 'event'});",
    "  });",
    "}"
  )

  row_highlight<-which(duplicated(df$isin_or_account)==TRUE & !is.na(df$isin_or_account) & df$isin_or_account !="")-1
  row_even<- which(even_rows <- seq_len(nrow(df)) %% 2 == 0)-1
  #rhot<-hottooltip(df)

  sort_order<-gsub("-","",sort_vector)
  asc_cols<-sort_vector[grepl("-",sort_vector)==F]
  desc_cols<-sort_vector[grepl("-",sort_vector)==T]
  desc_cols<-gsub("-","",desc_cols)
  # Build arrange() expression
  arrange_args <- lapply(sort_order, function(col) {
    col_sym <- sym(col)
    if (col %in% desc_cols) {
      call("desc", col_sym)   # creates a call like desc(D)
    } else {
      col_sym                 # just A, B, etc.
    }
  })

  # Apply sort
  df <- df %>% arrange(!!!arrange_args)

  rhot<-rhandsontable(df, row_highlight = row_highlight,stretchV = "all")%>%
    hot_col("AssetClass", colWidths = 100)%>%
    hot_col("SubClass", colWidths = 120)%>%
    hot_col("Asset", colWidths = 180)%>%
    hot_col("Symbol", colWidths = 100)%>%
    hot_col("quantity", colWidths = 90, type = "numeric", format = "0,0.000")%>%
    hot_col("CRNCY", colWidths = 40)%>%
    hot_col("PriceLC", colWidths = 70, type = "numeric", format = "0,0.000")%>%
    hot_col("isin_or_account", colWidths = 100)%>%
    hot_col("cprice", colWidths = 50, type = "numeric", format = "0,0.000")%>%
    hot_col("current_net_total_ref_ccy", type = "numeric", format = "0,0.000", colWidths = 80)%>%
    hot_col("pl_total_value_last_price", type = "numeric", format = "0,0.000", colWidths = 80)%>%
    hot_col("perc_of_portfolio", format = "0.000%", colWidths = 80)%>%
    hot_col("pos_target", format = "0.000%")%>%
    hot_col("trade_value", type = "numeric", format = "0,0.000")%>%
    hot_col("trade_value_adj", type = "numeric", format = "0,0.000")%>%
    hot_col("trades", format = "0.000%")%>%
    hot_col("TradeSimulation", format = "0.000%", colWidths = 70)%>%
    hot_cols(wordWrap = FALSE)%>%
    hot_table(highlightCol = TRUE)%>%
    onRender(jsCode)%>%
    hot_cols(manualColumnMove = FALSE,
             manualColumnResize = TRUE
    )%>%
    hot_cell(1, 1, "Shows whether the asset is included in the selected Model Portfolio")%>%
    hot_cell(1, 2, "Shows the Asset Class according to Amadeus' methodology")%>%
    hot_cell(1, 3, "Shows the Sub Asset Class according to Amadeus' methodology")%>%
    hot_cell(1, 4, "Shows the Asset's name as saved in the PMS")%>%
    hot_cell(1, 5, "Shows the ISIN or other symbol in cash of instruments without ISIN as saved in the PMS")%>%
    hot_cell(1, 17, "Shows the price of the traded asset in local currency")%>%
    hot_cell(1, 18, "Shows the price of the portfolio reference currency in the asset's currency (Exchange Rate)")%>%
    hot_cell(1, 19, "Shows the number of shares to be traded")

  row_breaks <- which(df$AssetClass[-1] != df$AssetClass[-nrow(df)])  # Row numbers where `B` changes
  row_grey <- seq(1, nrow(df), by = 2)  # Every second row (odd-indexed) gets grey background
  for(ixy in 3:8)
  {
    rhot <- rhot %>%
      hot_col(col = ixy, renderer = paste0("
                  function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);

                    // Align numbers to the right
                    if (typeof value === 'number') {
                      td.style.textAlign = 'right';
                    }

                    // Apply grey background to every second row
                    if ([", paste0(row_grey, collapse = ","), "].includes(row)) {
                      td.style.backgroundColor = '#FAFAFA';
                    }

                    // Apply top border when column B changes
                    if ([", paste0(row_breaks, collapse = ","), "].includes(row)) {
                      td.style.borderTop = '1px solid #969696';
                    }
                  }
                "))
  }

  rhot<-rhot%>%
    hot_col(col = 6, renderer = paste0("
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);

          if (typeof value === 'number') {
            td.style.textAlign = 'right';  // Align numbers to the right
          }

          if ([", paste0(row_highlight, collapse = ","), "].includes(row) && col === 5) {
            td.style.backgroundColor = '#D1E2EC';  // Blue background for highlighted rows
          } else if ([", paste0(row_even, collapse = ","), "].includes(row) && col === 5) {
            td.style.backgroundColor = '#FAFAFA';  // Grey background for even rows
          }

          // Apply top border when column B changes
          if ([", paste0(row_breaks, collapse = ","), "].includes(row)) {
            td.style.borderTop = '1px solid #969696';
          }
        }
      ")
    )


  # Precalculate deviations in R
  library(jsonlite)
  highlightthreshold <- 0.02
  highlightthreshold2 <- 0.05
  threshold_js <- format(highlightthreshold, scientific = FALSE)
  threshold_js2 <- format(highlightthreshold2, scientific = FALSE)
  deviation_vector <- abs(df$current_net_total_ref_ccy - df$pl_total_value_last_price) / abs(df$current_net_total_ref_ccy)
  deviation_vector2 <- abs(df$current_net_total_ref_ccy - df$pl_total_value_last_price) / abs(df$current_net_total_ref_ccy)
  deviation_json <- jsonlite::toJSON(deviation_vector, auto_unbox = TRUE)
  deviation_json2 <- jsonlite::toJSON(deviation_vector2, auto_unbox = TRUE)
  try(detach("package:jsonlite", unload=TRUE),silent=T)

  rhot<-rhot %>%
    hot_col("Symbol", renderer = sprintf("
    function(instance, td, row, col, prop, value, cellProperties) {
      const deviations = %s;
      const threshold1 = %s;
      const threshold2 = %s;

      if (deviations[row] > threshold2) {
        td.style.background = 'lightred';
      } else if (deviations[row] > threshold1) {
        td.style.background = 'lightorange';
      }

      Handsontable.renderers.TextRenderer.apply(this, arguments);
      return td;
    }
  ", deviation_json, threshold_js, threshold_js2))

  tryCatch({
    rhot<-rhot%>%
      hot_col("WBenchmark", format = "0.000%")%>%
      hot_col("WBenchmarkAdj", format = "0.000%")%>%
      hot_cell(1, 18, "Shows the weight of each asset in the Model Portfolio")%>%
      hot_cell(1, 19, "Shows the price of the traded asset in local currency")%>%
      hot_cell(1, 20, "Shows the price of the portfolio reference currency in the asset's currency (Exchange Rate)")%>%
      hot_cell(1, 21, "Shows the number of shares to be traded")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})


  return(rhot)
}

hedge_table_overviewtgR<-function(rlist_rebalancing,df,collapser="Hedge Currency -> Asset",pf_nav,hedging_excluder,underlying_asset="Yes")
{

  #df<-df[,c("hedge_group","hedge_currency","CRNCY","AssetClass","SubClass","Asset","isin_or_account","Symbol","current_net_total_ref_ccy","perc_of_portfolio","TradeSimulation","hedge_ratio")]

  df$hedge_pre_trade<-df$perc_of_portfolio*df$hedge_ratio*pf_nav
  df$hedge_post_trade<-df$TradeSimulation*df$hedge_ratio*pf_nav

  if(collapser=="Hedge Group -> Hedge Currency")
  {
    df1<-df%>%group_by(hedge_group,hedge_currency)%>%summarise(CRNCY="",AssetClass="",SubClass=head(SubClass,1),Asset="",isin_or_account="",Symbol="",pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),TradeSimulation=sum(TradeSimulation,na.rm=T),hedge_ratio=tail(hedge_ratio,1),hedge_pre_trade=sum(hedge_pre_trade,na.rm=T),hedge_post_trade=sum(hedge_post_trade,na.rm=T))
    df1<-as.data.table(df1)[order(hedge_group,hedge_currency,SubClass,-perc_of_portfolio)]
  }
  if(collapser=="Hedge Currency")
  {
    df1<-df%>%group_by(hedge_currency)%>%summarise(hedge_group="",CRNCY="",AssetClass=head(AssetClass,1),SubClass=head(SubClass,1),Asset="",isin_or_account="",Symbol="",pl_total_value_last_price=sum(pl_total_value_last_price),perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),TradeSimulation=sum(TradeSimulation,na.rm=T),hedge_ratio=tail(hedge_ratio,1),hedge_pre_trade=sum(hedge_pre_trade,na.rm=T),hedge_post_trade=sum(hedge_post_trade,na.rm=T))
    df1<-as.data.table(df1)[order(hedge_currency,AssetClass,-perc_of_portfolio)]
  }
  if(collapser=="Hedge Currency -> SubClass")
  {
    df1<-df%>%group_by(hedge_currency,hedge_group,AssetClass,SubClass)%>%summarise(CRNCY="",AssetClass=head(AssetClass,1),SubClass=head(SubClass,1),Asset="",isin_or_account="",Symbol="",pl_total_value_last_price=sum(pl_total_value_last_price),perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),TradeSimulation=sum(TradeSimulation,na.rm=T),hedge_ratio=tail(hedge_ratio,1),hedge_pre_trade=sum(hedge_pre_trade,na.rm=T),hedge_post_trade=sum(hedge_post_trade,na.rm=T))
    df1<-as.data.table(df1)[order(hedge_currency,SubClass,-perc_of_portfolio)]
  }
  if(collapser=="AssetClass -> Hedge Currency")
  {
    df1<-df%>%group_by(AssetClass,SubClass,hedge_currency,hedge_group)%>%summarise(CRNCY="",AssetClass=head(AssetClass,1),SubClass=head(SubClass,1),Asset="",isin_or_account="",Symbol="",pl_total_value_last_price=sum(pl_total_value_last_price),perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),TradeSimulation=sum(TradeSimulation,na.rm=T),hedge_ratio=tail(hedge_ratio,1),hedge_pre_trade=sum(hedge_pre_trade,na.rm=T),hedge_post_trade=sum(hedge_post_trade,na.rm=T))
    df1<-as.data.table(df1)[order(AssetClass,hedge_currency,hedge_group,SubClass,-perc_of_portfolio)]
  }
  if(collapser=="Hedge Currency -> Asset")
  {
    df1<-df%>%group_by(hedge_currency,hedge_group,Asset,isin_or_account,Symbol)%>%summarise(AssetClass=head(AssetClass,1),SubClass=head(SubClass,1),CRNCY="",pl_total_value_last_price=sum(pl_total_value_last_price),perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),TradeSimulation=sum(TradeSimulation,na.rm=T),hedge_ratio=tail(hedge_ratio,1),hedge_pre_trade=sum(hedge_pre_trade,na.rm=T),hedge_post_trade=sum(hedge_post_trade,na.rm=T))
    df1<-as.data.table(df1)[order(hedge_currency,hedge_group,-perc_of_portfolio)]
  }

  if(underlying_asset=="Yes")
  {
    df$underlying_asset<-ifelse(grepl("OTHERS",toupper(df$hedge_group))==T,"Others",df$hedge_currency)
    sums<-df[!df$AssetClass%in% hedging_excluder,] %>%group_by(hedge_currency)%>%summarise(hedge_pre_trade=sum(hedge_pre_trade,na.rm=T),hedge_post_trade=sum(hedge_post_trade,na.rm=T))
    sucs<-df[!df$AssetClass%in% hedging_excluder,] %>%group_by(hedge_currency,underlying_asset)%>%summarise(pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),TradeSimulation=sum(TradeSimulation,na.rm=T))
    sucs$hedge_currency<-ifelse(toupper(sucs$underlying_asset)==toupper("Others"),sucs$underlying_asset,sucs$hedge_currency)
    sums<-merge(sums,sucs,by="hedge_currency",all.y=T)
    sums$hedge_pre_trade<-ifelse(is.na(sums$hedge_pre_trade),0,sums$hedge_pre_trade)
    sums$hedge_post_trade<-ifelse(is.na(sums$hedge_post_trade),0,sums$hedge_post_trade)
  }else{
    sums<-df[!df$AssetClass%in% hedging_excluder,] %>%group_by(hedge_currency)%>%summarise(pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),TradeSimulation=sum(TradeSimulation,na.rm=T),hedge_pre_trade=sum(hedge_pre_trade,na.rm=T),hedge_post_trade=sum(hedge_post_trade,na.rm=T))
  }


  sums$avg_hr_pre<-sums$hedge_pre_trade/(pf_nav*sums$perc_of_portfolio)
  sums$avg_hr_post<-sums$hedge_pre_trade/(pf_nav*sums$TradeSimulation)
  sums$hedge_currency<-ifelse(is.na(sums$hedge_currency),"Not Classified",sums$hedge_currency)

  consider_futures<-"No"
  #FX Hedges
  if(consider_futures=="Yes")
  {
    #Forwardss
    hsubset<-rlist_rebalancing$df[( toupper(rlist_rebalancing$df$AssetClass) %in% "FORWARDS" & !rlist_rebalancing$df$CRNCY == rlist_rebalancing$df$portfolio_ref_ccy[1])]

    #Use ifelse and load futures mapping if required
    #then rbind with forwards
    hsubset$CRNCY<-ifelse(grepl("CHF/USD",hsubset$Asset)==T,"USD",hsubset$CRNCY)
    hsubset$pl_total_value_last_price<-ifelse(grepl("CHF/USD",hsubsett$Asset)==T,hsubset$pl_total_value_last_price*-1,hsubset$pl_total_value_last_price)
    hsubset$perc_of_portfolio<-ifelse(grepl("CHF/USD",hsubset$Asset)==T,hsubset$perc_of_portfolio*-1,hsubset$perc_of_portfolio)

    hsubset$CRNCY<-ifelse(grepl("EUR/CHF",hsubset$Asset)==T,"EUR",hsubset$CRNCY)
    hsubset$pl_total_value_last_price<-ifelse(grepl("EUR/CHF",hsubsett$Asset)==T,hsubset$pl_total_value_last_price*-1,hsubset$pl_total_value_last_price)
    hsubset$perc_of_portfolio<-ifelse(grepl("EUR/CHF",hsubset$Asset)==T,hsubset$perc_of_portfolio*-1,hsubset$perc_of_portfolio)

    hsubset %>%group_by(CRNCY)%>%summarise(HedgeAct=sum(pl_total_value_last_price,na.rm=T),HedgeActP=sum(perc_of_portfolio,na.rm=T))
  }

  #Forwards
  sums=merge(sums,rlist_rebalancing$df[toupper(rlist_rebalancing$df$AssetClass) %in%"FORWARDS" & !rlist_rebalancing$df$CRNCY == rlist_rebalancing$df$portfolio_ref_ccy[1],] %>%group_by(CRNCY)%>%summarise(HedgeAct=sum(pl_total_value_last_price,na.rm=T),HedgeActP=sum(perc_of_portfolio,na.rm=T)),
             by.x="hedge_currency",by.y="CRNCY",all.x=T)


  rlist_rebalancing$df

  sums$cpair<-paste0(rlist_rebalancing$df$portfolio_ref_ccy[1],sums$hedge_currency," Curncy")
  sums<-merge(sums,rlist_rebalancing$cry,by="cpair",all.x=T)

  sums<-sums[order(-sums$perc_of_portfolio),]
  sums$hedge_pre_tradeLC<-sums$hedge_pre_trade*sums$cprice
  sums$hedge_pre_tradeP<-sums$hedge_pre_trade/pf_nav
  sums$hedge_post_tradeP<-sums$hedge_post_trade/pf_nav
  sums$hedge_post_tradeLC<-sums$hedge_post_trade*sums$cprice


  sums$HedgeActLC<-sums$HedgeAct*sums$cprice*-1
  sums$HedgeAct<-sums$HedgeAct*-1
  sums$HedgeActP<-sums$HedgeActP*-1

  sums$DeltaHedgeLC<-sums$hedge_post_tradeLC-sums$HedgeActLC
  sums$DeltaHedge<-sums$hedge_post_trade-sums$HedgeAct

  sums$ExpNetAct<-ifelse(is.na(sums$HedgeActP),sums$perc_of_portfolio,sums$perc_of_portfolio-sums$HedgeActP)
  sums$ExpNetPost<-sums$TradeSimulation-sums$hedge_post_tradeP
  sums$ExpNetAct[toupper(sums$hedge_currency)==toupper("Domestic_Investment")]<-sums$ExpNetAct[toupper(sums$hedge_currency)==toupper("Domestic_Investment")]+sum(sums$HedgeActP,na.rm=T)
  sums$ExpNetPost[toupper(sums$hedge_currency)==toupper("Domestic_Investment")]<-sums$ExpNetPost[toupper(sums$hedge_currency)==toupper("Domestic_Investment")]+sum(sums$hedge_post_tradeP,na.rm=T)

  futuresdb<-dbGetQuery(pool, paste0("select * from gaa.gaa_futuresdb"))
  futuresdb<-futuresdb[!is.na(futuresdb$Reference_Currency),]
  futuresdb$cpair<-paste0(futuresdb$Reference_Currency, futuresdb$Hedged_Currency," Curncy")
  sums<-merge(sums,futuresdb,by="cpair",all.x=T)
  sums$cprice_future<-ifelse(sums$Direction==-1,1,sums$cprice)
  sums$QLarge<-floor(sums$hedge_post_tradeLC/(sums$Contract_Size*sums$cprice_future))
  sums$QMini<-floor((sums$hedge_post_tradeLC-(sums$QLarge*sums$Contract_Size*sums$cprice_future))/(sums$Contract_Size_Mini*sums$cprice_future))
  sums$QLarge<-sums$QLarge*sums$Direction
  sums$QMini<-sums$QMini*sums$Direction
  sums$CSize<-gsub("NA","",paste0(format(round(sums$Contract_Size,1), big.mark = ",", scientific = FALSE),"/",format(round(sums$Contract_Size_Mini,1), big.mark = ",", scientific = FALSE)))
  sums<-sums[,c("hedge_currency","cprice","pl_total_value_last_price","perc_of_portfolio","TradeSimulation","HedgeActLC","HedgeAct","HedgeActP","hedge_pre_tradeLC","hedge_pre_trade","hedge_pre_tradeP","hedge_post_tradeLC","hedge_post_trade","hedge_post_tradeP","DeltaHedgeLC","DeltaHedge","ExpNetAct","ExpNetPost",
                "CSize","Crncy_Bought","Direction","QLarge","QMini")]


  colsumfuntgR<-function(x)
  {
    x<-round(sum(x,na.rm=T),2)
    return(x)
  }

  numeric_cols <- sapply(sums, is.numeric)
  sumsa <- apply(sums[, numeric_cols], 2, colsumfun)
  sumsa<-as.data.frame(t(sumsa))
  sumsa$hedge_currency<-"Sum"
  sumsa$cprice<-""
  sumsa$Crncy_Bought<-""
  sumsa$Direction<-""
  sumsa$QLarge<-""
  sumsa$QMini<-""
  sumsa$CSize<-""

  erow<-head(sums,1)
  erow[1,]<-""
  sums<-rbind(sums,erow,sumsa)

  #names(sums)[!names(sums)%in% names(sumsa)]

  hdrr<-rhandsontable(df1)%>%
    hot_cols(wordWrap = FALSE)%>%
    hot_col("SubClass", colWidths = 120)%>%
    hot_col("AssetClass", colWidths = 120)%>%
    hot_col("Asset", colWidths = 120)%>%
    hot_col("CRNCY", colWidths = 80)%>%
    hot_col("isin_or_account", colWidths = 80)%>%
    hot_col("TradeSimulation", type = "numeric", format = "0.000%")%>%
    hot_col("pl_total_value_last_price", type = "numeric", format = "0,0.0", colWidths = 100)%>%
    hot_col("perc_of_portfolio", type = "numeric", format = "0.000%")%>%
    hot_col("hedge_ratio", type = "numeric", format = "0.000%")%>%
    hot_col("hedge_pre_trade", type = "numeric", format = "0,0.0")%>%
    hot_col("hedge_post_trade", type = "numeric", format = "0,0.0")%>%
    hot_cols(manualColumnMove = FALSE,
             manualColumnResize = TRUE
    )

  hdrrs<-rhandsontable(sums)%>%
    hot_cols(wordWrap = FALSE)%>%
    hot_col("pl_total_value_last_price", type = "numeric", format = "0,0.0", colWidths = 100)%>%
    hot_col("perc_of_portfolio", type = "numeric", format = "0.000%", colWidths = 70)%>%
    hot_col("cprice", format = "0,0.000")%>%
    hot_col("TradeSimulation", type = "numeric", format = "0.000%", colWidths = 100)%>%
    hot_col("hedge_pre_tradeP", type = "numeric", format = "0.000%", colWidths = 70)%>%
    hot_col("hedge_post_tradeP", type = "numeric", format = "0.000%", colWidths = 70)%>%
    hot_col("HedgeActP", type = "numeric", format = "0.000%")%>%
    hot_col("HedgeAct", type = "numeric", format = "0,0.0")%>%
    hot_col("HedgeActLC", type = "numeric", format = "0,0.0")%>%
    hot_col("hedge_pre_tradeLC", type = "numeric", format = "0,0.0", colWidths = 100)%>%
    hot_col("hedge_pre_trade", type = "numeric", format = "0,0.0", colWidths = 100)%>%
    hot_col("hedge_post_trade", type = "numeric", format = "0,0.0", colWidths = 100)%>%
    hot_col("hedge_post_tradeLC", type = "numeric", format = "0,0.0", colWidths = 100)%>%
    hot_col("DeltaHedgeLC", type = "numeric", format = "0,0.0")%>%
    hot_col("DeltaHedge", type = "numeric", format = "0,0.0")%>%
    hot_col("ExpNetAct", type = "numeric", format = "0.000%", colWidths = 60)%>%
    hot_col("ExpNetPost", type = "numeric", format = "0.000%", colWidths = 60)%>%
    hot_col("Crncy_Bought", type = "numeric", format = "0,0.0", colWidths = 60)%>%
    hot_col("Direction", type = "numeric", format = "0", colWidths = 60)%>%
    hot_col("CSize", type = "numeric", format = "0,0", colWidths = 80)%>%
    hot_col("QLarge", type = "numeric", format = "0,0")%>%
    hot_col("QMini", type = "numeric", format = "0,0")%>%
    hot_table(fixedColumnsLeft = 1) %>%
    hot_cols(manualColumnMove = FALSE,
             manualColumnResize = TRUE
    )

  rlist<-list("hdrr"=hdrr,"hdrrs"=hdrrs,"df"=df)
  return(rlist)
}


get_last_valid_date <- function() {
  today <- Sys.Date()
  weekday <- weekdays(today)

  if (weekday %in% c("Sunday", "Monday")) {
    # If today is Sunday or Monday, return last Friday
    return(today - as.integer(format(today, "%u")) - 2)
  } else {
    # Otherwise, return yesterday
    return(today - 1)
  }
}


hottooltip_trade_filetgR<-function(dfl,global_wd)
{
  #dfl<-tf_data

  jsCode <- JS(
    "function(el, x) {",
    "  Handsontable.hooks.add('afterSelectionEnd', function(r, c, r2, c2) {",
    "    Shiny.setInputValue('selectedCellRange', {r: r, c: c, r2: r2, c2: c2}, {priority: 'event'});",
    "  });",
    "}"
  )
  library(rhandsontable)
  library(htmltools)


  dfl<-
    browsable(tagList(
      rhandsontable(
        data = dfl,
        rowHeaders = NULL,stretchV = "all",
        # see http://jsfiddle.net/pn3rv48p/ for another example with afterGetColHeader
        afterGetColHeader = htmlwidgets::JS(htmltools::HTML(
          sprintf(
            "
function(i, TH) {
  var titleLookup = %s;
  // destroy previous tippy instance if it exists
  if(TH.hasOwnProperty('_tippy')) {TH._tippy.destroy()}
  // initialize tooltip and set content to description from our titleLookup
  tippy(TH, {
    content: titleLookup[i].desc,
  });
}
",
# use column information from ?mtcars
# sprintf will place this json array of objects in our script above at %s
jsonlite::toJSON(
  read.delim(
    textConnection('
[, 1]	mpg	Miles/(US) gallon
[, 2]	cyl	Number of cylinders
[, 3]	disp	Displacement (cu.in.)
[, 4]	hp	Gross horsepower
[, 5]	drat	Rear axle ratio
[, 6]	wt	Weight (1000 lbs)
[, 7]	qsec	1/4 mile time
[, 8]	vs	Engine (0 = V-shaped, 1 = straight)
[, 9]	am	Transmission (0 = automatic, 1 = manual)
[,10]	gear	Number of forward gears
[,11]	carb	Number of carburetors
[,12]	carb	Number of carburetors
[,13]	carb	Number of carburetors
[,14]	carb	Number of carburetors
[,15]	carb	Number of carburetors
[,16]	carb	Number of carburetors
[,17]	carb	Number of carburetors
[,18]	carb	Number of carburetors
'
    ),
    header = FALSE,
    col.names = c("loc","id","desc"),
    stringsAsFactors = FALSE
  ),
  auto_unbox = TRUE
)
          )
        ))
      )%>%
  hot_col("perc_of_portfolio", format = "0.000%", colWidths = 80)%>%
  hot_col("pos_target", format = "0.000%")%>%
  hot_col("trades", format = "0.000%")%>%
  hot_col("TradeSimulation", format = "0.000%")%>%
  hot_col("Portfolio", colWidths = 80)%>%
  hot_col("SubClass", colWidths = 120)%>%
  hot_col("AssetClass", colWidths = 120)%>%
  hot_col("Asset", colWidths = 150)%>%
  hot_cols(wordWrap = FALSE)%>%
  onRender(jsCode)%>%
  hot_cols(readOnly = TRUE)%>%hot_cols(manualColumnMove = FALSE,manualColumnResize = TRUE)


,
# use tippy/bootstrap since Bootstrap 3 tooltips are awful
#   and don't place nicely with handsontable
# better with htmlDependency but this works fine
tags$script(src = "https://unpkg.com/@popperjs/core@2"),
tags$script(src = "https://unpkg.com/tippy.js@6")
#tags$script(src = paste0(global_wd,"www/","popper.min.js")),
#tags$script(src = paste0(global_wd,"www/","tippy-bundle.umd.min.js"))

    ))

  return(dfl)
}



stats_functiontgR<-function(handsontable_trade_simulation,selectedCellRange)
{


  table_data <- hot_to_r(handsontable_trade_simulation)  # Convert rhandsontable to data.frame


  table_data$perc_of_portfolio<-as.numeric(table_data$perc_of_portfolio)
  table_data$pos_target<-as.numeric(table_data$pos_target)
  table_data$trades<-as.numeric(table_data$trades)
  table_data$TradeSimulation<-as.numeric(table_data$TradeSimulation)



  tryCatch({
    stats<-c(
      Sum=sum(table_data$TradeSimulation,na.rm=T),
      Mean=mean(table_data$TradeSimulation,na.rm=T),
      Min=min(table_data$TradeSimulation,na.rm=T),
      Max=max(table_data$TradeSimulation,na.rm=T)
    )


    table_data_red<-table_data[!table_data$AssetClass %in% c("LIQUIDITY","FORWARDS","LIABILITIES","FUTURES","DERIVATIVES"),]


    if("WBenchmark" %in% names(table_data))
    {
      table_data$AssetClassTmp<-ifelse(grepl("GMP",table_data$SubClass)==T,"GMP",table_data$AssetClass)

      stats <- c(
        Sum = round(sum(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4),
        Mean = round(mean(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4),
        Min = round(min(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4),
        Max = round(max(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4),
        SumCol = formatC(round(sum(table_data[(1):(nrow(table_data)),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4), format="f", big.mark=",", digits=3),
        SumColExCashDeriv = formatC(round(sum(table_data_red[(1):(nrow(table_data_red)),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4), format="f", big.mark=",", digits=3),
        SumAC =paste0(formatC(round(sum(table_data[table_data$AssetClass==table_data$AssetClass[selectedCellRange$r+1],(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4), format="f", big.mark=",", digits=3)),
        PercAC=
          paste0(round(
            sum(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T)/
              sum(table_data[table_data$AssetClass==table_data$AssetClass[selectedCellRange$r+1],(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4)*100,"%"),
        WeightsAC =paste0(round(sum(table_data$perc_of_portfolio[table_data$AssetClassTmp==table_data$AssetClassTmp[selectedCellRange$r+1]],na.rm=T)*100,2),"% / ",
                          round(sum(table_data$TradeSimulation[table_data$AssetClassTmp==table_data$AssetClassTmp[selectedCellRange$r+1]],na.rm=T)*100,2),
                          "% / ",round(sum(table_data$WBenchmark[table_data$AssetClassTmp==table_data$AssetClassTmp[selectedCellRange$r+1]],na.rm=T)*100,2),
                          "% / ",round(sum(table_data$WBenchmarkAdj[table_data$AssetClassTmp==table_data$AssetClassTmp[selectedCellRange$r+1]],na.rm=T)*100,2),"%"
        ),
        WeightsSC =paste0(round(sum(table_data$perc_of_portfolio[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1]],na.rm=T)*100,2),"% / ",
                          round(sum(table_data$TradeSimulation[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1]],na.rm=T)*100,2),
                          "% / ",round(sum(table_data$WBenchmark[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1]],na.rm=T)*100,2),
                          "% / ",round(sum(table_data$WBenchmarkAdj[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1]],na.rm=T)*100,2),"%"
        ),
        ActiveWeightAC = paste0("Now: ",paste0(round(sum(table_data$perc_of_portfolio[table_data$AssetClassTmp==table_data$AssetClassTmp[selectedCellRange$r+1]],na.rm=T)*100-
                                                       sum(table_data$WBenchmarkAdj[table_data$AssetClassTmp==table_data$AssetClassTmp[selectedCellRange$r+1]],na.rm=T)*100,2),"%"),
                                " / Sim: ",paste0(round(sum(table_data$TradeSimulation[table_data$AssetClassTmp==table_data$AssetClassTmp[selectedCellRange$r+1]],na.rm=T)*100-
                                                          sum(table_data$WBenchmarkAdj[table_data$AssetClassTmp==table_data$AssetClassTmp[selectedCellRange$r+1]],na.rm=T)*100,2),"%")),
        ActiveWeightSC = paste0("Now: ",paste0(round(sum(table_data$perc_of_portfolio[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1]],na.rm=T)*100-
                                                       sum(table_data$WBenchmarkAdj[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1]],na.rm=T)*100,2),"%"),
                                " / Sim: ",paste0(round(sum(table_data$TradeSimulation[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1]],na.rm=T)*100-
                                                          sum(table_data$WBenchmarkAdj[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1]],na.rm=T)*100,2),"%"))
      )
    }else{
      stats <- c(
        Sum = round(sum(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4),
        Mean = round(mean(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4),
        Min = round(min(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4),
        Max = round(max(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4),
        SumAC =paste0(formatC(round(sum(table_data[table_data$AssetClass==table_data$AssetClass[selectedCellRange$r+1],(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4), format="f", big.mark=",", digits=3)),
        PercAC=
          paste0(round(
            sum(table_data[(selectedCellRange$r+1):(selectedCellRange$r2+1),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T)/
              sum(table_data[table_data$AssetClass==table_data$AssetClass[selectedCellRange$r+1],(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4)*100,"%"),
        SumSC =paste0(formatC(round(sum(table_data[table_data$SubClass==table_data$SubClass[selectedCellRange$r+1],(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4), format="f", big.mark=",", digits=3)),
        SumCol = formatC(round(sum(table_data[(1):(nrow(table_data)),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4), format="f", big.mark=",", digits=3),
        SumColExCashDeriv = formatC(round(sum(table_data_red[(1):(nrow(table_data_red)),(selectedCellRange$c+1):(selectedCellRange$c2+1)],na.rm=T),4), format="f", big.mark=",", digits=3)
      )
    }





  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  return(stats)
}



remove_duplicates_from_tabletgR<-function(df,sumby="Sum by ISIN")
{

  if(sumby=="Sum by ISIN" | sumby=="Sum ex Cash & Derivatives")
  {
    df<-df %>%group_by(isin_or_account)%>%summarise(crytr=head(crytr,1),
                                                    portfolio_ref_ccy=head(portfolio_ref_ccy,1),
                                                    Symbol=head(Symbol,1),
                                                    position_type=head(position_type,1),
                                                    AssetClass=head(AssetClass,1),
                                                    SubClass=head(SubClass,1),
                                                    Asset=head(Asset,1),
                                                    Market_ccy=head(Market_ccy,1),
                                                    CRNCY=head(CRNCY,1),
                                                    quantity=sum(quantity,na.rm=T),
                                                    current_net_total_ref_ccy=sum(current_net_total_ref_ccy,na.rm=T),
                                                    current_price_market_ccy=head(current_price_market_ccy,1),
                                                    LAST_PRICE=head(LAST_PRICE,1),
                                                    perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),
                                                    cprice=head(cprice,1),
                                                    pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),
                                                    pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),
                                                    perc_of_portfolio_md=sum(perc_of_portfolio_md,na.rm=T),
                                                    pos_target=NA)
  }

  if(sumby=="Sum AC by ISIN" | sumby=="Sum AC ex Cash & Derivatives")
  {
    df<-df %>%group_by(AssetClass,isin_or_account)%>%summarise(crytr=head(crytr,1),
                                                    portfolio_ref_ccy=head(portfolio_ref_ccy,1),
                                                    Symbol=head(Symbol,1),
                                                    position_type=head(position_type,1),
                                                    #AssetClass=head(AssetClass,1),
                                                    SubClass=head(SubClass,1),
                                                    Asset=head(Asset,1),
                                                    Market_ccy=head(Market_ccy,1),
                                                    CRNCY=head(CRNCY,1),
                                                    quantity=sum(quantity,na.rm=T),
                                                    current_net_total_ref_ccy=sum(current_net_total_ref_ccy,na.rm=T),
                                                    current_price_market_ccy=head(current_price_market_ccy,1),
                                                    LAST_PRICE=head(LAST_PRICE,1),
                                                    perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),
                                                    cprice=head(cprice,1),
                                                    pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),
                                                    pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),
                                                    perc_of_portfolio_md=sum(perc_of_portfolio_md,na.rm=T),
                                                    pos_target=NA)
  }

  if(sumby=="Sum ex Cash & Derivatives" | sumby=="Sum ex ... by ISIN & Crncy")
  {
    df<-df %>%group_by(isin_or_account,CRNCY)%>%summarise(crytr=head(crytr,1),
                                                          portfolio_ref_ccy=head(portfolio_ref_ccy,1),
                                                          Symbol=head(Symbol,1),
                                                          position_type=head(position_type,1),
                                                          AssetClass=head(AssetClass,1),
                                                          SubClass=head(SubClass,1),
                                                          Asset=head(Asset,1),
                                                          Market_ccy=head(Market_ccy,1),
                                                          #CRNCY=head(CRNCY,1),
                                                          quantity=sum(quantity,na.rm=T),
                                                          current_net_total_ref_ccy=sum(current_net_total_ref_ccy,na.rm=T),
                                                          current_price_market_ccy=head(current_price_market_ccy,1),
                                                          LAST_PRICE=head(LAST_PRICE,1),
                                                          perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),
                                                          cprice=head(cprice,1),
                                                          pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),
                                                          pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),
                                                          perc_of_portfolio_md=sum(perc_of_portfolio_md,na.rm=T),
                                                          pos_target=NA)
  }

  if(sumby=="Sum AC ex Cash & Derivatives" | sumby=="Sum AC ex ... by ISIN & Crncy")
  {
    df<-df %>%group_by(AssetClass,isin_or_account,CRNCY)%>%summarise(crytr=head(crytr,1),
                                                          portfolio_ref_ccy=head(portfolio_ref_ccy,1),
                                                          Symbol=head(Symbol,1),
                                                          position_type=head(position_type,1),
                                                          #AssetClass=head(AssetClass,1),
                                                          SubClass=head(SubClass,1),
                                                          Asset=head(Asset,1),
                                                          Market_ccy=head(Market_ccy,1),
                                                          #CRNCY=head(CRNCY,1),
                                                          quantity=sum(quantity,na.rm=T),
                                                          current_net_total_ref_ccy=sum(current_net_total_ref_ccy,na.rm=T),
                                                          current_price_market_ccy=head(current_price_market_ccy,1),
                                                          LAST_PRICE=head(LAST_PRICE,1),
                                                          perc_of_portfolio=sum(perc_of_portfolio,na.rm=T),
                                                          cprice=head(cprice,1),
                                                          pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),
                                                          pl_total_value_last_price=sum(pl_total_value_last_price,na.rm=T),
                                                          perc_of_portfolio_md=sum(perc_of_portfolio_md,na.rm=T),
                                                          pos_target=NA)
  }

  return(df)
}



export_aam_etf_listtgR<-function(aam_file="AAM_GUI_2025",by_pf="Yes")
{
  wl<-dbGetQuery(pool,paste0("select * from gaa.gaa_allocations where AAM_File = '",aam_file,"'"))

  if(by_pf=="By Portfolio")
  {
    wl<-wl[,c("AAM_File","Portfolio","ISIN","AssetClass","SubClass","Currency","AssetName")]
  }else{
    wl<-wl[,c("AAM_File","ISIN","AssetClass","SubClass","Currency","AssetName")]
  }

  wl<-unique(wl)
  row.names(wl)<-index(wl)
  wl<-wl[!wl$AssetClass %in%c("Cash","FXHedge"),]
  wl<-wl[!is.na(wl$ISIN),]
  wl<-wl[order(wl$ISIN),]

  return(wl)
}


export_aam_portfoliotgR<-function(aam_file="AAM_GUI_2025",aam_portfolio="CHF_B")
{
  showNotification(paste("Preparing data..."), duration = 3,type="warning")
  wl<-dbGetQuery(pool,paste0("select * from gaa.gaa_allocations where AAM_File = '",aam_file,"' and Portfolio = '",aam_portfolio,"'"))
  showNotification(paste("Download is ready"), duration = 3)
  return(wl)
}

export_client_listtgR<-function(available_pf_settings_selected)
{
  showNotification(paste("Preparing data..."), duration = 3,type="warning")
  dfa<-dbGetQuery(pool,paste0("select distinct portfolio_id,portfolio_nr,portfolio_name  from portfolio_presentation.pms_positions"))
  showNotification(paste("Download is ready"), duration = 3)
  return(dfa)
}

export_client_all_portfoliostgR<-function(use_hist_database,holdings_data_as_of)
{
  showNotification(paste("Preparing data..."), duration = 3,type="warning")
  if(use_hist_database=="No")
  {
    dfa<-dbGetQuery(pool,"select * from portfolio_presentation.pms_positions")
  }else{
    dfa<-dbGetQuery(pool,paste0("select * from portfolio_presentation.pms_positions_hist where file_production_date = '",holdings_data_as_of,"'"))
  }
  showNotification(paste("Download is ready"), duration = 3)
  return(dfa)
}

export_client_portfoliotgR<-function(use_hist_database,holdings_data_as_of,selected_portfolio)
{
  showNotification(paste("Preparing data..."), duration = 3,type="warning")
  if(use_hist_database=="No")
  {
    dfa<-dbGetQuery(pool,paste0("select * from portfolio_presentation.pms_positions where portfolio_name = '",selected_portfolio,"'"))
  }else{
    dfa<-dbGetQuery(pool,paste0("select * from portfolio_presentation.pms_positions_hist where portfolio_name = '",selected_portfolio,"' and file_production_date = '",holdings_data_as_of,"'"))
  }
  showNotification(paste("Download is ready"), duration = 3)
  return(dfa)
}

as_num_hedgingtgR<-function(hedging_file)
{
  hedging_file$cprice<-as.numeric(hedging_file$cprice)
  hedging_file$pl_total_value_last_price<-as.numeric(hedging_file$pl_total_value_last_price)
  hedging_file$perc_of_portfolio<-as.numeric(hedging_file$perc_of_portfolio)
  hedging_file$TradeSimulation<-as.numeric(hedging_file$TradeSimulation)
  hedging_file$HedgeActLC<-as.numeric(hedging_file$HedgeActLC)
  hedging_file$HedgeAct<-as.numeric(hedging_file$HedgeAct)
  hedging_file$HedgeActP<-as.numeric(hedging_file$HedgeActP)
  hedging_file$hedge_pre_tradeLC<-as.numeric(hedging_file$hedge_pre_tradeLC)
  hedging_file$hedge_pre_trade  <-as.numeric(hedging_file$hedge_pre_trade)
  hedging_file$hedge_post_tradeLC  <-as.numeric(hedging_file$hedge_post_tradeLC)
  hedging_file$hedge_post_trade   <-as.numeric(hedging_file$hedge_post_trade)
  hedging_file$DeltaHedgeLC  <-as.numeric(hedging_file$DeltaHedgeLC)
  hedging_file$hedge_post_tradeP    <-as.numeric(hedging_file$hedge_post_tradeP)
  hedging_file$ExpNetAct     <-as.numeric(hedging_file$ExpNetAct       )
  hedging_file$hedge_pre_tradeP    <-as.numeric(hedging_file$hedge_pre_tradeP)
  hedging_file$ExpNetPost<-as.numeric(hedging_file$ExpNetPost)
  hedging_file$DeltaHedge<-as.numeric(hedging_file$DeltaHedge)


  return(hedging_file)
}


trade_new_assettgR<-function(asset_to_ad,df,rlist_rebalancing)
{

  add<-head(df,1)
  add[1,]<-NA
  add$SubClass<-toupper(head(rlist_rebalancing$sec$Custom_Name[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add$Asset<-toupper(head(rlist_rebalancing$sec$SECURITY_NAME[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add$current_net_total_ref_ccy<-0
  add$perc_of_portfolio<-0
  add$pos_target<-NA
  add$CRNCY<-head(rlist_rebalancing$sec$CRNCY[rlist_rebalancing$sec$ID==asset_to_ad],1)
  add$AssetClass<-toupper(head(rlist_rebalancing$sec$ASSET_CLASS_PF_PRES[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add$SubClass<-toupper(head(rlist_rebalancing$sec$Custom_Name[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add$isin_or_account <-toupper(head(rlist_rebalancing$sec$ID_ISIN[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add$Symbol <-toupper(head(rlist_rebalancing$sec$Symbol[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add$portfolio_ref_ccy <- df$portfolio_ref_ccy[1]
  add$PriceLC<-head(rlist_rebalancing$sec$LAST_PRICE[rlist_rebalancing$sec$ID==asset_to_ad],1)
  add$quantity<-0
  add$pl_total_value_last_price<-0
  add$CRNCY<-toupper(add$CRNCY)

  cpricedf<-rlist_rebalancing$cry[!is.na(rlist_rebalancing$cry$cpair) & rlist_rebalancing$cry$cpair==paste0(rlist_rebalancing$df$portfolio_ref_ccy,add$CRNCY," Curncy"),]
  if(nrow(cpricedf)==0)
  {
    showNotification("Could not add instrument to portfolio. Instrument currency seems to be missing. Please add it to allocation framework. ",type="error")
  }
  cpricedf<-cpricedf[!is.na(cpricedf$cprice),]
  cpricedf<-cpricedf$cprice[!is.na(cpricedf$cprice)]
  add$cprice<-cpricedf

  df<-rbind(df,add)

  df<-df[order(df$AssetClass,df$SubClass),]


  #Add to rlist_rebalancing
  add2<-head(rlist_rebalancing$df,1)
  add2[1,]<-NA
  add2$SubClass<-toupper(head(rlist_rebalancing$sec$Custom_Name[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add2$Asset<-toupper(head(rlist_rebalancing$sec$SECURITY_NAME[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add2$current_net_total_ref_ccy<-0
  add2$perc_of_portfolio<-0
  add2$pos_target<-NA
  add2$CRNCY<-head(rlist_rebalancing$sec$CRNCY[rlist_rebalancing$sec$ID==asset_to_ad],1)
  add2$AssetClass<-toupper(head(rlist_rebalancing$sec$ASSET_CLASS_PF_PRES[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add2$SubClass<-toupper(head(rlist_rebalancing$sec$Custom_Name[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add2$isin_or_account <-toupper(head(rlist_rebalancing$sec$ID_ISIN[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add2$Symbol <-toupper(head(rlist_rebalancing$sec$Symbol[rlist_rebalancing$sec$ID==asset_to_ad],1))
  add2$portfolio_ref_ccy <- rlist_rebalancing$df$portfolio_ref_ccy[1]
  add2$LAST_PRICE<-head(rlist_rebalancing$sec$LAST_PRICE[rlist_rebalancing$sec$ID==asset_to_ad],1)
  add2$quantity<-0
  add2$pl_total_value_last_price<-0
  add2$CRNCY<-toupper(add$CRNCY)
  add2$cprice<-add$cprice
  add2$perc_of_portfolio_md<-0
  add2$current_price_market_ccy<-head(rlist_rebalancing$sec$LAST_PRICE[rlist_rebalancing$sec$ID==asset_to_ad],1)
  add2$Market_ccy<-head(rlist_rebalancing$sec$CRNCY[rlist_rebalancing$sec$ID==asset_to_ad],1)
  crytr<-rlist_rebalancing$cry$cpair[!is.na(rlist_rebalancing$cry$cpair) & rlist_rebalancing$cry$cpair==paste0(rlist_rebalancing$df$portfolio_ref_ccy,add$CRNCY," Curncy")]
  add2$crytr<-crytr[!is.na(crytr)]

  rlist_rebalancing$df<-rbind(rlist_rebalancing$df,add2)

  rlistreturn<-list("rlist_rebalancing"=rlist_rebalancing,df=df)

  return(rlistreturn)
}

custom_mappings_tabletgR<-function(pf_name)
{
  custom_mappings<-data.frame(
    "Portfolio"=pf_name,
    "isin_or_account"="",
    "CRNCY"="",
    "CRNCY_OVERWRITE"="",
    "AssetClass"="",
    "SubClass"="",
    "Datestamp"=Sys.Date()
  )
  return(custom_mappings)
}

save_custom_mappings_functiontgR<-function(custom_mappings,pf_name)
{
  #dbWriteTable(pool,"custom_mappings",custom_mappings,overwrite=T,row.names=F)
  dbGetQuery(pool, paste0("delete from custom_mappings where Portfolio = '",pf_name,"'"))
  custom_mappings$Portfolio<-pf_name
  custom_mappings$Datestamp<-Sys.Date()
  dbWriteTable(pool,"custom_mappings",custom_mappings,append=T,row.names=F)
}

create_post_trade_rhandsontabletgR<-function(pta)
{

  row_breaks <- which(pta$Portfolio[-1] != pta$Portfolio[-nrow(pta)])  # Row numbers where `B` changes
  row_grey <- seq(1, nrow(pta), by = 2)  # Every second row (odd-indexed) gets grey background

  pta<-cbind(subset(pta,select=-post_trade_comment),subset(pta,select=post_trade_comment))
  pta$executed<-as.character(pta$executed)

  pta<-pta[,c("Portfolio","isin_or_account","CRNCY","AssetClass","SubClass","current_status","executed","PreTradeQuantity","TQuantity","PostTradeQuantity","ExpectedQuantity","Diff","DiffQPerc",
              "CheckQ","PreTradePosition","PostTradePosition","PreTradeTargetWeight" ,"PostTradeWeight","DiffW","CheckW","PreTradePrice","PostTradePrice","DiffP","CheckP","PreTradeFX","PostTradeFX","DiffFX","CheckFX","post_trade_comment")]

  rhot<-
    rhandsontable(pta,stretchV = "all")%>%
    hot_cols(wordWrap = FALSE)%>%
    hot_cols(manualColumnMove = FALSE,manualColumnResize = TRUE)%>%
    hot_col("PreTradePosition", type = "numeric", format = "0,0.0")%>%
    hot_col("PostTradePosition", type = "numeric", format = "0,0.0")%>%
    hot_col("PreTradeQuantity", type = "numeric", format = "0,0.0")%>%
    hot_col("TQuantity", type = "numeric", format = "0,0.0")%>%
    hot_col("PostTradeQuantity", type = "numeric", format = "0,0.0")%>%
    hot_col("ExpectedQuantity", type = "numeric", format = "0,0.0")%>%
    hot_col("PreTradePrice", type = "numeric", format = "0,0.0")%>%
    hot_col("PostTradePrice", type = "numeric", format = "0,0.0")%>%
    hot_col("DiffP", format = "0.000%")%>%
    hot_col("Diff", type = "numeric", format = "0,0.0")%>%
    hot_col("DiffFX", format = "0.000%")%>%
    hot_col("CheckFX", type = "numeric", format = "0,0.0")%>%
    hot_col("DiffQPerc", format = "0.000%")%>%
    hot_col("PreTradeTargetWeight", format = "0.000%")%>%
    hot_col("PostTradeWeight", format = "0.000%")%>%
    hot_col("DiffW", format = "0.000%")%>%
    hot_col("post_trade_comment", colWidths = 50,manualColumnResize = TRUE)%>%
    hot_col("CheckQ",
            renderer = "
                  function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (value === 'Please check') {
                      td.style.background = 'red';
                      td.style.color = 'white';
                    } else if (value === 'Warning') {
                      td.style.background = 'yellow';
                      td.style.color = 'black';
                    } else if (value === 'Ok') {
                      td.style.background = 'lightgreen';
                      td.style.color = 'black';
                    } else {
                      td.style.background = '';
                      td.style.color = '';
                    }
                    return td;
                  }
                ")%>%
    hot_col("CheckW",
            renderer = "
                  function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (value === 'Please check') {
                      td.style.background = 'red';
                      td.style.color = 'white';
                    } else if (value === 'Warning') {
                      td.style.background = 'yellow';
                      td.style.color = 'black';
                    } else if (value === 'Ok') {
                      td.style.background = 'lightgreen';
                      td.style.color = 'black';
                    } else {
                      td.style.background = '';
                      td.style.color = '';
                    }
                    return td;
                  }
                ")%>%
    hot_col("CheckP",
            renderer = "
                  function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (value === 'Please check') {
                      td.style.background = 'red';
                      td.style.color = 'white';
                    } else if (value === 'Warning') {
                      td.style.background = 'yellow';
                      td.style.color = 'black';
                    } else if (value === 'Ok') {
                      td.style.background = 'lightgreen';
                      td.style.color = 'black';
                    } else {
                      td.style.background = '';
                      td.style.color = '';
                    }
                    return td;
                  }
                ")%>%
    hot_col("CheckFX",
            renderer = "
                    function(instance, td, row, col, prop, value, cellProperties) {
                      Handsontable.renderers.TextRenderer.apply(this, arguments);
                      if (value === 'Please check') {
                        td.style.background = 'red';
                        td.style.color = 'white';
                      } else if (value === 'Warning') {
                        td.style.background = 'yellow';
                        td.style.color = 'black';
                      } else if (value === 'Ok') {
                        td.style.background = 'lightgreen';
                        td.style.color = 'black';
                      } else {
                        td.style.background = '';
                        td.style.color = '';
                      }
                      return td;
                    }
                  ")


  for(ixy in 1:6)
  {
    rhot <- rhot %>%
      hot_col(col = ixy, renderer = paste0("
                  function(instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);

                    // Align numbers to the right
                    if (typeof value === 'number') {
                      td.style.textAlign = 'right';
                    }

                    // Apply grey background to every second row
                    if ([", paste0(row_grey, collapse = ","), "].includes(row)) {
                      td.style.backgroundColor = '#FAFAFA';
                    }

                    // Apply top border when column B changes
                    if ([", paste0(row_breaks, collapse = ","), "].includes(row)) {
                      td.style.borderTop = '1px solid #969696';
                    }
                  }
                "))
  }
  return(rhot)
}
