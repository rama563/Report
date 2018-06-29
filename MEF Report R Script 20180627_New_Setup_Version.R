#!/usr/bin/env Rscript
################################################################
######## Script to create Monashee Daily Report  ###############
######## Scripted by: Rama Krishna Ventrapragada ###############
################################################################

#### Input files #####
#1. MDD.csv
#2. Daily Trade Blotter.csv
#3. Monashee Positions.csv
#4. Risk Reward Report.csv
######################


#### Output Files ####
#1. Monashee Daily Report Long.csv
#2. Monashee Daily Report Short.csv
#3. Recent Trades_Buy.csv
#4. Recent Trades_Sell.csv
#5. Recent Trades_Short.csv
#6. Recent Trades_Cover.csv
#7. Top_10_Stocks_Exposure.csv
#8. Sector_Exposure.csv
######################


###Clearing the Environment
rm(list=ls())

##AWS
#do.call(file.remove, list(list.files("/home/ghcadmin/M_Daily_Report/Output Files", full.names = TRUE)))

### Loading Required Pakcages
library(stringr)
library(plyr)
library(lubridate)
library(svDialogs)
library(gtools)

### Setting up working directory - Windows
setwd("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup")

##AWS
#setwd("/home/ghcadmin/M_Daily_Report")

### Alerts

#dlgMessage("Make sure the lookups are upto date and input files are copied as per the instructions!!! ", type=c("ok"))

### Reading and preprocessing the input files 

## Monashee Positions
Positions                     <-   read.csv("Input Files/Monashee_Positions.csv",header = T,stringsAsFactors = F)
P_Lkup                        <-   read.csv("Lookup Files/Corrections_Lookup.csv",header = T,stringsAsFactors = F)
#ID3_Lkup                      <-   read.csv("Input Files/ID3_Lookup.csv",header = T,stringsAsFactors = F)
P_Strat_Lkup                  <-   read.csv("Lookup Files/Strategy_Lookup.csv",header = T,stringsAsFactors = F)
#P_Lkup$symbol                 <-   paste0(P_Lkup$symbol," US")

##MMLS Positions
#if(c("MMLS_Positions.csv")%in%list.files("/home/ghcadmin/M_Daily_Report/Input Files"))
if(c("MMLS_Positions.csv")%in%list.files("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Input Files"))
{
  mmls_positions<-read.csv("Input Files/MMLS_Positions.csv",stringsAsFactors = F)
  mmls_positions<-mmls_positions[,-c(2)]
  mmls_positions[,2]<-as.character(mmls_positions[,2])
  mmls_positions[1,2]<-"symbol"
  mmls_positions<-mmls_positions[,-c(1)]
  colnames(mmls_positions)<-as.character(mmls_positions[1,])
  mmls_positions<-mmls_positions[-c(1),]
  mmls_positions$symbol<-paste0(mmls_positions$symbol," US")
  mmls_positions<-mmls_positions[mmls_positions$symbol!=" US",]
  colnames(mmls_positions)[which(names(mmls_positions) == "Long/Short")] <- "longShort"
  colnames(mmls_positions)[which(names(mmls_positions) == "Security Type")] <- "assetType"
  colnames(mmls_positions)[which(names(mmls_positions) == "Country")] <- "country"
  colnames(mmls_positions)[which(names(mmls_positions) == "Region")] <- "region"
  colnames(mmls_positions)[which(names(mmls_positions) == "instrument Description")] <- "instrument.Description"
  mmls_positions<-mmls_positions[colnames(mmls_positions)%in%c("symbol","instrument.Description","exchange","identifier",
                                                               "assetType",
                                                               "qty",
                                                               "priceClose",
                                                               "plDay",
                                                               "LongShort",
                                                               "sub_fund",
                                                               "country",
                                                               "region",
                                                               "Strategy")]
  mmls_positions$qty<-as.numeric(mmls_positions$qty)
  mmls_positions$priceClose<-gsub(",","",mmls_positions$priceClose)
  mmls_positions$priceClose<-as.numeric(mmls_positions$priceClose)
  mmls_positions$plDay<-gsub(",","",mmls_positions$plDay)
  mmls_positions$plDay<-as.numeric(mmls_positions$plDay)
  ###Shradha&Piyush to put this filter
  mmls_positions<-mmls_positions[mmls_positions$country=='UnitedStates',]
  Positions<-smartbind(Positions,mmls_positions)
}


#ID3_Lkup<-as.data.frame(ID3_Lkup[c(-2),])
#names(ID3_Lkup)<-"id_3"

##Updating data based on information from lookup
for(i in 1:nrow(Positions))
{
  for(j in 1:nrow(P_Lkup))
  {
    if(Positions$symbol[i]==P_Lkup$Symbol[j] & Positions$"Strategy"[i]==P_Lkup$"Current.Strategy"[j])
    {
      if(P_Lkup$Correct.Strategy[j]!="")
      {
        Positions$Strategy[i]<- P_Lkup$Correct.Strategy[j]
        print(paste0("Strategy updated for symbol: ",Positions$symbol[i]))
      }
      
    }
    if(Positions$symbol[i]==P_Lkup$Symbol[j] & Positions$assetType[i]==P_Lkup$Current.Asset.Type[j])
    {
      if(P_Lkup$"Correct.Asset.Type"[j]!="")
      {
        Positions$assetType[i]<- P_Lkup$Correct.Asset.Type[j]
        print(paste0("Asset Type updated for symbol: ",Positions$symbol[i]))
      }
    }
    if(Positions$symbol[i]==P_Lkup$Symbol[j])
    {
      if(P_Lkup$Correct.Ticker[j]!="")
      {
        Positions$symbol[i]<- P_Lkup$Correct.Ticker[j]
        print(paste0("Symbol updated for symbol: ",Positions$symbol[i]))
      } 
    }
    
  }
}

### Custom Update - Shradha
Positions$Strategy[Positions$symbol=="RCUS US"&Positions$Strategy=="IPO AM"]<-"FO"


##Getting Curr date
date_curr<-unique(as.character(Positions$Trade_date[1]))
date_curr<-paste0(substr(date_curr,1,4),"-",substr(date_curr,5,6),"-",substr(date_curr,7,8))
date_curr<-as.Date(date_curr)


## Trade Blotter
Trade_Blotter                 <-   read.csv("Input Files/Daily Trade Blotter.csv",header = T,stringsAsFactors = F)
colnames(Trade_Blotter)       <-   as.character(Trade_Blotter[3,])
Trade_Blotter                 <-   Trade_Blotter[c(-1,-2,-3),]
Trade_Blotter                 <-   Trade_Blotter[Trade_Blotter$Desc!="",]
Trade_Blotter$"Symbol US"     <-   paste0(Trade_Blotter$Symbol," US")
Trade_Blotter$Done            <-   as.numeric(Trade_Blotter$Done)
Trade_Blotter$"ID_3"          <-   paste0(Trade_Blotter$`Symbol US`,"_",as.character(date_curr))

## MDD
MDD                           <-   read.csv("Input Files/MDD.csv",header = T,stringsAsFactors = F,check.names = F)
#colnames(MDD)                 <-   as.character(MDD[1,])
#MDD                           <-   MDD[c(-1),]
MDD$`Pricing Date`            <-   as.Date(MDD$`Pricing Date`,"%Y-%m-%d")
MDD                           <-   MDD[order(MDD$`Pricing Date`,decreasing = T),]


## Risk Reward file
Risk_Reward                   <-   read.csv("Input Files/Risk Reward Report.csv",header = T,stringsAsFactors = F)
colnames(Risk_Reward)         <-   as.character(Risk_Reward[1,])
Risk_Reward                   <-   Risk_Reward[c(-1),]
Risk_Reward                   <-   Risk_Reward[Risk_Reward$Desc!="",]
Risk_Reward$"Symbol US"       <-   paste0(Risk_Reward$Symbol," US")


## V5        
V5                            <-   read.csv("Input Files/V5.csv",header = T,stringsAsFactors = F) 
V5$"Strategy"                 <-   P_Strat_Lkup$Strategy_Updated[match(V5$Custom_Grp1,P_Strat_Lkup$Strategy)]
V5$"Temp_Sym_Strat"           <-   paste0(V5$Client_Symbol,V5$Strategy)
V5                            <-   V5[!is.na(V5$Trade_Dt)|V5$Trade_Dt!="",]


##Daily Report Generation
MP                                      <-   Positions
MP$"Sans_US"                            <-   str_replace_all(MP$symbol," US","")
MP$"Is this US Deal"                    <-   ifelse(!str_detect(MP$Sans_US," ")&MP$assetType!='Warrants',"US Deal","")
MP$Strategy                             <-   P_Strat_Lkup$Strategy_Updated[match(MP$Strategy,P_Strat_Lkup$Strategy)]
MP$Temp_Sym_strat                       <-   paste0(MP$symbol,MP$Strategy)
MP$V5_ID3                               <-   V5$ID_3[match(MP$Temp_Sym_strat,V5$Temp_Sym_Strat)]
DTB_Aggr_Buy                            <-   aggregate(Done~`Symbol US`,data=Trade_Blotter[Trade_Blotter$Side=="Buy",],sum)
DTB_Aggr_Buy                            <-   DTB_Aggr_Buy[DTB_Aggr_Buy$Done>0,]
MP$TradeBlotter_ID3                     <-   ifelse(MP$symbol%in%DTB_Aggr_Buy$`Symbol US`,Trade_Blotter$ID_3[match(MP$symbol,Trade_Blotter$`Symbol US`)],NA)

Counts_Sansus                           <-   count(MP,'Sans_US')
MP$"temp"                               <-   1 
Counts_Sansus_Strat                     <-   aggregate(temp~Sans_US+Temp_Sym_strat,data=MP,sum)


MP$'Counts_Sansus'                      <-   Counts_Sansus$freq[match(MP$Sans_US,Counts_Sansus$Sans_US)]
##MP$'Counts_Sansus_Strat'                <-   Counts_Sansus_Strat$temp[match(MP$Sans_US,Counts_Sansus_Strat$Sans_US)&match(MP$Temp_Sym_strat,Counts_Sansus_Strat)]
for(i in 1:nrow(MP))
{
  tvar<-MP$Sans_US[i]
  t_Strat<-MP$Temp_Sym_strat[i]
  tset<-Counts_Sansus_Strat[Counts_Sansus_Strat$Sans_US==tvar&Counts_Sansus_Strat$Temp_Sym_strat==t_Strat,]
  MP$Counts_Sansus_Strat[i]<-tset$temp[1]
}
MP$"ID_3_Calc"<-""
MP_NonNa<-MP[!is.na(MP$V5_ID3),]

for(i in 1:nrow(MP))
{
  MP$ID_3_Calc[i]<- ifelse(MP$Counts_Sansus[i]!=MP$Counts_Sansus_Strat[i],MP_NonNa$V5_ID3[match(MP$Sans_US[i],MP_NonNa$Sans_US)],"")
}

MP$"ID3_Final"<- ifelse(is.na(MP$V5_ID3),ifelse(is.na(MP$TradeBlotter_ID3),MP$ID_3_Calc,MP$TradeBlotter_ID3),MP$V5_ID3)



### ID3 dynamic Lookup creation
ID3_Final_IDs<-unique(MP$ID3_Final[MP$`Is this US Deal`=="US Deal"])
ID3_Final_IDs<- data.frame("id_3"=ID3_Final_IDs)
#ID3_Lkup<-rbind(ID3_Final_IDs,ID3_Lkup)
ID3_Lkup<-ID3_Final_IDs
ID3_Lkup<-ID3_Lkup[!is.na(ID3_Lkup$id_3),]
ID3_Lkup<-data.frame(ID3_Lkup)
ID3_Lkup$ID3_Lkup<-as.character(ID3_Lkup$ID3_Lkup)
ID3_Lkup<-ID3_Lkup[ID3_Lkup$ID3_Lkup!="",]
ID3_Lkup<-data.frame(ID3_Lkup)
ID3_Lkup$ID3_Lkup<-as.character(ID3_Lkup$ID3_Lkup)
ID3_Lkup$Client_Symbol<-substr(ID3_Lkup$ID3_Lkup,1,str_locate(ID3_Lkup$ID3_Lkup,"_")-1)
#ID3_Lkup$"Count_ID"<-""
#for(i in 1:nrow(ID3_Lkup)){
#  
#  temp<-ID3_Lkup$Client_Symbol[i]
#  
#  if(i==1){
#    ID3_Lkup$Count_ID[i]<-1
#  }else{
#    ID3_Lkup$Count_ID[i]<-ifelse(ID3_Lkup$Client_Symbol[i]%in%ID3_Lkup$Client_Symbol[1:i-1],2,1)
#  }
#}
#ID3_Lkup$"Ticker"<-paste0(ID3_Lkup$Client_Symbol,ID3_Lkup$Count_ID)
names(ID3_Lkup)[1]<-"id_3"
ID3_Lkup$"Strategy_Updated"<-MP$Strategy[match(ID3_Lkup$id_3,MP$ID3_Final)]
ID3_Lkup$"Deal_Type"<-P_Strat_Lkup$Deal_Type[match(ID3_Lkup$Strategy_Updated,P_Strat_Lkup$Strategy_Updated)]
ID3_Lkup_Long<-ID3_Lkup[ID3_Lkup$Deal_Type%in%c("FO","IPO","LONG-FND","Other"),]
ID3_Lkup_Short<-ID3_Lkup[ID3_Lkup$Deal_Type%in%c("Hedging","SHORT-FN"),]

ID3_Lkup_Long$"Count_ID"<-""
for(i in 1:nrow(ID3_Lkup_Long)){
  
  temp<-ID3_Lkup_Long$Client_Symbol[i]
  
  if(i==1){
    ID3_Lkup_Long$Count_ID[i]<-1
  }else{
    ID3_Lkup_Long$Count_ID[i]<-ifelse(ID3_Lkup_Long$Client_Symbol[i]%in%ID3_Lkup_Long$Client_Symbol[1:i-1],2,1)
  }
}
ID3_Lkup_Long$"Ticker"<-paste0(ID3_Lkup_Long$Client_Symbol,ID3_Lkup_Long$Count_ID)


ID3_Lkup_Short$"Count_ID"<-""
for(i in 1:nrow(ID3_Lkup_Short)){
  
  temp<-ID3_Lkup_Short$Client_Symbol[i]
  
  if(i==1){
    ID3_Lkup_Short$Count_ID[i]<-1
  }else{
    ID3_Lkup_Short$Count_ID[i]<-ifelse(ID3_Lkup_Short$Client_Symbol[i]%in%ID3_Lkup_Short$Client_Symbol[1:i-1],2,1)
  }
}
ID3_Lkup_Short$"Ticker"<-paste0(ID3_Lkup_Short$Client_Symbol,ID3_Lkup_Short$Count_ID)


##Getting Long Tickers
MP_Long                       <-   MP[MP$`Is this US Deal`=='US Deal'& MP$Strategy!="*FNPOSN" & MP$Strategy!="*REORG"& MP$Strategy!="OTHER"& MP$Strategy!="SHORT-FN"& MP$Strategy!="Hedging"& MP$Strategy!="XXXX",]
#MP_Long                       <-   MP_Long[!is.na(MP_Long$Trade_date),]
Tickers_Long                  <-   data.frame("Ticker"=unique(MP_Long$symbol))

#Getting Short & Hedge Tickers
MP_Short                      <-   MP[MP$`Is this US Deal`=='US Deal'&MP$Strategy=="SHORT-FN",]
MP_Short                      <-   MP_Short[!is.na(MP_Short$symbol),]
Tickers_Short                 <-   data.frame("Ticker"=unique(MP_Short$symbol))
MP_Hedge                      <-   MP[MP$`Is this US Deal`=='US Deal'& MP$Strategy=="Hedging"&MP$symbol!='EWW US'&MP$symbol!='RSX US',]
MP_Hedge                      <-   MP_Hedge[!is.na(MP_Hedge$symbol),]
Tickers_Hedge                 <-   data.frame("Ticker"=unique(MP_Hedge$symbol))
Tickers_Short                 <-   rbind(Tickers_Short,Tickers_Hedge)
MP_Hedge_Short<-rbind(MP_Short,MP_Hedge)


##Adding required columns for Long output
Tickers_Long$"Name"                        <-   ""
Tickers_Long$"Type"                        <-   ""
Tickers_Long$"Deal Captain"                <-   ""
Tickers_Long$"Percentage_of_max"           <-   ""
Tickers_Long$"Days_held"                   <-   ""
Tickers_Long$"Avg_Cost_Price"              <-   ""
Tickers_Long$"Sector"                      <-   ""
Tickers_Long$"Current Quantity"            <-   ""
Tickers_Long$"Last Close Price"            <-   ""
Tickers_Long$"DTD Gross P&L"               <-   ""
Tickers_Long$"DTD Gross P&L as % of AUM"   <-   ""
Tickers_Long$"Gross Exposure"              <-   ""
Tickers_Long$"Ultimate Stop Price"         <-   ""
Tickers_Long$"Target Price"                <-   ""

##Adding required columns for Short output
Tickers_Short$"Name"                       <-   ""
Tickers_Short$"Type"                        <-   ""
Tickers_Short$"Deal Captain"               <-   ""
Tickers_Short$"Percentage_of_max"           <-   ""
Tickers_Short$"Days_held"                   <-   ""
Tickers_Short$"Avg_Cost_Price"              <-   ""
Tickers_Short$"Sector"                     <-   ""
Tickers_Short$"Current Quantity"           <-   ""
Tickers_Short$"Last Close Price"           <-   ""
Tickers_Short$"DTD Gross P&L"              <-   ""
Tickers_Short$"DTD Gross P&L as % of AUM"  <-   ""
Tickers_Short$"Gross Exposure"             <-   ""
Tickers_Short$"Ultimate Stop Price"        <-   ""
Tickers_Short$"Target Price"               <-   ""


##Pulling required data for Long output
Name_DF                                                                                                                <-unique(Positions[,c(2,3)])
Tickers_Long$Ticker                                                                                                    <-  as.character(Tickers_Long$Ticker)
Tickers_Long$Name                                                                                                      <-  Name_DF$instrument.Description[match(Tickers_Long$Ticker,Name_DF$symbol)]
#Tickers_Long$`Deal Captain`                                                                                            <-  MDD$`Deal Captain`[match(Tickers_Long$Ticker,MDD$`Ticker US`)]
#DC_DF                                                                                                                  <-  unique(Trade_Blotter[,c("Analyst","Symbol US")])
#Tickers_Long$`Deal Captain`                                                                                            <-  ifelse(is.na(Tickers_Long$`Deal Captain`),DC_DF$Analyst[match(Tickers_Long$Ticker,DC_DF$`Symbol US`)],Tickers_Long$`Deal Captain`)
Qty_Agg                                                                                                                <-  aggregate(qty~symbol,data=MP_Long,sum)
Tickers_Long$`Current Quantity`                                                                                        <-  Qty_Agg$qty[match(Tickers_Long$Ticker,Qty_Agg$symbol)]
Price_Avg                                                                                                              <-  aggregate(priceClose~symbol,data=Positions[Positions$qty!=0,],mean)
Tickers_Long$`Last Close Price`                                                                                        <-  Price_Avg$priceClose[match(Tickers_Long$Ticker,Price_Avg$symbol)]
PNL_Agg                                                                                                                <-  aggregate(plDay~symbol,data=MP_Long,sum)
Tickers_Long$`DTD Gross P&L`                                                                                           <-  PNL_Agg$plDay[match(Tickers_Long$Ticker,PNL_Agg$symbol)]
Positions$Daily.NAV                                                                                                    <-  gsub(",","",Positions$Daily.NAV)
Positions$Daily.NAV                                                                                                    <-  as.numeric(Positions$Daily.NAV)
AUM_Agg                                                                                                                <-  aggregate(Daily.NAV~sub_fund,data=Positions,mean)
AUM_total                                                                                                              <-  sum(AUM_Agg$Daily.NAV)
Tickers_Long$`DTD Gross P&L as % of AUM`                                                                               <-  Tickers_Long$`DTD Gross P&L`/AUM_total
Tickers_Long$`Gross Exposure`                                                                                          <-  Tickers_Long$`Current Quantity`*Tickers_Long$`Last Close Price`
Tickers_Long$`Ultimate Stop Price`                                                                                     <-  Risk_Reward$`Ultimate Stop Price`[match(Tickers_Long$Ticker,Risk_Reward$`Symbol US`)]
Tickers_Long$`Target Price`                                                                                            <-  Risk_Reward$`Target Price`[match(Tickers_Long$Ticker,Risk_Reward$`Symbol US`)]
Tickers_Long$`Ultimate Stop Price`[Tickers_Long$`Ultimate Stop Price`==0|is.na(Tickers_Long$`Ultimate Stop Price`)]    <-  ""
Tickers_Long$`Target Price`[Tickers_Long$`Target Price`==0|is.na(Tickers_Long$`Target Price`)]                         <-  ""

###Long Extra Additions
Tickers_Long$ID_3<-MP_Long$ID3_Final[match(Tickers_Long$Ticker,MP_Long$symbol)]
Tickers_Long$"Type_ref"<- MP_Long$Strategy[match(Tickers_Long$ID_3,MP_Long$ID3_Final)]
Tickers_Long$Temp_ID3<- paste0(Tickers_Long$Ticker,"2")
Tickers_Long$ID3_Multiple<- ID3_Lkup_Long$id_3[match(Tickers_Long$Temp_ID3,ID3_Lkup_Long$Ticker)]
Tickers_Long$"Type_ref2"<- ifelse(is.na(Tickers_Long$ID3_Multiple),NA,MP_Long$Strategy[match(Tickers_Long$ID3_Multiple,MP_Long$ID3_Final)])

Tickers_Long$Type_ref_Cat<- P_Strat_Lkup$Deal_Type[match(Tickers_Long$Type_ref,P_Strat_Lkup$Strategy_Updated)]
Tickers_Long$Type_ref2_Cat<- P_Strat_Lkup$Deal_Type[match(Tickers_Long$Type_ref2,P_Strat_Lkup$Strategy_Updated)]

Tickers_Long$Type<-ifelse(is.na(Tickers_Long$Type_ref2_Cat),Tickers_Long$Type_ref_Cat,paste0(Tickers_Long$Type_ref_Cat,", ",Tickers_Long$Type_ref2_Cat))

Tickers_Long$Ttl_Shr_bought_1<- as.numeric(MDD$`Total Shares Bought`[match(Tickers_Long$ID_3,MDD$`Kiski Deals (Unique ID)`)])
Tickers_Long$Ttl_Shr_bought_2<-as.numeric( MDD$`Total Shares Bought`[match(Tickers_Long$ID3_Multiple,MDD$`Kiski Deals (Unique ID)`)] )
Buy_TB<- Trade_Blotter[Trade_Blotter$Side=="Buy",]
Buy_TB_Totals<-aggregate(Done ~ Buy_TB$`Symbol US`,data=Buy_TB,sum)
names(Buy_TB_Totals)[1]<-"Symbol"

Tickers_Long$"Quantity_Sum"<-""
Tickers_Long$Ttl_Shr_bought_TB<- as.numeric(Buy_TB_Totals$Done[match(Tickers_Long$Ticker,Buy_TB_Totals$Symbol)])
for(i in 1:nrow(Tickers_Long))
{
  Tickers_Long$Quantity_Sum[i]<-sum(Tickers_Long$Ttl_Shr_bought_1[i],Tickers_Long$Ttl_Shr_bought_2[i],Tickers_Long$Ttl_Shr_bought_TB[i],na.rm=T)
}
Tickers_Long$Percentage_of_max<-Tickers_Long$`Current Quantity`/as.numeric(Tickers_Long$Quantity_Sum)


### Percentage of Max handling issues
max_per<-max(Tickers_Long$Percentage_of_max[Tickers_Long$Percentage_of_max!=Inf],na.rm=T)

if(max_per>1)
{
  max_per_Vars<-Tickers_Long$Ticker[Tickers_Long$Percentage_of_max>1&Tickers_Long$Percentage_of_max!=Inf&!is.nan(Tickers_Long$Percentage_of_max)]
  New_ID3s<-data.frame("id_3"=unique(V5$ID_3[V5$Client_Symbol %in% max_per_Vars]))
  New_ID3s$id_3<-as.character(New_ID3s$id_3)
  total_ID3s<-c(as.character(ID3_Lkup_Long$id_3),as.character(New_ID3s$id_3))
  ID3_Lkup_Long<-data.frame("id_3"=unique(total_ID3s))
  ID3_Lkup_Long<-ID3_Lkup_Long[!is.na(ID3_Lkup_Long$id_3),]
  ID3_Lkup_Long<-data.frame(ID3_Lkup_Long)
  ID3_Lkup_Long$ID3_Lkup_Long<-as.character(ID3_Lkup_Long$ID3_Lkup_Long)
  ID3_Lkup_Long<-ID3_Lkup_Long[ID3_Lkup_Long$ID3_Lkup_Long!="",]
  ID3_Lkup_Long<-data.frame(ID3_Lkup_Long)
  ID3_Lkup_Long$ID3_Lkup_Long<-as.character(ID3_Lkup_Long$ID3_Lkup_Long)
  ID3_Lkup_Long$Client_Symbol<-substr(ID3_Lkup_Long$ID3_Lkup_Long,1,str_locate(ID3_Lkup_Long$ID3_Lkup_Long,"_")-1)
  ID3_Lkup_Long$"Count_ID"<-""
  for(i in 1:nrow(ID3_Lkup_Long)){
    
    temp<-ID3_Lkup_Long$Client_Symbol[i]
    
    if(i==1){
      ID3_Lkup_Long$Count_ID[i]<-1
    }else{
      ID3_Lkup_Long$Count_ID[i]<-ifelse(ID3_Lkup_Long$Client_Symbol[i]%in%ID3_Lkup_Long$Client_Symbol[1:i-1],2,1)
    }
  }
  ID3_Lkup_Long$"Ticker"<-paste0(ID3_Lkup_Long$Client_Symbol,ID3_Lkup_Long$Count_ID)
  names(ID3_Lkup_Long)[1]<-"id_3"
  Tickers_Long$ID3_Multiple<- ID3_Lkup_Long$id_3[match(Tickers_Long$Temp_ID3,ID3_Lkup_Long$Ticker)]
  Tickers_Long$"Type_ref2"<- ifelse(is.na(Tickers_Long$ID3_Multiple),NA,MP_Long$Strategy[match(Tickers_Long$ID3_Multiple,MP_Long$ID3_Final)])
  Tickers_Long$Type_ref2_Cat<- P_Strat_Lkup$Deal_Type[match(Tickers_Long$Type_ref2,P_Strat_Lkup$Strategy_Updated)]
  
  Tickers_Long$Type<-ifelse(is.na(Tickers_Long$Type_ref2_Cat),Tickers_Long$Type_ref_Cat,paste0(Tickers_Long$Type_ref_Cat,", ",Tickers_Long$Type_ref2_Cat))
  Tickers_Long$Ttl_Shr_bought_2<-as.numeric( MDD$`Total Shares Bought`[match(Tickers_Long$ID3_Multiple,MDD$`Kiski Deals (Unique ID)`)] )
  for(i in 1:nrow(Tickers_Long))
  {
    Tickers_Long$Quantity_Sum[i]<-sum(Tickers_Long$Ttl_Shr_bought_1[i],Tickers_Long$Ttl_Shr_bought_2[i],Tickers_Long$Ttl_Shr_bought_TB[i],na.rm=T)
  }
  Tickers_Long$Percentage_of_max<-Tickers_Long$`Current Quantity`/as.numeric(Tickers_Long$Quantity_Sum)
}




#MDD$`Total Committed Capital`<- gsub(",","",MDD$`Total Committed Capital`)
#MDD$`Total Committed Capital`<- substr(MDD$`Total Committed Capital`,2,length(MDD$`Total Committed Capital`))

Tickers_Long$"Ttl_Cap_cmtd_1"<-as.numeric(MDD$`Total Committed Capital`[match(Tickers_Long$ID_3,MDD$`Kiski Deals (Unique ID)`)])
Tickers_Long$"Ttl_Cap_cmtd_2"<-as.numeric(MDD$`Total Committed Capital`[match(Tickers_Long$ID3_Multiple,MDD$`Kiski Deals (Unique ID)`)])
Buy_TB$`Net Money (USD)`<-as.numeric(Buy_TB$`Net Money (USD)`)
Buy_TB_Cap_Totals<-aggregate(Buy_TB$`Net Money (USD)` ~ Buy_TB$`Symbol US`,data=Buy_TB,sum)
names(Buy_TB_Cap_Totals)<-c("Symbol","Total_Cap")

Tickers_Long$"Ttl_Cap_cmtd_TB"<- as.numeric(Buy_TB_Cap_Totals$Total_Cap[match(Tickers_Long$Ticker,Buy_TB_Cap_Totals$Symbol)])
for(i in 1:nrow(Tickers_Long))
{
  Tickers_Long$Cap_Sum[i]<-sum(Tickers_Long$Ttl_Cap_cmtd_1[i],Tickers_Long$Ttl_Cap_cmtd_2[i],Tickers_Long$Ttl_Cap_cmtd_TB[i],na.rm=T)
}

Tickers_Long$Avg_Cost_Price<-Tickers_Long$Cap_Sum/as.numeric(Tickers_Long$Quantity_Sum)
MDD$`First Trade Date`<- as.Date(MDD$`First Trade Date`,"%Y-%m-%d")
Tickers_Long$First_Trade_Date<- MDD$`First Trade Date`[match(Tickers_Long$ID_3,MDD$`Kiski Deals (Unique ID)`)]
Tickers_Long$Buy_TB_Totals<- Buy_TB_Totals$Done[match(Tickers_Long$Ticker,Buy_TB_Totals$Symbol)]
temp_date<-as.Date(date_curr)
#Tickers_Long$First_Trade_Date<-ifelse(is.na(Tickers_Long$First_Trade_Date),ifelse(Tickers_Long$Buy_TB_Totals>0,as.Date(temp_date),""),Tickers_Long$First_Trade_Date)
for(i in 1:nrow(Tickers_Long))
{
  if(is.na(Tickers_Long$First_Trade_Date[i]))
  {
    
    if(Tickers_Long$Buy_TB_Totals[i]>0|is.na(Tickers_Long$Buy_TB_Totals[i]))
    {
      Tickers_Long$First_Trade_Date[i]<-temp_date
    }else{
      
      Tickers_Long$First_Trade_Date[i]<-NA
    }
  }
  
}
Tickers_Long$Days_held<-difftime(date_curr,Tickers_Long$First_Trade_Date,units='days')+1
Tickers_Long$Sector<-  MDD$`GICS Sector from Bloomberg`[match(Tickers_Long$Ticker,MDD$`Ticker US`)]

### Changes Abyn
Tickers_Long$"DC1"<-MDD$`Deal Captain`[match(Tickers_Long$ID_3,MDD$`Kiski Deals (Unique ID)`)]
Tickers_Long$DC1<-ifelse(is.na(Tickers_Long$DC1),Risk_Reward$Analyst[match(Tickers_Long$Ticker,Risk_Reward$`Symbol US`)],Tickers_Long$DC1)

Tickers_Long$"DC2"<-MDD$`Deal Captain`[match(Tickers_Long$ID3_Multiple,MDD$`Kiski Deals (Unique ID)`)]
#Tickers_Long$DC2<-ifelse(is.na(Tickers_Long$DC2),Risk_Reward$Analyst[match(Tickers_Long$Ticker,Risk_Reward$`Symbol US`)],Tickers_Long$DC2)

Tickers_Long$DC1<-toupper(Tickers_Long$DC1)
Tickers_Long$DC2<-toupper(Tickers_Long$DC2)

for(j in 1:nrow(Tickers_Long))
{
  temp_dc1<-Tickers_Long$DC1[j]
  temp_dc2<-Tickers_Long$DC2[j]
  temp_dc1<-ifelse(is.na(temp_dc1),"",temp_dc1)
  temp_dc2<-ifelse(is.na(temp_dc2),"",temp_dc2)
  if(temp_dc1!=""&temp_dc2=="")
  {
      Tickers_Long$`Deal Captain`[j]=temp_dc1
  }
  if(temp_dc1==""&temp_dc2!="")
  {
      Tickers_Long$`Deal Captain`[j]=temp_dc2
  }
  if(temp_dc1!=""&temp_dc2!=""&temp_dc1==temp_dc2)
  {
      Tickers_Long$`Deal Captain`[j]=temp_dc1
  }
  if(temp_dc1!=""&temp_dc2!=""&temp_dc1!=temp_dc2)
  {
    Tickers_Long$`Deal Captain`[j]=paste0(temp_dc1,", ",temp_dc2)
  }
}


##Pulling required data for Short output
Name_DF                                                                                                                <-  unique(Positions[,c(2,3)])
Tickers_Short$Ticker                                                                                                   <-  as.character(Tickers_Short$Ticker)
Tickers_Short$Name                                                                                                     <-  Name_DF$instrument.Description[match(Tickers_Short$Ticker,Name_DF$symbol)]
#Tickers_Short$`Deal Captain`                                                                                           <-  MDD$`Deal Captain`[match(Tickers_Short$Ticker,MDD$`Ticker US`)]
#DC_DF                                                                                                                  <-  unique(Trade_Blotter[,c("Analyst","Symbol US")])
#Tickers_Short$`Deal Captain`                                                                                           <-  ifelse(is.na(Tickers_Short$`Deal Captain`),DC_DF$Analyst[match(Tickers_Short$Ticker,DC_DF$`Symbol US`)],Tickers_Short$`Deal Captain`)
Qty_Agg                                                                                                                <-  aggregate(qty~symbol,data=MP_Hedge_Short,sum)
Tickers_Short$`Current Quantity`                                                                                       <-  Qty_Agg$qty[match(Tickers_Short$Ticker,Qty_Agg$symbol)]
Price_Avg                                                                                                              <-  aggregate(priceClose~symbol,data=Positions[Positions$qty!=0,],mean)
Tickers_Short$`Last Close Price`                                                                                       <-  Price_Avg$priceClose[match(Tickers_Short$Ticker,Price_Avg$symbol)]
PNL_Agg                                                                                                                <-  aggregate(plDay~symbol,data=MP_Hedge_Short,sum)
Tickers_Short$`DTD Gross P&L`                                                                                          <-  PNL_Agg$plDay[match(Tickers_Short$Ticker,PNL_Agg$symbol)]
Positions$Daily.NAV                                                                                                    <-  gsub(",","",Positions$Daily.NAV)
Positions$Daily.NAV                                                                                                    <-  as.numeric(Positions$Daily.NAV)
AUM_Agg                                                                                                                <-  aggregate(Daily.NAV~sub_fund,data=Positions,mean)
AUM_total                                                                                                              <-  sum(AUM_Agg$Daily.NAV)
Tickers_Short$`DTD Gross P&L as % of AUM`                                                                              <-  Tickers_Short$`DTD Gross P&L`/AUM_total
Tickers_Short$`Gross Exposure`                                                                                         <-  Tickers_Short$`Current Quantity`*Tickers_Short$`Last Close Price`
Tickers_Short$`Ultimate Stop Price`                                                                                    <-  Risk_Reward$`Ultimate Stop Price`[match(Tickers_Short$Ticker,Risk_Reward$`Symbol US`)]
Tickers_Short$`Target Price`                                                                                           <-  Risk_Reward$`Target Price`[match(Tickers_Short$Ticker,Risk_Reward$`Symbol US`)]
Tickers_Short$`Ultimate Stop Price`[Tickers_Short$`Ultimate Stop Price`==0|is.na(Tickers_Short$`Ultimate Stop Price`)] <-  ""
Tickers_Short$`Target Price`[Tickers_Short$`Target Price`==0|is.na(Tickers_Short$`Target Price`)]                      <-  ""


### Short extra additions

Tickers_Short$ID_3<-MP_Hedge_Short$ID3_Final[match(Tickers_Short$Ticker,MP_Hedge_Short$symbol)]
Tickers_Short$"Type_ref"<- MP_Hedge_Short$Strategy[match(Tickers_Short$ID_3,MP_Hedge_Short$ID3_Final)]
Tickers_Short$Temp_ID3<- paste0(Tickers_Short$Ticker,"2")
Tickers_Short$"ID3_Multiple"<- ID3_Lkup_Short$id_3[match(Tickers_Short$Temp_ID3,ID3_Lkup_Short$Ticker)]
Tickers_Short$"Type_ref2"<- ifelse(is.na(Tickers_Short$ID3_Multiple),NA,MP_Hedge_Short$Strategy[match(Tickers_Short$ID3_Multiple,MP_Hedge_Short$ID3_Final)])

Tickers_Short$Type_ref_Cat<- P_Strat_Lkup$Deal_Type[match(Tickers_Short$Type_ref,P_Strat_Lkup$Strategy_Updated)]
Tickers_Short$Type_ref2_Cat<- P_Strat_Lkup$Deal_Type[match(Tickers_Short$Type_ref2,P_Strat_Lkup$Strategy_Updated)]

Tickers_Short$Type<-ifelse(is.na(Tickers_Short$Type_ref2_Cat),Tickers_Short$Type_ref_Cat,paste0(Tickers_Short$Type_ref_Cat,", ",Tickers_Short$Type_ref2_Cat))

Tickers_Short$Ttl_Shr_bought_1<- as.numeric(MDD$`Total Shares Bought`[match(Tickers_Short$ID_3,MDD$`Kiski Deals (Unique ID)`)])
Tickers_Short$Ttl_Shr_bought_2<-as.numeric( MDD$`Total Shares Bought`[match(Tickers_Short$ID3_Multiple,MDD$`Kiski Deals (Unique ID)`)] )
Buy_TB<- Trade_Blotter[Trade_Blotter$Side=="Short",]
Buy_TB_Totals<-aggregate(Done ~ Buy_TB$`Symbol US`,data=Buy_TB,sum)
names(Buy_TB_Totals)[1]<-"Symbol"

Tickers_Short$"Quantity_Sum"<-""
Tickers_Short$Ttl_Shr_bought_TB<- as.numeric(Buy_TB_Totals$Done[match(Tickers_Short$Ticker,Buy_TB_Totals$Symbol)])
for(i in 1:nrow(Tickers_Short))
{
  Tickers_Short$Quantity_Sum[i]<-sum(Tickers_Short$Ttl_Shr_bought_1[i],Tickers_Short$Ttl_Shr_bought_2[i],Tickers_Short$Ttl_Shr_bought_TB[i],na.rm=T)
}
Tickers_Short$Percentage_of_max<-(Tickers_Short$`Current Quantity`/as.numeric(Tickers_Short$Quantity_Sum))*-1

#MDD$`Total Committed Capital`<- gsub(",","",MDD$`Total Committed Capital`)
#MDD$`Total Committed Capital`<- substr(MDD$`Total Committed Capital`,2,length(MDD$`Total Committed Capital`))

Tickers_Short$"Ttl_Cap_cmtd_1"<-as.numeric(MDD$`Total Committed Capital`[match(Tickers_Short$ID_3,MDD$`Kiski Deals (Unique ID)`)])
Tickers_Short$"Ttl_Cap_cmtd_2"<-as.numeric(MDD$`Total Committed Capital`[match(Tickers_Short$ID3_Multiple,MDD$`Kiski Deals (Unique ID)`)])
Buy_TB$`Net Money (USD)`<-as.numeric(Buy_TB$`Net Money (USD)`)
Buy_TB_Cap_Totals<-aggregate(Buy_TB$`Net Money (USD)` ~ Buy_TB$`Symbol US`,data=Buy_TB,sum)
names(Buy_TB_Cap_Totals)<-c("Symbol","Total_Cap")

Tickers_Short$"Ttl_Cap_cmtd_TB"<- as.numeric(Buy_TB_Cap_Totals$Total_Cap[match(Tickers_Short$Ticker,Buy_TB_Cap_Totals$Symbol)])
for(i in 1:nrow(Tickers_Short))
{
  Tickers_Short$Cap_Sum[i]<-sum(Tickers_Short$Ttl_Cap_cmtd_1[i],Tickers_Short$Ttl_Cap_cmtd_2[i],Tickers_Short$Ttl_Cap_cmtd_TB[i],na.rm=T)
}

Tickers_Short$Avg_Cost_Price<-Tickers_Short$Cap_Sum/as.numeric(Tickers_Short$Quantity_Sum)
#MDD$`First Trade Date`<- as.Date(MDD$`First Trade Date`,"%m/%d/%Y")
Tickers_Short$First_Trade_Date<- MDD$`First Trade Date`[match(Tickers_Short$ID_3,MDD$`Kiski Deals (Unique ID)`)]
Tickers_Short$Buy_TB_Totals<- Buy_TB_Totals$Done[match(Tickers_Short$Ticker,Buy_TB_Totals$Symbol)]
temp_date<-as.Date(date_curr)
#Tickers_Short$First_Trade_Date<-ifelse(is.na(Tickers_Short$First_Trade_Date),ifelse(Tickers_Short$Buy_TB_Totals>0,as.Date(temp_date),""),Tickers_Short$First_Trade_Date)
for(i in 1:nrow(Tickers_Short))
{
  if(is.na(Tickers_Short$First_Trade_Date[i]))
  {
    
    if(Tickers_Short$Buy_TB_Totals[i]>0|is.na(Tickers_Short$Buy_TB_Totals[i]))
    {
      Tickers_Short$First_Trade_Date[i]<-temp_date
    }else{
      
      Tickers_Short$First_Trade_Date[i]<-NA
    }
  }
  
}


Tickers_Short$Days_held<-difftime(date_curr,Tickers_Short$First_Trade_Date,units='days')+1
Tickers_Short$Days_held[Tickers_Short$Type=="Hedging"]<-0
Tickers_Short$Sector<-  MDD$`GICS Sector from Bloomberg`[match(Tickers_Short$Ticker,MDD$`Ticker US`)]



### Changes Abyn
Tickers_Short$"DC1"<-MDD$`Deal Captain`[match(Tickers_Short$ID_3,MDD$`Kiski Deals (Unique ID)`)]
Tickers_Short$DC1<-ifelse(is.na(Tickers_Short$DC1),Risk_Reward$Analyst[match(Tickers_Short$Ticker,Risk_Reward$`Symbol US`)],Tickers_Short$DC1)

Tickers_Short$"DC2"<-MDD$`Deal Captain`[match(Tickers_Short$ID3_Multiple,MDD$`Kiski Deals (Unique ID)`)]
#Tickers_Short$DC2<-ifelse(is.na(Tickers_Short$DC2),Risk_Reward$Analyst[match(Tickers_Short$Ticker,Risk_Reward$`Symbol US`)],Tickers_Short$DC2)

Tickers_Short$DC1<-toupper(Tickers_Short$DC1)
Tickers_Short$DC2<-toupper(Tickers_Short$DC2)


###Stucked Here
for(j in 1:nrow(Tickers_Short))
{
  temp_dc1<-Tickers_Short$DC1[j]
  temp_dc2<-Tickers_Short$DC2[j]
  temp_dc1<-ifelse(is.na(temp_dc1),"",temp_dc1)
  temp_dc2<-ifelse(is.na(temp_dc2),"",temp_dc2)
  if(temp_dc1!=""&temp_dc2=="")
  {
    Tickers_Short$`Deal Captain`[j]=temp_dc1
  }
  if(temp_dc1==""&temp_dc2!="")
  {
    Tickers_Short$`Deal Captain`[j]=temp_dc2
  }
  if(temp_dc1!=""&temp_dc2!=""&temp_dc1==temp_dc2)
  {
    Tickers_Short$`Deal Captain`[j]=temp_dc1
  }
  if(temp_dc1!=""&temp_dc2!=""&temp_dc1!=temp_dc2)
  {
    Tickers_Short$`Deal Captain`[j]=paste0(temp_dc1,", ",temp_dc2)
  }
}

Tickers_Long<-Tickers_Long[order(Tickers_Long$Ticker),]
Tickers_Short<-Tickers_Short[order(Tickers_Short$Ticker),]
Tickers_Long$`Current Quantity`<-as.numeric(Tickers_Long$`Current Quantity`)
Tickers_Short$`Current Quantity`<-as.numeric(Tickers_Short$`Current Quantity`)                                          
Tickers_Long<-Tickers_Long[Tickers_Long$`Current Quantity`>0,]
Tickers_Short<-Tickers_Short[Tickers_Short$`Current Quantity`<0,]
Tickers_Long_Final<-Tickers_Long[,c(1,2,3,4,8,9,5,6,10,7,14,15,11,13)]
Tickers_Short_Final<-Tickers_Short[,names(Tickers_Long_Final)]


### Days Hld adjustments for warrants (Need to add ticker here if there is a new warrant)
Tickers_Long_Final$Days_held[Tickers_Long_Final$Ticker%in%c("KAACW US","LCAHW US","ONSIW US","ONSIZ US")]<-""
Tickers_Short_Final$Days_held[Tickers_Short_Final$Ticker%in%c("KAACW US","LCAHW US","ONSIW US","ONSIZ US")]<-""

##Sector Aliases
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Consumer Discretionary"]<-"CD"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Consumer Staples"]<-"CS"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Energy"]<-"NRG"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Health Care"]<-"HC"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Industrials"]<-"IND"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Information Technology"]<-"Tech"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Financials"]<-"Fin"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Materials"]<-"MTLS"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Real Estate"]<-"RE"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Telecommunication Services"]<-"Telcom"
Tickers_Long_Final$Sector[Tickers_Long_Final$Sector=="Utilities"]<-"UTLS"


Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Consumer Discretionary"]<-"CD"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Consumer Staples"]<-"CS"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Energy"]<-"NRG"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Health Care"]<-"HC"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Industrials"]<-"IND"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Information Technology"]<-"Tech"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Financials"]<-"Fin"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Materials"]<-"MTLS"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Real Estate"]<-"RE"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Telecommunication Services"]<-"Telcom"
Tickers_Short_Final$Sector[Tickers_Short_Final$Sector=="Utilities"]<-"UTLS"

Tickers_Short_Final$Name<-substr(Tickers_Short_Final$Name,1,30)
Tickers_Long_Final$Name<-substr(Tickers_Long_Final$Name,1,30)

Tickers_Long_Final$Percentage_of_max[is.infinite(Tickers_Long_Final$Percentage_of_max)]<-""
Tickers_Short_Final$Percentage_of_max[is.infinite(Tickers_Short_Final$Percentage_of_max)]<-""

#Removing NAs from the outputs
Tickers_Long_Final[is.na(Tickers_Long_Final)]    <-  ""
Tickers_Short_Final[is.na(Tickers_Short_Final)]  <-  ""

Tickers_Short_Final$`Deal Captain`[Tickers_Short_Final$Type=="Hedging"]<-""
Tickers_Short_Final$Percentage_of_max[Tickers_Short_Final$Type=="Hedging"]<-""

### Numeric Conversions
Tickers_Long_Final$Percentage_of_max<-as.numeric(Tickers_Long_Final$Percentage_of_max)
Tickers_Long_Final$Days_held<-as.numeric(Tickers_Long_Final$Days_held)
Tickers_Long_Final$Avg_Cost_Price<-as.numeric(Tickers_Long_Final$Avg_Cost_Price)
Tickers_Long_Final$`Ultimate Stop Price`<-as.numeric(Tickers_Long_Final$`Ultimate Stop Price`)
Tickers_Long_Final$`Target Price`<-as.numeric(Tickers_Long_Final$`Target Price`)

Tickers_Short_Final$Percentage_of_max<-as.numeric(Tickers_Short_Final$Percentage_of_max)
Tickers_Short_Final$Days_held<-as.numeric(Tickers_Short_Final$Days_held)
Tickers_Short_Final$Avg_Cost_Price<-as.numeric(Tickers_Short_Final$Avg_Cost_Price)
Tickers_Short_Final$`Ultimate Stop Price`<-as.numeric(Tickers_Short_Final$`Ultimate Stop Price`)
Tickers_Short_Final$`Target Price`<-as.numeric(Tickers_Short_Final$`Target Price`)

Tickers_Short_Final_NotHedge<-Tickers_Short_Final[Tickers_Short_Final$Type!='Hedging',]
Tickers_Short_Final_Hedge<-Tickers_Short_Final[Tickers_Short_Final$Type=='Hedging',]
Tickers_Short_Final_Hedge<-Tickers_Short_Final_Hedge[Tickers_Short_Final_Hedge$Ticker%in%c("SPY_M US","XBI US","XLE US","IWM US"),]
Tickers_Short_Final<-rbind(Tickers_Short_Final_NotHedge,Tickers_Short_Final_Hedge)


#Writing Output files
write.csv(Tickers_Long_Final,"Output Files/Output_Long.csv",row.names = F, na="")
write.csv(Tickers_Short_Final,"Output Files/Output_Short.csv",row.names = F, na="")


###Recent Trades Tab
Tb1                                <-  Trade_Blotter
Tb1$Done                           <-  as.numeric(Tb1$Done)
Tb1$`Net Money (USD)`              <-  as.numeric(Tb1$`Net Money (USD)`)

##Abyn Changes
Tb1$"Check_US_Deal"<-ifelse(str_detect(Tb1$Symbol," "),"NON-US","US")
Tb1<-Tb1[Tb1$Check_US_Deal=="US",]
Tb1<-Tb1[Tb1$Currency=="USD",]

Buy_tickers                        <-  unique(Tb1$Symbol[Tb1$Side=='Buy'&Tb1$Currency=='USD'])
if(length(Buy_tickers)>0)
{
  Buy_tickers                        <-  sort(Buy_tickers)
  Buy_df                             <-  data.frame("Ticker"=Buy_tickers)
}


Sell_tickers                       <-  unique(Tb1$Symbol[(Tb1$Side=='Sell') & Tb1$Currency=='USD'])
if(length(Sell_tickers)>0)
{
  Sell_tickers                       <-  sort(Sell_tickers)
  Sell_df                            <-  data.frame("Ticker"=Sell_tickers)
  Sell_df$Ticker                     <-  as.character(Sell_df$Ticker)
  Sell_df                            <-  Sell_df[!(Sell_df$Ticker %in% c("SPY","XBI","IWM","XLE","EWW", "RSX")),]
  Sell_df                            <-  data.frame("Ticker"=Sell_df)
}

Short_tickers                      <-  unique(Tb1$Symbol[Tb1$Side=='Short' & Tb1$Currency=='USD' & Tb1$Strategy!="HEDGE"])
if(length(Short_tickers)>0)
{
  Short_tickers                      <-  sort(Short_tickers)
  Short_df                           <-  data.frame("Ticker"=Short_tickers)
  Short_df$Ticker                    <-  as.character(Short_df$Ticker)
  Short_df                           <-  Short_df[!(Short_df$Ticker %in% c("SPY","XBI","IWM","XLE","EWW", "RSX")),]
  Short_df                           <-  data.frame("Ticker"=Short_df)
}

Cover_tickers                      <-  unique(Tb1$Symbol[Tb1$Side=='Cover' & Tb1$Currency=='USD' & Tb1$Strategy!="HEDGE"])
if(length(Cover_tickers)>0)
{
  Cover_tickers                      <-  sort(Cover_tickers)
  Cover_df                           <-  data.frame("Ticker"=Cover_tickers)
  Cover_df$Ticker                    <-  as.character(Cover_df$Ticker)
  Cover_df                           <-  Cover_df[!(Cover_df$Ticker %in% c("SPY","XBI","IWM","XLE","EWW", "RSX")),]
  Cover_df                           <-  data.frame("Ticker"=Cover_df)
}

if(length(Buy_tickers)>0)
{
  Buy_df$"Name"                      <-  ""           
  Buy_df$"Deal Captain"              <-  ""      
  Buy_df$"Quantity Bought"           <-  ""
  Buy_df$"Net Money ($)"             <-  ""    
  Buy_df$"Average Bought Price ($)"  <-  ""            
  Buy_df$"EOD Quantity"              <-  ""
}

if(length(Sell_tickers)>0)
{
  Sell_df$"Name"                      <-  ""           
  Sell_df$"Deal Captain"              <-  ""       
  Sell_df$"Quantity Bought"           <-  ""
  Sell_df$"Net Money ($)"             <-  ""    
  Sell_df$"Average Bought Price ($)"  <-  ""            
  Sell_df$"EOD Quantity"              <-  ""
}

if(length(Short_tickers)>0)
{
  Short_df$"Name"                      <-  ""        
  Short_df$"Deal Captain"              <-  ""   
  Short_df$"Quantity Bought"           <-  ""
  Short_df$"Net Money ($)"             <-  "" 
  Short_df$"Average Bought Price ($)"  <-  ""        
  Short_df$"EOD Quantity"              <-  ""
}

if(length(Cover_tickers)>0)
{
  Cover_df$"Name"                      <-  ""       
  Cover_df$"Deal Captain"              <-  ""  
  Cover_df$"Quantity Bought"           <-  ""
  Cover_df$"Net Money ($)"             <-  ""                
  Cover_df$"Average Bought Price ($)"  <-  ""        
  Cover_df$"EOD Quantity"              <-  ""
}

if(length(Buy_tickers)>0)
{
  Buy_df$Name                         <-  Tb1$Desc[match(Buy_df$Ticker,Tb1$Symbol)]
  Buy_df$`Deal Captain`               <-  toupper(Tb1$Analyst[match(Buy_df$Ticker,Tb1$Symbol)])        
  qty_bought_agg                      <-  aggregate(Done~Symbol,data=Tb1[Tb1$Side=='Buy'&Tb1$Currency=='USD',],sum)
  Buy_df$`Quantity Bought`            <-  qty_bought_agg$Done[match(Buy_df$Ticker,qty_bought_agg$Symbol)]
  Net_money_agg                       <-  aggregate(`Net Money (USD)` ~Symbol,data=Tb1[Tb1$Side=='Buy'&Tb1$Currency=='USD',],sum)
  Buy_df$`Net Money ($)`              <-  Net_money_agg$`Net Money (USD)`[match(Buy_df$Ticker,Net_money_agg$Symbol)]
  Buy_df$`Average Bought Price ($)`   <-  Buy_df$`Net Money ($)`/Buy_df$`Quantity Bought`
  Buy_df$"Temp"                       <-  paste0(as.character(Buy_df$Ticker)," US")
  qty_eod_agg                         <-  aggregate(qty~symbol,data=Positions,sum)
  Buy_df$`EOD Quantity`               <-  qty_eod_agg$qty[match(Buy_df$Temp,qty_eod_agg$symbol)]
  Buy_df$"Last Close Price"           <-  Price_Avg$priceClose[match(Buy_df$Temp,Price_Avg$symbol)]
  Buy_df                              <-  Buy_df[,c(1,2,3,4,5,6,9,7)]
}

if(length(Sell_tickers)>0)
{
  Sell_df$Name                         <-  Tb1$Desc[match(Sell_df$Ticker,Tb1$Symbol)]
  Sell_df$`Deal Captain`               <-  toupper(Tb1$Analyst[match(Sell_df$Ticker,Tb1$Symbol)])        
  qty_bought_agg                       <-  aggregate(Done~Symbol,data=Tb1[Tb1$Side=='Sell'&Tb1$Currency=='USD',],sum)
  Sell_df$`Quantity Bought`            <-  qty_bought_agg$Done[match(Sell_df$Ticker,qty_bought_agg$Symbol)]
  Net_money_agg                        <-  aggregate(`Net Money (USD)` ~Symbol,data=Tb1[Tb1$Side=='Sell'&Tb1$Currency=='USD',],sum)
  Sell_df$`Net Money ($)`              <-  Net_money_agg$`Net Money (USD)`[match(Sell_df$Ticker,Net_money_agg$Symbol)]
  Sell_df$`Average Bought Price ($)`   <-  Sell_df$`Net Money ($)`/Sell_df$`Quantity Bought`
  Sell_df$"Temp"                       <-  paste0(as.character(Sell_df$Ticker)," US")
  qty_eod_agg                          <-  aggregate(qty~symbol,data=Positions,sum)
  Sell_df$`EOD Quantity`               <-  qty_eod_agg$qty[match(Sell_df$Temp,qty_eod_agg$symbol)] 
  Sell_df$"Last Close Price"           <-  Price_Avg$priceClose[match(Sell_df$Temp,Price_Avg$symbol)]
  Sell_df                              <-  Sell_df[,c(1,2,3,4,5,6,9,7)]
}

if(length(Short_tickers)>0)
{
  Short_df$Name                         <-  Tb1$Desc[match(Short_df$Ticker,Tb1$Symbol)]
  Short_df$`Deal Captain`               <-  toupper(Tb1$Analyst[match(Short_df$Ticker,Tb1$Symbol)])
  qty_bought_agg                       <-  aggregate(Done~Symbol,data=Tb1[Tb1$Side=='Short'&Tb1$Currency=='USD',],sum)
  Short_df$`Quantity Bought`            <-  qty_bought_agg$Done[match(Short_df$Ticker,qty_bought_agg$Symbol)]
  Net_money_agg                        <-  aggregate(`Net Money (USD)` ~Symbol,data=Tb1[Tb1$Side=='Short'&Tb1$Currency=='USD',],sum)
  Short_df$`Net Money ($)`              <-  Net_money_agg$`Net Money (USD)`[match(Short_df$Ticker,Net_money_agg$Symbol)]
  Short_df$`Average Bought Price ($)`   <-  Short_df$`Net Money ($)`/Short_df$`Quantity Bought`
  Short_df$"Temp"                       <-  paste0(as.character(Short_df$Ticker)," US")
  qty_eod_agg                          <-  aggregate(qty~symbol,data=Positions,sum)
  Short_df$`EOD Quantity`               <-  qty_eod_agg$qty[match(Short_df$Temp,qty_eod_agg$symbol)] 
  Short_df$"Last Close Price"           <-  Price_Avg$priceClose[match(Short_df$Temp,Price_Avg$symbol)]
  Short_df                              <-  Short_df[,c(1,2,3,4,5,6,9,7)]
}

if(length(Cover_tickers)>0)
{
  Cover_df$Name                         <-  Tb1$Desc[match(Cover_df$Ticker,Tb1$Symbol)]
  Cover_df$`Deal Captain`               <-  toupper(Tb1$Analyst[match(Cover_df$Ticker,Tb1$Symbol)])
  qty_bought_agg                       <-  aggregate(Done~Symbol,data=Tb1[Tb1$Side=='Cover'&Tb1$Currency=='USD',],sum)
  Cover_df$`Quantity Bought`            <-  qty_bought_agg$Done[match(Cover_df$Ticker,qty_bought_agg$Symbol)]
  Net_money_agg                        <-  aggregate(`Net Money (USD)` ~Symbol,data=Tb1[Tb1$Side=='Cover'&Tb1$Currency=='USD',],sum)
  Cover_df$`Net Money ($)`              <-  Net_money_agg$`Net Money (USD)`[match(Cover_df$Ticker,Net_money_agg$Symbol)]
  Cover_df$`Average Bought Price ($)`   <-  Cover_df$`Net Money ($)`/Cover_df$`Quantity Bought`
  Cover_df$"Temp"                       <-  paste0(as.character(Cover_df$Ticker)," US")
  qty_eod_agg                          <-  aggregate(qty~symbol,data=Positions,sum)
  Cover_df$`EOD Quantity`               <-  qty_eod_agg$qty[match(Cover_df$Temp,qty_eod_agg$symbol)] 
  Cover_df$"Last Close Price"           <-  Price_Avg$priceClose[match(Cover_df$Temp,Price_Avg$symbol)]
  Cover_df                              <-  Cover_df[,c(1,2,3,4,5,6,9,7)]
}

###Exposure tab
Tickers_Long$`Gross Exposure`        <- as.numeric(Tickers_Long$`Gross Exposure`)
Long_agg                             <- aggregate(`Gross Exposure`~Sector, data=Tickers_Long,sum)
Long_agg                             <- Long_agg[Long_agg$Sector!="",]
Long_agg$"Exposure as % of AUM"      <- Long_agg$`Gross Exposure`/AUM_total
Total_Exposure                       <- sum(Long_agg$`Gross Exposure`)
Long_agg$"Exposure as % of Total"    <- Long_agg$`Gross Exposure`/Total_Exposure
names(Long_agg)[2]                   <- "Gross $ Exposure"


sectors<- c('Consumer Discretionary','Consumer Staples','Energy','Financials','Health Care','Industrials','Information Technology','Materials','Real Estate','Telecommunication Services','Utilities')
sectors_Df<-data.frame("Sector"=sectors,"C1"="0","C2"="0","C3"="0")
sectors_Df<-sectors_Df[!sectors_Df$Sector%in%Long_agg$Sector,]
names(sectors_Df)<-names(Long_agg)
Long_agg<-rbind(Long_agg,sectors_Df)

Tickers_Long_top10                   <- Tickers_Long[,c(1,13,11,12)]
Tickers_Long_top10                   <- Tickers_Long_top10[order(Tickers_Long_top10$`Gross Exposure`,decreasing = T),]
Tickers_Long_top10                   <- Tickers_Long_top10[1:10,]



if(length(Buy_tickers)>0)
{
  Buy_df[is.na(Buy_df)]    <-  ""
  Buy_df$`Last Close Price`           <-  as.numeric(Buy_df$`Last Close Price`)
}
if(length(Sell_tickers)>0)
{
  Sell_df[is.na(Sell_df)]    <-  ""
  Sell_df$`Last Close Price`           <-  as.numeric(Sell_df$`Last Close Price`)
}
if(length(Short_tickers)>0)
{
  Short_df[is.na(Short_df)]    <-  ""
  Short_df$`Last Close Price`           <-  as.numeric(Short_df$`Last Close Price`)
}
if(length(Cover_tickers)>0)
{
  Cover_df[is.na(Cover_df)]    <-  ""
  Cover_df$`Last Close Price`           <-  as.numeric(Cover_df$`Last Close Price`)
}

Tickers_Long_top10[is.na(Tickers_Long_top10)]    <-  ""
Long_agg[is.na(Long_agg)]    <-  ""


###Wrtiting output files
if(length(Buy_tickers)>0)
{
  write.csv(Buy_df,"Output Files/Recent Trades_Buy.csv",row.names = F, na="")
}else{
  message("There are no buy trades for the day")
}

if(length(Sell_tickers)>0)
{
  write.csv(Sell_df,"Output Files/Recent Trades_Sell.csv",row.names = F, na="")
}else{
  
  message("There are no Sell trades for the day")
}


if(length(Short_tickers)>0)
{
  write.csv(Short_df,"Output Files/Recent Trades_Short.csv",row.names = F, na="")
}else{
  message("There are no Short trades for the day")
}


if(length(Cover_tickers)>0)
{
  write.csv(Cover_df,"Output Files/Recent Trades_Cover.csv",row.names = F, na="")
} else{
  message("There are no Cover trades for the day")
} 

Long_agg$`Gross $ Exposure`<-as.numeric(Long_agg$`Gross $ Exposure`)
Long_agg$`Exposure as % of AUM`<-as.numeric(Long_agg$`Exposure as % of AUM`)
Long_agg$`Exposure as % of Total`<-as.numeric(Long_agg$`Exposure as % of Total`)


write.csv(Tickers_Long_top10,"Output Files/Top_10_Stocks_Exposure.csv",row.names = F, na="")
write.csv(Long_agg,"Output Files/Sector_Exposure.csv",row.names = F, na="")

## Delete files in Input Files folder
##AWS
#do.call(file.remove, list(list.files("/home/ghcadmin/M_Daily_Report/Input Files", full.names = TRUE)))

## Clear the console screen
#cat("\014") 

# Writing Excel code 
# write.xlsx(Tickers_Long_Final,paste0("Output Files/Output_",as.character(date_curr),".xlsx"),sheetName = "Long",col.names = TRUE,row.names = F,append = F,showNA = FALSE)
# write.xlsx(Tickers_Short_Final,paste0("Output Files/Output_",as.character(date_curr),".xlsx"),sheetName = "Short",col.names = TRUE,row.names = F,append = T,showNA = FALSE)
# if(length(Buy_tickers)>0)
# {
#   write.xlsx(Buy_df,paste0("Output Files/Output_",as.character(date_curr),".xlsx"),sheetName = "Recent_Trades_Buy",col.names = TRUE,row.names = F,append = T,showNA = FALSE)
# }else{
#   dlgMessage("There are no buy trades for the day",type=c("ok"))
# }
# 
# if(length(Sell_tickers)>0)
# {
#   write.xlsx(Sell_df,paste0("Output Files/Output_",as.character(date_curr),".xlsx"),sheetName = "Recent_Trades_Sell",col.names = TRUE,row.names = F,append = T,showNA = FALSE)
# }else{
#   
#   dlgMessage("There are no Sell trades for the day",type=c("ok"))
# }
# 
# if(length(Short_tickers)>0)
# {
#   write.xlsx(Short_df,paste0("Output Files/Output_",as.character(date_curr),".xlsx"),sheetName = "Recent_Trades_Short",col.names = TRUE,row.names = F,append = T,showNA = FALSE)
# }else{
#   dlgMessage("There are no Short trades for the day",type=c("ok"))
# }
# 
# if(length(Cover_tickers)>0)
# {
#   write.xlsx(Cover_df,paste0("Output Files/Output_",as.character(date_curr),".xlsx"),sheetName = "Recent_Trades_Cover",col.names = TRUE,row.names = F,append = T,showNA = FALSE)
# }else{
#   dlgMessage("There are no Cover trades for the day",type=c("ok"))
# } 
# write.xlsx(Long_agg,paste0("Output Files/Output_",as.character(date_curr),".xlsx"),sheetName = "Sector Exposure",col.names = TRUE,row.names = F,append = T,showNA = FALSE)
# write.xlsx(Tickers_Long_top10,paste0("Output Files/Output_",as.character(date_curr),".xlsx"),sheetName = "Top_10_Long",col.names = TRUE,row.names = F,append = T,showNA = FALSE)
