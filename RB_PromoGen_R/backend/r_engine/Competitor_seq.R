include_comp_promo <- function(nielsen,EDA,HEA_start_date = as.Date("2017-07-22"),brand,retailer_end_day,retailer){
  source("pattern_detection.R") ##Sourcing pattern detection function
  
  HEA_tesco_start_date <- HEA_start_date + 3
  retailer_week_end_day = retailer_end_day$`Week Ending Day`[retailer_end_day$Retailer == retailer]
  
  ##Step1 - Finding brands in Nielsen corresponding to top 95% sales
  nielsen$`Week End Date` <- as.Date(nielsen$`Week End Date`)
  nielsen_52 <- nielsen[nielsen$`Week End Date` >= HEA_start_date,]
  nielsen_52_Brand <- nielsen_52[,list("Value Sales" = sum(Value)),by = list(Brand)]
  nielsen_52_Brand$`Value Share` <- nielsen_52_Brand$`Value Sales`*100/sum(nielsen_52_Brand$`Value Sales`)
  nielsen_52_Brand <- nielsen_52_Brand[order(-`Value Share`),]
  nielsen_52_Brand$`Cum Value Share` <- cumsum(nielsen_52_Brand$`Value Share`)
  
  if(nrow(nielsen_52_Brand[nielsen_52_Brand$`Cum Value Share` <= 95,]) == 1){
    nielsen_52_Brand_90 <- nielsen_52_Brand[c(1:2),]  ##Taking top two brands if single brand corresponds to 95%
  }else{
    nielsen_52_Brand_90 <- nielsen_52_Brand[nielsen_52_Brand$`Cum Value Share` <= 95,]  ##Brands > 90% value share
  }
  
  ##Step2 - Getting latest week for each Item in Nielsen data(Useful for latest week "5th Largest Regular Price")
  nielsen_52_price <- nielsen_52[order(-as.Date(`Week End Date`)),head(.SD,1),by = ITEM_NAME]
  
  ##Filtering EDA data for brands corresponding to top 95% sales and join with nielsen to get latest week price for each Item
  EDA_Brand_req <- EDA[EDA$BRAND %in% unique(nielsen_52_Brand_90$Brand),]
  EDA_Brand_req_RSP <- left_join(EDA_Brand_req,nielsen_52_price[,c("ITEM_NAME","5th Largest Regular Price")],by = c("Modeled Item Description"="ITEM_NAME"))
  
  ##Step3 - Rounding the latest price to nearest integer
  EDA_Brand_req_RSP$RSP <- round(EDA_Brand_req_RSP$`5th Largest Regular Price`,0)
  
  ###Cleaning the date - Changing "NOT APPLICABLE", "NOT STATED" values present in "WEIGHT VOLUME BASE" to NA
  if(nrow(EDA_Brand_req_RSP[is.na(EDA_Brand_req_RSP$`WEIGHT VOLUME BASE`),]) != 0){
    EDA_Brand_req_RSP[is.na(EDA_Brand_req_RSP$`WEIGHT VOLUME BASE`),]$`WEIGHT VOLUME BASE` <- "NA"
  }
  if(nrow(EDA_Brand_req_RSP[(EDA_Brand_req_RSP$`WEIGHT VOLUME BASE` == "NOT APPLICABLE" | EDA_Brand_req_RSP$`WEIGHT VOLUME BASE` == "NOT STATED"),]) != 0){
    EDA_Brand_req_RSP[(EDA_Brand_req_RSP$`WEIGHT VOLUME BASE` == "NOT APPLICABLE" | EDA_Brand_req_RSP$`WEIGHT VOLUME BASE` == "NOT STATED"),]$`WEIGHT VOLUME BASE` <- "NA"
  }
  
  ###Step4 - Creating PPGs for selected brands - combining "Market","BRAND","BRAND 2 - EXTENSION", "BRAND 3 - ITEM IDENTIFIER","CATEGORY","MANUFACTURER","SUPER GROUP","TRADING COMPANY","WEIGHT VOLUME BASE","RSP"
  EDA_Brand_req_RSP$`PPG Name`<- paste(EDA_Brand_req_RSP$Market,EDA_Brand_req_RSP$BRAND,EDA_Brand_req_RSP$`BRAND 2 - EXTENSION`,EDA_Brand_req_RSP$`BRAND 3 - ITEM IDENTIFIER`,
                                       EDA_Brand_req_RSP$CATEGORY,EDA_Brand_req_RSP$MANUFACTURER,EDA_Brand_req_RSP$`SUPER GROUP`,EDA_Brand_req_RSP$`TRADING COMPANY`,EDA_Brand_req_RSP$`WEIGHT VOLUME BASE`,EDA_Brand_req_RSP$RSP,sep = "_")
  
  
  ###Step5 - Taking only Competition brands
  EDA_Brand_other <- EDA_Brand_req_RSP[EDA_Brand_req_RSP$BRAND != (brand),]
  ###Creating short PPG names
  unique_PPG <- data.frame("PPG Name" = unique(EDA_Brand_other$`PPG Name`), "PPG_Comp" = paste0("PPG",c(1:length(unique(EDA_Brand_other$`PPG Name`)))),check.names = FALSE)
  
  ##Cleaning the weight and RSP columns
  EDA_Brand_other <- left_join(EDA_Brand_other,unique_PPG,by = "PPG Name")
  EDA_Brand_other$weight <- EDA_Brand_other$`WEIGHT VOLUME BASE`
  EDA_Brand_other$weight <- gsub("ML","",EDA_Brand_other$weight)
  EDA_Brand_other$weight <- gsub("G","",EDA_Brand_other$weight)
  EDA_Brand_other$weight <- gsub("NA",0,EDA_Brand_other$weight)
  EDA_Brand_other$weight <- as.numeric(EDA_Brand_other$weight)
  
  ###Step6 - Competitor Mapping for Airwick PPG
  EDA_own_brand <- unique(EDA_Brand_req_RSP[EDA_Brand_req_RSP$BRAND == (brand),c("Modeled Item Description","Market","BRAND","BRAND 2 - EXTENSION","BRAND 3 - ITEM IDENTIFIER","CATEGORY",
                                                                                 "MANUFACTURER","SUPER GROUP","TRADING COMPANY","WEIGHT VOLUME BASE","RSP")])
  nielsen_52[,"Total_Units" := sum(Units),list(by = ITEM_NAME)]
  EDA_own_brand <- left_join(EDA_own_brand,unique(nielsen_52[,c("ITEM_NAME","PPG","EAN Code","Total_Units")]),by = c("Modeled Item Description"="ITEM_NAME"))
  EDA_own_brand$weight <- EDA_own_brand$`WEIGHT VOLUME BASE`
  EDA_own_brand$weight <- gsub("ML","",EDA_own_brand$weight)
  EDA_own_brand$weight <- gsub("G","",EDA_own_brand$weight)
  EDA_own_brand$weight <- gsub("NA",0,EDA_own_brand$weight)
  EDA_own_brand$weight <- as.numeric(EDA_own_brand$weight)
  
  EDA_own_brand_PPG <- setDT(EDA_own_brand)[,list("Total_Units" = sum(Total_Units),"RSP" = sum(Total_Units * RSP)/sum(Total_Units),"weight" = sum(Total_Units * weight)/sum(Total_Units)),by = list(`BRAND 3 - ITEM IDENTIFIER`,CATEGORY,`SUPER GROUP`,PPG)]
  
  PPG_Mapping <- data.frame()
  
  for(ppg in unique(EDA_own_brand_PPG$PPG)){
    #print(ppg)
    ppg_aw <- EDA_own_brand_PPG[EDA_own_brand_PPG$PPG == ppg,]
    ppg_other <- EDA_Brand_other[EDA_Brand_other$`BRAND 3 - ITEM IDENTIFIER` %in% unique(ppg_aw$`BRAND 3 - ITEM IDENTIFIER`) & EDA_Brand_other$CATEGORY %in% unique(ppg_aw$CATEGORY) & EDA_Brand_other$`SUPER GROUP` %in% unique(ppg_aw$`SUPER GROUP`) & 
                                   (EDA_Brand_other$weight >= (0 * unique(ppg_aw$weight)) & EDA_Brand_other$weight <= (5 * unique(ppg_aw$weight))) & (EDA_Brand_other$RSP >= (0 * unique(ppg_aw$RSP)) & EDA_Brand_other$RSP <= (5 * unique(ppg_aw$RSP))),]
    
    ppg_other$PPG_Comp <- as.character(ppg_other$PPG_Comp)
    
    tmp <- data.frame("Competitor PPG" = unique(ppg_other$PPG_Comp), "RB PPG" = rep(ppg,length(unique(ppg_other$PPG_Comp))),check.names = FALSE)
    PPG_Mapping <- rbind(PPG_Mapping,tmp) 
  }
  
  ##step7 - Pattern Detection function
  #Step 7.1 - Getting HEA flag
  ##Item Level
  Item_Event_full <- left_join(nielsen_52[,c("ITEM_NAME","Week End Date","Flag_HEA","Units")],EDA_Brand_other, by = c("ITEM_NAME" = "Modeled Item Description"))
  Item_Event_full <- Item_Event_full[!(is.na(Item_Event_full$PPG_Comp)),]
  
  ###PPG Level
  PPG_Event <- setDT(Item_Event_full)[,list("Flag_HEA" = sum(Flag_HEA), "Total_Units" = sum(Units)),by = list(PPG_Comp,`Week End Date`)]
  PPG_Event$Flag_HEA <- ifelse(PPG_Event$Flag_HEA >= 1,1,0)
  Comp_PPG_Units_mapping <- PPG_Event[,list("Total_Units" = sum(Total_Units)),by = list(PPG_Comp)]
  
  setnames(PPG_Event,"PPG_Comp","ITEM_NAME")
  
  event_gap <- data.frame()
  ###To run pattern detection at Item level, replace PPG_Event by Item_Event_full
  for(item in unique(PPG_Event$ITEM_NAME)){
    #print(item)
    Item_Event <- PPG_Event[PPG_Event$ITEM_NAME == item,]
    Item_Event$`Week End Date` <- as.Date(Item_Event$`Week End Date`)
    # Item_Event <- setDT(Item_Event)[,list("Flag_HEA" = sum(Flag_HEA)),by = list(ITEM_NAME,`Week End Date`)]
    # Item_Event$Flag_HEA <- ifelse(Item_Event$Flag_HEA > 0, 1,0)
    Item_Event <- Item_Event[order(`Week End Date`),]
    Item_Event$Event_gap <- (Item_Event$Flag_HEA - lag(Item_Event$Flag_HEA))
    Item_Event[is.na(Item_Event)] <- 0
    
    Item_Event_gap <- rbind(Item_Event[1,],Item_Event[Item_Event$Event_gap != 0,])
    Item_Event_gap$`Week End Date` <- as.Date(Item_Event_gap$`Week End Date`)
    Item_Event_gap$Week_gap <- as.numeric((lead(Item_Event_gap$`Week End Date`) - Item_Event_gap$`Week End Date`)/7)
    
    #Check
    # if(Item_Event_gap$Flag_HEA[nrow(Item_Event_gap)] == Item_Event_gap$Flag_HEA[nrow(Item_Event_gap)-1]){
    #   Item_Event_gap$Week_gap[nrow(Item_Event_gap)-1] <- Item_Event_gap$Week_gap[nrow(Item_Event_gap)-1] + 1 
    # }
    # 
    # Item_Event_gap <- Item_Event_gap[c(1:(nrow(Item_Event_gap)-1)),]
    Item_Event_gap$Promo <- ifelse(Item_Event_gap$Flag_HEA == 1, "P","NP")
    
    Item_Event_gap_1 <- Item_Event_gap[Item_Event_gap$`Week End Date` >= (Item_Event_gap[Item_Event_gap$Promo == "P",]$`Week End Date`[1]) &
                                         Item_Event_gap$`Week End Date` <= Item_Event_gap$`Week End Date`[(length(Item_Event_gap$`Week End Date`)-1)],]
    
    pattern <- combinations(paste(Item_Event_gap_1$Week_gap,collapse = ","))
    tmp <- data.frame("ITEM_NAME" = unique(Item_Event_gap$ITEM_NAME),"Weekly_Pattern" = paste(Item_Event$Flag_HEA,collapse = ","),"Week_gap" = paste(Item_Event_gap$Week_gap,collapse = ","), "Promo" = paste(Item_Event_gap$Promo,collapse = ","),"Week_gap_checked" = paste(Item_Event_gap_1$Week_gap,collapse = ","), "Pattern" = paste(pattern,collapse = ","))
    
    event_gap <- rbind(event_gap, tmp)
  }
  
  ###Step8 - Mapping the competition for PPG's with pattern
  ppg_pattern <- event_gap[event_gap$Pattern != "No Pattern",]
  
  if(nrow(ppg_pattern) != 0 & nrow(PPG_Mapping) != 0){
    PPG_Mapping_pattern <- left_join(PPG_Mapping,ppg_pattern,by = c("Competitor PPG"="ITEM_NAME"))
    
    PPG_Mapping_pattern <- PPG_Mapping_pattern[!(is.na(PPG_Mapping_pattern$Pattern)),]
    PPG_Mapping_pattern <- left_join(PPG_Mapping_pattern,unique(EDA_Brand_other[,c("PPG_Comp","PPG Name")]), by = c("Competitor PPG" = "PPG_Comp"))
    PPG_Mapping_pattern <- left_join(PPG_Mapping_pattern,Comp_PPG_Units_mapping,by = c("Competitor PPG" = "PPG_Comp"))
    if(nrow(PPG_Mapping_pattern) != 0){
      final_pattern <- data.frame()
      for (ppg in unique(PPG_Mapping_pattern$`RB PPG`)) {
        ###Taking highest sales competition ppg
        PPG_Mapping_pattern_tmp <- PPG_Mapping_pattern[PPG_Mapping_pattern$`RB PPG` == ppg,]
        if(nrow(PPG_Mapping_pattern_tmp) > 1){
          PPG_Mapping_pattern_tmp <- PPG_Mapping_pattern[(-order("Total_Units")),]
          PPG_Mapping_pattern_tmp <- PPG_Mapping_pattern_tmp[1,]
        }
        weekly_pattern <- as.numeric(strsplit(as.vector(unique(PPG_Mapping_pattern_tmp$Weekly_Pattern)),",")[[1]])
        weekly_pattern_tesco <- tesco_event_num(weekly_pattern,retailer_week_end_day)
        tmp <- data.frame("Date" =  seq(HEA_tesco_start_date,HEA_tesco_start_date+(length(weekly_pattern_tesco)-1)*7,by = "week"),"Pattern" = weekly_pattern_tesco)
        tmp$PPG <- rep(as.character(ppg),nrow(tmp))
        final_pattern <- rbind(final_pattern,tmp)
      }
      
      #seq((final_pattern$Date + years(1) - 4), (final_pattern$Date + years(1)),by = "day")[which(weekdays(seq((final_pattern$Date + years(1) - 4), (final_pattern$Date + years(1)),by = "day")) == "Tuesday")]
      final_pattern$Date_next_year <- as.Date(ifelse(year(final_pattern$Date) == 2018,(final_pattern$Date + years(1) - 1),(final_pattern$Date + years(2) - 2)),origin)
      repeat_func <- function(x){
        tot <- c(0,0)
        for (i in c(1:x)) {
          tot = c(tot,rep(i,3))
        }
        return(tot)
      }
      
      slot_mapping <- data.frame("Date" = seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "week"),"Slot No" = repeat_func(17),check.names = FALSE)
      final_pattern <- data.table(left_join(final_pattern,slot_mapping,by = c("Date_next_year" = "Date")))
      final_pattern <- final_pattern[order(final_pattern$PPG,final_pattern$Date_next_year),]
      final_pattern_slot <- data.frame()
      for(j in unique(final_pattern$PPG)){
        for(i in unique(final_pattern$`Slot No`)){
          tmp <- final_pattern[final_pattern$`Slot No` == i & final_pattern$PPG == j,]
          if(any(tmp$Pattern == 1)){
            if(all(tmp$Pattern == 1)){
              flag = 1
            }else if(tmp$Pattern[1] == 0){
              flag = 1
            }else{
              flag = 0
            }
          }else{
            flag = 0
          }
          tmp_df <- data.frame("PPG" = j,"Date" = max(tmp$Date_next_year), "Slot No" = i,check.names = FALSE)
          tmp_df$Comp_Promo_Flag <- flag
          
          final_pattern_slot <- rbind(tmp_df,final_pattern_slot)
        }
        #browser()
      }
      return(final_pattern_slot)
    }else{
      final_pattern_slot <- data.frame(matrix(numeric(),ncol = 4),check.names = FALSE)
      names(final_pattern_slot) <- c("PPG","Date","Slot No","Comp_Promo_Flag")
      final_pattern_slot$PPG <- as.character(final_pattern_slot$PPG) 
      return(final_pattern_slot)
    }
  }else{
    final_pattern_slot <- data.frame(matrix(numeric(),ncol = 4),check.names = FALSE)
    names(final_pattern_slot) <- c("PPG","Date","Slot No","Comp_Promo_Flag")
    final_pattern_slot$PPG <- as.character(final_pattern_slot$PPG) 
    return(final_pattern_slot)
  }
}
