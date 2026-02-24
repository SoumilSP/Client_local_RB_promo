###Data preparation
# Create broadcast function
broadcast = function(vector) {
  for (i in 1:length(vector)){
    if (vector[i] == "" | is.na(vector[i])) {
      vector[i] = vector[i-1]
    }
  }
  return(vector)
}

#Calculate Event Number per PPG

event_no_fun = function(tpr_flag, display_flag, Event_Num, k){
  
  # pad previous values for "start detection"
  tpr_flag     = c(0, tpr_flag)
  display_flag = c(0, display_flag)
  
  for (j in 2:length(tpr_flag)) {
    
    # start a new event if:
    # - TPR turns on (0->1), OR
    # - TPR stays on but display toggles (0<->1)
    new_start = (tpr_flag[j] == 1) && (
      tpr_flag[j-1] == 0 || (tpr_flag[j-1] == 1 && display_flag[j] != display_flag[j-1])
    )
    
    if (new_start) {
      Event_Num[j-1] = k
      k = k + 1
    } else if (tpr_flag[j] == 1 && tpr_flag[j-1] == 1) {
      Event_Num[j-1] = k - 1
    } else {
      Event_Num[j-1] = 0
    }
  }
  
  Event_Num
}

#Define regular price - 5th largest in 6 months


regPriceFun <- function(vector) {
  
  n_row <- length(vector)
  regPrice <- numeric(n_row)
  divisions <- ceiling(n_row / 24)
  
  for (i in seq_len(divisions)) {
    
    start <- 1 + 24 * (i - 1)
    end   <- min(24 * i, n_row)
    
    div <- vector[start:end]
    
    if (!anyNA(div)) {
      regPrice[start:end] <- sort(div, decreasing = TRUE)[5]
    } else {
      start2 <- max(1, n_row - 23)
      div2 <- vector[start2:n_row]
      
      regPrice[start:end] <- sort(div2, decreasing = TRUE)[5]
    }
  }
  
  regPrice
}

#Tesco Event Number Function
tesco_event_num = function(vec,ret_week_end_day){
  
  e_num = vector(mode = "numeric", length = 0)
  cell = vector(mode = "numeric", length = 0)
  
  if( ret_week_end_day == "Saturday" ){
    e_num = vec
  } else {
    
    for (i in 2:(length(vec)+1)){
      cell = c(0,vec,0)
      if( cell[i-1] == 0 & cell[i] != 0 & cell[i+1] == 0){
        e_num[i-1] = cell[i]
      } else if ( cell[i-1] != 0 & cell[i] != 0 & cell[i+1] == 0){
        e_num[i-1] = 0 
      } else {
        e_num[i-1] = cell[i]
      }
      
    } #for  
    
  }
  
  return(e_num)
  
} #function

#Tesco Base Units Function
tesco_base_units =function(vector,days_cur_week,days_nex_week){
  
  res = vector()
  
  for( i in 1:length(vector)){
    if( i != length(vector)){
      res[i] = (days_cur_week/7)*vector[i] + (days_nex_week/7)*vector[i+1]
    } else {
      res[i] = (days_cur_week/7)*vector[i]
    }
  }
  
  return(res)
}

#Event Multiplier Function
event_multiplier_fun = function(ppg_events, retailer_type = "Carrefour"){
  
  #ppg_df = display_events[PPG == "AA6_2"]
  ppg_df = copy(ppg_events)
  
  
  ppg_df[,Price_Chg_Idx := Promo_Price/RSP_Unit]
  
  ppg_df[,Inc_TPR_Only := ((Price_Chg_Idx^`Promoted Price Elasticity_Betas`)-1)* ACV_D_Unsupported*Base_Units/ACV_D]
  ppg_df[,Inc_TPR_Feature := ((Price_Chg_Idx^`Promoted Price Elasticity_Betas`)*`Feature Only_Betas`-1)*ACV_D_Feature_Only*Base_Units/ACV_D]
  ppg_df[,Inc_TPR_Display := ((Price_Chg_Idx^`Promoted Price Elasticity_Betas`)*`Display Only_Betas`-1)*ACV_D_Display_Only*Base_Units/ACV_D]
  ppg_df[,Inc_TPR_Feature_Display := ((Price_Chg_Idx^`Promoted Price Elasticity_Betas`)*`Feature and Display_Betas`-1)*ACV_D_Feature_Display*Base_Units/ACV_D]
  ppg_df[,Inc_Multibuy_Feature :=(`Total Multibuy Effect_Betas`*`Feature Only_Betas`- 1)*ACV_D_Multibuy_Feature *Base_Units/ACV_D]
  ppg_df[,Inc_Multibuy_Display := (`Total Multibuy Effect_Betas`*`Display Only_Betas` - 1)*ACV_D_Multibuy_Display*Base_Units/ACV_D]
  ppg_df[,Inc_Multibuy_Feature_Display := (`Total Multibuy Effect_Betas`*`Feature and Display_Betas` - 1)*ACV_D_Multibuy_Feature_Display*Base_Units/ACV_D]
  ppg_df[,Inc_Multibuy_Only := (`Total Multibuy Effect_Betas` - 1)*ACV_D_Total_Multibuy*Base_Units/ACV_D]    #addon
  
  ppg_df[,Inc_Display := Inc_TPR_Feature + Inc_TPR_Display + Inc_TPR_Feature_Display + Inc_Multibuy_Feature + Inc_Multibuy_Display +
           Inc_Multibuy_Feature_Display + Inc_Multibuy_Only]
  
  #Event_Multiplier = (paste0("Event_Multiplier",id)) ;  Event_Multipler_Display = paste0("Event_Multiplier_Display",id) ; Event_Multipler_Discount = paste0("Event_Multipler_Discount",id)
  
  
  ppg_df[,Total_Units_Sales := Base_Units + Inc_TPR_Only + Inc_Display]
  ppg_df[,Total_Inc_Units := Inc_Display + Inc_TPR_Only]
  
  if (toupper(retailer_type) == "TESCO") {
    # Tesco: use Tesco-aligned base units
    ppg_df[, Event_Multiplier_Tesco := Total_Inc_Units / Base_Units_Tesco]
    ppg_df[, Event_Multiplier_Display_Tesco := Inc_Display / Base_Units_Tesco]
    ppg_df[, Event_Multiplier_Discount_Tesco := Inc_TPR_Only / Base_Units_Tesco]
    
  
    
  } else {
    # Non-Tesco: use original base units
    ppg_df[, Event_Multiplier := Total_Inc_Units / Base_Units]
    ppg_df[, Event_Multiplier_Display := Inc_Display / Base_Units]
    ppg_df[, Event_Multiplier_Discount := Inc_TPR_Only / Base_Units]
    
    # Set Tesco multipliers to 0 or NA for non-Tesco (or skip if not needed)
    ppg_df[, Event_Multiplier_Tesco := 0]
    ppg_df[, Event_Multiplier_Display_Tesco := 0]
    ppg_df[, Event_Multiplier_Discount_Tesco := 0]
  }
  ppg_df[,Discount := (RSP_Unit - Promo_Price)/RSP_Unit]
  ppg_df[,FM_Abs := (RSP_Unit/(1+VAT)) - Net_Cost_Unit]
  ppg_df[,FM_Percent := FM_Abs*(1+VAT)/RSP_Unit]
  ppg_df[,Retro_Fund_Unit := 0.335]
  #ppg_df[,Retro_Fund_Total := Total_Units_Sales*Retro_Fund_Unit]
  ppg_df[,OID_Total := OID_Unit*Total_Units_Sales]                                         #addon
  ppg_df[,UNCR_Total := (RSP_Unit - Promo_Price)*Total_Units_Sales]                                       #addon
  ppg_df[,Gross_Sales := Promo_Price*Total_Units_Sales]  
  ppg_df[,Retro_Fund_Total := Gross_Sales*Retro_Fund_Unit]
  
  ppg_df[,Trade_Investment := UNCR_Total+Retro_Fund_Total]    #addon
  ppg_df[,Net_Revenue := Gross_Sales- Trade_Investment]                                    #addon
  ppg_df[,Net_Cost_Total := Total_Units_Sales*Net_Cost_Unit]
  ppg_df[,COGS_Total := COGS_Unit*Total_Units_Sales]
  ppg_df[,GM_Abs := Net_Revenue - COGS_Total]
  ppg_df[,NIS := Gross_Sales - UNCR_Total - OID_Total]                                     #addon
  ppg_df[,Inc_GM_Abs := Total_Inc_Units*GM_Abs/Total_Units_Sales]                     
  ppg_df[,Inc_Revenue := Total_Inc_Units*Net_Revenue/Total_Units_Sales]                       #addon
  ppg_df[,Inc_NIS := Total_Inc_Units*NIS/Total_Units_Sales]                                   #addon
  
  
  ppg_df[,ROI_GM := Inc_GM_Abs/Trade_Investment]                                       
  ppg_df[,ROI_Rev := Inc_Revenue/Trade_Investment]                                        #addon
  ppg_df[,ROI_NIS := Inc_NIS/Trade_Investment]                                            #addon
  
  
  #additional columns for roi ------------------------------------------------------------ new addition
  ppg_df[,R_Total_Sales := Base_Units + Total_Inc_Units]
  ppg_df[,R_Gross_Sales_Base := STP_Unit *Base_Units]
  ppg_df[,R_Gross_Sales_Inc := STP_Unit *Total_Inc_Units]
  ppg_df[,R_Gross_Sales_Total := R_Gross_Sales_Base + R_Gross_Sales_Inc]
  ppg_df[,R_UNCR_Base := (RSP_Unit - Promo_Price)*Base_Units]
  ppg_df[,R_UNCR_Inc := (RSP_Unit - Promo_Price)*Total_Inc_Units]
  ppg_df[,R_UNCR_Total := R_UNCR_Base + R_UNCR_Inc]
  ppg_df[,R_OID_Base := OID_Unit*Base_Units]
  ppg_df[,R_OID_Inc := OID_Unit*Total_Inc_Units]
  ppg_df[,R_OID_Total := R_OID_Base + R_OID_Inc]
  ppg_df[,R_Retro_Base := 0]
  ppg_df[,R_Retro_Inc := Retro_Fund_Unit*R_Total_Sales]
  ppg_df[,R_Retro_Total := R_Retro_Base + R_Retro_Inc]
  ppg_df[,R_NIS_Base := R_Gross_Sales_Base - R_UNCR_Base - R_OID_Base]
  ppg_df[,R_NIS_Inc := R_Gross_Sales_Inc - R_UNCR_Inc - R_OID_Inc]
  ppg_df[,R_NIS_Total := R_NIS_Base + R_NIS_Inc]
  ppg_df[,R_COGS_Base := COGS_Unit*Base_Units]
  ppg_df[,R_COGS_Inc := COGS_Unit*Total_Inc_Units]
  ppg_df[,R_COGS_Total := R_COGS_Base + R_COGS_Inc]
  
  #additional calculations for roi
  ppg_df[,R_Retro_Inc := R_Retro_Inc]
  ppg_df[,R_Retro_Base := R_Retro_Base]
  ppg_df[,R_Retro_Total := R_Retro_Total]
  ppg_df[,R_Display_Fee_Base := 0]
  ppg_df[,R_Display_Fee_Inc := Display_Cost]
  ppg_df[,R_Display_Fee_Total := R_Display_Fee_Base + R_Display_Fee_Inc]  
  ppg_df[,R_Total_Trade_Inv_Base := R_UNCR_Base + R_OID_Base + R_Retro_Base + 0]
  ppg_df[,R_Total_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + 0]
  ppg_df[,R_Total_Trade_Inv_Total := R_Total_Trade_Inv_Base + R_Total_Trade_Inv_Inc]
  ppg_df[,R_Net_Revenue_Base := R_NIS_Base - R_Retro_Base - R_Display_Fee_Base]
  ppg_df[,R_Net_Revenue_Inc := R_NIS_Inc - R_Retro_Inc - 0]
  ppg_df[,R_Net_Revenue_Total := R_Net_Revenue_Base + R_Net_Revenue_Inc]
  ppg_df[,R_GM_Base := R_Net_Revenue_Base - R_COGS_Base]
  ppg_df[,R_GM_Inc := R_Net_Revenue_Inc - R_COGS_Inc]
  ppg_df[,R_GM_Total := R_GM_Base + R_GM_Inc]
  
  #Calculating ROI
  ppg_df[,R_ROI_GM := R_GM_Inc/R_Total_Trade_Inv_Inc]
  ppg_df[,R_ROI_Rev := R_Net_Revenue_Inc/R_Total_Trade_Inv_Inc]
  ppg_df[,R_ROI_NIS := R_NIS_Inc/R_Total_Trade_Inv_Inc]
  
  #write.csv(ppg_df,"test.csv",row.names = F)
  
  return(list(ppg_df$Event_Multiplier, ppg_df$Event_Multiplier_Display, ppg_df$Event_Multiplier_Discount,
              ppg_df$ROI_GM,ppg_df$Event_Multiplier_Tesco, ppg_df$Event_Multiplier_Display_Tesco, 
              ppg_df$Event_Multiplier_Discount_Tesco, ppg_df$ROI_Rev,ppg_df$ROI_NIS,ppg_df$R_ROI_GM, 
              ppg_df$R_ROI_Rev, ppg_df$R_ROI_NIS))
  #return(ppg_df)
}

#Tesco inc units function
tesco_inc_units = function(nielsen_inc,tesco_hea){
  
  res = vector()
  
  #check for 0,1,0 sequence
  
  if( tesco_hea[1] == 0 ){
    res[1] = 0
  } else if( tesco_hea[1] == 1 & tesco_hea[2] == 0 ){
    res[1] = nielsen_inc[1]
  } else if( tesco_hea[1] == 1 & tesco_hea[2] == 1 ){
    res[1] = nielsen_inc[1] + (3/7)*nielsen_inc[2]
  }
  
  
  for( i in 2:(length(nielsen_inc) - 1)){
    if( tesco_hea[i-1] == 1 & tesco_hea[i] == 1 & tesco_hea[i+1] == 1 ){           #for between instance of event 1/1/1
      res[i] = (4/7)*nielsen_inc[i] + (3/7)*nielsen_inc[i+1]
    } else if( tesco_hea[i-1] == 0 & tesco_hea[i] == 1 & tesco_hea[i+1] == 1 ){    #for first instance of event 0/1/1
      res[i] = nielsen_inc[i] + (3/7)*nielsen_inc[i+1]
    } else if( tesco_hea[i-1] == 1 & tesco_hea[i] == 1 & tesco_hea[i+1] == 0 ){    #for last instance of event 1/1/0
      res[i] = (4/7)*nielsen_inc[i] + nielsen_inc[i+1]
    } else if( tesco_hea[i-1] == 0 & tesco_hea[i] == 1 & tesco_hea[i+1] == 0 ){    #for single instance of event 0/1/0
      res[i] = nielsen_inc[i] + nielsen_inc[i+1]
    } else{
      res[i] = 0
    }
  }
  
  if( tesco_hea[i+1] == 0 ) {
    res[i+1] = 0
  } else if( tesco_hea[i+1] == 1 ){
    res[i+1] = nielsen_inc[i+1]
  }
  
  return(res)
  
}

#Tesco display type function
tesco_display_type = function(disp_type,disp_flag){
  
  # x = tesco_week_cal[PPG == "AA6"]
  # disp_type = x$Display_Type
  # disp_flag = x$Flag_Display_HEA_Tesco
  # disp_cost = x$Display_Cost
  
  res = vector()
  cost = vector()
  
  #update inputs, to accomodate for last value
  disp_type = c(disp_type,"0")
  disp_flag = c(disp_flag,0)
  #disp_cost = c(disp_cost,0)
  
  
  #display type loop
  for(i in 1:(length(disp_flag)-1)){
    if( disp_flag[i] == 1 ){
      
      disp_events = disp_type[i:(i+1)]                                          #get consecutive display type
      disp_events = disp_events[disp_events != "0"]                             #remove null display events
      res[i] = ifelse(length(disp_events) == 0, "0", disp_events[1])
      
    } else {
      res[i] = "0"
    }
  }
  
  # #display cost loop
  # for(i in 1:(length(disp_flag)-1)){
  #   if( disp_flag[i] == 1 &  disp_flag[i+1] == 0 ){
  #    cost[i] = disp_cost[i] + disp_cost[i+1] 
  #   } else if( disp_flag[i] == 0 ){
  #     cost[i] = 0
  #   }else {
  #     cost[i] = disp_cost[i]
  #   }
  # }
  
  result = res
  
  return(result)
}

#Input files data prep
data_prep <- function(nielsen,model_results,event_r,maping,cost_bible,tesco_slots,ean_to_ldesc_map,dl_nl,lsm_new,promo_seq,retailer_end_day,retailer,manuf,brand,date_maping){
  source("Rounding_LSM_Function.R")
  options(warn = -1)
  shiny_ip_list <- list()
  
  # Preserve retailer as a local variable to avoid scoping issues
  RETAILER <- if (missing(retailer) || is.null(retailer)) "Carrefour" else retailer
  cat("DATA_PREP: RETAILER =", RETAILER, "\n")
  
  #------------------------Find Days to add/subtract for retailer Calendar----------------------------#
  days_to_add = data.table("WeekEndDay" = c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday"),
                           "DaysToAdd" = c(0,1,2,3,4,5,6))
  retailer_week_end_day = retailer_end_day$`Week Ending Day`[retailer_end_day$Retailer == RETAILER]
  
  days_to_add = days_to_add$DaysToAdd[days_to_add$WeekEndDay == retailer_week_end_day]
  
  days_in_current_week = 7 - days_to_add     #used later in calculation - TescoBaseUnits_Fun
  days_in_next_week = days_to_add            #used later in calculation - TescoBaseUnits_Fun
  
  
  weekday_no = data.table("WeekDay" = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                          "WeekDayNo" = c(1,2,3,4,5,6,7))
  retailer_weekEndDay_no = weekday_no$WeekDayNo[weekday_no$WeekDay == retailer_week_end_day]
  
  
  shiny_ip_retailer_week_end_day <- retailer_week_end_day
  shiny_ip_retailer_weekEndDay_no <- retailer_weekEndDay_no
  
  
  
  #--------------------------LSM NEW Formatting--------------------------------#
  
  names(lsm_new) = c("Country","Retailer Code","Retailer Description","PPG","Year","Currency (local)", "MRRP Max","MRRP Min",
                     "Promo Price", "Promo Price Tolerance","Max_Shelf_Weeks", "Min_Shelf_Weeks","Max_Display_Weeks","Min_Display_Weeks",
                     "Global_Floor_Price")
  
  
  
  #Apply rounding
  lsm_new = rounding_lsm(lsm_new,"Max_Shelf_Weeks","Max_Display_Weeks")
  lsm_new = rounding_lsm(lsm_new,"Min_Shelf_Weeks","Min_Display_Weeks")
  
  lsm_new[,Shelf_Remainder := NULL] ; lsm_new[,Display_Remainder := NULL]
  
  
  lsm_new[,Min_Total_Weeks := Min_Shelf_Weeks + Min_Display_Weeks]
  lsm_new[,Max_Total_Weeks := Max_Shelf_Weeks + Max_Display_Weeks]
  
  lsm_new[,Non_LSM_Min_Display_Weeks := ifelse(Min_Display_Weeks - 3 < 0, 0, Min_Display_Weeks - 3) ]
  lsm_new[,Non_LSM_Max_Display_Weeks := ifelse(Max_Display_Weeks + 3 > 27, 27, Max_Display_Weeks + 3) ]
  lsm_new[,Non_LSM_Min_Total_Weeks := ifelse(Min_Total_Weeks - 3 < 0, 0, Min_Total_Weeks - 3)]
  lsm_new[,Non_LSM_Max_Total_Weeks := ifelse(Max_Total_Weeks + 3 > 27, 27, Max_Total_Weeks + 3)]
  lsm_new[,LSM_Promo_Price_Min := (1 - `Promo Price Tolerance`)*`Promo Price`]
  #lsm_new[,LSM_Promo_Price_Max := latest week rsp ]   to be done later
  
  
  #-------------------------1 Nielsen / RMS Formatting----------------------
  
  #1.1 Formating
  retailer="Carrefour"
  # browser() # DISABLED for API
  nielsen = as.data.frame(nielsen)
  nielsen$FORMAT="ALL"
  nielsen$`ACV Distribution (C) (Unsupported)`=1
  maping$PPG_Parent="D77"
  names(nielsen)[9] = "Week End Date"
  nielsen = nielsen[-1,]
  nielsen1 = nielsen           #Creating a copy
  
  #1.2 Correct week end date
  nielsen1$`Week End Date` = dmy(nielsen1$`Week End Date`)
  
  #1.3 Broadcast columns 1 to 8
  nielsen1[,1:8] = sapply(nielsen1[,1:8], FUN = broadcast)
  
  nielsen1$ITEM = as.numeric(nielsen1$ITEM)   #convert item to numeric
  nielsen1 <- nielsen1 %>% distinct() 
  
  #Remove rows where item =NA
  nielsen1 = nielsen1[!is.na(nielsen1$ITEM),] 
  
  #1.4 Filter date - latest 104 weeks only
  latest_104_weeks = sort(unique(nielsen1$`Week End Date`), decreasing = T)[1:720]
  nielsen1 = subset(nielsen1, `Week End Date` %in% latest_104_weeks)
  rm(latest_104_weeks)
  
  #1.5 Filter Items with 0 Unit Sales in 2 years
  sales_104_weeks = aggregate(data = nielsen1, as.numeric(Units)~ITEM, sum)
  items_to_keep = sales_104_weeks[sales_104_weeks$`as.numeric(Units)` != 0,]
  nielsen1 = subset(nielsen1, ITEM %in% items_to_keep$ITEM)
  rm(items_to_keep)
  rm(sales_104_weeks)
  
  #1.6 Correct Column Names at 2 places - column R and T in raw file
  #correct ACV Distribution multibuy and display names
  
  nielsen1[,"ACV Distribution Multibuy_Feature_Display"] = nielsen1$`ACV Distribution (C) (multibuy and featu...24`
  nielsen1[,"ACV Distribution Multibuy_Feature"] = nielsen1$`ACV Distribution (C) (multibuy and featu...26`
  
  #1.7 Nielsen EAN to LDESC Maping
  # Fixed: Safely select only columns that exist
  ean_ldesc_cols_desired <- c("ItemID...1", "ITEM_NAME")
  ean_ldesc_cols <- intersect(ean_ldesc_cols_desired, names(ean_to_ldesc_map))
  if (length(ean_ldesc_cols) >= 2) {
    ean_to_ldesc = ean_to_ldesc_map[, ..ean_ldesc_cols]
    nielsen1 = merge(nielsen1, ean_to_ldesc, by.x = "ITEM", by.y = "ItemID...1", all.x = T)
    rm(ean_to_ldesc)
  } else {
    cat("Warning: ean_to_ldesc_map missing required columns\n")
    nielsen1$ITEM_NAME <- "All Others"
  }
  
  #Make NA as All Others
  nielsen1$ITEM_NAME[is.na(nielsen1$ITEM_NAME)] = "All Others"
  
  #Save File
  #write.csv(nielsen1, paste0(out_path,"1 RMS prep.csv"),row.names = F)
  
  
  #----------------------------------------------2 Model Results / EDA ---------------------------------------#
  
  #Add Model/Not Modeled Flag
  
  model_results[,"Flag_Modeled"] = 1
  model_results = model_results[,c("UPC_EAN","BasePrice","Total Regular Price Elasticity_","Regular Price Elasticity",
                                   "Reg Price Ratio Rest of Brand","Reg Price Ratio Rest of Manufacturer",
                                   "Reg Price Ratio Rest of Category","Promoted Price Elasticity","Display Only",
                                   "Feature Only","Feature and Display","Multi Buy","Total Multibuy Effect",
                                   "Modeled Item Description","Flag_Modeled"
  )]
  
  names(model_results)[3] = "Total Regular Price Elasticiy"
  
  names(model_results) = paste(names(model_results),"Betas",sep = "_")
  
  #2.3 Merging nielsen rms data and model results - Keeping all nielsen data
  
  #New Mapping
  nielsen_including_model_betas = merge(nielsen1,model_results,by.x = "ITEM_NAME", by.y = "Modeled Item Description_Betas",
                                        all.x = T)
  
  rm(nielsen1)
  
  #2.4 Convert character column to numeric
  nielsen_including_model_betas[,11:(ncol(nielsen_including_model_betas))] = 
    sapply(nielsen_including_model_betas[,11:(ncol(nielsen_including_model_betas) )], as.numeric)
  
  #replace na by 0
  nielsen_including_model_betas[is.na(nielsen_including_model_betas)] = 0
  
  #2.5 Calculate average price, base price - #Actual Price = Value/Units - #Regular Price = Base Value/Base Units
  
  nielsen_including_model_betas[,"Actual Price"] = nielsen_including_model_betas$Value/nielsen_including_model_betas$Units
  nielsen_including_model_betas[,"Regular Price"] = nielsen_including_model_betas$`Base Value`/nielsen_including_model_betas$`Base Units`
  
  
  #remove NA and Inf
  nielsen_including_model_betas[,c("Actual Price", "Regular Price")] = do.call(data.frame, 
                                                                               lapply(nielsen_including_model_betas[,c("Actual Price", "Regular Price")], 
                                                                                      function(x) {replace(x, is.infinite(x) | is.na(x), 0)   } ) )
  
  #rounding regular price and actual price
  nielsen_including_model_betas$`Regular Price` = round(nielsen_including_model_betas$`Regular Price`,1)
  
  #2.6) Define regular price - 5th largest in 6 months
  
  #sort data - first by ITEM then by Date
  sort_index = order(nielsen_including_model_betas$ITEM, nielsen_including_model_betas$`Week End Date`)
  nielsen_including_model_betas = nielsen_including_model_betas[sort_index,]
  rm(sort_index)
  
  nielsen_including_model_betas = data.table(nielsen_including_model_betas)
  # browser() # DISABLED for API
  
  
  # Fixed: Use character vector for by= instead of .() for Plumber API compatibility
  master_data = nielsen_including_model_betas[, `5th Largest Regular Price` := regPriceFun(`Regular Price`), by = "ITEM"]
  master_data = master_data[, `Mean Actual Units` := mean(Units), by = "ITEM"]
  master_data = as.data.frame(master_data)
  rm(nielsen_including_model_betas)
  
  # 2.8) Calculating Incremental Columns-###################################
  
  master_data[,"Price Change Index"] = master_data$`Actual Price`/master_data$`5th Largest Regular Price`
  head(unique(master_data$`Price Change Index`))
  
  #if price change index >= 1 or price change index <= -1 , make it 0
  master_data$`Price Change Index` = ifelse( master_data$`Price Change Index` >= 1 | master_data$`Price Change Index` <= -1, 0,
                                             master_data$`Price Change Index`)
  #if NA or Inf
  master_data$`Price Change Index` = replace(master_data$`Price Change Index`, is.infinite(master_data$`Price Change Index`) |
                                               is.na(master_data$`Price Change Index`), 0)
  
  master_data[,"Inc TPR only"] = (master_data$`Price Change Index`^master_data$`Promoted Price Elasticity_Betas` - 1)*
    master_data$`ACV Distribution (C) (Unsupported)`*master_data$`Base Units`/master_data$`ACV Distribution`
  
  master_data[,"Inc TPR + Feature Only"] = ((master_data$`Price Change Index`^master_data$`Promoted Price Elasticity_Betas`)*
                                              master_data$`Feature Only_Betas` - 1)*master_data$`ACV Distribution (C) (feature only)`*master_data$`Base Units`/master_data$`ACV Distribution`
  
  master_data[,"Inc TPR + Display Only"] = ((master_data$`Price Change Index`^master_data$`Promoted Price Elasticity_Betas`)*
                                              master_data$`Display Only_Betas`-1)*master_data$`ACV Distribution (C) (display only)`*master_data$`Base Units`/master_data$`ACV Distribution`
  
  master_data[,"TPR+Feature+Dsiplay"] = (master_data$`Price Change Index`^master_data$`Promoted Price Elasticity_Betas`*
                                           master_data$`Feature and Display_Betas`-1)*master_data$`ACV Distribution (C) (feature and displa`*master_data$`Base Units`/master_data$`ACV Distribution`
  
  master_data[,"Inc Multibuy + Feature Only"] = (master_data$`Total Multibuy Effect_Betas`*master_data$`Feature Only_Betas` - 1)*master_data$`ACV Distribution Multibuy_Feature`*
    master_data$`Base Units`/master_data$`ACV Distribution`
  
  
  master_data[,"Inc Multibuy + Display Only"] = (master_data$`Total Multibuy Effect_Betas`*master_data$`Display Only_Betas` - 1)*master_data$`ACV Distribution (C) (multibuy and displ`*
    master_data$`Base Units`/master_data$`ACV Distribution`
  
  
  master_data[,"Inc Multibuy + Feature + Display"] = (master_data$`Total Multibuy Effect_Betas`*master_data$`Feature and Display_Betas` - 1)*master_data$`ACV Distribution Multibuy_Feature_Display`*
    master_data$`Base Units`/master_data$`ACV Distribution`
  
  
  master_data[,"Inc Total Multibuy Only"] = (master_data$`Total Multibuy Effect_Betas` - 1)*master_data$`ACV Distribution (C) (total multibuy)`*
    master_data$`Base Units`/master_data$`ACV Distribution`
  
  
  #remove NA and Inf
  
  cols = c("Inc TPR only","Inc TPR + Feature Only","Inc TPR + Display Only","TPR+Feature+Dsiplay","Inc Multibuy + Feature Only","Inc Multibuy + Display Only",
           "Inc Multibuy + Feature + Display","Inc Total Multibuy Only")
  
  master_data[,cols] = do.call(data.frame, lapply(master_data[,cols],function(x) {replace(x, is.infinite(x) | is.na(x), 0)   } ) )
  
  master_data[,"Total Incremental Unit Sales"] =  master_data[,"Inc TPR only"]+master_data[,"Inc TPR + Feature Only"]+
    master_data[,"Inc TPR + Display Only"]+master_data[,"TPR+Feature+Dsiplay"]+master_data[,"Inc Multibuy + Feature Only"] +
    master_data[,"Inc Multibuy + Display Only"]  + master_data[,"Inc Multibuy + Feature + Display"] + master_data[,"Inc Total Multibuy Only"]
  
  master_data[,"Inc Display Only"] = master_data$`Total Incremental Unit Sales` - master_data$`Inc TPR only`         #addon
  
  #V.IMP
  Inc_cols = c("Inc TPR only","Inc Display Only","Total Incremental Unit Sales","TPR+Feature+Dsiplay")
  
  for (inc in Inc_cols) {
    master_data[,inc] = ifelse(master_data$Flag_Modeled_Betas == 0 ,0,
                               master_data[,inc])
    master_data[,inc] = ifelse(master_data[,inc] < 0, 0,
                               master_data[,inc])
  }
  
  rm(Inc_cols)
  
  master_data[,"Total Predicted Unit Sales"] = master_data$`Total Incremental Unit Sales` + master_data$`Base Units`
  
  #2.9) Calculate Abs Error, % Error, R-square and MAPE
  
  master_data[,"Diff (Pred - Actual)"] = master_data$`Total Predicted Unit Sales` - master_data$Units
  master_data[,"Abs Error %"] = abs(master_data$`Diff (Pred - Actual)`)*100/master_data$Units
  
  #calculating r-square and MAPE
  master_data[,"Actual - Mean(Actual)"] = master_data$Units - master_data$`Mean Actual Units`
  master_data[is.na(master_data)] = 0  #replacing NA with 0
  
  master_data = data.table(master_data)
  # Fixed: Use character vector for by= instead of .() for Plumber API compatibility
  master_data[, `MAPE %` := mean(`Abs Error %`), by = "ITEM"]
  master_data[, `R Square %` := cor(`Total Predicted Unit Sales`, Units)^2 * 100, by = "ITEM"]
  master_data = as.data.frame(master_data)
  
  #creating r square and MAPE summary for all sku's
  master_data$ITEM = as.factor(master_data$ITEM)
  sku_accuracy = aggregate(cbind(`MAPE %`,`R Square %`, Flag_Modeled_Betas)~ITEM ,data = master_data, mean,drop = F)
  
  rm(sku_accuracy)
  
  #3--------------------------------------------EAN to PPG Maping----------------------------------------------
  maping[,"Flag_PPG"] = 1
  
  #3.2) Map EAN to PPG
  master_data$ITEM = as.character(master_data$ITEM)   #making sure both have same class
  maping$EAN = as.character(maping$EAN)     #making sure both have same class
  master_ppg = merge(master_data,maping, by.x = "ITEM", by.y = "EAN", all.x = T)
  
  rm(master_data)
  
  master_ppg$Flag_PPG[is.na(master_ppg$Flag_PPG)] =0
  master_ppg$PPG[is.na(master_ppg$PPG)] = "All Others"
  master_ppg$PPG_Description[is.na(master_ppg$PPG_Description)] = "All Others"
  master_ppg$PPG_Parent[is.na(master_ppg$PPG_Parent)] = "All_Others"                     #addon
  
  
  #4------------------------------------------------RB FINANCIAL-----------------------------------------------
  
  cost_bible = data.table(cost_bible)
  cost_bible[,Flag_RB_Financial := 1]
  
  cat("cost_bible has", nrow(cost_bible), "rows\n")
  cat("cost_bible PPG values:", paste(head(cost_bible$PPG, 5), collapse=", "), "...\n")
  cat("master_ppg PPG_Parent values:", paste(head(unique(master_ppg$PPG_Parent), 5), collapse=", "), "...\n")
  
  #4.2) Calculation
  cost_bible[,Net_Cost_Case := BIP_Case - OID]
  cost_bible[,Net_Cost_Unit := Net_Cost_Case/No_Of_Units]
  cost_bible[,COGS_Unit := COGS_Case/No_Of_Units]
  cost_bible[,STP_Unit := STP_Case/No_Of_Units]
  cost_bible[,UNCR_Unit := (STP_Case - BIP_Case)/ No_Of_Units]   #addon
  cost_bible[,OID_Unit := OID/No_Of_Units]                       #addon
  
  #4.3) Join master_ppg with cost_bible
  master_cost = merge(master_ppg, cost_bible, by.x = "PPG_Parent", by.y = "PPG", all.x = T)         #change
  
  cat("master_cost after merge has", nrow(master_cost), "rows\n")
  cat("Flag_RB_Financial values after merge - 1:", sum(master_cost$Flag_RB_Financial == 1, na.rm=TRUE), ", NA:", sum(is.na(master_cost$Flag_RB_Financial)), "\n")
  
  rm(master_ppg)
  
  #4.4) Make NA 0
  master_cost[is.na(master_cost)] = 0
  
  #----------------------------------------------Calculations-----------------------------------------------------
  
  #5.1) Selecting Optimization Output and Non Optimization Output
  master_cost = as.data.table(master_cost)
  master_cost[,Row_ID := row.names(master_cost)]
  
  cat("master_cost before fund filtering has", nrow(master_cost), "rows\n")
  cat("Flag_Modeled_Betas==1:", sum(master_cost$Flag_Modeled_Betas == 1, na.rm=TRUE), "rows\n")
  cat("Flag_RB_Financial==1:", sum(master_cost$Flag_RB_Financial == 1, na.rm=TRUE), "rows\n")
  cat("Flag_PPG==1:", sum(master_cost$Flag_PPG == 1, na.rm=TRUE), "rows\n")
  
  fund = master_cost[Flag_Modeled_Betas == 1  & Flag_RB_Financial == 1 & Flag_PPG == 1]
  cat("fund after filtering has", nrow(fund), "rows\n")
  not_fund = master_cost[!(Row_ID %in% fund$Row_ID) ]
  
  fund[,Flag_funding := 1]
  not_fund[,Flag_funding := 0]
  
  #5.2) Calculate retro funding
  fund[,RSP_Unit := `5th Largest Regular Price` ]
  fund[,Promo_Price := `Actual Price`]                            
  fund[,FM_Abs_Unit_1 := (RSP_Unit/(1+VAT)) - Net_Cost_Unit]
  fund[,FM_Percent_1 := FM_Abs_Unit_1*(1+VAT)/RSP_Unit]
  fund[,Retro_Fund_Unit := 0.335]
  
  #fund[,Retro_Fund_Total := round(Retro_Fund_Unit*Units,2)]
  
  fund[,COGS_Total := COGS_Unit*Units]
  fund[,Net_Cost_Total := Net_Cost_Unit*Units]
  fund[,Gross_Sales := Promo_Price*Units]
  fund[,Retro_Fund_Total := round(Retro_Fund_Unit*Gross_Sales,2)]
  fund[,UNCR_Total := (RSP_Unit - Promo_Price)*Units]                 
  fund[,OID_Total := OID_Unit*Units]                   
  fund[,NIS := Gross_Sales - UNCR_Total - OID_Total]   
  
  #additional columns for roi
  
  fund[is.na(fund)] = 0
  
  
  #5.3) Calculation not fund
  not_fund[,RSP_Unit := `5th Largest Regular Price` ]
  not_fund[,Promo_Price := `Actual Price`]  
  
  not_fund[,COGS_Total := COGS_Unit*Units]
  not_fund[,Net_Cost_Total := Net_Cost_Unit*Units]
  not_fund[,Gross_Sales := Promo_Price*Units]
  not_fund[,UNCR_Total := (RSP_Unit - Promo_Price)*Units]                
  not_fund[,OID_Total := OID_Unit*Units]                    
  not_fund[,NIS := Gross_Sales - UNCR_Total - OID_Total]    
  not_fund[is.na(not_fund)] = 0
  
  master_fund = fund
  master_fund$`Week End Date` = ymd(master_fund$`Week End Date`)
  #6--------------------------------------------------MAP HEA RESULTS-------------------------------------------------
  
  #6.1 Reading event file
  #File read on the top
  # browser() # DISABLED for API
  event = as.data.table(event_r) #creating a copy
 
 
  event$"Week End Date"= event$Date
  #event$`Week End Date`= dmy(event$Date)
  event$"Week End Date"=as.Date(event$`Week End Date`)
  master_fund$ITEM=as.numeric(master_fund$ITEM)
  
  event$ITEM=as.numeric(event$ITEM)
  master_with_flag = data.table(merge(master_fund, event, all.x = T,by = c("ITEM","Week End Date")))
  
  master_with_flag[is.na(master_with_flag)] = 0
  master_with_flag$Flag_Display_HEA= as.numeric(master_with_flag$Flag_Display_HEA)
  
  
  #6.9) Create additional Flags
  #Create ACV Flag
  master_with_flag[,"Flag_ACV_C"] = ifelse(master_with_flag$`ACV Distribution (C) (any promo)` -0.3 >0,1,0)
  #Create Display flag
  master_with_flag[,"Display&Feature"] = master_with_flag$`ACV Distribution (C) (display only)` +
    master_with_flag$`ACV Distribution (C) (feature and displa` + master_with_flag$`ACV Distribution (C) (feature only)` 
  
  master_with_flag[,"Flag_F&D_C"] = ifelse(master_with_flag$`Display&Feature` -0.30 > 0, 1,0)
  
  #6.10) Create field Country and Retailer
  master_with_flag[,"Retailer"] = "Carrefour"
  master_with_flag[,"Country"] = "UAE"
  master_with_flag$Flag_Display_HEA=as.numeric(master_with_flag$Flag_Display_HEA)
  
  #6.11) Making sure retro funding is 0 where promotion was not there
  master_with_flag[,Retro_Fund_Total := Retro_Fund_Total*Flag_TPR_HEA]
  master_with_flag[,Retro_Fund_Unit := Retro_Fund_Unit*Flag_TPR_HEA]
  master_with_flag[,`Total Incremental Unit Sales` := `Total Incremental Unit Sales` * Flag_TPR_HEA]
  master_with_flag[,`Inc TPR only` := `Inc TPR only`*Flag_TPR_HEA]                                                     #addon
  master_with_flag[,`Inc Display Only` := `Inc Display Only`*Flag_Display_HEA]                                         #addon
  
  #additional calculations for roi
  #additional columns for roi
  master_with_flag[,R_Total_Sales := `Base Units` + `Total Incremental Unit Sales`]
  master_with_flag[,R_Gross_Sales_Base := STP_Unit *`Base Units`]
  master_with_flag[,R_Gross_Sales_Inc := STP_Unit *`Total Incremental Unit Sales`]
  master_with_flag[,R_Gross_Sales_Total := R_Gross_Sales_Base + R_Gross_Sales_Inc]
  master_with_flag[,R_UNCR_Base := (RSP_Unit - Promo_Price)*`Base Units`]
  master_with_flag[,R_UNCR_Inc := (RSP_Unit - Promo_Price)*`Total Incremental Unit Sales`]
  master_with_flag[,R_UNCR_Total := R_UNCR_Base + R_UNCR_Inc]
  master_with_flag[,R_OID_Base := OID_Unit*`Base Units`]
  master_with_flag[,R_OID_Inc := OID_Unit*`Total Incremental Unit Sales`]
  master_with_flag[,R_OID_Total := R_OID_Base + R_OID_Inc]
  master_with_flag[,R_Retro_Base := 0]
  master_with_flag[,R_Retro_Inc := Retro_Fund_Unit*R_Total_Sales]
  master_with_flag[,R_Retro_Inc := ifelse(Flag_Modeled_Betas == 1  & Flag_RB_Financial == 1 & Flag_PPG == 1, R_Retro_Inc,0)] # to avoid fund/not_fund
  master_with_flag[,R_Retro_Total := R_Retro_Base + R_Retro_Inc]
  master_with_flag[,R_NIS_Base := R_Gross_Sales_Base - R_UNCR_Base - R_OID_Base]
  master_with_flag[,R_NIS_Inc := R_Gross_Sales_Inc - R_UNCR_Inc - R_OID_Inc]
  master_with_flag[,R_NIS_Total := R_NIS_Base + R_NIS_Inc]
  master_with_flag[,R_COGS_Base := COGS_Unit*`Base Units`]
  master_with_flag[,R_COGS_Inc := COGS_Unit*`Total Incremental Unit Sales`]
  master_with_flag[,R_COGS_Total := R_COGS_Base + R_COGS_Inc]
  
  
  master_with_flag[,R_Retro_Inc := ifelse(`Total Incremental Unit Sales` !=0 ,R_Retro_Inc*Flag_TPR_HEA,0)]      #ifelse to avoid any sales when Incremental saless were negative
  master_with_flag[,R_Retro_Base := ifelse(`Total Incremental Unit Sales` !=0 ,R_Retro_Base*Flag_TPR_HEA,0)]
  master_with_flag[,R_Retro_Total := ifelse(`Total Incremental Unit Sales` !=0 ,R_Retro_Total*Flag_TPR_HEA,0)]
  
  master_with_flag[,R_Display_Fee_Base := 0]
  master_with_flag[,R_Display_Fee_Inc := `Net invetsments_HEA`]
  master_with_flag[,R_Display_Fee_Total := R_Display_Fee_Base + R_Display_Fee_Inc]  
  master_with_flag[,R_Total_Trade_Inv_Base := R_UNCR_Base + R_OID_Base + R_Retro_Base + R_Display_Fee_Base]
  master_with_flag[,R_Total_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Fee_Inc]
  master_with_flag[,R_Total_Trade_Inv_Total := R_Total_Trade_Inv_Base + R_Total_Trade_Inv_Inc]
  master_with_flag[,R_Net_Revenue_Base := R_NIS_Base - R_Retro_Base - R_Display_Fee_Base]
  master_with_flag[,R_Net_Revenue_Inc := R_NIS_Inc - R_Retro_Inc - R_Display_Fee_Inc]
  master_with_flag[,R_Net_Revenue_Total := R_Net_Revenue_Base + R_Net_Revenue_Inc]
  master_with_flag[,R_GM_Base := R_Net_Revenue_Base - R_COGS_Base]
  master_with_flag[,R_GM_Inc := R_Net_Revenue_Inc - R_COGS_Inc]
  master_with_flag[,R_GM_Total := R_GM_Base + R_GM_Inc]
  
  master_with_flag[,FM_Abs_Unit_2 := (Promo_Price/(1+VAT))-Net_Cost_Unit+Retro_Fund_Unit]
  master_with_flag[,FM_Abs_Unit_2 := ifelse(Promo_Price == 0, 0, FM_Abs_Unit_2)]   #making sure fm_abs_unit is not negative when promo_price is 0
  master_with_flag[,FM_Total := FM_Abs_Unit_2*Units]
  master_with_flag[,Retailer_Revenue := Units * Promo_Price]
  
  #6.12) Some Formatting
  master_with_flag[is.na(master_with_flag)] = 0
  master_with_flag[,RSP_Unit := ifelse(is.infinite(RSP_Unit),0,RSP_Unit)]
  master_with_flag[,Promo_Price := ifelse(is.infinite(Promo_Price),0,Promo_Price)]
  
  master_with_flag_2 = merge(master_with_flag, dl_nl, by.x ="ITEM" , by.y = "EAN", all.x = T)
  master_with_flag_2[is.na(master_with_flag_2)] = 0
  
  master_with_flag_1 = copy(master_with_flag_2)
  
  master_with_flag_2[, Trade_Investment := Retro_Fund_Total + `Net invetsments_HEA` + UNCR_Total + OID_Total]
  master_with_flag_2[, Net_Revenue := Gross_Sales - Trade_Investment]
  master_with_flag_2[, GM_Abs := Net_Revenue - COGS_Total]
  master_with_flag_2[,Inc_GM_Abs := `Total Incremental Unit Sales`*GM_Abs/Units]
  master_with_flag_2[,Inc_Revenue := `Total Incremental Unit Sales`*Net_Revenue/Units]
  master_with_flag_2[,Inc_NIS := `Total Incremental Unit Sales`*NIS/Units]
  
  master_with_flag_2[is.na(master_with_flag_2)] = 0
  
  shiny_ip_nielsen = master_with_flag_2
  
  
  #Calculate Elasticities for PPG, to be used to create Event List
  # Fixed: Safely select only columns that exist
  ppg_elasticity_cols_desired <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description", "Units",
                           "Total Regular Price Elasticiy_Betas", "Regular Price Elasticity_Betas",
                           "Reg Price Ratio Rest of Brand_Betas", "Reg Price Ratio Rest of Manufacturer_Betas",
                           "Reg Price Ratio Rest of Category_Betas", "Promoted Price Elasticity_Betas",
                           "Display Only_Betas", "Feature Only_Betas", "Feature and Display_Betas",
                           "Multi Buy_Betas", "Total Multibuy Effect_Betas")
  ppg_elasticity_cols <- intersect(ppg_elasticity_cols_desired, names(master_with_flag_2))
  cat("ppg_elasticity selecting", length(ppg_elasticity_cols), "of", length(ppg_elasticity_cols_desired), "desired columns\n")
  
  # Check if we have the required filter columns
  filter_cols <- c("Flag_RB_Financial", "Flag_Modeled_Betas", "Flag_HEA", "DL")
  has_filter_cols <- all(filter_cols %in% names(master_with_flag_2))
  
  if (length(ppg_elasticity_cols) > 0 && has_filter_cols) {
    ppg_elasticity = master_with_flag_2[(Flag_RB_Financial == 1 & Flag_Modeled_Betas == 1 & Flag_HEA == 1 & DL != 1), ..ppg_elasticity_cols]
    
    # Fixed: Use character vector for by= instead of .()
    ppg_elasticity_by_cols <- intersect(c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT"), names(ppg_elasticity))
    if (length(ppg_elasticity_by_cols) > 0 && "Units" %in% names(ppg_elasticity) && nrow(ppg_elasticity) > 0) {
      ppg_elasticity[, weight := Units / sum(Units), by = ppg_elasticity_by_cols]
      
      # Fixed: Use list() with explicit expressions instead of .() for aggregation
      ppg_elasticity_by_cols2 <- intersect(c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description"), names(ppg_elasticity))
      
      # Build aggregation dynamically based on available columns
      beta_cols <- c("Total Regular Price Elasticiy_Betas", "Regular Price Elasticity_Betas",
                     "Reg Price Ratio Rest of Brand_Betas", "Reg Price Ratio Rest of Manufacturer_Betas",
                     "Reg Price Ratio Rest of Category_Betas", "Promoted Price Elasticity_Betas",
                     "Display Only_Betas", "Feature Only_Betas", "Feature and Display_Betas",
                     "Multi Buy_Betas", "Total Multibuy Effect_Betas")
      available_beta_cols <- intersect(beta_cols, names(ppg_elasticity))
      
      if (length(available_beta_cols) > 0 && length(ppg_elasticity_by_cols2) > 0) {
        # Create weighted sums for available columns
        ppg_elasticity = ppg_elasticity[, lapply(.SD, function(x) sum(x * weight)), 
                                        by = ppg_elasticity_by_cols2, 
                                        .SDcols = available_beta_cols]
      }
    }
  } else {
    cat("Warning: Cannot compute ppg_elasticity - missing columns or filter conditions\n")
    ppg_elasticity = data.table()
  }
  
  
  #6.5--------------------------------Additional work for competition-----------------------------
  
  #1 Retro Funding for competetion
  
  mean_pos = function (y){
    mean(y[y > 0],na.rm = T)
  } 
  
  #Latest 52 weeks
  latest_52_weeks = sort(unique(master_with_flag$`Week End Date`), decreasing = T)[1:365]
  
  master_comp = master_with_flag[`Week End Date` %in% latest_52_weeks]
  master_comp = master_comp[!(`TRADING COMPANY` == "RECKITT")]
  master_comp[,FM_Percent_Comp := 0.3]
  master_comp[,Retailer_Revenue_Comp := Promo_Price*Units]
  master_comp[,FM_Abs_Comp := FM_Percent_Comp*Retailer_Revenue_Comp/(1+VAT)]
  master_comp[,Net_Cost_Total_Comp := ifelse(Flag_TPR_HEA == 0, Retailer_Revenue_Comp/(1+VAT) - FM_Abs_Comp, 0)]
  master_comp[,Net_Cost_Unit_Comp := Net_Cost_Total_Comp/Units]
  # Fixed: Use character vector for by= instead of .()
  master_comp[,Net_Cost_Unit_Comp_2 := mean_pos(Net_Cost_Unit_Comp), by = "ITEM"]
  master_comp[,Net_Cost_Total_Comp_2 := Net_Cost_Unit_Comp_2*Units]
  master_comp[,Retro_Fund_Total_Comp := ifelse(Flag_TPR_HEA == 1,FM_Abs_Comp - Retailer_Revenue_Comp/(1+VAT) + Net_Cost_Total_Comp_2,0) ]
  
  col_names = c("FM_Percent_Comp","Retailer_Revenue_Comp","FM_Abs_Comp","Net_Cost_Total_Comp", "Net_Cost_Unit_Comp","Net_Cost_Unit_Comp_2",
                "Net_Cost_Total_Comp_2","Retro_Fund_Total_Comp")
  
  master_comp = as.data.frame(master_comp)
  master_comp[,col_names] = 
    do.call(data.frame, lapply(master_comp[,col_names],
                               function(x) { replace(x, is.infinite(x) | is.na(x), 0) } ) )
  master_comp = data.table(master_comp)
  
  shiny_ip_competition = master_comp
  
  rm(col_names)
  
  
  #7--------------------------------------------------EVENT TAGGING-----------------------------------------------------------------
  
  # Fixed: Use character vector for by= instead of .()
  master_with_flag_by_cols <- c("PPG", "Week End Date")
  master_with_flag[, weight := Units / sum(Units), by = master_with_flag_by_cols] 
  master_with_flag[, weight := ifelse(is.infinite(weight) | is.na(weight), 0, weight)] 
  
  #Create Numeric Mapping for Display Type
  Display_Type = unique(master_with_flag$`DISPLAY TYPE_HEA`)[!(unique(master_with_flag$`DISPLAY TYPE_HEA`) %in% c("0","none",NA))]
  Display_Num = seq(from = 1, to = length(Display_Type) )
  display_num_map = data.table("Display_Type" = Display_Type, "Display_Num" = Display_Num)
  master_with_flag = merge(master_with_flag, display_num_map, by.x = "DISPLAY TYPE_HEA", by.y = "Display_Type", all.x = T)
  master_with_flag[is.na(master_with_flag)] = 0 
  
  cat("master_with_flag before event_list aggregation has", nrow(master_with_flag), "rows and", ncol(master_with_flag), "columns\n")
  cat("master_with_flag columns:", paste(names(master_with_flag)[1:min(20,ncol(master_with_flag))], collapse=", "), "...\n")
  
  #7.1)Roll upto PPG level
  # Fixed: Use list() instead of .() and character vector for by= in Plumber API environment
  event_list_by_cols <- c("PPG", "Week End Date", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description", "DISPLAY TYPE_HEA")
  
  # Check which columns exist for aggregation
  available_by_cols <- intersect(event_list_by_cols, names(master_with_flag))
  cat("Available by_cols:", paste(available_by_cols, collapse=", "), "\n")
  
  if (length(available_by_cols) < length(event_list_by_cols)) {
    missing <- setdiff(event_list_by_cols, available_by_cols)
    cat("WARNING: Missing by columns:", paste(missing, collapse=", "), "\n")
  }
  
  tryCatch({
    event_list = master_with_flag[, list(
    RSP_Unit = sum(RSP_Unit * weight),
    Flag_Display_HEA = max(Flag_Display_HEA),
    Flag_TPR_HEA = max(Flag_TPR_HEA),
    Base_Units = sum(`Base Units`),
    Retro_Fund_Total = sum(Retro_Fund_Total),
    Promo_Price = sum(Promo_Price * weight),
    Flag_Modeled_Betas = max(Flag_Modeled_Betas),
    Flag_PPG = max(Flag_PPG),
    VAT = max(VAT),
    Net_Cost_Unit = max(Net_Cost_Unit),
    FM_Abs_Unit_2 = sum(FM_Abs_Unit_2),
    Net_Cost_Total = sum(Net_Cost_Total),
    COGS_Total = sum(COGS_Total),
    Units = sum(Units),
    FM_Abs_Unit_1 = sum(FM_Abs_Unit_1),
    Flag_RB_Financial = max(Flag_RB_Financial),
    `Base Units` = sum(`Base Units`),
    COGS_Unit = max(COGS_Unit),
    Gross_Sales = sum(Gross_Sales),
    Retailer_Revenue = sum(Retailer_Revenue),
    FM_Total = sum(FM_Total),
    No_Of_Units = mean(No_Of_Units),
    Display_Cost = max(`Net invetsments_HEA`),
    Display_Num = first(Display_Num),
    OID_Total = sum(OID_Total),
    UNCR_Total = sum(UNCR_Total),
    Total_Inc_Unit_Sales = sum(`Total Incremental Unit Sales`),
    Inc_Unit_Discount = sum(`Inc TPR only`),
    Inc_Unit_Display = sum(`Inc Display Only`),
    ACV_D = sum(`ACV Distribution` * weight),
    NIS = sum(NIS),
    ACV_D_Any_Promo = sum(`ACV Distribution (C) (any promo)` * weight),
    ACV_D_Multibuy_Display = sum(`ACV Distribution (C) (multibuy and displ` * weight),
    ACV_D_Multibuy_Only = sum(`ACV Distribution (C) (multibuy only)` * weight),
    ACV_D_Feature_Display = sum(`ACV Distribution (C) (feature and displa` * weight),
    ACV_D_Display_Only = sum(`ACV Distribution (C) (display only)` * weight),
    ACV_D_Feature_Only = sum(`ACV Distribution (C) (feature only)` * weight),
    ACV_D_Unsupported = sum(`ACV Distribution (C) (Unsupported)` * weight),
    ACV_D_Total_Multibuy = sum(`ACV Distribution (C) (total multibuy)` * weight),
    ACV_D_Multibuy_Feature_Display = sum(`ACV Distribution Multibuy_Feature_Display` * weight),
    ACV_D_Multibuy_Feature = sum(`ACV Distribution Multibuy_Feature` * weight),
    `Total Regular Price Elasticiy_Betas` = sum(`Total Regular Price Elasticiy_Betas` * weight),
    `Regular Price Elasticity_Betas` = sum(`Regular Price Elasticity_Betas` * weight),
    `Reg Price Ratio Rest of Brand_Betas` = sum(`Reg Price Ratio Rest of Brand_Betas` * weight),
    `Reg Price Ratio Rest of Manufacturer_Betas` = sum(`Reg Price Ratio Rest of Manufacturer_Betas` * weight),
    `Reg Price Ratio Rest of Category_Betas` = sum(`Reg Price Ratio Rest of Category_Betas` * weight),
    `Promoted Price Elasticity_Betas` = sum(`Promoted Price Elasticity_Betas` * weight),
    `Display Only_Betas` = sum(`Display Only_Betas` * weight),
    `Feature Only_Betas` = sum(`Feature Only_Betas` * weight),
    `Feature and Display_Betas` = sum(`Feature and Display_Betas` * weight),
    `Multi Buy_Betas` = sum(`Multi Buy_Betas` * weight),
    `Total Multibuy Effect_Betas` = sum(`Total Multibuy Effect_Betas` * weight),
    R_GM_Inc = sum(R_GM_Inc),
    R_Net_Revenue_Inc = sum(R_Net_Revenue_Inc),
    R_NIS_Inc = sum(R_NIS_Inc),
    R_Total_Trade_Inv_Inc = sum(R_Total_Trade_Inv_Inc),
    Value = sum(Value)
  ), by = event_list_by_cols]
    cat("event_list aggregation successful:", nrow(event_list), "rows\n")
  }, error = function(e) {
    cat("ERROR in event_list aggregation:", as.character(e), "\n")
    event_list <<- data.table()
  })
  
  event_list[is.na(event_list)] = 0 
  
  
  
  #---------------------------Calculations-------------------------------------
  
  setDT(display_num_map)
  
  event_list[
    display_num_map,
    Display_Type := i.Display_Type,
    on = "Display_Num"
  ]
  event_list[,Trade_Investment := Retro_Fund_Total + Display_Cost + UNCR_Total + OID_Total]    #modified                         
  event_list[,Net_Revenue := Gross_Sales - Trade_Investment]          #addon
  event_list[,GM_Abs := Net_Revenue - COGS_Total]
  event_list[,Inc_GM_Abs := Total_Inc_Unit_Sales*GM_Abs/Units]
  event_list[,Inc_Revenue := Total_Inc_Unit_Sales*Net_Revenue/Units]  #addon
  event_list[,Inc_NIS := Total_Inc_Unit_Sales*NIS/Units]      #addon
  event_list[,c("Trade_Investment","Net_Revenue","GM_Abs","Inc_GM_Abs","Inc_Revenue","Inc_NIS")] = 
    do.call(data.frame, lapply(event_list[,c("Trade_Investment","Net_Revenue","GM_Abs","Inc_GM_Abs","Inc_Revenue","Inc_NIS")],
                               function(x) { replace(x, is.infinite(x) | is.na(x), 0) } ) )
  
  idx = order(event_list$PPG, event_list$`Week End Date`)
  event_list = event_list[idx,]
  rm(idx)
  
  #7.2.0 Create key - carrying the hierarchy
  cat("event_list before Key creation has", nrow(event_list), "rows and", ncol(event_list), "columns\n")
  cat("event_list columns:", paste(names(event_list)[1:min(10,ncol(event_list))], collapse=", "), "...\n")
  
  event_list[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description, sep = "_")]
  
  #7.2 Finding Event Number for PPG
  df = data.frame()
  for (key in unique(event_list$Key)) {
    k=1 ;Event_Num = vector(mode="numeric", length=0)
    key_data = event_list[event_list$Key == key,]
    key_data[,"Event_Number"] = event_no_fun(key_data$Flag_TPR_HEA,key_data$Flag_Display_HEA, Event_Num, k )
    df = rbind(df, key_data)
  }
  
  cat("df after loop has", nrow(df), "rows and", ncol(df), "columns\n")
  cat("df columns:", paste(names(df)[1:min(15,ncol(df))], collapse=", "), "...\n")
  
  df[is.na(df)]=0

  event_list = df
  write.csv(df,"df2.csv")
  #7.3.1) Sheet for Optimizer screen
  ly_kpi = data.table(df)  # Convert to data.table
  
  # Fixed: Safely select only columns that exist in the data
  ly_kpi_cols_desired <- c("PPG", "Week End Date", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description",
                   "Inc_GM_Abs", "Trade_Investment", "Units", "Gross_Sales", "Net_Cost_Total", "GM_Abs", "FM_Total",
                   "Retailer_Revenue", "Net_Revenue", "Base_Units", "Total_Inc_Unit_Sales", "No_Of_Units", "ACV_D",
                   "ACV_D_Any_Promo", "ACV_D_Multibuy_Display", "ACV_D_Multibuy_Only", "ACV_D_Feature_Display",
                   "ACV_D_Display_Only", "ACV_D_Feature_Only", "ACV_D_Unsupported", "ACV_D_Total_Multibuy",
                   "ACV_D_Multibuy_Feature_Display", "ACV_D_Multibuy_Feature", "Total Regular Price Elasticiy_Betas",
                   "Regular Price Elasticity_Betas", "Reg Price Ratio Rest of Brand_Betas",
                   "Reg Price Ratio Rest of Manufacturer_Betas", "Reg Price Ratio Rest of Category_Betas",
                   "Promoted Price Elasticity_Betas", "Display Only_Betas", "Feature Only_Betas",
                   "Feature and Display_Betas", "Multi Buy_Betas", "Total Multibuy Effect_Betas",
                   "Flag_Display_HEA", "Flag_TPR_HEA", "Display_Num", "Inc_Revenue", "NIS", "Inc_NIS",
                   "Display_Cost", "Retro_Fund_Total", "VAT", "R_GM_Inc", "R_Net_Revenue_Inc", "R_NIS_Inc",
                   "R_Total_Trade_Inv_Inc", "Value")
  # Only select columns that actually exist
  ly_kpi_cols <- intersect(ly_kpi_cols_desired, names(ly_kpi))
  cat("ly_kpi selecting", length(ly_kpi_cols), "of", length(ly_kpi_cols_desired), "desired columns\n")
  if (length(ly_kpi_cols) > 0) {
    ly_kpi = ly_kpi[, ..ly_kpi_cols]
  }
  
  ly_kpi[,Units_Sale_in_Case := Units/No_Of_Units]
  
  ly_kpi[is.na(Units_Sale_in_Case) | is.infinite(Units_Sale_in_Case), Units_Sale_in_Case := 0]   #remove NA or Inf
  
  shiny_ip_opti_constraints = ly_kpi
  
  # Fixed: Safely select only columns that exist in the data
  exclude_ppg_cols_desired <- c("PPG", "Week End Date", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description",
                        "Inc_GM_Abs", "Trade_Investment", "Units", "Gross_Sales", "Net_Revenue", "GM_Abs", "Promo_Price",
                        "RSP_Unit", "VAT", "Net_Cost_Unit", "FM_Total", "Retailer_Revenue", "Base_Units", "No_Of_Units",
                        "NIS", "Inc_NIS", "Inc_Revenue", "R_GM_Inc", "R_Net_Revenue_Inc", "R_NIS_Inc",
                        "R_Total_Trade_Inv_Inc", "Value")
  exclude_ppg_dt = data.table(df)
  exclude_ppg_cols <- intersect(exclude_ppg_cols_desired, names(exclude_ppg_dt))
  cat("exclude_ppg selecting", length(exclude_ppg_cols), "of", length(exclude_ppg_cols_desired), "desired columns\n")
  if (length(exclude_ppg_cols) > 0) {
    exclude_ppg = exclude_ppg_dt[, ..exclude_ppg_cols]
  } else {
    exclude_ppg = exclude_ppg_dt
  }
  
  exclude_ppg[,Units_Sale_in_Case := Units/No_Of_Units]
  
  exclude_ppg[is.na(Units_Sale_in_Case) | is.infinite(Units_Sale_in_Case), Units_Sale_in_Case := 0]
  
  ################I/P FOR SHINY HERE##################
  
  # Fixed: Use character vector for by= instead of .()
  event_list_weight_by <- c("PPG", "Event_Number")
  event_list[, weight := (Units) / sum(Units), by = event_list_weight_by]
  event_list[, weight := ifelse(is.infinite(weight) | is.na(weight), 0, weight)]
  
  event_list[is.na(event_list)] = 0
  
  #7.3.2) Final aggregation for Event Sheet
  # Fixed: Use list() instead of .() and character vector for by= in Plumber API environment
  events_by_cols <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "Event_Number", "PPG_Description", "Display_Num", "Display_Type")
  Events = event_list[, list(
    RSP_Unit = sum(RSP_Unit * weight),
    Promo_Price = sum(Promo_Price * weight),
    Flag_Display_HEA = max(Flag_Display_HEA),
    Flag_TPR_HEA = max(Flag_TPR_HEA),
    Total_Inc_Unit_Sales = sum(Total_Inc_Unit_Sales),
    Flag_RB_Financial = max(Flag_RB_Financial),
    Base_Units = sum(Base_Units),
    Inc_GM_Abs = sum(Inc_GM_Abs),
    Retro_Fund_Total = sum(Retro_Fund_Total),
    Display_Cost = max(Display_Cost),
    Display_Type_Agg = first(`DISPLAY TYPE_HEA`),
    Trade_Investment = sum(Trade_Investment),
    Flag_Modeled_Betas = max(Flag_Modeled_Betas),
    Flag_PPG = max(Flag_PPG),
    Start_Date = min(`Week End Date`),
    End_Date = max(`Week End Date`),
    FM_Total = sum(FM_Total),
    Retailer_Revenue = sum(Retailer_Revenue),
    No_Of_Units = mean(No_Of_Units),
    Display_Num_Agg = first(Display_Num),
    Display_Type_Max = max(`DISPLAY TYPE_HEA`),
    Inc_Revenue = sum(Inc_Revenue),
    NIS = sum(NIS),
    Inc_NIS = sum(Inc_NIS),
    Inc_Unit_Discount = sum(Inc_Unit_Discount),
    Inc_Unit_Display = sum(Inc_Unit_Display),
    R_GM_Inc = sum(R_GM_Inc),
    R_Net_Revenue_Inc = sum(R_Net_Revenue_Inc),
    R_NIS_Inc = sum(R_NIS_Inc),
    R_Total_Trade_Inv_Inc = sum(R_Total_Trade_Inv_Inc),
    ACV_D = sum(ACV_D * weight),
    ACV_D_Any_Promo = sum(ACV_D_Any_Promo * weight),
    ACV_D_Multibuy_Display = sum(ACV_D_Multibuy_Display * weight),
    ACV_D_Multibuy_Only = sum(ACV_D_Multibuy_Only * weight),
    ACV_D_Feature_Display = sum(ACV_D_Feature_Display * weight),
    ACV_D_Display_Only = sum(ACV_D_Display_Only * weight),
    ACV_D_Feature_Only = sum(ACV_D_Feature_Only * weight),
    ACV_D_Unsupported = sum(ACV_D_Unsupported * weight),
    ACV_D_Total_Multibuy = sum(ACV_D_Total_Multibuy * weight),
    ACV_D_Multibuy_Feature_Display = sum(ACV_D_Multibuy_Feature_Display * weight),
    ACV_D_Multibuy_Feature = sum(ACV_D_Multibuy_Feature * weight),
    `Total Regular Price Elasticiy_Betas` = sum(`Total Regular Price Elasticiy_Betas` * weight),
    `Regular Price Elasticity_Betas` = sum(`Regular Price Elasticity_Betas` * weight),
    `Reg Price Ratio Rest of Brand_Betas` = sum(`Reg Price Ratio Rest of Brand_Betas` * weight),
    `Reg Price Ratio Rest of Manufacturer_Betas` = sum(`Reg Price Ratio Rest of Manufacturer_Betas` * weight),
    `Reg Price Ratio Rest of Category_Betas` = sum(`Reg Price Ratio Rest of Category_Betas` * weight),
    `Promoted Price Elasticity_Betas` = sum(`Promoted Price Elasticity_Betas` * weight),
    `Display Only_Betas` = sum(`Display Only_Betas` * weight),
    `Feature Only_Betas` = sum(`Feature Only_Betas` * weight),
    `Feature and Display_Betas` = sum(`Feature and Display_Betas` * weight),
    `Multi Buy_Betas` = sum(`Multi Buy_Betas` * weight),
    `Total Multibuy Effect_Betas` = sum(`Total Multibuy Effect_Betas` * weight)
  ), by = events_by_cols]
  
  Events = Events[Event_Number != 0 ]
  Events[,Discount := (RSP_Unit - Promo_Price)/RSP_Unit]
  Events[,Event_Multiplier := Total_Inc_Unit_Sales/Base_Units]
  Events[,Event_Multiplier_Discount := Inc_Unit_Discount/Base_Units]               #addon
  Events[,Event_Multiplier_Display := Inc_Unit_Display/Base_Units]                 #addon
  
  Events[,ROI_GM := Inc_GM_Abs/Trade_Investment]
  Events[,ROI_Rev := Inc_Revenue/Trade_Investment]
  Events[,ROI_NIS := Inc_NIS/Trade_Investment]
  
  #New ROI Events
  Events[,R_ROI_GM := R_GM_Inc/R_Total_Trade_Inv_Inc]
  Events[,R_ROI_Rev := R_Net_Revenue_Inc/R_Total_Trade_Inv_Inc]
  Events[,R_ROI_NIS := R_NIS_Inc/R_Total_Trade_Inv_Inc]
  
  
  Events[is.na(Events)] = 0 
  Events[,RSP_Unit := ifelse(is.infinite(RSP_Unit),0,RSP_Unit)]
  Events[,Promo_Price := ifelse(is.infinite(Promo_Price),0,Promo_Price)]
  
  Events[
    display_num_map,
    Display_Type := i.Display_Type,
    on = "Display_Num"
  ]
  
  #8--------------------------------------Create Files for 2nd Screen--------------------------------------------------
  
  #7.1) Last Year Events and ROI
  
  events_shiny = Events[Flag_Modeled_Betas == 1 & Flag_PPG == 1 & Flag_RB_Financial == 1 & Flag_TPR_HEA == 1]
  
  shiny_ip_cal_event = events_shiny
  
  write.csv(shiny_ip_cal_event,"cal.csv")
  
  #======================================For SHINY optimizer Input================================================#
  
  # browser() # DISABLED for API
  latest_52_weeks = sort(unique(event_list$`Week End Date`), decreasing = T)[1:365]
  
  e_list = event_list[`Week End Date` %in% latest_52_weeks]
  
  ly_kpi_2 = e_list[Flag_Modeled_Betas == 1 & Flag_PPG == 1 & Flag_RB_Financial == 1 & Flag_TPR_HEA == 1,
                    list(LY_Investment = sum(Trade_Investment), LY_Any_Promo_Weeks = .N, LY_Display_Weeks = sum(Flag_Display_HEA)),
                    by = c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description")]
  
  #ly_kpi_2 = merge(ly_kpi_2, lsm[,c("PPG","Display Wks","TOTAL WKS")], by = "PPG")
  
  #Find Latest RSP
  latest_rsp = event_list[Flag_RB_Financial == 1 & Flag_PPG == 1]
  latest_rsp = latest_rsp[`Week End Date` == max(`Week End Date`)]
  # Fixed: Use ..cols syntax instead of .() for column selection
  latest_rsp_cols <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description", "RSP_Unit")
  latest_rsp = latest_rsp[, ..latest_rsp_cols]
  
  # Fixed: Use ..cols syntax instead of .() for column selection
  lsm_subset_cols <- c("PPG", "LSM_Promo_Price_Min", "Min_Total_Weeks", "Max_Total_Weeks", "Non_LSM_Min_Total_Weeks",
                       "Non_LSM_Max_Total_Weeks", "Min_Display_Weeks", "Max_Display_Weeks", "Non_LSM_Min_Display_Weeks",
                       "Non_LSM_Max_Display_Weeks", "MRRP Max", "MRRP Min", "Global_Floor_Price")
  lsm_subset = lsm_new[, ..lsm_subset_cols]
  
  map = unique(maping[,c("PPG","PPG_Parent")])
  
  lsm_subset = merge(lsm_subset,map, by.x = "PPG", by.y = "PPG_Parent")
  
  setnames(lsm_subset,"PPG","PPG_Parent")
  setnames(lsm_subset,"PPG.y","PPG")
  
  # Fixed: Use ..cols syntax instead of .() for column selection
  lsm_merge_cols <- c("PPG", "RSP_Unit")
  lsm_subset = merge(lsm_subset, latest_rsp[, ..lsm_merge_cols], by = "PPG")
  
  ly_kpi_2 = merge(ly_kpi_2, lsm_subset, by = "PPG")
  
  ly_kpi_2[,LY_Investment := round(LY_Investment,0)]
  ly_kpi_2[,Min_Investment := round(0.1*LY_Investment,0)]
  ly_kpi_2[,Max_Investment := round(10*LY_Investment,0)]
  
  #ly_kpi_2 => Used later in the code.

  # ===================== 7/8. CALENDAR & MULTIPLIERS (TESCO vs SLOT-BASED) =====================
  
  cat("SECTION 7/8: RETAILER value is:", RETAILER, "\n")
  
  if (toupper(RETAILER) == "TESCO") {
    
    #--------------------------- EXISTING TESCO LOGIC (UNCHANGED) ---------------------------#
    
    # Fixed: Use ..cols syntax instead of .() for column selection in Plumber API environment
    tesco_events_cols <- c("PPG", "Week End Date", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description",
                           "Flag_TPR_HEA", "Flag_Display_HEA", "Event_Number", "Display_Type",
                           "Units", "Base_Units", "OID_Total", "UNCR_Total", "Total_Inc_Unit_Sales", "Retro_Fund_Total",
                           "RSP_Unit", "R_GM_Inc", "R_Net_Revenue_Inc", "R_NIS_Inc", "R_Total_Trade_Inv_Inc")
    tesco_events = event_list[Flag_Modeled_Betas == 1 & Flag_PPG == 1 & Flag_RB_Financial == 1, ..tesco_events_cols]
    
    setnames(tesco_events, "Week End Date", "Date")
    setnames(tesco_events, "Flag_TPR_HEA", "Flag_TPR_HEA_Nielsen")
    setnames(tesco_events,"Flag_Display_HEA","Flag_Display_HEA_Nielsen")
    
    tesco_events[,Tesco_Week_Ending := Date + days_to_add]   # days_to_add from earlier retailer section
    
    tesco_events[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,sep = "_")]
    
    df = data.frame()
    for(key in unique(tesco_events$Key)) {
      one_key = tesco_events[tesco_events$Key == key,]
      one_key[,"Flag_Display_HEA_Tesco"] = tesco_event_num(one_key$Flag_Display_HEA_Nielsen,retailer_week_end_day)
      one_key[,"Flag_TPR_HEA_Tesco"]     = tesco_event_num(one_key$Flag_TPR_HEA_Nielsen,retailer_week_end_day)
      df = rbind(df,one_key)
    }
    
    tesco_events_shiny = data.table(df)
    rm(df, one_key, tesco_events)
    
    tesco_events_shiny$Key = NULL
    
    # 7.3) Display Mapping as per KAM input
    tesco_events_shiny[,Display_Flag_KAM_Input := ifelse(Display_Type == "0", 0, 1)]
    tesco_events_shiny[,Display_Flag_KAM_Input_Tesco := tesco_event_num(Display_Flag_KAM_Input,retailer_week_end_day)]
    
    shiny_ip_cal_tesco_mapping = tesco_events_shiny
    
    #=========================== 8.1 Creating Tesco Calendar ===========================#
    
    tesco_week_cal = tesco_events_shiny
    
    tesco_week_cal[,Scale_Factor := Units/(Base_Units + Total_Inc_Unit_Sales)]
    tesco_week_cal[,Base_Units_New := Scale_Factor*Base_Units]
    tesco_week_cal[,Total_Inc_Unit_Sales_New := Total_Inc_Unit_Sales*Scale_Factor]
    tesco_week_cal[,Units_New := Base_Units_New + Total_Inc_Unit_Sales_New]
    tesco_week_cal[,Base_Units_Tesco := tesco_base_units(Base_Units,days_in_current_week,days_in_next_week)]
    
    tesco_week_cal[,Key := paste(PPG, `SECTOR 2`, `TRADING COMPANY`, `PRODUCT RANGE`, FORMAT,PPG_Description)]
    # Fixed: Use character vector for by= instead of .()
    tesco_week_cal[,Base_Units_Tesco_New := tesco_base_units(Base_Units_New,days_in_current_week,days_in_next_week), by = "Key"]
    tesco_week_cal[,Units_Tesco := tesco_base_units(Units_New,days_in_current_week,days_in_next_week), by = "Key" ]
    tesco_week_cal[,Total_Inc_Unit_Sales_Tesco := tesco_inc_units(Total_Inc_Unit_Sales_New,Flag_TPR_HEA_Tesco), by = "Key"]
    tesco_week_cal[,Retro_Fund_Total_Tesco := tesco_inc_units(Retro_Fund_Total,Flag_TPR_HEA_Tesco), by = "Key"]
    tesco_week_cal[,Display_Type_Tesco := tesco_display_type( Display_Type,Flag_Display_HEA_Tesco ), by = "Key" ]
    
    tesco_week_cal[,Event_Number_Tesco := event_no_fun(Flag_TPR_HEA_Tesco,k=1,
                                                       Event_Num = vector(mode="numeric", length=0)),
                   by = "Key"]
    
    tesco_week_cal[,Event_Number_Tesco_2 := ifelse(Display_Type_Tesco == "0", 0, Event_Number_Tesco)]
    
    # 8.1.2 Map Display Cost from event list
    # Fixed: Use ..cols syntax instead of .() for column selection
    events_to_map_cols <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description", "Event_Number", "Display_Cost")
    events_to_map = events_shiny[, ..events_to_map_cols]
    
    tesco_week_cal[,Key := paste(PPG, `SECTOR 2`, `TRADING COMPANY`, `PRODUCT RANGE`,
                                 FORMAT,PPG_Description,Event_Number_Tesco_2)]
    events_to_map = events_to_map[, Key := paste(PPG, `SECTOR 2`, `TRADING COMPANY`,
                                                 `PRODUCT RANGE`, FORMAT,PPG_Description,Event_Number)]
    
    events_to_map[,c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE",
                     "FORMAT","PPG_Description","Event_Number")] = NULL
    
    tesco_week_cal = merge(tesco_week_cal, events_to_map, by = "Key", all.x = TRUE, allow.cartesian = TRUE)
    tesco_week_cal[is.na(tesco_week_cal)] = 0
    
    # Fixed: Use character vector for by= instead of .()
    tesco_week_cal[,Display_Cost := Display_Cost/.N , by = "Key"]
    
    tesco_week_cal = merge(tesco_week_cal, cost_bible, by = "PPG", all.x = TRUE)
    
    tesco_week_cal[,Retro_Fund_Unit := Retro_Fund_Total_Tesco/Units_Tesco]
    tesco_week_cal[,FM_Abs_Unit_1 := (RSP_Unit/(1+VAT)) - Net_Cost_Unit]
    tesco_week_cal[,FM_Percent_1 := FM_Abs_Unit_1*(1+VAT)/RSP_Unit]
    tesco_week_cal[,Retailer_Revenue := (Retro_Fund_Total_Tesco)*(1+VAT)/(1 - FM_Percent_1)]
    
    tesco_week_cal[,Promo_Price := ifelse( Flag_TPR_HEA_Tesco == 1,Retailer_Revenue/Units_Tesco, 0)]
    tesco_week_cal[,Gross_Sales := Promo_Price*Units_Tesco]
    
    tesco_week_cal[,OID_Total := OID_Unit*Units_Tesco]
    tesco_week_cal[,Total_Trade_Investment := (UNCR_Total + OID_Total +  Retro_Fund_Total_Tesco + Display_Cost)]
    tesco_week_cal[,Net_Revenue := Gross_Sales - Total_Trade_Investment]
    tesco_week_cal[,NIS := Gross_Sales - UNCR_Total - OID_Total]
    tesco_week_cal[,BIP := Units_Tesco*Net_Cost_Unit]
    tesco_week_cal[,COGS_Total := Units_Tesco*COGS_Unit]
    tesco_week_cal[,GM_Abs := Net_Revenue - COGS_Total]
    tesco_week_cal[,Inc_GM_Abs := Total_Inc_Unit_Sales_Tesco*GM_Abs/Units_Tesco]
    tesco_week_cal[,Inc_Revenue := Total_Inc_Unit_Sales_Tesco*Net_Revenue/Units_Tesco]
    tesco_week_cal[,Inc_NIS := Total_Inc_Unit_Sales_Tesco*NIS/Units_Tesco]
    tesco_week_cal[,Units_Sale_in_Case := Units_Tesco/No_Of_Units]
    tesco_week_cal[is.na(Units_Sale_in_Case) | is.infinite(Units_Sale_in_Case), Units_Sale_in_Case := 0]
    tesco_week_cal[,Event_Multiplier_Tesco := ifelse(Flag_TPR_HEA_Tesco == 1,
                                                     Total_Inc_Unit_Sales_Tesco/Base_Units_Tesco_New,0)]
    
    tesco_week_cal[,Discount := (RSP_Unit - Promo_Price) /RSP_Unit]
    tesco_week_cal[,UNCR_Total := (RSP_Unit - Promo_Price)*Units_Tesco]
    
    tesco_week_cal[,FM_Abs_Unit_2 := (Promo_Price/(1+VAT))-Net_Cost_Unit+Retro_Fund_Unit]
    tesco_week_cal[,FM_Abs_Unit_2 := ifelse(Promo_Price == 0, 0, FM_Abs_Unit_2)]
    tesco_week_cal[,FM_Total := FM_Abs_Unit_2*Units_Tesco]
    
    tesco_week_cal[,`FM%` := FM_Percent_1]
    tesco_week_cal[,`FM%_2` := FM_Percent_1]
    
    tesco_week_cal[,R_UNCR_Inc := Total_Inc_Unit_Sales_Tesco*(RSP_Unit - Promo_Price)]
    tesco_week_cal[,R_OID_Inc := Total_Inc_Unit_Sales_Tesco*OID_Unit]
    tesco_week_cal[,R_Retro_Inc := Units_Tesco*Retro_Fund_Unit]
    tesco_week_cal[,R_Display_Cost := Display_Cost]
    tesco_week_cal[,R_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Cost]
    tesco_week_cal[,R_NIS_Inc := (STP_Unit - (RSP_Unit - Promo_Price) - OID_Unit)*Total_Inc_Unit_Sales_Tesco]
    tesco_week_cal[,R_Net_Rev_Inc := R_NIS_Inc - R_Retro_Inc - R_Display_Cost ]
    tesco_week_cal[,R_GM_Inc := R_Net_Rev_Inc - (COGS_Unit*Total_Inc_Unit_Sales_Tesco)]
    
    idx = order(tesco_week_cal$PPG, tesco_week_cal$Date)
    tesco_week_cal = tesco_week_cal[idx,]
    
    tesco_week_cal[,Tesco_Week_No := "-"]
    
    # Fixed: Use ..cols syntax instead of .() for column selection
    final_tesco_calendar_cols <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description", "Tesco_Week_No", "Tesco_Week_Ending", "Base_Units_Tesco",
                                   "RSP_Unit", "Net_Cost_Unit", "COGS_Unit", "VAT", "BIP_Case", "OID", "No_Of_Units", "STP_Unit", "UNCR_Unit", "OID_Unit", "FM_Abs_Unit_1",
                                   "FM%", "Discount", "Display_Type_Tesco", "Display_Cost", "Event_Multiplier_Tesco", "Flag_Display_HEA_Tesco", "Flag_TPR_HEA_Tesco",
                                   "Total_Inc_Unit_Sales_Tesco", "Units_Tesco", "Promo_Price", "Retro_Fund_Unit", "Retro_Fund_Total_Tesco", "UNCR_Total", "OID_Total",
                                   "Total_Trade_Investment", "Gross_Sales", "Net_Revenue", "NIS", "BIP", "COGS_Total", "GM_Abs", "Inc_GM_Abs", "FM_Abs_Unit_2", "FM%_2",
                                   "FM_Total", "Retailer_Revenue", "Inc_NIS", "Inc_Revenue", "Units_Sale_in_Case",
                                   "R_NIS_Inc", "R_Net_Rev_Inc", "R_GM_Inc", "R_Trade_Inv_Inc")
    final_tesco_calendar = tesco_week_cal[, ..final_tesco_calendar_cols]
    
    names(final_tesco_calendar) = c("PPG","Category","Manufacturer","Brand","Format","PPG_Description","Tesco_Week_No","Week_Ending","Base_Units","RSP_Unit",
                                    "Net_Cost_Unit","COGS_Unit","VAT","BIP_Case","OID","No_Of_Units","STP_Unit","UNCR_Unit","OID_Unit",
                                    "FM_Abs_Unit_1","FM%","Discount","Display","Display_Cost","Event_Multiplier_Tesco","Display_Flag",
                                    "TPR_Flag","Event_Lift","Total_Sales","Promo_Price","Retro_Funding_Unit","Retro_Funding_Total",
                                    "UNCR_Total","OID_Total","Total_Trade_Investment","Gross_Sales","Net_Revenue","NIS","BIP","COGS_Total",
                                    "GM_Abs","Inc_GM_Abs","FM_Abs_Unit_2","FM%_2","FM_Total","Retailer_Revenue","Inc_NIS","Inc_Revenue",
                                    "Units_Sale_in_Case","R_NIS_Inc", "R_Net_Rev_Inc", "R_GM_Inc", "R_Trade_Inv_Inc")
    
    shiny_ip_tesco_cal <- final_tesco_calendar
    
  # browser() # DISABLED for API
    
    # 8.2.1 Tesco event base units and multipliers
    # Fixed: Use list() and character vector for by= instead of .()
    tesco_event_by_cols <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description", "Event_Number_Tesco")
    tesco_event_base_units = tesco_week_cal[, list(Base_Units_Tesco = sum(Base_Units_Tesco)), by = tesco_event_by_cols]
    tesco_event_base_units = tesco_event_base_units[Event_Number_Tesco != 0]
    
    events_shiny[,Key := paste(PPG, `SECTOR 2`,`TRADING COMPANY`, `PRODUCT RANGE`, FORMAT,PPG_Description, Event_Number)]
    tesco_event_base_units[, Key := paste(PPG, `SECTOR 2`,`TRADING COMPANY`, `PRODUCT RANGE`, FORMAT,PPG_Description, Event_Number_Tesco)]
    
    setDT(events_shiny)
    setDT(tesco_event_base_units)
    
    events_shiny_2 = events_shiny[
      tesco_event_base_units,
      Base_Units_Tesco := i.Base_Units_Tesco,
      on = "Key"
    ]
    events_shiny_2[,Event_Multiplier_Tesco := Total_Inc_Unit_Sales/Base_Units_Tesco]
    events_shiny_2[,Event_Multiplier_Display_Tesco := Inc_Unit_Display/Base_Units_Tesco]
    events_shiny_2[,Event_Multiplier_Discount_Tesco := Inc_Unit_Discount/Base_Units_Tesco]
    
  } else {#==============================================Closed====================================================#
    
    tesco_events = event_list[Flag_Modeled_Betas == 1 & Flag_PPG == 1 & Flag_RB_Financial == 1 ,
                              .(PPG,`Week End Date`,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Flag_TPR_HEA,Flag_Display_HEA,Event_Number, Display_Type,
                                Units, Base_Units, OID_Total, UNCR_Total, Total_Inc_Unit_Sales, Retro_Fund_Total,RSP_Unit, R_GM_Inc,
                                R_Net_Revenue_Inc, R_NIS_Inc, R_Total_Trade_Inv_Inc)]
    
    
    tesco_events_shiny = tesco_events
    
    tesco_events_shiny$Key = NULL
    setDT(tesco_events_shiny)
    
    #7.3) Display Mapping as per KAM input - Yoshi for Airwick
    tesco_events_shiny[,Display_Flag_KAM_Input := ifelse(Display_Type == "0", 0, 1)]
    
    shiny_ip_cal_tesco_mapping = tesco_events_shiny
    
    
    #=========================== 8.1 Creating Tesco Calendar -Base Units, Retro Funding, Display Cost, Display Type, Inc Sales ===============
    
    tesco_week_cal = tesco_events_shiny
    
    tesco_week_cal[,Key := paste(PPG, `SECTOR 2`, `TRADING COMPANY`, `PRODUCT RANGE`, FORMAT,PPG_Description)]
    
    
    
    #8.1.2 Map Retro Funding, Display Cost, Display Type, Inc Sales from Event List
    
    events_to_map = events_shiny[,.(PPG, `SECTOR 2`, `TRADING COMPANY`,`PRODUCT RANGE`, FORMAT,PPG_Description, Event_Number, 
                                    Display_Cost)]
    
    #Create key in both dataframes
    tesco_week_cal[,Key := paste(PPG, `SECTOR 2`, `TRADING COMPANY`, `PRODUCT RANGE`, FORMAT,PPG_Description,Event_Number)]
    events_to_map = events_to_map[, Key := paste(PPG, `SECTOR 2`, `TRADING COMPANY`, `PRODUCT RANGE`, FORMAT,PPG_Description,Event_Number)]
    
    #Remove un-necessary columns
    events_to_map[,c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT","PPG_Description","Event_Number")] = NULL
    
    tesco_week_cal = merge(tesco_week_cal, events_to_map, by = "Key", all.x = T)
    
    #Make NA as 0
    tesco_week_cal[is.na(tesco_week_cal)] = 0
    
    #8.1.3 Divide Retro Funding, Display Cost, Display Type, Inc Sales equally
    tesco_week_cal[,  Display_Cost := Display_Cost/.N , by = .(Key)]
    
    
    #Get cost bible kpi's
    tesco_week_cal = merge(tesco_week_cal, cost_bible, by = "PPG", all.x = T)
    
    #Calculate all kpi's
    
    tesco_week_cal[,Retro_Fund_Unit := Retro_Fund_Total/Units]
    tesco_week_cal[,FM_Abs_Unit_1 := (RSP_Unit/(1+VAT)) - Net_Cost_Unit]
    tesco_week_cal[,FM_Percent_1 := FM_Abs_Unit_1*(1+VAT)/RSP_Unit]
    tesco_week_cal[,Retailer_Revenue := (Retro_Fund_Total)*(1+VAT)/(1 - FM_Percent_1)]
    
    tesco_week_cal[,Promo_Price := ifelse( Flag_TPR_HEA == 1,Retailer_Revenue/Units, 0)]
    tesco_week_cal[,Gross_Sales := Promo_Price*Units]
    
    tesco_week_cal[,OID_Total := OID_Unit*Units]                                                                    #addon
    tesco_week_cal[,Total_Trade_Investment := (UNCR_Total + OID_Total +  Retro_Fund_Total + Display_Cost)]          #addon
    tesco_week_cal[,Net_Revenue := Gross_Sales - Total_Trade_Investment]                                                  #addon
    tesco_week_cal[,NIS := Gross_Sales - UNCR_Total - OID_Total]                                                          #addon
    tesco_week_cal[,BIP := Units*Net_Cost_Unit]
    tesco_week_cal[,COGS_Total := Units*COGS_Unit]
    tesco_week_cal[,GM_Abs := Net_Revenue - COGS_Total]
    tesco_week_cal[,Inc_GM_Abs := Total_Inc_Unit_Sales*GM_Abs/Units]
    tesco_week_cal[,Inc_Revenue := Total_Inc_Unit_Sales*Net_Revenue/Units]                                                    #addon
    tesco_week_cal[,Inc_NIS := Total_Inc_Unit_Sales*NIS/Units] 
    tesco_week_cal[,Units_Sale_in_Case := Units/No_Of_Units]
    tesco_week_cal[is.na(Units_Sale_in_Case) | is.infinite(Units_Sale_in_Case), Units_Sale_in_Case := 0]   #remove NA and Inf
    tesco_week_cal[,Event_Multiplier := ifelse(Flag_TPR_HEA == 1,Total_Inc_Unit_Sales/Base_Units,0)]
    
    tesco_week_cal[,Discount := (RSP_Unit - Promo_Price) /RSP_Unit]
    tesco_week_cal[,UNCR_Total := (RSP_Unit - Promo_Price)*Units]                                                                  #addon
    
    tesco_week_cal[,FM_Abs_Unit_2 := (Promo_Price/(1+VAT))-Net_Cost_Unit+Retro_Fund_Unit]
    tesco_week_cal[,FM_Abs_Unit_2 := ifelse(Promo_Price == 0, 0, FM_Abs_Unit_2)]   #making sure fm_abs_unit is not negative when promo_price is 0
    tesco_week_cal[,FM_Total := FM_Abs_Unit_2*Units]
    
    tesco_week_cal[,`FM%` := FM_Percent_1]
    tesco_week_cal[,`FM%_2` := FM_Percent_1]
    
    tesco_week_cal[,R_UNCR_Inc := Total_Inc_Unit_Sales*(RSP_Unit - Promo_Price)]                                                 #addon2
    tesco_week_cal[,R_OID_Inc := Total_Inc_Unit_Sales*OID_Unit]                                                   #addon2
    tesco_week_cal[,R_Retro_Inc := Units*Retro_Fund_Unit]                                                         #addon2
    tesco_week_cal[,R_Display_Cost := Display_Cost]                                                                     #addon2
    tesco_week_cal[,R_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Cost]                           #addon2
    tesco_week_cal[,R_NIS_Inc := (STP_Unit - (RSP_Unit - Promo_Price) - OID_Unit)*Total_Inc_Unit_Sales]                          #addon2
    tesco_week_cal[,R_Net_Rev_Inc := R_NIS_Inc - R_Retro_Inc - R_Display_Cost ]                                         #addon2
    tesco_week_cal[,R_GM_Inc := R_Net_Rev_Inc - (COGS_Unit*Total_Inc_Unit_Sales)]                                 #addon2
    
    idx = order(tesco_week_cal$PPG, tesco_week_cal$`Week End Date`)
    tesco_week_cal = tesco_week_cal[idx,] 
    
    tesco_week_cal[,Tesco_Week_No := tesco_week_cal$Event_Number]
    
    #Order the columns
    final_tesco_calendar = 
      tesco_week_cal[,.(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Tesco_Week_No,`Week End Date`,Base_Units,
                        RSP_Unit,Net_Cost_Unit,COGS_Unit,VAT,BIP_Case,OID,No_Of_Units,STP_Unit,UNCR_Unit,OID_Unit,FM_Abs_Unit_1,
                        `FM%`,Discount,Display_Type, Display_Cost,Event_Multiplier, Flag_Display_HEA,Flag_TPR_HEA,
                        Total_Inc_Unit_Sales, Units,Promo_Price,Retro_Fund_Unit, Retro_Fund_Total, UNCR_Total,OID_Total,
                        Total_Trade_Investment,Gross_Sales,Net_Revenue,NIS,BIP,COGS_Total,GM_Abs,Inc_GM_Abs,FM_Abs_Unit_2,`FM%_2`,
                        FM_Total,Retailer_Revenue,Inc_NIS,Inc_Revenue,Units_Sale_in_Case,
                        R_NIS_Inc, R_Net_Rev_Inc, R_GM_Inc, R_Trade_Inv_Inc)]
    
    names(final_tesco_calendar) = c("PPG","Category","Manufacturer","Brand","Format","PPG_Description","Tesco_Week_No","Week_Ending","Base_Units","RSP_Unit",
                                    "Net_Cost_Unit","COGS_Unit","VAT","BIP_Case","OID","No_Of_Units","STP_Unit","UNCR_Unit","OID_Unit",
                                    "FM_Abs_Unit_1","FM%","Discount","Display","Display_Cost","Event_Multiplier_Tesco","Display_Flag",
                                    "TPR_Flag","Event_Lift","Total_Sales","Promo_Price","Retro_Funding_Unit","Retro_Funding_Total",
                                    "UNCR_Total","OID_Total","Total_Trade_Investment","Gross_Sales","Net_Revenue","NIS","BIP","COGS_Total",
                                    "GM_Abs","Inc_GM_Abs","FM_Abs_Unit_2","FM%_2","FM_Total","Retailer_Revenue","Inc_NIS","Inc_Revenue",
                                    "Units_Sale_in_Case","R_NIS_Inc", "R_Net_Rev_Inc", "R_GM_Inc", "R_Trade_Inv_Inc")
    
    
    
    write.csv(final_tesco_calendar,"9 Tesco Weekly Calendar_2.csv")
    
    
  }
    
 #  ============================================-CREATE DATA FOR OPTIMIZATION-==================================================
  
  #9.1) Format TESCO slots to Calendar
  
  #Read data
  #data is read above
  
  tesco_slots$`Start Date`[1]= "25/12/2025"
  tesco_slots$`Start Date` = dmy(tesco_slots$`Start Date`)
  tesco_slots$`End Date` = dmy(tesco_slots$`End Date`)
  
  
  #Find Min and Max date
  all_date = c(tesco_slots$`Start Date`, tesco_slots$`End Date`)
  min_date = min(all_date)
  max_date = max(all_date)
  
  #Create time series
  time_series = as.data.table(seq.Date(from = (min_date), to = (max_date), by = 1))
  names(time_series) = "Date"
  
  #Map Tesco Week Number
  tesco_week = merge(tesco_slots,time_series, by.x = "Start Date", by.y = "Date", all.x = T,all.y = T)
  tesco_week$`End Date` = NULL
  tesco_week$Tesco_Week_No = broadcast(tesco_week$Slot)
  setDT(tesco_week)
  tesco_week[, Tesco_Week_No := broadcast(Slot)]
  #Add Week End Date
 
  tesco_week$Date= tesco_week$`Start Date`
  tesco_week[,Day := weekdays(`Start Date`)]
  
  tesco_week[,Week_Ending := as_date(ifelse( weekdays(Date) == retailer_week_end_day, Date,
                                             ceiling_date(Date,"week",week_start = retailer_weekEndDay_no) ))]
  
  # weekday = data.table("Day" = c("Wednesday","Thursday","Friday","Saturday","Sunday","Monday","Tuesday"),
  #                          "days_to_add" = c(6,5,4,3,2,1,0))
  # tesco_week = merge(tesco_week, weekday, by = "Day", all.x = T)
  # tesco_week[,Week_Ending := Date + days_to_add]
  
  tesco_week = tesco_week[order(tesco_week$Date)]
  #tesco_week$days_to_add = NULL
  
  tesco_calendar = tesco_week
  
  rm(all_date)
  rm(max_date)
  rm(min_date)
  rm(time_series)
  rm(tesco_week)
  #rm(weekday)
  
  #9.2) Calculate Base Sales
  
  
  #9.2.1 Base Sales of PPG's
  
  #Create Daily Date
  # Fixed: Use ..cols syntax instead of .() for column selection
  base_sales_cols <- c("ITEM", "Base Units", "PPG", "Week End Date", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description")
  base_sales = master_with_flag[Flag_PPG == 1 & Flag_RB_Financial == 1, ..base_sales_cols]
  
  #dl_nl$EAN = as.character(dl_nl$EAN)
  base_sales = merge(base_sales,dl_nl, by.y = "EAN", by.x = "ITEM",all.x = T)
  
  #Filter out delist and roll up
  base_sales = base_sales[DL != 1]
  
  #Calculate base sales of new launch
  mean_pos = function(x) { mean(x[x>0]) }   #mean of values except 0
  #setDT(df)
  # More efficient data.table approach
  df <- base_sales[, `Base Units` := ifelse(NL == 1 & `Base Units` == 0, 
                                           mean_pos(`Base Units`), 
                                          `Base Units`), 
                   by = ITEM]
  
 
  
  #Roll up and filter latest weeks only
  # Fixed: Use list() and character vector for by= instead of .()
  base_sales_by_cols <- c("PPG", "Week End Date", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description")
  base_sales = base_sales[, list(Base_Units = sum(`Base Units`)), by = base_sales_by_cols]
  latest_55_weeks = sort(unique(base_sales$`Week End Date`), decreasing = T)
  base_sales = base_sales[`Week End Date` %in% latest_55_weeks]
  base_sales[,Key := paste(`Week End Date`,PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  
  min_date = min(base_sales$`Week End Date`)
  max_date = max(base_sales$`Week End Date`)
  
  time_series = as.data.frame(seq.Date(from = min_date, to = max_date, by = 1))
  
  #base_tmp
  # Fixed: Use ..cols syntax instead of .() for column selection
  base_tmp_cols <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description")
  base_tmp = unique(base_sales[, ..base_tmp_cols])
  
  #Add cross join
  time_series_2 = as.data.table(merge(time_series,base_tmp, by = NULL))
  names(time_series_2)[1] = c("Date")
  time_series_2[,Key := paste(Date,PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  
  
  rm(min_date)
  rm(max_date)
  rm(latest_55_weeks)
  rm(time_series)
  
  #9.2.2 Adding Base Sales to Daily Data
  # Fixed: Use ..cols syntax instead of .() for column selection
  base_sales_tmp_cols <- c("Base_Units", "Key")
  base_sales_tmp = base_sales[, ..base_sales_tmp_cols]
  base_sales_2 = merge(time_series_2, base_sales_tmp, by = "Key", all.x = T)
  base_sales_2$Key = NULL
  
  rm(base_sales)
  rm(time_series_2)
  rm(base_sales_tmp)
  
  #9.2.3 Split weekly sales to Daily Sales
  idx = order(base_sales_2$PPG, base_sales_2$Date, decreasing = T,na.last = F)
  base_sales_2 = base_sales_2[idx,]
 
  rm(idx)
  #base_sales_2$PPG.y = NULL
  #base_sales_2$`Week End Date` = NULL
  #names(base_sales_2)[3] = "PPG"
  
  #final step
  base_sales_2$Base_Units = broadcast(base_sales_2$Base_Units)
  base_sales_2[,Base_Units := Base_Units]
  base_sales_2$Date
  #9.2.4 Get Latest 365 days
  latest_365_days = sort(unique(base_sales_2$Date), decreasing = T)[1:365]
  base_sales_2 = base_sales_2[Date %in% latest_365_days]
  year(base_sales_2$Date) = 2026
  
  
  #9.2.5 Converting Base Sales to Tesco 2019 Calendar
  #tesco_calendar$Date = as.Date(tesco_calendar$Date)
  base_sales_2 = base_sales_2[Date %in% tesco_calendar$Date]
  
  tesco_base_sales = merge(base_sales_2,tesco_calendar, by = "Date", all.x = T)
  
  
  #rm(base_sales_2)
  
  tesco_base_sales = tesco_base_sales[,.(Base_Units = sum(Base_Units)),
                                      by = .(PPG,`SECTOR 2`,`TRADING COMPANY`,
                                             `PRODUCT RANGE`,FORMAT,PPG_Description,Tesco_Week_No,Date)]
  
  idx = order(tesco_base_sales$PPG, tesco_base_sales$Tesco_Week_No)
  tesco_base_sales = tesco_base_sales[idx,]
  
  tesco_base_sales[,Key:= paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]  #for merging
  
  rm(idx)
  
  #9.3) Calculate Latest Week RSP
  
  #9.3.1 Roll to PPG level
  RB_Fin = event_list[Flag_RB_Financial == 1 & Flag_PPG == 1]
  RB_Fin = RB_Fin[`Week End Date` == max(`Week End Date`)]
  # Fixed: Use ..cols syntax instead of .() for column selection
  RB_Fin_cols <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description", "RSP_Unit")
  RB_Fin = RB_Fin[, ..RB_Fin_cols]
  
  map = unique(maping[,c("PPG","PPG_Parent")])         #addon_parent_ppg
  
  RB_Fin = merge(RB_Fin,map,by = "PPG", all.x = T)   #Get Parent PPG
  
  #Add Net Cost and COGS from cost bible
  # Fixed: Use ..cols syntax instead of .() for column selection
  var_cols <- c("PPG", "Net_Cost_Unit", "COGS_Unit", "VAT", "BIP_Case", "OID", "No_Of_Units", "STP_Unit", "UNCR_Unit", "OID_Unit")
  var = cost_bible[, ..var_cols]
  RB_Fin  = merge(RB_Fin, var, by.x = "PPG_Parent", by.y = "PPG", all.x = T)
  
  RB_Fin[,FM_Abs_Unit_1 := (RSP_Unit/(1+VAT)) - Net_Cost_Unit]
  RB_Fin[,`FM%` := FM_Abs_Unit_1*(1+VAT)/RSP_Unit]
  
  RB_Fin[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]   #for merging
  RB_Fin[,c("PPG","SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT","PPG_Description","PPG_Parent")] = NULL
  #RB_Fin[,1:5] = NULL
  
  #-----------------------------Modification required in RB_Fin for Seasonal PPG -------------------------------------#
  
  
  #1 If all EAN in a PPG are delisted, remove them - taken care
  
  #2 Find RSP where latest RSP = 0
  # Fixed: Use ..cols syntax instead of .() for column selection
  ppg_zero_rsp = RB_Fin[RSP_Unit == 0, "Key", with = FALSE]
  
  #3 Get latest 52 weeks and find promoted and non promoted price at PPG level for these PPG's
  latest_52 = sort(unique(master_with_flag_1$`Week End Date`), decreasing = T)[1:365]
  master_zero_rsp = master_with_flag_1[`Week End Date` %in% latest_52]
  
  master_zero_rsp[,Key_Zero := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  master_zero_rsp = master_zero_rsp[Key_Zero %in% ppg_zero_rsp$Key]              #FILTER only those PPG which have latest rsp = 0
  
  #3.1 Promoted weeks price
  master_zero_rsp_prom = master_zero_rsp[Flag_TPR_HEA == 1]
  # Fixed: Use list() and character vector for by= instead of .()
  zero_rsp_by_cols_item <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description", "ITEM")
  master_zero_rsp_prom = master_zero_rsp_prom[, list(Value = sum(Value), Units = sum(Units), Price = sum(Value)/sum(Units)), by = zero_rsp_by_cols_item]
  
  #roll to PPG level
  zero_rsp_by_cols <- c("PPG", "SECTOR 2", "TRADING COMPANY", "PRODUCT RANGE", "FORMAT", "PPG_Description")
  master_zero_rsp_prom[, weight := Units/sum(Units), by = zero_rsp_by_cols]
  master_zero_rsp_prom = master_zero_rsp_prom[, list(Promo_Price = sum(Price*weight)), by = zero_rsp_by_cols]
  master_zero_rsp_prom = master_zero_rsp_prom[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  
  #3.2 Not Promoted weeks price
  master_zero_rsp_not_prom = master_zero_rsp[Flag_TPR_HEA == 0]
  master_zero_rsp_not_prom = master_zero_rsp_not_prom[, list(Value = sum(Value), Units = sum(Units), Price = sum(Value)/sum(Units)), by = zero_rsp_by_cols_item]
  
  #roll to PPG level
  master_zero_rsp_not_prom[, weight := Units/sum(Units), by = zero_rsp_by_cols]
  master_zero_rsp_not_prom = master_zero_rsp_not_prom[, list(Not_Promo_Price = sum(Price*weight)), by = zero_rsp_by_cols]
  master_zero_rsp_not_prom = master_zero_rsp_not_prom[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  
  #4 Get RSP for these PPG
  ppg_zero_rsp = merge(ppg_zero_rsp, master_zero_rsp_prom, all.x = T, by = "Key" )
  # Fixed: Use ..cols syntax instead of .() for column selection
  not_prom_merge_cols <- c("Key", "Not_Promo_Price")
  ppg_zero_rsp = merge(ppg_zero_rsp, master_zero_rsp_not_prom[, ..not_prom_merge_cols], all.x = T, by = "Key" )
  
  ppg_zero_rsp[,RSP_Unit := ifelse(Not_Promo_Price != 0, round(Not_Promo_Price,1), round(Promo_Price,1))]
  
  #5 Replace this rsp in RB_Fin
  for (u_key in ppg_zero_rsp$Key){
    ppg_rsp = ppg_zero_rsp$RSP_Unit[ppg_zero_rsp$Key == u_key]
    RB_Fin[Key == u_key,RSP_Unit := ppg_rsp]
    
  }
  
  #6 Re-calculate RSP dependent columns
  RB_Fin[,FM_Abs_Unit_1 := (RSP_Unit/(1+VAT)) - Net_Cost_Unit]
  RB_Fin[,`FM%` := FM_Abs_Unit_1*(1+VAT)/RSP_Unit]
  
  
  #----------------------------------------RB_Fin modification done----------------------------------------------------#
  
  
  
  #MERGE BASE SALES, RSP, AND RB FINANCIAL
  
  opt_data = merge(tesco_base_sales, RB_Fin, by = "Key", all.x = T)
  opt_data$Key = NULL
  
  base_sale = opt_data
  
  #Input for Shiny
  shiny_ip_tesco_cal = base_sale
  
  #Save data for shiny
  write.csv(shiny_ip_tesco_cal,"5 Base_Sales_Weekly_and_RB_Fin.csv", row.names = F)
  
  #rm(var)
  
  
  #============================================> 10 Shiny Optimizer Files <===================================================
  
  #10.1 Investment and Slot restriction optimizer
  
  #KEEP ONLY PPG WHICH ARE IN OPT_DATA
  #Because, In opt_data, delist ppg are excluded. Therefore, no need to show delist PPG in optimizer screen (ly_kpi_2 - is shown
  # on optimizer screen)
  
  ly_kpi_2[,Key := paste(PPG, `SECTOR 2`, `TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  ly_kpi_2$RSP_Unit = NULL   #First remove and then add new RSP (new RSP take care of seasonality)
  ly_kpi_2 = merge(ly_kpi_2, RB_Fin[,.(Key, RSP_Unit)], by = "Key", all.x = T)   #add new RSP
  
  #Remove Key
  ly_kpi_2$Key = NULL
  
  #Calculate KPI's related to RSP_Unit
  ly_kpi_2[,LSM_Promo_Price_Max := RSP_Unit]
  ly_kpi_2[,Non_LSM_Min_Promo_Price := 0.33*RSP_Unit]
  ly_kpi_2[,Non_LSM_Max_Promo_Price := 0.9*RSP_Unit]
  
  ly_kpi_2 = ly_kpi_2[ ly_kpi_2$PPG %in% opt_data$PPG, ]
  shiny_ip_prod_restrictions = ly_kpi_2
  
  exclude_ppg = exclude_ppg[ exclude_ppg$PPG %in% opt_data$PPG ]
  
  ###Inserting 2019 dates###
  # Fix: Ensure date types match for merge
  date_maping$last_year_date = as.Date(date_maping$last_year_date, format="%m/%d/%Y")
  date_maping$current_year_date = as.Date(date_maping$current_year_date, format="%m/%d/%Y")
  exclude_ppg$`Week End Date` = as.Date(exclude_ppg$`Week End Date`)
  
  exclude_ppg = merge(exclude_ppg, date_maping, by.x = "Week End Date", by.y = "last_year_date", all.x = T)                #addon
  exclude_ppg = exclude_ppg[complete.cases(exclude_ppg),]   #filter only dates where 2019 data is present - 52 weeks   #addon
  
  shiny_ip_exclude_ppg = exclude_ppg
  
  #Save shiny output
  #write.csv(shiny_ip_prod_restrictions, paste0(opt_path,"4 Investment_and_Slot_restrictions_optimizer.csv"), row.names = F)
  #write.csv(shiny_ip_exclude_ppg, paste0(opt_path,"7 Last year Incremental kpi's weekly_for_excluded_ppg.csv"), row.names = F)
  
  #10.2 Calculate Tesco Cal Base Sales 2019
  
  base_sale_roll = base_sale[,.(Base_Units = sum(Base_Units), RSP_Unit = max(RSP_Unit),Net_Cost_Unit = max(Net_Cost_Unit),
                                `FM%` = max(`FM%`), COGS_Unit = max(COGS_Unit), BIP_Case = max(BIP_Case),
                                OID = max(OID), No_Of_Units = max(No_Of_Units), STP_Unit = max(STP_Unit),
                                OID_Unit = max(OID_Unit), UNCR_Unit = max(UNCR_Unit)), 
                             by = .(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Tesco_Week_No)]
  
  #base_sale_roll = base_sale_roll[Tesco_Week_No != 0]
  
  shiny_ip_optimizer = base_sale_roll
  
  #Save data for shiny
  write.csv(shiny_ip_optimizer,"6 Base_Sales_Rolled_up_and_RB_Fin.csv", row.names = F)
  
  
  #----------------------------------------11. Generate Additional Event List - TPR ------------------------------------------
  
  #11.1 Get Required fileds
  # event_optim = master_with_flag[Flag_RB_Financial == 1 & Flag_Modeled_Betas==1 & Flag_HEA ==1 ,
  #                                .(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,Units,`ACV Distribution (C) (any promo)`,
  #                                 `Promoted Price Elasticity_Betas`)]
  
  
  #quick fix for display type
  
  event_optim = master_with_flag_1[Flag_RB_Financial == 1 & Flag_Modeled_Betas==1 & Flag_HEA ==1 & 
                                     Flag_Display_HEA == 0 & DL != 1,
                                   .(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Units,
                                     `ACV Distribution`,`ACV Distribution (C) (any promo)`,
                                     `ACV Distribution (C) (multibuy and displ`, `ACV Distribution (C) (multibuy only)`,
                                     `ACV Distribution (C) (feature and displa`, `ACV Distribution (C) (display only)`,
                                     `ACV Distribution (C) (feature only)`,`ACV Distribution (C) (Unsupported)`,`ACV Distribution (C) (total multibuy)`,
                                     `ACV Distribution Multibuy_Feature_Display`, `ACV Distribution Multibuy_Feature`
                                   )]
  
  #11.2 Calcluate weighted_ACV_ANY_PROMO and weighted_price_elasticity
  #ppg_volume = event_optim[,.(Total_Sales = sum(Volume)), by =.(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT)]
  #event_optim = merge(event_optim, ppg_volume, by = "PPG", all.x = T)
  #event_optim[,Weight := Volume/Total_Sales]
  
  event_optim[,weight := Units/sum(Units), by = .(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description) ]
  
  #Calulate weighted parameters and sum up to PPG level
  # event_optim[,Weighted_ACV_Any_Promo := `ACV Distribution (C) (any promo)`*Weight]
  # event_optim[,Weighted_Price_Elasticity := `Promoted Price Elasticity_Betas`*Weight]
  
  event_optim = event_optim[,.(ACV_D = sum(`ACV Distribution`*weight), 
                               ACV_D_Any_Promo = sum(`ACV Distribution (C) (any promo)`*weight),
                               ACV_D_Multibuy_Display = sum(`ACV Distribution (C) (multibuy and displ`*weight),
                               ACV_D_Multibuy_Only = sum(`ACV Distribution (C) (multibuy only)`*weight),
                               ACV_D_Feature_Display = sum(`ACV Distribution (C) (feature and displa`*weight),
                               ACV_D_Display_Only = sum(`ACV Distribution (C) (display only)`*weight),
                               ACV_D_Feature_Only = sum(`ACV Distribution (C) (feature only)`*weight),
                               ACV_D_Unsupported = sum(`ACV Distribution (C) (Unsupported)` * weight),
                               ACV_D_Total_Multibuy = sum(`ACV Distribution (C) (total multibuy)`* weight),
                               ACV_D_Multibuy_Feature_Display = sum(`ACV Distribution Multibuy_Feature_Display`*weight),
                               ACV_D_Multibuy_Feature = sum(`ACV Distribution Multibuy_Feature`*weight)
  ),
  by = .(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  
  event_optim[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]  #for merging
  
  #add PPG Elasticities
  event_optim = data.table(merge(event_optim,ppg_elasticity, by = c("PPG","SECTOR 2","TRADING COMPANY",
                                                                    "PRODUCT RANGE","FORMAT","PPG_Description"), all.x = T))
  
  event_optim = event_optim[complete.cases(event_optim)]                             #remove PPG's which are not present in ppg_elasticity
  
  #11.3 Get latest RSP against all
  event_optim = merge(event_optim,RB_Fin[,.(Key,RSP_Unit,Net_Cost_Unit,`FM%`,VAT,COGS_Unit, OID_Unit, UNCR_Unit,STP_Unit)], 
                      by = "Key", all.x = T)
  event_optim[,RSP_Unit_Min := RSP_Unit/3]
  event_optim[,RSP_Unit_Min := round(RSP_Unit_Min*2,0)/2]
  
  #11.4 For a PPG, increase Min RSP_Unit by 0.5 and record EVENT MULTIPLIER, DISCOUNT.....
  df <- data.table()
  
  ppg_list <- unique(event_optim$PPG)
  
  for (ppg in ppg_list) {
    
    one_ppg <- event_optim[PPG == ppg]
    
    min_promo <- 0.5
    max_promo <- floor(as.numeric(one_ppg$RSP_Unit) * 2) / 2
    max_promo <- ifelse(
      max_promo == as.numeric(one_ppg$RSP_Unit),
      max_promo - 0.5,
      max_promo
    )
    
    Promo_Price <- seq(from = min_promo, to = max_promo, by = 0.5)
    
    one_ppg <- cbind(one_ppg, Promo_Price)
    
    one_ppg[, Price_Chg_Idx := Promo_Price / RSP_Unit]
    one_ppg[, Base_Units := 100000]
    
    one_ppg[, Inc_TPR_Only :=
              ((Price_Chg_Idx^`Promoted Price Elasticity_Betas`) - 1) *
              ACV_D_Unsupported * Base_Units / ACV_D]
    
    one_ppg[, Inc_TPR_Feature :=
              ((Price_Chg_Idx^`Promoted Price Elasticity_Betas`) *
                 `Feature Only_Betas` - 1) *
              ACV_D_Feature_Only * Base_Units / ACV_D]
    
    one_ppg[, Inc_TPR_Display :=
              ((Price_Chg_Idx^`Promoted Price Elasticity_Betas`) *
                 `Display Only_Betas` - 1) *
              ACV_D_Display_Only * Base_Units / ACV_D]
    
    one_ppg[, Inc_TPR_Feature_Display :=
              ((Price_Chg_Idx^`Promoted Price Elasticity_Betas`) *
                 `Feature and Display_Betas` - 1) *
              ACV_D_Feature_Display * Base_Units / ACV_D]
    
    one_ppg[, Inc_Multibuy_Feature :=
              (`Total Multibuy Effect_Betas` *
                 `Feature Only_Betas` - 1) *
              ACV_D_Multibuy_Feature * Base_Units / ACV_D]
    
    one_ppg[, Inc_Multibuy_Display :=
              (`Total Multibuy Effect_Betas` *
                 `Display Only_Betas` - 1) *
              ACV_D_Multibuy_Display * Base_Units / ACV_D]
    
    one_ppg[, Inc_Multibuy_Feature_Display :=
              (`Total Multibuy Effect_Betas` *
                 `Feature and Display_Betas` - 1) *
              ACV_D_Multibuy_Feature_Display * Base_Units / ACV_D]
    
    one_ppg[, Inc_Multibuy_Only :=
              (`Total Multibuy Effect_Betas` - 1) *
              ACV_D_Total_Multibuy * Base_Units / ACV_D]
    
    one_ppg[, Inc_Total :=
              Inc_TPR_Only + Inc_TPR_Feature + Inc_TPR_Display +
              Inc_TPR_Feature_Display + Inc_Multibuy_Feature +
              Inc_Multibuy_Display + Inc_Multibuy_Feature_Display +
              Inc_Multibuy_Only]
    
    one_ppg[, Event_Multiplier := Inc_Total / Base_Units]
    one_ppg[, Event_Multiplier_Discount := Inc_Total / Base_Units]
    one_ppg[, Event_Multiplier_Display := 0]
    one_ppg[, Total_Units_Sales := Base_Units + Inc_TPR_Only]
    one_ppg[, Discount := (RSP_Unit - Promo_Price) / RSP_Unit]
    
    df <- rbind(df, one_ppg)
  }
  
  # browser() # DISABLED for API
  
  additional_events = df
  setDT(additional_events)
  additional_events[,FM_Abs := (RSP_Unit/(1+VAT)) - Net_Cost_Unit]
  additional_events[,FM_Percent := FM_Abs*(1+VAT)/RSP_Unit]
  additional_events[,Retro_Fund_Unit := 0.335]
 # additional_events[,Retro_Fund_Total := Total_Units_Sales*Retro_Fund_Unit]
  additional_events[,OID_Total := OID_Unit*Total_Units_Sales]                       #addon
  additional_events[,UNCR_Total := (RSP_Unit - Promo_Price)*Total_Units_Sales]                     #addon
  additional_events[,Gross_Sales := Promo_Price*Total_Units_Sales]    
  additional_events[,Retro_Fund_Total := Gross_Sales*Retro_Fund_Unit]
  #addon
  additional_events[,Trade_Investment := UNCR_Total + OID_Total + Retro_Fund_Total] #addon
  additional_events[,Net_Revenue := Gross_Sales- Trade_Investment]                  #addon
  additional_events[,NIS :=  Gross_Sales - UNCR_Total - OID_Total]                  #addon
  additional_events[,Net_Cost_Total := Total_Units_Sales*Net_Cost_Unit]
  additional_events[,COGS_Total := COGS_Unit*Total_Units_Sales]
  additional_events[,GM_Abs := Net_Revenue - COGS_Total]
  additional_events[,Inc_GM_Abs := Inc_TPR_Only*GM_Abs/Total_Units_Sales]
  additional_events[,Inc_Revenue := Inc_TPR_Only*Net_Revenue/Total_Units_Sales]
  additional_events[,Inc_NIS := Inc_TPR_Only*NIS/Total_Units_Sales]
  additional_events[,ROI_GM := Inc_GM_Abs/Trade_Investment]
  additional_events[,ROI_Rev := Inc_Revenue/Trade_Investment]
  additional_events[,ROI_NIS := Inc_NIS/Trade_Investment]
  
  
  #additional columns for roi
  additional_events[,R_Total_Sales := Base_Units + Inc_Total]
  additional_events[,R_Gross_Sales_Base := STP_Unit *Base_Units]
  additional_events[,R_Gross_Sales_Inc := STP_Unit *Inc_Total]
  additional_events[,R_Gross_Sales_Total := R_Gross_Sales_Base + R_Gross_Sales_Inc]
  additional_events[,R_UNCR_Base := (RSP_Unit - Promo_Price)*Base_Units]
  additional_events[,R_UNCR_Inc := (RSP_Unit - Promo_Price)*Inc_Total]
  additional_events[,R_UNCR_Total := R_UNCR_Base + R_UNCR_Inc]
  additional_events[,R_OID_Base := OID_Unit*Base_Units]
  additional_events[,R_OID_Inc := OID_Unit*Inc_Total]
  additional_events[,R_OID_Total := R_OID_Base + R_OID_Inc]
  additional_events[,R_Retro_Base := 0]
  additional_events[,R_Retro_Inc := Retro_Fund_Unit*R_Total_Sales]
  additional_events[,R_Retro_Total := R_Retro_Base + R_Retro_Inc]
  additional_events[,R_NIS_Base := R_Gross_Sales_Base - R_UNCR_Base - R_OID_Base]
  additional_events[,R_NIS_Inc := R_Gross_Sales_Inc - R_UNCR_Inc - R_OID_Inc]
  additional_events[,R_NIS_Total := R_NIS_Base + R_NIS_Inc]
  additional_events[,R_COGS_Base := COGS_Unit*Base_Units]
  additional_events[,R_COGS_Inc := COGS_Unit*Inc_Total]
  additional_events[,R_COGS_Total := R_COGS_Base + R_COGS_Inc]
  
  #additional calculations for roi
  additional_events[,R_Retro_Inc := R_Retro_Inc]
  additional_events[,R_Retro_Base := R_Retro_Base]
  additional_events[,R_Retro_Total := R_Retro_Total]
  additional_events[,R_Display_Fee_Base := 0]
  additional_events[,R_Display_Fee_Inc := 0]
  additional_events[,R_Display_Fee_Total := R_Display_Fee_Base + R_Display_Fee_Inc]  
  additional_events[,R_Total_Trade_Inv_Base := R_UNCR_Base + R_OID_Base + R_Retro_Base + R_Display_Fee_Base]
  additional_events[,R_Total_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Fee_Inc]
  additional_events[,R_Total_Trade_Inv_Total := R_Total_Trade_Inv_Base + R_Total_Trade_Inv_Inc]
  additional_events[,R_Net_Revenue_Base := R_NIS_Base - R_Retro_Base - R_Display_Fee_Base]
  additional_events[,R_Net_Revenue_Inc := R_NIS_Inc - R_Retro_Inc - R_Display_Fee_Inc]
  additional_events[,R_Net_Revenue_Total := R_Net_Revenue_Base + R_Net_Revenue_Inc]
  additional_events[,R_GM_Base := R_Net_Revenue_Base - R_COGS_Base]
  additional_events[,R_GM_Inc := R_Net_Revenue_Inc - R_COGS_Inc]
  additional_events[,R_GM_Total := R_GM_Base + R_GM_Inc]
  
  #Calculating ROI
  additional_events[,R_ROI_GM := R_GM_Inc/R_Total_Trade_Inv_Inc]
  additional_events[,R_ROI_Rev := R_Net_Revenue_Inc/R_Total_Trade_Inv_Inc]
  additional_events[,R_ROI_NIS := R_NIS_Inc/R_Total_Trade_Inv_Inc]
  
  #write.csv(additional_events, "test.csv",row.names = F)
  
  additional_events = additional_events[,.(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,RSP_Unit,Net_Cost_Unit ,
                                           COGS_Unit,Promo_Price,Event_Multiplier,Event_Multiplier_Display,Event_Multiplier_Discount,
                                           Discount,ROI_GM,ROI_Rev,ROI_NIS,Promo_Price, R_ROI_GM,R_ROI_Rev,R_ROI_NIS
  )]
  additional_events[,Display := " "]
  additional_events[,Display_Cost := 0]
  additional_events[,Display_Flag := 0]
  additional_events[,Event_Multiplier_Tesco := Event_Multiplier]
  additional_events[,Event_Multiplier_Display_Tesco := Event_Multiplier_Display]
  additional_events[,Event_Multiplier_Discount_Tesco := Event_Multiplier_Discount]
  
  #write.csv(additional_events,"1 Additional_Discount_Events.csv", row.names = F)
  
  e1 = additional_events[,.(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Discount,Display,Display_Cost,Display_Flag,
                            Event_Multiplier,Event_Multiplier_Discount, Event_Multiplier_Display,Event_Multiplier_Tesco, 
                            Event_Multiplier_Discount_Tesco, Event_Multiplier_Display_Tesco,
                            ROI_GM,ROI_Rev,ROI_NIS,Promo_Price,RSP_Unit,R_ROI_GM,R_ROI_Rev,R_ROI_NIS)]
  
  
  e1[,Flag_Event_Exist_in_History := 1]
  e1[,Flag_Historical_Display_Event := 0]
  
  # e2 = events_shiny_2[,.(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,Discount,Display_Type,Display_Cost,
  #                      Flag_Display_HEA,Event_Multiplier, Event_Multiplier_Discount, Event_Multiplier_Display,
  #                      Event_Multiplier_Tesco, Event_Multiplier_Discount_Tesco, Event_Multiplier_Display_Tesco,
  #                      ROI_GM,ROI_Rev,ROI_NIS,Promo_Price)]
  # names(e2) = c("PPG","SECTOR 2","TRADING COMPANY","PRODUCT RANGE", "FORMAT" ,"Discount","Display","Display_Cost","Display_Flag",
  #               "Event_Multiplier","Event_Multiplier_Discount", "Event_Multiplier_Display" ,
  #               "Event_Multiplier_Tesco", "Event_Multiplier_Discount_Tesco", "Event_Multiplier_Display_Tesco",
  #               "ROI_GM","ROI_Rev","ROI_NIS","Promo_Price")
  
  # complete_events = rbind(e1,e2)
  
  
  #write.csv(complete_events, "History Events and Additional Discount Events.csv", row.names = F)
  
  
  #-------------------------------------------------------Events List Closed-----------------------------------------------------#
  
  #rm(e1)
  #rm(e2)
  rm(df)
  rm(one_ppg)
  rm(Promo_Price)
  rm(min_promo)
  rm(max_promo)
  rm(additional_events)
  
  
  #-------------------------------------------------------12 Generate Additional Display List-----------------------------------------#
  
  #12.1 Read and keep only display
  
  # display_events = events_shiny_2[,.(Key, PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,Event_Number,Flag_Display_HEA,
  #                                    Base_Units, Base_Units_Tesco, Display_Cost, Display_Type,
  #                                    ACV_D_Any_Promo, ACV_D_Multibuy_Display, ACV_D_Multibuy_Only, ACV_D_Feature_Display,
  #                                    ACV_D_Display_Only, ACV_D_Feature_Only, ACV_D_Unsupported, ACV_D_Total_Multibuy, 
  #                                    ACV_D_Multibuy_Feature_Display, ACV_D_Multibuy_Feature, Discount
  #                                    )]
  # 
  # display_events = display_events[Flag_Display_HEA == 1 & !(is.na(Display_Type))]             #!(is.na(Display_Type)) -> remove retailer funded events      
  
  #Changes in master_with_flag
  
  idx = order(master_with_flag$ITEM, master_with_flag$`Week End Date`)
  master_with_flag = master_with_flag[idx,]
  
  # Ensure retailer is available
  if (!exists("retailer")) retailer <- "Carrefour"
  
  if (toupper(RETAILER) == "TESCO") {
    # Tesco: re-time to Tesco weeks and use Tesco-specific display flag
    master_with_flag[, Flag_Display_Tesco :=
                       tesco_event_num(Flag_Display_HEA, retailer_week_end_day),
                     by = .(ITEM)]
    master_with_flag[, Base_Units_Tesco :=
                       tesco_base_units(`Base Units`, days_in_current_week, days_in_next_week)]
    master_with_flag[, Base_Units_Tesco := Base_Units_Tesco * Flag_Display_Tesco]
    
  } else {
    # Non‑Tesco: no week shifting, just use original flags/units
    master_with_flag[, Flag_Display_Tesco := Flag_Display_HEA]
    master_with_flag[, Base_Units_Tesco := `Base Units` * Flag_Display_Tesco]
  }
  
  # browser() # DISABLED for API
 events_shiny_2=events_shiny
  display_cost_per_ppg_event = events_shiny_2[Flag_Display_HEA == 1, .(Display_Cost = max(Display_Cost)),
                                              by = .(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,
                                                     FORMAT,PPG_Description,Display_Type)]
  setnames(display_cost_per_ppg_event,"Display_Type","DISPLAY TYPE_HEA")
  
  
  display_events = master_with_flag[Flag_RB_Financial == 1 & Flag_Modeled_Betas==1 & Flag_HEA ==1 & 
                                      Flag_Display_HEA == 1,
                                    .(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Units,`Base Units`,Base_Units_Tesco,
                                      `ACV Distribution`,`ACV Distribution (C) (any promo)`,
                                      `ACV Distribution (C) (multibuy and displ`, `ACV Distribution (C) (multibuy only)`,
                                      `ACV Distribution (C) (feature and displa`, `ACV Distribution (C) (display only)`,
                                      `ACV Distribution (C) (feature only)`,`ACV Distribution (C) (Unsupported)`,`ACV Distribution (C) (total multibuy)`,
                                      `ACV Distribution Multibuy_Feature_Display`, `ACV Distribution Multibuy_Feature`,`DISPLAY TYPE_HEA`,
                                      Flag_Display_HEA, `Net invetsments_HEA`
                                    )]
  
  
  #12.2 Making sure distince display type appear for PPG
  display_events[,weight := Units/sum(Units), by = .(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,
                                                     FORMAT,PPG_Description,`DISPLAY TYPE_HEA`)]
  
  # Roll up again with weight in consideration
  
  display_events = display_events[,.(Flag_Display_HEA = max(Flag_Display_HEA),
                                     Base_Units = sum(`Base Units`), Base_Units_Tesco = sum(display_events$Base_Units_Tesco),
                                     Units = sum(Units),
                                     Display_Cost = sum(`Net invetsments_HEA`*weight), 
                                     ACV_D = sum(`ACV Distribution`*weight), 
                                     ACV_D_Any_Promo = sum(`ACV Distribution (C) (any promo)`*weight),
                                     ACV_D_Multibuy_Display = sum(`ACV Distribution (C) (multibuy and displ`*weight),
                                     ACV_D_Multibuy_Only = sum(`ACV Distribution (C) (multibuy only)`*weight),
                                     ACV_D_Feature_Display = sum(`ACV Distribution (C) (feature and displa`*weight),
                                     ACV_D_Display_Only = sum(`ACV Distribution (C) (display only)`*weight),
                                     ACV_D_Feature_Only = sum(`ACV Distribution (C) (feature only)`*weight),
                                     ACV_D_Unsupported = sum(`ACV Distribution (C) (Unsupported)` * weight),
                                     ACV_D_Total_Multibuy = sum(`ACV Distribution (C) (total multibuy)`* weight),
                                     ACV_D_Multibuy_Feature_Display = sum(`ACV Distribution Multibuy_Feature_Display`*weight),
                                     ACV_D_Multibuy_Feature = sum(`ACV Distribution Multibuy_Feature`*weight)
  ),
  by = .(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,
         FORMAT,PPG_Description,`DISPLAY TYPE_HEA`)]
  
  #Merge PPG display cost - normal join to Ignore NA in display cost
  
  
  #display_events = merge(display_events, display_cost_per_ppg_event, by = c("PPG", "SECTOR 2","TRADING COMPANY","PRODUCT RANGE",
  #                                                                         "FORMAT","PPG_Description","DISPLAY TYPE_HEA"), allow.cartesian = T)
  
  
  # Merge ppg_elasticities 
  display_events = merge(display_events, ppg_elasticity, by = c("PPG", "SECTOR 2","TRADING COMPANY","PRODUCT RANGE",
                                                                "FORMAT","PPG_Description"), all.x = T)
  
  display_events_brand = display_events     #creating a copy => to use later
  
  #12.3 Get latest rsp against all
  display_events[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  display_events = merge(display_events,RB_Fin[,.(Key,RSP_Unit,Net_Cost_Unit,`FM%`,VAT,COGS_Unit, OID_Unit, UNCR_Unit,STP_Unit)], 
                         by = "Key", all.x = T)
  
  
  empty_df = data.table()
  for (i in 1:nrow(display_events)){
    
    
    ppg_events = display_events[i, ]
    
    #12.3 Create all possible events
    
    #Calculate promo price sequence
    ppg_events[,RSP_Unit_Min := RSP_Unit/3]
    ppg_events[,RSP_Unit_Min := round(RSP_Unit_Min*2,0)/2]
    
    rsp_min = mean(ppg_events$RSP_Unit_Min)
    rsp = mean(ppg_events$RSP_Unit)
    
    #min_promo = as.numeric(rsp_min)
    min_promo = 0.5                                 #after Atif's input
    max_promo = floor(as.numeric(rsp)*2)/2
    max_promo = ifelse(max_promo == as.numeric(rsp),max_promo - 0.5,max_promo)
    Promo_Price = seq(from = min_promo, to = max( max_promo), by = 0.5)
    
    #Attach Promo Price
    ppg_events = cbind(ppg_events, Promo_Price)
    
    empty_df = rbind(empty_df, ppg_events)
    
  }
  
  # #12.4 Calculate Event Multiplier - 2 iterations - for tesco/nielsen
  
  display_events = empty_df
  
  rm(min_promo,max_promo,Promo_Price, ppg_events, empty_df)
  
  #12.4.1 For Nielsen
  df = event_multiplier_fun(display_events, RETAILER)
  
  #12.4.2 Place in the data
  display_events[,Event_Multiplier := df[[1]] ] ; display_events[,Event_Multiplier_Display := df[[2]] ] ;
  display_events[,Event_Multiplier_Discount := df[[3]] ] ; display_events[,ROI_GM := df[[4]] ];
  display_events[,ROI_Rev := df[[8]] ] ; display_events[,ROI_NIS := df[[9]] ]
  display_events[,R_ROI_GM := df[[10]] ] ; display_events[,R_ROI_Rev := df[[11]] ]
  display_events[,R_ROI_NIS := df[[12]] ]
  
  
  #12.4.3 Calculate event multiplier for tesco
  
  display_events[,Event_Multiplier_Tesco := df[[1]] ] ; display_events[,Event_Multiplier_Display_Tesco := df[[2]] ] ;
  display_events[,Event_Multiplier_Discount_Tesco := df[[3]] ] 
  #display_events$`DISPLAY TYPE_HEA`
  display_events[,Display_Flag := 1]
  display_events[,Display := `DISPLAY TYPE_HEA`]
  display_events[,Discount := 1- (Promo_Price/RSP_Unit)]
  
  
  write.csv(display_events,"2 Display Events that Exists in History_.csv", row.names = F)
  
  e2 = display_events[,.(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Discount,Display,Display_Cost,Display_Flag,
                         Event_Multiplier,Event_Multiplier_Discount, Event_Multiplier_Display,Event_Multiplier_Tesco, 
                         Event_Multiplier_Discount_Tesco, Event_Multiplier_Display_Tesco,
                         ROI_GM,ROI_Rev,ROI_NIS,Promo_Price,RSP_Unit,R_ROI_GM,R_ROI_Rev,R_ROI_NIS)]
  
  write.csv(display_events,"dis.csv")
  e2[,Flag_Event_Exist_in_History := 1]
  e2[,Flag_Historical_Display_Event := 1]
  
  #write.csv(display_events, "Additional Display Events for PPG with historic display events.csv", row.names = F)
  
  #Find out the combination of PPG and display type allready covered in display_events
  display_covered = unique(display_events[,.(PPG,`DISPLAY TYPE_HEA`)])
  display_covered$`DISPLAY TYPE_HEA`=display_covered$`DISPLAY TYPE_HEA`
  display_covered[,U_Key := paste(PPG, `DISPLAY TYPE_HEA`)]
  
  #Compute elasticities for PPG
  
  #---------------------------Compute Dsiplay Events at Brand Level-----------------------------------
  
  display_cost_per_display_type = events_shiny_2[Flag_Display_HEA == 1, .(Display_Cost = max(Display_Cost)),
                                                 by = .(Display_Type)]
  
  
  
  #12.4.4 Display Events - get ACV event
  display_events_brand$`DISPLAY TYPE_HEA`=display_events_brand$`DISPLAY TYPE_HEA`
  display_events_ACV = display_events_brand[,weight := Units/sum(Units), by =`DISPLAY TYPE_HEA`]   #weight before roll up
  display_events_ACV = display_events_ACV[,.(
    #Display_Cost = sum(Display_Cost*weight),  --------- no longer needed / added later
    ACV_D = sum(ACV_D*weight),ACV_D_Any_Promo = sum(ACV_D_Any_Promo*weight),
    ACV_D_Multibuy_Display = sum(ACV_D_Multibuy_Display*weight),
    ACV_D_Multibuy_Only = sum(ACV_D_Multibuy_Only*weight),
    ACV_D_Feature_Display = sum(ACV_D_Feature_Display*weight),
    ACV_D_Display_Only = sum(ACV_D_Display_Only *weight),
    ACV_D_Feature_Only = sum(ACV_D_Feature_Only*weight),
    ACV_D_Unsupported = sum(ACV_D_Unsupported* weight),
    ACV_D_Total_Multibuy = sum(ACV_D_Total_Multibuy* weight),
    ACV_D_Multibuy_Feature_Display = sum(ACV_D_Multibuy_Feature_Display *weight),
    ACV_D_Multibuy_Feature = sum(ACV_D_Multibuy_Feature*weight)),
    by = .(`DISPLAY TYPE_HEA`)
  ]
  
  #Add display cost in above
  display_events_ACV = merge(display_events_ACV, display_cost_per_display_type, by.x = "DISPLAY TYPE_HEA",
                             by.y = "Display_Type",all.x = T)
  
  
  
  #12.4.4 Elasticities at PPG level
  display_events_Betas = copy(ppg_elasticity)
  
  display_events_Betas = display_events_Betas[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  display_events_Betas[,`:=`(PPG = NULL, `SECTOR 2`= NULL, `TRADING COMPANY` = NULL,
                             `PRODUCT RANGE` = NULL, FORMAT = NULL, PPG_Description = NULL)]
  
  #Get display events ACV and Betas together
  display_events_acv_betas = data.table(merge(as.data.frame(display_events_Betas),display_events_ACV, by = NULL))
  
  #12.4.5 Get the base sales per PPG from Historic Event List
  base_units_historic_events = events_shiny_2[,.(Base_Units_T = mean(Base_Units), Base_Units = mean(Base_Units)),
                                              by = .(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description)]
  base_units_historic_events = base_units_historic_events[,Key := paste(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,
                                                                        PPG_Description)]
  
  #12.4.6 Get financial for these PPG
  fin_ppg = RB_Fin[,.(Key,RSP_Unit,COGS_Unit,Net_Cost_Unit,OID_Unit,UNCR_Unit,STP_Unit,VAT)]
  
  #Merge Base Sale per PPG and Financial
  

  additional_d_events = merge(base_units_historic_events, fin_ppg, all.x = T, by = "Key")
  
  #Merge display_events_acv_betas
  additional_d_events = merge(display_events_acv_betas, additional_d_events, by = "Key", all.x = T)
  
  #12.4.7 Calculate event multiplier by varying Promo Price
  
  empty_df = data.table()
  
  for (i in 1:nrow(additional_d_events)){
    
    #ppg = "AA6"
    #ppg_events = additional_d_events[PPG == ppg ]
    #i=1
    ppg_events = additional_d_events[i,]
    
    #12.3 Create all possible events
    
    #Calculate promo price sequence
    ppg_events[,RSP_Unit_Min := RSP_Unit/3]
    ppg_events[,RSP_Unit_Min := round(RSP_Unit_Min*2,0)/2]
    
    rsp_min = mean(ppg_events$RSP_Unit_Min)
    rsp = mean(ppg_events$RSP_Unit)
    
    #min_promo = as.numeric(rsp_min)
    min_promo = 0.5
    max_promo = floor(as.numeric(rsp)*2)/2
    max_promo = ifelse(max_promo == as.numeric(rsp),max_promo - 0.5,max_promo)
    Promo_Price = seq(from = min_promo, to = max(max_promo), by = 0.5)
    
    #Attach Promo Price
    ppg_events = merge(as.data.frame(ppg_events), data.frame(Promo_Price), by = NULL)
    
    empty_df = rbind(empty_df, ppg_events)
    
  }
  
  #12.4 Calculate Event Multiplier - 2 iterations - for tesco/nielsen
  
  additional_d_events = empty_df
  
  rm(min_promo,max_promo,Promo_Price, ppg_events, empty_df)
  
  #12.4.1 For Nielsen
  # browser() # DISABLED for API
  write.csv(e2,"hello.csv")
  df = event_multiplier_fun(additional_d_events, RETAILER)
  additional_d_events[,Event_Multiplier := df[[1]] ] ; additional_d_events[,Event_Multiplier_Display := df[[2]] ] ;
  additional_d_events[,Event_Multiplier_Discount := df[[3]] ] ; additional_d_events[,ROI_GM := df[[4]] ];
  additional_d_events[,ROI_Rev := df[[8]] ] ; additional_d_events[,ROI_NIS := df[[9]] ]
  additional_d_events[,R_ROI_GM := df[[10]] ] ; additional_d_events[,R_ROI_Rev := df[[11]] ]
  additional_d_events[,R_ROI_NIS := df[[12]] ]
  
  #12.4.3 Calculate event multiplier for tesco
  
  # Ensure retailer is available
  if (!exists("retailer")) retailer <- "Carrefour"
  
  if (toupper(RETAILER) == "TESCO") {
  additional_d_events[,Event_Multiplier_Tesco := df[[1]] ] ; additional_d_events[,Event_Multiplier_Display_Tesco := df[[2]] ] ;
  additional_d_events[,Event_Multiplier_Discount_Tesco := df[[3]] ] }
  else {
    additional_d_events[,Event_Multiplier_Tesco := 0 ] ; additional_d_events[,Event_Multiplier_Display_Tesco := 0 ] ;
    additional_d_events[,Event_Multiplier_Discount_Tesco := 0] 
  }
  
  #12.4.4 Exclude Display Events which are allready covered
  additional_d_events[, U_Key :=  paste(PPG, `DISPLAY TYPE_HEA`)]
  #additional_d_events = additional_d_events[!(U_Key %in% display_covered$U_Key)]
  
  
  additional_d_events = additional_d_events[,Discount := 1 - (Promo_Price/RSP_Unit)]
  additional_d_events = additional_d_events[,Display := `DISPLAY TYPE_HEA`]
  additional_d_events = additional_d_events[,Display_Flag := 1]
  
  #write.csv(additional_d_events,"3 Addtional Display Events_.csv", row.names = F)
  
  e3 = additional_d_events[,.(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Discount,Display,Display_Cost,Display_Flag,
                              Event_Multiplier,Event_Multiplier_Discount, Event_Multiplier_Display,
                              ROI_GM,ROI_Rev,ROI_NIS,Promo_Price,RSP_Unit,R_ROI_GM,R_ROI_Rev,R_ROI_NIS)]
  
  e3[,Flag_Event_Exist_in_History := 0]
  e3[,Flag_Historical_Display_Event := 0]
  
  #write.csv(additional_d_events, "Display Events for all PPG with Brand Level Elasticities and Distribution.csv", row.names = F)
  
  #Save file
  write.csv(display_events, "11 Display Events.csv", row.names = F)
  
  
  
  #13==============================================Final Event List - LSM/Non-LSM=========================================#
  
  final_events = rbind(e1,e2,e3,fill = TRUE)
  
  write.csv(e1,"E1.csv")
  write.csv(e1,"E2.csv")
  write.csv(e1,"E3.csv")
  
  final_events = final_events[!(Display %in% c("Retailers back", "Retailer Clearance"))]
  
  # Check if the event happened 90% of the time
  
  display_criteria = events_shiny_2[!(is.na(Display_Type))]
  
  display_criteria = display_criteria[,.(Number_Of_Events = .N), by = .(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,
                                                                        PPG_Description, Display_Type)]
  
  display_criteria = display_criteria[,weight := Number_Of_Events/sum(Number_Of_Events),
                                      by = .(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,                                                                           FORMAT,PPG_Description)]
  
  display_criteria[,Flag_Event_More_Than_90_Percent := ifelse(weight >= 0.9,1,0)]
  display_criteria[,Display := Display_Type]
  
  display_criteria_flag = display_criteria[,.(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT,PPG_Description,Display
                                              ,Flag_Event_More_Than_90_Percent)]
  
  final_events = data.table(merge(as.data.frame(final_events), display_criteria_flag,  
                                  by = c("PPG", "SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT","PPG_Description","Display"), all.x = T))
  
  final_events[is.na(Flag_Event_More_Than_90_Percent), Flag_Event_More_Than_90_Percent := 0]
  
  shiny_ip_event = final_events
  
  shiny_ip_list <- list(shiny_ip_nielsen,shiny_ip_cal_event,shiny_ip_cal_tesco_mapping,shiny_ip_opti_constraints,shiny_ip_prod_restrictions,shiny_ip_optimizer,shiny_ip_tesco_cal,shiny_ip_exclude_ppg,shiny_ip_event,shiny_ip_competition,shiny_ip_retailer_weekEndDay_no,
                        shiny_ip_retailer_week_end_day,shiny_ip_retailer_weekEndDay_no)
  
  #shiny_ip_retailer_week_end_day,shiny_ip_retailer_weekEndDay_no
  
  write.csv(shiny_ip_nielsen,"shiny_ip_nielsen.csv")
  write.csv(shiny_ip_cal_event,"shiny_ip_cal_event.csv")
  write.csv(shiny_ip_cal_tesco_mapping,"shiny_ip_cal_tesco_mapping.csv")
  write.csv(shiny_ip_opti_constraints,"shiny_ip_opti_constraints.csv")
  write.csv(shiny_ip_prod_restrictions,"shiny_ip_prod_restrictions.csv")
  write.csv(shiny_ip_optimizer,"shiny_ip_optimizer.csv")
  write.csv(shiny_ip_tesco_cal,"shiny_ip_tesco_cal.csv")
  write.csv(shiny_ip_exclude_ppg,"shiny_ip_exclude_ppg.csv")
  write.csv(shiny_ip_event,"shiny_ip_event.csv")
  write.csv(shiny_ip_competition,"shiny_ip_competition.csv")
  
  
  return(shiny_ip_list)
}

event_list <- function(final_events,shiny_price){
  #Merge required info
  shiny_price_subset = shiny_price[,.(PPG,LSM_Promo_Price_Min, LSM_Promo_Price_Max, Non_LSM_Max_Promo_Price, Non_LSM_Min_Promo_Price, Global_Floor_Price)]
  final_events = merge(final_events, shiny_price_subset, all.x = T, by = "PPG")
  
  #Remove PPG where LSM price info not present
  final_events = final_events[complete.cases(final_events)]
  
  #Keep only those discounts where Discount is more then 10%
  final_events = final_events[Discount >= 0.1]
  
  #0 Filter Display Events based on Rule
  discount_events = final_events[Display_Flag == 0]   #send all discount events
  
  #Send selected display_events
  display_events = final_events[Display_Flag == 1]
  
  #Check which ppg's have display events in history
  ppg_history = display_events[,.(Display_Exists_in_History = max(Flag_Event_Exist_in_History)),
                               by = .(PPG,`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT)]
  
  #Create display events list - for ppg not promoted in history
  ppg_not_in_history = ppg_history$PPG[ppg_history$Display_Exists_in_History == 0]
  display_events_for_ppg_not_in_history  = display_events[PPG %in% ppg_not_in_history]
  
  #Create display events list - for ppg promoted in history
  display_events_for_ppg_in_history = display_events[!(PPG %in% ppg_not_in_history)]
  
  #If any display event (like GE, Ladder) is promoted more than 90% of time, select only that event - for each PPG
  display_events_dominant = display_events_for_ppg_in_history[Flag_Event_More_Than_90_Percent == 1 ]
  ppg_dominant = unique(display_events_dominant$PPG)
  
  #If there are no display events (like GE, Ladder) that are promoted more than 90% of time, select all display events from history
  display_events_not_dominant = display_events_for_ppg_in_history[!(PPG %in% ppg_dominant) & Flag_Historical_Display_Event == 1]
  
  #Final Display Events
  display_events_for_ppg_in_history = rbind(display_events_dominant, display_events_not_dominant)
  final_display_events = rbind(display_events_for_ppg_in_history, display_events_for_ppg_not_in_history)
  
  #All Events
  final_events = rbind(discount_events,final_display_events)
  
  #1 Filter events based on Global Floor Price
  final_events = final_events[Promo_Price >= Global_Floor_Price]
  
  #2 Filter LSM Events
  shiny_ip_events_lsm = final_events[Promo_Price >= LSM_Promo_Price_Min & Promo_Price <= LSM_Promo_Price_Max ]
  
  #3 Filter LSM Events
  shiny_ip_events_non_lsm = final_events[Promo_Price >= Non_LSM_Min_Promo_Price & Promo_Price <= Non_LSM_Max_Promo_Price ]
  return(list(shiny_ip_events_lsm,shiny_ip_events_non_lsm))
}



comp_display_cost <- function(all_brand_raw,cut_off = 85,manuf,brand){
  #Manipulation
  
  all_brand = copy(all_brand_raw)
  
  #1 Latest 52 weeks
  latest_52_weeks = sort(unique(all_brand$`Week End Date`), decreasing = T)[1:52]
  all_brand = all_brand[`Week End Date` %in% latest_52_weeks]
  
  all_brand_copy = all_brand
  
  #2 Get display cost for RB
  display_cost = sum(all_brand$`Net invetsments_HEA`[all_brand$`TRADING COMPANY` == manuf])
  
  #3 Aggregate to Company-Brand level
  all_brand = all_brand[,.(Units = sum(Units)), by = .(`TRADING COMPANY`,`PRODUCT RANGE`)]
  all_brand[,Market_Share := Units*100/sum(Units)]
  
  #4 Order by Descending Market Share and cumulative Market Share
  all_brand = all_brand[order(all_brand$Market_Share, decreasing = T),]
  all_brand[,Market_Share_Cum := cumsum(Market_Share)]
  
  #5 Keep values within cut-off
  all_brand = all_brand[Market_Share_Cum < cut_off]
  
  #6 Exclude RB Brand
  rb_sales = sum(all_brand$Units[all_brand$`TRADING COMPANY` == manuf])
  comp_brand = all_brand[!(`TRADING COMPANY` == manuf)]
  
  #7 Display Cost Per Unit - from RB data
  display_cost_per_unit = display_cost/rb_sales
  
  #8 Display cost for competition
  comp_brand[,Display_Cost_Comp := display_cost_per_unit*Units]
  comp_brand$Units = NULL
  
  #--------------------nEW CHANGES For Format Level------------------------#
  
  #9 Generate at Brand Format Level
  all_brand_2 = unique(all_brand_copy[,.(Units = sum(Units)),by = .(`TRADING COMPANY`,`PRODUCT RANGE`,FORMAT)])
  all_brand_2 = all_brand_2[,Proportion := Units/sum(Units),by = .(`TRADING COMPANY`,`PRODUCT RANGE`)]
  
  #Keep only those brand which are in comp_brand
  all_brand_2 = all_brand_2[`PRODUCT RANGE` %in% comp_brand$`PRODUCT RANGE`]
  
  #all_brand_2[,.(sum(Units)),by = .(`TRADING COMPANY`,`PRODUCT RANGE`)]
  
  #Merge Comp Brnad
  all_brand_2 = merge(all_brand_2, comp_brand, all.x = T, by = c("TRADING COMPANY","PRODUCT RANGE"))
  
  #10 Update columns as per proportions
  all_brand_2[,Market_Share := Proportion*Market_Share]
  all_brand_2[,Display_Cost_Comp := Proportion*Display_Cost_Comp]
  return(all_brand_2)
}
