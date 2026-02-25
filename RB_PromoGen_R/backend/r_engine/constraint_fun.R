constraint_fun = function(prom_Total_Sales, prom_GM_Abs, prom_BIP, prom_Gross_Sales, prom_Inc_GM,
                          prom_Total_Trade_Investment,prom_Net_Revenue,other_sales,prom_NIS,not_prom_Total_Sales,not_prom_GM_Abs,
                          not_prom_BIP,not_prom_Gross_Sales,not_prom_Net_Revenue,not_prom_NIS, not_prom_Total_Trade_Investment,
                          exc_Total_Sales,exc_GM_Abs,exc_BIP,exc_Gross_Sales,exc_Inc_GM,exc_Total_Trade_Investment,exc_Net_Revenue,exc_NIS,
                          con1,con2,con3,con4,con5,con6,con1_min,con2_min,con3_min,con4_min,con5_min,con6_min,con1_max,con2_max,con3_max,
                          con4_max,con5_max,con6_max,goal,roi,
                          prom_R_Trade_Inv_Inc,exc_R_Trade_Inv_Inc,not_prom_R_Trade_Inv_Inc,
                          prom_R_GM_Inc, exc_R_GM_Inc,prom_R_NIS_Inc, exc_R_NIS_Inc,
                          prom_R_Net_Rev_Inc,exc_R_Net_Rev_Inc,
                          exc_Value_Sales, not_prom_Value_Sales, prom_Value_Sales, other_sales_value,
                          scope = "PROMO_ONLY"){
  
  # Scope determines whether to use PROMO_ONLY or TOTAL (prom + not_prom + exc)
  if(scope == "PROMO_ONLY") {
    # Use only promoted values for constraints
    tot_sales = prom_Total_Sales
    tot_GM_Abs = prom_GM_Abs
    tot_Revenue = prom_Net_Revenue
    tot_Gross_Sales = prom_Gross_Sales
    tot_Trade_Investment = prom_Total_Trade_Investment
    R_tot_Trade_Investment_Inc = prom_R_Trade_Inv_Inc
    tot_NIS = prom_NIS
    tot_Value_Sales = prom_Value_Sales
  } else {
    # Use TOTAL (original behavior)
    tot_sales = prom_Total_Sales + not_prom_Total_Sales + exc_Total_Sales
    tot_GM_Abs = prom_GM_Abs + not_prom_GM_Abs + exc_GM_Abs
    tot_Revenue = prom_Net_Revenue + not_prom_Net_Revenue + exc_Net_Revenue
    tot_Gross_Sales = prom_Gross_Sales + not_prom_Gross_Sales + exc_Gross_Sales
    tot_Trade_Investment = prom_Total_Trade_Investment + exc_Total_Trade_Investment + not_prom_Total_Trade_Investment
    R_tot_Trade_Investment_Inc = prom_R_Trade_Inv_Inc + exc_R_Trade_Inv_Inc + not_prom_R_Trade_Inv_Inc
    tot_NIS = prom_NIS + not_prom_NIS + exc_NIS
    tot_Value_Sales = prom_Value_Sales + not_prom_Value_Sales + exc_Value_Sales
  }
  
  # Calculate KPIs (handle division by zero)
  GM_percent_model = ifelse(tot_Revenue != 0, tot_GM_Abs*100/tot_Revenue, 0)
  Volume_sales_model = tot_sales
  Gross_sales_model = tot_Gross_Sales
  Trade_as_per_NR_model = ifelse(tot_Revenue != 0, (tot_Trade_Investment)*100/tot_Revenue, 0)
  Net_Sales_model = tot_Revenue
  Gross_margin_model = tot_GM_Abs
  Trade_as_per_NIS_model = ifelse(tot_NIS != 0, (tot_Trade_Investment)*100/tot_NIS, 0)
  Market_Share_model = ifelse((tot_Value_Sales + other_sales_value) != 0, tot_Value_Sales*100/(tot_Value_Sales + other_sales_value), 0)
  
  ROI_model = if(roi == "ROI_GM"){ 
    ifelse(tot_Trade_Investment != 0, (prom_Inc_GM + exc_Inc_GM)/(tot_Trade_Investment), 0)
  } else if(roi == "R_ROI_GM"){ 
    ifelse(R_tot_Trade_Investment_Inc != 0, (prom_R_GM_Inc + exc_R_GM_Inc)/(R_tot_Trade_Investment_Inc), 0)
  } else if(roi == "R_ROI_Rev"){ 
    ifelse(R_tot_Trade_Investment_Inc != 0, (prom_R_Net_Rev_Inc + exc_R_Net_Rev_Inc)/(R_tot_Trade_Investment_Inc), 0)
  } else if(roi == "R_ROI_NIS"){ 
    ifelse(R_tot_Trade_Investment_Inc != 0, (prom_R_NIS_Inc + exc_R_NIS_Inc)/(R_tot_Trade_Investment_Inc), 0)
  } else { 0 }
  
  #For yogitha - fron end
  kpi <- data.frame("Scan Net Revenue" = Net_Sales_model,"GM % NR" = GM_percent_model,"TI % NR" = Trade_as_per_NR_model, "TI % NIS" = Trade_as_per_NIS_model,
                    "Trade ROI" = ROI_model, "Scan Gross Sales" = Gross_sales_model,"Gross Margin" = Gross_margin_model, "Volume Sales" = Volume_sales_model, "Value Market Share" = Market_Share_model, check.names = FALSE)
  
  
  #Check all the constraints here and return the flag
  con1_flag = ifelse(get(con1) >= con1_min  & get(con1) <= con1_max,1,0)
  con2_flag = ifelse(get(con2) >= con2_min  & get(con2) <= con2_max,1,0)
  con3_flag = ifelse(get(con3) >= con3_min  & get(con3) <= con3_max,1,0)
  con4_flag = ifelse(get(con4) >= con4_min  & get(con4) <= con4_max,1,0)
  con5_flag = ifelse(get(con5) >= con5_min  & get(con5) <= con5_max,1,0)
  con6_flag = ifelse(get(con6) >= con6_min  & get(con6) <= con6_max,1,0)
  
  #Check if the constraints are below or above
  con1_status = ifelse(get(con1) < con1_min ,-2,ifelse(get(con1) > con1_max ,2,1))
  con2_status = ifelse(get(con2) < con2_min ,-2,ifelse(get(con2) > con2_max ,2,1))
  con3_status = ifelse(get(con3) < con3_min ,-2,ifelse(get(con3) > con3_max ,2,1))
  con4_status = ifelse(get(con4) < con4_min ,-2,ifelse(get(con4) > con4_max ,2,1))
  con5_status = ifelse(get(con5) < con5_min ,-2,ifelse(get(con5) > con5_max ,2,1))
  con6_status = ifelse(get(con6) < con6_min ,-2,ifelse(get(con6) > con6_max ,2,1))
  
  
  #Get Difference from minimum/maximum value
  con1_diff = ifelse(con1_status == -2, con1_min - get(con1), ifelse( con1_status == 2, get(con1) - con1_max,0))
  con2_diff = ifelse(con2_status == -2, con2_min - get(con2), ifelse( con2_status == 2, get(con2) - con2_max,0))
  con3_diff = ifelse(con3_status == -2, con3_min - get(con3), ifelse( con3_status == 2, get(con3) - con3_max,0))
  con4_diff = ifelse(con4_status == -2, con4_min - get(con4), ifelse( con4_status == 2, get(con4) - con4_max,0))
  con5_diff = ifelse(con5_status == -2, con5_min - get(con5), ifelse( con5_status == 2, get(con5) - con5_max,0))
  con6_diff = ifelse(con6_status == -2, con6_min - get(con6), ifelse( con6_status == 2, get(con6) - con6_max,0))
  
  #Get Optimization Goal Value
  goal_val = get(goal)
  
  flag_list = list(con1_flag,con2_flag,con3_flag,con4_flag,con5_flag,con6_flag)   #constraint satisfied or not
  flag_value = list(get(con1), get(con2), get(con3), get(con4), get(con5), get(con6))   #abs value of the constraints
  con_dir = list(con1_status,con2_status,con3_status,con4_status,con5_status,con6_status)   #which side they are located
  con_diff = list(con1_diff, con2_diff, con3_diff, con4_diff, con5_diff, con6_diff)    #difference from min/max
  opt_val = goal_val   #goal value
  
    
  return(list(flag_list,flag_value,con_dir,con_diff,opt_val,kpi))
}