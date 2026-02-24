constraint_fun = function(prom_Total_Sales, prom_GM_Abs, prom_BIP, prom_Gross_Sales, prom_Inc_GM,
                          prom_Total_Trade_Investment,prom_Net_Revenue,other_sales,prom_NIS,not_prom_Total_Sales,not_prom_GM_Abs,
                          not_prom_BIP,not_prom_Gross_Sales,not_prom_Net_Revenue,not_prom_NIS, not_prom_Total_Trade_Investment,
                          exc_Total_Sales,exc_GM_Abs,exc_BIP,exc_Gross_Sales,exc_Inc_GM,exc_Total_Trade_Investment,exc_Net_Revenue,exc_NIS,
                          con1,con2,con3,con4,con5,con6,con1_min,con2_min,con3_min,con4_min,con5_min,con6_min,con1_max,con2_max,con3_max,
                          con4_max,con5_max,con6_max,goal,roi,
                          prom_R_Trade_Inv_Inc,exc_R_Trade_Inv_Inc,not_prom_R_Trade_Inv_Inc,
                          prom_R_GM_Inc, exc_R_GM_Inc,prom_R_NIS_Inc, exc_R_NIS_Inc,
                          prom_R_Net_Rev_Inc,exc_R_Net_Rev_Inc,
                          exc_Value_Sales, not_prom_Value_Sales, prom_Value_Sales, other_sales_value){
  
  tot_sales = prom_Total_Sales + not_prom_Total_Sales + exc_Total_Sales
  tot_GM_Abs = prom_GM_Abs + not_prom_GM_Abs + exc_GM_Abs
  #tot_BIP = prom_BIP + not_prom_BIP + exc_BIP
  tot_Revenue = prom_Net_Revenue + not_prom_Net_Revenue + exc_Net_Revenue                                           #addon
  tot_Gross_Sales = prom_Gross_Sales + not_prom_Gross_Sales + exc_Gross_Sales
  tot_Trade_Investment =  prom_Total_Trade_Investment+exc_Total_Trade_Investment + not_prom_Total_Trade_Investment  #addon
  R_tot_Trade_Investment_Inc = prom_R_Trade_Inv_Inc + exc_R_Trade_Inv_Inc + not_prom_R_Trade_Inv_Inc                    #addon_2
  tot_NIS = prom_NIS + not_prom_NIS + exc_NIS
  tot_Value_Sales = prom_Value_Sales + not_prom_Value_Sales + exc_Value_Sales                                    #addon_3
  
  # DEFENSIVE CHECK: Handle division by zero and ensure scalar values
  safe_divide <- function(num, denom, default = 0) {
    if (length(num) == 0 || length(denom) == 0) return(default)
    if (any(is.na(denom)) || any(denom == 0) || any(is.infinite(denom))) return(default)
    result <- num / denom
    if (any(is.na(result)) || any(is.infinite(result))) return(default)
    return(result)
  }
  
  ensure_scalar <- function(x, default = 0) {
    if (length(x) == 0 || all(is.na(x))) return(default)
    val <- x[1]
    if (is.na(val) || is.infinite(val)) return(default)
    return(val)
  }
  
  tot_sales <- ensure_scalar(tot_sales)
  tot_GM_Abs <- ensure_scalar(tot_GM_Abs)
  tot_Revenue <- ensure_scalar(tot_Revenue)
  tot_Gross_Sales <- ensure_scalar(tot_Gross_Sales)
  tot_Trade_Investment <- ensure_scalar(tot_Trade_Investment)
  R_tot_Trade_Investment_Inc <- ensure_scalar(R_tot_Trade_Investment_Inc)
  tot_NIS <- ensure_scalar(tot_NIS)
  tot_Value_Sales <- ensure_scalar(tot_Value_Sales)
  
  GM_percent_model = safe_divide(tot_GM_Abs * 100, tot_Revenue)
  Volume_sales_model = tot_sales
  Gross_sales_model = tot_Gross_Sales
  Trade_as_per_NR_model = safe_divide(tot_Trade_Investment * 100, tot_Revenue)
  #Market_Share_model = tot_sales*100/(tot_sales + other_sales)
  #Net_Sales_model = tot_BIP
  Net_Sales_model = tot_Revenue                                                                                     #addon - confirm abhinav?
  Gross_margin_model = tot_GM_Abs
  Trade_as_per_NIS_model = safe_divide(tot_Trade_Investment * 100, tot_NIS)
  Market_Share_value_denom <- ensure_scalar(tot_Value_Sales + other_sales_value)
  Market_Share_model = safe_divide(tot_Value_Sales * 100, Market_Share_value_denom)
  
  
  ROI_model = if(roi == "ROI_GM"){ safe_divide(prom_Inc_GM + exc_Inc_GM, tot_Trade_Investment)
  } else if(roi == "R_ROI_GM"){ safe_divide(prom_R_GM_Inc + exc_R_GM_Inc, R_tot_Trade_Investment_Inc)
  } else if(roi == "R_ROI_Rev"){ safe_divide(prom_R_Net_Rev_Inc + exc_R_Net_Rev_Inc, R_tot_Trade_Investment_Inc)
  } else if(roi == "R_ROI_NIS"){ safe_divide(prom_R_NIS_Inc + exc_R_NIS_Inc, R_tot_Trade_Investment_Inc)
  } else { 0 }
  
  #For yogitha - fron end
  kpi <- data.frame("Scan Net Revenue" = Net_Sales_model,"GM % NR" = GM_percent_model,"TI % NR" = Trade_as_per_NR_model, "TI % NIS" = Trade_as_per_NIS_model,
                    "Trade ROI" = ROI_model, "Scan Gross Sales" = Gross_sales_model,"Gross Margin" = Gross_margin_model, "Volume Sales" = Volume_sales_model, "Value Market Share" = Market_Share_model, check.names = FALSE)
  

  # Create KPI name aliases for get() lookup in constraint checking
  # Maps frontend KPI names to calculated model variables
  `GM%NR` <- GM_percent_model
  `TS` <- tot_Trade_Investment
  `NR` <- Net_Sales_model
  `GS` <- Gross_sales_model
  `IMS` <- Market_Share_model
  `ROI` <- ROI_model
  `Volume_sales_model` <- Volume_sales_model
  `Gross_sales_model` <- Gross_sales_model
  `Net_Sales_model` <- Net_Sales_model
  `GM_percent_model` <- GM_percent_model
  `Trade_as_per_NR_model` <- Trade_as_per_NR_model
  `Trade_as_per_NIS_model` <- Trade_as_per_NIS_model
  `ROI_model` <- ROI_model
  `Gross_margin_model` <- Gross_margin_model
  `Market_Share_model` <- Market_Share_model
  
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
  
  # Build constraint names list for warning messages
  con_names = c(con1, con2, con3, con4, con5, con6)
  con_mins = c(con1_min, con2_min, con3_min, con4_min, con5_min, con6_min)
  con_maxs = c(con1_max, con2_max, con3_max, con4_max, con5_max, con6_max)
  con_flags = c(con1_flag, con2_flag, con3_flag, con4_flag, con5_flag, con6_flag)
  con_statuses = c(con1_status, con2_status, con3_status, con4_status, con5_status, con6_status)
  con_values = c(get(con1), get(con2), get(con3), get(con4), get(con5), get(con6))
  
  # Build list of violated constraints for warning message
  violated_constraints = list()
  for (i in 1:6) {
    if (con_flags[i] == 0) {  # Constraint violated
      # Determine if below min or above max
      violation_type = ifelse(con_statuses[i] == -2, "below_min", "above_max")
      
      # Format the actual value
      actual_val = con_values[i]
      formatted_val = ifelse(abs(actual_val) >= 1e6, 
                              sprintf("%.2fM", actual_val / 1e6),
                              ifelse(abs(actual_val) >= 1e3,
                                     sprintf("%.2fK", actual_val / 1e3),
                                     sprintf("%.2f", actual_val)))
      
      violated_constraints[[length(violated_constraints) + 1]] = list(
        name = con_names[i],
        actual_value = actual_val,
        actual_value_formatted = formatted_val,
        min_value = con_mins[i],
        max_value = con_maxs[i],
        violation_type = violation_type
      )
    }
  }
  
  # Check if any constraint was violated
  all_constraints_satisfied = (sum(con_flags) == 6)

    
  return(list(flag_list, flag_value, con_dir, con_diff, opt_val, kpi, 
              violated_constraints = violated_constraints,
              all_constraints_satisfied = all_constraints_satisfied))
}