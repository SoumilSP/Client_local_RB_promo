#Check budget per PPG

# For Testing purpose

#base_file = prom_base; ppg_to_check = "AA6"; budget_const = budget_const
 #base_file_2 = not_prom

budget_check = function(base_file,base_file_2,ppg_to_check,budget_const){
  
  #Get Investment done for PPG - prom_base
  investment_per_ppg = base_file[,.(Total_Trade_Investment = sum(Total_Trade_Investment, na.rm = TRUE)),by = .(PPG)]
  #investment_per_ppg = prom_base[,.(Total_Trade_Investment = sum(Total_Trade_Investment)),by = .(PPG)]
  
  #investment_ppg = investment_per_ppg[PPG == "UJB"]
  investment_ppg = investment_per_ppg[PPG == ppg_to_check]
  
  #Get Investment done for PPG - not_prom
  investment_per_ppg_not_prom = base_file_2[,.(Total_Trade_Investment = sum(Total_Trade_Investment, na.rm = TRUE)),by = .(PPG)]
  #investment_per_ppg_not_prom = prom_base[,.(Total_Trade_Investment = sum(Total_Trade_Investment)),by = .(PPG)]
  
  investment_ppg_not_prom = investment_per_ppg_not_prom[PPG == ppg_to_check]
  #investment_ppg_not_prom = investment_per_ppg_not_prom[PPG == "UJB"]
  
  #Total Investment - handle empty data.tables and NA values
  investment_prom = 0
  if(nrow(investment_ppg) > 0){
    val = investment_ppg$Total_Trade_Investment[1]
    if(!is.na(val) && length(val) > 0){
      investment_prom = val
    }
  }
  
  investment_not_prom = 0
  if(nrow(investment_ppg_not_prom) > 0){
    val = investment_ppg_not_prom$Total_Trade_Investment[1]
    if(!is.na(val) && length(val) > 0){
      investment_not_prom = val
    }
  }
  
  total_investment = investment_prom + investment_not_prom
  
  #Get budget alloted for PPG
  budget_ppg = budget_const[PPG == ppg_to_check]
  #budget_ppg = budget_const[PPG == "UJB"]
  
  # Handle case where PPG is not found in budget_const
  if(nrow(budget_ppg) == 0){
    # Return default values if PPG not found in budget constraints
    return_list = list("Status" = "between", "Investment_ppg" = total_investment,
                       "Min_Inv_Req" = 0, "Max_Inv_Req" = Inf)
    return(return_list)
  }
  
  # Get budget values, handling NA and empty cases
  min_investment = 0
  max_investment = Inf
  
  if(nrow(budget_ppg) > 0){
    min_val = budget_ppg$Min_Investment[1]
    max_val = budget_ppg$Max_Investment[1]
    
    if(!is.na(min_val) && length(min_val) > 0){
      min_investment = min_val
    }
    if(!is.na(max_val) && length(max_val) > 0){
      max_investment = max_val
    }
  }
  
  #Check if investment is below, between or above PPG
  # All values should be valid at this point, but add safety check
  if(is.na(total_investment)){
    total_investment = 0
  }
  if(is.na(min_investment)){
    min_investment = 0
  }
  if(is.na(max_investment)){
    max_investment = Inf
  }
  
  # Now perform the comparison - all values should be valid
  if(total_investment < min_investment) {
    investment_status = "below"
  } else if(total_investment > max_investment) {
    investment_status = "above"
  } else {
    investment_status = "between"
  }
  
  return_list = list("Status" = investment_status,"Investment_ppg" = total_investment,
                     "Min_Inv_Req" = min_investment, "Max_Inv_Req" = max_investment)
  return(return_list)
}