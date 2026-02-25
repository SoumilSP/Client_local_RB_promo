do_calculation = function(tmp){
 
  
tmp[, Best_ROI_Flag := trimws(Best_ROI_Flag)]
  tmp[, Eff_Multiplier := fcase(
    Best_ROI_Flag == "Discount_Only",
    Event_Multiplier_Discount,
    Best_ROI_Flag == "Display_with_Discount",
    Event_Multiplier_Display + Event_Multiplier_Discount,
    Best_ROI_Flag == "Flyer_with_Discount",
    Event_Multiplier_Flyer + Event_Multiplier_Discount,
    Best_ROI_Flag %in% c("Display and flyer with discount", "Display_and_Flyer_with_Discount"),
    Event_Multiplier_Flyer_Display + Event_Multiplier_Discount,
    default = 0
  )]
 
  tmp[,Event_Multiplier_Tesco := Eff_Multiplier]
  tmp[,Event_Lift := Event_Multiplier_Tesco * `Base Sales`]
  tmp[,Total_Sales := `Base Sales` + Event_Lift]
  tmp[,Promo_Price := `RSP (unit)`*(1-Discount)]
  tmp[,Retro_Funding_Unit := 0.335]
  #tmp[,Retro_Funding_Total := Retro_Funding_Unit*Total_Sales]
  tmp[,Gross_Sales := Promo_Price*Total_Sales]
  tmp[,Retro_Funding_Total := Retro_Funding_Unit*Gross_Sales]
  tmp[,UNCR_Total := (`RSP (unit)` - Promo_Price)*Total_Sales]                                                                  #addon
  tmp[,OID_Total := OID_Unit*Total_Sales]                                                                    #addon
  tmp[,Total_Trade_Investment := (UNCR_Total + OID_Total +  Retro_Funding_Total + Display_Cost)]             #addon
  tmp[,Net_Revenue := Gross_Sales - Total_Trade_Investment]                                                  #addon
  tmp[,NIS := Gross_Sales - UNCR_Total - OID_Total]                                                          #addon
  tmp[,BIP := Total_Sales*`Net Cost (Unit)`]
  tmp[,COGS_Total := Total_Sales*`COGS (unit)`]
  tmp[,GM_Abs := Net_Revenue - COGS_Total]                                                                   #modified
  
  tmp[,Inc_GM_Abs := Event_Lift*GM_Abs/Total_Sales]
  tmp[,Inc_Revenue := Event_Lift*Net_Revenue/Total_Sales]                                                    #addon
  tmp[,Inc_NIS := Event_Lift*NIS/Total_Sales]                                                                #addon
  
  
  tmp[,R_UNCR_Inc := Event_Lift*(`RSP (unit)`- Promo_Price)]                                                    #addon2
  tmp[,R_OID_Inc := Event_Lift*OID_Unit]                                                      #addon2
  tmp[,R_Retro_Inc := Total_Sales*Retro_Funding_Unit]                                         #addon2
  tmp[,R_Display_Cost := Display_Cost]                                                        #addon2
  tmp[,R_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Cost]              #addon2
  tmp[,R_NIS_Inc := (`STP (Unit)` - (`RSP (unit)` - Promo_Price) - OID_Unit)*Event_Lift]                         #addon2
  tmp[,R_Net_Rev_Inc := R_NIS_Inc - R_Retro_Inc - R_Display_Cost ]                            #addon2
  tmp[,R_GM_Inc := R_Net_Rev_Inc - (`COGS (unit)`*Event_Lift)]                                #addon2
  
  tmp[,Value_Sales := Total_Sales*Promo_Price]                #addon_3
  
  #####DEBUG: Check do_calculation output for Inf/NA
  cat("\n========== CHECKING DO_CALCULATION OUTPUT ==========\n")
  cat("Total_Sales zeros:", sum(tmp$Total_Sales == 0, na.rm=TRUE), "\n")
  cat("Inc_GM_Abs Inf:", sum(is.infinite(tmp$Inc_GM_Abs), na.rm=TRUE), "\n")
  cat("Inc_Revenue Inf:", sum(is.infinite(tmp$Inc_Revenue), na.rm=TRUE), "\n")
  cat("Inc_NIS Inf:", sum(is.infinite(tmp$Inc_NIS), na.rm=TRUE), "\n")
  cat("==========================================\n\n")
  
  return(tmp)
  
}
