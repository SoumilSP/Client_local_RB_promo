recalculate = function(df){

  df[,Event_Lift := Event_Multiplier_Tesco * Base_Units]
  df[,Total_Sales := Base_Units + Event_Lift]
  df[,Promo_Price := round(RSP_Unit*(1-Discount),1)]
  df[,Retro_Funding_Unit := 0.335]
 # df[,Retro_Funding_Total := Retro_Funding_Unit*Total_Sales]
  df[,UNCR_Total := (RSP_Unit - Promo_Price)*Total_Sales]                                                        #addon
  df[,OID_Total := OID_Unit*Total_Sales]                                                          #addon
  df[,Gross_Sales := Promo_Price*Total_Sales] 
  df[,Retro_Funding_Total := Retro_Funding_Unit*Gross_Sales]
  df[,Total_Trade_Investment := Retro_Funding_Total + Display_Cost + UNCR_Total + OID_Total]
  
  #addon
  df[,Net_Revenue := Gross_Sales - Total_Trade_Investment]                                        #addon
  df[,NIS := Gross_Sales - UNCR_Total - OID_Total]                                                #addon
  
  df[,BIP := Total_Sales*Net_Cost_Unit]
  df[,COGS_Total := Total_Sales* COGS_Unit]
  df[,GM_Abs := Net_Revenue - COGS_Total ]                                                        #modified
  #df[,Gross_Sales := STP_Unit*Total_Sales]
  df[,Inc_GM_Abs := Event_Lift*GM_Abs/Total_Sales]
  df[,FM_Abs_Unit_2 := (Promo_Price/(1+VAT)) - Net_Cost_Unit + Retro_Funding_Unit]
  df[,`FM%_2` := FM_Abs_Unit_2*(1+VAT)/Promo_Price]
  df[,FM_Total := FM_Abs_Unit_2 * Total_Sales]
  df[,Retailer_Revenue := Total_Sales * Promo_Price]
  
  df[,Inc_NIS := Event_Lift*NIS/Total_Sales]                                                      #addon
  df[,Inc_Revenue := Event_Lift*Net_Revenue/Total_Sales]                                          #addon
  df[,Units_Sale_in_Case := Total_Sales/No_Of_Units]                                              #addon
  
  df[,R_UNCR_Inc := Event_Lift*(RSP_Unit - Promo_Price)]                                                    #addon2
  df[,R_OID_Inc := Event_Lift*OID_Unit]                                                      #addon2
  df[,R_Retro_Inc := Total_Sales*Retro_Funding_Unit]                                         #addon2
  df[,R_Display_Cost := Display_Cost]                                                        #addon2
  df[,R_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Cost]              #addon2
  df[,R_NIS_Inc := (STP_Unit - (RSP_Unit - Promo_Price) - OID_Unit)*Event_Lift]                             #addon2
  df[,R_Net_Rev_Inc := R_NIS_Inc - R_Retro_Inc - R_Display_Cost ]                            #addon2
  df[,R_GM_Inc := R_Net_Rev_Inc - (COGS_Unit*Event_Lift)]                                    #addon2
  
  df[,Value_Sales := ifelse(Promo_Price == 0, Total_Sales*RSP_Unit, Total_Sales*Promo_Price)]     #addon3
  
  return(df)
  
}


