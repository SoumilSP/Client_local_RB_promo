# Prom Update Function

update_base = function(prom_update1, event1, row){

  # prom_update1[row,"Discount"] = event1$Discount    #place the Discount
  # prom_update1[row,"Display"] = event1$Display    #place the Display Type
  # prom_update1[row,"Display_Cost"] = event1$Display_Cost    #place Display Cost
  # prom_update1[row,"Display_Flag"] = event1$Display_Flag    #place the Display Flag
  # prom_update1[row,"Event_Multiplier_Tesco"] = event1$Event_Multiplier_Tesco    #place the event1 Multiplier
  # prom_update1[row,"ROI"] = event1$ROI    #place the ROI
  # 
  # prom_update1[row,"Extra_Slot_Flag"] = 1   #making sure the flag is 1
  # prom_update1[row,"Flag_Check_Counter"] = 1   #making sure to update the counter
  prom_update1[row,c("Discount","Display","Display_Cost","Display_Flag",
                     "Flyer","Flyer_Cost","Flyer_Flag",
                     "Event_Multiplier_Tesco","ROI")] = 
    event1[,c("Discount","Display","Display_Cost","Display_Flag",
              "Flyer","Flyer_Cost","Flyer_Flag",
              "Event_Multiplier_Tesco","ROI")]
  prom_update1[row,c("Extra_Slot_Flag","Flag_Check_Counter")] = 1
  
  prom_update1 = do_calculation(prom_update1)  #get the results for best event1 ----> at brand level

  return(prom_update1)
}