source("do_calculation.R")
source("constraint_fun.R")
source("ppg_budget_check_fun.R")
source("prom_update_fun.R")
source("sequence_generator_function.R")

# Load gtools for smartbind() used at line ~1986
suppressPackageStartupMessages(library(gtools))

#####DEBUG: Helper function to save debug data
save_debug_data <- function(iteration, data, filename_prefix = "debug"){
  filename <- paste0(filename_prefix, "_iter_", iteration, ".RData")
  tryCatch({
    save(data, file = filename)
    cat("Saved debug data to", filename, "\n")
  }, error = function(e){
    cat("Could not save debug data:", e$message, "\n")
  })
}

#####DEBUG: Set this to TRUE to enable   at key points
DEBUG_MODE <- TRUE  # Set to TRUE to enable interactive debugging


best_seq <- function(shiny_data,shiny_slot,competition_slot,include_competition,tesco_slot,start_date,end_date){
  
  #0.1 Filter data between the time range - USING DYNAMIC SLOT LOGIC
  # REMOVED week end date logic - now using slot-based dates directly
  
  cat("\n[BEST_SEQ] Starting best_seq() function\n")
  cat("[BEST_SEQ] Input tesco_slot rows:", nrow(tesco_slot), "\n")
  cat("[BEST_SEQ] Input start_date:", as.character(start_date), "class:", class(start_date), "\n")
  cat("[BEST_SEQ] Input end_date:", as.character(end_date), "class:", class(end_date), "\n")
  
  # CRITICAL: Save original PPG values BEFORE any processing
  # This ensures we can restore them even if cbind/merge operations corrupt them
  original_ppg_values <- as.character(unique(shiny_data$PPG))
  cat("[BEST_SEQ] Original PPG values saved:", paste(original_ppg_values, collapse=", "), "\n")
  
  # Ensure tesco_slot dates are properly formatted as Date objects
  if (!inherits(tesco_slot$`Start Date`, "Date")) {
    # Try to parse dates - handle multiple formats
    tryCatch({
      tesco_slot$`Start Date` = as.Date(tesco_slot$`Start Date`)
    }, error = function(e) {
      tryCatch({
        tesco_slot$`Start Date` = dmy(tesco_slot$`Start Date`)
      }, error = function(e2) {
        tesco_slot$`Start Date` = ymd(tesco_slot$`Start Date`)
      })
    })
  }
  if (!inherits(tesco_slot$`End Date`, "Date")) {
    tryCatch({
      tesco_slot$`End Date` = as.Date(tesco_slot$`End Date`)
    }, error = function(e) {
      tryCatch({
        tesco_slot$`End Date` = dmy(tesco_slot$`End Date`)
      }, error = function(e2) {
        tesco_slot$`End Date` = ymd(tesco_slot$`End Date`)
      })
    })
  }
  
  # Ensure start_date and end_date are Date objects
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  
  cat("[BEST_SEQ] tesco_slot Start Date range:", as.character(min(tesco_slot$`Start Date`, na.rm=TRUE)), 
      "to", as.character(max(tesco_slot$`Start Date`, na.rm=TRUE)), "\n")
  cat("[BEST_SEQ] tesco_slot End Date range:", as.character(min(tesco_slot$`End Date`, na.rm=TRUE)), 
      "to", as.character(max(tesco_slot$`End Date`, na.rm=TRUE)), "\n")
  
  # Find slot_start and slot_end - use >= and <= for date matching since exact match may fail
  slot_start = tesco_slot$Slot[tesco_slot$`Start Date` >= start_date][1]
  slot_end = tesco_slot$Slot[tesco_slot$`End Date` <= end_date]
  slot_end = slot_end[length(slot_end)]  # Get last matching slot
  
  # Fallback: if no match found, use first and last slots
  if (length(slot_start) == 0 || is.na(slot_start)) {
    cat("[BEST_SEQ] WARNING: No slot_start found, using first slot\n")
    slot_start = min(tesco_slot$Slot, na.rm = TRUE)
  }
  if (length(slot_end) == 0 || is.na(slot_end)) {
    cat("[BEST_SEQ] WARNING: No slot_end found, using last slot\n")
    slot_end = max(tesco_slot$Slot, na.rm = TRUE)
  }
  
  cat("[BEST_SEQ] slot_start:", slot_start, ", slot_end:", slot_end, "\n")
  
  all_slot = seq(slot_start, slot_end, 1)
  cat("[BEST_SEQ] all_slot count:", length(all_slot), "\n")
  
  
  # Filter data based on slot numbers (not week end dates)
  cat("[BEST_SEQ] shiny_data rows BEFORE filter:", nrow(shiny_data), "\n")
  cat("[BEST_SEQ] shiny_data Tesco_Week_No values:", paste(head(unique(shiny_data$Tesco_Week_No), 10), collapse=", "), "\n")
  cat("[BEST_SEQ] all_slot values:", paste(head(all_slot, 10), collapse=", "), "\n")
  
  shiny_data = shiny_data[Tesco_Week_No %in% all_slot]
  competition_slot = competition_slot[`Slot No` %in% all_slot]
  
  cat("[BEST_SEQ] shiny_data rows AFTER filter:", nrow(shiny_data), "\n")
  if (nrow(shiny_data) == 0) {
    cat("[BEST_SEQ] WARNING: All rows filtered out! Check Tesco_Week_No vs all_slot mismatch\n")
  } else {
    cat("[BEST_SEQ] PPG values after filter:", paste(unique(shiny_data$PPG), collapse=", "), "\n")
  }
  
  # Create slot metadata with start/end dates and duration for dynamic slot handling
  slot_metadata = tesco_slot[Slot %in% all_slot, .(Slot, `Start Date`, `End Date`)]
  slot_metadata[, Slot_Duration := as.numeric(`End Date` - `Start Date` + 1)]  # +1 to include both start and end dates
  
  # Merge slot metadata with shiny_data to have dates for each slot
  shiny_data = merge(shiny_data, slot_metadata, by.x = "Tesco_Week_No", by.y = "Slot", all.x = TRUE)
  
  
  
  #0.2 Create Promo Seq File
  #slots = length(all_slot)e
  slots = length(all_slot[all_slot != 0]) #quick fix 12th April 2019
  
  # CRITICAL: Limit slots to prevent memory explosion
  # sequence_generator creates 2^slots combinations, so:
  # 20 slots = 1 million combinations (OK)
  # 25 slots = 33 million combinations (borderline)
  # 30 slots = 1 billion combinations (will crash)
  # 36 slots = 68 billion combinations (impossible)
  MAX_SLOTS_FOR_SEQ_GEN <- 20
  if (slots > MAX_SLOTS_FOR_SEQ_GEN) {
    cat("[BEST_SEQ] WARNING: Too many slots (", slots, ") for sequence_generator.\n")
    cat("[BEST_SEQ] Limiting to", MAX_SLOTS_FOR_SEQ_GEN, "slots to prevent memory issues.\n")
    cat("[BEST_SEQ] 2^", slots, "=", format(2^slots, big.mark=",", scientific=FALSE), "combinations would require too much memory.\n")
    slots <- MAX_SLOTS_FOR_SEQ_GEN
  }
  cat("[BEST_SEQ] Using", slots, "slots for sequence generation (2^", slots, "=", format(2^slots, big.mark=","), "combinations)\n")
  
  promo_seq = sequence_generator(slots)
  
  #1 Sort competition file
  competition_slot$Date = mdy(competition_slot$Date)
  competition_slot = competition_slot[order(PPG,Date)]
  competition_slot =  competition_slot[`Slot No` != 0]
  
  #2 Promote our PPG 1 week before the competition
  # Ensure Promo_RB is a strict 0/1 flag (can be used safely in sequence filtering)
  competition_slot[,Promo_RB := as.integer(shift(Comp_Promo_Flag, type = "lead") > 0)]
  competition_slot$Promo_RB[is.na(competition_slot$Promo_RB)] = 0L
  
  #3 Define the function for finding the best sequence
  # SLOT-BASED: Work directly with slots, not weeks
  # If input columns are in weeks, convert to slots once at the beginning
  # Otherwise, assume they're already in slots
  avg_slot_duration = ifelse(exists("slot_metadata") && nrow(slot_metadata) > 0, 
                             as.integer(mean(slot_metadata$Slot_Duration, na.rm = TRUE), 3))  # Default to 3 if not available
  
  # Convert to slots if columns exist in weeks format, otherwise use as-is
  # NOTE: slot counts must be integers (0 decimals). Use ceiling() for mins and floor() for maxes.
  if("Min_Display_Weeks" %in% names(shiny_slot)){
    shiny_slot[, Min_Display_Slots := as.integer(ceiling(Min_Display_Weeks * 7 / avg_slot_duration))]
    shiny_slot[, Max_Display_Slots := as.integer(floor(  Max_Display_Weeks * 7 / avg_slot_duration))]
    shiny_slot[, Min_Total_Slots   := as.integer(ceiling(Min_Total_Weeks   * 7 / avg_slot_duration))]
    shiny_slot[, Max_Total_Slots   := as.integer(floor(  Max_Total_Weeks   * 7 / avg_slot_duration))]
  } else if("Min_Display_Slots" %in% names(shiny_slot)){
    # Already in slots, normalize to integer in case upstream produced decimals
    shiny_slot[, Min_Display_Slots := as.integer(ceiling(Min_Display_Slots))]
    shiny_slot[, Max_Display_Slots := as.integer(floor(  Max_Display_Slots))]
    shiny_slot[, Min_Total_Slots   := as.integer(ceiling(Min_Total_Slots))]
    shiny_slot[, Max_Total_Slots   := as.integer(floor(  Max_Total_Slots))]
  } else {
    # Create slot columns from whatever is available
    shiny_slot[, Min_Display_Slots := as.integer(ceiling(ifelse("Min_Display_Weeks" %in% names(shiny_slot), Min_Display_Weeks * 7 / avg_slot_duration, 0)))]
    shiny_slot[, Max_Display_Slots := as.integer(floor(  ifelse("Max_Display_Weeks" %in% names(shiny_slot), Max_Display_Weeks * 7 / avg_slot_duration, 0)))]
    shiny_slot[, Min_Total_Slots   := as.integer(ceiling(ifelse("Min_Total_Weeks" %in% names(shiny_slot),   Min_Total_Weeks   * 7 / avg_slot_duration, 0)))]
    shiny_slot[, Max_Total_Slots   := as.integer(floor(  ifelse("Max_Total_Weeks" %in% names(shiny_slot),   Max_Total_Weeks   * 7 / avg_slot_duration, 0)))]
  }
  
  #base_sale_roll = shiny_data
  #slot = shiny_slot
  #promo_seq = promo_seq
  #ppg = "AA6"
  
  
  optimizer_data = function(base_sale_roll,slot,promo_seq,competition_slot, include_competition,slots){
    
    #10.1 Use lapply instead of loop for better performance
    base_sale_roll=shiny_data  # Set once outside loop
    
    df_list = lapply(unique(base_sale_roll$PPG), function(ppg){
      
      #1 Filter ppg data and no of slots to fill
      one_ppg = base_sale_roll[PPG == ppg & Tesco_Week_No != 0]
      one_ppg_zero = base_sale_roll[PPG == ppg & Tesco_Week_No == 0]
      one_ppg[, Rank:= rank(Base_Units,ties.method = c("first"))]
      # SLOT-BASED: Use Max_Total_Slots if available, otherwise convert from weeks
      
      if("Max_Total_Slots" %in% names(slot)){
        one_ppg_slots = as.integer(round(slot$Max_Total_Slots[slot$PPG == ppg], 0))
      } else if("Max_Total_Weeks" %in% names(slot)){
        one_ppg_slots = as.integer(round(slot$Max_Total_Weeks[slot$PPG == ppg] * 7 / avg_slot_duration, 0))
      } else {
        one_ppg_slots = 0L
      }
      
      
      #1 Filter ppg data and no of slots to fill
      
      #2 Extract promo sequence
      
      max_allowed <- floor((slots + 1) / 2)   # 14 slots → max 7 promos
      one_ppg_slots <- min(one_ppg_slots, max_allowed)
      #2 Extract promo sequence
      #2 Extract promo sequence
      promo_slots = grepl(pattern = paste0("_", one_ppg_slots, "$"), x = names(promo_seq))
      promo_slots = data.table(promo_seq[, promo_slots])
      promo_slots_copy = copy(promo_slots)           #for error handling
      
      
      #2.1 If competition pattern exist for PPG, extract only those sequence
      comp_pattern = competition_slot[PPG == ppg]
      
      #### Only for PPG where competition info is present
      if( nrow(comp_pattern) != 0 & include_competition == 1){     #if loop start
        
        #Multiply comp_pattern to all_patterns - VECTORIZED
        promo_slots_matrix = as.matrix(promo_slots)
        promo_slots_matrix = promo_slots_matrix * comp_pattern$Promo_RB[1]  # Vectorized multiplication
        promo_slots = data.table(promo_slots_matrix)
        setnames(promo_slots, names(promo_seq)[grepl(pattern = paste0("_",one_ppg_slots), x = names(promo_seq))])
        
        #Extract those columns which satisfy the comp_pattern
        num_of_promo = colSums(promo_slots)
        num_of_promo = num_of_promo[num_of_promo == sum(comp_pattern$Promo_RB)]
        
        #Update promo_slots
        if(length(num_of_promo) > 0){
          promo_slots = promo_seq[,names(num_of_promo), with = FALSE]
        } else {
          promo_slots = promo_slots_copy
        }
        
      }  #if loop ends
      
      #3 Combine promo ppg data and promo sequence - ERROR HANDLING DONE HERE
      
      if(ncol(promo_slots) == 0){
        one_ppg = as.data.frame(cbind(one_ppg, promo_slots_copy))
      } else {
        one_ppg = as.data.frame(cbind(one_ppg, promo_slots))
      }
      
      
      #4 Find best sequence - VECTORIZED for performance
      n_col = ncol(one_ppg)
      
      #Initialize sequence
      seq_sum = vector(mode = "numeric", length = n_col)
      seq_sum[1:22] = -10^25     #keeping very high negative no
      
      #   
      #bilal code debug best sequence
      # Vectorized calculation using matrix multiplication (much faster than loop)
      if (n_col >= 22) {
        # Convert Rank to matrix and multiply with sequence columns
        rank_matrix = matrix(one_ppg$Rank, nrow = nrow(one_ppg), ncol = n_col - 22)
        seq_matrix = as.matrix(one_ppg[, 23:n_col, drop = FALSE])
        seq_sum[23:n_col] = colSums(rank_matrix * seq_matrix, na.rm = TRUE)
      }
      
      if(ppg %in% slot$PPG){
        #best sequence
        Best_Seq = which.max(seq_sum)
        Best_Seq = one_ppg[,Best_Seq]
      } else { 
        Best_Seq = rep(0L,slots)
      }
      
      
      # Force strict binary 0/1 output (protects against any upstream scaling/coercion)
      
      #Best_Seq = as.integer(as.numeric(Best_Seq) > 0)
      
      # CRITICAL FIX: Save PPG before cbind which can corrupt it
      saved_ppg <- as.character(one_ppg$PPG)
      
      #5 Merge PPG data with best seq
      ppg_best_seq = cbind(one_ppg[,1:21],Best_Seq)
      
      # Restore PPG after cbind if it got corrupted
      if (is.numeric(ppg_best_seq[[1]]) || any(is.na(ppg_best_seq[[1]]))) {
        ppg_best_seq[[1]] <- saved_ppg
      }
      
      #6 Add slot 0 in ppg_best_seq
      zero_slot = one_ppg_zero[,Best_Seq := 0L]
      ppg_best_seq = rbind(zero_slot, ppg_best_seq)
      
      return(ppg_best_seq)
    })
    
    # Combine all results at once (faster than rbind in loop)
    df = do.call(rbind, df_list)
    return(df) 
  }
  
  
  #5 Assuming competition sequence for a PPG- sanity check - available promo slots should be greater then competition slots
  
  compete_slot_req = competition_slot[,.(Min_Slots_Req = sum(Promo_RB)), by = PPG]
  # SLOT-BASED: Use Max_Display_Slots if available
  if("Max_Display_Slots" %in% names(shiny_slot)){
    slots_avail_from_ui = shiny_slot[,.(Slots_Avail = as.integer(round(sum(Max_Display_Slots), 0))), by = PPG]
  } else if("Max_Display_Weeks" %in% names(shiny_slot)){
    # Availability is a "max", so floor() is safest
    slots_avail_from_ui = shiny_slot[,.(Slots_Avail = as.integer(floor(sum(Max_Display_Weeks * 7 / avg_slot_duration)))), by = PPG]
  } else {
    slots_avail_from_ui = shiny_slot[,.(Slots_Avail = 0L), by = PPG]
  }
  
  compete_slot_req = merge(compete_slot_req,slots_avail_from_ui, by = "PPG", all.x = T)
  compete_slot_req =  compete_slot_req[PPG != "All Others"]   #remove all others
  
  
  #Find if criteria met
  compete_slot_req[,Criteria_Met := Slots_Avail > Min_Slots_Req]
  #If criteria not met, no need to consider competetion promo sequence - may change in future .....
  compete_slot_req = compete_slot_req[Criteria_Met == FALSE]
  ppg_criteria_not_met = unique(compete_slot_req$PPG)
  
  #Remove PPG Competition sequence
  competition_slot = competition_slot[!(PPG %in% ppg_criteria_not_met)]
  
  
  opt_ip = optimizer_data(base_sale_roll = shiny_data, slot = shiny_slot, promo_seq = promo_seq,
                          competition_slot = competition_slot, include_competition = include_competition, slots = slots)
  
  
  
  names(opt_ip) = c("PPG","SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT" ,"PPG_Description","Tesco_Week_No","Base Sales",
                    "RSP (unit)","Net Cost (Unit)", "FM%","COGS (unit)","BIP (Case)","OID","No_Of_Units","STP (Unit)",
                    "OID_Unit","UNCR_Unit","Start Date","End Date","Duration","Seq")
  
  # DEBUG: Trace PPG values at each stage
  cat("\n========== PPG TRACING IN BEST_SEQ ==========\n")
  cat("[TRACE] opt_ip$PPG (col 1) after rename:\n")
  cat("        - class:", class(opt_ip$PPG), "\n")
  cat("        - unique values:", paste(head(unique(opt_ip$PPG), 10), collapse=", "), "\n")
  cat("[TRACE] shiny_data$PPG (original input):\n")
  cat("        - class:", class(shiny_data$PPG), "\n")
  cat("        - unique values:", paste(unique(shiny_data$PPG), collapse=", "), "\n")
  cat("============================================\n")
  
  # CRITICAL FIX: If PPG became numeric (row numbers), restore from saved original values
  if (is.numeric(opt_ip$PPG) || all(opt_ip$PPG %in% 1:1000) || any(is.na(opt_ip$PPG))) {
    cat("[BEST_SEQ-FIX] PPG values are corrupt (numeric/NA), restoring from saved original values\n")
    # Use the saved original_ppg_values from the beginning of this function
    if (exists("original_ppg_values") && length(original_ppg_values) > 0 && !is.na(original_ppg_values[1])) {
      opt_ip[, PPG := as.character(original_ppg_values[1])]
      cat("[BEST_SEQ-FIX] Restored opt_ip$PPG to:", paste(unique(opt_ip$PPG), collapse=", "), "\n")
    } else {
      cat("[BEST_SEQ-FIX] WARNING: No original PPG values available to restore!\n")
    }
  }
  
  # Final safety: ensure the output promo decision is strict binary 0/1
  
  opt_ip[,VAT := 0.2]
  
  #6 Save File
  #write.csv(opt_ip,"9 Optimizer Input.csv", row.names = F)
  
  return(opt_ip)
}

best_seq_cannib <- function(shiny_data,slot,canibalize,tesco_slot,start_date,end_date){
  #0.1 Filter data between the time range - USING DYNAMIC SLOT LOGIC
  # Ensure tesco_slot dates are properly formatted
  tesco_slot$`Start Date` = dmy(tesco_slot$`Start Date`)
  tesco_slot$`End Date` = dmy(tesco_slot$`End Date`)
  
  slot_start = tesco_slot$Slot[tesco_slot$`Start Date` == start_date]
  slot_end = tesco_slot$Slot[tesco_slot$`End Date` == end_date]
  all_slot = seq(slot_start,slot_end,1)
  shiny_data = shiny_data[Tesco_Week_No %in% all_slot]
  
  # Create slot metadata with start/end dates and duration
  slot_metadata = tesco_slot[Slot %in% all_slot, .(Slot, `Start Date`, `End Date`)]
  slot_metadata[, Slot_Duration := as.numeric(`End Date` - `Start Date` + 1)]
  shiny_data = merge(shiny_data, slot_metadata, by.x = "Tesco_Week_No", by.y = "Slot", all.x = TRUE)
  
  #0.2 Create Promo Seq File
  slots = length(all_slot)
  
  # CRITICAL: Limit slots to prevent memory explosion (same as best_seq)
  MAX_SLOTS_FOR_SEQ_GEN <- 20
  if (slots > MAX_SLOTS_FOR_SEQ_GEN) {
    cat("[BEST_SEQ_CANNIB] WARNING: Limiting slots from", slots, "to", MAX_SLOTS_FOR_SEQ_GEN, "\n")
    slots <- MAX_SLOTS_FOR_SEQ_GEN
  }
  
  promo_seq = sequence_generator(slots)
  
  # SLOT-BASED: Convert slot constraints to slots if input is in weeks
  avg_slot_duration = mean(slot_metadata$Slot_Duration, na.rm = TRUE)
  
  # Convert to slots if columns exist in weeks format
  if("Min_Display_Weeks" %in% names(slot)){
    slot[, Min_Display_Slots := as.integer(ceiling(Min_Display_Weeks * 7 / avg_slot_duration))]
    slot[, Max_Display_Slots := as.integer(floor(  Max_Display_Weeks * 7 / avg_slot_duration))]
    slot[, Min_Total_Slots   := as.integer(ceiling(Min_Total_Weeks   * 7 / avg_slot_duration))]
    slot[, Max_Total_Slots   := as.integer(floor(  Max_Total_Weeks   * 7 / avg_slot_duration))]
  } else if(!"Min_Display_Slots" %in% names(slot)){
    # Create slot columns if they don't exist
    slot[, Min_Display_Slots := 0L]
    slot[, Max_Display_Slots := 0L]
    slot[, Min_Total_Slots   := 0L]
    slot[, Max_Total_Slots   := 0L]
  }
  
  #Initialize
  freeze = vector()
  final_df = data.table()
  
  #1. Filter and sort Canibalize file
  ppg_res = unique(slot$PPG)
  can = canibalize[Colour %in% c("Green","Red") & Item2 %in% ppg_res & Item1 %in% ppg_res]
  can = can[order(can$Ranking)]
  
  #2. Functions
  
  #2.1 Find Valid slots for a PPG
  
  valid_slots = function(slot,item,promo_seq,shiny_data){
    
    #item = ppg
    # SLOT-BASED: Work directly with slots
    avg_slot_duration = ifelse(exists("slot_metadata") && nrow(slot_metadata) > 0,
                               mean(slot_metadata$Slot_Duration, na.rm = TRUE), 3)
    # Use Max_Total_Slots if available, otherwise convert from weeks
    if("Max_Total_Slots" %in% names(slot)){
      s = as.integer(round(slot$Max_Total_Slots[slot$PPG == item], 0))
    } else if("Max_Total_Weeks" %in% names(slot)){
      s = as.integer(round(slot$Max_Total_Weeks[slot$PPG == item] * 7 / avg_slot_duration, 0))
    } else {
      s = 0L
    }
    
    num_of_slots = colSums(promo_seq)
    prom_poss_names = names(num_of_slots[num_of_slots == s])
    
    prom_poss = promo_seq[,prom_poss_names]
    
    ppg_base = shiny_data$Base_Units[shiny_data$PPG == item & shiny_data$Tesco_Week_No != 0]
    ppg_week_no = shiny_data$Tesco_Week_No[shiny_data$PPG == item & shiny_data$Tesco_Week_No != 0]
    
    # Ensure Tesco_Week_No is numeric to avoid type mismatch in merge
    ppg_week_no = as.numeric(as.character(ppg_week_no))
    
    # Create data.table directly to avoid type conversion issues with cbind
    df_item = data.table(
      Base_Units = ppg_base,
      prom_poss,
      PPG = item,
      Tesco_Week_No = ppg_week_no
    )
    
    df_item[, Rank:= rank(ppg_base,ties.method = c("first"))]
    
    return(df_item)
    
  }
  
  #2.2 Find best sequence for a PPG
  
  best_seq = function(item){
    
    #item = item2_df
    
    n_col = grep("*V",names(item))    #identify all the sequence
    
    # Vectorized calculation using matrix operations
    if(length(n_col) > 0){
      rank_vec = item$Rank
      seq_matrix = as.matrix(item[, n_col, with = FALSE])
      seq_sum = colSums(rank_vec * seq_matrix, na.rm = TRUE)
      # Pad with zeros for indices before n_col[1]
      seq_sum_full = rep(0, max(n_col))
      seq_sum_full[n_col] = seq_sum
      best_seq_idx = which.max(seq_sum_full)
    } else {
      best_seq_idx = 1
    }
    
    best_seq_df = data.table(
      PPG = item$PPG,
      Tesco_Week_No = as.numeric(as.character(item$Tesco_Week_No)),
      Base_Units = item$Base_Units,
      Best_Seq = item[[best_seq_idx]]
    )
    
    return(best_seq_df)
  }
  
  #2.3 Find the best sequence when cannibalization in consideration
  
  best_seq_can = function(item,col,dependent_best_seq){
    
    # item = item1_df
    # dependent_best_seq = item2_best$Best_Seq
    
    #2 Satisfy item1, with cannibalization in mind
    v_cols = grep("*V",names(item))
    if(length(v_cols) > 0){
      item1_seq = data.frame(item[, v_cols, with = F ])
    } else {
      item1_seq = data.frame()
    }
    
    #Multiply item1 pattern to item2_best_seq - VECTORIZED
    if(ncol(item1_seq) > 0 && nrow(item1_seq) > 0){
      # Convert dependent_best_seq to numeric vector and handle edge cases
      dependent_best_seq <- as.numeric(as.character(dependent_best_seq))
      
      # Handle empty or all-NA vectors
      if(length(dependent_best_seq) == 0 || all(is.na(dependent_best_seq))){
        dependent_best_seq <- rep(0, nrow(item1_seq))
      } else {
        # Handle length mismatch - ensure dependent_best_seq matches item1_seq rows
        if(length(dependent_best_seq) != nrow(item1_seq)){
          if(length(dependent_best_seq) < nrow(item1_seq)){
            # Pad with zeros if too short
            dependent_best_seq <- c(dependent_best_seq, rep(0, nrow(item1_seq) - length(dependent_best_seq)))
          } else {
            # Truncate if too long
            dependent_best_seq <- dependent_best_seq[1:nrow(item1_seq)]
          }
        }
      }
      
      # Replace any NA values with 0
      dependent_best_seq[is.na(dependent_best_seq)] <- 0
      
      # Convert item1_seq to numeric matrix - ensure all columns are numeric
      item1_seq_numeric <- data.frame(lapply(item1_seq, function(x) {
        # Convert to numeric, handling factors and characters
        num_val <- suppressWarnings(as.numeric(as.character(x)))
        # Replace NA with 0
        num_val[is.na(num_val)] <- 0
        return(num_val)
      }))
      
      item1_seq_matrix <- as.matrix(item1_seq_numeric)
      
      # Ensure matrix is numeric (should be, but double-check)
      if(!is.numeric(item1_seq_matrix)){
        item1_seq_matrix <- matrix(as.numeric(item1_seq_matrix), 
                                   nrow = nrow(item1_seq_matrix), 
                                   ncol = ncol(item1_seq_matrix))
      }
      
      # Replace any NA values with 0
      item1_seq_matrix[is.na(item1_seq_matrix)] <- 0
      
      # Create dependent_matrix with matching dimensions
      dependent_matrix = matrix(dependent_best_seq, nrow = length(dependent_best_seq), ncol = ncol(item1_seq_matrix))
      
      # Perform multiplication
      item1_seq = data.frame(item1_seq_matrix * dependent_matrix)
      if(length(v_cols) > 0){
        names(item1_seq) = names(item)[v_cols]
      }
    }
    
    #Find suitable pattern
    pattern_similarity_idx = colSums(item1_seq, na.rm = TRUE)
    
    #If col = "red" -> choose pattern with minimum idx / If "green" -> choose pattern with maximum idx
    if(length(pattern_similarity_idx) > 0 && all(!is.na(pattern_similarity_idx))){
      best_pattern_idx = ifelse(col == "Red", which.min(pattern_similarity_idx), which.max(pattern_similarity_idx))
      best_pattern_name = names(pattern_similarity_idx[best_pattern_idx])
    } else if(ncol(item1_seq) > 0 && length(names(item1_seq)) > 0){
      # Fallback: use first available column
      best_pattern_name = names(item1_seq)[1]
    } else {
      # Last resort: find first column with *V pattern in original item
      v_cols = grep("*V",names(item), value = TRUE)
      if(length(v_cols) > 0){
        best_pattern_name = v_cols[1]
      } else {
        # If no V columns exist, return zeros
        item1_best = data.table(
          PPG = item$PPG,
          Tesco_Week_No = as.numeric(as.character(item$Tesco_Week_No)),
          Base_Units = item$Base_Units,
          Best_Seq = 0
        )
        return(item1_best)
      }
    }
    
    # Validate best_pattern_name before using it
    if(is.na(best_pattern_name) || is.null(best_pattern_name) || best_pattern_name == "" || !(best_pattern_name %in% names(item))){
      # Fallback: use first available V column
      v_cols = grep("*V",names(item), value = TRUE)
      if(length(v_cols) > 0){
        best_pattern_name = v_cols[1]
      } else {
        # If no V columns exist, return zeros
        item1_best = data.table(
          PPG = item$PPG,
          Tesco_Week_No = as.numeric(as.character(item$Tesco_Week_No)),
          Base_Units = item$Base_Units,
          Best_Seq = 0
        )
        return(item1_best)
      }
    }
    
    # Ensure Tesco_Week_No is numeric to avoid type mismatch in merge
    item1_best = data.table(
      PPG = item$PPG,
      Tesco_Week_No = as.numeric(as.character(item$Tesco_Week_No)),
      Base_Units = item$Base_Units,
      Best_Seq = item[[best_pattern_name]]
    )
    
    return(item1_best)
    
  }
  
  #3 Loop over each row in cannibalization file
  
  if(nrow(can) != 0 ){   #exception handling/ when there are no combination found in cannibalization file
    
    for( i in 1:nrow(can) ){  #FOR
      
      
      #i = 2
      item2 = can$Item2[i]
      item1 = can$Item1[i]
      col = can$Colour[i]
      
      #Identify the status of item2 and item1
      item2_status = ifelse(item2 %in% freeze,"Freeze","Open")
      item1_status = ifelse(item1 %in% freeze,"Freeze","Open")
      
      #Case 1 - item2 and item1, both are open ---------------------------
      if( item2_status == "Open" & item1_status == "Open" ){   #IF1 starts
        
        item2_df = valid_slots(slot,item2,promo_seq,shiny_data)
        item1_df = valid_slots(slot,item1,promo_seq,shiny_data)
        
        #1 Give priority to item2 and freeze the best seq
        item2_best = best_seq(item2_df)                 #function used
        freeze = c(freeze,item2)                        #add item2 in freeze list
        final_df = rbind(final_df, item2_best)          #update final_df
        
        item1_best = best_seq_can(item1_df,col,item2_best$Best_Seq)            #function used
        
        freeze = c(freeze,item1)                        #add item1 in freeze list
        final_df = rbind(final_df, item1_best)          #update final_df
        
      } #IF1 OVER
      
      
      #Case 2 - item1 & item2, both are closed ---------------
      if( item2_status == "Freeze" & item1_status == "Freeze" ){ #IF2
        next
      } #IF2
      
      
      #Case 3 - one of item1/item2 is open --------------------
      if( (item2_status == "Freeze" & item1_status == "Open") |
          (item2_status == "Open" & item1_status == "Freeze") ){  #IF3
        
        item_f = ifelse(item2_status == "Freeze", item2, item1)  #identify which ppg is freeze
        item_o = ifelse(item2_status == "Open", item2, item1)    #identify which ppg is open
        
        item_o_df = valid_slots(slot,item_o,promo_seq,shiny_data)   #Find possible slots of open PPG
        
        freeze_ppg_pattern = final_df$Best_Seq[final_df$PPG == item_f]       #Find fixed pattern of Freezed PPG
        
        # Validate freeze_ppg_pattern exists and has data
        if(length(freeze_ppg_pattern) == 0 || all(is.na(freeze_ppg_pattern))){
          # If no pattern found, fallback to best_seq without cannibalization
          item_o_best = best_seq(item_o_df)
        } else {
          item_o_best = best_seq_can(item_o_df,col,freeze_ppg_pattern)    #Find Best pattern of Open PPG, with Cannibalization in mind
        }
        
        freeze = c(freeze,item_o)                        #add open item in freeze list
        final_df = rbind(final_df, item_o_best)          #update final_df
        
      } #IF3
      
    } #FOR
    
  } #IF
  
  #4 Find best sequence for PPG's which are still open - OPTIMIZED with lapply
  
  slot = slot[!(PPG %in% freeze)]
  
  if(nrow(slot) > 0){
    # Use lapply for better performance
    remaining_ppg_list = lapply(slot$PPG, function(ppg){
      ppg_df = valid_slots(slot, ppg, promo_seq, shiny_data)
      best_seq(ppg_df)
    })
    
    # Combine all at once (faster than rbind in loop)
    if(length(remaining_ppg_list) > 0){
      remaining_ppg_df = do.call(rbind, remaining_ppg_list)
      final_df = rbind(final_df, remaining_ppg_df)
      freeze = c(freeze, slot$PPG)  # Update freeze list
    }
  }
  
  #5 Attach the Best Seq column in optimization data
  # Ensure Tesco_Week_No is numeric in both data.tables to avoid type mismatch in merge
  shiny_data_merge = copy(shiny_data)
  shiny_data_merge[, Tesco_Week_No := as.numeric(as.character(Tesco_Week_No))]
  
  if(nrow(final_df) > 0){
    final_df_merge = copy(final_df)
    final_df_merge[, Tesco_Week_No := as.numeric(as.character(Tesco_Week_No))]
    opt_ip = merge(shiny_data_merge, final_df_merge[,.(PPG, Tesco_Week_No, Best_Seq)], by = c("PPG","Tesco_Week_No"), all.x = T)
  } else {
    # If final_df is empty, create opt_ip with Best_Seq = 0
    opt_ip = copy(shiny_data_merge)
    opt_ip[, Best_Seq := 0]
  }
  
  opt_ip[is.na(Best_Seq),Best_Seq := 0]       #Take care of those PPG also, for which sequence should always be 0 (Eg, PPG not in LSM)
  
  names(opt_ip) = c("PPG","Tesco_Week_No","SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT" ,"PPG_Description","Base Sales",
                    "RSP (unit)","Net Cost (Unit)", "FM%","COGS (unit)","BIP (Case)","OID","No_Of_Units","STP (Unit)",
                    "OID_Unit","UNCR_Unit","Seq")
  
  opt_ip[,VAT := 0.2]
  
  return(opt_ip)
}

time_based_prod_const <- function(prod_res,ly_kpi,start_date,end_date,tesco_slot){
  #0. Delete allready existing columns - we will recreate them below
  #   
  prod_res[, c("LY_Investment","Min_Investment","Max_Investment")] = NULL
  # REMOVED: Week end date logic - no longer using ceiling_date
  # start_date = ceiling_date(start_date,"week",week_start = 2)   # 2 means week ending on Tuesday
  #1. Get slot information from tesco_slot instead of calculating weeks
  #tesco_slot$`Start Date`[1]=dmy("25/12/2025")
  setDT(tesco_slot)
  tesco_slot$`Start Date` = dmy(tesco_slot$`Start Date`)
  tesco_slot$`Start Date`[1]=dmy("25/12/2025")
  
  tesco_slot$`End Date` = dmy(tesco_slot$`End Date`)
  
  # Filter slots within date range
  relevant_slots = tesco_slot[`Start Date` >= start_date & tesco_slot$`End Date` <= end_date]
  
  # Calculate slot durations dynamically (can be 2-13 days based on data)
  relevant_slots[, Slot_Duration := as.numeric(`End Date` - `Start Date` + 1)]
  
  # Get total days and number of slots
  total_days = sum(relevant_slots$Slot_Duration)
  no_of_slots = nrow(relevant_slots)
  
  # Calculate max promo slots (can be adjusted based on your business logic)
  max_promo_slots = ifelse(no_of_slots %% 2 == 0, no_of_slots/2, (no_of_slots + 1)/2)
  
  # Convert to days for max promo calculation (using average slot duration)
  avg_slot_duration = ifelse(nrow(relevant_slots) > 0, mean(relevant_slots$Slot_Duration), 3)
  max_promo_days = max_promo_slots * avg_slot_duration
  
  #2. Modify the columns - SLOT-BASED: Work entirely with slots
  prod_res[,Maximum_Promotion_Slots := max_promo_slots]
  prod_res[,Maximum_Promotion_Days := max_promo_days]
  
  # Non LSM columns - SLOT-BASED: Work with slots directly
  
  #3. Modify the columns - budget constraint
  ly_kpi[, current_year_date := ymd(current_year_date)]
  
  #Filtering on selected date range (not just week end dates)
  ly_kpi = ly_kpi[current_year_date >= start_date & current_year_date <= end_date]
  ly_investment_per_ppg = ly_kpi[,.(LY_Investment = sum(Trade_Investment)), 
                                 by = .(PPG, Category, Manufacturer, Brand, Format, PPG_Description)]
  
  prod_res = merge(prod_res,ly_investment_per_ppg, 
                   by = c("PPG", "Category", "Manufacturer", "Brand", "Format", "PPG_Description"), all.x = T)
  
  ## Yogitha's Shiny has different columns names - below set of commented syntax will be used by Yogitha
  # ly_investment_per_ppg = ly_kpi[,.(LY_Investment = sum(Trade_Investment)), 
  #                                by = .(PPG, Category, Manufacturer, Brand, Format, PPG_Description)]
  # 
  # prod_res = merge(prod_res,ly_investment_per_ppg, 
  #                  by = c("PPG", "Category", "Manufacturer", "Brand", "Format", "PPG_Description"), all.x = T)
  
  
  #Add Min and Max Investment
  prod_res[,Min_Investment := 0.1* LY_Investment]
  prod_res[,Max_Investment := 10*LY_Investment]
  
  
  #write.csv(prod_res,"4 Investment_and_Slot_restrictions_Time_Based.csv", row.names = F)
  return(prod_res)
}

optimization <- function(brand,shiny_const,budget_const,all_other_sales,opti_goal,opti_sign,events_base,ppg_slots,exclude_ppg,
                         last_year_kpi,include_format,roi,all_other_sales_value,include_ppg,start_date,end_date,progress = FALSE){
  # REMOVED: Week end date logic - no longer using ceiling_date
  # start_date = ceiling_date(start_date,"week",week_start = 2)   # 2 means week ending on Tuesday
  
  # FIX: Do NOT overwrite PPG with SECTOR 2! 
  # The original line "brand$PPG=brand$`SECTOR 2`" was destroying the actual PPG values.
  # Commenting out these reassignments as they seem incorrect:
  # brand$PPG=brand$`SECTOR 2`                                    # REMOVED - this was the bug!
  # brand$`SECTOR 2`=unique(events_base$`SECTOR 2`)               # REMOVED
  # brand$`TRADING COMPANY`=unique(events_base$`TRADING COMPANY`) # REMOVED
  # brand$`PRODUCT RANGE`=unique(events_base$`PRODUCT RANGE`)     # REMOVED
  # brand$FORMAT=unique(events_base$FORMAT)                       # REMOVED
  # brand$PPG_Description=unique(events_base$PPG_Description)     # REMOVED
  
  # Filter last year KPI data based on date range (not week end dates)
  last_year_kpi[,current_year_date := ymd(current_year_date)]   #date formatting
  last_year_kpi = last_year_kpi[current_year_date >= start_date & current_year_date <= end_date]
  
  # ------------------------------------------------------------------------------------------------------#
  
  zero_slot = ppg_slots[Max_Disc_Slots == 0]
  
  #Remove these PPG's from budget const (only if zero_slot has rows)
  if(nrow(zero_slot) > 0) {
    budget_const = budget_const[!(PPG %in% zero_slot$PPG)]
    events_base = events_base[!(PPG %in% zero_slot$PPG)]
    ppg_slots = ppg_slots[!(Max_Disc_Slots == 0)]
  }
  
  #-------------Exception handling finished------------------------#
  
  
  #####specify constraints ---- maximise sales and keep GM% above certain value
  goal = opti_goal$KPI_Mapping[1]
  con1 = shiny_const$KPI_Mapping[1]
  con2 = shiny_const$KPI_Mapping[2]
  con3 = shiny_const$KPI_Mapping[3]
  con4 = shiny_const$KPI_Mapping[4]
  con5 = shiny_const$KPI_Mapping[5]
  con6 = shiny_const$KPI_Mapping[6]
  
  
  
  con1_min = shiny_const$`Minimum Value`[1] ; con1_max = shiny_const$`Maximum Value`[1]
  con2_min = shiny_const$`Minimum Value`[2] ; con2_max = shiny_const$`Maximum Value`[2]
  con3_min = shiny_const$`Minimum Value`[3] ; con3_max = shiny_const$`Maximum Value`[3]
  con4_min = shiny_const$`Minimum Value`[4] ; con4_max = shiny_const$`Maximum Value`[4]
  con5_min = shiny_const$`Minimum Value`[5] ; con5_max = shiny_const$`Maximum Value`[5]
  con6_min = shiny_const$`Minimum Value`[6] ; con6_max = shiny_const$`Maximum Value`[6]
  
  #####
  
  
  ####initialize - PRE-ALLOCATE VECTORS FOR PERFORMANCE
  stop_opt = 0
  
  # Pre-calculate maximum iterations for pre-allocation
  max_events = nrow(events_base)
  max_track_ids_per_ppg = if(nrow(brand) > 0) {
    max(sapply(unique(brand$PPG), function(p) sum(brand$PPG == p & brand$Seq == 1)), na.rm = TRUE)
  } else { 0 }
  max_iterations = max(max_events * max_track_ids_per_ppg, 10000)  # Minimum 10000 to avoid issues
  
  # Pre-allocate vectors (much faster than growing dynamically)
  replace_flag_iter = vector(mode = "numeric", length = max_iterations)
  update_flag_iter = vector(mode = "numeric", length = max_iterations)
  final_flag_iter = vector(mode = "numeric", length = max_iterations)
  check_base_con_iter = vector("list", length = max_iterations)
  check_update_con_iter = vector("list", length = max_iterations)
  my_check = vector("list", length = max_iterations)
  
  j = 0
  ####
  
  
  #inserting ID for each row - later used in logic
  brand[,"Track ID"] = row.names(brand)
  brand$Tesco_Week_No=brand$`Track ID`
  
  #DEBUG: Check PPG values before filtering
  cat("\n========== PPG DEBUG ==========\n")
  cat("PPG column exists:", "PPG" %in% names(brand), "\n")
  if ("PPG" %in% names(brand)) {
    cat("Unique PPG values in brand BEFORE filter:", paste(unique(brand$PPG), collapse=", "), "\n")
    cat("PPG class:", class(brand$PPG), "\n")
    
    # FIX: If PPG is numeric (row numbers from best_seq), convert to character using include_ppg
    if (is.numeric(brand$PPG)) {
      cat("[FIX] PPG is numeric! Converting to character using include_ppg value\n")
      if (length(include_ppg) > 0 && !is.null(include_ppg) && include_ppg[1] != "") {
        brand[, PPG := as.character(include_ppg[1])]
        cat("[FIX] Set all PPG values to:", include_ppg[1], "\n")
      } else {
        brand[, PPG := as.character(PPG)]
        cat("[FIX] Converted numeric PPG to character\n")
      }
    }
    cat("Number of rows BEFORE filter:", nrow(brand), "\n")
  } else {
    cat("Available columns:", paste(names(brand), collapse=", "), "\n")
  }
  cat("include_ppg value:", include_ppg, "\n")
  cat("================================\n\n")
  
  #Keep only selected PPG
  # include_ppg should be passed from plumber_api.R, not hardcoded
  # For backward compatibility, check if include_ppg was passed
  if (!exists("include_ppg") || is.null(include_ppg) || length(include_ppg) == 0 || include_ppg == "") {
    include_ppg = unique(brand$PPG)  # Use all PPGs if not specified
    cat("Using ALL PPGs from brand data:", paste(include_ppg, collapse=", "), "\n")
  }
  
  brand = brand[ PPG %in% include_ppg ]
  cat("Number of rows AFTER filter:", nrow(brand), "\n")
  cat("PPGs in brand after filter:", paste(unique(brand$PPG), collapse=", "), "\n")
  budget_const = budget_const[PPG %in% include_ppg]
  events_base = events_base[PPG %in% include_ppg]
  ppg_slots = ppg_slots[PPG %in% include_ppg]
  
  # CRITICAL FIX: Ensure ALL PPG columns are character type for consistent merges
  cat("[FIX] Ensuring all PPG columns are character type for merges...\n")
  if ("PPG" %in% names(brand)) brand[, PPG := as.character(PPG)]
  if ("PPG" %in% names(budget_const)) budget_const[, PPG := as.character(PPG)]
  if ("PPG" %in% names(events_base)) events_base[, PPG := as.character(PPG)]
  if ("PPG" %in% names(ppg_slots)) ppg_slots[, PPG := as.character(PPG)]
  cat("[FIX] PPG types - brand:", class(brand$PPG), ", events:", class(events_base$PPG), 
      ", ppg_slots:", class(ppg_slots$PPG), ", budget:", class(budget_const$PPG), "\n")
  
  
  #Remove excluded PPG from optimizer data
  brand = brand[!(PPG %in% exclude_ppg) ]
  
  # REMOVED DEBUG PRINTS for performance
  # print("brand")
  # print(brand$Seq)
  
  budget_const = budget_const[!(PPG %in% exclude_ppg)]
  events_base = events_base[!(PPG %in% exclude_ppg)]
  ppg_slots = ppg_slots[!(PPG %in% exclude_ppg)]
  
  #Create ROI Column
  events_base[,"ROI"] = events_base[,roi, with = FALSE]                                              #addon 3 ROI's
  
  #Set keys for fast lookups - CRITICAL OPTIMIZATION
  setkey(budget_const, PPG)
  setkey(events_base, PPG, ROI)
  setkey(ppg_slots, PPG)
  
  #Keep only latest 52 slots/periods of last year kpi (instead of week end dates)
  # If you have slot information in last_year_kpi, use that
  # Otherwise, use date-based filtering for latest 52 periods
  if("Slot" %in% names(last_year_kpi)){
    latest_slots = sort(unique(last_year_kpi$Slot), decreasing = T)[1:min(52, length(unique(last_year_kpi$Slot)))]
    last_year_kpi = last_year_kpi[Slot %in% latest_slots]
  } else if("Week End Date" %in% names(last_year_kpi)){
    # Fallback: use week end date if slot column doesn't exist
    latest_52 = sort(unique(last_year_kpi$`Week End Date`), decreasing = T)[1:min(52, length(unique(last_year_kpi$`Week End Date`)))]
    last_year_kpi = last_year_kpi[ `Week End Date` %in% latest_52 ]
  } else {
    # Use date-based approach for latest 52 periods
    last_year_kpi[, Period_End := max(current_year_date), by = .(PPG, FORMAT)]
    latest_52_periods = sort(unique(last_year_kpi$Period_End), decreasing = T)[1:min(52, length(unique(last_year_kpi$Period_End)))]
    last_year_kpi = last_year_kpi[Period_End %in% latest_52_periods]
    last_year_kpi[, Period_End := NULL]
  }
  
  #Get last year kpi's for Format which are not included
  #exc_brand_1 = last_year_kpi[ !(FORMAT %in% include_format) ]       #no need now, since we are optimizing only for selected Format
  
  #Get last year kpi's for excluded ppg - latest 52 weeks
  exc_brand_2 = last_year_kpi[PPG %in% exclude_ppg]                 
  
  #Combine both
  exc_brand = exc_brand_2
  
  #####DEBUG: Data Summary After Filtering
  cat("\n========== OPTIMIZER DATA SUMMARY ==========\n")
  cat("Number of weeks in period: Calculating...\n")
  cat("PPGs in brand:", length(unique(brand$PPG)), "\n")
  cat("PPGs in events:", length(unique(events_base$PPG)), "\n")
  cat("PPGs in budget:", length(unique(budget_const$PPG)), "\n")
  cat("Total events available:", nrow(events_base), "\n")
  cat("Excluded PPGs:", ifelse(length(exclude_ppg) > 0, paste(exclude_ppg, collapse = ", "), "None"), "\n")
  cat("Included Formats:", ifelse(length(include_format) > 0, paste(include_format, collapse = ", "), "ALL"), "\n")
  cat("Included PPGs:", ifelse(length(include_ppg) > 0, paste(include_ppg[1:min(10, length(include_ppg))], collapse = ", "), "ALL"), 
      ifelse(length(include_ppg) > 10, paste0(" (and ", length(include_ppg) - 10, " more)"), ""), "\n")
  cat("==========================================\n\n")         
  #exc_brand = rbind(exc_brand_1, exc_brand_2)                        #no need now
  
  
  #These KPI's are used for calculations at Brand Level
  exc_Total_Sales = sum(exc_brand$Units)
  exc_GM_Abs = sum(exc_brand$GM_Abs)
  exc_BIP = sum(exc_brand$Net_Cost_Unit*exc_brand$Units)
  exc_Net_Revenue = sum(exc_brand$Net_Revenue)                                                    #addon
  exc_Gross_Sales = sum(exc_brand$Gross_Sales)
  exc_Inc_GM = sum(exc_brand$Inc_GM_Abs)
  exc_Total_Trade_Investment = sum(exc_brand$Trade_Investment)
  exc_NIS = sum(exc_brand$NIS)                                     
  exc_R_GM_Inc =  sum(exc_brand$R_GM_Inc)                          #addon2
  exc_R_Net_Rev_Inc = sum(exc_brand$R_Net_Revenue_Inc)             #addon2
  exc_R_NIS_Inc = sum(exc_brand$R_NIS_Inc)                         #addon2
  exc_R_Trade_Inv_Inc = sum(exc_brand$R_Total_Trade_Inv_Inc)       #addon2
  exc_Value_Sales = sum(exc_brand$Value_Sales)                                #addon_3
  
  #####calculate fix numbers for non promoted weeks####
  
  not_prom = brand[Seq == 0,]
  
  # DEBUG: Comprehensive field and formula logging
  cat("\n")
  cat("##########################################################\n")
  cat("##       COMPREHENSIVE DEBUG - ALL FIELDS & FORMULAS    ##\n")
  cat("##########################################################\n\n")
  
  cat("========== 1. INPUT BRAND DATA ==========\n")
  cat("Total brand rows:", nrow(brand), "\n")
  cat("not_prom rows (Seq==0):", nrow(not_prom), "\n")
  cat("prom rows (Seq==1):", nrow(brand[Seq == 1,]), "\n")
  cat("\nAll columns in brand:\n")
  cat(paste(names(brand), collapse=", "), "\n\n")
  
  if (nrow(not_prom) > 0) {
    cat("========== 2. KEY INPUT FIELDS (from brand/not_prom) ==========\n")
    cat("PPG values:", paste(unique(not_prom$PPG), collapse=", "), "\n")
    
    # Financial columns from cost bible
    if ("OID_Unit" %in% names(not_prom)) {
      cat("\nOID_Unit (Off-Invoice Discount per Unit):\n")
      cat("  min:", min(not_prom$OID_Unit, na.rm=TRUE), "\n")
      cat("  max:", max(not_prom$OID_Unit, na.rm=TRUE), "\n")
      cat("  mean:", mean(not_prom$OID_Unit, na.rm=TRUE), "\n")
      cat("  sum:", sum(not_prom$OID_Unit, na.rm=TRUE), "\n")
      cat("  first 5 values:", paste(head(not_prom$OID_Unit, 5), collapse=", "), "\n")
    } else {
      cat("\nOID_Unit: *** COLUMN NOT FOUND! ***\n")
    }
    
    if ("UNCR_Unit" %in% names(not_prom)) {
      cat("\nUNCR_Unit (Unconditional Rebate per Unit):\n")
      cat("  min:", min(not_prom$UNCR_Unit, na.rm=TRUE), "\n")
      cat("  max:", max(not_prom$UNCR_Unit, na.rm=TRUE), "\n")
      cat("  sum:", sum(not_prom$UNCR_Unit, na.rm=TRUE), "\n")
    } else {
      cat("\nUNCR_Unit: *** COLUMN NOT FOUND! ***\n")
    }
    
    if ("RSP (unit)" %in% names(not_prom)) {
      cat("\nRSP (unit) - Retail Selling Price:\n")
      cat("  min:", min(not_prom$`RSP (unit)`, na.rm=TRUE), "\n")
      cat("  max:", max(not_prom$`RSP (unit)`, na.rm=TRUE), "\n")
      cat("  mean:", mean(not_prom$`RSP (unit)`, na.rm=TRUE), "\n")
    } else {
      cat("\nRSP (unit): *** COLUMN NOT FOUND! ***\n")
    }
    
    if ("Net Cost (Unit)" %in% names(not_prom)) {
      cat("\nNet Cost (Unit):\n")
      cat("  min:", min(not_prom$`Net Cost (Unit)`, na.rm=TRUE), "\n")
      cat("  max:", max(not_prom$`Net Cost (Unit)`, na.rm=TRUE), "\n")
    } else {
      cat("\nNet Cost (Unit): *** COLUMN NOT FOUND! ***\n")
    }
    
    if ("COGS (unit)" %in% names(not_prom)) {
      cat("\nCOGS (unit) - Cost of Goods Sold:\n")
      cat("  min:", min(not_prom$`COGS (unit)`, na.rm=TRUE), "\n")
      cat("  max:", max(not_prom$`COGS (unit)`, na.rm=TRUE), "\n")
    } else {
      cat("\nCOGS (unit): *** COLUMN NOT FOUND! ***\n")
    }
    
    if ("STP (Unit)" %in% names(not_prom)) {
      cat("\nSTP (Unit) - Standard Transfer Price:\n")
      cat("  min:", min(not_prom$`STP (Unit)`, na.rm=TRUE), "\n")
      cat("  max:", max(not_prom$`STP (Unit)`, na.rm=TRUE), "\n")
    } else {
      cat("\nSTP (Unit): *** COLUMN NOT FOUND! ***\n")
    }
    
    if ("Base Sales" %in% names(not_prom)) {
      cat("\nBase Sales (units):\n")
      cat("  sum:", sum(not_prom$`Base Sales`, na.rm=TRUE), "\n")
      cat("  min:", min(not_prom$`Base Sales`, na.rm=TRUE), "\n")
      cat("  max:", max(not_prom$`Base Sales`, na.rm=TRUE), "\n")
    } else {
      cat("\nBase Sales: *** COLUMN NOT FOUND! ***\n")
    }
  }
  cat("\n")
  
  not_prom[,Event_Multiplier_Tesco := 0]
  not_prom[,Event_Lift := Event_Multiplier_Tesco * `Base Sales`]
  not_prom[,Total_Sales := `Base Sales` + Event_Lift]
  not_prom[,Promo_Price := `RSP (unit)`]
  not_prom[,Retro_Funding_Unit := 0.335]
#  not_prom[,Retro_Funding_Total := Retro_Funding_Unit*Total_Sales]
  not_prom[,BIP := Total_Sales*`Net Cost (Unit)`]
  not_prom[,COGS_Total := Total_Sales*`COGS (unit)`]
  not_prom[,Gross_Sales :=Promo_Price*Total_Sales]
  not_prom[,Retro_Funding_Total := Retro_Funding_Unit*Gross_Sales]
  
  not_prom[,UNCR_Total := (`RSP (unit)` - Promo_Price)*`Base Sales`]                                                  #addon
  not_prom[,OID_Total := OID_Unit*`Base Sales`]                                                    #addon
  not_prom[,Total_Trade_Investment := (UNCR_Total + OID_Total+Retro_Funding_Total)]                                  #addon
  
  # DEBUG: Comprehensive formula results
  cat("========== 3. CALCULATED FIELDS (not_prom) ==========\n")
  cat("\nFormula: Event_Lift = Event_Multiplier_Tesco * Base Sales\n")
  cat("  Event_Multiplier_Tesco (set to 0 for non-promo):", unique(not_prom$Event_Multiplier_Tesco)[1], "\n")
  cat("  Event_Lift sum:", sum(not_prom$Event_Lift, na.rm=TRUE), "\n")
  
  cat("\nFormula: Total_Sales = Base Sales + Event_Lift\n")
  cat("  Total_Sales sum:", sum(not_prom$Total_Sales, na.rm=TRUE), "\n")
  
  cat("\nFormula: Promo_Price = RSP (unit) [for non-promo weeks]\n")
  cat("  Promo_Price min:", min(not_prom$Promo_Price, na.rm=TRUE), "\n")
  cat("  Promo_Price max:", max(not_prom$Promo_Price, na.rm=TRUE), "\n")
  
  cat("\nFormula: Gross_Sales = Promo_Price * Total_Sales\n")
  cat("  Gross_Sales sum:", sum(not_prom$Gross_Sales, na.rm=TRUE), "\n")
  
  cat("\nFormula: UNCR_Total = (RSP (unit) - Promo_Price) * Base Sales\n")
  cat("  NOTE: For non-promo, RSP == Promo_Price, so UNCR_Total should be 0\n")
  cat("  RSP (unit) sample:", head(not_prom$`RSP (unit)`, 3), "\n")
  cat("  Promo_Price sample:", head(not_prom$Promo_Price, 3), "\n")
  cat("  (RSP - Promo_Price) sample:", head(not_prom$`RSP (unit)` - not_prom$Promo_Price, 3), "\n")
  cat("  UNCR_Total sum:", sum(not_prom$UNCR_Total, na.rm=TRUE), "\n")
  
  cat("\nFormula: OID_Total = OID_Unit * Base Sales\n")
  cat("  OID_Unit sample:", head(not_prom$OID_Unit, 3), "\n")
  cat("  Base Sales sample:", head(not_prom$`Base Sales`, 3), "\n")
  cat("  OID_Total sum:", sum(not_prom$OID_Total, na.rm=TRUE), "\n")
  
  cat("\n*** CRITICAL: Total_Trade_Investment = UNCR_Total + OID_Total ***\n")
  cat("  UNCR_Total sum:", sum(not_prom$UNCR_Total, na.rm=TRUE), "\n")
  cat("  OID_Total sum:", sum(not_prom$OID_Total, na.rm=TRUE), "\n")
  cat("  Total_Trade_Investment sum:", sum(not_prom$Total_Trade_Investment, na.rm=TRUE), "\n")
  cat("  >>> THIS IS WHAT FEEDS INTO Investment_ppg <<<\n")
  
  cat("\nFormula: Net_Revenue = Gross_Sales - Total_Trade_Investment\n")
  cat("Formula: NIS = Gross_Sales - UNCR_Total - OID_Total\n")
  cat("Formula: GM_Abs = Net_Revenue - COGS_Total\n")
  cat("===========================================\n\n")
  
  not_prom[,Net_Revenue := Gross_Sales - Total_Trade_Investment]                                   #addon
  not_prom[,NIS := Gross_Sales - UNCR_Total - OID_Total]                                           #addon
  not_prom[,GM_Abs := Net_Revenue - COGS_Total ]
  
  not_prom[,R_UNCR_Inc := Event_Lift*(`RSP (unit)`- Promo_Price)]                                                    #addon2
  not_prom[,R_OID_Inc := Event_Lift*OID_Unit]                                                      #addon2
  not_prom[,R_Retro_Inc := Total_Sales*Retro_Funding_Unit]                                         #addon2
  not_prom[,R_Display_Cost := 0]                                                                   #addon2
  not_prom[,R_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Cost]              #addon2
  not_prom[,R_NIS_Inc := (`STP (Unit)` - (`RSP (unit)` - Promo_Price) - OID_Unit)*Event_Lift]                         #addon2
  not_prom[,R_Net_Rev_Inc := R_NIS_Inc - R_Retro_Inc - R_Display_Cost ]                            #addon2
  not_prom[,R_GM_Inc := R_Net_Rev_Inc - (`COGS (unit)`*Event_Lift)]                                #addon2
  
  not_prom[,Inc_GM_Abs := Event_Lift*GM_Abs/Total_Sales]
  
  not_prom[,Value_Sales := Total_Sales*`RSP (unit)`]                        #addon_3
  
  #non promoted numbers are used in calculations at brand level
  not_prom_GM_Abs = sum(not_prom$GM_Abs)
  not_prom_Total_Sales = sum(not_prom$Total_Sales)
  not_prom_BIP = sum(not_prom$BIP)
  not_prom_Gross_Sales = sum(not_prom$Gross_Sales)
  not_prom_Total_Trade_Investment = sum(not_prom$Total_Trade_Investment)                          #addon
  not_prom_Net_Revenue = sum(not_prom$Net_Revenue)                                                #addon
  not_prom_NIS = sum(not_prom$NIS)
  not_prom_R_Trade_Inv_Inc = sum(not_prom$R_Trade_Inv_Inc)                                        #addon_2
  not_prom_Value_Sales = sum(not_prom$Value_Sales)                                #addon_3
  
  # DEBUG: Summary of non-promoted weeks calculations
  cat("========== 4. NON-PROMOTED WEEKS SUMMARY ==========\n")
  cat("not_prom_GM_Abs:", not_prom_GM_Abs, "\n")
  cat("not_prom_Total_Sales:", not_prom_Total_Sales, "\n")
  cat("not_prom_BIP:", not_prom_BIP, "\n")
  cat("not_prom_Gross_Sales:", not_prom_Gross_Sales, "\n")
  cat("not_prom_Total_Trade_Investment:", not_prom_Total_Trade_Investment, " *** KEY VALUE ***\n")
  cat("not_prom_Net_Revenue:", not_prom_Net_Revenue, "\n")
  cat("not_prom_NIS:", not_prom_NIS, "\n")
  cat("not_prom_Value_Sales:", not_prom_Value_Sales, "\n")
  cat("===========================================\n\n")
  
  ####play with promoted weeks####
  
  prom = brand[Seq == 1]   #filter promoted weeks
  
  cat("========== 5. PROMOTED WEEKS DATA ==========\n")
  cat("prom rows (Seq==1):", nrow(prom), "\n")
  if (nrow(prom) > 0) {
    cat("prom PPG values:", paste(unique(prom$PPG), collapse=", "), "\n")
    cat("prom Base Sales sum:", sum(prom$`Base Sales`, na.rm=TRUE), "\n")
  } else {
    cat("*** NO PROMOTED WEEKS FOUND! Seq column may not have value 1 ***\n")
    cat("Seq values in brand:", paste(unique(brand$Seq), collapse=", "), "\n")
  }
  cat("===========================================\n\n")
  
  #create a rank within PPG
  
  prom[,Rank := rank(-`Base Sales`, ties.method = c("first")), by = .(PPG)]  #highest sales => rank 1
  
  # REMOVED DEBUG PRINTS for performance
  # print("promm")
  # print(prom)
  
  #read minimum slots information
  ppg_slots_min = ppg_slots[,.(PPG,Min_Disc_Slots)]
  
  #insert min slots information in data table - p
  setkey(prom,PPG)
  setkey(ppg_slots_min, PPG)
  prom = prom[ppg_slots_min]     #left join data.table style
  
  #insert flag if slots are extra (more then minumum discount slots)  #0 => extra flag
  prom[,Extra_Slot_Flag := ifelse(Rank > Min_Disc_Slots,0,1)]
  
  events_base[,key := paste(PPG,ROI)]
  
  
  #####################################------------------------######################################
  
  #2) split all the PPG's in dataframe and store in a list
  
  #3) Place best ROI
  
  #3.1 events_base - Best ROI - Display Flag is 0
  events_base <- events_base %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.) | is.infinite(.), 0, .)))
  best_roi_discount = events_base[Display_Flag == 0,.(ROI = max(ROI)),by = .(PPG)]
  best_roi_discount[,key := paste(PPG,ROI)]
  best_roi_discount = events_base[events_base$key %in% best_roi_discount$key,]   #to extract all columns
  best_roi_discount[,"Best_ROI_Flag"] = "Discount_Only"
  
  #3.2 events_base - finding worst event - Min Display Multiplier for each PPG
  best_roi_display = events_base[Display_Flag == 1 ,.(ROI = max(ROI)),by = .(PPG)]
  best_roi_display[,key := paste(PPG,ROI)]
  best_roi_display = events_base[events_base$key %in% best_roi_display$key,]   #to extract all columns
  best_roi_display[,"Best_ROI_Flag"] = "Display_with_Discount"
  
  #3.3 merge both discount and display events
  best_roi = rbind(best_roi_discount, best_roi_display)
  
  # REMOVED DEBUG PRINTS for performance
  # print(" best_roi ")
  # print(best_roi)
  # print(" b")
  # print(prom)
  # print("slots")
  # print(ppg_slots)
  
  
  #3.4 prom - CREATE Best_ROI_Flag in prom
  prom = merge(prom,ppg_slots[,c("PPG","Min_Display_Slots","Max_Display_Slots")], all.x = T, by = "PPG")
  
  # REMOVED DEBUG PRINTS for performance
  # print("merge")
  # print(prom)
  prom$Min_Display_Slots
  prom[,Best_ROI_Flag := ifelse(Min_Display_Slots - Rank >= 0, "Display_with_Discount", "Discount_Only")]
  #3.5 creating promotion base
  prom_base = merge(prom,best_roi[,.(PPG,Discount,Display,Display_Cost,Display_Flag,Event_Multiplier_Tesco,ROI,Best_ROI_Flag)],
                    all.x = T, by = c("PPG","Best_ROI_Flag"),allow.cartesian = T)
  
  #####DEBUG: Before Starting Optimization Loop
  cat("\n========== STARTING OPTIMIZATION LOOP ==========\n")
  cat("Promotion base rows:", nrow(prom_base), "\n")
  cat("Total events to process:", nrow(events_base), "\n")
  cat("Max iterations pre-allocated:", max_iterations, "\n")
  cat("==========================================\n\n")
  
  if(DEBUG_MODE)    # Interactive debugging point
    
    # Remove duplicates based on the 'Name' column
    prom_base <- prom_base %>% distinct(Tesco_Week_No, .keep_all = TRUE)
  #prom_base$Best_ROI_Flag
  #3.6 CREATE Additional Display Flag
  prom_base[,Adi_Display_Flag := Max_Display_Slots - Rank]
  prom_base[,Flag_Check := ifelse(Display_Flag == 1,1,ifelse(Display_Flag == 0 & Adi_Display_Flag >= 0,2,0))]
  #Flag_check = 1 => only display can be done
  #Flag_check = 2 => both display and discount can be done
  #Flag_check = 0 => only discount can be done
  prom_base[,Flag_Check_Counter := 0]
  #This counter is updated to 1, when Flag_Check = 2 is found in algorithm.
  # 
  prom_base = do_calculation(prom_base)  
  setkey(prom_base, PPG, Rank)  # Set key for fast lookups
  prom_base = prom_base[order(prom_base$PPG, prom_base$Rank),]
  events_base = events_base[order(events_base$ROI, decreasing = T),]
  
  # Pre-calculate base constraint values ONCE (will be updated when prom_base changes)
  base_prom_sums = list(
    Total_Sales = sum(prom_base$Total_Sales),
    GM_Abs = sum(prom_base$GM_Abs),
    BIP = sum(prom_base$BIP),
    Gross_Sales = sum(prom_base$Gross_Sales),
    Inc_GM_Abs = sum(prom_base$Inc_GM_Abs),
    Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
    Net_Revenue = sum(prom_base$Net_Revenue),
    NIS = sum(prom_base$NIS),
    R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc),
    R_GM_Inc = sum(prom_base$R_GM_Inc),
    R_NIS_Inc = sum(prom_base$R_NIS_Inc),
    R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
    Value_Sales = sum(prom_base$Value_Sales)
  )
  
  #3.7 Satisfy Minimum Budget
  
  #<========================================------SATISFY MINIMUM BUDGET CRITERIA----------==================================#>
  
  # Pre-create track_id lookup for faster access
  track_id_lookup = split(prom_base$`Track ID`, prom_base$PPG)
  
  for(ppg in unique(budget_const$PPG)){ # FOR LOOP 1
    
    bud_satisf = 0
    ppg = as.character(ppg)
    events_ppg = events_base[PPG == ppg]  # Use keyed lookup (key already set on PPG)
    
    # REMOVED DEBUG PRINT for performance
    # print(prom_base)
    
    # Check budget status once
    budget_status = budget_check(prom_base, not_prom, ppg, budget_const)[[1]]
    
    #####DEBUG: Budget Status Check
    cat("\n--- Budget Status Check for PPG:", ppg, "---\n")
    cat("Budget Status:", budget_status, "\n")
    budget_result = budget_check(prom_base, not_prom, ppg, budget_const)
    if(length(budget_result) > 1){
      cat("Budget Details:", paste(names(budget_result), budget_result, sep="=", collapse=", "), "\n")
    } else {
      cat("Budget Details:", budget_result, "\n")
    }
    cat("---\n")
    
    if(budget_status == "between"){
      # REMOVED DEBUG PRINT for performance
      # print(paste0(ppg," -> With Best ROI events in place, Min Budget is allready Achieved"))
      next
    } else if(budget_status == "above"){
      cat("WARNING: Budget already above maximum for PPG:", ppg, "\n")
      cat("Stopping optimization - cannot proceed with budget above maximum\n")
      stop_opt = 1
      next
    } else if(budget_status == "below"){       #ELSEIF
      
      # Get track IDs once (use cached lookup)
      track_id = track_id_lookup[[ppg]]
      if(is.null(track_id) || length(track_id) == 0) next
      
      for(i in 1:nrow(events_ppg)){ #FOR LOOP 2
        if(bud_satisf == 1) break  # Early exit when budget satisfied
        
        event = events_ppg[i,]
        
        #Loop over each track id
        for(id in track_id){    #---------------------------------------------------------------------FOR LOOP 3
          if(bud_satisf == 1) break  # Early exit
          
          # Use copy only when needed (not full copy every time)
          prom_update = copy(prom_base)
          
          row_to_change = which(prom_update$`Track ID` == id)
          if(length(row_to_change) == 0) next
          
          #Get Info if it is discount only slot, display only slot or both discount/display slot
          flag_check = as.numeric(prom_update[row_to_change,"Flag_Check"])
          ROI = as.numeric(prom_update[row_to_change,"ROI"])
          counter_check = as.numeric(prom_update[row_to_change,"Flag_Check_Counter"])
          extra_slot_flag = as.numeric(prom_update[row_to_change,"Extra_Slot_Flag"])
          ppg = as.character(event$PPG)
          
          #Update slot in prom_update based on criteria's
          
          #1. All possibilities where display_flag = 0
          if(event$Display_Flag == 0 & flag_check == 0 & extra_slot_flag == 0 & bud_satisf == 0){
            prom_update = update_base(prom_update, event, row_to_change)
            con = budget_check(prom_update, not_prom, ppg, budget_const)
            if(con["Status"] == "between"){
              prom_base <- prom_update
              bud_satisf = 1
              # Update cached sums when prom_base changes
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales),
                GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP),
                Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs),
                Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue),
                NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc),
                R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc),
                R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
              break
            } else if(con["Status"] == "below"){
              prom_base <- prom_update
              # Update cached sums
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales),
                GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP),
                Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs),
                Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue),
                NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc),
                R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc),
                R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
            } else { 
              #####DEBUG: Budget Exceeded Maximum During Event Placement
              cat("\nERROR: Budget exceeded maximum for PPG:", ppg, "during event placement\n")
              cat("Event:", event$PPG, "| Track ID:", id, "| Display Flag:", event$Display_Flag, "\n")
              budget_result = budget_check(prom_update, not_prom, ppg, budget_const)
              cat("Budget Status:", budget_result[[1]], "\n")
              if(length(budget_result) > 1){
                cat("Budget Details:", paste(names(budget_result), budget_result, sep="=", collapse=", "), "\n")
              }
              cat("Stopping optimization - budget exceeded maximum\n")
              cat("---\n")
              
              stop_opt = 1
              break
            }
          }
          
          if(event$Display_Flag == 0 & flag_check == 0 & extra_slot_flag == 1 & ROI > event$ROI & bud_satisf == 0){
            prom_update = update_base(prom_update, event, row_to_change)
            con = budget_check(prom_update, not_prom, ppg, budget_const)
            if(con["Status"] == "between"){
              prom_base <- prom_update
              bud_satisf = 1
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
              break
            } else if(con["Status"] == "below"){
              prom_base <- prom_update
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
            } else { 
              #####DEBUG: Budget Exceeded Maximum During Event Placement
              cat("\nERROR: Budget exceeded maximum for PPG:", ppg, "during event placement\n")
              cat("Event:", event$PPG, "| Track ID:", id, "| Display Flag:", event$Display_Flag, "\n")
              budget_result = budget_check(prom_update, not_prom, ppg, budget_const)
              cat("Budget Status:", budget_result[[1]], "\n")
              if(length(budget_result) > 1){
                cat("Budget Details:", paste(names(budget_result), budget_result, sep="=", collapse=", "), "\n")
              }
              cat("Stopping optimization - budget exceeded maximum\n")
              cat("---\n")
              
              stop_opt = 1; break 
            }
          }
          
          if(event$Display_Flag == 0 & flag_check == 2 & counter_check == 0 & bud_satisf == 0){
            prom_update = update_base(prom_update, event, row_to_change)
            con = budget_check(prom_update, not_prom, ppg, budget_const)
            if(con["Status"] == "between"){
              prom_base <- prom_update
              bud_satisf = 1
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
              break
            } else if(con["Status"] == "below"){
              prom_base <- prom_update
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
            } else { 
              #####DEBUG: Budget Exceeded Maximum During Event Placement
              cat("\nERROR: Budget exceeded maximum for PPG:", ppg, "during event placement\n")
              cat("Event:", event$PPG, "| Track ID:", id, "| Display Flag:", event$Display_Flag, "\n")
              budget_result = budget_check(prom_update, not_prom, ppg, budget_const)
              cat("Budget Status:", budget_result[[1]], "\n")
              if(length(budget_result) > 1){
                cat("Budget Details:", paste(names(budget_result), budget_result, sep="=", collapse=", "), "\n")
              }
              cat("Stopping optimization - budget exceeded maximum\n")
              cat("---\n")
              
              stop_opt = 1; break 
            }
          }
          
          if(event$Display_Flag == 0 & flag_check == 2 & counter_check == 1 & ROI > event$ROI & bud_satisf == 0){
            prom_update = update_base(prom_update, event, row_to_change)
            con = budget_check(prom_update, not_prom, ppg, budget_const)
            if(con["Status"] == "between"){
              prom_base <- prom_update
              bud_satisf = 1
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
              break
            } else if(con["Status"] == "below"){
              prom_base <- prom_update
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
            } else { 
              #####DEBUG: Budget Exceeded Maximum During Event Placement
              cat("\nERROR: Budget exceeded maximum for PPG:", ppg, "during event placement\n")
              cat("Event:", event$PPG, "| Track ID:", id, "| Display Flag:", event$Display_Flag, "\n")
              budget_result = budget_check(prom_update, not_prom, ppg, budget_const)
              cat("Budget Status:", budget_result[[1]], "\n")
              if(length(budget_result) > 1){
                cat("Budget Details:", paste(names(budget_result), budget_result, sep="=", collapse=", "), "\n")
              }
              cat("Stopping optimization - budget exceeded maximum\n")
              cat("---\n")
              
              stop_opt = 1; break 
            }
          }
          
          #2. All possibilities where display_flag = 1
          if(event$Display_Flag == 1 & flag_check == 1 & ROI > event$ROI & bud_satisf == 0){
            prom_update = update_base(prom_update, event, row_to_change)
            con = budget_check(prom_update, not_prom, ppg, budget_const)
            if(con["Status"] == "between"){
              prom_base <- prom_update
              bud_satisf = 1
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
              break
            } else if(con["Status"] == "below"){
              prom_base <- prom_update
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
            } else { 
              #####DEBUG: Budget Exceeded Maximum During Event Placement
              cat("\nERROR: Budget exceeded maximum for PPG:", ppg, "during event placement\n")
              cat("Event:", event$PPG, "| Track ID:", id, "| Display Flag:", event$Display_Flag, "\n")
              budget_result = budget_check(prom_update, not_prom, ppg, budget_const)
              cat("Budget Status:", budget_result[[1]], "\n")
              if(length(budget_result) > 1){
                cat("Budget Details:", paste(names(budget_result), budget_result, sep="=", collapse=", "), "\n")
              }
              cat("Stopping optimization - budget exceeded maximum\n")
              cat("---\n")
              
              stop_opt = 1; break 
            }
          }
          
          if(event$Display_Flag == 1 & flag_check == 2 & counter_check == 0 & bud_satisf == 0){
            prom_update = update_base(prom_update, event, row_to_change)
            con = budget_check(prom_update, not_prom, ppg, budget_const)
            if(con["Status"] == "between"){
              prom_base <- prom_update
              bud_satisf = 1
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
              break
            } else if(con["Status"] == "below"){
              prom_base <- prom_update
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
            } else { stop_opt = 1; break }
          }
          
          if(event$Display_Flag == 1 & flag_check == 2 & counter_check == 1 & ROI > event$ROI & bud_satisf == 0){
            prom_update = update_base(prom_update, event, row_to_change)
            con = budget_check(prom_update, not_prom, ppg, budget_const)
            if(con["Status"] == "between"){
              prom_base <- prom_update
              bud_satisf = 1
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
              break
            } else if(con["Status"] == "below"){
              prom_base <- prom_update
              base_prom_sums = list(
                Total_Sales = sum(prom_base$Total_Sales), GM_Abs = sum(prom_base$GM_Abs),
                BIP = sum(prom_base$BIP), Gross_Sales = sum(prom_base$Gross_Sales),
                Inc_GM_Abs = sum(prom_base$Inc_GM_Abs), Total_Trade_Investment = sum(prom_base$Total_Trade_Investment),
                Net_Revenue = sum(prom_base$Net_Revenue), NIS = sum(prom_base$NIS),
                R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc), R_GM_Inc = sum(prom_base$R_GM_Inc),
                R_NIS_Inc = sum(prom_base$R_NIS_Inc), R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc),
                Value_Sales = sum(prom_base$Value_Sales)
              )
            } else { 
              #####DEBUG: Budget Exceeded Maximum During Event Placement
              cat("\nERROR: Budget exceeded maximum for PPG:", ppg, "during event placement\n")
              cat("Event:", event$PPG, "| Track ID:", id, "| Display Flag:", event$Display_Flag, "\n")
              budget_result = budget_check(prom_update, not_prom, ppg, budget_const)
              cat("Budget Status:", budget_result[[1]], "\n")
              if(length(budget_result) > 1){
                cat("Budget Details:", paste(names(budget_result), budget_result, sep="=", collapse=", "), "\n")
              }
              cat("Stopping optimization - budget exceeded maximum\n")
              cat("---\n")
              
              stop_opt = 1; break 
            }
          }
          
        } # FOR LOOP 3
        if(bud_satisf == 1) break  # Exit event loop when budget satisfied
      } # FOR LOOP 2
      
      if(bud_satisf == 0){
        #####DEBUG: Budget Not Satisfied
        cat("\nERROR: Could not achieve minimum budget for PPG:", ppg, "\n")
        cat("Tried all", nrow(events_ppg), "events but budget still below minimum\n")
        budget_result = budget_check(prom_base, not_prom, ppg, budget_const)
        cat("Final Budget Status:", budget_result[[1]], "\n")
        if(length(budget_result) > 1){
          cat("Budget Details:", paste(names(budget_result), budget_result, sep="=", collapse=", "), "\n")
        }
        cat("Stopping optimization - cannot proceed without minimum budget\n")
        cat("---\n")
        
        stop_opt = 1
        # REMOVED DEBUG PRINT for performance
        # print(paste0(ppg," -> Used all ROI events, but could not achieve minimum Budget."))
      }
      
    } #ELSEIF
    
  } #FOR LOOP 1
  
  # Get PPG's budget information
  budget_info_prom = prom_base[,.(Total_Trade_Investment_prom = sum(Total_Trade_Investment)), by = .(`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,
                                                                                                     FORMAT,PPG,PPG_Description)]
  
  budget_info_not_prom = not_prom[,.(Total_Trade_Investment_not_prom = sum(Total_Trade_Investment)), by = .(`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,
                                                                                                            FORMAT,PPG,PPG_Description)]
  
  budget_info = merge(budget_info_prom, budget_info_not_prom,  by = c("SECTOR 2","TRADING COMPANY","PRODUCT RANGE",
                                                                      "FORMAT","PPG","PPG_Description"), all.x = T)
  
  # DEBUG: Show budget_const before merge
  cat("[DEBUG] budget_const columns:", paste(names(budget_const), collapse=", "), "\n")
  cat("[DEBUG] budget_const data:\n")
  print(budget_const)
  cat("[DEBUG] budget_info keys before merge:\n")
  print(unique(budget_info[, c("PRODUCT RANGE", "FORMAT", "PPG", "PPG_Description"), with=FALSE]))
  
  # CRITICAL FIX: Use only PPG as the merge key for budget constraints
  # This ensures user-provided Min/Max Investment values are applied regardless of 
  # PRODUCT RANGE, FORMAT, or PPG_Description mismatches between tables
  # Reference: R Shiny server.R line 3105 builds budget constraints per PPG
  budget_const_ppg <- budget_const[, c("PPG", "Min_Investment", "Max_Investment"), with=FALSE]
  cat("[DEBUG] Merging budget_const by PPG only:\n")
  print(budget_const_ppg)
  
  budget_info = merge(budget_info, budget_const_ppg, by = "PPG", all.x = TRUE)
  
  # DEBUG: Show budget_info after merge to verify Min/Max Investment
  cat("[DEBUG] budget_info after merge:\n")
  print(budget_info[, c("PPG", "Min_Investment", "Max_Investment", "Total_Trade_Investment_prom", "Total_Trade_Investment_not_prom"), with=FALSE])
  
  budget_info[,"Total_Trade_Investment"] = budget_info$Total_Trade_Investment_prom + budget_info$Total_Trade_Investment_not_prom
  
  
  # Handle NA values in Min/Max Investment by using large defaults
  # This ensures optimization can proceed even if merge fails
  budget_info[is.na(Min_Investment), Min_Investment := 0]  # No minimum constraint
  budget_info[is.na(Max_Investment), Max_Investment := 1e20]  # No maximum constraint
  
  cat("[DEBUG] After NA handling - Min_Investment:", unique(budget_info$Min_Investment), "\n")
  cat("[DEBUG] After NA handling - Max_Investment:", unique(budget_info$Max_Investment), "\n")
  
  budget_info[,Status := ifelse(Total_Trade_Investment<Min_Investment,"Budget less than Minimum", ifelse(Total_Trade_Investment > Max_Investment,
                                                                                                         "Budget more than Maximum", "Budget Range Satisfied"))]
  
  cat("[DEBUG] Budget Status:", unique(budget_info$Status), "\n")
  
  
  #<================================================-MINIMUM BUDGET SATISFIED-====================================================>
  
  
  events_base = events_base[order(events_base$ROI, decreasing = T),]
  prom_base = prom_base[order(prom_base$PPG, prom_base$Rank),]
  
  #events_base = events_base[PPG == "AW9"]
  
  
  #If Stop_opt = 1, optimization will not run
  
  if(stop_opt != 1){
    
    # Pre-create track_id lookup for faster access (update if prom_base changed)
    if(!exists("track_id_lookup") || length(track_id_lookup) != length(unique(prom_base$PPG))){
      track_id_lookup = split(prom_base$`Track ID`, prom_base$PPG)
    }
    
    for( i in 1:nrow(events_base)){   #------------------------------------------------------------FOR LOOP 1
      
      if(stop_opt == 1) break  # Early exit if optimization stopped
      
      #Extract Best ROI event
      event = events_base[i,]
      
      #Get all the track id of a PPG - use cached lookup
      
      track_id = track_id_lookup[[event$PPG]]
      if(is.null(track_id) || length(track_id) == 0) next
      
      progress_total = (nrow(events_base) * length(track_id))
      
      #Loop over each track id
      for(id in track_id){    #---------------------------------------------------------------------FOR LOOP 2
        
        prom_update = copy(prom_base)  # Use copy() explicitly
        replacement_flag = 0
        final_flag = 0
        con_satis_flag = 0
        j = j+1
        
        #####DEBUG: Progress Tracking (every 1000 iterations)
        if(j %% 1000 == 0){
          cat("\n--- Progress Update (Iteration", j, ") ---\n")
          cat("Current PPG:", ppg, "\n")
          cat("Event ROI:", event$ROI, "\n")
          cat("Replacement Flag:", replacement_flag, "\n")
          cat("Budget Flag:", budget_flag, "\n")
          cat("Update Flag:", update_flag, "\n")
          cat("Final Flag:", final_flag, "\n")
          cat("Constraints Satisfied Flag:", con_satis_flag, "\n")
          cat("---\n")
        }
        
        #####DEBUG: Conditional browser for specific scenarios
        if(DEBUG_MODE && (j == 1 || (j %% 5000 == 0))){
          # Interactive debugging every 5000 iterations or first iteration
        }
        
        if(progress) incProgress((1/progress_total),detail = paste("Iteration", j))
        
        row_to_change = which(prom_update$`Track ID` == id)
        if(length(row_to_change) == 0) next
        
        #Get Info if it is discount only slot, display only slot or both discount/display slot
        flag_check = as.numeric(prom_update[row_to_change,"Flag_Check"])
        ROI = as.numeric(prom_update[row_to_change,"ROI"])
        counter_check = as.numeric(prom_update[row_to_change,"Flag_Check_Counter"])
        extra_slot_flag  = as.numeric(prom_update[row_to_change,"Extra_Slot_Flag"])
        ppg = as.character(event$PPG)
        
        #Update slot in prom_update based on criteria's - OPTIMIZED WITH EARLY EXITS
        
        #1. All possibilities where display_flag = 0
        if(event$Display_Flag == 0 & flag_check == 0 & extra_slot_flag == 0){
          prom_update = update_base(prom_update,event,row_to_change)
          replacement_flag = 1
        } else if(event$Display_Flag == 0 & flag_check == 0 & extra_slot_flag == 1 & ROI > event$ROI){
          prom_update = update_base(prom_update,event,row_to_change)
          replacement_flag = 1
        } else if(event$Display_Flag == 0 & flag_check == 2 & counter_check == 0){
          prom_update = update_base(prom_update,event,row_to_change)
          replacement_flag = 1
        } else if(event$Display_Flag == 0 & flag_check == 2 & counter_check == 1 & ROI > event$ROI){
          prom_update = update_base(prom_update,event,row_to_change)
          replacement_flag = 1
        } else if(event$Display_Flag == 1 & flag_check == 1 & ROI > event$ROI){
          prom_update = update_base(prom_update,event,row_to_change)
          replacement_flag = 1
        } else if(event$Display_Flag == 1 & flag_check == 2 & counter_check == 0){
          prom_update = update_base(prom_update,event,row_to_change)
          replacement_flag = 1
        } else if(event$Display_Flag == 1 & flag_check == 2 & counter_check == 1 & ROI > event$ROI){
          prom_update = update_base(prom_update,event,row_to_change)
          replacement_flag = 1
        }
        
        # Only check budget if replacement happened
        if(replacement_flag == 1) {
          budget_check_result = budget_check(prom_update,not_prom,ppg,budget_const)
          budget_flag = ifelse(budget_check_result[[1]] == "between",1,0)
          
          #####DEBUG: Budget Check Issues
          if(budget_flag == 0 && (j %% 100 == 0 || j <= 10)){
            cat("Budget check failed at iteration", j, "| PPG:", ppg, "| Status:", budget_check_result[[1]], "\n")
          }
        } else {
          budget_flag = 0
        }
        
        replace_flag_iter[j] = replacement_flag
        #print(replacement_flag)
        
        update_flag = replacement_flag * budget_flag
        
        update_flag_iter[j] = update_flag
        #============================================ROI PLACEMENT DONE==================================================#
        
        # Calculate sums for prom_update (needed for tracking even if update_flag == 0)
        update_prom_sums = NULL
        if(replacement_flag == 1) {
          update_prom_sums = list(
            Total_Sales = sum(prom_update$Total_Sales),
            GM_Abs = sum(prom_update$GM_Abs),
            BIP = sum(prom_update$BIP),
            Gross_Sales = sum(prom_update$Gross_Sales),
            Inc_GM_Abs = sum(prom_update$Inc_GM_Abs),
            Total_Trade_Investment = sum(prom_update$Total_Trade_Investment),
            Net_Revenue = sum(prom_update$Net_Revenue),
            NIS = sum(prom_update$NIS),
            R_Trade_Inv_Inc = sum(prom_update$R_Trade_Inv_Inc),
            R_GM_Inc = sum(prom_update$R_GM_Inc),
            R_NIS_Inc = sum(prom_update$R_NIS_Inc),
            R_Net_Rev_Inc = sum(prom_update$R_Net_Rev_Inc),
            Value_Sales = sum(prom_update$Value_Sales)
          )
        }
        
        #Run this loop only if budget constraint is okay and ROI was replaced
        
        #####DEBUG: When Update is Rejected
        if(update_flag == 1 && final_flag == 0 && (j %% 1000 == 0 || j <= 20)){
          cat("WARNING: Update rejected at iteration", j, "| PPG:", ppg, 
              "| Reason: Constraints not improved\n")
        }
        
        if( update_flag == 1 ){   #IF1
          
          # Use cached base sums (base_prom_sums) instead of recalculating
          check_update_con = constraint_fun(
            update_prom_sums$Total_Sales, update_prom_sums$GM_Abs, update_prom_sums$BIP,
            update_prom_sums$Gross_Sales, update_prom_sums$Inc_GM_Abs, update_prom_sums$Total_Trade_Investment,
            update_prom_sums$Net_Revenue, all_other_sales, update_prom_sums$NIS,
            not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
            not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
            exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
            exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
            con1, con2, con3, con4, con5, con6,
            con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
            con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
            goal, roi,
            update_prom_sums$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
            update_prom_sums$R_GM_Inc, exc_R_GM_Inc, update_prom_sums$R_NIS_Inc, exc_R_NIS_Inc,
            update_prom_sums$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
            exc_Value_Sales, not_prom_Value_Sales, update_prom_sums$Value_Sales, all_other_sales_value
          )
          
          # Use cached base constraint check (calculate once, reuse)
          if(!exists("base_constraint_check")){
            base_constraint_check = constraint_fun(
              base_prom_sums$Total_Sales, base_prom_sums$GM_Abs, base_prom_sums$BIP,
              base_prom_sums$Gross_Sales, base_prom_sums$Inc_GM_Abs, base_prom_sums$Total_Trade_Investment,
              base_prom_sums$Net_Revenue, all_other_sales, base_prom_sums$NIS,
              not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
              not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
              exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
              exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
              con1, con2, con3, con4, con5, con6,
              con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
              con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
              goal, roi,
              base_prom_sums$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
              base_prom_sums$R_GM_Inc, exc_R_GM_Inc, base_prom_sums$R_NIS_Inc, exc_R_NIS_Inc,
              base_prom_sums$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
              exc_Value_Sales, not_prom_Value_Sales, base_prom_sums$Value_Sales, all_other_sales_value
            )
          }
          check_base_con = base_constraint_check
          # 
          #####DEBUG: Constraint Check (every 5000 iterations or when constraints satisfied)
          if(j %% 5000 == 0 || (j <= 50)){
            cat("\n--- Constraint Check (Iteration", j, ") ---\n")
            cat("PPG:", ppg, "| Event ROI:", event$ROI, "\n")
            if(exists("check_base_con") && !is.null(check_base_con) && !is.null(check_base_con[[1]])){
              cat("Base Constraints:", 
                  paste(sapply(1:min(6, length(check_base_con[[1]])), function(x) {
                    if(!is.null(check_base_con[[1]][[x]])) check_base_con[[1]][[x]] else "NA"
                  }), collapse = ", "), "\n")
            }
            if(exists("check_update_con") && !is.null(check_update_con) && !is.null(check_update_con[[1]])){
              cat("Update Constraints:", 
                  paste(sapply(1:min(6, length(check_update_con[[1]])), function(x) {
                    if(!is.null(check_update_con[[1]][[x]])) check_update_con[[1]][[x]] else "NA"
                  }), collapse = ", "), "\n")
            }
            cat("---\n")
          }
          
          #0. Goal Flag - making sure goal always moves in right direction - always multiplied in final_flag
          
          # Initialize flags properly
          final_flag = 0
          goal_flag = 0
          con_satis_flag = 0
          
          # Get goal values from constraint checks
          base_goal_val = if(!is.null(check_base_con) && length(check_base_con) >= 5) check_base_con[[5]] else NA
          update_goal_val = if(!is.null(check_update_con) && length(check_update_con) >= 5) check_update_con[[5]] else NA
          
          # Compare goal values - goal should improve (increase for maximize, decrease for minimize)
          if(!is.na(base_goal_val) && !is.na(update_goal_val)) {
            # Check opti_sign to determine if we're maximizing or minimizing
            if(!is.null(opti_sign) && opti_sign == "minimize") {
              goal_flag = ifelse(update_goal_val < base_goal_val, 1, 0)  # Minimize: accept if update is smaller
            } else {
              goal_flag = ifelse(update_goal_val > base_goal_val, 1, 0)  # Maximize: accept if update is larger (default)
            }
            
            # If goal values are equal, allow the update (goal_flag = 1)
            # This lets constraint improvements be accepted even if goal doesn't change
            if(update_goal_val == base_goal_val) {
              goal_flag = 1
            }
          } else {
            # If goal values are NA, default to allowing update (but constraints will still filter)
            goal_flag = 1
          }
          
          #####DEBUG: When Constraints Are Satisfied
          if(con_satis_flag == 1){
            cat("✓ Constraints satisfied at iteration", j, "| PPG:", ppg, "| Event ROI:", event$ROI, "\n")
          }
          
          # Defensive accessor to avoid out-of-bounds errors when constraint
          # sublists are missing; returns NA when the requested slot is absent.
          safe_get <- function(x, i, j){
            if(!is.null(x) && length(x) >= i && length(x[[i]]) >= j){
              x[[i]][[j]]
            } else {
              NA
            }
          }
          
          #1. Constraint priority 1
          if( !is.na(check_base_con[[1]][[1]]) && check_base_con[[1]][[1]] == 0 ){
            
            #check if constraint is satisfied in prom_update. If yes, no need to check same_side_flag and diff_dec_flag
            
            if(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1){
              con_satis_flag = 1
              final_flag = con_satis_flag * goal_flag  # Multiply by goal_flag
              
            } else {
              
              #check if both prom_base and prom_update lie on same side of minimum/maximum
              base_dir1 = safe_get(check_base_con, 3, 1)
              upd_dir1 = safe_get(check_update_con, 3, 1)
              base_diff1 = safe_get(check_base_con, 4, 1)
              upd_diff1 = safe_get(check_update_con, 4, 1)
              
              same_side_flag = ifelse(!is.na(base_dir1) && !is.na(upd_dir1) && 
                                        base_dir1 == upd_dir1, 1, 0)
              diff_dec_flag = ifelse(!is.na(base_diff1) && !is.na(upd_diff1) && 
                                       upd_diff1 > base_diff1, 1, 0)
              final_flag = same_side_flag * diff_dec_flag * goal_flag  # Multiply by goal_flag
            }
          }
          
          #2. Constraint priority 2
          if( !is.na(check_base_con[[1]][[1]]) && check_base_con[[1]][[1]] == 1 && 
              !is.na(check_base_con[[1]][[2]]) && check_base_con[[1]][[2]] == 0 ){
            
            #check if constraint is satisfied in prom_update. If yes, no need to check same_side_flag and diff_dec_flag
            
            if(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1 && 
               !is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1 ){
              con_satis_flag = 1
              final_flag = con_satis_flag * goal_flag  # Multiply by goal_flag
              
            } else {
              
              #check if both prom_base and prom_update lie on same side of minimum/maximum
              base_dir2 = safe_get(check_base_con, 3, 2)
              upd_dir2 = safe_get(check_update_con, 3, 2)
              base_diff2 = safe_get(check_base_con, 4, 2)
              upd_diff2 = safe_get(check_update_con, 4, 2)
              
              same_side_flag = ifelse(!is.na(base_dir2) && !is.na(upd_dir2) && 
                                        base_dir2 == upd_dir2, 1, 0)
              diff_dec_flag = ifelse(!is.na(base_diff2) && !is.na(upd_diff2) && 
                                       upd_diff2 > base_diff2, 1, 0)
              
              #check if con1 is not satisfied in prom_update
              con1_flag_in_update = ifelse(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1, 1, 0)
              
              final_flag = same_side_flag * diff_dec_flag * con1_flag_in_update * goal_flag  # Multiply by goal_flag
            }
          }
          
          #3. Constraint priority 3
          if( !is.na(check_base_con[[1]][[1]]) && check_base_con[[1]][[1]] == 1 && 
              !is.na(check_base_con[[1]][[2]]) && check_base_con[[1]][[2]] == 1 && 
              !is.na(check_base_con[[1]][[3]]) && check_base_con[[1]][[3]] == 0 ){
            
            #check if constraint is satisfied in prom_update. If yes, no need to check same_side_flag and diff_dec_flag
            
            if(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1 && 
               !is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1 && 
               !is.na(check_update_con[[1]][[3]]) && check_update_con[[1]][[3]] == 1){
              con_satis_flag = 1
              final_flag = con_satis_flag * goal_flag  # Multiply by goal_flag
              
            } else {
              
              #check if both prom_base and prom_update lie on same side of minimum/maximum
              base_dir3 = safe_get(check_base_con, 3, 3)
              upd_dir3 = safe_get(check_update_con, 3, 3)
              base_diff3 = safe_get(check_base_con, 4, 3)
              upd_diff3 = safe_get(check_update_con, 4, 3)
              
              same_side_flag = ifelse(!is.na(base_dir3) && !is.na(upd_dir3) && 
                                        base_dir3 == upd_dir3, 1, 0)
              diff_dec_flag = ifelse(!is.na(base_diff3) && !is.na(upd_diff3) && 
                                       upd_diff3 > base_diff3, 1, 0)
              
              #check if con1, con2 is not satisfied in prom_update
              con1_flag_in_update = ifelse(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1, 1, 0)
              con2_flag_in_update = ifelse(!is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1, 1, 0)
              
              final_flag = same_side_flag * diff_dec_flag * con1_flag_in_update * con2_flag_in_update * goal_flag  # Multiply by goal_flag
            }
          }
          
          #4. Constraint priority 4
          if( !is.na(check_base_con[[1]][[1]]) && check_base_con[[1]][[1]] == 1 && 
              !is.na(check_base_con[[1]][[2]]) && check_base_con[[1]][[2]] == 1 && 
              !is.na(check_base_con[[1]][[3]]) && check_base_con[[1]][[3]] == 1 &&
              !is.na(check_base_con[[1]][[4]]) && check_base_con[[1]][[4]] == 0 ){
            
            #check if constraint is satisfied in prom_update. If yes, no need to check same_side_flag and diff_dec_flag
            
            if(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1 && 
               !is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1 && 
               !is.na(check_update_con[[1]][[3]]) && check_update_con[[1]][[3]] == 1 &&
               !is.na(check_update_con[[1]][[4]]) && check_update_con[[1]][[4]] == 1){
              con_satis_flag = 1
              final_flag = con_satis_flag * goal_flag  # Multiply by goal_flag
              
            } else {
              
              #check if both prom_base and prom_update lie on same side of minimum/maximum
              base_dir4 = safe_get(check_base_con, 3, 4)
              upd_dir4 = safe_get(check_update_con, 3, 4)
              base_diff4 = safe_get(check_base_con, 4, 4)
              upd_diff4 = safe_get(check_update_con, 4, 4)
              
              same_side_flag = ifelse(!is.na(base_dir4) && !is.na(upd_dir4) && 
                                        base_dir4 == upd_dir4, 1, 0)
              diff_dec_flag = ifelse(!is.na(base_diff4) && !is.na(upd_diff4) && 
                                       upd_diff4 > base_diff4, 1, 0)
              
              #check if con1, con2, con3 is not satisfied in prom_update
              con1_flag_in_update = ifelse(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1, 1, 0)
              con2_flag_in_update = ifelse(!is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1, 1, 0)
              con3_flag_in_update = ifelse(!is.na(check_update_con[[1]][[3]]) && check_update_con[[1]][[3]] == 1, 1, 0)
              
              final_flag = same_side_flag * diff_dec_flag * con1_flag_in_update * con2_flag_in_update * con3_flag_in_update * goal_flag  # Multiply by goal_flag
            }
          }
          
          #5. Constraint priority 5
          if( !is.na(check_base_con[[1]][[1]]) && check_base_con[[1]][[1]] == 1 && 
              !is.na(check_base_con[[1]][[2]]) && check_base_con[[1]][[2]] == 1 && 
              !is.na(check_base_con[[1]][[3]]) && check_base_con[[1]][[3]] == 1 &&
              !is.na(check_base_con[[1]][[4]]) && check_base_con[[1]][[4]] == 1 && 
              !is.na(check_base_con[[1]][[5]]) && check_base_con[[1]][[5]] == 0 ){
            
            #check if constraint is satisfied in prom_update. If yes, no need to check same_side_flag and diff_dec_flag
            
            if(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1 && 
               !is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1 && 
               !is.na(check_update_con[[1]][[3]]) && check_update_con[[1]][[3]] == 1 &&
               !is.na(check_update_con[[1]][[4]]) && check_update_con[[1]][[4]] == 1 && 
               !is.na(check_update_con[[1]][[5]]) && check_update_con[[1]][[5]] == 1){
              con_satis_flag = 1
              final_flag = con_satis_flag * goal_flag  # Multiply by goal_flag
              
            } else {
              
              #check if both prom_base and prom_update lie on same side of minimum/maximum
              base_dir5 = safe_get(check_base_con, 3, 5)
              upd_dir5 = safe_get(check_update_con, 3, 5)
              base_diff5 = safe_get(check_base_con, 4, 5)
              upd_diff5 = safe_get(check_update_con, 4, 5)
              
              same_side_flag = ifelse(!is.na(base_dir5) && !is.na(upd_dir5) && 
                                        base_dir5 == upd_dir5, 1, 0)
              diff_dec_flag = ifelse(!is.na(base_diff5) && !is.na(upd_diff5) && 
                                       upd_diff5 > base_diff5, 1, 0)
              
              #check if con1, con2, con3, con4 is not satisfied in prom_update
              con1_flag_in_update = ifelse(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1, 1, 0)
              con2_flag_in_update = ifelse(!is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1, 1, 0)
              con3_flag_in_update = ifelse(!is.na(check_update_con[[1]][[3]]) && check_update_con[[1]][[3]] == 1, 1, 0)
              con4_flag_in_update = ifelse(!is.na(check_update_con[[1]][[4]]) && check_update_con[[1]][[4]] == 1, 1, 0)
              
              final_flag = same_side_flag * diff_dec_flag * con1_flag_in_update * con2_flag_in_update * con3_flag_in_update *
                con4_flag_in_update * goal_flag  # Multiply by goal_flag
            }
          }
          
          #6. Constraint priority 6
          if( !is.na(check_base_con[[1]][[1]]) && check_base_con[[1]][[1]] == 1 && 
              !is.na(check_base_con[[1]][[2]]) && check_base_con[[1]][[2]] == 1 && 
              !is.na(check_base_con[[1]][[3]]) && check_base_con[[1]][[3]] == 1 &&
              !is.na(check_base_con[[1]][[4]]) && check_base_con[[1]][[4]] == 1 && 
              !is.na(check_base_con[[1]][[5]]) && check_base_con[[1]][[5]] == 1 && 
              !is.na(check_base_con[[1]][[6]]) && check_base_con[[1]][[6]] == 0 ){
            
            #check if constraint is satisfied in prom_update. If yes, no need to check same_side_flag and diff_dec_flag
            
            if(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1 && 
               !is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1 && 
               !is.na(check_update_con[[1]][[3]]) && check_update_con[[1]][[3]] == 1 &&
               !is.na(check_update_con[[1]][[4]]) && check_update_con[[1]][[4]] == 1 && 
               !is.na(check_update_con[[1]][[5]]) && check_update_con[[1]][[5]] == 1 && 
               !is.na(check_update_con[[1]][[6]]) && check_update_con[[1]][[6]] == 1){
              con_satis_flag = 1
              final_flag = con_satis_flag * goal_flag  # Multiply by goal_flag
              
            } else {
              
              #check if both prom_base and prom_update lie on same side of minimum/maximum
              base_dir6 = safe_get(check_base_con, 3, 6)
              upd_dir6 = safe_get(check_update_con, 3, 6)
              base_diff6 = safe_get(check_base_con, 4, 6)
              upd_diff6 = safe_get(check_update_con, 4, 6)
              
              same_side_flag = ifelse(!is.na(base_dir6) && !is.na(upd_dir6) && 
                                        base_dir6 == upd_dir6, 1, 0)
              diff_dec_flag = ifelse(!is.na(base_diff6) && !is.na(upd_diff6) && 
                                       upd_diff6 > base_diff6, 1, 0)
              
              #check if con1, con2, con3, con4, con5 is not satisfied in prom_update
              con1_flag_in_update = ifelse(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1, 1, 0)
              con2_flag_in_update = ifelse(!is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1, 1, 0)
              con3_flag_in_update = ifelse(!is.na(check_update_con[[1]][[3]]) && check_update_con[[1]][[3]] == 1, 1, 0)
              con4_flag_in_update = ifelse(!is.na(check_update_con[[1]][[4]]) && check_update_con[[1]][[4]] == 1, 1, 0)
              con5_flag_in_update = ifelse(!is.na(check_update_con[[1]][[5]]) && check_update_con[[1]][[5]] == 1, 1, 0)
              
              final_flag = same_side_flag * diff_dec_flag * con1_flag_in_update * con2_flag_in_update * con3_flag_in_update *
                con4_flag_in_update * con5_flag_in_update * goal_flag  # Multiply by goal_flag
            }
          }
          
          #7. Constraint priority 7
          if( !is.na(check_base_con[[1]][[1]]) && check_base_con[[1]][[1]] == 1 && 
              !is.na(check_base_con[[1]][[2]]) && check_base_con[[1]][[2]] == 1 && 
              !is.na(check_base_con[[1]][[3]]) && check_base_con[[1]][[3]] == 1 &&
              !is.na(check_base_con[[1]][[4]]) && check_base_con[[1]][[4]] == 1 && 
              !is.na(check_base_con[[1]][[5]]) && check_base_con[[1]][[5]] == 1 && 
              !is.na(check_base_con[[1]][[6]]) && check_base_con[[1]][[6]] == 1 ){
            
            #check if con1, con2, con3, con4, con5, con6 is not satisfied in prom_update
            con1_flag_in_update = ifelse(!is.na(check_update_con[[1]][[1]]) && check_update_con[[1]][[1]] == 1, 1, 0)
            con2_flag_in_update = ifelse(!is.na(check_update_con[[1]][[2]]) && check_update_con[[1]][[2]] == 1, 1, 0)
            con3_flag_in_update = ifelse(!is.na(check_update_con[[1]][[3]]) && check_update_con[[1]][[3]] == 1, 1, 0)
            con4_flag_in_update = ifelse(!is.na(check_update_con[[1]][[4]]) && check_update_con[[1]][[4]] == 1, 1, 0)
            con5_flag_in_update = ifelse(!is.na(check_update_con[[1]][[5]]) && check_update_con[[1]][[5]] == 1, 1, 0)
            con6_flag_in_update = ifelse(!is.na(check_update_con[[1]][[6]]) && check_update_con[[1]][[6]] == 1, 1, 0)
            
            #all constraints satisfied. now check if goal is improving or not - done in point 0.
            
            final_flag =   con1_flag_in_update * con2_flag_in_update * con3_flag_in_update *
              con4_flag_in_update * con5_flag_in_update * con6_flag_in_update * goal_flag
          }
          
        } #IF1
        
        
        # Safety check: handle NA values in final_flag
        if(is.na(final_flag)) {
          final_flag = 0
        }
        
        # DO NOT reset check_update_con - it contains the constraint check results!
        # check_update_con=0  # <-- REMOVED: This was overwriting the constraint check results
        
        # Only update prom_base if final_flag == 1 (best iteration)
        if(final_flag == 1){
          prom_base <- prom_update
          # Update cached sums and constraint check when prom_base changes
          base_prom_sums = update_prom_sums
          base_constraint_check = check_update_con  # Keep the actual constraint check result
          # Update track_id_lookup if needed
          track_id_lookup = split(prom_base$`Track ID`, prom_base$PPG)
        }
        
        final_flag_iter[j] = final_flag
        
        # Store constraint checks for iteration tracking
        if(update_flag == 1 && exists("check_update_con")){
          check_update_con_iter[[j]] = check_update_con
          check_base_con_iter[[j]] = check_base_con
        } else if(!is.null(update_prom_sums)){
          # Calculate constraint check for tracking even if update_flag == 0
          check_update_con_iter[[j]] = constraint_fun(
            update_prom_sums$Total_Sales, update_prom_sums$GM_Abs, update_prom_sums$BIP,
            update_prom_sums$Gross_Sales, update_prom_sums$Inc_GM_Abs, update_prom_sums$Total_Trade_Investment,
            update_prom_sums$Net_Revenue, all_other_sales, update_prom_sums$NIS,
            not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
            not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
            exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
            exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
            con1, con2, con3, con4, con5, con6,
            con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
            con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
            goal, roi,
            update_prom_sums$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
            update_prom_sums$R_GM_Inc, exc_R_GM_Inc, update_prom_sums$R_NIS_Inc, exc_R_NIS_Inc,
            update_prom_sums$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
            exc_Value_Sales, not_prom_Value_Sales, update_prom_sums$Value_Sales, all_other_sales_value
          )
          check_base_con_iter[[j]] = if(exists("base_constraint_check")) base_constraint_check else NULL
        } else {
          check_update_con_iter[[j]] = NULL
          check_base_con_iter[[j]] = NULL
        }
        
        my_check[[j]] = c(event$PPG, event$ROI)
        
        #j= j+1
        
        
      } # FOR LOOP 2
      
    } # FOR LOOP 1
    
  } #Stop_Opt
  
  
  x = data.table(replace_flag_iter,update_flag_iter,final_flag_iter)
  opti_output = data.table(smartbind(prom_base,not_prom))
  opti_output$`Track ID` = as.numeric(opti_output$`Track ID`)
  opti_output = opti_output[order(opti_output$`Track ID`, decreasing = F),]
  
  opti_output[is.na(opti_output)] = 0
  
  out_path = "C:/"
  
  kpi_iteration <- data.frame()
  if(length(check_base_con_iter) != 0 && j > 0){
    # Filter out NULL entries and combine at once
    valid_iterations = !sapply(check_base_con_iter[1:j], is.null)
    if(any(valid_iterations)){
      # Safely extract KPI slot (6) only when present to avoid subscript errors
      kpi_list = lapply(
        which(valid_iterations),
        function(i){
          entry <- check_base_con_iter[[i]]
          if(!is.null(entry) && length(entry) >= 6) entry[[6]] else NULL
        }
      )
      kpi_list = Filter(Negate(is.null), kpi_list)
      
      # Only bind when we have at least one KPI entry to avoid 1:NULL errors
      if(length(kpi_list) > 0){
        kpi_iteration = do.call(rbind, kpi_list)
        kpi_iteration$Iteration <- c(1:nrow(kpi_iteration))
      }
    }
  }
  
  #####DEBUG: Final Optimization Summary
  cat("\n========== OPTIMIZATION COMPLETE ==========\n")
  cat("Total iterations run:", j, "\n")
  cat("Total replacements attempted:", sum(replace_flag_iter[1:j], na.rm = TRUE), "\n")
  cat("Total updates accepted:", sum(update_flag_iter[1:j], na.rm = TRUE), "\n")
  cat("Total final updates:", sum(final_flag_iter[1:j], na.rm = TRUE), "\n")
  cat("Optimization stopped:", ifelse(stop_opt == 1, "YES (early stop)", "NO (completed)"), "\n")
  cat("Output rows:", nrow(opti_output), "\n")
  cat("KPI iterations tracked:", nrow(kpi_iteration), "\n")
  cat("==========================================\n\n")
  
  # write.csv(opti_output,paste0(out_path,"Output_sample.csv"), row.names = F)
  # write.csv(exc_brand,paste0(out_path,"Excluded_Brand.csv"), row.names = F)
  
  return(list(opti_output,exc_brand,kpi_iteration,budget_info))
}


optimizer_op_prep <- function(opti_out,base_tesco,exclude_ppg,include_format,include_ppg,start_date,end_date,tesco_slot = NULL){
  # REMOVED: Week end date logic - no longer using ceiling_date
  # start_date = ceiling_date(start_date,"week",week_start = 2)   # 2 means week ending on Tuesday
  # start_date = as_date(ifelse(start_date == ymd("2019/01/09") , ymd("2019/01/01") , start_date))
  
  #Filter data between start and end date (no week end date adjustment)
  base_tesco = base_tesco[Date>= start_date & Date <= end_date]
  
 
  ####Take Care of  FORMAT
  if( sum(grepl("ALL", include_format)) == 1 ){
    include_format = unique(base_tesco$FORMAT)
  }
  
  ####Take Care of  PPG
  if( sum(grepl("ALL", include_ppg)) == 1 ){
    include_ppg = unique(base_tesco$PPG)
  }
  
  #1.1 Get required data only
   
  base_tesco[
    order(Date),
    `:=`(
      `Start Date` = min(Date),
      `End Date`   = max(Date)
    ),
    by = .(Tesco_Week_No)          # or .(PPG, Tesco_Week_No) if you need per PPG
  ]
  
  opti_out_prom = opti_out[Seq == 1 & Extra_Slot_Flag == 1]
  
  opti_out_prom = opti_out_prom[,.(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`, FORMAT,PPG_Description, Tesco_Week_No,`Start Date`,`End Date`,Discount,Display,
                                   Display_Cost, Event_Multiplier_Tesco,Display_Flag, Promo_Price)]   
  opti_out_prom = opti_out_prom[,TPR_Flag := 1]
  
  if(sum(is.infinite(opti_out_prom$Event_Multiplier_Tesco), na.rm=TRUE) > 0){
    cat("\nSample rows with Inf Event_Multiplier_Tesco:\n")
    print(opti_out_prom[is.infinite(Event_Multiplier_Tesco), .(PPG, Event_Multiplier_Tesco, Discount, Display_Flag)][1:5])
  }
  
  #keep only selected FORMAT from tesco_base
  base_tesco = base_tesco[ FORMAT %in% include_format ]
  
  #keep only selected PPG from tesco_base
  base_tesco = base_tesco[ PPG %in% include_ppg ]
  
  #remove excluded PPG from tesco_base
  base_tesco = base_tesco[!(PPG %in% exclude_ppg)]
  
  #1.2 Join the data
  

  
  opti_out_prom$Tesco_Week_No=as.numeric(opti_out_prom$Tesco_Week_No)
 #aggreagation
  df_sum <- base_tesco %>%
    group_by(Tesco_Week_No) %>%
    summarise(
      across(where(is.character), first),
      across(where(is.Date),     first),
      RSP_Unit = first(RSP_Unit),                          # do NOT sum
      across(where(is.numeric) & !all_of("RSP_Unit"), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  tesco_full_cal = merge(df_sum,opti_out_prom, by = c("PPG","SECTOR 2","TRADING COMPANY","PRODUCT RANGE", "FORMAT","PPG_Description",
                                                          "Tesco_Week_No","Start Date","End Date"), all.x = T)
  tesco_full_cal[is.na(tesco_full_cal)] = 0
  
  
  setDT(tesco_full_cal)
  #1.3 Divide the display cost by slot duration (dynamic, not fixed 3)
  # If slot duration is available, use it; otherwise default to 3
  if("Slot_Duration" %in% names(tesco_full_cal)){
    tesco_full_cal[,Display_Cost := Display_Cost/Slot_Duration]
  } else {
    tesco_full_cal[,Display_Cost := Display_Cost/3]  # Default fallback
  }
  
  #1.4 Calculate everything
  tesco_full_cal[,Event_Lift := Event_Multiplier_Tesco * Base_Units]
  tesco_full_cal[,Total_Sales := Base_Units + Event_Lift]
  tesco_full_cal[,Promo_Price := RSP_Unit*(1-Discount)]
  tesco_full_cal[,Retro_Funding_Unit := 0.335]
  #tesco_full_cal[,Retro_Funding_Total := Retro_Funding_Unit*Total_Sales]
  tesco_full_cal[,UNCR_Total := (RSP_Unit - Promo_Price)*Total_Sales]                                                        #addon
  tesco_full_cal[,OID_Total := OID_Unit*Total_Sales]                                                          #addon
  tesco_full_cal[,Gross_Sales := Promo_Price*Total_Sales]      
  tesco_full_cal[,Retro_Funding_Total := Retro_Funding_Unit*Gross_Sales]
  
  tesco_full_cal[,Total_Trade_Investment := Retro_Funding_Total + Display_Cost + UNCR_Total + OID_Total]
  #addon
  tesco_full_cal[,Net_Revenue := Gross_Sales - Total_Trade_Investment]                                        #addon
  tesco_full_cal[,NIS := Gross_Sales - UNCR_Total - OID_Total]                                                #addon
  
  tesco_full_cal$Total_Trade_Investment
  tesco_full_cal[,BIP := Total_Sales*Net_Cost_Unit]
  tesco_full_cal[,COGS_Total := Total_Sales* COGS_Unit]
  tesco_full_cal[,GM_Abs := Net_Revenue - COGS_Total ]                                                        #modified
  tesco_full_cal[,Gross_Sales := STP_Unit*Total_Sales]
  tesco_full_cal[,Inc_GM_Abs := Event_Lift*GM_Abs/Total_Sales]
  tesco_full_cal[,FM_Abs_Unit_2 := (Promo_Price/(1+VAT)) - Net_Cost_Unit + Retro_Funding_Unit]
  tesco_full_cal[,`FM%_2` := FM_Abs_Unit_2*(1+VAT)/Promo_Price]
  tesco_full_cal[,FM_Total := FM_Abs_Unit_2 * Total_Sales]
  tesco_full_cal[,Retailer_Revenue := Total_Sales * Promo_Price]
  
  tesco_full_cal[,Inc_NIS := Event_Lift*NIS/Total_Sales]                                                      #addon
  tesco_full_cal[,Inc_Revenue := Event_Lift*Net_Revenue/Total_Sales]                                          #addon
  tesco_full_cal[,Units_Sale_in_Case := Total_Sales/No_Of_Units]                                              #addon
  
  tesco_full_cal[,R_UNCR_Inc := Event_Lift*(RSP_Unit - Promo_Price)]                                                    #addon2
  tesco_full_cal[,R_OID_Inc := Event_Lift*OID_Unit]                                                      #addon2
  tesco_full_cal[,R_Retro_Inc := Total_Sales*Retro_Funding_Unit]                                         #addon2
  tesco_full_cal[,R_Display_Cost := Display_Cost]                                                        #addon2
  tesco_full_cal[,R_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Cost]              #addon2
  tesco_full_cal[,R_NIS_Inc := (STP_Unit - (RSP_Unit - Promo_Price) - OID_Unit)*Event_Lift]                             #addon2
  tesco_full_cal[,R_Net_Rev_Inc := R_NIS_Inc - R_Retro_Inc - R_Display_Cost ]                            #addon2
  tesco_full_cal[,R_GM_Inc := R_Net_Rev_Inc - (COGS_Unit*Event_Lift)]                                    #addon2
  tesco_full_cal[,ROI := R_GM_Inc / R_Trade_Inv_Inc]

  tesco_full_cal[,Value_Sales := ifelse(Promo_Price == 0, Total_Sales*RSP_Unit, Total_Sales*Promo_Price)]     #addon3
 
 
 
 tesco_full_cal[, Duration := abs(as.numeric(`End Date` - `Start Date`))]
 
 # Export Final Calendar to CSV - using Linux-compatible path
 output_csv_path <- file.path("/app/backend/r_engine", "12 Final Calendar Tesco.csv")
 tryCatch({
   write.csv(tesco_full_cal, output_csv_path, row.names = FALSE)
   cat("[EXPORT] Wrote tesco_full_cal to:", output_csv_path, "\n")
 }, error = function(e) {
   cat("[EXPORT ERROR] Failed to write CSV:", as.character(e), "\n")
 })

  return(tesco_full_cal)
}

display_promo_const <- function(tesco_cal,event_list,roi){
  source("Recalculate_Function.R")
  ###########-- Replace Slots --############ - OPTIMIZED with lapply
  
  # Use lapply instead of loop for better performance
  df_list = lapply(unique(tesco_cal$PPG), function(ppg){
    
    #1 Filter PPG data
    ppg_cal = tesco_cal[PPG == ppg]
    ppg_event = event_list[PPG == ppg]
    
    
    #2 Check if display slot should be replaced or discount slot
    min_discount = min(ppg_cal$Promo_Price[ppg_cal$TPR_Flag == 1 & ppg_cal$Display_Flag == 0])
    max_display = max(ppg_cal$Promo_Price[ppg_cal$Display_Flag == 1])
    
    # Check how many display slots have price more then min_discount price
    display_slots_to_replace = sum( (ppg_cal$Promo_Price > min_discount)[ppg_cal$Display_Flag == 1] )
    
    # Check how many discount slots have price less then min_display price
    discount_slots_to_replace = sum( (ppg_cal$Promo_Price < max_display)[ppg_cal$TPR_Flag == 1 & ppg_cal$Display_Flag == 0] )
    
    
    x = vector()
    
    if( display_slots_to_replace == 0 & discount_slots_to_replace == 0){
      x = "all good"
      # REMOVED print for performance
      # print(paste(ppg,x))
    } else if( display_slots_to_replace == discount_slots_to_replace){
      x = "equal slots for replacement"
      # REMOVED print for performance
      # print(paste(ppg,x))
    } else if( display_slots_to_replace > discount_slots_to_replace){
      x = "discount"
      # REMOVED print for performance
      # print(paste(ppg,x))
    } else if( display_slots_to_replace < discount_slots_to_replace){
      x = "display"
      # REMOVED print for performance
      # print(paste(ppg,x))
    }
    
    
    if( x %in% c("equal slots for replacement", "discount")){
      event = ppg_event[ Promo_Price == max_display & Display_Flag == 0]                          
      
      ppg_cal <- as.data.frame(ppg_cal)
      ppg_cal[(ppg_cal$TPR_Flag == 1 & ppg_cal$Display_Flag == 0 & ppg_cal$Promo_Price < max_display)  ,c("Discount","Display","Display_Cost","Display_Flag","Event_Multiplier_Tesco")] = 
        event[,c("Discount","Display","Display_Cost","Display_Flag","Event_Multiplier_Tesco")]
      ppg_cal <- data.table(ppg_cal)
      
    } else if( x == "display"){
      event = ppg_event[ Promo_Price == min_discount & Display_Flag == 1]                          #criteria for display ?? 
      event = event[get(roi) == max(get(roi))]                                                     #taking best roi event
      # Use dynamic slot duration if available in calendar, otherwise default to 3
      if("Slot_Duration" %in% names(ppg_cal)){
        avg_slot_duration = mean(ppg_cal$Slot_Duration, na.rm = TRUE)
        event[,Display_Cost := Display_Cost/avg_slot_duration]
      } else {
        event[,Display_Cost := Display_Cost/3]  # Default fallback
      }
      
      ppg_cal <- as.data.frame(ppg_cal)
      ppg_cal[(ppg_cal$Display_Flag == 1 & ppg_cal$Promo_Price > min_discount),c("Discount","Display","Display_Cost","Display_Flag","Event_Multiplier_Tesco")] = 
        event[,c("Discount","Display","Display_Cost","Display_Flag","Event_Multiplier_Tesco")]
      ppg_cal <- data.table(ppg_cal)
      
    }
    
    return(ppg_cal)
  })
  
  # Combine all results at once (faster than rbind in loop)
  df = do.call(rbind, df_list)
  
  ######-- Recalculate KPI's --######
  df = recalculate(df)
  return(df)
}

display_slot_const <- function(calendar,max_disp,roi){
  source("Recalculate_Function.R")
  #2 Check how many slots have more than max_disp display promotion
  # Use dynamic slot duration if available, otherwise default to 3
  if("Slot_Duration" %in% names(calendar)){
    display_per_slot = calendar[,.(Num_Of_Display = sum(Display_Flag)/mean(Slot_Duration, na.rm = TRUE)), by = .(Tesco_Week_No)]
  } else {
    display_per_slot = calendar[,.(Num_Of_Display = sum(Display_Flag)/3), by = .(Tesco_Week_No)]
  }
  slots_over_limit = display_per_slot[Num_Of_Display > max_disp]
  slots_over_limit =  slots_over_limit[order(Num_Of_Display, decreasing = T)]
  
  #3 Check which slots have bandwidth for more display promotion
  slots_with_bandwidth = display_per_slot[Num_Of_Display < max_disp & Tesco_Week_No != 0]
  slots_with_bandwidth[,Bandwidth := max_disp - Num_Of_Display]
  slots_with_bandwidth = slots_with_bandwidth[order(Bandwidth,decreasing = T)]
  
  #4 Shuffle Display Events 
  
  ppg_not_possible_to_move = "none"
  
  while(nrow(slots_over_limit) != 0){
    
    # Slot with maximum no of display
    slot_no = slots_over_limit$Tesco_Week_No[1]
    num_of_display = slots_over_limit$Num_Of_Display[1]
    
    # Get PPG with worst ROI and find the alternative slots for same
    cal = calendar[Tesco_Week_No == slot_no & Display_Flag == 1, .(R_ROI_GM = sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),
                                                                   R_ROI_Rev = sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),
                                                                   R_ROI_NIS = sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),
                                                                   ROI_GM = sum(Inc_GM_Abs)/sum(Total_Trade_Investment))
                   ,by = .(PPG)]
    
    cal = cal[,ROI := get(roi)]
    
    cal = cal[!(PPG %in% ppg_not_possible_to_move)]
    
    if( nrow(cal) == 0 ){       #exception handling
      slots_over_limit = slots_over_limit[-1,]
      ppg_not_possible_to_move = "none"
      next
    }
    
    ppg_to_move = cal$PPG[which.min(cal$ROI)]
    alternate_slots = slots_with_bandwidth$Tesco_Week_No
    
    # Alternative slots available for a ppg
    avail_slots = c(unique(calendar[PPG == ppg_to_move & TPR_Flag == 1 & Display_Flag == 0, .(Tesco_Week_No)]))
    avail_slots = alternate_slots[alternate_slots %in% avail_slots$Tesco_Week_No]
    
    if( length(avail_slots) != 0){
      slot_to_replace = avail_slots[1]
      
      event_dp = calendar[Tesco_Week_No == slot_no & PPG == ppg_to_move,.(Discount,Display,Display_Cost,Display_Flag,Event_Multiplier_Tesco)]
      event_dp = event_dp[1,]
      
      event_dc = calendar[Tesco_Week_No == slot_to_replace & PPG == ppg_to_move,.(Discount,Display,Display_Cost,Display_Flag,Event_Multiplier_Tesco)]
      event_dc = event_dc[1,]
      
      #replace slot with display event
      calendar[Tesco_Week_No == slot_to_replace & PPG == ppg_to_move,c("Discount","Display","Display_Cost","Display_Flag","Event_Multiplier_Tesco")] =
        event_dp
      
      #replace slot with discount event
      calendar[Tesco_Week_No == slot_no & PPG == ppg_to_move,c("Discount","Display","Display_Cost","Display_Flag","Event_Multiplier_Tesco")] = 
        event_dc
      
      #2 Check how many slots have more than max_disp display promotion
      # Use dynamic slot duration if available
      if("Slot_Duration" %in% names(calendar)){
        display_per_slot = calendar[,.(Num_Of_Display = sum(Display_Flag)/mean(Slot_Duration, na.rm = TRUE)), by = .(Tesco_Week_No)]
      } else {
        display_per_slot = calendar[,.(Num_Of_Display = sum(Display_Flag)/3), by = .(Tesco_Week_No)]
      }
      slots_over_limit = display_per_slot[Num_Of_Display > max_disp]
      slots_over_limit =  slots_over_limit[order(Num_Of_Display, decreasing = T)]
      
      #Remove slot after it is reduced to max_display
      slots_over_limit = slots_over_limit[Num_Of_Display != max_disp]
      
      #3 Check which slots have bandwidth for more display promotion
      slots_with_bandwidth = display_per_slot[Num_Of_Display < max_disp & Tesco_Week_No != 0]
      slots_with_bandwidth[,Bandwidth := max_disp - Num_Of_Display]
      slots_with_bandwidth = slots_with_bandwidth[order(Bandwidth,decreasing = T)]
      
      ppg_not_possible_to_move = "none"
      
      print(sum(slots_over_limit$Num_Of_Display))
      
    } else{
      
      ppg_not_possible_to_move = c(ppg_not_possible_to_move,ppg_to_move)
      
    }
    
    #
    
  } #while end
  
  
  #5 Recalculate everything
  calendar = recalculate(calendar)
  
  #Display which slots violate constraint
  # Use dynamic slot duration if available
  if("Slot_Duration" %in% names(calendar)){
    display_per_slot = calendar[,.(Num_Of_Display = sum(Display_Flag)/mean(Slot_Duration, na.rm = TRUE)), by = .(Tesco_Week_No)]
  } else {
    display_per_slot = calendar[,.(Num_Of_Display = sum(Display_Flag)/3), by = .(Tesco_Week_No)]
  }
  slots_over_limit = display_per_slot[Num_Of_Display > max_disp]
  
  print(paste("These slots violate maximum display limit ==>",slots_over_limit$Tesco_Week_No))
  
  return(calendar)
}

simulator <- function(opti_cal,exc_brand,event,event_to_replace,other_sales,all_events,ROI_selected,other_sales_value){
  #Get discount, display, display_cost,display_flag, event_multiplier, ROI for event
  if("Promo Slots" %in% names(event)){
    setnames(event,"Promo Slots","Event ID")
  }
  
  # Guard: Check if event_to_replace is empty
  if (is.null(event_to_replace) || nrow(event_to_replace) == 0) {
    cat("[SIMULATOR WARNING] event_to_replace is empty, creating from event\n")
    event_to_replace <- event
  }
  
  idx = which(opti_cal$PPG == event$PPG & opti_cal$Tesco_Week_No == as.numeric(str_split(event$`Event ID`,pattern = "-")[[1]][2]))
  
  ###Adding event selected to all_events list
  event_to_replace <- event_to_replace[,!(names(event_to_replace) %in% "Select"),with = FALSE]
  
  if("PPG_Description" %in% names(event)){
    setnames(event,"PPG_Description","PPG Description")
  }
  
  event_to_replace <- rbind(event_to_replace,event)
  #initialize
  summary_df_brand = data.frame()
  summary_df_ppg = data.frame()
  summary_df_event = data.frame()
  df_list = list()
  LSM_Violate_list = list()
  for (i in 1:nrow(event_to_replace)){
    
    LSM_Violated = FALSE
    #Get info about which replace_row to replace
    replacement = event_to_replace[i,]
    #
    if(i == nrow(event_to_replace)){
      
      df = opti_cal
      
    }else if(replacement$`Event ID` != "No Event"){
      replace_row = which(opti_cal$Tesco_Week_No == as.numeric(str_split(replacement$`Event ID`,pattern = "-")[[1]][2]) & opti_cal$PPG == replacement$PPG)
      event_info = all_events[all_events$PPG == replacement$PPG & all_events$`Event ID` == replacement$`Event ID`,]
      #make a copy of opti_cal
      df = opti_cal
      df$Display_Flag
      
      if((unique(df[idx,]$Display_Flag) != event_info$Display_Flag)){
        LSM_Violated = TRUE
      }
      #do replacement in the data
      df[idx,"Discount"] = event_info$Discount    #place the Discount
      df[idx,"Display"] = event_info$Display    #place the Display Type
      df[idx,"Display_Cost"] = event_info$Display_Cost    #place Display Cost
      df[idx,"Display_Flag"] = event_info$Display_Flag    #place the Display Flag
      df[idx,"TPR_Flag"] = 1    #place the TPR Flag
      df[idx,"Event_Multiplier_Tesco"] = event_info$Event_Multiplier_Tesco    #place the event_info Multiplier
    }else if(replacement$`Event ID` == "No Event"){
      df = opti_cal
      if((unique(df[idx,]$Display_Flag) == 1) | (unique(df[idx,]$TPR_Flag) == 1)){
        LSM_Violated = TRUE
      }
      #do replacement in the data
      df[idx,"Discount"] = 0    #place the Discount
      df[idx,"Display"] =  " "   #place the Display Type
      df[idx,"Display_Cost"] = 0    #place Display Cost
      df[idx,"Display_Flag"] = 0    #place the Display Flag
      df[idx,"TPR_Flag"] = 0
      df[idx,"Event_Multiplier_Tesco"] = 0    #place the event_info Multiplier
    }
    #do calculations again
    df[,Event_Lift := Event_Multiplier_Tesco * Base_Units]
    df[,Total_Sales := Base_Units + Event_Lift]
    df[,Promo_Price := RSP_Unit*(1-Discount)]
    df[,Retro_Funding_Unit := 0.335]
    #df[,Retro_Funding_Total := Retro_Funding_Unit*Total_Sales]
    df[,Gross_Sales := Promo_Price*Total_Sales]
    df[,Retro_Funding_Total := Retro_Funding_Unit*Gross_Sales]
    
    df[,UNCR_Total := (RSP_Unit - Promo_Price)*Total_Sales]                                                                  #addon
    df[,OID_Total := OID_Unit*Total_Sales]                                                                    #addon
    df[,Total_Trade_Investment := (UNCR_Total + OID_Total +  Retro_Funding_Total + Display_Cost)]             #addon
    df[,Net_Revenue := Gross_Sales - Total_Trade_Investment]
    df[,NIS := Gross_Sales - UNCR_Total - OID_Total] 
    df[,BIP := Total_Sales* Net_Cost_Unit]
    df[,COGS_Total := Total_Sales*COGS_Unit]
    df[,GM_Abs := Net_Revenue - COGS_Total]
    df[,Retailer_Revenue := Total_Sales * Promo_Price]
    
    # df[,Inc_GM_Abs := Event_Lift*GM_Abs/Total_Sales]
    # df[,Inc_Revenue := Event_Lift*Net_Revenue/Total_Sales]                                                    
    # df[,Inc_NIS := Event_Lift*NIS/Total_Sales] 
    
    df[,R_UNCR_Inc := Event_Lift*(RSP_Unit - Promo_Price)]                                                    #addon2
    df[,R_OID_Inc := Event_Lift*OID_Unit]                                                      #addon2
    df[,R_Retro_Inc := Total_Sales*Retro_Funding_Unit]                                         #addon2
    df[,R_Display_Cost := Display_Cost]                                                        #addon2
    df[,R_Trade_Inv_Inc := R_UNCR_Inc + R_OID_Inc + R_Retro_Inc + R_Display_Cost]              #addon2
    df[,Inc_NIS := (STP_Unit - (RSP_Unit - Promo_Price) - OID_Unit)*Event_Lift]                               #addon2
    df[,Inc_Revenue := R_NIS_Inc - R_Retro_Inc - R_Display_Cost ]                              #addon2
    df[,Inc_GM_Abs := R_Net_Rev_Inc - (COGS_Unit*Event_Lift)]
    
    df[,Value_Sales := Total_Sales*Promo_Price]                                                   #addon_3
    
    #Calculate summary table
    tot_sales = sum(df$Total_Sales) + sum(exc_brand$Units)
    print(sum(exc_brand$Units))
    tot_GM_Abs = sum(df$GM_Abs) + sum(exc_brand$GM_Abs)
    #tot_BIP = sum(df$BIP) + sum(exc_brand$Net_Cost_Total)
    tot_Gross_Sales = sum(df$Gross_Sales) + sum(exc_brand$Gross_Sales)
    tot_Revenue = sum(df$Net_Revenue) + sum(exc_brand$Net_Revenue)
    
    print(sum(exc_brand$Net_Revenue))
    
    tot_NIS = sum(df$NIS) + sum(exc_brand$NIS)
    tot_Gross_Sales = sum(df$Gross_Sales) + sum(exc_brand$Gross_Sales)
    tot_Trade_Investment = sum(df$Total_Trade_Investment) + sum(exc_brand$Trade_Investment)
    tot_Trade_Investment_new = sum(df$R_Trade_Inv_Inc) + sum(exc_brand$Trade_Investment)
    tot_Inc_GM_Abs = sum(df$Inc_GM_Abs) + sum(exc_brand$Inc_GM_Abs)
    tot_base_sales = sum(df$Base_Units) + sum(exc_brand$Base_Units)
    tot_Inc_Revenue = sum(df$Inc_Revenue) + sum(exc_brand$Inc_Revenue)
    tot_Inc_NIS = sum(df$Inc_NIS) + sum(exc_brand$Inc_NIS)
    tot_Retailer_Revenue = sum(df$Retailer_Revenue)
    
    
    GM_percent_model = tot_GM_Abs*100/tot_Revenue
    Volume_sales_model = tot_sales
    Gross_sales_model = tot_Gross_Sales
    if(ROI_selected == "Incremental GM ROI"){
      ROI_model = tot_Inc_GM_Abs/tot_Trade_Investment_new
    }else if(ROI_selected == "Incremental NR ROI"){
      ROI_model = tot_Inc_Revenue/tot_Trade_Investment_new
    }else if(ROI_selected == "Incremental NIS ROI"){
      ROI_model = tot_Inc_NIS/tot_Trade_Investment_new
    }
    
    Trade_as_per_NR_model =  tot_Trade_Investment*100/tot_Revenue
    Trade_as_per_NIS_model =  tot_Trade_Investment*100/tot_NIS
    Market_Share_model = tot_Retailer_Revenue*100/(tot_Retailer_Revenue + other_sales_value)
    Net_Sales_model = tot_Revenue
    
    base_sales = tot_base_sales
    inc_sales = tot_sales - tot_base_sales
    #
    tmp = data.frame("Replaced Event" = event_to_replace[i,]$`Event ID`,Gross_sales_model/10^6,Volume_sales_model/10^6,Net_Sales_model/10^6,tot_GM_Abs/10^6,GM_percent_model,Trade_as_per_NR_model,
                     Trade_as_per_NIS_model,ROI_model, Market_Share_model, base_sales/10^6,inc_sales/10^6)
    
    summary_df_brand = rbind(tmp,summary_df_brand)
    
    #Calculate summary table for the selected PPG
    tot_sales_ppg = sum(df[df$PPG == replacement$PPG,]$Total_Sales)
    tot_GM_Abs_ppg = sum(df[df$PPG == replacement$PPG,]$GM_Abs)
    #tot_BIP_ppg = sum(df[df$PPG == replacement$PPG,]$BIP)
    tot_Revenue_ppg = sum(df[df$PPG == replacement$PPG,]$Net_Revenue)
    tot_Gross_Sales_ppg = sum(df[df$PPG == replacement$PPG,]$Gross_Sales)
    tot_Trade_Investment_ppg = sum(df[df$PPG == replacement$PPG,]$Total_Trade_Investment)
    tot_Trade_Investment_new_ppg = sum(df[df$PPG == replacement$PPG,]$R_Trade_Inv_Inc)
    
    tot_Inc_GM_Abs_ppg = sum(df[df$PPG == replacement$PPG,]$Inc_GM_Abs)
    tot_base_sales_ppg = sum(df[df$PPG == replacement$PPG,]$Base_Units)
    tot_Inc_Revenue_ppg = sum(df[df$PPG == replacement$PPG,]$Inc_Revenue)
    tot_Inc_NIS_ppg = sum(df[df$PPG == replacement$PPG,]$Inc_NIS)
    
    GM_percent_model_ppg = tot_GM_Abs_ppg*100/tot_Revenue_ppg
    Volume_sales_model_ppg = tot_sales_ppg
    Gross_sales_model_ppg = tot_Gross_Sales_ppg
    if(ROI_selected == "Incremental GM ROI"){
      ROI_model_ppg = tot_Inc_GM_Abs_ppg/tot_Trade_Investment_new_ppg
    }else if(ROI_selected == "Incremental NR ROI"){
      ROI_model_ppg = tot_Inc_Revenue_ppg/tot_Trade_Investment_new_ppg
    }else if(ROI_selected == "Incremental NIS ROI"){
      ROI_model_ppg = tot_Inc_NIS_ppg/tot_Trade_Investment_new_ppg
    }
    Trade_as_per_NR_model_ppg =  tot_Trade_Investment_ppg*100/tot_Revenue_ppg
    Net_Sales_model_ppg = tot_Revenue_ppg
    base_sales_ppg = tot_base_sales_ppg
    inc_sales_ppg = tot_sales_ppg - tot_base_sales_ppg
    #
    tmp_ppg = data.frame("Replaced Event" = event_to_replace[i,]$`Event ID`, GM_percent_model_ppg, Volume_sales_model_ppg, Gross_sales_model_ppg, ROI_model_ppg, Trade_as_per_NR_model_ppg,
                         Net_Sales_model_ppg,base_sales_ppg,inc_sales_ppg)
    
    summary_df_ppg = rbind(tmp_ppg,summary_df_ppg)
    
    #Calculate summary table for the selected event
    tot_sales_event = sum(df[idx,]$Total_Sales)
    df[idx]
    tot_GM_Abs_event = sum(df[idx,]$GM_Abs)
    #tot_BIP_event = sum(df[idx,]$BIP)
    tot_Revenue_event = sum(df[idx,]$Net_Revenue)
    tot_Gross_Sales_event = sum(df[idx,]$Gross_Sales)
    tot_Trade_Investment_event = sum(df[idx,]$Total_Trade_Investment)
    tot_Trade_Investment_new_event = sum(df[idx,]$R_Trade_Inv_Inc)
    
    tot_Inc_GM_Abs_event = sum(df[idx,]$Inc_GM_Abs)
    tot_base_sales_event = sum(df[idx,]$Base_Units)
    tot_Inc_Revenue_event = sum(df[idx,]$Inc_Revenue)
    tot_Inc_NIS_event = sum(df[idx,]$Inc_NIS)
    
    GM_percent_model_event = tot_GM_Abs_event*100/tot_Revenue_event
    Volume_sales_model_event = tot_sales_event
    Gross_sales_model_event = tot_Gross_Sales_event
    if(ROI_selected == "Incremental GM ROI"){
      ROI_model_event = tot_Inc_GM_Abs_event/tot_Trade_Investment_new_event
    }else if(ROI_selected == "Incremental NR ROI"){
      ROI_model_event = tot_Inc_Revenue_event/tot_Trade_Investment_new_event
    }else if(ROI_selected == "Incremental NIS ROI"){
      ROI_model_event = tot_Inc_NIS_event/tot_Trade_Investment_new_event
    }
    Trade_as_per_NR_model_event =  tot_Trade_Investment_event*100/tot_Revenue_event
    Net_Sales_model_event = tot_Revenue_event
    base_sales_event = tot_base_sales_event
    inc_sales_event = tot_sales_event - tot_base_sales_event
    
    tmp_event = data.frame("Replaced Event" = event_to_replace[i,]$`Event ID`,"RSP" = event_to_replace[i,]$RSP,"DisplayType" = event_to_replace[i,]$`Display Type`,"PromotedPrice" = event_to_replace[i,]$`Promoted Price`, GM_percent_model_event, Volume_sales_model_event, Gross_sales_model_event, ROI_model_event, Trade_as_per_NR_model_event,
                           Net_Sales_model_event,base_sales_event,inc_sales_event)
    
    summary_df_event = rbind(tmp_event,summary_df_event)
    
    df_list[[as.character(event_to_replace[i,]$`Event ID`)]] = df
    LSM_Violate_list[[as.character(event_to_replace[i,]$`Event ID`)]] = LSM_Violated
    
  }
  
  return(list(df_list,summary_df_brand,summary_df_ppg,summary_df_event,LSM_Violate_list))
}