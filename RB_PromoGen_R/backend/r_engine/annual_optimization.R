source("do_calculation.R")
source("constraint_fun.R")
source("ppg_budget_check_fun.R")
source("prom_update_fun.R")
source("sequence_generator_function.R")

# Load parallel processing libraries
if(!require(parallel)) install.packages("parallel")
if(!require(foreach)) install.packages("foreach")
if(!require(doParallel)) install.packages("doParallel")
library(parallel)
library(foreach)
library(doParallel)
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
  
  # Ensure tesco_slot dates are properly formatted
  # Only parse if not already Date objects - dates may come pre-parsed from plumber_api.R
  if (!inherits(tesco_slot$`Start Date`, "Date")) {
    # Try dmy first (dd/mm/yyyy), then ymd (yyyy-mm-dd) as fallback
    parsed_start <- suppressWarnings(dmy(tesco_slot$`Start Date`))
    if (all(is.na(parsed_start))) {
      parsed_start <- suppressWarnings(ymd(tesco_slot$`Start Date`))
    }
    if (all(is.na(parsed_start))) {
      parsed_start <- suppressWarnings(as.Date(tesco_slot$`Start Date`))
    }
    tesco_slot$`Start Date` <- parsed_start
  }
  
  if (!inherits(tesco_slot$`End Date`, "Date")) {
    parsed_end <- suppressWarnings(dmy(tesco_slot$`End Date`))
    if (all(is.na(parsed_end))) {
      parsed_end <- suppressWarnings(ymd(tesco_slot$`End Date`))
    }
    if (all(is.na(parsed_end))) {
      parsed_end <- suppressWarnings(as.Date(tesco_slot$`End Date`))
    }
    tesco_slot$`End Date` <- parsed_end
  }
  
  # Ensure start_date and end_date parameters are Date objects
  if (!inherits(start_date, "Date")) {
    start_date <- suppressWarnings(ymd(start_date))
    if (is.na(start_date)) start_date <- suppressWarnings(dmy(start_date))
    if (is.na(start_date)) start_date <- suppressWarnings(as.Date(start_date))
  }
  if (!inherits(end_date, "Date")) {
    end_date <- suppressWarnings(ymd(end_date))
    if (is.na(end_date)) end_date <- suppressWarnings(dmy(end_date))
    if (is.na(end_date)) end_date <- suppressWarnings(as.Date(end_date))
  }
  
  # Use slot-based logic instead of week end date logic
  # First try exact match
  slot_start = tesco_slot$Slot[tesco_slot$`Start Date` == start_date]
  slot_end = tesco_slot$Slot[tesco_slot$`End Date` == end_date]
  
  # ROBUST FALLBACK: If exact match fails, find closest slots by date range
  if (length(slot_start) == 0 || length(slot_end) == 0) {
    cat("[best_seq] Exact date match failed, using range-based slot detection...\n")
    
    # Find slot whose Start Date is closest to (but not after) start_date
    valid_start_slots <- tesco_slot$Slot[tesco_slot$`Start Date` <= start_date]
    if (length(valid_start_slots) > 0) {
      slot_start <- max(valid_start_slots)
    } else {
      # Use first available slot
      slot_start <- min(tesco_slot$Slot, na.rm = TRUE)
    }
    
    # Find slot whose End Date is closest to (but not before) end_date
    valid_end_slots <- tesco_slot$Slot[tesco_slot$`End Date` >= end_date]
    if (length(valid_end_slots) > 0) {
      slot_end <- min(valid_end_slots)
    } else {
      # Use last available slot
      slot_end <- max(tesco_slot$Slot, na.rm = TRUE)
    }
    
    cat("[best_seq] Range-based detection: slot_start =", slot_start, ", slot_end =", slot_end, "\n")
  }
  
  # Ensure slot_start and slot_end are single values
  if (length(slot_start) > 1) slot_start <- slot_start[1]
  if (length(slot_end) > 1) slot_end <- slot_end[length(slot_end)]
  
  # Final validation - ensure we have valid single values
  if (length(slot_start) == 0 || is.na(slot_start)) {
    slot_start <- min(tesco_slot$Slot, na.rm = TRUE)
    cat("[best_seq] WARNING: Using minimum slot as start:", slot_start, "\n")
  }
  if (length(slot_end) == 0 || is.na(slot_end)) {
    slot_end <- max(tesco_slot$Slot, na.rm = TRUE)
    cat("[best_seq] WARNING: Using maximum slot as end:", slot_end, "\n")
  }
  
  # Ensure slot_start <= slot_end
  if (slot_start > slot_end) {
    temp <- slot_start
    slot_start <- slot_end
    slot_end <- temp
    cat("[best_seq] WARNING: Swapped slot_start and slot_end\n")
  }
  
  all_slot = seq(slot_start, slot_end, 1)
  
  
  # Filter data based on slot numbers (not week end dates)
  shiny_data = shiny_data[Tesco_Week_No %in% all_slot]
  competition_slot = competition_slot[`Slot No` %in% all_slot]
  
  # Create slot metadata with start/end dates and duration for dynamic slot handling
  slot_metadata = tesco_slot[Slot %in% all_slot, .(Slot, `Start Date`, `End Date`)]
  slot_metadata[, Slot_Duration := as.numeric(`End Date` - `Start Date` + 1)]  # +1 to include both start and end dates
  
  # Merge slot metadata with shiny_data to have dates for each slot
  shiny_data = merge(shiny_data, slot_metadata, by.x = "Tesco_Week_No", by.y = "Slot", all.x = TRUE)
  
  
  
  #0.2 Create Promo Seq File
  #slots = length(all_slot)e
  slots = length(all_slot[all_slot != 0]) #quick fix 12th April 2019
  
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
      
      
      #5 Merge PPG data with best seq
      ppg_best_seq = cbind(one_ppg[,1:21],Best_Seq)
      
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
  
  
  
  names(opt_ip) = c("Tesco_Week_No","PPG","SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT" ,"PPG_Description","Base Sales",
                    "RSP (unit)","Net Cost (Unit)", "FM%","COGS (unit)","BIP (Case)","OID","No_Of_Units","STP (Unit)",
                    "OID_Unit","UNCR_Unit","Start Date","End Date","Duration","Seq")
  
  # Final safety: ensure the output promo decision is strict binary 0/1
  
  
  opt_ip[,VAT := 0.2]
  
  #6 Save File
  #write.csv(opt_ip,"9 Optimizer Input.csv", row.names = F)
  
  return(opt_ip)
}

best_seq_cannib <- function(shiny_data,slot,canibalize,tesco_slot,start_date,end_date){
  #0.1 Filter data between the time range - USING DYNAMIC SLOT LOGIC
  # Ensure tesco_slot dates are properly formatted
  # Only parse if not already Date objects - dates may come pre-parsed from plumber_api.R
  if (!inherits(tesco_slot$`Start Date`, "Date")) {
    parsed_start <- suppressWarnings(dmy(tesco_slot$`Start Date`))
    if (all(is.na(parsed_start))) {
      parsed_start <- suppressWarnings(ymd(tesco_slot$`Start Date`))
    }
    if (all(is.na(parsed_start))) {
      parsed_start <- suppressWarnings(as.Date(tesco_slot$`Start Date`))
    }
    tesco_slot$`Start Date` <- parsed_start
  }
  
  if (!inherits(tesco_slot$`End Date`, "Date")) {
    parsed_end <- suppressWarnings(dmy(tesco_slot$`End Date`))
    if (all(is.na(parsed_end))) {
      parsed_end <- suppressWarnings(ymd(tesco_slot$`End Date`))
    }
    if (all(is.na(parsed_end))) {
      parsed_end <- suppressWarnings(as.Date(tesco_slot$`End Date`))
    }
    tesco_slot$`End Date` <- parsed_end
  }
  
  # Ensure start_date and end_date parameters are Date objects
  if (!inherits(start_date, "Date")) {
    start_date <- suppressWarnings(ymd(start_date))
    if (is.na(start_date)) start_date <- suppressWarnings(dmy(start_date))
    if (is.na(start_date)) start_date <- suppressWarnings(as.Date(start_date))
  }
  if (!inherits(end_date, "Date")) {
    end_date <- suppressWarnings(ymd(end_date))
    if (is.na(end_date)) end_date <- suppressWarnings(dmy(end_date))
    if (is.na(end_date)) end_date <- suppressWarnings(as.Date(end_date))
  }
  
  # First try exact match
  slot_start = tesco_slot$Slot[tesco_slot$`Start Date` == start_date]
  slot_end = tesco_slot$Slot[tesco_slot$`End Date` == end_date]
  
  # ROBUST FALLBACK: If exact match fails, find closest slots by date range
  if (length(slot_start) == 0 || length(slot_end) == 0) {
    cat("[best_seq_cannib] Exact date match failed, using range-based slot detection...\n")
    
    valid_start_slots <- tesco_slot$Slot[tesco_slot$`Start Date` <= start_date]
    if (length(valid_start_slots) > 0) {
      slot_start <- max(valid_start_slots)
    } else {
      slot_start <- min(tesco_slot$Slot, na.rm = TRUE)
    }
    
    valid_end_slots <- tesco_slot$Slot[tesco_slot$`End Date` >= end_date]
    if (length(valid_end_slots) > 0) {
      slot_end <- min(valid_end_slots)
    } else {
      slot_end <- max(tesco_slot$Slot, na.rm = TRUE)
    }
    
    cat("[best_seq_cannib] Range-based detection: slot_start =", slot_start, ", slot_end =", slot_end, "\n")
  }
  
  # Ensure slot_start and slot_end are single values
  if (length(slot_start) > 1) slot_start <- slot_start[1]
  if (length(slot_end) > 1) slot_end <- slot_end[length(slot_end)]
  
  # Final validation
  if (length(slot_start) == 0 || is.na(slot_start)) {
    slot_start <- min(tesco_slot$Slot, na.rm = TRUE)
    cat("[best_seq_cannib] WARNING: Using minimum slot as start:", slot_start, "\n")
  }
  if (length(slot_end) == 0 || is.na(slot_end)) {
    slot_end <- max(tesco_slot$Slot, na.rm = TRUE)
    cat("[best_seq_cannib] WARNING: Using maximum slot as end:", slot_end, "\n")
  }
  
  # Ensure slot_start <= slot_end
  if (slot_start > slot_end) {
    temp <- slot_start
    slot_start <- slot_end
    slot_end <- temp
    cat("[best_seq_cannib] WARNING: Swapped slot_start and slot_end\n")
  }
  
  all_slot = seq(slot_start, slot_end, 1)
  shiny_data = shiny_data[Tesco_Week_No %in% all_slot]
  
  # Create slot metadata with start/end dates and duration
  slot_metadata = tesco_slot[Slot %in% all_slot, .(Slot, `Start Date`, `End Date`)]
  slot_metadata[, Slot_Duration := as.numeric(`End Date` - `Start Date` + 1)]
  shiny_data = merge(shiny_data, slot_metadata, by.x = "Tesco_Week_No", by.y = "Slot", all.x = TRUE)
  
  #0.2 Create Promo Seq File
  slots = length(all_slot)
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
  
  
  # Add Min and Max Investment
  # Relax minimum investment: let optimization & constraints decide how much to spend,
  # using only a maximum guardrail per PPG.
  prod_res[, Min_Investment := 0]
  prod_res[, Max_Investment := 10 * LY_Investment]
  
  
  write.csv(prod_res,"4 Investment_and_Slot_restrictions_Time_Based.csv", row.names = F)
  return(prod_res)
}

optimization <- function(brand,shiny_const,budget_const,all_other_sales,opti_goal,opti_sign,events_base,ppg_slots,exclude_ppg,
                         last_year_kpi,include_format,roi,all_other_sales_value,include_ppg,start_date,end_date,progress = FALSE){
  # REMOVED: Week end date logic - no longer using ceiling_date
  # start_date = ceiling_date(start_date,"week",week_start = 2)   # 2 means week ending on Tuesday
  
  if(length(include_ppg) == 1 && grepl("ALL", include_ppg, ignore.case = TRUE)) {
    # Get all unique PPGs from the data sources
    all_ppgs = unique(c(
      unique(brand$PPG),
      unique(budget_const$PPG),
      unique(events_base$PPG),
      unique(ppg_slots$PPG)
    ))
    # Remove any NA values
    all_ppgs = all_ppgs[!is.na(all_ppgs)]
    include_ppg = all_ppgs
  }
  
   
  
  
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
  #brand$Tesco_Week_No=brand$`Track ID`
  
  #Keep only selected PPG
  
  brand = brand[ PPG %in% include_ppg ]
  budget_const = budget_const[PPG %in% include_ppg]
  events_base = events_base[PPG %in% include_ppg]
  ppg_slots = ppg_slots[PPG %in% include_ppg]
  
  
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
  
  events_base$Flyer_Flag
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
  not_prom[,Total_Trade_Investment := (UNCR_Total + OID_Total+Retro_Funding_Total)]                                    #addon
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
  
  ####play with promoted weeks####
   
  prom = brand[Seq == 1]   #filter promoted weeks
  
  
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
  best_roi_discount = events_base[Display_Flag == 0 & Flyer_Flag!=1,.(ROI = max(ROI)),by = .(PPG)]
  best_roi_discount[,key := paste(PPG,ROI)]
  best_roi_discount = events_base[events_base$key %in% best_roi_discount$key,]   #to extract all columns
  best_roi_discount[,"Best_ROI_Flag"] = "Discount_Only"
  
  #3.2 events_base - finding worst event - Min Display Multiplier for each PPG
  best_roi_display = events_base[Display_Flag == 1 & Flyer_Flag!=1 ,.(ROI = max(ROI)),by = .(PPG)]
  best_roi_display[,key := paste(PPG,ROI)]
  best_roi_display = events_base[events_base$key %in% best_roi_display$key,]   #to extract all columns
  best_roi_display[,"Best_ROI_Flag"] = "Display_with_Discount"
  
  
  best_roi_flyer= events_base[Flyer_Flag == 1 ,.(ROI = max(ROI)),by = .(PPG)]
  best_roi_flyer[,key := paste(PPG,ROI)]
  best_roi_flyer = events_base[events_base$key %in% best_roi_flyer$key,]   #to extract all columns
  best_roi_flyer[,"Best_ROI_Flag"] = "Flyer_with_Discount"
  
  best_roi_display_flyer <- events_base[
    Display_Flag == 1 & Flyer_Flag == 1,
    .(ROI = max(ROI)),
    by = .(PPG)
  ]
  best_roi_display_flyer[,key := paste(PPG,ROI)]
  best_roi_display_flyer = events_base[events_base$key %in% best_roi_display_flyer$key,]   #to extract all columns
  best_roi_display_flyer[,"Best_ROI_Flag"] = "Display and flyer with discount"
  #3.3 merge both discount and display events
  
  best_roi = rbind(best_roi_discount, best_roi_display,best_roi_flyer, best_roi_display_flyer)
  
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
  prom[,Best_ROI_Flag := ifelse(Min_Display_Slots - Rank >= 0, "Display_with_Discount", "Discount_Only")]
  #3.5 creating promotion base
 
  prom_base = merge(prom,best_roi[,.(PPG,Discount,Display,Display_Cost,Display_Flag
                                     ,Flyer,Flyer_Cost,Flyer_Flag,Event_Multiplier_Tesco,Event_Multiplier_Discount,
                                     Event_Multiplier_Flyer,Event_Multiplier_Display,Event_Multiplier_Flyer_Display,ROI,Best_ROI_Flag)],
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
  #3.6 CREATE Additional Display and Flyer Flags
  prom_base[,Adi_Display_Flag := Max_Display_Slots - Rank]
  
  # Initialize Flyer_Flag if missing
  if(!"Flyer_Flag" %in% names(prom_base)) {
    prom_base[,Flyer_Flag := 0]
  }
  if(!"Flyer" %in% names(prom_base)) {
    prom_base[,Flyer := NA]
  }
  if(!"Flyer_Cost" %in% names(prom_base)) {
    prom_base[,Flyer_Cost := 0]
  }
  
  # Add flyer flag if flyer slots exist, otherwise use display slots
  if("Max_Flyer_Slots" %in% names(prom_base)) {
    prom_base[,Adi_Flyer_Flag := Max_Flyer_Slots - Rank]
  } else {
    prom_base[,Adi_Flyer_Flag := Adi_Display_Flag]  # Use display slots for flyer if no separate constraint
  }
  
  # Update Flag_Check to handle flyer events
  # Priority: Display+Flyer > Display only > Flyer only > Discount+Display/Flyer > Discount only
  prom_base[,Flag_Check := ifelse(Display_Flag == 1 & Flyer_Flag == 1, 3,  # Both display and flyer (must keep both)
                                  ifelse(Display_Flag == 1 & Flyer_Flag == 0, 1,  # Only display (must keep display)
                                         ifelse(Display_Flag == 0 & Flyer_Flag == 1, 4,  # Only flyer (must keep flyer)
                                                ifelse(Display_Flag == 0 & Flyer_Flag == 0 & Adi_Display_Flag >= 0, 2, 0))))]
  #Flag_check = 0 => only discount can be done (no display/flyer slots available)
  #Flag_check = 1 => only display can be done (display already active)
  #Flag_check = 2 => both display/flyer and discount can be done (slots available, no display/flyer active)
  #Flag_check = 3 => display + flyer can be done (both active)
  #Flag_check = 4 => only flyer can be done (flyer already active)
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
  
  # Pre-calculate base constraint check BEFORE minimum budget loop (needed for constraint validation)
  base_sums_before_budget = list(
    Total_Sales = sum(prom_base$Total_Sales, na.rm = TRUE),
    GM_Abs = sum(prom_base$GM_Abs, na.rm = TRUE),
    BIP = sum(prom_base$BIP, na.rm = TRUE),
    Gross_Sales = sum(prom_base$Gross_Sales, na.rm = TRUE),
    Inc_GM_Abs = sum(prom_base$Inc_GM_Abs, na.rm = TRUE),
    Total_Trade_Investment = sum(prom_base$Total_Trade_Investment, na.rm = TRUE),
    Net_Revenue = sum(prom_base$Net_Revenue, na.rm = TRUE),
    NIS = sum(prom_base$NIS, na.rm = TRUE),
    R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc, na.rm = TRUE),
    R_GM_Inc = sum(prom_base$R_GM_Inc, na.rm = TRUE),
    R_NIS_Inc = sum(prom_base$R_NIS_Inc, na.rm = TRUE),
    R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc, na.rm = TRUE),
    Value_Sales = sum(prom_base$Value_Sales, na.rm = TRUE)
  )
  
  check_base_con_before_budget = constraint_fun(
    base_sums_before_budget$Total_Sales, base_sums_before_budget$GM_Abs, base_sums_before_budget$BIP,
    base_sums_before_budget$Gross_Sales, base_sums_before_budget$Inc_GM_Abs,
    base_sums_before_budget$Total_Trade_Investment, base_sums_before_budget$Net_Revenue,
    all_other_sales, base_sums_before_budget$NIS,
    not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
    not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
    exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
    exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
    con1, con2, con3, con4, con5, con6,
    con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
    con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
    goal, roi,
    base_sums_before_budget$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
    base_sums_before_budget$R_GM_Inc, exc_R_GM_Inc, base_sums_before_budget$R_NIS_Inc, exc_R_NIS_Inc,
    base_sums_before_budget$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
    exc_Value_Sales, not_prom_Value_Sales, base_sums_before_budget$Value_Sales, all_other_sales_value,
    scope = "PROMO_ONLY"
  )
  
  # Pre-create track_id lookup for faster access
  track_id_lookup = split(prom_base$`Track ID`, prom_base$PPG)
  
  # Helper function to check if budget update respects constraints
  check_budget_update_constraints = function(prom_update_check, check_base_con_ref) {
    update_sums_check = list(
      Total_Sales = sum(prom_update_check$Total_Sales, na.rm = TRUE),
      GM_Abs = sum(prom_update_check$GM_Abs, na.rm = TRUE),
      BIP = sum(prom_update_check$BIP, na.rm = TRUE),
      Gross_Sales = sum(prom_update_check$Gross_Sales, na.rm = TRUE),
      Inc_GM_Abs = sum(prom_update_check$Inc_GM_Abs, na.rm = TRUE),
      Total_Trade_Investment = sum(prom_update_check$Total_Trade_Investment, na.rm = TRUE),
      Net_Revenue = sum(prom_update_check$Net_Revenue, na.rm = TRUE),
      NIS = sum(prom_update_check$NIS, na.rm = TRUE),
      R_Trade_Inv_Inc = sum(prom_update_check$R_Trade_Inv_Inc, na.rm = TRUE),
      R_GM_Inc = sum(prom_update_check$R_GM_Inc, na.rm = TRUE),
      R_NIS_Inc = sum(prom_update_check$R_NIS_Inc, na.rm = TRUE),
      R_Net_Rev_Inc = sum(prom_update_check$R_Net_Rev_Inc, na.rm = TRUE),
      Value_Sales = sum(prom_update_check$Value_Sales, na.rm = TRUE)
    )
    
    check_update_con_check = constraint_fun(
      update_sums_check$Total_Sales, update_sums_check$GM_Abs, update_sums_check$BIP,
      update_sums_check$Gross_Sales, update_sums_check$Inc_GM_Abs,
      update_sums_check$Total_Trade_Investment, update_sums_check$Net_Revenue,
      all_other_sales, update_sums_check$NIS,
      not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
      not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
      exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
      exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
      con1, con2, con3, con4, con5, con6,
      con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
      con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
      goal, roi,
      update_sums_check$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
      update_sums_check$R_GM_Inc, exc_R_GM_Inc, update_sums_check$R_NIS_Inc, exc_R_NIS_Inc,
      update_sums_check$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
      exc_Value_Sales, not_prom_Value_Sales, update_sums_check$Value_Sales, all_other_sales_value,
      scope = "PROMO_ONLY"
    )
    
    # Priority 1 constraint check: Only accept if constraint is satisfied OR not violated worse
    base_con1_satisfied = tryCatch(check_base_con_ref[[1]][[1]] == 1, error = function(e) FALSE)
    update_con1_satisfied = tryCatch(check_update_con_check[[1]][[1]] == 1, error = function(e) FALSE)
    
    if(update_con1_satisfied) {
      return(list(acceptable = TRUE, check_update_con = check_update_con_check, update_sums = update_sums_check))
    } else if(base_con1_satisfied) {
      # Base satisfies Priority 1, update must also satisfy it
      return(list(acceptable = FALSE, check_update_con = check_update_con_check, update_sums = update_sums_check))
    } else {
      # Both violate Priority 1 - only accept if update is better (closer to satisfying)
      base_con1_value = tryCatch(check_base_con_ref[[2]][[1]], error = function(e) NA_real_)
      update_con1_value = tryCatch(check_update_con_check[[2]][[1]], error = function(e) NA_real_)
      
      if(!is.na(base_con1_value) && !is.na(update_con1_value)) {
        # Accept if update value is >= base value (moving toward constraint satisfaction)
        acceptable = update_con1_value >= base_con1_value
        return(list(acceptable = acceptable, check_update_con = check_update_con_check, update_sums = update_sums_check))
      } else {
        return(list(acceptable = FALSE, check_update_con = check_update_con_check, update_sums = update_sums_check))
      }
    }
  }
  
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
      # Min-budget repair is disabled; we let the main optimization loop
      # and per-candidate budget_check (in evaluate_candidate_parallel)
      # handle budget as a guardrail. Just log and continue.
      cat("Budget below minimum for PPG", ppg, "- no dedicated repair loop (handled in main optimization).\n")
      next
      
    } #ELSEIF
    
  } #FOR LOOP 1
  
  # Get PPG's budget information
  
  budget_info_prom = prom_base[,.(Total_Trade_Investment_prom = sum(Total_Trade_Investment)), by = .(`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,
                                                                                                     FORMAT,PPG,PPG_Description)]
  
  budget_info_not_prom = not_prom[,.(Total_Trade_Investment_not_prom = sum(Total_Trade_Investment)), by = .(`SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`,
                                                                                                            FORMAT,PPG,PPG_Description)]
  
  budget_info = merge(budget_info_prom, budget_info_not_prom,  by = c("SECTOR 2","TRADING COMPANY","PRODUCT RANGE",
                                                                      "FORMAT","PPG","PPG_Description"), all.x = T)
  
  budget_info = merge(budget_info,budget_const, by = c("PRODUCT RANGE","FORMAT","PPG","PPG_Description"), all.x = T)
  budget_info[,"Total_Trade_Investment"] = budget_info$Total_Trade_Investment_prom + budget_info$Total_Trade_Investment_not_prom
  
  
  budget_info[,Status := ifelse(Total_Trade_Investment<Min_Investment,"Budget less than Minimum", ifelse(Total_Trade_Investment > Max_Investment,
                                                                                                         "Budget more than Maximum", "Budget Range Satisfied"))]
  
  
  #<================================================-MINIMUM BUDGET SATISFIED-====================================================>
  
  # Update base constraint check after budget loop (prom_base may have changed)
  # This ensures the main optimization loop uses the correct base state
  if(exists("check_base_con_before_budget")) {
    # Recalculate from current prom_base to ensure consistency
    base_sums_after_budget = list(
      Total_Sales = sum(prom_base$Total_Sales, na.rm = TRUE),
      GM_Abs = sum(prom_base$GM_Abs, na.rm = TRUE),
      BIP = sum(prom_base$BIP, na.rm = TRUE),
      Gross_Sales = sum(prom_base$Gross_Sales, na.rm = TRUE),
      Inc_GM_Abs = sum(prom_base$Inc_GM_Abs, na.rm = TRUE),
      Total_Trade_Investment = sum(prom_base$Total_Trade_Investment, na.rm = TRUE),
      Net_Revenue = sum(prom_base$Net_Revenue, na.rm = TRUE),
      NIS = sum(prom_base$NIS, na.rm = TRUE),
      R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc, na.rm = TRUE),
      R_GM_Inc = sum(prom_base$R_GM_Inc, na.rm = TRUE),
      R_NIS_Inc = sum(prom_base$R_NIS_Inc, na.rm = TRUE),
      R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc, na.rm = TRUE),
      Value_Sales = sum(prom_base$Value_Sales, na.rm = TRUE)
    )
    
    check_base_con_before_budget = constraint_fun(
      base_sums_after_budget$Total_Sales, base_sums_after_budget$GM_Abs, base_sums_after_budget$BIP,
      base_sums_after_budget$Gross_Sales, base_sums_after_budget$Inc_GM_Abs,
      base_sums_after_budget$Total_Trade_Investment, base_sums_after_budget$Net_Revenue,
      all_other_sales, base_sums_after_budget$NIS,
      not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
      not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
      exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
      exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
      con1, con2, con3, con4, con5, con6,
      con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
      con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
      goal, roi,
      base_sums_after_budget$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
      base_sums_after_budget$R_GM_Inc, exc_R_GM_Inc, base_sums_after_budget$R_NIS_Inc, exc_R_NIS_Inc,
      base_sums_after_budget$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
      exc_Value_Sales, not_prom_Value_Sales, base_sums_after_budget$Value_Sales, all_other_sales_value,
      scope = "PROMO_ONLY"
    )
  }
  
  # Sort events by ROI (highest first) to try best events first
  # But also ensure we iterate through ALL events, not just the first few
  events_base = events_base[order(events_base$ROI, decreasing = T),]
  prom_base = prom_base[order(prom_base$PPG, prom_base$Rank),]
  
  
  #events_base = events_base[PPG == "D77"]
  
  #####DEBUG: Event Selection Summary
  cat("\n========== EVENT SELECTION SUMMARY ==========\n")
  cat("Total events to iterate:", nrow(events_base), "\n")
  cat("Events by Display Flag:\n")
  if(nrow(events_base) > 0) {
    display_flag_summary = table(events_base$Display_Flag, useNA = "ifany")
    print(display_flag_summary)
  }
  cat("Top 10 events by ROI:\n")
  if(nrow(events_base) > 0) {
    print(head(events_base[, c("PPG", "ROI", "Display_Flag", "Discount", "Display","Flyer_Flag")], 10))
  }
  cat("==========================================\n\n")
  
  #If Stop_opt = 1, optimization will not run
  
  
  if(stop_opt != 1){
    
    # ===== PRE-COMPUTATION: Do once before loops for performance =====
    
    # 1. Create track_id lookup table (hash map) - O(1) lookup instead of O(n)
    track_id_lookup = split(prom_base$`Track ID`, prom_base$PPG)
    
    # 2. Create row index lookup for Track IDs - O(1) instead of which() every time
    track_id_to_row = setNames(seq_len(nrow(prom_base)), as.character(prom_base$`Track ID`))
    
    # 3. Pre-calculate base sums once (only recalculate when prom_base changes)
    base_sums_cache = list(
      Total_Sales = sum(prom_base$Total_Sales, na.rm = TRUE),
      GM_Abs = sum(prom_base$GM_Abs, na.rm = TRUE),
      BIP = sum(prom_base$BIP, na.rm = TRUE),
      Gross_Sales = sum(prom_base$Gross_Sales, na.rm = TRUE),
      Inc_GM_Abs = sum(prom_base$Inc_GM_Abs, na.rm = TRUE),
      Total_Trade_Investment = sum(prom_base$Total_Trade_Investment, na.rm = TRUE),
      Net_Revenue = sum(prom_base$Net_Revenue, na.rm = TRUE),
      NIS = sum(prom_base$NIS, na.rm = TRUE),
      R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc, na.rm = TRUE),
      R_GM_Inc = sum(prom_base$R_GM_Inc, na.rm = TRUE),
      R_NIS_Inc = sum(prom_base$R_NIS_Inc, na.rm = TRUE),
      R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc, na.rm = TRUE),
      Value_Sales = sum(prom_base$Value_Sales, na.rm = TRUE)
    )
    
    # 4. Pre-calculate base constraint check once (PROMO_ONLY scope)
    # CRITICAL: Initialize BEFORE evaluate_candidate_parallel function is defined
    # Use check_base_con_before_budget if it exists (from budget loop), otherwise calculate fresh
    if(exists("check_base_con_before_budget")) {
      check_base_con_cached = check_base_con_before_budget
    } else {
      check_base_con_cached = constraint_fun(
        base_sums_cache$Total_Sales, base_sums_cache$GM_Abs, base_sums_cache$BIP,
        base_sums_cache$Gross_Sales, base_sums_cache$Inc_GM_Abs,
        base_sums_cache$Total_Trade_Investment, base_sums_cache$Net_Revenue,
        all_other_sales, base_sums_cache$NIS,
        not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
        not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
        exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
        exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
        con1, con2, con3, con4, con5, con6,
        con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
        con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
        goal, roi,
        base_sums_cache$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
        base_sums_cache$R_GM_Inc, exc_R_GM_Inc, base_sums_cache$R_NIS_Inc, exc_R_NIS_Inc,
        base_sums_cache$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
        exc_Value_Sales, not_prom_Value_Sales, base_sums_cache$Value_Sales, all_other_sales_value,
        scope = "PROMO_ONLY"
      )
    }
    
    # Make opti_sign comparison case-insensitive (calculate once)
    opti_sign_upper = toupper(trimws(as.character(opti_sign)))
    
    # ===== PARALLEL PROCESSING SETUP =====
    cat("\n========== SETTING UP PARALLEL PROCESSING ==========\n")
    n_cores = min(detectCores() - 1, 4)  # Leave 1 core free, max 8 cores
    cat("Using", n_cores, "cores for parallel processing\n")
    
    cl = makeCluster(n_cores, outfile = "")
    registerDoParallel(cl)
    
    # Export all necessary variables and functions to workers
    clusterExport(cl, c(
      "prom_base", "events_base", "not_prom", "budget_const",
      "goal", "con1", "con2", "con3", "con4", "con5", "con6",
      "con1_min", "con1_max", "con2_min", "con2_max", "con3_min", "con3_max",
      "con4_min", "con4_max", "con5_min", "con5_max", "con6_min", "con6_max",
      "opti_sign_upper", "roi",
      "all_other_sales", "all_other_sales_value",
      "not_prom_Total_Sales", "not_prom_GM_Abs", "not_prom_BIP", 
      "not_prom_Gross_Sales", "not_prom_Net_Revenue", "not_prom_NIS", 
      "not_prom_Total_Trade_Investment", "not_prom_R_Trade_Inv_Inc",
      "exc_Total_Sales", "exc_GM_Abs", "exc_BIP", "exc_Gross_Sales", 
      "exc_Inc_GM", "exc_Total_Trade_Investment", "exc_Net_Revenue", 
      "exc_NIS", "exc_R_Trade_Inv_Inc", "exc_R_GM_Inc", "exc_R_NIS_Inc",
      "exc_R_Net_Rev_Inc", "exc_Value_Sales", "not_prom_Value_Sales",
      "base_sums_cache", "check_base_con_cached", "track_id_to_row"
    ), envir = environment())
    
    clusterEvalQ(cl, {
      library(data.table)
      source("do_calculation.R")
      source("prom_update_fun.R")
      source("constraint_fun.R")
      source("ppg_budget_check_fun.R")
      # Verify functions are loaded
      if(!exists("update_base")) stop("update_base not found")
      if(!exists("budget_check")) stop("budget_check not found")
      if(!exists("constraint_fun")) stop("constraint_fun not found")
      if(!exists("do_calculation")) stop("do_calculation not found")
    })
    
    # ===== HELPER FUNCTION FOR PARALLEL EVALUATION =====
    evaluate_candidate_parallel = function(event_data, track_id_val, row_idx_val) {
      debug_step = "start"
      tryCatch({
        debug_step = "convert_event"
        # Convert event data back to data.table row
        event = as.data.table(event_data)
        event
        debug_step = "get_row_info"
        # Get current row info from prom_base snapshot
        flag_check = as.numeric(prom_base[row_idx_val, "Flag_Check"])
        ROI = as.numeric(prom_base[row_idx_val, "ROI"])
        counter_check = as.numeric(prom_base[row_idx_val, "Flag_Check_Counter"])
        extra_slot_flag = as.numeric(prom_base[row_idx_val, "Extra_Slot_Flag"])
        ppg = as.character(event$PPG)
        
        debug_step = "extract_flags"
        # Check replacement eligibility
        # Get event flags (handle missing/NA values)
        
        names(event)
        event_display_flag = event$Display_Flag
        event_flyer_flag = event$Flyer_Flag
        event_roi = event$ROI
        debug_step = "sanitize_numeric"
        # Ensure all numeric values are not NA
        flag_check = ifelse(is.na(flag_check), 0, flag_check)
        ROI = ifelse(is.na(ROI), -Inf, ROI)
        counter_check = ifelse(is.na(counter_check), 0, counter_check)
        extra_slot_flag = ifelse(is.na(extra_slot_flag), 0, extra_slot_flag)
        
        
        debug_step = "replacement_check"
        replacement_flag = 0
        
        
        # Precompute helpers
        roi_ok        <- counter_check == 0 | ROI > event_roi
        extra_ok      <- extra_slot_flag == 0 | (extra_slot_flag == 1 & ROI > event_roi)
        
        discount_only <- event_display_flag == 0 & event_flyer_flag == 0
        display_only  <- event_display_flag == 1 & event_flyer_flag == 0
        flyer_only    <- event_flyer_flag  == 1 & event_display_flag == 0
        both          <- event_display_flag == 1 & event_flyer_flag == 1
        
        
        replacement_flag <- FALSE
        reason <- NULL
        
        
        # -------- DISCOUNT ONLY --------
        if(discount_only && flag_check %in% c(0,2) && extra_ok && roi_ok){
          replacement_flag <- TRUE
          reason <- "discount"
        }
        
        # -------- DISPLAY ONLY --------
        if(!replacement_flag &&
           display_only &&
           (
             (flag_check == 1 && ROI > event_roi) ||
             (flag_check == 2 && roi_ok)
           )
        ){
          replacement_flag <- TRUE
          reason <- "display"
        }
        
        # -------- FLYER ONLY --------
        if(!replacement_flag &&
           flyer_only &&
           flag_check %in% c(4,2) &&
           extra_ok &&
           roi_ok
        ){
          replacement_flag <- TRUE
          reason <- "flyer"
        }
        
        # -------- DISPLAY + FLYER --------
        if(!replacement_flag &&
           both &&
           flag_check %in% c(3,2) &&
           extra_ok &&
           roi_ok
        ){
          replacement_flag <- TRUE
          reason <- "display+flyer"
        }
        
        
        # -------- DEBUG OUTPUT --------
        if(replacement_flag){
          cat("DEBUG: Set replacement_flag=1 (", reason, ")\n")
        }
        
        
        # Reject if not eligible
        if (!replacement_flag) {
          return(list(accepted = FALSE, reason = "not_eligible"))
        }
        
        # Create update
        prom_update <- copy(prom_base)
        prom_update <- update_base(prom_update, event, row_idx_val)
        debug_step = "budget_check"
        
        # Budget check
        budget_check_result = budget_check(prom_update, not_prom, ppg, budget_const)
        budget_flag = ifelse(budget_check_result[[1]] %in% c("below", "between"), 1, 0)
        
        cat("DEBUG: budget_flag =", budget_flag, "\n")
        
        if(budget_flag == 0) {
          return(list(accepted = FALSE, reason = "budget_exceeded"))
        }
        
        debug_step = "calculate_sums"
        # Calculate sums
        update_sums = list(
          Total_Sales = sum(prom_update$Total_Sales, na.rm = TRUE),
          GM_Abs = sum(prom_update$GM_Abs, na.rm = TRUE),
          BIP = sum(prom_update$BIP, na.rm = TRUE),
          Gross_Sales = sum(prom_update$Gross_Sales, na.rm = TRUE),
          Inc_GM_Abs = sum(prom_update$Inc_GM_Abs, na.rm = TRUE),
          Total_Trade_Investment = sum(prom_update$Total_Trade_Investment, na.rm = TRUE),
          Net_Revenue = sum(prom_update$Net_Revenue, na.rm = TRUE),
          NIS = sum(prom_update$NIS, na.rm = TRUE),
          R_Trade_Inv_Inc = sum(prom_update$R_Trade_Inv_Inc, na.rm = TRUE),
          R_GM_Inc = sum(prom_update$R_GM_Inc, na.rm = TRUE),
          R_NIS_Inc = sum(prom_update$R_NIS_Inc, na.rm = TRUE),
          R_Net_Rev_Inc = sum(prom_update$R_Net_Rev_Inc, na.rm = TRUE),
          Value_Sales = sum(prom_update$Value_Sales, na.rm = TRUE)
        )
        
        debug_step = "constraint_check"
        # Constraint check (PROMO_ONLY scope)
        check_update_con = constraint_fun(
          update_sums$Total_Sales, update_sums$GM_Abs, update_sums$BIP,
          update_sums$Gross_Sales, update_sums$Inc_GM_Abs,
          update_sums$Total_Trade_Investment, update_sums$Net_Revenue,
          all_other_sales, update_sums$NIS,
          not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
          not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
          exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
          exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
          con1, con2, con3, con4, con5, con6,
          con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
          con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
          goal, roi,
          update_sums$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
          update_sums$R_GM_Inc, exc_R_GM_Inc, update_sums$R_NIS_Inc, exc_R_NIS_Inc,
          update_sums$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
          exc_Value_Sales, not_prom_Value_Sales, update_sums$Value_Sales, all_other_sales_value,
          scope = "PROMO_ONLY"
        )
        
        check_base_con = check_base_con_cached
        
        # Goal flag check
        base_goal_val = tryCatch(check_base_con[[5]][[1]], error = function(e) check_base_con[[5]])
        update_goal_val = tryCatch(check_update_con[[5]][[1]], error = function(e) check_update_con[[5]])
        
        if(is.na(base_goal_val)) base_goal_val = 0
        if(is.na(update_goal_val)) update_goal_val = 0
        
        goal_flag = ifelse(opti_sign_upper %in% c("MAX", "MAXIMIZE") & (base_goal_val <= update_goal_val), 1,
                           ifelse(opti_sign_upper %in% c("MIN", "MINIMIZE") & (base_goal_val >= update_goal_val), 1, 0))
        
        # Goal safeguard
        goal_safeguard = 1
        if(opti_sign_upper %in% c("MAX", "MAXIMIZE")) {
          if(base_goal_val > 0 && update_goal_val < 0) {
            goal_safeguard = 0
          } else if(base_goal_val > 0 && update_goal_val < (base_goal_val * 0.5)) {
            goal_safeguard = 0
          } else if(base_goal_val <= 0 && update_goal_val < (base_goal_val * 1.5)) {
            goal_safeguard = 0
          }
        } else {
          if(base_goal_val < 0 && update_goal_val > 0) {
            goal_safeguard = 0
          } else if(base_goal_val < 0 && update_goal_val > (base_goal_val * 0.5)) {
            goal_safeguard = 0
          } else if(base_goal_val >= 0 && update_goal_val > (base_goal_val * 1.5)) {
            goal_safeguard = 0
          }
        }
        
        # GM safeguard
        base_gm_percent = tryCatch({
          base_kpi = check_base_con[[6]]
          if(!is.null(base_kpi) && "GM % NR" %in% names(base_kpi)) {
            base_kpi[["GM % NR"]]
          } else { NA }
        }, error = function(e) NA)
        
        update_gm_percent = tryCatch({
          update_kpi = check_update_con[[6]]
          if(!is.null(update_kpi) && "GM % NR" %in% names(update_kpi)) {
            update_kpi[["GM % NR"]]
          } else { NA }
        }, error = function(e) NA)
        
        gm_safeguard = 1
        # IMPORTANT: GM safeguard is only meaningful when constraint 1 is GM % NR (percent).
        # If constraint 1 is something else (e.g., Gross Margin absolute), don't apply this rule.
        if(identical(as.character(con1), "GM_percent_model") && !is.na(con1_min) && con1_min > 0) {
          if(!is.na(update_gm_percent) && update_gm_percent < 0) {
            gm_safeguard = 0
          } else if(!is.na(base_gm_percent) && !is.na(update_gm_percent)) {
            if(base_gm_percent >= con1_min && update_gm_percent < 0) {
              gm_safeguard = 0
            }
            if(base_gm_percent < 0 && update_gm_percent < base_gm_percent * 1.5) {
              gm_safeguard = 0
            }
          }
        }
        
        # =========================
        # STRICT PRIORITY CONSTRAINTS
        # =========================
        # Enforce constraints in the user-defined order:
        # - Find the first violated constraint in the current/base plan
        # - Only accept candidates that make ALL constraints up to that priority satisfied
        # This prevents returning plans that still violate Priority 1 (e.g., GM absolute < minimum).
        base_flags = suppressWarnings(as.numeric(unlist(check_base_con[[1]])))
        update_flags = suppressWarnings(as.numeric(unlist(check_update_con[[1]])))
        base_flags[is.na(base_flags)] = 0
        update_flags[is.na(update_flags)] = 0
        
        first_violated = which(base_flags == 0)[1]
        if(is.na(first_violated)) first_violated = 7  # all 6 constraints satisfied in base
        
        required_idx = if(first_violated >= 1 && first_violated <= 6) 1:first_violated else 1:6
        required_ok = all(update_flags[required_idx] == 1)
        
        final_flag = as.numeric(required_ok) * goal_safeguard * gm_safeguard
        # If all constraints are already satisfied, require goal to move in the right direction too
        if(first_violated == 7) {
          final_flag = final_flag * goal_flag
        }
        
        if(final_flag == 1) {
          return(list(
            accepted = TRUE,
            score = update_goal_val,
            event = event,
            row_idx = row_idx_val,
            track_id = track_id_val,
            update_sums = update_sums,
            check_update_con = check_update_con,
            replacement_flag = replacement_flag,
            budget_flag = budget_flag
          ))
        } else {
          return(list(accepted = FALSE, reason = "constraints_not_satisfied"))
        }
        
      }, error = function(e) {
        cat("DEBUG: ERROR in evaluate_candidate_parallel at step:", debug_step, "| Error:", e$message, "\n")
        return(list(accepted = FALSE, reason = paste("error:", e$message)))
      })
    }
    
    # Export helper function (functions are already loaded in workers via clusterEvalQ above)
    clusterExport(cl, "evaluate_candidate_parallel", envir = environment())
    
    # ===== GENERATE ALL CANDIDATE COMBINATIONS =====
    cat("Generating candidate combinations...\n")
    all_candidates = list()
    
    for(i in 1:nrow(events_base)) {
      event = events_base[i,]
      track_ids = track_id_lookup[[event$PPG]]
      if(is.null(track_ids) || length(track_ids) == 0) next
      
      for(track_id in track_ids) {
        row_idx = track_id_to_row[[as.character(track_id)]]
        if(is.null(row_idx)) next
        
        all_candidates[[length(all_candidates) + 1]] = list(
          event_idx = i,
          track_id = track_id,
          row_idx = row_idx
        )
      }
    }
    
    cat("Total candidates to evaluate:", length(all_candidates), "\n")
    cat("==========================================\n\n")
    
    # ===== PARALLEL EVALUATION =====
    cat("Evaluating candidates in parallel...\n")
    
    # Process in batches for better memory management
    batch_size = 100000
    all_results = list()
    # Note: j is already initialized at line 770 for iteration tracking
    
    for(batch_start in seq(1, length(all_candidates), by = batch_size)) {
      batch_end = min(batch_start + batch_size - 1, length(all_candidates))
      batch_candidates = all_candidates[batch_start:batch_end]
      
      batch_results = foreach(i = 1:length(batch_candidates),
                              .combine = c,
                              .packages = c("data.table"),
                              .errorhandling = "pass") %dopar% {
                                
                                tryCatch({
                                  combo = batch_candidates[[i]]
                                  event = events_base[combo$event_idx,]
                                  
                                  result = evaluate_candidate_parallel(
                                    as.list(event), combo$track_id, combo$row_idx
                                  )
                                  
                                  # Always return the result (even if rejected) so we can see rejection reasons
                                  if(!is.null(result)) {
                                    return(list(result))
                                  } else {
                                    return(list(list(accepted = FALSE, reason = "null_result")))
                                  }
                                }, error = function(e) {
                                  # Return error info for debugging
                                  return(list(list(accepted = FALSE, reason = paste("parallel_error:", e$message))))
                                })
                              }
      
      all_results = c(all_results, batch_results)
      
      
      # Debug: Check batch results
      if(length(batch_results) > 0) {
        non_null_count = sum(!sapply(batch_results, is.null))
        cat(sprintf("Batch %d-%d: %d results, %d non-null\n", 
                    batch_start, batch_end, length(batch_results), non_null_count))
      }
      
      if(progress && batch_end %% 1000 == 0) {
        cat(sprintf("Processed %d/%d candidates...\n", batch_end, length(all_candidates)))
      }
    }
    
    stopCluster(cl)
    cat("Parallel evaluation complete.\n")
    cat("Total results collected:", length(all_results), "\n\n")
    
    # ===== FILTER AND SORT ACCEPTED RESULTS =====
    # Filter out NULL results and error results
    accepted_results = all_results[!sapply(all_results, is.null)]
    cat("Non-null results:", length(accepted_results), "\n")
    
    accepted_results = accepted_results[sapply(accepted_results, function(x) {
      !is.null(x) && is.list(x) && !is.null(x$accepted) && x$accepted == TRUE
    })]
    
    # Debug: Count rejection reasons
    if(length(all_results) > 0) {
      rejection_reasons = sapply(all_results, function(x) {
        if(is.null(x) || !is.list(x) || is.null(x$accepted)) return("unknown")
        if(x$accepted) return("accepted")
        if(!is.null(x$reason)) return(x$reason)
        return("unknown")
      })
      rejection_summary = table(rejection_reasons)
      cat("Rejection summary:\n")
      print(rejection_summary)
      cat("\n")
    }
    
    if(length(accepted_results) > 0) {
      # Sort by score (best first)
      scores = sapply(accepted_results, function(x) x$score)
      accepted_results = accepted_results[order(scores, decreasing = (opti_sign_upper == "MAX"))]
      
      cat("Found", length(accepted_results), "accepted candidates. Applying updates...\n")
      
      # ===== APPLY UPDATES SEQUENTIALLY =====
      successful_updates_per_event = rep(0, nrow(events_base))
      names(successful_updates_per_event) = seq_len(nrow(events_base))
      
      applied_count = 0
      # j is already initialized at line 770, we'll use it to track applied updates
      # If j hasn't been incremented yet, it will start at 0
      
      for(result in accepted_results) {
        # CRITICAL: Recalculate base constraint check from CURRENT prom_base before re-checking.
        # The parallel stage evaluated against the initial base snapshot, which becomes stale
        # once we start applying updates. This ensures we're comparing against the actual current state.
        base_sums_cache = list(
          Total_Sales = sum(prom_base$Total_Sales, na.rm = TRUE),
          GM_Abs = sum(prom_base$GM_Abs, na.rm = TRUE),
          BIP = sum(prom_base$BIP, na.rm = TRUE),
          Gross_Sales = sum(prom_base$Gross_Sales, na.rm = TRUE),
          Inc_GM_Abs = sum(prom_base$Inc_GM_Abs, na.rm = TRUE),
          Total_Trade_Investment = sum(prom_base$Total_Trade_Investment, na.rm = TRUE),
          Net_Revenue = sum(prom_base$Net_Revenue, na.rm = TRUE),
          NIS = sum(prom_base$NIS, na.rm = TRUE),
          R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc, na.rm = TRUE),
          R_GM_Inc = sum(prom_base$R_GM_Inc, na.rm = TRUE),
          R_NIS_Inc = sum(prom_base$R_NIS_Inc, na.rm = TRUE),
          R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc, na.rm = TRUE),
          Value_Sales = sum(prom_base$Value_Sales, na.rm = TRUE)
        )
        
        check_base_con_cached = constraint_fun(
          base_sums_cache$Total_Sales, base_sums_cache$GM_Abs, base_sums_cache$BIP,
          base_sums_cache$Gross_Sales, base_sums_cache$Inc_GM_Abs,
          base_sums_cache$Total_Trade_Investment, base_sums_cache$Net_Revenue,
          all_other_sales, base_sums_cache$NIS,
          not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
          not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
          exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
          exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
          con1, con2, con3, con4, con5, con6,
          con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
          con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
          goal, roi,
          base_sums_cache$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
          base_sums_cache$R_GM_Inc, exc_R_GM_Inc, base_sums_cache$R_NIS_Inc, exc_R_NIS_Inc,
          base_sums_cache$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
          exc_Value_Sales, not_prom_Value_Sales, base_sums_cache$Value_Sales, all_other_sales_value,
          scope = "PROMO_ONLY"
        )
        
        # Re-evaluate candidate against CURRENT prom_base before applying.
        # The parallel stage evaluated against the initial base snapshot, which can become stale
        # once we start applying updates. This re-check prevents drifting into constraint violations.
        
        # Ensure event has all required columns before recheck
        if(!"Flyer_Flag" %in% names(result$event)) {
          result$event$Flyer_Flag = 0
        }
        if(!"Flyer" %in% names(result$event)) {
          result$event$Flyer = NA
        }
        if(!"Flyer_Cost" %in% names(result$event)) {
          result$event$Flyer_Cost = 0
        }
        
        recheck = evaluate_candidate_parallel(as.list(result$event), result$track_id, result$row_idx)
        if(is.null(recheck) || is.null(recheck$accepted) || !isTRUE(recheck$accepted)) {
          # Debug: log why recheck failed
          # Safely extract PPG value
          event_ppg = tryCatch({
            if(is.list(result$event) && "PPG" %in% names(result$event)) {
              as.character(result$event$PPG)[1]
            } else if(is.data.frame(result$event) && "PPG" %in% names(result$event)) {
              as.character(result$event$PPG[1])
            } else {
              "unknown"
            }
          }, error = function(e) "unknown")
          
          if(!is.null(recheck) && !is.null(recheck$reason)) {
            cat("Recheck failed - Event:", event_ppg, "| Reason:", recheck$reason, "\n")
          } else if(is.null(recheck)) {
            cat("Recheck returned NULL for event:", event_ppg, "\n")
          }
          next
        }
        
        j = j + 1  # Increment iteration counter ONLY when we actually apply an update
        
        replacement_flag = ifelse(!is.null(recheck$replacement_flag), recheck$replacement_flag, 1)
        budget_flag = ifelse(!is.null(recheck$budget_flag), recheck$budget_flag, 1)
        update_flag = replacement_flag * budget_flag
        final_flag = 1
        
        # Store iteration tracking
        if(j <= length(replace_flag_iter)) {
          replace_flag_iter[j] = replacement_flag
          update_flag_iter[j] = update_flag
          final_flag_iter[j] = final_flag
          check_base_con_iter[[j]] = check_base_con_cached
          check_update_con_iter[[j]] = recheck$check_update_con
        }
        
        # Apply update (using the accepted candidate from current-state recheck)
        prom_base = update_base(prom_base, recheck$event, recheck$row_idx)
        
        # Update cache with CURRENT accepted update results
        base_sums_cache = recheck$update_sums
        check_base_con_cached = recheck$check_update_con
        track_id_to_row = setNames(seq_len(nrow(prom_base)), as.character(prom_base$`Track ID`))
        
        # Track successful update
        event_idx = which(events_base$PPG == recheck$event$PPG & 
                            events_base$ROI == recheck$event$ROI &
                            events_base$Display_Flag == recheck$event$Display_Flag)[1]
        if(!is.na(event_idx)) {
          successful_updates_per_event[event_idx] = successful_updates_per_event[event_idx] + 1
        }
        
        applied_count = applied_count + 1
        if(applied_count >= 5000) break
      }
      
      cat("Applied", applied_count, "updates to prom_base\n")
      cat("Total iterations tracked:", j, "\n")
    } else {
      cat("No accepted candidates found\n")
    }
    
    # Note: successful_updates_per_event is already initialized above, no need to reinitialize
    
    # Track consecutive EVENT failures (not iteration failures) to detect if we're stuck
    consecutive_event_failures = 0
    max_consecutive_event_failures = 10000  # If 20 consecutive events fail, we might be done
    # write.csv(all_results,"all.csv")
    if(FALSE)  
      
      # ORIGINAL SEQUENTIAL LOOP (commented out - replaced by parallel processing above)
      # for( i in 1:nrow(events_base)){   #------------------------------------------------------------FOR LOOP 1
      
      # ORIGINAL NESTED LOOPS REPLACED BY PARALLEL PROCESSING ABOVE
      # The parallel processing evaluates all candidates and applies the best ones sequentially
      # This preserves all constraint logic, goal optimization, and safeguards
      
      # OLD CODE (commented out - replaced by parallel processing):
      # if(stop_opt == 1) break  # Early exit if optimization stopped
      # 
      # #Extract event (try ALL events, not just best ROI)
      # event = events_base[i,]
      # 
      # # Track if this event resulted in any successful updates (reset for each event)
      # event_successful_updates = 0
      # 
      # # Fast lookup using pre-computed hash map
      # track_id = track_id_lookup[[event$PPG]]
      # if(is.null(track_id) || length(track_id) == 0) next
      # 
      # progress_total = (nrow(events_base) * length(track_id))
      # 
      # #Loop over each track id
      # for(id in track_id){    #---------------------------------------------------------------------FOR LOOP 2
      
      # OLD NESTED LOOP CODE - COMMENTED OUT (replaced by parallel processing above)
      # The code below is kept for reference but is no longer executed
      
      # replacement_flag = 0
      # final_flag = 0
      # con_satis_flag = 0
      # j = j+1
      # #print(j)
      # if(progress) incProgress((1/progress_total),detail = paste("Iteration", j))
      # 
      # # Fast row lookup using pre-computed index
      # row_to_change = track_id_to_row[[as.character(id)]]
      # if(is.null(row_to_change)) {
      #   replace_flag_iter[j] = 0
      #   update_flag_iter[j] = 0
      #   final_flag_iter[j] = 0
      #   next
      # }
      # 
      # #Get Info if it is discount only slot, display only slot or both discount/display slot
      # # Read from prom_base directly (no need for prom_update yet)
      # flag_check = as.numeric(prom_base[row_to_change,"Flag_Check"])
      # ROI = as.numeric(prom_base[row_to_change,"ROI"])
      # counter_check = as.numeric(prom_base[row_to_change,"Flag_Check_Counter"])
      # extra_slot_flag  = as.numeric(prom_base[row_to_change,"Extra_Slot_Flag"])
      # ppg = as.character(event$PPG)
      
      # OLD CODE CONTINUED - ALL COMMENTED OUT (replaced by parallel processing)
      # #Update slot in prom_update based on criteria's - Vectorized check
      # #1. All possibilities where display_flag = 0
      # if(event$Display_Flag == 0 &  flag_check == 0 & extra_slot_flag == 0 ){
      #   replacement_flag = 1
      # } else if(event$Display_Flag == 0 &  flag_check == 0 & extra_slot_flag == 1 & ROI > event$ROI ){
      #   replacement_flag = 1
      # } else if(event$Display_Flag == 0 & flag_check == 2 & counter_check == 0){
      #   replacement_flag = 1
      # } else if(event$Display_Flag == 0 & flag_check == 2 & counter_check == 1 & ROI > event$ROI){
      #   replacement_flag = 1
      # } else if(event$Display_Flag == 1 &  flag_check == 1 & ROI > event$ROI ){
      #   replacement_flag = 1
      # } else if(event$Display_Flag == 1 & flag_check == 2 & counter_check == 0){
      #   replacement_flag = 1
      # } else if(event$Display_Flag == 1 & flag_check == 2 & counter_check == 1 & ROI > event$ROI){
      #   replacement_flag = 1
      # }
      # 
      # # Early exit if no replacement needed - skip expensive operations
      # if(replacement_flag == 0) {
      #   replace_flag_iter[j] = 0
      #   update_flag_iter[j] = 0
      #   final_flag_iter[j] = 0
      #   next
      # }
      # 
      # # Only create copy when replacement is needed
      # prom_update = copy(prom_base)  # Use data.table::copy() for efficiency
      # prom_update = update_base(prom_update, event, row_to_change)
      # 
      #     # Only check budget if replacement happened
      #     if(replacement_flag == 1) {
      #       budget_check_result = budget_check(prom_update, not_prom, ppg, budget_const)
      #       # Accept updates when budget is "below" (need to add more) or "between" (within range)
      #       # Only reject when "above" (exceeds maximum)
      #       budget_flag = ifelse(budget_check_result[[1]] %in% c("below", "between"), 1, 0)
      #       
      #       # Debug: Log budget rejections (first 10 and then every 100th)
      #       if(budget_flag == 0 && (j <= 10 || j %% 100 == 0)) {
      #         cat(sprintf("Budget rejection at iteration %d: Status=%s, Investment=%.2f, Min=%.2f, Max=%.2f\n",
      #                     j, budget_check_result[[1]], budget_check_result$Investment_ppg,
      #                     budget_check_result$Min_Inv_Req, budget_check_result$Max_Inv_Req))
      #       }
      #     } else {
      #       budget_flag = 0
      #     }
      #     
      #     replace_flag_iter[j] = replacement_flag
      #     #print(replacement_flag)
      #     
      #     update_flag = replacement_flag * budget_flag
      #     
      #     update_flag_iter[j] = update_flag
      #     #============================================ROI PLACEMENT DONE==================================================#
      #     
      #     # Early exit if budget constraint fails
      #     if(update_flag == 0) {
      #       final_flag_iter[j] = 0
      #       next
      #     }
      #     
      #     #Run this loop only if budget constraint is okay and ROI was replaced
      #     
      #     if( update_flag == 1 ){   #IF1
      #       
      #       # ===== OPTIMIZED SUM CALCULATION =====
    #       # Only calculate sums when update_flag == 1
    #       # Use na.rm = TRUE for robustness
    #       update_sums = list(
    #         Total_Sales = sum(prom_update$Total_Sales, na.rm = TRUE),
    #         GM_Abs = sum(prom_update$GM_Abs, na.rm = TRUE),
    #         BIP = sum(prom_update$BIP, na.rm = TRUE),
    #         Gross_Sales = sum(prom_update$Gross_Sales, na.rm = TRUE),
    #         Inc_GM_Abs = sum(prom_update$Inc_GM_Abs, na.rm = TRUE),
    #         Total_Trade_Investment = sum(prom_update$Total_Trade_Investment, na.rm = TRUE),
    #         Net_Revenue = sum(prom_update$Net_Revenue, na.rm = TRUE),
    #         NIS = sum(prom_update$NIS, na.rm = TRUE),
    #         R_Trade_Inv_Inc = sum(prom_update$R_Trade_Inv_Inc, na.rm = TRUE),
    #         R_GM_Inc = sum(prom_update$R_GM_Inc, na.rm = TRUE),
    #         R_NIS_Inc = sum(prom_update$R_NIS_Inc, na.rm = TRUE),
    #         R_Net_Rev_Inc = sum(prom_update$R_Net_Rev_Inc, na.rm = TRUE),
    #         Value_Sales = sum(prom_update$Value_Sales, na.rm = TRUE)
    #       )
    #       
    #       # Constraint check only when needed - use cached base sums
    #       check_update_con = constraint_fun(
    #         update_sums$Total_Sales, update_sums$GM_Abs, update_sums$BIP,
    #         update_sums$Gross_Sales, update_sums$Inc_GM_Abs,
    #         update_sums$Total_Trade_Investment, update_sums$Net_Revenue,
    #         all_other_sales, update_sums$NIS,
    #         not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
    #         not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
    #         exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
    #         exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
    #         con1, con2, con3, con4, con5, con6,
    #         con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
    #         con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
    #         goal, roi,
    #         update_sums$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
    #         update_sums$R_GM_Inc, exc_R_GM_Inc, update_sums$R_NIS_Inc, exc_R_NIS_Inc,
    #         update_sums$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
    #         exc_Value_Sales, not_prom_Value_Sales, update_sums$Value_Sales, all_other_sales_value
    #       )
    #       
    #       # Use cached base constraint check (recalculate only when prom_base changes)
    #       check_base_con = check_base_con_cached
    #       
    #       #0. Goal Flag - making sure goal always moves in right direction - always multiplied in final_flag
    #       # opti_sign_upper already calculated above
    #       
    #           #       base_goal_val = tryCatch(check_base_con[[5]][[1]], error = function(e) check_base_con[[5]])
    #       update_goal_val = tryCatch(check_update_con[[5]][[1]], error = function(e) check_update_con[[5]])
    #       
    #       # Handle NA values
    #       if(is.na(base_goal_val)) base_goal_val = 0
    #       if(is.na(update_goal_val)) update_goal_val = 0
    #       
    #       goal_flag = ifelse( opti_sign_upper %in% c("MAX", "MAXIMIZE") &  (base_goal_val <= update_goal_val),1,
    #                           ifelse(opti_sign_upper %in% c("MIN", "MINIMIZE") &  (base_goal_val >= update_goal_val),1,0))
    #       
    #       # GOAL SAFEGUARD: Prevent goal from going negative when it was positive, or getting significantly worse
    #       # Reject updates that make goal negative when base was positive (unless it's a small negative)
    #       goal_safeguard = 1
    #       if(opti_sign_upper %in% c("MAX", "MAXIMIZE")) {
    #         # For maximize: reject if goal goes from positive to negative, or if it decreases by more than 50%
    #         if(base_goal_val > 0 && update_goal_val < 0) {
    #           goal_safeguard = 0  # Reject: going from positive to negative
    #         } else if(base_goal_val > 0 && update_goal_val < (base_goal_val * 0.5)) {
    #           goal_safeguard = 0  # Reject: goal decreased by more than 50%
    #         } else if(base_goal_val <= 0 && update_goal_val < (base_goal_val * 1.5)) {
    #           goal_safeguard = 0  # Reject: if already negative, don't make it much worse (50% worse)
    #         }
    #       } else {
    #         # For minimize: reject if goal goes from negative to positive, or if it increases by more than 50%
    #         if(base_goal_val < 0 && update_goal_val > 0) {
    #           goal_safeguard = 0  # Reject: going from negative to positive
    #         } else if(base_goal_val < 0 && update_goal_val > (base_goal_val * 0.5)) {
    #           goal_safeguard = 0  # Reject: goal increased by more than 50% (less negative)
    #         } else if(base_goal_val >= 0 && update_goal_val > (base_goal_val * 1.5)) {
    #           goal_safeguard = 0  # Reject: if already positive, don't make it much worse (50% worse)
    #         }
    #       }
    #       
    #       # CONSTRAINT 1 (GM % NR) SAFEGUARD: Prevent GM % NR from going/staying negative if it has a positive minimum
    #       # Extract GM % NR from constraint check (it's in the KPI dataframe)
    #       base_gm_percent = tryCatch({
    #         base_kpi = check_base_con[[6]]
    #         if(!is.null(base_kpi) && "GM % NR" %in% names(base_kpi)) {
    #           base_kpi[["GM % NR"]]
    #         } else {
    #           NA
    #         }
    #       }, error = function(e) NA)
    #       
    #       update_gm_percent = tryCatch({
    #         update_kpi = check_update_con[[6]]
    #         if(!is.null(update_kpi) && "GM % NR" %in% names(update_kpi)) {
    #           update_kpi[["GM % NR"]]
    #         } else {
    #           NA
    #         }
    #       }, error = function(e) NA)
    #       
    #       gm_safeguard = 1
    #       if(!is.na(con1_min) && con1_min > 0) {
    #         # If GM % NR has a positive minimum constraint, reject updates that make it negative
    #         if(!is.na(update_gm_percent) && update_gm_percent < 0) {
    #           gm_safeguard = 0  # Reject: GM % NR is negative when minimum is positive
    #         } else if(!is.na(base_gm_percent) && !is.na(update_gm_percent)) {
    #           # If base was positive or at least above minimum, don't let it go negative
    #           if(base_gm_percent >= con1_min && update_gm_percent < 0) {
    #             gm_safeguard = 0  # Reject: going from acceptable to negative
    #           }
    #           # If both are negative but update is worse (more negative), reject if it's significantly worse
    #           if(base_gm_percent < 0 && update_gm_percent < base_gm_percent * 1.5) {
    #             gm_safeguard = 0  # Reject: making negative GM % NR significantly worse
    #           }
    #         }
    #       }
    #       
    #       # All constraint priority logic (1-7) would go here - commented out as it's in parallel function
    #       
    #     } #IF1
    #     
    #     if(final_flag == 1){
    #       prom_base = prom_update
    #       # Update cache when prom_base changes
    #       base_sums_cache = update_sums
    #       check_base_con_cached = check_update_con
    #       # Update row index lookup if Track IDs changed (unlikely but safe)
    #       track_id_to_row = setNames(seq_len(nrow(prom_base)), as.character(prom_base$`Track ID`))
    #       
    #       # Track successful update for this event
    #       event_successful_updates = event_successful_updates + 1
    #       # Don't reset consecutive_event_failures here - reset it at event level after loop
    #     }
    #     
    #     final_flag_iter[j] = final_flag
    #     
    #     # Store iteration results (only when needed for debugging/tracking)
    #     # Store every iteration for first 1000, then every 1000th iteration to save memory
    #     if(j <= 1000 || j %% 1000 == 0) {
    #       if(update_flag == 1) {
    #         # Use already calculated values if available
    #         check_update_con_iter[[j]] = check_update_con
    #         check_base_con_iter[[j]] = check_base_con
    #       } else {
    #         # Calculate only if not already done (shouldn't happen often)
    #         update_sums_iter = list(
    #           Total_Sales = sum(prom_update$Total_Sales, na.rm = TRUE),
    #           GM_Abs = sum(prom_update$GM_Abs, na.rm = TRUE),
    #           BIP = sum(prom_update$BIP, na.rm = TRUE),
    #           Gross_Sales = sum(prom_update$Gross_Sales, na.rm = TRUE),
    #           Inc_GM_Abs = sum(prom_update$Inc_GM_Abs, na.rm = TRUE),
    #           Total_Trade_Investment = sum(prom_update$Total_Trade_Investment, na.rm = TRUE),
    #           Net_Revenue = sum(prom_update$Net_Revenue, na.rm = TRUE),
    #           NIS = sum(prom_update$NIS, na.rm = TRUE),
    #           R_Trade_Inv_Inc = sum(prom_update$R_Trade_Inv_Inc, na.rm = TRUE),
    #           R_GM_Inc = sum(prom_update$R_GM_Inc, na.rm = TRUE),
    #           R_NIS_Inc = sum(prom_update$R_NIS_Inc, na.rm = TRUE),
    #           R_Net_Rev_Inc = sum(prom_update$R_Net_Rev_Inc, na.rm = TRUE),
    #           Value_Sales = sum(prom_update$Value_Sales, na.rm = TRUE)
    #         )
    #         check_update_con_iter[[j]] = constraint_fun(
    #           update_sums_iter$Total_Sales, update_sums_iter$GM_Abs, update_sums_iter$BIP,
    #           update_sums_iter$Gross_Sales, update_sums_iter$Inc_GM_Abs,
    #           update_sums_iter$Total_Trade_Investment, update_sums_iter$Net_Revenue,
    #           all_other_sales, update_sums_iter$NIS,
    #           not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
    #           not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
    #           exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
    #           exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
    #           con1, con2, con3, con4, con5, con6,
    #           con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
    #           con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
    #           goal, roi,
    #           update_sums_iter$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
    #           update_sums_iter$R_GM_Inc, exc_R_GM_Inc, update_sums_iter$R_NIS_Inc, exc_R_NIS_Inc,
    #           update_sums_iter$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
    #           exc_Value_Sales, not_prom_Value_Sales, update_sums_iter$Value_Sales, all_other_sales_value
    #         )
    #         check_base_con_iter[[j]] = check_base_con_cached
    #       }
    #       my_check[[j]] = c(event$PPG, event$ROI)
    #     }
    #     
    #     #j= j+1
    #     
    #     
    # } # FOR LOOP 2 (commented out - replaced by parallel processing)
    # 
    # # Track successful updates for this event
    # successful_updates_per_event[i] = event_successful_updates
    # 
    # # Reset consecutive event failures if this event had any successful updates
    # if(event_successful_updates > 0) {
    #   consecutive_event_failures = 0
    # } else {
    #   consecutive_event_failures = consecutive_event_failures + 1
    # }
    # 
    # # Progress reporting every 10 events
    # if(i %% 10 == 0 || i == nrow(events_base)) {
    #   cat(sprintf("Processed %d/%d events | Successful updates: %d | Consecutive event failures: %d\n", 
    #               i, nrow(events_base), sum(successful_updates_per_event[1:i]), consecutive_event_failures))
    # }
    # 
    # # REMOVED: Early exit logic - process ALL events to ensure comprehensive optimization
    # # The optimizer will now iterate through all events regardless of consecutive failures
    # # This ensures all possible improvements are explored
    # 
    # } # FOR LOOP 1 (commented out - replaced by parallel processing)
    
    #####DEBUG: Event Iteration Summary (updated for parallel processing)
    cat("\n========== PARALLEL OPTIMIZATION SUMMARY ==========\n")
    cat("Total events processed:", nrow(events_base), "\n")
    #  cat("Total successful updates:", sum(successful_updates_per_event), "\n")
    cat("Events with most updates:\n")
    #top_events = head(sort(successful_updates_per_event, decreasing = TRUE), 10)
    #if(length(top_events) > 0 && sum(top_events) > 0) {
    # top_event_indices = as.numeric(names(top_events[top_events > 0]))
    #if(length(top_event_indices) > 0) {
    # print(events_base[top_event_indices, c("PPG", "ROI", "Display_Flag", "Discount", "Display")])
    #}
    #}
    cat("==========================================\n\n")
    
  } #Stop_Opt
  
  # ===== FINAL STRICT CONSTRAINT VALIDATION =====
  # Do not return a plan that violates user-input constraints.
  # Ensure check_base_con_cached exists (initialize if it doesn't, e.g., if stop_opt == 1)
  if(!exists("check_base_con_cached")) {
    # Calculate from current prom_base
    base_sums_final = list(
      Total_Sales = sum(prom_base$Total_Sales, na.rm = TRUE),
      GM_Abs = sum(prom_base$GM_Abs, na.rm = TRUE),
      BIP = sum(prom_base$BIP, na.rm = TRUE),
      Gross_Sales = sum(prom_base$Gross_Sales, na.rm = TRUE),
      Inc_GM_Abs = sum(prom_base$Inc_GM_Abs, na.rm = TRUE),
      Total_Trade_Investment = sum(prom_base$Total_Trade_Investment, na.rm = TRUE),
      Net_Revenue = sum(prom_base$Net_Revenue, na.rm = TRUE),
      NIS = sum(prom_base$NIS, na.rm = TRUE),
      R_Trade_Inv_Inc = sum(prom_base$R_Trade_Inv_Inc, na.rm = TRUE),
      R_GM_Inc = sum(prom_base$R_GM_Inc, na.rm = TRUE),
      R_NIS_Inc = sum(prom_base$R_NIS_Inc, na.rm = TRUE),
      R_Net_Rev_Inc = sum(prom_base$R_Net_Rev_Inc, na.rm = TRUE),
      Value_Sales = sum(prom_base$Value_Sales, na.rm = TRUE)
    )
    
    check_base_con_cached = constraint_fun(
      base_sums_final$Total_Sales, base_sums_final$GM_Abs, base_sums_final$BIP,
      base_sums_final$Gross_Sales, base_sums_final$Inc_GM_Abs,
      base_sums_final$Total_Trade_Investment, base_sums_final$Net_Revenue,
      all_other_sales, base_sums_final$NIS,
      not_prom_Total_Sales, not_prom_GM_Abs, not_prom_BIP, not_prom_Gross_Sales,
      not_prom_Net_Revenue, not_prom_NIS, not_prom_Total_Trade_Investment,
      exc_Total_Sales, exc_GM_Abs, exc_BIP, exc_Gross_Sales, exc_Inc_GM,
      exc_Total_Trade_Investment, exc_Net_Revenue, exc_NIS,
      con1, con2, con3, con4, con5, con6,
      con1_min, con2_min, con3_min, con4_min, con5_min, con6_min,
      con1_max, con2_max, con3_max, con4_max, con5_max, con6_max,
      goal, roi,
      base_sums_final$R_Trade_Inv_Inc, exc_R_Trade_Inv_Inc, not_prom_R_Trade_Inv_Inc,
      base_sums_final$R_GM_Inc, exc_R_GM_Inc, base_sums_final$R_NIS_Inc, exc_R_NIS_Inc,
      base_sums_final$R_Net_Rev_Inc, exc_R_Net_Rev_Inc,
      exc_Value_Sales, not_prom_Value_Sales, base_sums_final$Value_Sales, all_other_sales_value,
      scope = "PROMO_ONLY"
    )
  }
  
  final_flags = suppressWarnings(as.numeric(unlist(check_base_con_cached[[1]])))
  final_flags[is.na(final_flags)] = 0
  if(length(final_flags) > 0 && any(final_flags == 0)) {
    # Helpful decomposition so users can see whether the issue is driven by promo vs non-promo vs excluded rows
    prom_gm = tryCatch(sum(prom_base$GM_Abs, na.rm = TRUE), error = function(e) NA_real_)
    not_prom_gm = tryCatch(sum(not_prom$GM_Abs, na.rm = TRUE), error = function(e) NA_real_)
    exc_gm = tryCatch(sum(exc_brand$GM_Abs, na.rm = TRUE), error = function(e) NA_real_)
    tot_gm = sum(c(prom_gm, not_prom_gm, exc_gm), na.rm = TRUE)
    
    prom_nr = tryCatch(sum(prom_base$Net_Revenue, na.rm = TRUE), error = function(e) NA_real_)
    not_prom_nr = tryCatch(sum(not_prom$Net_Revenue, na.rm = TRUE), error = function(e) NA_real_)
    exc_nr = tryCatch(sum(exc_brand$Net_Revenue, na.rm = TRUE), error = function(e) NA_real_)
    tot_nr = sum(c(prom_nr, not_prom_nr, exc_nr), na.rm = TRUE)
    
    tot_gm_percent = ifelse(!is.na(tot_gm) && !is.na(tot_nr) && tot_nr != 0, tot_gm * 100 / tot_nr, NA_real_)
    
    failed_idx = which(final_flags == 0)
    failed_names = tryCatch({
      as.character(shiny_const$KPI[failed_idx])
    }, error = function(e) paste0("Constraint_", failed_idx))
    failed_vals = tryCatch({
      as.numeric(unlist(check_base_con_cached[[2]]))[failed_idx]
    }, error = function(e) rep(NA_real_, length(failed_idx)))
    failed_mins = tryCatch({
      as.numeric(shiny_const$`Minimum Value`[failed_idx])
    }, error = function(e) rep(NA_real_, length(failed_idx)))
    failed_maxs = tryCatch({
      as.numeric(shiny_const$`Maximum Value`[failed_idx])
    }, error = function(e) rep(NA_real_, length(failed_idx)))
    
    msg_lines = paste0(
      "- ", failed_names,
      " (value=", round(failed_vals, 6),
      ", min=", round(failed_mins, 6),
      ", max=", round(failed_maxs, 6), ")"
    )
    
    # Calculate PROMO_ONLY totals for the warning message
    prom_gm_only = tryCatch(sum(prom_base$GM_Abs, na.rm = TRUE), error = function(e) NA_real_)
    prom_nr_only = tryCatch(sum(prom_base$Net_Revenue, na.rm = TRUE), error = function(e) NA_real_)
    prom_gm_percent_only = ifelse(!is.na(prom_gm_only) && !is.na(prom_nr_only) && prom_nr_only != 0, prom_gm_only * 100 / prom_nr_only, NA_real_)
    
    # Issue warning instead of stopping - return best plan found
    warning_msg = paste(
      "WARNING: Optimization completed but some constraints are not fully satisfied.",
      "Constraint scope is PROMO_ONLY (only the optimized promotional plan).",
      paste0(
        "Promo-only totals: ",
        "GM=", round(prom_gm_only, 6),
        " | NR=", round(prom_nr_only, 6),
        " | GM%NR=", round(prom_gm_percent_only, 6)
      ),
      "The final plan still violates:",
      paste(msg_lines, collapse = "\n"),
      sep = "\n"
    )
    warning(warning_msg)
    cat("\n", warning_msg, "\n\n")
  }
  
  if(FALSE)  
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
  
  write.csv(opti_output,"Output_sample.csv")
  # write.csv(exc_brand,paste0(out_path,"Excluded_Brand.csv"), row.names = F)
  
  return(list(opti_output,exc_brand,kpi_iteration,budget_info))
}


optimizer_op_prep <- function(opti_out,base_tesco,exclude_ppg,include_format,include_ppg,start_date,end_date,tesco_slot = NULL){
  # REMOVED: Week end date logic - no longer using ceiling_date
  # start_date = ceiling_date(start_date,"week",week_start = 2)   # 2 means week ending on Tuesday
  # start_date = as_date(ifelse(start_date == ymd("2019/01/09") , ymd("2019/01/01") , start_date))
  
  #Filter data between start and end date (no week end date adjustment)
  base_tesco = base_tesco[Date>= start_date & Date <= end_date]
  opti_out$Event_Lift
 
   if(length(include_ppg) == 1 && grepl("ALL", include_ppg, ignore.case = TRUE)) {
    # Get all unique PPGs from the data sources
    all_ppgs = unique(c(
      unique(opti_out$PPG),
      unique(base_tesco$PPG)
      
    ))
    # Remove any NA values
    all_ppgs = all_ppgs[!is.na(all_ppgs)]
    include_ppg = all_ppgs
  }
  
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
  
  opti_out_prom = opti_out[Seq == 1]
  
  opti_out_prom = opti_out_prom[,.(PPG, `SECTOR 2`,`TRADING COMPANY`,`PRODUCT RANGE`, FORMAT,PPG_Description, Tesco_Week_No,`Start Date`,`End Date`,Discount,Display,
                                   Display_Cost, Event_Multiplier_Tesco,Display_Flag,Flyer_Flag, Promo_Price)]   
  opti_out_prom = opti_out_prom[,TPR_Flag := 1]
  
  
  #keep only selected FORMAT from tesco_base
  base_tesco = base_tesco[ FORMAT %in% include_format ]
  
  #keep only selected PPG from tesco_base
  base_tesco = base_tesco[ PPG %in% include_ppg ]
  
  #remove excluded PPG from tesco_base
  base_tesco = base_tesco[!(PPG %in% exclude_ppg)]
  
  #1.2 Join the data
  
  
  
  opti_out_prom$Tesco_Week_No=as.numeric(opti_out_prom$Tesco_Week_No)
  #aggreagation
  static_cols <- c(
    "Net_Cost_Unit","COGS_Unit","VAT","BIP_Case","OID","No_Of_Units",
    "STP_Unit","UNCR_Unit","OID_Unit","FM_Abs_Unit_1","RSP_Unit"
  )
 
  df_sum <- base_tesco %>%
    group_by(PPG, `SECTOR 2`, `TRADING COMPANY`, `PRODUCT RANGE`, FORMAT, PPG_Description, Tesco_Week_No) %>%
    summarise(
      # keep text/date columns
      across(where(is.character), first),
      across(where(is.Date), first),
      
      # keep these numeric columns static (do NOT sum)
      across(all_of(static_cols), first),
      
      # sum all other numeric columns (excluding static ones)
      across(where(is.numeric) & !all_of(static_cols), ~sum(.x, na.rm = TRUE)),
      
      .groups = "drop"
    )
  
 
  tesco_full_cal = merge(df_sum,opti_out_prom, by = c("PPG","SECTOR 2","TRADING COMPANY","PRODUCT RANGE", "FORMAT","PPG_Description",
                                                      "Tesco_Week_No","Start Date","End Date"), all.x = T)
  tesco_full_cal[is.na(tesco_full_cal)] = 0
  
 
  setDT(tesco_full_cal)
  #1.3 Divide the display cost by slot duration (dynamic, not fixed 3)
  # If slot duration is available, use it; otherwise default to 3
  if("Duration" %in% names(tesco_full_cal)){
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
  
  tesco_full_cal[,Value_Sales := ifelse(Promo_Price == 0, Total_Sales*RSP_Unit, Total_Sales*Promo_Price)]     #addon3
  
  
  
  tesco_full_cal[, Duration := abs(as.numeric(`End Date` - `Start Date`))]
  
  
  
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
    #replacement = event_to_replace[1,]
    
    #
    if(i == nrow(event_to_replace)){
      
      df = opti_cal
      
    }else if(replacement$`Event ID` != "No Event"){
      replace_row = which(opti_cal$Tesco_Week_No == as.numeric(str_split(replacement$`Event ID`,pattern = "-")[[1]][2]) & opti_cal$PPG == replacement$PPG)
      event_info = all_events[all_events$PPG == replacement$PPG & all_events$`Event ID` == replacement$`Event ID`,]
      #make a copy of opti_cal
      df = opti_cal
      
      
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