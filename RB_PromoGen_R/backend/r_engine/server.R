#### Server function ####
shinyServer(function(input, output, session) {
  addClass(selector = "body",class = "sidebar-collapse")
  options(shiny.maxRequestSize=10000*1024^2)
  #- create userInfo dynamically responsive to URL input 
  #- .../?user=<username>[&status=<online|away|offline>]
  
  #- create dashboardUser() and sidebarUserPanel()
  create_userInfo <- reactive({
    
    query <- parseQueryString(session$clientData$url_search)
    
    # parse URL for status
    user_status <- "Online"
    
    if ( "status" %in% names(query) ) {
      
      if (query[["status"]] == "away") {
        
        user_status = "Away"
        
      } else if (query[["status"]] == "offline") {
        
        user_status = "Offline"
        
      }
      
    }
    
    # set default userInfo
    head_user <- dashboardUser(
      name = "User",
      image = "user.png",
      description = "",
      sub_description = "",
      stat1 = "Proprietor",
      stat2 = "Wayne Industries",
      stat3 = "Gotham City",
      btn1 = "Profile",
      btn2 = "Sign Out"
    )
    
    side_user <- sidebarUserPanel(
      name = "User", subtitle = "Hi!", status = user_status,
      image = "user.png"
    )
    
    # parse URL for userInfo
    if ( "user" %in% names(query) ) {
      
      # message("login as user: ", query[["user"]], "\n")
      
      if (query[["user"]] == "bm") {
        
        head_user <- dashboardUser(
          name = "Batman",
          image = "Batman.png",
          description = "Alchemist",
          sub_description = "Bruce Wayne",
          stat1 = "Proprietor",
          stat2 = "Wayne Industries",
          stat3 = "Gotham City",
          btn1 = "Profile",
          btn2 = "Sign Out"
        )
        
        side_user <- sidebarUserPanel(
          name = "Batman", subtitle = "Hi!", status = user_status,
          image = "Batman.png"
        )
        
      } else if (query[["user"]] == "su") {
        
        head_user <- dashboardUser(
          name = "Superman", 
          image = "Superman.png",
          description = "A.I.",
          sub_description = "Clark Kent",
          stat1 = "Super Hero",
          stat2 = "Justice League of America",
          stat3 = "Planet Krypton"
        )
        
        side_user <- sidebarUserPanel(
          name = "Superman", subtitle = "Hello World!", status = user_status,
          image = "Superman.png"
        )
        
      }
      
    }
    
    return( list(head_user = head_user, side_user = side_user) )
    
  })
  
  output$user <- renderUser({ create_userInfo() %$% head_user })
  
  output$dynamicSidebarUserPanel <- renderMenu({ create_userInfo() %$% side_user })
  
  #- create footer as running message commnunication
  output$dynamicFooter <- renderFooter({
    # dashboardFooter(mainText = span(img(src='logo_mini.png',width = 50,height = 50),
    #                                 HTML('<font size="2"> &copy Copyright Reckitt Benckiser Group plc. All Rights reserved </font>')),
    #                 subText = span(actionButton(inputId = 'fback',label = 'FEEDBACK'),HTML("<b>| </b>"),icon("envelope"),HTML("Analytics@RBcom.onmicrosoft.com |"),icon("phone"),HTML("(345) 237 4562")))
  })
  
  ####Reading all the input Data sets####
  #Reading input nielsen rms data
  #Uncomment
  SP_input_nielsen_rms <- reactive({
    # inFile <- input$SP_inputpath_nielsen_rms
    #
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""),sheet=1)
    input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path_root,"//RMS NEW.xlsx"))
    return(input_nielsen)
  })
  
  #Reading input nielsen model results data
  SP_input_nielsen_model_results <- reactive({
    # inFile <- input$SP_inputpath_nielsen_model_results
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""),sheet = "Data")
    input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path_root,"//Model Results Sample.xlsx"))
    return(input_nielsen)
  })
  
  #Reading input nielsen Event list
  SP_input_nielsen_event <- reactive({
    # inFile <- input$SP_inputpath_nielsen_event
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""),sheet = "Event_List")
    library(readr)
    input_nielsen <- read_csv(
      paste0(SP_reactive_input$folder_path_root, "/HEA Event PC.csv")
    )
    return(input_nielsen)
  })
  
  #Reading input Cost bible data
  SP_input_cost_bible <- reactive({
    # inFile <- input$SP_inputpath_cost_bible
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = 1)
    input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path_root,"//5 Cost Bible Brand JV.xlsx"))
    return(input_nielsen)
  })
  
  #Reading input tesco slots information
  SP_input_tesco_slot <- reactive({
    # inFile <- input$SP_inputpath_tesco_slot
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = 1)
    input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path_root,"//Carrefour Slots 2025-2026.xlsx"))
    return(input_nielsen)
  })
  
  #Reading input tesco slots information
  SP_input_ean_ldesc_map <- reactive({
    # inFile <- input$SP_inputpath_ean_ldesc_map
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = 1)
    input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path_root,"//8 Nielsen EAN to LDESC D77 (1).xlsx"))
    return(input_nielsen)
  })
  
  #Reading input LSM information
  SP_input_LSM <- reactive({
    # inFile <- input$SP_inputpath_LSM
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- data.table(read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = "LSM"))
    input_nielsen <- data.table(read_excel(paste0(SP_reactive_input$folder_path_root,"//new lsm.xlsx")))
    return(input_nielsen)
  })
  
  #Reading input delist SKUs information
  SP_input_delist <- reactive({
    # inFile <- input$SP_inputpath_delist
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = 1)
    input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path_root,"//Brand_DL_NL (1).xlsx"),sheet="Sheet1")
    
    return(input_nielsen)
  })
  
  #Reading input Promo Seq information
  SP_input_promo_seq <- reactive({
    # inFile <- input$SP_inputpath_delist
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = 1)
    input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path_root,"//1 All Possible Promo Sequence.xlsx"),sheet="Sheet1")
    
    return(input_nielsen)
  })
  
  SP_input_retailer_end_day <- reactive({
    # inFile <- input$SP_inputpath_delist
    # if (is.null(inFile))
    #   return(NULL)
    # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
    # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = 1)
    input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path_root,"//Retailer WeekEnd Day CF.xlsx"))
    
    return(input_nielsen)
  })
  
  #Initializing reactive values
  SP_reactive_input <- reactiveValues()
  SP_reactive_input$SP_opti_old_brand <- ""
  SP_reactive_input$SP_opti_old_format <- ""
  ####Calculating compare scenario table for existing plans#####
  SP_cmp_scn_ip <- function(path){
    paste0(path,"/Saved Optimizers Annual/",SP_reactive_input$TPO_Name,".RData")
    cmp_scn_old <- list()
    #
    if(length(list.files(paste0(path,"/Saved Optimizers Annual"))) != 0){
      for(i in list.files(paste0(path,"/Saved Optimizers Annual"))){
        load(paste0(path,"/Saved Optimizers Annual/",i))
        cmp_scn_old[[i]] <- tpo_list_temp$Compare_Scenario
      }
      
      return(do.call(rbind,cmp_scn_old))
    }else{
      cmp_scn_old <- data.frame(matrix("",ncol = 16))
      names(cmp_scn_old) <- c("TPO ID","Country","Customer","Category","Brand","Format","PPG","Goal","Goal Objective",
                              "LSM Constrained/Unconstrained","Scan Net Revenue","GM % NR","TI % NR","Gross Sales","Volume Sales","Trade ROI")
      return(cmp_scn_old)
    }
    
  }
  
  SP_saved_tpo <- function(path){
    saved_tpo_old <- list()
    if(length(list.files(paste0(path,"/Saved Optimizers Annual"))) != 0){
      for(i in list.files(paste0(path,"/Saved Optimizers Annual"))){
        if(!(i %like% "Merge")){
          load(paste0(path,"/Saved Optimizers Annual/",i))
          saved_tpo_old[[i]] <- data.frame("TPO ID" = tpo_list_temp$Compare_Scenario$`TPO ID`,"Customer" = tpo_list_temp$Compare_Scenario$Customer,"LSM Constrained/Unconstrained" = tpo_list_temp$Compare_Scenario$`LSM Constrained/Unconstrained`, "Optimization Objective" = paste0(tpo_list_temp$Compare_Scenario$Goal,"(",tpo_list_temp$Compare_Scenario$`Goal Objective`,")"),check.names = FALSE)
        }
      }
      return(do.call(rbind,saved_tpo_old))
    }else{
      saved_tpo_old <- data.frame(matrix("",ncol = 4))
      names(saved_tpo_old) <- c("TPO ID","Customer","LSM Constrained/Unconstrained","Optimization Objective")
      return(saved_tpo_old)
    }
  }
  
  observeEvent({input$SP_level},{
    rm(list = ls())
    closeAllConnections()
  })
  
  
  #####################Loading the data, doing the data prep and updating all select inputs##########################
  observeEvent(input$SP_proceed_summ,{
    showModal(
      modalDialog(
        title = "Processing the data",
        "Will take a few minutes based on the data size",
        size = "m",
        footer = "Please wait"
      )
    )
    
    rm(list = ls())
    ##Cannibalization File Generation
    source("Cannibalization_File_Gen.R")
    #Uncomment
    # options(warn=-1)
    # Cannibalization_CoPromotion(paste0(str_sub(getwd(),1,gregexpr("/",getwd())[[1]][length(gregexpr("/",getwd())[[1]])]-1),
    #                                    "/"))
    # options(warn=0)
    SP_reactive_input$folder_path_root <- paste0(str_sub(getwd(),1,gregexpr("/",getwd())[[1]][length(gregexpr("/",getwd())[[1]])]-1),
                                                 "/",input$SP_brand,"/Root")
    
    SP_reactive_input$folder_path <- paste0(str_sub(getwd(),1,gregexpr("/",getwd())[[1]][length(gregexpr("/",getwd())[[1]])]-1),
                                            "/",input$SP_brand,"/",input$SP_level)
    
    #Reading input EAN to PPG mapping
    SP_input_EAN_PPG <- reactive({
      # inFile <- input$SP_inputpath_EAN_PPG
      # if (is.null(inFile))
      #   return(NULL)
      # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
      # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = 1, col_types = "text")
      input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path,"//4 EAN to PPG Mapping.xlsx"), col_types = "text")
      return(input_nielsen)
    })
    
    SP_input_cannibalization <- reactive({
      # inFile <- input$SP_inputpath_delist
      # if (is.null(inFile))
      #   return(NULL)
      # file.rename(inFile$datapath,paste(inFile$datapath,".xlsx",sep = ""))
      # input_nielsen <- read_excel(paste(inFile$datapath,".xlsx",sep = ""), sheet = 1)
      input_nielsen <- read_excel(paste0(SP_reactive_input$folder_path,"//9 Cannibalization Brand.xlsx"),sheet="Sheet1")
      
      return(input_nielsen)
    })
    SP_reactive_input$date_mapping <- fread(paste0(SP_reactive_input$folder_path_root,"/Date_Mapping.csv"))
    SP_reactive_input$date_mapping$current_year_date <- as.Date(SP_reactive_input$date_mapping$current_year_date,"%m/%d/%Y")
    SP_reactive_input$date_mapping$last_year_date <- as.Date(SP_reactive_input$date_mapping$last_year_date,"%m/%d/%Y")
    
    SP_reactive_input$SP_manuf <- input$SP_manuf
    if(input$SP_brand == "BARSOAP"){
      SP_reactive_input$SP_brand_short <- "BS"
    }else if(input$SP_brand == "FINISH"){
      SP_reactive_input$SP_brand_short <- "FN"
    }else if(input$SP_brand == "VANISH"){
      SP_reactive_input$SP_brand_short <- "VN"
    }else{
      SP_reactive_input$SP_brand_short <- "BR"
    }
    opti_log_empty <- data.frame()
    #write.csv(opti_log_empty, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),row.names = FALSE)
    SP_reactive_input$SP_cmp_scn_ip <- SP_cmp_scn_ip(SP_reactive_input$folder_path)
    SP_reactive_input$SP_saved_tpo <- SP_saved_tpo(SP_reactive_input$folder_path)
    SP_reactive_input$EAN_PPG_mapping <- SP_input_EAN_PPG()
    #Calling data prep function
    if(!file.exists(paste0(SP_reactive_input$folder_path,"/","data_prep_op.RData"))){
      strt_time <- Sys.time()
      data_prep_op <- data_prep(SP_input_nielsen_rms(),SP_input_nielsen_model_results(),SP_input_nielsen_event(),SP_input_EAN_PPG(),SP_input_cost_bible(),SP_input_tesco_slot(),data.table(SP_input_ean_ldesc_map()),SP_input_delist(),SP_input_LSM(),SP_input_promo_seq(),SP_input_retailer_end_day(),input$SP_cust,SP_reactive_input$SP_manuf,input$SP_brand,SP_reactive_input$date_mapping)
      end_time <- Sys.time()
      opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
      log_tmp <- data.frame("Function" = "Data Preparation", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time, check.names = FALSE)
      opti_log <- rbind(opti_log,log_tmp)
      write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
      save(data_prep_op,file = paste0(SP_reactive_input$folder_path,"/","data_prep_op.RData")) 
    }else{
      load(paste0(SP_reactive_input$folder_path,"/","data_prep_op.RData"))
    }
    # DEBUG: Check data_prep_op contents
    cat("\n========== DATA_PREP_OP CHECK ==========\n")
    cat("data_prep_op length:", length(data_prep_op), "\n")
    if(length(data_prep_op) >= 6){
      cat("data_prep_op[[6]] (shiny_opti_data_ip): rows =", 
          ifelse(is.null(data_prep_op[[6]]), "NULL", nrow(data_prep_op[[6]])), "\n")
    }
    if(length(data_prep_op) >= 5){
      cat("data_prep_op[[5]] (prod_restrictions): rows =", 
          ifelse(is.null(data_prep_op[[5]]), "NULL", nrow(data_prep_op[[5]])), "\n")
    }
    if(length(data_prep_op) >= 8){
      cat("data_prep_op[[8]] (exclude_ppg): rows =", 
          ifelse(is.null(data_prep_op[[8]]), "NULL", nrow(data_prep_op[[8]])), "\n")
    }
    if(length(data_prep_op) >= 4){
      cat("data_prep_op[[4]] (SP_opti_const): rows =", 
          ifelse(is.null(data_prep_op[[4]]), "NULL", nrow(data_prep_op[[4]])), "\n")
    }
    cat("========================================\n\n")
    
    ##Initializing counters
    SP_reactive_input$save_counter <- 0
    SP_reactive_input$save_counter_sim <- 0
    
    SP_reactive_input$save_counter_tbo <- 0
    SP_reactive_input$save_counter_sim_tbo <- 0
    #SP_reactive_input$save_nonlsm_counter <- 0
    
    #Reading data prep output for first two screens in required format
    SP_reactive_input$SP_nielsen <- data_prep_op[[1]]
    
    SP_reactive_input$competition_display <- comp_display_cost(SP_reactive_input$SP_nielsen,manuf = SP_reactive_input$SP_manuf,brand = (input$SP_brand))
    
    SP_reactive_input$SP_nielsen <- column_header_mapping(SP_reactive_input$SP_nielsen,"Nielsen_RMS")
    if(any(is.na(names(SP_reactive_input$SP_nielsen)))){
      SP_reactive_input$SP_nielsen <- SP_reactive_input$SP_nielsen[,!(is.na(names(SP_reactive_input$SP_nielsen)))]
    }
    SP_reactive_input$SP_nielsen$Date <- ymd(SP_reactive_input$SP_nielsen$`Week End Date`)
    SP_reactive_input$SP_nielsen <- data.table(SP_reactive_input$SP_nielsen)
    write.csv(SP_reactive_input$SP_nielsen,paste0(SP_reactive_input$folder_path,"/","nielsen.csv"),row.names = FALSE)
    SP_reactive_input$cal_sample <- data_prep_op[[2]]
    SP_reactive_input$cal_sample <- column_header_mapping(SP_reactive_input$cal_sample,"Promotion_calendar")
    if(any(is.na(names(SP_reactive_input$cal_sample)))){
      SP_reactive_input$cal_sample <- SP_reactive_input$cal_sample[,!(is.na(names(SP_reactive_input$cal_sample)))]
    }
    SP_reactive_input$cal_sample$Event <- ifelse(SP_reactive_input$cal_sample$Display_Flag == 1,"Display",ifelse(SP_reactive_input$cal_sample$Promo_Flag == 1,"TPR","No Promo"))
    SP_reactive_input$cal_sample$Country <- "UAE"
    SP_reactive_input$cal_sample$Retailer <- "Carrefour"
    
    event_file <- data_prep_op[[3]]
    
    #event_file$Date <- ymd(event_file$Date)
    event_file$Date= event_file$`Week End Date`
    event_file <- event_file[event_file$Date %in% sort(unique(event_file$Date))[(length(sort(unique(event_file$Date)))-365):length(sort(unique(event_file$Date)))],]
    event_file$Flag_TPR_HEA
    event_file$Event <- ifelse(event_file$Display_Flag_KAM_Input == 1,"Display",ifelse(event_file$Flag_TPR_HEA== 1,"TPR","No Promo"))
    # event_file$Tesco_Event <- ifelse(event_file$Display_Flag_KAM_Input_Tesco == 1,"Display",ifelse(event_file$Flag_TPR_HEA_Tesco == 1,"TPR","No Promo"))
    SP_reactive_input$event_file <- column_header_mapping(event_file,"Promotion_calendar")
    if(any(is.na(names(SP_reactive_input$event_file)))){
      SP_reactive_input$event_file <- SP_reactive_input$event_file[,!(is.na(names(SP_reactive_input$event_file)))]
    }
    
     
    week_no <- data.frame("Date" = sort(unique(SP_reactive_input$event_file$Date)),"Week No" = c(1:length(sort(unique(SP_reactive_input$event_file$Date)))),check.names = FALSE)
    SP_reactive_input$event_file <- left_join(SP_reactive_input$event_file,week_no,by = "Date")
    #write.csv(SP_reactive_input$event_file,"r.csv")
    #Reading data prep output for optimizer screen
    SP_reactive_input$SP_opti_const <- data_prep_op[[4]]
    SP_reactive_input$SP_opti_prod_restrictions_default <- data_prep_op[[5]]
    SP_reactive_input$SP_opti_const <- column_header_mapping(SP_reactive_input$SP_opti_const,"opti_const")
    SP_reactive_input$SP_opti_prod_restrictions_default <- column_header_mapping(SP_reactive_input$SP_opti_prod_restrictions_default,"prod_const")
    SP_reactive_input$SP_opti_const$Date <- ymd(SP_reactive_input$SP_opti_const$`Week End Date`)
    SP_reactive_input$date_mapping=read.csv("map.csv",header = T)
    SP_reactive_input$date_mapping$last_year_date=dmy(SP_reactive_input$date_mapping$last_year_date)
    SP_reactive_input$date_mapping$current_year_date=dmy(SP_reactive_input$date_mapping$current_year_date)
    SP_reactive_input$SP_opti_const <- left_join(SP_reactive_input$SP_opti_const,SP_reactive_input$date_mapping, by = c("Date" = "last_year_date"))
    
    #write.csv(SP_reactive_input$date_mapping,"map.csv")
    SP_reactive_input$shiny_opti_data_ip <- data_prep_op[[6]]
    #SP_reactive_input$shiny_ip_events <- data_prep_op[[7]]
    #write.csv(SP_reactive_input$shiny_ip_events,paste0(SP_reactive_input$folder_path,"/","Event_List.csv"),row.names = FALSE)
    SP_reactive_input$shiny_ip_tesco_cal <- data_prep_op[[7]]
    #SP_reactive_input$shiny_ip_events_lsm <- data_prep_op[[9]]
    SP_reactive_input$exclude_ppg_data_prep <- data_prep_op[[8]]
    SP_reactive_input$shiny_ip_events_final <- data_prep_op[[9]]
    SP_reactive_input$competition_KPI <- data_prep_op[[10]]
    SP_reactive_input$retailer_week_end_day <- data_prep_op[[11]]
    SP_reactive_input$retailer_weekEndDay_no <- data_prep_op[[12]]
    
    SP_reactive_input$exclude_ppg_data_prep <- column_header_mapping(SP_reactive_input$exclude_ppg_data_prep,"opti_const")
    
    SP_reactive_input$const_chg_flag <- 0
    SP_reactive_input$const_chg_flag_on <- 0
    SP_reactive_input$SP_promo_seq <- SP_input_promo_seq()
    SP_reactive_input$SP_cannibalization<- SP_input_cannibalization()
    #Uncomment
    #SP_reactive_input$comp_seq <- data.table(include_comp_promo(data.table(SP_reactive_input$SP_nielsen),SP_input_nielsen_model_results(),brand = input$SP_brand,retailer_end_day = SP_input_retailer_end_day(),retailer = input$SP_cust))
    #SP_reactive_input$comp_seq = data.table()
    SP_reactive_input$comp_seq <- data.table(matrix(numeric(),ncol = 4),check.names = FALSE)
    names(SP_reactive_input$comp_seq) <- c("PPG","Date","Slot No","Comp_Promo_Flag")
    SP_reactive_input$comp_seq$PPG = as.character(SP_reactive_input$comp_seq$PPG)
    
    
    
    #
    ####Format Mapping
    SP_reactive_input$format_mapping <- read_excel(paste(SP_reactive_input$folder_path_root,"/Format_Mapping.xlsx",sep = ""),sheet = 1)
    updateSelectizeInput(session,"SP_summ_cat",selected = unique(SP_reactive_input$SP_nielsen$Category)[1],choices = unique(SP_reactive_input$SP_nielsen$Category))
    
    output$SP_summ_start_date_ui <- renderUI({
      SP_reactive_input$SP_date_vector <- unique(SP_reactive_input$SP_nielsen$Date)[order(unique(SP_reactive_input$SP_nielsen$Date))]
      SP_reactive_input$SP_date_vector_start <- max(SP_reactive_input$SP_date_vector) - dweeks(51)
      selectizeInput("SP_summ_date_start","Start Date",choices = SP_reactive_input$SP_date_vector,selected = as.Date(SP_reactive_input$SP_date_vector_start))
    })
    
    output$SP_summ_end_date_ui <- renderUI({
      if(!(is.null(input$SP_summ_date_start))){
        SP_reactive_input$SP_date_vector_end <- SP_reactive_input$SP_date_vector[SP_reactive_input$SP_date_vector >= input$SP_summ_date_start]
      }else{
        SP_reactive_input$SP_date_vector_end <- SP_reactive_input$SP_date_vector
      }
      selectizeInput("SP_summ_date_end","End Date",choices = SP_reactive_input$SP_date_vector_end,selected = max(SP_reactive_input$SP_date_vector_end))
    })
    
    
    observeEvent(input$SP_summ_cat,{
      updateSelectizeInput(session,"SP_summ_manuf",choices = unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat,]$Manufacturer))
    })
    
    observeEvent({
      input$SP_summ_cat
      input$SP_summ_manuf},{
        updateSelectizeInput(session,"SP_summ_brand",choices = unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Manufacturer %in% input$SP_summ_manuf,]$Brand))
      }
    )
    
    observeEvent({
      input$SP_summ_cat
      input$SP_summ_manuf
      input$SP_summ_brand},{
        updateSelectizeInput(session,"SP_summ_format",choices = unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand,]$Format))
      })
    
    observeEvent({
      input$SP_summ_cat
      input$SP_summ_manuf
      input$SP_summ_format
      input$SP_summ_brand},{
        updateSelectizeInput(session,"SP_summ_ppg",choices = unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_summ_format,]$`PPG`))
      }
    )
    
    #Loading KAM cokpit with Final Plan present
    if(file.exists(paste0(SP_reactive_input$folder_path,"/Final_Optimized_Plan.RData"))){
      load(paste0(SP_reactive_input$folder_path,"/Final_Optimized_Plan.RData"))
      SP_reactive_input$SP_TPO_selected <- tpo_list_temp
      
      updateSelectizeInput(session,"SP_kam_cat",selected = as.character(SP_reactive_input$SP_TPO_selected$cat),choices = as.character(SP_reactive_input$SP_TPO_selected$cat))
      updateSelectizeInput(session,"SP_kam_brand",choices = as.character(SP_reactive_input$SP_TPO_selected$brand),selected = as.character(SP_reactive_input$SP_TPO_selected$brand))      
      updateSelectizeInput(session,"SP_kam_format",choices = as.character(SP_reactive_input$SP_TPO_selected$format),selected = as.character(SP_reactive_input$SP_TPO_selected$format))
      updateSelectizeInput(session,"SP_kam_tpo",choices = as.character(SP_reactive_input$SP_TPO_selected$tpo_id),selected = as.character(SP_reactive_input$SP_TPO_selected$tpo_id))
      
      SP_reactive_input$SP_TPO_selected_weekly <- SP_reactive_input$SP_TPO_selected$opti_output[SP_reactive_input$SP_TPO_selected$opti_output$`Category` == SP_reactive_input$SP_TPO_selected$Compare_Scenario$Category & SP_reactive_input$SP_TPO_selected$opti_output$`Brand` == SP_reactive_input$SP_TPO_selected$Compare_Scenario$Brand,.("Net Invoice Sales" = sum(NIS),"CPD" = sum(Retro_Funding_Total), "TI" = sum(Total_Trade_Investment),"Scan Net Revenue" = sum(Net_Revenue),"Gross Margin" = sum(GM_Abs), "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                                                                                                                                                                                                                                                              "Trade ROI" = ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc))),
                                                                                                                                                                                                                                                                                                                                              "Inc_GM_Abs" = sum(Inc_GM_Abs),"R_GM_Inc" = sum(R_GM_Inc),"Inc_Revenue" = sum(Inc_Revenue), "R_Net_Rev_Inc" = sum(R_Net_Rev_Inc), "Inc_NIS" = sum(Inc_NIS), "R_NIS_Inc" = sum(R_NIS_Inc), "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue),"Retailer_Revenue" = sum(Retailer_Revenue),
                                                                                                                                                                                                                                                                                                                                              "CPD % NIS" = sum(Retro_Funding_Total) * 100/sum(NIS), "Total_Sales" = sum(Total_Sales),"R_Trade_Inv_Inc" = sum(R_Trade_Inv_Inc),
                                                                                                                                                                                                                                                                                                                                              "RSV" = sum(Retailer_Revenue),"COGS" = sum(Net_Cost_Unit * Total_Sales),"Front Margin" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales), "FM %" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales)/sum(Retailer_Revenue/(1+VAT)), 
                                                                                                                                                                                                                                                                                                                                              "Fixed" = sum(Display_Cost), "Back Margin" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales) + sum(Display_Cost), "BM %" = (sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales) + sum(Display_Cost))/sum(Retailer_Revenue/(1+VAT))),by = .(`Category`,`Brand`,Week_Ending)]
      
      #SP_reactive_input$SP_cmp_scn_selected <- t(SP_reactive_input$SP_TPO_selected$Compare_Scenario[,c("Scan Net Revenue","GM % NR","TI % NR","Gross Sales","Trade ROI","Volume Sales")])
      
      SP_reactive_input$SP_cmp_scn_selected_epos <- data.frame("KPI Metrics" = c("Net Invoice Sales","CPD","TI","Scan Net Revenue","Gross Margin","GM % NR","Trade ROI","TI % NR","CPD % NIS","Value Market Share"),
                                                               "Predicted" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`),sum(SP_reactive_input$SP_TPO_selected_weekly$CPD),sum(SP_reactive_input$SP_TPO_selected_weekly$TI),
                                                                               sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`),(sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`)),
                                                                               (ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",sum(SP_reactive_input$SP_TPO_selected_weekly$R_GM_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",sum(SP_reactive_input$SP_TPO_selected_weekly$R_Net_Rev_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc),sum(SP_reactive_input$SP_TPO_selected_weekly$R_NIS_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc)))),
                                                                               (sum(SP_reactive_input$SP_TPO_selected_weekly$TI) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`)),
                                                                               (sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`)),(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + SP_reactive_input$SP_TPO_selected$other_sales_value))),
                                                               "Actual" = rep("",10),
                                                               "Plan Projection" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`),sum(SP_reactive_input$SP_TPO_selected_weekly$CPD),sum(SP_reactive_input$SP_TPO_selected_weekly$TI),
                                                                                     sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`),(sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`)),
                                                                                     (ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",sum(SP_reactive_input$SP_TPO_selected_weekly$R_GM_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",sum(SP_reactive_input$SP_TPO_selected_weekly$R_Net_Rev_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc),sum(SP_reactive_input$SP_TPO_selected_weekly$R_NIS_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc)))),
                                                                                     (sum(SP_reactive_input$SP_TPO_selected_weekly$TI) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`)),
                                                                                     (sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`)),(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + SP_reactive_input$SP_TPO_selected$other_sales_value))),
                                                               
                                                               "Forecast Projection" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`),sum(SP_reactive_input$SP_TPO_selected_weekly$CPD),sum(SP_reactive_input$SP_TPO_selected_weekly$TI),
                                                                                         sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`),(sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`)),
                                                                                         (ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",sum(SP_reactive_input$SP_TPO_selected_weekly$R_GM_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",sum(SP_reactive_input$SP_TPO_selected_weekly$R_Net_Rev_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc),sum(SP_reactive_input$SP_TPO_selected_weekly$R_NIS_Inc)/sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc)))),
                                                                                         (sum(SP_reactive_input$SP_TPO_selected_weekly$TI) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`)),
                                                                                         (sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`)),(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + SP_reactive_input$SP_TPO_selected$other_sales_value))),check.names = FALSE)
      
      SP_reactive_input$SP_cmp_scn_selected_epos[SP_reactive_input$SP_cmp_scn_selected_epos$`KPI Metrics` %in% c("Net Invoice Sales","CPD","TI","Scan Net Revenue","Gross Margin"), c("Predicted", "Plan Projection","Forecast Projection")] <- SP_reactive_input$SP_cmp_scn_selected_epos[SP_reactive_input$SP_cmp_scn_selected_epos$`KPI Metrics` %in% c("Net Invoice Sales","CPD","TI","Scan Net Revenue","Gross Margin"), c("Predicted", "Plan Projection","Forecast Projection")]/10^6
      SP_reactive_input$SP_cmp_scn_selected_epos$`KPI Metrics` <- c("Net Invoice Sales(MM GBP)","CPD(MM GBP)","TI(MM GBP)","Scan Net Revenue(MM GBP)","Gross Margin(MM GBP)","GM % NR","Trade ROI","TI % NR","CPD % NIS","Value Market Share")
      
      # SP_reactive_input$SP_TPO_selected_epos_table <- rbind(data.frame("KPI Metrics" = rownames(SP_reactive_input$SP_cmp_scn_selected),"Predicted" = rep("",nrow(SP_reactive_input$SP_cmp_scn_selected)), "Actual" = rep("",nrow(SP_reactive_input$SP_cmp_scn_selected)),"Plan Projection" = SP_reactive_input$SP_cmp_scn_selected[,1], "Forecast Projection" = SP_reactive_input$SP_cmp_scn_selected[,1],check.names = FALSE),
      #                                                       SP_reactive_input$SP_cmp_scn_selected_1)
      SP_reactive_input$SP_cmp_scn_selected_cust <- data.frame("KPI Metrics" = c("RSV", "COGS","Front Margin","FM %","CPD","Fixed","Back Margin","BM %"),
                                                               "Predicted" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV),sum(SP_reactive_input$SP_TPO_selected_weekly$COGS),sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2),sum(SP_reactive_input$SP_TPO_selected_weekly$CPD),sum(SP_reactive_input$SP_TPO_selected_weekly$Fixed),
                                                                               sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2)),
                                                               "Actual" = rep("",8),
                                                               "Plan Projection" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV),sum(SP_reactive_input$SP_TPO_selected_weekly$COGS),sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2),sum(SP_reactive_input$SP_TPO_selected_weekly$CPD),sum(SP_reactive_input$SP_TPO_selected_weekly$Fixed),
                                                                                     sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2)),
                                                               "Forecast Projection" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV),sum(SP_reactive_input$SP_TPO_selected_weekly$COGS),sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2),sum(SP_reactive_input$SP_TPO_selected_weekly$CPD),sum(SP_reactive_input$SP_TPO_selected_weekly$Fixed),
                                                                                         sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`),sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) * 100/sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2)),check.names = FALSE)
      
      SP_reactive_input$SP_cmp_scn_selected_cust[SP_reactive_input$SP_cmp_scn_selected_cust$`KPI Metrics` %in% c("RSV","COGS","Front Margin","CPD","Fixed","Back Margin"),c("Predicted", "Plan Projection","Forecast Projection")] <- SP_reactive_input$SP_cmp_scn_selected_cust[SP_reactive_input$SP_cmp_scn_selected_cust$`KPI Metrics` %in% c("RSV","COGS","Front Margin","CPD","Fixed","Back Margin"),c("Predicted", "Plan Projection","Forecast Projection")]/10^6
      SP_reactive_input$SP_cmp_scn_selected_cust$`KPI Metrics` <- c("RSV(MM GBP)", "COGS(MM GBP)","FM(MM GBP)","FM %","CPD(MM GBP)","Fixed(MM GBP)","BM(MM GBP)","BM %")
      
      output$SP_kam_RB_table <- renderDataTable({
        datatable(SP_reactive_input$SP_cmp_scn_selected_epos,class="cell-border stripe",extensions = c('FixedColumns'),
                  options = list(
                    fixedColumns = list(leftColumns = 1),
                    # paging = FALSE,
                    dom = "t",
                    # searching = FALSE,
                    scrollX = TRUE,
                    scrollY = "250px"
                  ),rownames = F) %>%
          formatStyle(names(SP_reactive_input$SP_cmp_scn_selected_epos),textAlign = 'center') %>%
          formatRound(names(select_if(SP_reactive_input$SP_cmp_scn_selected_epos,is.numeric)), digits = 2)
        
      })
      
      SP_reactive_input$SP_TPO_selected_epos_chart_RB <- SP_reactive_input$SP_TPO_selected_weekly[,c("Date","Net Invoice Sales"),with = FALSE]
      ###Actual vs predicted plot
      output$SP_kam_RB_chart <- renderPlotly({
        plot_ly(SP_reactive_input$SP_TPO_selected_epos_chart_RB) %>%
          add_trace(x = ~Week_Ending,y = ~`Net Invoice Sales`, type = 'scatter', mode = 'lines', line = list(width = 1,color = "#0099DC"),
                    hovertext = paste0(SP_reactive_input$SP_TPO_selected_epos_chart_RB$Week_Ending,", ",round(SP_reactive_input$SP_TPO_selected_epos_chart_RB[["Net Invoice Sales"]],2)),hoverinfo = 'text',
                    name = 'Predicted') %>%
          layout(showlegend = TRUE,margin = list(r= 60),xaxis = list(title = 'Time Period'),paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',yaxis = list(side = 'left',title = "Net Invoice Sales", zeroline = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
      })
      
      output$SP_kam_cust_table <- renderDataTable({
        datatable(SP_reactive_input$SP_cmp_scn_selected_cust,class="cell-border stripe",extensions = c('FixedColumns'),
                  options = list(
                    fixedColumns = list(leftColumns = 1),
                    # paging = FALSE,
                    dom = "t",
                    # searching = FALSE,
                    scrollX = TRUE,
                    scrollY = "250px"
                  ),rownames = F) %>%
          formatStyle(names(SP_reactive_input$SP_cmp_scn_selected_cust),textAlign = 'center') %>%
          formatRound(names(select_if(SP_reactive_input$SP_cmp_scn_selected_cust,is.numeric)), digits = 2)
      })
      
      SP_reactive_input$SP_TPO_selected_epos_chart_cust <- SP_reactive_input$SP_TPO_selected_weekly[,c("Date","RSV"),with = FALSE]
      ###Actual vs predicted plot
      output$SP_kam_cust_chart <- renderPlotly({
        plot_ly(SP_reactive_input$SP_TPO_selected_epos_chart_cust) %>%
          add_trace(x = ~Week_Ending,y = ~RSV, type = 'scatter', mode = 'lines', line = list(width = 1,color = "#0099DC"),
                    hovertext = paste0(SP_reactive_input$SP_TPO_selected_epos_chart_cust$Week_Ending,", ",round(SP_reactive_input$SP_TPO_selected_epos_chart_cust[["RSV"]],2)),hoverinfo = 'text',
                    name = 'Predicted') %>%
          layout(showlegend = TRUE,margin = list(r= 60),xaxis = list(title = 'Time Period'),paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',yaxis = list(side = 'left',title = "RSV", zeroline = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
      })
    }
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu2")
    removeModal()
    
    
    ###############Calculations and populating charts for the first screen#####################
    observeEvent({
      #input$SP_level
      input$SP_summ_cat
      input$SP_summ_manuf
      input$SP_summ_brand
      input$SP_summ_format
      input$SP_summ_ppg
      input$SP_summ_date_start
      input$SP_summ_date_end},{
        
        #Calculating the Year ago dates(Next Saturday for the corresponding year ago date) and Filtering selected time period
        SP_reactive_input$ya_start_week <- seq((ymd(input$SP_summ_date_start)-dyears(1)),(ymd(input$SP_summ_date_start)-dyears(1))+6,by='day')
        SP_reactive_input$ya_end_week <- seq((ymd(input$SP_summ_date_end)-dyears(1)),(ymd(input$SP_summ_date_end)-dyears(1))+6,by='day')
        
        SP_reactive_input$SP_nielsen_selected <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen$Date <= ymd(input$SP_summ_date_end),]
        SP_reactive_input$SP_nielsen_selected_ya <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],]
        if((ymd(input$SP_summ_date_start)-dyears(1)) >= (min(SP_reactive_input$SP_nielsen$Date) - 5)){
          SP_reactive_input$YA_start_check <- 1
        }else if((ymd(input$SP_summ_date_start)-dyears(1)) < (min(SP_reactive_input$SP_nielsen$Date) - 5)){
          SP_reactive_input$YA_start_check <- NULL
        }
        #####################When Only Category is being selected###########################
        if(all(input$SP_summ_manuf == "") & all(input$SP_summ_brand == "") & all(input$SP_summ_format == "") & all(input$SP_summ_ppg == "")){
          #Grouping and aggregating current year sales to the selected level
          #
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                              "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                              "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                              "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,Brand)]
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_grouped[SP_reactive_input$SP_nielsen_grouped$`CY_Value` != 0 & SP_reactive_input$SP_nielsen_grouped$`CY_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped$CY_Price <- SP_reactive_input$SP_nielsen_grouped$CY_Value/SP_reactive_input$SP_nielsen_grouped$CY_Volume
          SP_reactive_input$SP_nielsen_grouped$val_share <- SP_reactive_input$SP_nielsen_grouped$CY_Value*100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Value)
          SP_reactive_input$SP_nielsen_grouped$vol_share <- SP_reactive_input$SP_nielsen_grouped$CY_Volume *100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Volume)
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Category == input$SP_summ_cat,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                       "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                       "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                       "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,Brand)]
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_grouped_ya[SP_reactive_input$SP_nielsen_grouped_ya$`YA_Value` != 0 & SP_reactive_input$SP_nielsen_grouped_ya$`YA_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped_ya$YA_Price <- SP_reactive_input$SP_nielsen_grouped_ya$YA_Value/SP_reactive_input$SP_nielsen_grouped_ya$YA_Volume
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_nielsen_grouped_total <- left_join(SP_reactive_input$SP_nielsen_grouped,SP_reactive_input$SP_nielsen_grouped_ya,by = c("Category","Brand"))
          
          #Calculating % change in Value, Volume and price
          SP_reactive_input$SP_nielsen_grouped_total$vol_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$val_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$price_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Price
          SP_reactive_input$SP_nielsen_grouped_total[is.na(SP_reactive_input$SP_nielsen_grouped_total)] <- 0
          
          #Value Share plot
          output$SP_summ_value_share_plot <- renderPlotly({
            value_share_plot(SP_reactive_input$SP_nielsen_grouped,"Brand",input$SP_brand)
          })
          
          #Volume Share Plot
          output$SP_summ_volume_share_plot <- renderPlotly({
            volume_share_plot(SP_reactive_input$SP_nielsen_grouped,"Brand")
          })
          
          # % change in Volume versus % change in Price
          output$SP_summ_bubble_chart <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            vol_price_bubble_chart(SP_reactive_input$SP_nielsen_grouped_total,"Brand")
          })
          
          #Value and Volume sales for CY,YA table
          SP_reactive_input$SP_nielsen_CY_YA_table <- SP_reactive_input$SP_nielsen_grouped_total[,c("Brand","CY_Volume","YA_Volume","vol_chg","CY_Price","YA_Price","price_chg")]
          SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")] <- SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")]/1000
          names(SP_reactive_input$SP_nielsen_CY_YA_table) <- c("Brand","CY Volume('000 Units)","YA Volume('000 Units)","% Volume Chg","CY Price","YA Price","% Price Chg")
          
          
          output$SP_summ_cy_ya_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            cy_ya_table(SP_reactive_input$SP_nielsen_CY_YA_table)
          })
          
          #Each lower level contribution to % change in Value sales at aggregated level
          SP_reactive_input$SP_nielsen_grouped_total <- data.table(SP_reactive_input$SP_nielsen_grouped_total)
          SP_reactive_input$SP_nielsen_total_val_chg <- SP_reactive_input$SP_nielsen_grouped_total[,.("CY_Value" = sum(SP_reactive_input$SP_nielsen_grouped_total$CY_Value),"CY_Volume"= sum(SP_reactive_input$SP_nielsen_grouped_total$CY_Volume),
                                                                                                      "YA_Value"=sum(SP_reactive_input$SP_nielsen_grouped_total$YA_Value),"YA_Volume"=sum(SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)),by=c("Category")]
          SP_reactive_input$SP_nielsen_total_val_chg$CY_Price <- SP_reactive_input$SP_nielsen_total_val_chg$CY_Value/SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$YA_Price <- SP_reactive_input$SP_nielsen_total_val_chg$YA_Value/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$val_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Value - SP_reactive_input$SP_nielsen_total_val_chg$YA_Value)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_total_val_chg$vol_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume - SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$prc_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Price - SP_reactive_input$SP_nielsen_total_val_chg$YA_Price)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          SP_reactive_input$SP_nielsen_grouped_total$total_val_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$total_vol_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$total_prc_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          #Absolute Price plot
          output$SP_summ_price_abs_plot <- renderPlotly({
            price_abs_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Category")
          })
          
          #Year on Year % change in Value Sales
          output$SP_summ_value_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            value_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Category")
          })
          
          #Year on Year % change in Volume Sales
          output$SP_summ_volume_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            volume_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Category")
          })
          
          
          #Year on Year % change in Price
          output$SP_summ_price_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            price_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Category")
          })
          
          
          #Value share contribution to aggregate level
          output$SP_summ_val_share_contri <- renderPlotly({
            val_share_contri_chart(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Category")
          })
          
          #Volume Decomposition Tab
          SP_reactive_input$SP_nielsen_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "TDP" = sum(`ACV Distribution`),
                                                                                                                                           "WD" = max(`ACV Distribution`),"ND" = max(`Num Selling Dist`),"value_promo" = sum(`Value (C) (any promo)`), 
                                                                                                                                           "volume_promo" = sum(`Units (C) (any promo)`),"base_value" = sum(`Base Value`),"base_volume" = sum(`Base Units`),"promo_flag" = max(`Promo_Flag`)),by = .(Category,Date)]
          SP_reactive_input$SP_nielsen_weekly_bp <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$`5th Largest Regular Price` != 0,.("base_price" = mean(`5th Largest Regular Price`)),by = .(Category,Date)]
          SP_reactive_input$SP_nielsen_weekly <- data.table(left_join(SP_reactive_input$SP_nielsen_weekly,SP_reactive_input$SP_nielsen_weekly_bp,by = c("Category","Date")))
          is.na(SP_reactive_input$SP_nielsen_weekly$base_price) <- 0
          SP_reactive_input$SP_nielsen_weekly$CY_Price <- SP_reactive_input$SP_nielsen_weekly$CY_Value/SP_reactive_input$SP_nielsen_weekly$CY_Volume
          SP_reactive_input$SP_nielsen_weekly$discount <- (1-(SP_reactive_input$SP_nielsen_weekly$CY_Price/SP_reactive_input$SP_nielsen_weekly$base_price))*100
          SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$promo_flag ==0,"discount"] <- 0
          
          SP_reactive_input$SP_nielsen_weekly_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),]
          SP_reactive_input$SP_nielsen_weekly_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],]
          
          
          SP_reactive_input$SP_nielsen_agg_product_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6, 
                                                                                                                                                                                                                                                   "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category)]
          SP_reactive_input$SP_nielsen_agg_product_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6,
                                                                                                                                                                                                                                                                                                                                                                                                                            "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category)]
          SP_reactive_input$SP_nielsen_agg_product_CY$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_CY$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_CY$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_agg_product_YA$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_YA$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_YA$`Volume Sales('000 Units)`
          if(nrow(SP_reactive_input$SP_nielsen_agg_product_YA) != 0){
            SP_reactive_input$SP_nielsen_facts <- data.frame("Metrics"=names(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_YA))
          }else{
            SP_reactive_input$SP_nielsen_facts <- data.frame("Metrics"=names(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_CY),rep("",nrow(transpose(SP_reactive_input$SP_nielsen_agg_product_CY))))
          }
          
          names(SP_reactive_input$SP_nielsen_facts) <- c("Metrics","CY","YA")
          SP_reactive_input$SP_nielsen_facts <- SP_reactive_input$SP_nielsen_facts[!(SP_reactive_input$SP_nielsen_facts$Metrics %in% c("Category","Manufacturer","Brand","Format","Sub Brand","PPG","SKU")),]
          SP_reactive_input$SP_nielsen_facts[,c("CY","YA")] <- as.numeric(unlist(SP_reactive_input$SP_nielsen_facts[,c("CY","YA")]))
          SP_reactive_input$SP_nielsen_facts$`Absolute Change` <- SP_reactive_input$SP_nielsen_facts$CY - SP_reactive_input$SP_nielsen_facts$YA
          SP_reactive_input$SP_nielsen_facts$`% Change` <- SP_reactive_input$SP_nielsen_facts$`Absolute Change`*100/SP_reactive_input$SP_nielsen_facts$YA
          
          #Fact Table at aggregated product level
          output$SP_summ_fact_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            agg_fact_table(SP_reactive_input$SP_nielsen_facts)
          })
          
          #Stacked area chart of base and increment sales
          output$SP_summ_volume_price_chart <- renderPlotly({
            vol_price_area_chart(SP_reactive_input$SP_nielsen_weekly_CY)
          })
          
          #Grouping and aggregating current year sales to the selected level
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                            "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                            "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                            "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_base_value" = sum(`Base Value`),"CY_base_volume" = sum(`Base Units`)),by = .(Category)]
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_promo_eff_agg[SP_reactive_input$SP_promo_eff_agg$`CY_Value` != 0 & SP_reactive_input$SP_promo_eff_agg$`CY_Volume` != 0,]
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Category == input$SP_summ_cat,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                     "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                     "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                     "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`),"YA_base_value" = sum(`Base Value`),"YA_base_volume" = sum(`Base Units`)),by = .(Category)]
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_promo_eff_agg_ya[SP_reactive_input$SP_promo_eff_agg_ya$`YA_Value` != 0 & SP_reactive_input$SP_promo_eff_agg_ya$`YA_Volume` != 0,]
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_promo_eff_agg_total <- left_join(SP_reactive_input$SP_promo_eff_agg,SP_reactive_input$SP_promo_eff_agg_ya,by = c("Category"))
          
          #Promo sales split
          output$SP_promo_split <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            promo_split_chart_CY_YA(SP_reactive_input$SP_promo_eff_agg_total,"Category")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_promo_eff_agg_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                 "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                 "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (feature only)`),
                                                                                                                                                 "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),
                                                                                                                                                 "CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`)),by = .(Category,Date)]
          SP_reactive_input$SP_promo_eff_agg_weekly$CY_Price <- SP_reactive_input$SP_promo_eff_agg_weekly$CY_Value/SP_reactive_input$SP_promo_eff_agg_weekly$CY_Volume
          
          SP_reactive_input$SP_promo_eff_agg_weekly_CY <- SP_reactive_input$SP_promo_eff_agg_weekly[SP_reactive_input$SP_promo_eff_agg_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_promo_eff_agg_weekly$Date <= ymd(input$SP_summ_date_end),]
          
          output$SP_promo_WoW_split <- renderPlotly({
            promo_split_chart_weekly(SP_reactive_input$SP_promo_eff_agg_weekly_CY)
          })
          #
          #SKU level table
          SP_reactive_input$SP_nielsen_selected_SKU <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat),.("CY_Value" = sum(Value), "CY_Volume" = sum(Units),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`), "CY_Value_Promo" = sum(Value) - sum(`Base Value`),"CY_Volume_Promo" = sum(Units) - sum(`Base Units`),
                                                                                                                                                                     "CY_Value_TPR" = (sum(`Value (C) (unsupported)`)*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`))),
                                                                                                                                                                     "CY_Volume_TPR" = (sum(`Units (C) (Unsupported)`)*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),
                                                                                                                                                                     "CY_Value_Display" = (sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`))*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`)),
                                                                                                                                                                     "CY_Volume_Display" = (sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`))*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU_bp <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$`5th Largest Regular Price` != 0),.("Base_Price" = mean(`5th Largest Regular Price`)),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU <- left_join(SP_reactive_input$SP_nielsen_selected_SKU,SP_reactive_input$SP_nielsen_selected_SKU_bp,by = c("Category","Manufacturer","Brand","Format","PPG","PPG_Description","EAN Code"))
          SP_reactive_input$SP_nielsen_selected_SKU$CY_Price <- round(SP_reactive_input$SP_nielsen_selected_SKU$CY_Value/SP_reactive_input$SP_nielsen_selected_SKU$CY_Volume,2)
          
          #SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)] <- round(SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)],2)
          SP_reactive_input$SP_nielsen_selected_SKU[, 
                                                    lapply(.SD, round, 2), 
                                                    .SDcols = sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)
          ]
          names(SP_reactive_input$SP_nielsen_selected_SKU) <- c("Category","Manufacturer","Brand","Format","PPG","PPG_Description","EAN Code","Value Sales(GBP)","Volume Sales(Units)","Base Value(GBP)","Base Volume(Units)","Incremental Value(GBP)","Incremental Volume(Units)","Value Shelf(GBP)","Volume Shelf(Units)","Value Display &/or Feature(GBP)","Volume Display &/or Feature(Units)","Base Price","Average Price")
          
          output$SP_summ_SKU_table <- renderDataTable({
            sku_table(SP_reactive_input$SP_nielsen_selected_SKU)
          })
          #####################When Category & Manufacturer are being selected###########################
        }else if(all(input$SP_summ_brand == "") & all(input$SP_summ_format == "") & all(input$SP_summ_ppg == "")){
          #Grouping and aggregating current year sales to the selected level
          
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                            "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                            "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                            "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,Manufacturer,Brand)]
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_grouped[SP_reactive_input$SP_nielsen_grouped$`CY_Value` != 0 & SP_reactive_input$SP_nielsen_grouped$`CY_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped$CY_Price <- SP_reactive_input$SP_nielsen_grouped$CY_Value/SP_reactive_input$SP_nielsen_grouped$CY_Volume
          SP_reactive_input$SP_nielsen_grouped$val_share <- SP_reactive_input$SP_nielsen_grouped$CY_Value*100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Value)
          SP_reactive_input$SP_nielsen_grouped$vol_share <- SP_reactive_input$SP_nielsen_grouped$CY_Volume *100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Volume)
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                     "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                     "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                     "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,Manufacturer,Brand)]
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_grouped_ya[SP_reactive_input$SP_nielsen_grouped_ya$`YA_Value` != 0 & SP_reactive_input$SP_nielsen_grouped_ya$`YA_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped_ya$YA_Price <- SP_reactive_input$SP_nielsen_grouped_ya$YA_Value/SP_reactive_input$SP_nielsen_grouped_ya$YA_Volume
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_nielsen_grouped_total <- left_join(SP_reactive_input$SP_nielsen_grouped,SP_reactive_input$SP_nielsen_grouped_ya,by = c("Category","Manufacturer","Brand"))
          
          #Calculating % change in Value, Volume and price
          SP_reactive_input$SP_nielsen_grouped_total$vol_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$val_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$price_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Price
          SP_reactive_input$SP_nielsen_grouped_total[is.na(SP_reactive_input$SP_nielsen_grouped_total)] <- 0
          # if(nrow(SP_reactive_input$SP_nielsen_grouped) == 0){
          #   SP_reactive_input$SP_nielsen_grouped <- NULL
          # }
          # if(nrow(SP_reactive_input$SP_nielsen_grouped_total) == 0){
          #   SP_reactive_input$SP_nielsen_grouped_total <- NULL
          # }
          #Value Share plot
          output$SP_summ_value_share_plot <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_nielsen_grouped,"No Current year sales")
            )
            value_share_plot(SP_reactive_input$SP_nielsen_grouped,"Brand",input$SP_brand)
          })
          
          #Volume Share Plot
          output$SP_summ_volume_share_plot <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_nielsen_grouped,"No Current year sales")
            )
            volume_share_plot(SP_reactive_input$SP_nielsen_grouped,"Brand")
          })
          
          # % change in Volume versus % change in Price
          output$SP_summ_bubble_chart <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation"),
              need(SP_reactive_input$SP_nielsen_grouped_total,"No Current year sales")
            )
            vol_price_bubble_chart(SP_reactive_input$SP_nielsen_grouped_total,"Brand")
          })
          
          #Value and Volume sales for CY,YA table
          #if(!(is.null(SP_reactive_input$SP_nielsen_grouped_total))){
          SP_reactive_input$SP_nielsen_CY_YA_table <- SP_reactive_input$SP_nielsen_grouped_total[,c("Brand","CY_Volume","YA_Volume","vol_chg","CY_Price","YA_Price","price_chg")]
          SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")] <- SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")]/1000
          names(SP_reactive_input$SP_nielsen_CY_YA_table) <- c("Brand","CY Volume('000 Units)","YA Volume('000 Units)","% Volume Chg","CY Price","YA Price","% Price Chg")
          
          #}
          
          output$SP_summ_cy_ya_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            cy_ya_table(SP_reactive_input$SP_nielsen_CY_YA_table)
          })
          
          #Each lower level contribution to % change in Value sales at aggregated level
          SP_reactive_input$SP_nielsen_grouped_total <- data.table(SP_reactive_input$SP_nielsen_grouped_total)
          SP_reactive_input$SP_nielsen_total_val_chg <- SP_reactive_input$SP_nielsen_grouped_total[,.("CY_Value" = sum(SP_reactive_input$SP_nielsen_grouped_total$CY_Value),"CY_Volume"= sum(SP_reactive_input$SP_nielsen_grouped_total$CY_Volume),
                                                                                                      "YA_Value"=sum(SP_reactive_input$SP_nielsen_grouped_total$YA_Value),"YA_Volume"=sum(SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)),by=c("Category")]
          SP_reactive_input$SP_nielsen_total_val_chg$Manufacturer <- paste(input$SP_summ_manuf,collapse = "+")
          SP_reactive_input$SP_nielsen_total_val_chg$CY_Price <- SP_reactive_input$SP_nielsen_total_val_chg$CY_Value/SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$YA_Price <- SP_reactive_input$SP_nielsen_total_val_chg$YA_Value/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$val_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Value - SP_reactive_input$SP_nielsen_total_val_chg$YA_Value)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_total_val_chg$vol_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume - SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$prc_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Price - SP_reactive_input$SP_nielsen_total_val_chg$YA_Price)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          SP_reactive_input$SP_nielsen_grouped_total$total_val_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$total_vol_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$total_prc_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          #Absolute Price plot
          output$SP_summ_price_abs_plot <- renderPlotly({
            price_abs_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Manufacturer")
          })
          
          #Year on Year % change in Value Sales
          output$SP_summ_value_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            value_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Manufacturer")
          })
          
          #Year on Year % change in Volume Sales
          output$SP_summ_volume_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            volume_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Manufacturer")
          })
          
          
          #Year on Year % change in Price
          output$SP_summ_price_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            price_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Manufacturer")
          })
          
          
          #Value share contribution to aggregate level
          output$SP_summ_val_share_contri <- renderPlotly({
            val_share_contri_chart(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Brand","Manufacturer")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_nielsen_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Manufacturer %in% input$SP_summ_manuf,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "TDP" = sum(`ACV Distribution`),
                                                                                                                                                                                                                "WD" = max(`ACV Distribution`),"ND" = max(`Num Selling Dist`),"value_promo" = sum(`Value (C) (any promo)`), 
                                                                                                                                                                                                                "volume_promo" = sum(`Units (C) (any promo)`),"base_value" = sum(`Base Value`),"base_volume" = sum(`Base Units`),"promo_flag" = max(`Promo_Flag`)),by = .(Category,Date)]
          
          SP_reactive_input$SP_nielsen_weekly$Manufacturer <- paste(input$SP_summ_manuf,collapse = "+")
          SP_reactive_input$SP_nielsen_weekly_bp <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen$`5th Largest Regular Price` != 0,.("base_price" = mean(`5th Largest Regular Price`)),by = .(Category,Date)]
          SP_reactive_input$SP_nielsen_weekly_bp$Manufacturer <- paste(input$SP_summ_manuf,collapse = "+")
          
          SP_reactive_input$SP_nielsen_weekly <- data.table(left_join(SP_reactive_input$SP_nielsen_weekly,SP_reactive_input$SP_nielsen_weekly_bp,by = c("Category","Manufacturer","Date")))
          is.na(SP_reactive_input$SP_nielsen_weekly$base_price) <- 0
          SP_reactive_input$SP_nielsen_weekly$CY_Price <- SP_reactive_input$SP_nielsen_weekly$CY_Value/SP_reactive_input$SP_nielsen_weekly$CY_Volume
          SP_reactive_input$SP_nielsen_weekly$discount <- (1-(SP_reactive_input$SP_nielsen_weekly$CY_Price/SP_reactive_input$SP_nielsen_weekly$base_price))*100
          SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$promo_flag ==0,"discount"] <- 0
          
          SP_reactive_input$SP_nielsen_weekly_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),]
          SP_reactive_input$SP_nielsen_weekly_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],]
          
          
          SP_reactive_input$SP_nielsen_agg_product_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6, 
                                                                                                                                                                                                                                                   "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category)]
          SP_reactive_input$SP_nielsen_agg_product_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6,
                                                                                                                                                                                                                                                                                                                                                                                                                            "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category)]
          SP_reactive_input$SP_nielsen_agg_product_CY$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_CY$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_CY$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_agg_product_YA$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_YA$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_YA$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_agg_product_CY$Manufacturer <- paste(input$SP_summ_manuf,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_YA$Manufacturer <- paste(input$SP_summ_manuf,collapse = "+")
          
          SP_reactive_input$SP_nielsen_facts <- data.frame("Metrics"=names(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_YA))
          names(SP_reactive_input$SP_nielsen_facts) <- c("Metrics","CY","YA")
          SP_reactive_input$SP_nielsen_facts <- SP_reactive_input$SP_nielsen_facts[!(SP_reactive_input$SP_nielsen_facts$Metrics %in% c("Category","Manufacturer","Brand","Format","Sub Brand","PPG","SKU")),]
          SP_reactive_input$SP_nielsen_facts[,c("CY","YA")] <- as.numeric(unlist(SP_reactive_input$SP_nielsen_facts[,c("CY","YA")]))
          SP_reactive_input$SP_nielsen_facts$`Absolute Change` <- SP_reactive_input$SP_nielsen_facts$CY - SP_reactive_input$SP_nielsen_facts$YA
          SP_reactive_input$SP_nielsen_facts$`% Change` <- SP_reactive_input$SP_nielsen_facts$`Absolute Change`*100/SP_reactive_input$SP_nielsen_facts$YA
          
          #Fact Table at aggregated product level
          output$SP_summ_fact_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            agg_fact_table(SP_reactive_input$SP_nielsen_facts)
          })
          
          #Stacked area chart of base and increment sales
          output$SP_summ_volume_price_chart <- renderPlotly({
            
            vol_price_area_chart(SP_reactive_input$SP_nielsen_weekly_CY)
          })
          
          #Grouping and aggregating current year sales to the selected level
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                          "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                          "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                          "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_base_value" = sum(`Base Value`),"CY_base_volume" = sum(`Base Units`)),by = .(Category)]
          SP_reactive_input$SP_promo_eff_agg$Manufacturer <- paste(input$SP_summ_manuf,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_promo_eff_agg[SP_reactive_input$SP_promo_eff_agg$`CY_Value` != 0 & SP_reactive_input$SP_promo_eff_agg$`CY_Volume` != 0,]
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                   "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                   "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                   "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`),"YA_base_value" = sum(`Base Value`),"YA_base_volume" = sum(`Base Units`)),by = .(Category)]
          SP_reactive_input$SP_promo_eff_agg_ya$Manufacturer <- paste(input$SP_summ_manuf,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_promo_eff_agg_ya[SP_reactive_input$SP_promo_eff_agg_ya$`YA_Value` != 0 & SP_reactive_input$SP_promo_eff_agg_ya$`YA_Volume` != 0,]
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_promo_eff_agg_total <- left_join(SP_reactive_input$SP_promo_eff_agg,SP_reactive_input$SP_promo_eff_agg_ya,by = c("Manufacturer"))
          
          #Promo sales split
          output$SP_promo_split <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            promo_split_chart_CY_YA(SP_reactive_input$SP_promo_eff_agg_total,"Manufacturer")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_promo_eff_agg_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                               "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                               "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (feature only)`),
                                                                                                                                                                                                                               "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),
                                                                                                                                                                                                                               "CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`)),by = .(Category,Date)]
          SP_reactive_input$SP_promo_eff_agg_weekly$Manufacturer <- paste(input$SP_summ_manuf,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_weekly$CY_Price <- SP_reactive_input$SP_promo_eff_agg_weekly$CY_Value/SP_reactive_input$SP_promo_eff_agg_weekly$CY_Volume
          
          SP_reactive_input$SP_promo_eff_agg_weekly_CY <- SP_reactive_input$SP_promo_eff_agg_weekly[SP_reactive_input$SP_promo_eff_agg_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_promo_eff_agg_weekly$Date <= ymd(input$SP_summ_date_end),]
          output$SP_promo_WoW_split <- renderPlotly({
            promo_split_chart_weekly(SP_reactive_input$SP_promo_eff_agg_weekly_CY)
          })
          
          #SKU level table
          SP_reactive_input$SP_nielsen_selected_SKU <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf),.("CY_Value" = sum(Value), "CY_Volume" = sum(Units),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`), "CY_Value_Promo" = sum(Value) - sum(`Base Value`),"CY_Volume_Promo" = sum(Units) - sum(`Base Units`),
                                                                                                                                                                                                                                                   "CY_Value_TPR" = (sum(`Value (C) (unsupported)`)*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`))),
                                                                                                                                                                                                                                                   "CY_Volume_TPR" = (sum(`Units (C) (Unsupported)`)*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),
                                                                                                                                                                                                                                                   "CY_Value_Display" = (sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`))*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`)),
                                                                                                                                                                                                                                                   "CY_Volume_Display" = (sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`))*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU_bp <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat  & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen_selected$`5th Largest Regular Price` != 0),.("Base_Price" = mean(`5th Largest Regular Price`)),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU <- left_join(SP_reactive_input$SP_nielsen_selected_SKU,SP_reactive_input$SP_nielsen_selected_SKU_bp,by = c("Category","Manufacturer","Brand","Format","PPG","PPG_Description","EAN Code"))
          SP_reactive_input$SP_nielsen_selected_SKU$CY_Price <- round(SP_reactive_input$SP_nielsen_selected_SKU$CY_Value/SP_reactive_input$SP_nielsen_selected_SKU$CY_Volume,2)
          SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)] <- round(SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)],2)
          names(SP_reactive_input$SP_nielsen_selected_SKU) <- c("Category","Manufacturer","Brand","Format","PPG","PPG Description","EAN Code","Value Sales(GBP)","Volume Sales(Units)","Base Value(GBP)","Base Volume(Units)","Incremental Value(GBP)","Incremental Volume(Units)","Value Shelf(GBP)","Volume Shelf(Units)","Value Display &/or Feature(GBP)","Volume Display &/or Feature(Units)","Base Price","Average Price")
          output$SP_summ_SKU_table <- renderDataTable({
            sku_table(SP_reactive_input$SP_nielsen_selected_SKU)
          }) 
          
          #######################When Category, Manufacturer and Brand are being selected#####################
        }else if(all(input$SP_summ_format == "") & all(input$SP_summ_ppg == "")){
          #
          #Grouping and aggregating current year sales to the selected level
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                     "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                     "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                     "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,Format)]
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_grouped[SP_reactive_input$SP_nielsen_grouped$`CY_Value` != 0 & SP_reactive_input$SP_nielsen_grouped$`CY_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped$CY_Price <- SP_reactive_input$SP_nielsen_grouped$CY_Value/SP_reactive_input$SP_nielsen_grouped$CY_Volume
          SP_reactive_input$SP_nielsen_grouped$val_share <- SP_reactive_input$SP_nielsen_grouped$CY_Value*100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Value)
          SP_reactive_input$SP_nielsen_grouped$vol_share <- SP_reactive_input$SP_nielsen_grouped$CY_Volume *100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Volume)
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected_ya$Brand %in% input$SP_summ_brand,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                 "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                 "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                 "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,Format)]
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_grouped_ya[SP_reactive_input$SP_nielsen_grouped_ya$`YA_Value` != 0 & SP_reactive_input$SP_nielsen_grouped_ya$`YA_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped_ya$YA_Price <- SP_reactive_input$SP_nielsen_grouped_ya$YA_Value/SP_reactive_input$SP_nielsen_grouped_ya$YA_Volume
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_nielsen_grouped_total <- left_join(SP_reactive_input$SP_nielsen_grouped,SP_reactive_input$SP_nielsen_grouped_ya,by = c("Category","Format"))
          
          #Calculating % change in Value, Volume and price
          SP_reactive_input$SP_nielsen_grouped_total$vol_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$val_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$price_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Price
          SP_reactive_input$SP_nielsen_grouped_total[is.na(SP_reactive_input$SP_nielsen_grouped_total)] <- 0
          
          #Value Share plot
          output$SP_summ_value_share_plot <- renderPlotly({
            
            value_share_plot(SP_reactive_input$SP_nielsen_grouped,"Format",input$SP_brand)
          })
          
          #Volume Share Plot
          output$SP_summ_volume_share_plot <- renderPlotly({
            volume_share_plot(SP_reactive_input$SP_nielsen_grouped,"Format")
          })
          
          
          # % change in Volume versus % change in Price
          output$SP_summ_bubble_chart <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            vol_price_bubble_chart(SP_reactive_input$SP_nielsen_grouped_total,"Format")
          })
          
          #Value and Volume sales for CY,YA table
          SP_reactive_input$SP_nielsen_CY_YA_table <- SP_reactive_input$SP_nielsen_grouped_total[,c("Format","CY_Volume","YA_Volume","vol_chg","CY_Price","YA_Price","price_chg")]
          SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")] <- SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")]/1000
          names(SP_reactive_input$SP_nielsen_CY_YA_table) <- c("Format","CY Volume('000 Units)","YA Volume('000 Units)","% Volume Chg","CY Price","YA Price","% Price Chg")
          
          output$SP_summ_cy_ya_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            cy_ya_table(SP_reactive_input$SP_nielsen_CY_YA_table)
          })
          
          #Each lower level contribution to % change in Value sales at aggregated level
          SP_reactive_input$SP_nielsen_grouped_total <- data.table(SP_reactive_input$SP_nielsen_grouped_total)
          SP_reactive_input$SP_nielsen_total_val_chg <- SP_reactive_input$SP_nielsen_grouped_total[,.("CY_Value" = sum(SP_reactive_input$SP_nielsen_grouped_total$CY_Value),"CY_Volume"= sum(SP_reactive_input$SP_nielsen_grouped_total$CY_Volume),
                                                                                                      "YA_Value"=sum(SP_reactive_input$SP_nielsen_grouped_total$YA_Value),"YA_Volume"=sum(SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)),by=c("Category")]
          SP_reactive_input$SP_nielsen_total_val_chg$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_total_val_chg$CY_Price <- SP_reactive_input$SP_nielsen_total_val_chg$CY_Value/SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$YA_Price <- SP_reactive_input$SP_nielsen_total_val_chg$YA_Value/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$val_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Value - SP_reactive_input$SP_nielsen_total_val_chg$YA_Value)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_total_val_chg$vol_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume - SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$prc_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Price - SP_reactive_input$SP_nielsen_total_val_chg$YA_Price)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          SP_reactive_input$SP_nielsen_grouped_total$total_val_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$total_vol_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$total_prc_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          #Absolute Price plot
          output$SP_summ_price_abs_plot <- renderPlotly({
            price_abs_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Format","Brand")
          })
          
          #Year on Year % change in Value Sales
          output$SP_summ_value_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            value_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Format","Brand")
          })
          
          #Year on Year % change in Volume Sales
          output$SP_summ_volume_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            volume_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Format","Brand")
          })
          
          #Year on Year % change in Price
          output$SP_summ_price_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            price_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Format","Brand")
          })
          
          
          #Value share contribution to aggregate level
          output$SP_summ_val_share_contri <- renderPlotly({
            val_share_contri_chart(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"Format","Brand")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_nielsen_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "TDP" = sum(`ACV Distribution`),
                                                                                                                                                                                                         "WD" = max(`ACV Distribution`),"ND" = max(`Num Selling Dist`),"value_promo" = sum(`Value (C) (any promo)`), 
                                                                                                                                                                                                         "volume_promo" = sum(`Units (C) (any promo)`),"base_value" = sum(`Base Value`),"base_volume" = sum(`Base Units`),"promo_flag" = max(`Promo_Flag`)),by = .(Category,Date)]
          SP_reactive_input$SP_nielsen_weekly$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_weekly_bp <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen$`5th Largest Regular Price` != 0,.("base_price" = mean(`5th Largest Regular Price`)),by = .(Category,Date)]
          SP_reactive_input$SP_nielsen_weekly_bp$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_weekly <- data.table(left_join(SP_reactive_input$SP_nielsen_weekly,SP_reactive_input$SP_nielsen_weekly_bp,by = c("Category","Brand","Date")))
          is.na(SP_reactive_input$SP_nielsen_weekly$base_price) <- 0
          SP_reactive_input$SP_nielsen_weekly$CY_Price <- SP_reactive_input$SP_nielsen_weekly$CY_Value/SP_reactive_input$SP_nielsen_weekly$CY_Volume
          SP_reactive_input$SP_nielsen_weekly$discount <- (1-(SP_reactive_input$SP_nielsen_weekly$CY_Price/SP_reactive_input$SP_nielsen_weekly$base_price))*100
          SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$promo_flag ==0,"discount"] <- 0
          
          SP_reactive_input$SP_nielsen_weekly_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),]
          SP_reactive_input$SP_nielsen_weekly_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],]
          
          
          SP_reactive_input$SP_nielsen_agg_product_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6, 
                                                                                                                                                                                                                                                   "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category)]
          SP_reactive_input$SP_nielsen_agg_product_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6, 
                                                                                                                                                                                                                                                                                                                                                                                                                            "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category)]
          SP_reactive_input$SP_nielsen_agg_product_CY$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_YA$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_CY$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_CY$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_CY$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_agg_product_YA$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_YA$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_YA$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_facts <- data.frame("Metrics"=names(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_YA))
          names(SP_reactive_input$SP_nielsen_facts) <- c("Metrics","CY","YA")
          
          SP_reactive_input$SP_nielsen_facts <- SP_reactive_input$SP_nielsen_facts[!(SP_reactive_input$SP_nielsen_facts$Metrics %in% c("Category","Manufacturer","Brand","Format","Sub Brand","PPG","SKU")),]
          SP_reactive_input$SP_nielsen_facts[,c("CY","YA")] <- as.numeric(unlist(SP_reactive_input$SP_nielsen_facts[,c("CY","YA")]))
          SP_reactive_input$SP_nielsen_facts$`Absolute Change` <- SP_reactive_input$SP_nielsen_facts$CY - SP_reactive_input$SP_nielsen_facts$YA
          SP_reactive_input$SP_nielsen_facts$`% Change` <- SP_reactive_input$SP_nielsen_facts$`Absolute Change`*100/SP_reactive_input$SP_nielsen_facts$YA
          
          #Fact Table at aggregated product level
          output$SP_summ_fact_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            agg_fact_table(SP_reactive_input$SP_nielsen_facts)
          })
          
          #Stacked area chart of base and increment sales including the trend line for base price and average price
          output$SP_summ_volume_price_chart <- renderPlotly({
            vol_price_area_chart(SP_reactive_input$SP_nielsen_weekly_CY)
          })
          
          #Grouping and aggregating current year sales to the selected level
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                   "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                   "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                   "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_base_value" = sum(`Base Value`),"CY_base_volume" = sum(`Base Units`)),by = .(Category)]
          SP_reactive_input$SP_promo_eff_agg$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_promo_eff_agg[SP_reactive_input$SP_promo_eff_agg$`CY_Value` != 0 & SP_reactive_input$SP_promo_eff_agg$`CY_Volume` != 0,]
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected_ya$Brand %in% input$SP_summ_brand,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                               "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                               "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                               "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`),"YA_base_value" = sum(`Base Value`),"YA_base_volume" = sum(`Base Units`)),by = .(Category)]
          SP_reactive_input$SP_promo_eff_agg_ya$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_promo_eff_agg_ya[SP_reactive_input$SP_promo_eff_agg_ya$`YA_Value` != 0 & SP_reactive_input$SP_promo_eff_agg_ya$`YA_Volume` != 0,]
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_promo_eff_agg_total <- left_join(SP_reactive_input$SP_promo_eff_agg,SP_reactive_input$SP_promo_eff_agg_ya,by = c("Category","Brand"))
          
          #Promo sales split
          output$SP_promo_split <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            promo_split_chart_CY_YA(SP_reactive_input$SP_promo_eff_agg_total,"Brand")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_promo_eff_agg_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                               "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                               "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                               "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),
                                                                                                                                                                                                               "CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`)),by = .(Category,Date)]
          SP_reactive_input$SP_promo_eff_agg_weekly$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_weekly$CY_Price <- SP_reactive_input$SP_promo_eff_agg_weekly$CY_Value/SP_reactive_input$SP_promo_eff_agg_weekly$CY_Volume
          
          SP_reactive_input$SP_promo_eff_agg_weekly_CY <- SP_reactive_input$SP_promo_eff_agg_weekly[SP_reactive_input$SP_promo_eff_agg_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_promo_eff_agg_weekly$Date <= ymd(input$SP_summ_date_end),]
          output$SP_promo_WoW_split <- renderPlotly({
            promo_split_chart_weekly(SP_reactive_input$SP_promo_eff_agg_weekly_CY)
          })
          #SKU level table
          SP_reactive_input$SP_nielsen_selected_SKU <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand),.("CY_Value" = sum(Value), "CY_Volume" = sum(Units),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`), "CY_Value_Promo" = sum(Value) - sum(`Base Value`),"CY_Volume_Promo" = sum(Units) - sum(`Base Units`),
                                                                                                                                                                                                                                                                                                                          "CY_Value_TPR" = (sum(`Value (C) (unsupported)`)*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`))),
                                                                                                                                                                                                                                                                                                                          "CY_Volume_TPR" = (sum(`Units (C) (Unsupported)`)*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),
                                                                                                                                                                                                                                                                                                                          "CY_Value_Display" = (sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`))*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`)),
                                                                                                                                                                                                                                                                                                                          "CY_Volume_Display" = (sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`))*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU_bp <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$`5th Largest Regular Price` != 0),.("Base_Price" = mean(`5th Largest Regular Price`)),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU <- left_join(SP_reactive_input$SP_nielsen_selected_SKU,SP_reactive_input$SP_nielsen_selected_SKU_bp,by = c("Category","Manufacturer","Brand","Format","PPG","PPG_Description","EAN Code"))
          
          SP_reactive_input$SP_nielsen_selected_SKU$CY_Price <- round(SP_reactive_input$SP_nielsen_selected_SKU$CY_Value/SP_reactive_input$SP_nielsen_selected_SKU$CY_Volume,2)
          SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)] <- round(SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)],2)
          names(SP_reactive_input$SP_nielsen_selected_SKU) <- c("Category","Manufacturer","Brand","Format","PPG","PPG Description","EAN Code","Value Sales(GBP)","Volume Sales(Units)","Base Value(GBP)","Base Volume(Units)","Incremental Value(GBP)","Incremental Volume(Units)","Value Shelf(GBP)","Volume Shelf(Units)","Value Display &/or Feature(GBP)","Volume Display &/or Feature(Units)","Base Price","Average Price")
          output$SP_summ_SKU_table <- renderDataTable({
            sku_table(SP_reactive_input$SP_nielsen_selected_SKU)
          })
          
          #######################When Category, Manufacturer, Brand and Format are being selected#####################
        }else if(all(input$SP_summ_ppg == "")){
          #
          #Grouping and aggregating current year sales to the selected level
          #
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format, .("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                               "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                               "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                               "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,`PPG`)]
          SP_reactive_input$SP_nielsen_grouped_1 <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format, .("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                                 "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                                 "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                                 "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,Brand,Format,`PPG`)]
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_grouped[SP_reactive_input$SP_nielsen_grouped$`CY_Value` != 0 & SP_reactive_input$SP_nielsen_grouped$`CY_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped$CY_Price <- SP_reactive_input$SP_nielsen_grouped$CY_Value/SP_reactive_input$SP_nielsen_grouped$CY_Volume
          SP_reactive_input$SP_nielsen_grouped$val_share <- SP_reactive_input$SP_nielsen_grouped$CY_Value*100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Value)
          SP_reactive_input$SP_nielsen_grouped$vol_share <- SP_reactive_input$SP_nielsen_grouped$CY_Volume *100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Volume)
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected_ya$Format %in% input$SP_summ_format,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                    "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                    "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                    "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,`PPG`)]
          SP_reactive_input$SP_nielsen_grouped_ya_1 <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected_ya$Format %in% input$SP_summ_format,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                      "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                      "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                      "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,Brand,Format,`PPG`)]
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_grouped_ya[SP_reactive_input$SP_nielsen_grouped_ya$`YA_Value` != 0 & SP_reactive_input$SP_nielsen_grouped_ya$`YA_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped_ya$YA_Price <- SP_reactive_input$SP_nielsen_grouped_ya$YA_Value/SP_reactive_input$SP_nielsen_grouped_ya$YA_Volume
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_nielsen_grouped_total <- left_join(SP_reactive_input$SP_nielsen_grouped,SP_reactive_input$SP_nielsen_grouped_ya,by = c("Category","PPG"))
          SP_reactive_input$SP_nielsen_grouped_total_1 <- left_join(SP_reactive_input$SP_nielsen_grouped_1,SP_reactive_input$SP_nielsen_grouped_ya_1,by = c("Category","Brand","Format","PPG"))
          
          #Calculating % change in Value, Volume and price
          SP_reactive_input$SP_nielsen_grouped_total$vol_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$val_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$price_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Price
          SP_reactive_input$SP_nielsen_grouped_total[is.na(SP_reactive_input$SP_nielsen_grouped_total)] <- 0
          
          output$SP_summ_value_share_plot <- renderPlotly({
            value_share_plot(SP_reactive_input$SP_nielsen_grouped,"PPG",input$SP_brand)
          })
          
          #Volume Share Plot
          output$SP_summ_volume_share_plot <- renderPlotly({
            volume_share_plot(SP_reactive_input$SP_nielsen_grouped,"PPG")
          })
          
          # % change in Volume versus % change in Price
          output$SP_summ_bubble_chart <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            vol_price_bubble_chart(SP_reactive_input$SP_nielsen_grouped_total,"PPG")
          })
          
          #Value and Volume sales for CY,YA table
          SP_reactive_input$SP_nielsen_CY_YA_table <- SP_reactive_input$SP_nielsen_grouped_total[,c("PPG","CY_Volume","YA_Volume","vol_chg","CY_Price","YA_Price","price_chg")]
          SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")] <- SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")]/1000
          names(SP_reactive_input$SP_nielsen_CY_YA_table) <- c("PPG","CY Volume('000 Units)","YA Volume('000 Units)","% Volume Chg","CY Price","YA Price","% Price Chg")
          
          output$SP_summ_cy_ya_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            cy_ya_table(SP_reactive_input$SP_nielsen_CY_YA_table)
          })
          
          #Each lower level contribution to % change in Value sales at aggregated level
          SP_reactive_input$SP_nielsen_grouped_total <- data.table(SP_reactive_input$SP_nielsen_grouped_total)
          SP_reactive_input$SP_nielsen_grouped_total_1 <- data.table(SP_reactive_input$SP_nielsen_grouped_total_1)
          
          SP_reactive_input$SP_nielsen_total_val_chg <- SP_reactive_input$SP_nielsen_grouped_total_1[,.("CY_Value" = sum(SP_reactive_input$SP_nielsen_grouped_total_1$CY_Value),"CY_Volume"= sum(SP_reactive_input$SP_nielsen_grouped_total_1$CY_Volume),
                                                                                                        "YA_Value"=sum(SP_reactive_input$SP_nielsen_grouped_total_1$YA_Value),"YA_Volume"=sum(SP_reactive_input$SP_nielsen_grouped_total_1$YA_Volume)),by=c("Category")]
          SP_reactive_input$SP_nielsen_total_val_chg$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_total_val_chg$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_nielsen_total_val_chg$CY_Price <- SP_reactive_input$SP_nielsen_total_val_chg$CY_Value/SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$YA_Price <- SP_reactive_input$SP_nielsen_total_val_chg$YA_Value/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$val_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Value - SP_reactive_input$SP_nielsen_total_val_chg$YA_Value)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_total_val_chg$vol_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume - SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$prc_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Price - SP_reactive_input$SP_nielsen_total_val_chg$YA_Price)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          SP_reactive_input$SP_nielsen_grouped_total$total_val_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$total_vol_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$total_prc_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          #Absolute Price plot
          output$SP_summ_price_abs_plot <- renderPlotly({
            price_abs_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","Format")
          })
          
          #Year on Year % change in Value Sales
          output$SP_summ_value_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            value_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","Format")
          })
          
          #Year on Year % change in Volume Sales
          output$SP_summ_volume_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            volume_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","Format")
          })
          
          #Year on Year % change in Price
          output$SP_summ_price_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            price_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","Format")
          })
          
          #Value share contribution to aggregate level
          output$SP_summ_val_share_contri <- renderPlotly({
            val_share_contri_chart(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","Format")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_nielsen_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_summ_format,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "TDP" = sum(`ACV Distribution`),
                                                                                                                                                                                                                                                                         "WD" = max(`ACV Distribution`),"ND" = max(`Num Selling Dist`),"value_promo" = sum(`Value (C) (any promo)`), 
                                                                                                                                                                                                                                                                         "volume_promo" = sum(`Units (C) (any promo)`),"base_value" = sum(`Base Value`),"base_volume" = sum(`Base Units`),"promo_flag" = max(`Promo_Flag`)),by = .(Category,Date)]
          SP_reactive_input$SP_nielsen_weekly$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_weekly$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_nielsen_weekly_bp <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen$`5th Largest Regular Price` != 0,.("base_price" = mean(`5th Largest Regular Price`)),by = .(Category,Date)]
          SP_reactive_input$SP_nielsen_weekly_bp$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_weekly_bp$Format <- paste(input$SP_summ_format,collapse = "+")
          
          SP_reactive_input$SP_nielsen_weekly <- data.table(left_join(SP_reactive_input$SP_nielsen_weekly,SP_reactive_input$SP_nielsen_weekly_bp,by = c("Category","Brand","Date")))
          is.na(SP_reactive_input$SP_nielsen_weekly$base_price) <- 0
          
          SP_reactive_input$SP_nielsen_weekly$CY_Price <- SP_reactive_input$SP_nielsen_weekly$CY_Value/SP_reactive_input$SP_nielsen_weekly$CY_Volume
          SP_reactive_input$SP_nielsen_weekly$discount <- (1-(SP_reactive_input$SP_nielsen_weekly$CY_Price/SP_reactive_input$SP_nielsen_weekly$base_price))*100
          SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$promo_flag ==0,"discount"] <- 0
          SP_reactive_input$SP_nielsen_weekly_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),]
          SP_reactive_input$SP_nielsen_weekly_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],]
          
          
          SP_reactive_input$SP_nielsen_agg_product_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6, 
                                                                                                                                                                                                                                                   "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category)]
          SP_reactive_input$SP_nielsen_agg_product_CY$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_CY$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6, 
                                                                                                                                                                                                                                                                                                                                                                                                                            "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category)]
          SP_reactive_input$SP_nielsen_agg_product_YA$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_YA$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_CY$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_CY$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_CY$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_agg_product_YA$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_YA$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_YA$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_facts <- data.frame("Metrics"=names(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_YA))
          names(SP_reactive_input$SP_nielsen_facts) <- c("Metrics","CY","YA")
          SP_reactive_input$SP_nielsen_facts <- SP_reactive_input$SP_nielsen_facts[!(SP_reactive_input$SP_nielsen_facts$Metrics %in% c("Category","Manufacturer","Brand","Format","Sub Brand","PPG","SKU")),]
          SP_reactive_input$SP_nielsen_facts[,c("CY","YA")] <- as.numeric(unlist(SP_reactive_input$SP_nielsen_facts[,c("CY","YA")]))
          SP_reactive_input$SP_nielsen_facts$`Absolute Change` <- SP_reactive_input$SP_nielsen_facts$CY - SP_reactive_input$SP_nielsen_facts$YA
          SP_reactive_input$SP_nielsen_facts$`% Change` <- SP_reactive_input$SP_nielsen_facts$`Absolute Change`*100/SP_reactive_input$SP_nielsen_facts$YA
          
          #Fact Table at aggregated product level
          output$SP_summ_fact_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            agg_fact_table(SP_reactive_input$SP_nielsen_facts)
          })
          
          #Stacked area chart of base and increment sales including the trend line for base price and average price
          output$SP_summ_volume_price_chart <- renderPlotly({
            vol_price_area_chart(SP_reactive_input$SP_nielsen_weekly_CY)
          })
          
          #Grouping and aggregating current year sales to the selected level
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                            "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                            "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                            "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_base_value" = sum(`Base Value`),"CY_base_volume" = sum(`Base Units`)),by = .(Category)]
          SP_reactive_input$SP_promo_eff_agg$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_promo_eff_agg[SP_reactive_input$SP_promo_eff_agg$`CY_Value` != 0 & SP_reactive_input$SP_promo_eff_agg$`CY_Volume` != 0,]
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected_ya$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected_ya$Format %in% input$SP_summ_format,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                                           "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                                           "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                                           "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`),"YA_base_value" = sum(`Base Value`),"YA_base_volume" = sum(`Base Units`)),by = .(Category)]
          SP_reactive_input$SP_promo_eff_agg_ya$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_ya$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_promo_eff_agg_ya[SP_reactive_input$SP_promo_eff_agg_ya$`YA_Value` != 0 & SP_reactive_input$SP_promo_eff_agg_ya$`YA_Volume` != 0,]
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_promo_eff_agg_total <- left_join(SP_reactive_input$SP_promo_eff_agg,SP_reactive_input$SP_promo_eff_agg_ya,by = c("Category","Brand","Format"))
          
          #Promo sales split
          output$SP_promo_split <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            promo_split_chart_CY_YA(SP_reactive_input$SP_promo_eff_agg_total,"Format")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_promo_eff_agg_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_summ_format,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                               "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                               "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                               "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),
                                                                                                                                                                                                                                                                               "CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`)),by = .(Category,Date)]
          SP_reactive_input$SP_promo_eff_agg_weekly$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_weekly$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_weekly$CY_Price <- SP_reactive_input$SP_promo_eff_agg_weekly$CY_Value/SP_reactive_input$SP_promo_eff_agg_weekly$CY_Volume
          SP_reactive_input$SP_promo_eff_agg_weekly_CY <- SP_reactive_input$SP_promo_eff_agg_weekly[SP_reactive_input$SP_promo_eff_agg_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_promo_eff_agg_weekly$Date <= ymd(input$SP_summ_date_end),]
          output$SP_promo_WoW_split <- renderPlotly({
            promo_split_chart_weekly(SP_reactive_input$SP_promo_eff_agg_weekly_CY)
          })
          #SKU level table
          SP_reactive_input$SP_nielsen_selected_SKU <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format),.("CY_Value" = sum(Value), "CY_Volume" = sum(Units),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`), "CY_Value_Promo" = sum(Value) - sum(`Base Value`),"CY_Volume_Promo" = sum(Units) - sum(`Base Units`),
                                                                                                                                                                                                                                                                                                                                                                                                   "CY_Value_TPR" = (sum(`Value (C) (unsupported)`)*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`))),
                                                                                                                                                                                                                                                                                                                                                                                                   "CY_Volume_TPR" = (sum(`Units (C) (Unsupported)`)*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),
                                                                                                                                                                                                                                                                                                                                                                                                   "CY_Value_Display" = (sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`))*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`)),
                                                                                                                                                                                                                                                                                                                                                                                                   "CY_Volume_Display" = (sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`))*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU_bp <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen_selected$`5th Largest Regular Price` != 0),.("Base_Price" = mean(`5th Largest Regular Price`)),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU <- left_join(SP_reactive_input$SP_nielsen_selected_SKU,SP_reactive_input$SP_nielsen_selected_SKU_bp,by = c("Category","Manufacturer","Brand","Format","PPG","PPG_Description","EAN Code"))
          SP_reactive_input$SP_nielsen_selected_SKU$CY_Price <- round(SP_reactive_input$SP_nielsen_selected_SKU$CY_Value/SP_reactive_input$SP_nielsen_selected_SKU$CY_Volume,2)
          SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)] <- round(SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)],2)
          names(SP_reactive_input$SP_nielsen_selected_SKU) <- c("Category","Manufacturer","Brand","Format","PPG","PPG Description","EAN Code","Value Sales(GBP)","Volume Sales(Units)","Base Value(GBP)","Base Volume(Units)","Incremental Value(GBP)","Incremental Volume(Units)","Value Shelf(GBP)","Volume Shelf(Units)","Value Display &/or Feature(GBP)","Volume Display &/or Feature(Units)","Base Price","Average Price")
          output$SP_summ_SKU_table <- renderDataTable({
            sku_table(SP_reactive_input$SP_nielsen_selected_SKU)
          })
          
          #######################When Category, Manufacturer, Brand, Format and PPG are being selected#####################
        }else if(input$SP_summ_ppg != ""){
          
          #Grouping and aggregating current year sales to the selected level
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen_selected$PPG %in% input$SP_summ_ppg, .("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                            "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                            "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                            "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,`PPG`)]
          
          SP_reactive_input$SP_nielsen_grouped$CY_Price <- SP_reactive_input$SP_nielsen_grouped$CY_Value/SP_reactive_input$SP_nielsen_grouped$CY_Volume
          SP_reactive_input$SP_nielsen_grouped$val_share <- SP_reactive_input$SP_nielsen_grouped$CY_Value*100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Value)
          SP_reactive_input$SP_nielsen_grouped$vol_share <- SP_reactive_input$SP_nielsen_grouped$CY_Volume *100/sum(SP_reactive_input$SP_nielsen_grouped$CY_Volume)
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected_ya$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen_selected$PPG %in% input$SP_summ_ppg,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                                       "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                                       "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                                       "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`)),by = .(Category,`PPG`)]
          SP_reactive_input$SP_nielsen_grouped_ya <- SP_reactive_input$SP_nielsen_grouped_ya[SP_reactive_input$SP_nielsen_grouped_ya$`YA_Value` != 0 & SP_reactive_input$SP_nielsen_grouped_ya$`YA_Volume` != 0,]
          SP_reactive_input$SP_nielsen_grouped_ya$YA_Price <- SP_reactive_input$SP_nielsen_grouped_ya$YA_Value/SP_reactive_input$SP_nielsen_grouped_ya$YA_Volume
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_nielsen_grouped_total <- left_join(SP_reactive_input$SP_nielsen_grouped,SP_reactive_input$SP_nielsen_grouped_ya,by = c("Category","PPG"))
          SP_reactive_input$SP_nielsen_grouped_total <- SP_reactive_input$SP_nielsen_grouped_total[SP_reactive_input$SP_nielsen_grouped_total$`CY_Value` != 0 & SP_reactive_input$SP_nielsen_grouped_total$`CY_Volume` != 0 & SP_reactive_input$SP_nielsen_grouped_total$YA_Value !=0 & SP_reactive_input$SP_nielsen_grouped_total$YA_Volume != 0,]
          #Calculating % change in Value, Volume and price
          SP_reactive_input$SP_nielsen_grouped_total$vol_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Volume
          SP_reactive_input$SP_nielsen_grouped_total$val_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Value
          SP_reactive_input$SP_nielsen_grouped_total$price_chg <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/SP_reactive_input$SP_nielsen_grouped_total$YA_Price
          SP_reactive_input$SP_nielsen_grouped_total[is.na(SP_reactive_input$SP_nielsen_grouped_total)] <- 0
          SP_reactive_input$SP_nielsen_grouped <- SP_reactive_input$SP_nielsen_grouped[SP_reactive_input$SP_nielsen_grouped$`CY_Value` != 0 & SP_reactive_input$SP_nielsen_grouped$`CY_Volume` != 0,]
          if(nrow(SP_reactive_input$SP_nielsen_grouped) == 0){
            SP_reactive_input$SP_nielsen_grouped <- NULL
          }
          output$SP_summ_value_share_plot <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_nielsen_grouped,"No Sales for this PPG in the selected period")
            )
            value_share_plot(SP_reactive_input$SP_nielsen_grouped,"PPG",input$SP_brand)
          })
          if(is.null(SP_reactive_input$SP_nielsen_grouped_total)){
            SP_reactive_input$SP_nielsen_grouped_total <- NULL
          }else if(nrow(SP_reactive_input$SP_nielsen_grouped_total) == 0){
            SP_reactive_input$SP_nielsen_grouped_total <- NULL
          }
          # #Year on Year % change in Value Sales
          # output$SP_summ_value_yoy_chg <- renderPlotly({
          #   validate(
          #     need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
          #   )
          #   value_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,"PPG")
          # })
          
          #Volume Share Plot
          output$SP_summ_volume_share_plot <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_nielsen_grouped,"No Sales for this PPG in the selected period")
            )
            volume_share_plot(SP_reactive_input$SP_nielsen_grouped,"PPG")
          })
          
          # #Year on Year % change in Volume Sales
          # output$SP_summ_volume_yoy_chg <- renderPlotly({
          #   validate(
          #     need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
          #   )
          #   volume_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,"PPG")
          # })
          
          
          # #Year on Year % change in Price
          # output$SP_summ_price_yoy_chg <- renderPlotly({
          #   validate(
          #     need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
          #   )
          #   price_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,"PPG")
          # })
          
          # % change in Volume versus % change in Price
          output$SP_summ_bubble_chart <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            validate(
              need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
            )
            vol_price_bubble_chart(SP_reactive_input$SP_nielsen_grouped_total,"PPG")
          })
          
          #Value and Volume sales for CY,YA table
          
          if(!(nrow(SP_reactive_input$SP_nielsen_grouped_total) == 0 | is.null(SP_reactive_input$SP_nielsen_grouped_total))){
            SP_reactive_input$SP_nielsen_CY_YA_table <- SP_reactive_input$SP_nielsen_grouped_total[,c("PPG","CY_Volume","YA_Volume","vol_chg","CY_Price","YA_Price","price_chg")]
            SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")] <- SP_reactive_input$SP_nielsen_CY_YA_table[,c("CY_Volume","YA_Volume")]/1000
            names(SP_reactive_input$SP_nielsen_CY_YA_table) <- c("PPG","CY Volume('000 Units)","YA Volume('000 Units)","% Volume Chg","CY Price","YA Price","% Price Chg")
          }
          output$SP_summ_cy_ya_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            validate(
              need(SP_reactive_input$SP_nielsen_CY_YA_table, "No Sales for this PPG in the selected period")
            )
            cy_ya_table(SP_reactive_input$SP_nielsen_CY_YA_table)
          })
          
          #Each lower level contribution to % change in Value sales at aggregated level
          SP_reactive_input$SP_nielsen_grouped_total <- data.table(SP_reactive_input$SP_nielsen_grouped_total)
          SP_reactive_input$SP_nielsen_total_val_chg <- SP_reactive_input$SP_nielsen_grouped_total[,.("CY_Value" = sum(SP_reactive_input$SP_nielsen_grouped_total$CY_Value),"CY_Volume"= sum(SP_reactive_input$SP_nielsen_grouped_total$CY_Volume),
                                                                                                      "YA_Value"=sum(SP_reactive_input$SP_nielsen_grouped_total$YA_Value),"YA_Volume"=sum(SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)),by=c("Category","PPG")]
          
          SP_reactive_input$SP_nielsen_total_val_chg$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_total_val_chg$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_nielsen_total_val_chg$CY_Price <- SP_reactive_input$SP_nielsen_total_val_chg$CY_Value/SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$YA_Price <- SP_reactive_input$SP_nielsen_total_val_chg$YA_Value/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$val_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Value - SP_reactive_input$SP_nielsen_total_val_chg$YA_Value)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          SP_reactive_input$SP_nielsen_total_val_chg$vol_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Volume - SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          SP_reactive_input$SP_nielsen_total_val_chg$prc_chg <- (SP_reactive_input$SP_nielsen_total_val_chg$CY_Price - SP_reactive_input$SP_nielsen_total_val_chg$YA_Price)*100/SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          #SP_reactive_input$SP_nielsen_grouped_total$val_chg_normalized <- SP_reactive_input$SP_nielsen_grouped_total$val_chg/(sum(SP_reactive_input$SP_nielsen_grouped_total$val_chg))
          SP_reactive_input$SP_nielsen_grouped_total$total_val_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Value - SP_reactive_input$SP_nielsen_grouped_total$YA_Value)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Value
          
          #SP_reactive_input$SP_nielsen_grouped_total$vol_chg_normalized <- SP_reactive_input$SP_nielsen_grouped_total$vol_chg/(sum(SP_reactive_input$SP_nielsen_grouped_total$vol_chg))
          SP_reactive_input$SP_nielsen_grouped_total$total_vol_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Volume - SP_reactive_input$SP_nielsen_grouped_total$YA_Volume)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Volume
          
          #SP_reactive_input$SP_nielsen_grouped_total$prc_chg_normalized <- SP_reactive_input$SP_nielsen_grouped_total$price_chg/(sum(SP_reactive_input$SP_nielsen_grouped_total$price_chg))
          SP_reactive_input$SP_nielsen_grouped_total$total_prc_chg_contri <- (SP_reactive_input$SP_nielsen_grouped_total$CY_Price - SP_reactive_input$SP_nielsen_grouped_total$YA_Price)*100/ SP_reactive_input$SP_nielsen_total_val_chg$YA_Price
          
          if((nrow(SP_reactive_input$SP_nielsen_grouped_total) == 0 | is.null(SP_reactive_input$SP_nielsen_grouped_total)) & (nrow(SP_reactive_input$SP_nielsen_total_val_chg) == 0 | is.null(SP_reactive_input$SP_nielsen_total_val_chg))){
            SP_reactive_input$SP_nielsen_total_val_chg <- NULL
            SP_reactive_input$SP_nielsen_grouped_total <- NULL
          }
          #Absolute Price plot
          output$SP_summ_price_abs_plot <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
            )
            price_abs_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","PPG")
          })
          
          
          #Year on Year % change in Value Sales
          output$SP_summ_value_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            validate(
              need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
            )
            value_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","PPG")
          })
          
          #Year on Year % change in Volume Sales
          output$SP_summ_volume_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            validate(
              need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
            )
            volume_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","PPG")
          })
          
          #Year on Year % change in Price
          output$SP_summ_price_yoy_chg <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            validate(
              need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
            )
            price_yoy_chg_plot(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","PPG")
          })
          
          #Value share contribution to aggregate level
          output$SP_summ_val_share_contri <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_nielsen_grouped_total,"No Sales for this PPG in the selected period")
            )
            val_share_contri_chart(SP_reactive_input$SP_nielsen_grouped_total,SP_reactive_input$SP_nielsen_total_val_chg,"PPG","PPG")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_nielsen_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen$PPG %in% input$SP_summ_ppg,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "TDP" = sum(`ACV Distribution`),
                                                                                                                                                                                                                                                                      "WD" = max(`ACV Distribution`),"ND" = max(`Num Selling Dist`),"value_promo" = sum(`Value (C) (any promo)`), 
                                                                                                                                                                                                                                                                      "volume_promo" = sum(`Units (C) (any promo)`),"base_value" = sum(`Base Value`),"base_volume" = sum(`Base Units`),"promo_flag" = max(`Promo_Flag`)),by = .(Category,PPG,Date)]
          SP_reactive_input$SP_nielsen_weekly$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_weekly$Format <- paste(input$SP_summ_format,collapse = "+")
          
          SP_reactive_input$SP_nielsen_weekly_bp <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen$PPG %in% input$SP_summ_ppg & SP_reactive_input$SP_nielsen$`5th Largest Regular Price` != 0,.("base_price" = mean(`5th Largest Regular Price`)),by = .(Category,PPG,Date)]
          SP_reactive_input$SP_nielsen_weekly_bp$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_weekly_bp$Format <- paste(input$SP_summ_format,collapse = "+")
          
          SP_reactive_input$SP_nielsen_weekly <- data.table(left_join(SP_reactive_input$SP_nielsen_weekly,SP_reactive_input$SP_nielsen_weekly_bp,by = c("Category","Brand","Format","PPG","Date")))
          is.na(SP_reactive_input$SP_nielsen_weekly$base_price) <- 0
          
          SP_reactive_input$SP_nielsen_weekly$CY_Price <- SP_reactive_input$SP_nielsen_weekly$CY_Value/SP_reactive_input$SP_nielsen_weekly$CY_Volume
          #SP_reactive_input$SP_nielsen_bp_disc <- base_price_calc(SP_reactive_input$SP_nielsen_weekly$CY_Price)
          SP_reactive_input$SP_nielsen_weekly$discount <- (1-(SP_reactive_input$SP_nielsen_weekly$CY_Price/SP_reactive_input$SP_nielsen_weekly$base_price))*100
          SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$promo_flag ==0,"discount"] <- 0
          #SP_reactive_input$SP_nielsen_weekly <- cbind(SP_reactive_input$SP_nielsen_weekly,SP_reactive_input$SP_nielsen_bp_disc)
          
          SP_reactive_input$SP_nielsen_weekly_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),]
          SP_reactive_input$SP_nielsen_weekly_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],]
          
          
          SP_reactive_input$SP_nielsen_agg_product_CY <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= ymd(input$SP_summ_date_start) & SP_reactive_input$SP_nielsen_weekly$Date <= ymd(input$SP_summ_date_end),.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6, 
                                                                                                                                                                                                                                                   "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category,PPG)]
          SP_reactive_input$SP_nielsen_agg_product_CY$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_CY$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_YA <- SP_reactive_input$SP_nielsen_weekly[SP_reactive_input$SP_nielsen_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_summ_date_start))] & SP_reactive_input$SP_nielsen_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_summ_date_end))],.("Value Sales(Million GBP)" = sum(CY_Value)/10^6, "Volume Sales('000 Units)" = sum(CY_Volume)/10^3,"Value Sales Promo(Million GBP)" = (sum(CY_Value)-sum(base_value))/10^6, 
                                                                                                                                                                                                                                                                                                                                                                                                                            "Volume Sales Promo('000 Units)" = (sum(CY_Volume)-sum(base_volume))/10^3,"Value Sales Base(Million GBP)" = sum(base_value)/10^6,"Volume Sales Base('000 Units)" = sum(base_volume)/10^3,"Base Price" = mean(base_price)),by = .(Category,PPG)]
          SP_reactive_input$SP_nielsen_agg_product_YA$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_YA$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_nielsen_agg_product_CY$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_CY$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_CY$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_agg_product_YA$`Average Price` = SP_reactive_input$SP_nielsen_agg_product_YA$`Value Sales(Million GBP)`*1000/SP_reactive_input$SP_nielsen_agg_product_YA$`Volume Sales('000 Units)`
          SP_reactive_input$SP_nielsen_facts <- data.frame("Metrics"=names(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_CY),transpose(SP_reactive_input$SP_nielsen_agg_product_YA))
          names(SP_reactive_input$SP_nielsen_facts) <- c("Metrics","CY","YA")
          SP_reactive_input$SP_nielsen_facts <- SP_reactive_input$SP_nielsen_facts[!(SP_reactive_input$SP_nielsen_facts$Metrics %in% c("Category","Manufacturer","Brand","Format","Sub Brand","PPG","SKU")),]
          SP_reactive_input$SP_nielsen_facts[,c("CY","YA")] <- as.numeric(unlist(SP_reactive_input$SP_nielsen_facts[,c("CY","YA")]))
          SP_reactive_input$SP_nielsen_facts$`Absolute Change` <- SP_reactive_input$SP_nielsen_facts$CY - SP_reactive_input$SP_nielsen_facts$YA
          SP_reactive_input$SP_nielsen_facts$`% Change` <- SP_reactive_input$SP_nielsen_facts$`Absolute Change`*100/SP_reactive_input$SP_nielsen_facts$YA
          # if(all(SP_reactive_input$SP_nielsen_facts[SP_reactive_input$SP_nielsen_facts$Metrics %in% c("Value Sales(Million GBP)","Volume Sales('000 Units)"),]$CY == 0)){
          #   SP_reactive_input$SP_nielsen_facts <- NULL
          # }
          #Fact Table at aggregated product level
          output$SP_summ_fact_table <- renderDataTable({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            agg_fact_table(SP_reactive_input$SP_nielsen_facts)
          })
          
          if(all(SP_reactive_input$SP_nielsen_weekly_CY$CY_Value == 0 & SP_reactive_input$SP_nielsen_weekly_CY$CY_Volume == 0)){
            SP_reactive_input$SP_nielsen_weekly_CY <- NULL
          }
          #Stacked area chart of base and increment sales including the trend line for base price and average price
          output$SP_summ_volume_price_chart <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_nielsen_weekly_CY,"No Sales for this PPG in the selected time period")
            )
            vol_price_area_chart(SP_reactive_input$SP_nielsen_weekly_CY)
          })
          
          #Discount histogram, TDP and weighted distribution chart
          # output$SP_summ_disc_dist_chart <- renderPlotly({
          #   plot_ly(SP_reactive_input$SP_nielsen_weekly_CY) %>% 
          #     add_trace(x = ~Date, y = ~discount, type = 'bar',name = 'Discount') %>%
          #     add_trace(x = ~Date, y = ~TDP,type = 'scatter', mode = 'lines',yaxis = 'y2',line = list(color = '#45171D'),name = 'TDP') %>%
          #     add_trace(x = ~Date, y = ~(WD/2),type = 'scatter', mode = 'lines',name = 'Wt. Distribution') %>%
          #     layout(margin = list(r= 50),xaxis = list(title = 'Time Period'),yaxis = list(side = 'right',title = "Distribution/ Discount %", zeroline = FALSE,range=c(0,100)),yaxis2 = list(side = 'left', overlaying = "y",title = "Total Distribution Points", showgrid = FALSE, zeroline = FALSE,range=c(0,8000)),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
          # })
          #Grouping and aggregating current year sales to the selected level
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_nielsen_selected[SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen_selected$PPG %in% input$SP_summ_ppg,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                                                                                               "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                                                                                               "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                                                                                               "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),"CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_base_value" = sum(`Base Value`),"CY_base_volume" = sum(`Base Units`)),by = .(Category,PPG)]
          SP_reactive_input$SP_promo_eff_agg$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg <- SP_reactive_input$SP_promo_eff_agg[SP_reactive_input$SP_promo_eff_agg$`CY_Value` != 0 & SP_reactive_input$SP_promo_eff_agg$`CY_Volume` != 0,]
          #SP_reactive_input$SP_promo_eff_agg$CY_Price <- SP_reactive_input$SP_promo_eff_agg$CY_Value/SP_reactive_input$SP_promo_eff_agg$CY_Volume
          #SP_reactive_input$SP_promo_eff_agg$val_share <- SP_reactive_input$SP_promo_eff_agg$CY_Value*100/sum(SP_reactive_input$SP_promo_eff_agg$CY_Value)
          #SP_reactive_input$SP_promo_eff_agg$vol_share <- SP_reactive_input$SP_promo_eff_agg$CY_Volume *100/sum(SP_reactive_input$SP_promo_eff_agg$CY_Volume)
          
          #Grouping and aggregating year ago sales to the selected level
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_nielsen_selected_ya[SP_reactive_input$SP_nielsen_selected_ya$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected_ya$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected_ya$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen_selected_ya$PPG %in% input$SP_summ_ppg,.("YA_Value" = sum(Value), "YA_Volume" = sum(Units),"YA_Value_Promo" = sum(`Value (C) (any promo)`),"YA_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                                                                                                                 "YA_Value_Disp"=sum(`Value (C) (display only)`), "YA_Volume_Disp" = sum(`Units (C) (display only)`),"YA_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                                                                                                                 "YA_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"YA_Value_Feat"=sum(`Value (C) (feature only)`),"YA_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                                                                                                                 "YA_Value_Unsupp"=sum(`Value (C) (unsupported)`),"YA_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"YA_Value_Multi"=sum(`Value (C) (total multibuy)`),"YA_Volume_Multi"=sum(`Units (C) (total multibuy)`),"YA_base_value" = sum(`Base Value`),"YA_base_volume" = sum(`Base Units`)),by = .(Category,PPG)]
          SP_reactive_input$SP_promo_eff_agg_ya$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_ya$Format <- paste(input$SP_summ_format,collapse = "+")
          
          SP_reactive_input$SP_promo_eff_agg_ya <- SP_reactive_input$SP_promo_eff_agg_ya[SP_reactive_input$SP_promo_eff_agg_ya$`YA_Value` != 0 & SP_reactive_input$SP_promo_eff_agg_ya$`YA_Volume` != 0,]
          #SP_reactive_input$SP_promo_eff_agg_ya$YA_Price <- SP_reactive_input$SP_promo_eff_agg_ya$YA_Value/SP_reactive_input$SP_promo_eff_agg_ya$YA_Volume
          
          #Joining Current year and year ago columns
          SP_reactive_input$SP_promo_eff_agg_total <- left_join(SP_reactive_input$SP_promo_eff_agg,SP_reactive_input$SP_promo_eff_agg_ya,by = c("Category","Brand","Format","PPG"))
          if(nrow(SP_reactive_input$SP_promo_eff_agg_total) == 0 | is.null(SP_reactive_input$SP_promo_eff_agg_total)){
            SP_reactive_input$SP_promo_eff_agg_total <- NULL
          }
          #Promo sales split
          output$SP_promo_split <- renderPlotly({
            validate(
              need(SP_reactive_input$YA_start_check,"Start Date is out of bounds for Year ago calculation")
            )
            validate(
              need(SP_reactive_input$SP_promo_eff_agg_total,"No Sales in Current year")
            )
            promo_split_chart_CY_YA(SP_reactive_input$SP_promo_eff_agg_total,"PPG")
          })
          
          #Weekly Trend Tab
          SP_reactive_input$SP_promo_eff_agg_weekly <- SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen$PPG %in% input$SP_summ_ppg,.("CY_Value" = sum(Value), "CY_Volume" = sum(Units), "CY_Value_Promo" = sum(`Value (C) (any promo)`),"CY_Volume_Promo" = sum(`Units (C) (any promo)`),
                                                                                                                                                                                                                                                                                                                                         "CY_Value_Disp"=sum(`Value (C) (display only)`), "CY_Volume_Disp" = sum(`Units (C) (display only)`),"CY_Value_FeatDisp"=sum(`Value (C) (feature and display)`),
                                                                                                                                                                                                                                                                                                                                         "CY_Volume_FeatDisp"=sum(`Units (C) (feature and display)`),"CY_Value_Feat"=sum(`Value (C) (feature only)`),"CY_Volume_Feat"=sum(`Units (C) (Unsupported)`),
                                                                                                                                                                                                                                                                                                                                         "CY_Value_Unsupp"=sum(`Value (C) (unsupported)`),"CY_Volume_Unsupp"=sum(`Units (C) (Unsupported)`),"CY_Value_Multi"=sum(`Value (C) (total multibuy)`),
                                                                                                                                                                                                                                                                                                                                         "CY_Volume_Multi"=sum(`Units (C) (total multibuy)`),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`)),by = .(Category,PPG,Date)]
          SP_reactive_input$SP_promo_eff_agg_weekly$Brand <- paste(input$SP_summ_brand,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_weekly$Format <- paste(input$SP_summ_format,collapse = "+")
          SP_reactive_input$SP_promo_eff_agg_weekly$CY_Price <- SP_reactive_input$SP_promo_eff_agg_weekly$CY_Value/SP_reactive_input$SP_promo_eff_agg_weekly$CY_Volume
          #SP_reactive_input$SP_promo_eff_agg_bp_disc <- base_price_calc(SP_reactive_input$SP_promo_eff_agg_weekly$CY_Price)
          #SP_reactive_input$SP_promo_eff_agg_weekly <- cbind(SP_reactive_input$SP_promo_eff_agg_weekly,SP_reactive_input$SP_promo_eff_agg_bp_disc)
          
          SP_reactive_input$SP_promo_eff_agg_weekly_CY <- SP_reactive_input$SP_promo_eff_agg_weekly[SP_reactive_input$SP_promo_eff_agg_weekly$Date >= ymd(input$SP_cal_date_start) & SP_reactive_input$SP_promo_eff_agg_weekly$Date <= ymd(input$SP_cal_date_end),]
          
          #SP_reactive_input$SP_promo_eff_agg_weekly_YA <- SP_reactive_input$SP_promo_eff_agg_weekly[SP_reactive_input$SP_promo_eff_agg_weekly$Date >= SP_reactive_input$ya_start_week[weekdays(SP_reactive_input$ya_start_week)== weekdays(ymd(input$SP_cal_date_start))] & SP_reactive_input$SP_promo_eff_agg_weekly$Date <= SP_reactive_input$ya_end_week[weekdays(SP_reactive_input$ya_end_week)==weekdays(ymd(input$SP_cal_date_end))],]
          if(all(SP_reactive_input$SP_promo_eff_agg_weekly_CY$CY_Value == 0)){
            SP_reactive_input$SP_promo_eff_agg_weekly_CY <- NULL
          }
          output$SP_promo_WoW_split <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_promo_eff_agg_weekly_CY,"No Sales in Current year")
            )
            promo_split_chart_weekly(SP_reactive_input$SP_promo_eff_agg_weekly_CY)
          })
          
          #SKU level table
          SP_reactive_input$SP_nielsen_selected_SKU <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen_selected$PPG %in% input$SP_summ_ppg),.("CY_Value" = sum(Value), "CY_Volume" = sum(Units),"CY_Value_Base" = sum(`Base Value`),"CY_Volume_Base" = sum(`Base Units`), "CY_Value_Promo" = sum(Value) - sum(`Base Value`),"CY_Volume_Promo" = sum(Units) - sum(`Base Units`),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "CY_Value_TPR" = (sum(`Value (C) (unsupported)`)*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`))),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "CY_Volume_TPR" = (sum(`Units (C) (Unsupported)`)*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "CY_Value_Display" = (sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`))*(sum(Value) - sum(`Base Value`))/(sum(`Value (C) (display only)`) + sum(`Value (C) (feature and display)`) + sum(`Value (C) (feature only)`) + sum(`Value (C) (total multibuy)`) + sum(`Value (C) (unsupported)`)),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "CY_Volume_Display" = (sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`))*(sum(Units) - sum(`Base Units`))/(sum(`Units (C) (display only)`) + sum(`Units (C) (feature and display)`) + sum(`Units (C) (feature only)`) + sum(`Units (C) (total multibuy)`) + sum(`Units (C) (Unsupported)`))),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU_bp <- SP_reactive_input$SP_nielsen_selected[(SP_reactive_input$SP_nielsen_selected$Category == input$SP_summ_cat & SP_reactive_input$SP_nielsen_selected$Manufacturer %in% input$SP_summ_manuf & SP_reactive_input$SP_nielsen_selected$Brand %in% input$SP_summ_brand & SP_reactive_input$SP_nielsen_selected$Format %in% input$SP_summ_format & SP_reactive_input$SP_nielsen_selected$PPG %in% input$SP_summ_ppg & SP_reactive_input$SP_nielsen_selected$`5th Largest Regular Price` != 0),.("Base_Price" = mean(`5th Largest Regular Price`)),by = .(Category,Manufacturer,Brand,Format,`PPG`,PPG_Description,`EAN Code`)]
          SP_reactive_input$SP_nielsen_selected_SKU <- left_join(SP_reactive_input$SP_nielsen_selected_SKU,SP_reactive_input$SP_nielsen_selected_SKU_bp,by = c("Category","Manufacturer","Brand","Format","PPG","PPG_Description","EAN Code"))
          #SP_reactive_input$SP_nielsen_selected_SKU[,grep("Value",names(SP_reactive_input$SP_nielsen_selected_SKU))] <- SP_reactive_input$SP_nielsen_selected_SKU[,grep("Value",names(SP_reactive_input$SP_nielsen_selected_SKU))]/10^6
          #SP_reactive_input$SP_nielsen_selected_SKU[,grep("Volume",names(SP_reactive_input$SP_nielsen_selected_SKU))] <- SP_reactive_input$SP_nielsen_selected_SKU[,grep("Volume",names(SP_reactive_input$SP_nielsen_selected_SKU))]/10^3
          SP_reactive_input$SP_nielsen_selected_SKU$CY_Price <- round(SP_reactive_input$SP_nielsen_selected_SKU$CY_Value/SP_reactive_input$SP_nielsen_selected_SKU$CY_Volume,2)
          SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)] <- round(SP_reactive_input$SP_nielsen_selected_SKU[,sapply(SP_reactive_input$SP_nielsen_selected_SKU, is.numeric)],2)
          names(SP_reactive_input$SP_nielsen_selected_SKU) <- c("Category","Manufacturer","Brand","Format","PPG","PPG Description","EAN Code","Value Sales(GBP)","Volume Sales(Units)","Base Value(GBP)","Base Volume(Units)","Incremental Value(GBP)","Incremental Volume(Units)","Value Shelf(GBP)","Volume Shelf(Units)","Value Display &/or Feature(GBP)","Volume Display &/or Feature(Units)","Base Price","Average Price")
          
          output$SP_summ_SKU_table <- renderDataTable({
            sku_table(SP_reactive_input$SP_nielsen_selected_SKU)
          })
          
        }
      })
    
    ###Updating filters in Promotion Effectiveness tab
    observeEvent({c(input$SP_summ_top_panel_right,
                    #input$SP_level,
                    input$SP_proceed_summ)},{
                      
                      updateSelectizeInput(session,"SP_cal_cat",selected = unique(SP_reactive_input$cal_sample$Category)[1],choices = unique(SP_reactive_input$cal_sample$Category))
                      
                      observeEvent({
                        input$SP_cal_cat},{
                          updateSelectizeInput(session,"SP_cal_brand",choices = unique(SP_reactive_input$cal_sample[SP_reactive_input$cal_sample$Category == input$SP_cal_cat,]$Brand))
                        }
                      )
                      
                      observeEvent({
                        input$SP_cal_cat
                        input$SP_cal_brand},{
                          updateSelectizeInput(session,"SP_cal_format",choices = unique(SP_reactive_input$cal_sample[SP_reactive_input$cal_sample$Category == input$SP_cal_cat & SP_reactive_input$cal_sample$Brand %in% input$SP_cal_brand,]$Format))
                        })
                      
                      observeEvent({
                        input$SP_cal_cat
                        input$SP_cal_format
                        input$SP_cal_brand},{
                          updateSelectizeInput(session,"SP_cal_ppg",choices = unique(SP_reactive_input$cal_sample[SP_reactive_input$cal_sample$Category == input$SP_cal_cat & SP_reactive_input$cal_sample$Brand %in% input$SP_cal_brand & SP_reactive_input$cal_sample$Format %in% input$SP_cal_format,]$`PPG`))
                        }
                      )
                      
                    })
    
    observeEvent(c(
      input$SP_cal_cat,
      #input$SP_level,
      input$SP_cal_brand,
      input$SP_cal_format,
      input$SP_cal_ppg,
      input$SP_promo_ROI_selection),{
        SP_reactive_input$cal_sample_selected <- data.table(SP_reactive_input$cal_sample)
        # 
        if(!(is.null(SP_reactive_input$folder_path))){
          #write.csv(SP_reactive_input$cal_sample_selected, paste0(SP_reactive_input$folder_path,"/","PPG Attributes.csv"),row.names = FALSE)
        }
        #When Brand, Format and PPG are empty, populating the graphs
        if((input$SP_cal_cat != "") & all(input$SP_cal_brand == "") & all(input$SP_cal_format == "") & all(input$SP_cal_ppg == "")){
          #Grouping and aggregating current year sales to the next lower level
          
          SP_reactive_input$SP_promo_eff <- SP_reactive_input$cal_sample_selected[SP_reactive_input$cal_sample_selected$Category == input$SP_cal_cat,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Total_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),2),"Uplift" = round(sum(Inc_units)/sum(Base_Units),2), "Incremental NR ROI" = round(sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc),2)),by = .(Category,Brand)]
          
          output$SP_promo_fact_table <- renderDataTable({
            promo_split_fact_table(SP_reactive_input$SP_promo_eff,"Brand")
          })
          
          output$SP_promo_ROI <- renderPlotly({
            promo_ROI_chart(SP_reactive_input$SP_promo_eff,"Brand",input$SP_promo_ROI_selection)
          })
        }else if(!(is.null(input$SP_cal_brand)) & all(input$SP_cal_format == "") & all(input$SP_cal_ppg == "")){
          
          SP_reactive_input$SP_promo_eff <- SP_reactive_input$cal_sample_selected[SP_reactive_input$cal_sample_selected$Category == input$SP_cal_cat & SP_reactive_input$cal_sample_selected$Brand %in% input$SP_cal_brand,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Total_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),2),"Uplift" = round(sum(Inc_units)/sum(Base_Units),2), "Incremental NR ROI" = round(sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc),2)),by = .(Category,Brand,Format)]
          output$SP_promo_fact_table <- renderDataTable({
            promo_split_fact_table(SP_reactive_input$SP_promo_eff,"Format")
          })
          
          output$SP_promo_ROI <- renderPlotly({
            promo_ROI_chart(SP_reactive_input$SP_promo_eff,"Format",input$SP_promo_ROI_selection)
          })
          
          #When only PPG is empty, populating the graphs
        }else if(!(is.null(input$SP_cal_format)) & all(input$SP_cal_ppg == "")){
          
          SP_reactive_input$SP_promo_eff <- SP_reactive_input$cal_sample_selected[SP_reactive_input$cal_sample_selected$Category == input$SP_cal_cat & SP_reactive_input$cal_sample_selected$Brand %in% input$SP_cal_brand & SP_reactive_input$cal_sample_selected$Format %in% input$SP_cal_format,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Total_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),2),"Uplift" = round(sum(Inc_units)/sum(Base_Units),2), "Incremental NR ROI" = round(sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc),2)),by = .(Category,Brand,Format,PPG)]
          output$SP_promo_fact_table <- renderDataTable({
            promo_split_fact_table(SP_reactive_input$SP_promo_eff,"PPG")
          })
          
          output$SP_promo_ROI <- renderPlotly({
            promo_ROI_chart(SP_reactive_input$SP_promo_eff,"PPG",input$SP_promo_ROI_selection)
          })
          
        }else if(all(input$SP_cal_ppg != "") & !(is.null(input$SP_cal_ppg))){
          #
          SP_reactive_input$SP_promo_eff <- SP_reactive_input$cal_sample_selected[SP_reactive_input$cal_sample_selected$PPG %in% input$SP_cal_ppg,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Total_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),2),"Uplift" = round(sum(Inc_units)/sum(Base_Units),2), "Incremental NR ROI" = round(sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc),2)),by = .(PPG,Event_Number)]
          if(all(is.na(SP_reactive_input$SP_promo_eff$ROI)) & all(is.na(SP_reactive_input$SP_promo_eff$Investment))){
            SP_reactive_input$SP_promo_eff <- NULL
          }
          SP_reactive_input$SP_promo_eff$Event_Number <- paste0("Event-",SP_reactive_input$SP_promo_eff$Event_Number)
          setnames(SP_reactive_input$SP_promo_eff,"Event_Number","Event ID")
          
          #SP_reactive_input$SP_promo_eff[SP_reactive_input$SP_promo_eff$Event == "TPR",]$Event == "Shelf Promotion"
          #SP_reactive_input$SP_promo_eff[SP_reactive_input$SP_promo_eff$Event == "Display",]$Event == "Display &/or Feature Promotion"
          output$SP_promo_fact_table <- renderDataTable({
            validate(
              need(SP_reactive_input$SP_promo_eff,"No Promotion happened for the selected PPG within selected time period")
            )
            promo_split_fact_table(SP_reactive_input$SP_promo_eff,"Event ID")
          })
          
          output$SP_promo_ROI <- renderPlotly({
            validate(
              need(SP_reactive_input$SP_promo_eff,"No Promotion happened for the selected PPG within selected time period")
            )
            promo_ROI_chart(SP_reactive_input$SP_promo_eff,"Event ID",input$SP_promo_ROI_selection)
          })
        }
      })
    output$SP_cal_top_panel_download <- downloadHandler(
      filename = function() {
        paste("PPG Attributes.csv")
      },
      content = function(file) {
        write.csv(SP_reactive_input$cal_sample_selected, file,row.names = FALSE)
      }
    )
    
    #################Updating Promo calendar##############
    observeEvent(c(input$SP_cal_cat,
                   #input$SP_level,
                   input$SP_cal_brand,
                   input$SP_cal_format,
                   input$SP_promo_cal_selection),{
                     if(all(input$SP_cal_brand == "") & all(input$SP_cal_format == "") & all(input$SP_cal_ppg == "")){
                       SP_reactive_input$event_file_filtered <- SP_reactive_input$event_file[SP_reactive_input$event_file$Category == input$SP_cal_cat ,]
                     }else if(all(input$SP_cal_format == "") & all(input$SP_cal_ppg == "")){
                       SP_reactive_input$event_file_filtered <- SP_reactive_input$event_file[SP_reactive_input$event_file$Category == input$SP_cal_cat & SP_reactive_input$event_file$Brand %in% input$SP_cal_brand ,]
                     }else if(all(input$SP_cal_ppg == "")){
                       
                       SP_reactive_input$event_file_filtered <- SP_reactive_input$event_file[SP_reactive_input$event_file$Category == input$SP_cal_cat & SP_reactive_input$event_file$Brand %in% input$SP_cal_brand & SP_reactive_input$event_file$Format %in% input$SP_cal_format,]
                     }
                     event_file_bckp <- SP_reactive_input$event_file_filtered
                     
                     if(!(is.null(SP_reactive_input$event_file_filtered))){
                       if(nrow(SP_reactive_input$event_file_filtered) != 0){
                         SP_reactive_input$event_file_filtered_1 <- left_join(SP_reactive_input$event_file_filtered,SP_reactive_input$cal_sample[,c("Event_Number","PPG","Promo Price","R_ROI_GM")],by = c("Event_Number","PPG"))
                         
                         SP_reactive_input$event_file_filtered_1[SP_reactive_input$event_file_filtered_1$Promo_Flag == 0,]$`Promo Price` <- ""
                         SP_reactive_input$event_file_filtered_1[SP_reactive_input$event_file_filtered_1$Tesco_Event == "No Promo",]$`Promo Price` <- ""
                         SP_reactive_input$event_file_filtered_1 <- data.table(SP_reactive_input$event_file_filtered_1)
                         #SP_reactive_input$event_file_filtered_1[SP_reactive_input$event_file_filtered_1$Tesco_Event != "No Promo",tile := cut(ROI,breaks = quantile(ROI,probs = 0:3/3), labels = 1:3,include.lowest = TRUE)]
                         
                         SP_reactive_input$event_file_filtered_1[SP_reactive_input$event_file_filtered_1$Tesco_Event == "TPR",]$Tesco_Event <- "Shelf Promotion"
                         SP_reactive_input$event_file_filtered_1[SP_reactive_input$event_file_filtered_1$Tesco_Event == "Display",]$Tesco_Event <- "Display Feature Promotion"
                         SP_reactive_input$event_file_filtered_1$ROI_cal <- SP_reactive_input$event_file_filtered_1$R_ROI_GM
                         SP_reactive_input$event_file_filtered_1[SP_reactive_input$event_file_filtered_1$Tesco_Event == "No Promo",]$ROI_cal <- -100
                         SP_reactive_input$brks_ROI <- quantile(SP_reactive_input$event_file_filtered_1[SP_reactive_input$event_file_filtered_1$Tesco_Event != "No Promo" & SP_reactive_input$event_file_filtered_1$ROI_cal >= 0,]$ROI_cal, seq(0, 1, .05), na.rm = TRUE)
                         
                         #SP_reactive_input$cal_sample_RSP <- SP_reactive_input$cal_sample[,.("RSP" = (RSP))]
                         latest_week <- max(SP_reactive_input$event_file_filtered_1$Date)
                         SP_reactive_input$event_file_filtered_1 <- SP_reactive_input$event_file_filtered_1[,RSP_latest := mean(RSP[Date == latest_week]),by = .(PPG,Format,Brand,Category)]
                         SP_reactive_input$event_file_filtered_1<- as.data.table(SP_reactive_input$event_file_filtered_1)
                          
                         SP_reactive_input$event_file_dcast <- data.table::dcast(unique(SP_reactive_input$event_file_filtered_1[,c("Format","PPG","PPG_Description","Week No","Promo Price","RSP_latest","Event","ROI_cal")]),Format + PPG + PPG_Description + RSP_latest~`Week No`,value.var= c("Promo Price","Event","ROI_cal"))
                         
                         SP_reactive_input$event_file_dcast_ROI <- SP_reactive_input$event_file_dcast
                         names(SP_reactive_input$event_file_dcast) <- gsub("Promo Price_","",names(SP_reactive_input$event_file_dcast))
                         names(SP_reactive_input$event_file_dcast) <- gsub("Tesco_","",names(SP_reactive_input$event_file_dcast))
                         
                         names(SP_reactive_input$event_file_dcast_ROI) <- gsub("ROI_cal_","",names(SP_reactive_input$event_file_dcast_ROI))
                         names(SP_reactive_input$event_file_dcast_ROI) <- gsub("Tesco_","",names(SP_reactive_input$event_file_dcast_ROI))
                         setcolorder(SP_reactive_input$event_file_dcast,c("Format","PPG","PPG_Description","RSP_latest",sort(unique(SP_reactive_input$event_file_filtered$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$event_file_filtered$`Week No`))),paste0("ROI_cal","_",sort(unique(SP_reactive_input$event_file_filtered$`Week No`)))))
                         setcolorder(SP_reactive_input$event_file_dcast_ROI,c("Format","PPG","PPG_Description","RSP_latest",sort(unique(SP_reactive_input$event_file_filtered$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$event_file_filtered$`Week No`))),paste0("Promo Price","_",sort(unique(SP_reactive_input$event_file_filtered$`Week No`)))))
                         
                       }
                     }
                     # if(input$SP_summ_top_panel_right !=0){
                     #   updateTabItems(session, "sidebar_main", selected = "SP_subMenu3")
                     # }
                     #,'#90EE90','#FFDAB9'
                     output$SP_promo_cal_legend <- renderDataTable({
                       datatable(transpose(data.frame(c("Shelf Promotion","Display Feature Promotion"))),class='display',
                                 rownames = NULL, colnames = c(rep("",2)), 
                                 options = list(dom = 'b',ordering=FALSE)) %>%
                         formatStyle(columns=c(0:2),
                                     background = styleEqual(c("Shelf Promotion","Display Feature Promotion"), 
                                                             c('#0099DC', '#E42E92')),
                                     color = styleEqual(c("Shelf Promotion","Display Feature Promotion"), 
                                                        c('white', 'white'))) %>%
                         formatStyle(1,border = styleInterval(0, c('auto', '1px solid white'))) %>%
                         formatStyle(0,target = 'row',lineHeight='70%')
                     })
                     
                     observeEvent(input$SP_promo_cal_format_selection,{
                       output$SP_promo_cal <- renderDataTable({
                         validate(
                           need(SP_reactive_input$event_file_dcast,"")
                         )
                         #round(seq(40, 255, length.out = (length(SP_reactive_input$brks_ROI) + 1)/2), 0) %>% {paste0("rgb(255,", ., ",","0)")},
                         
                         clrs_ROI <- c(round(seq(255,132, length.out = (length(SP_reactive_input$brks_ROI) + 1)), 0) %>% {paste0("rgb(", ., ",192,0)")})
                         sketch = htmltools::withTags(table(
                           class = 'cell-border compact hover',
                           thead(
                             tr(
                               # th(colspan = 1, 'Month'),
                               # th(colspan = 4, 'January'),
                               # th(colspan = 4, 'February'),
                               # th(colspan = 5, 'March'),
                               # th(colspan = 4, 'April'),
                               # th(colspan = 4, 'May'),
                               # th(colspan = 5, 'June'),
                               # th(colspan = 4, 'July'),
                               # th(colspan = 4, 'August'),
                               # th(colspan = 5, 'September'),
                               # th(colspan = 4, 'October'),
                               # th(colspan = 4, 'November'),
                               # th(colspan = 5, 'December')
                               HTML(promo_cal_structure(SP_reactive_input$event_file_filtered)[[1]])
                             ),
                             tr(
                               lapply(
                                 c("Format","PPG","PPG Description","RSP",unlist(sapply(promo_cal_structure(SP_reactive_input$event_file_filtered)[[2]][1],function(x) c(1:x))),
                                   unlist(sapply(promo_cal_structure(SP_reactive_input$event_file_filtered)[[2]][2:length(promo_cal_structure(SP_reactive_input$event_file_filtered)[[2]])],function(x) c(1:x)))
                                   #1:length(unique(SP_reactive_input$event_file_filtered$`Week No`))
                                 ),
                                 th
                               )
                             )
                           )
                         ))
                         
                         if(input$SP_promo_cal_format_selection == "Promotion Type"){
                           datatable(SP_reactive_input$event_file_dcast,container=sketch,class="cell-border compact hover",extensions = c('FixedColumns','Buttons'),selection =list(target='cell',mode="single"),
                                     #selection=list(mode="single", target="cell"),
                                     options = list(
                                       order = list(0, 'asc'), 
                                       buttons = list('copy', list(extend = 'csv',filename = 'Historical Calendar'),
                                                      list(extend = 'excel', filename = 'Historical Calendar')),
                                       # list(extend = 'pdf',
                                       #      pageSize = 'LEGAL',
                                       #      orientation = 'landscape',
                                       #      filename = 'Historical Calendar'), 'print'),
                                       columnDefs=list(list(visible=FALSE, targets=grep("Event|ROI",names(SP_reactive_input$event_file_dcast))-1),list(width = '70px',targets=c(0)),list(width = '30px',targets=c(1,2)),list(width = '10px',targets=sort(unique(SP_reactive_input$event_file_filtered$`Week No`)) + 3)),
                                       fixedColumns = list(leftColumns = 4),
                                       paging = FALSE,
                                       searching = FALSE,
                                       dom = 'Bt',
                                       bSort=FALSE,
                                       scrollX = TRUE,
                                       scrollY = "400px"
                                     ),rownames = F) %>% 
                             formatStyle(as.character(c(1:length(unique(SP_reactive_input$event_file_filtered$`Week No`)))),paste0("Event_",sort(unique(SP_reactive_input$event_file_filtered$`Week No`))),
                                         background = styleEqual(c("No Promo","Shelf Promotion","Display Feature Promotion","Feature","Display Feature"), 
                                                                 c('white','#0099DC', '#E42E92','#90EE90','#FFDAB9')),
                                         color = styleEqual(c("No Promo","Shelf Promotion","Display Feature Promotion","Feature","Display Feature"), 
                                                            c('white','white', 'white','white','white'))) %>%
                             formatStyle(0,target = 'row',lineHeight='85%') %>%
                             formatRound(as.character(c("RSP_latest",1:length(unique(SP_reactive_input$event_file_filtered$`Week No`)))), digits=2) %>%
                             formatStyle(as.character(c(1:length(unique(SP_reactive_input$event_file_filtered$`Week No`)))),cursor = 'pointer')
                         }else if(input$SP_promo_cal_format_selection == "ROI Effectiveness"){
                           #
                           datatable(SP_reactive_input$event_file_dcast_ROI,container=sketch,class="cell-border compact hover",extensions = c('FixedColumns','Buttons'),selection =list(target='cell',mode="single"),
                                     #selection=list(mode="single", target="cell"),
                                     options = list(
                                       order = list(0, 'asc'),
                                       buttons = list('copy', list(extend = 'csv',filename = 'Historical Calendar'),
                                                      list(extend = 'excel', filename = 'Historical Calendar')),
                                       # list(extend = 'pdf',
                                       #      pageSize = 'A4',
                                       #      orientation = 'landscape',
                                       #      filename = 'Historical Calendar'), 'print'),
                                       columnDefs=list(list(visible=FALSE, targets=grep("Event_|Price_",names(SP_reactive_input$event_file_dcast_ROI))-1),list(width = '70px',targets=c(0)),list(width = '30px',targets=c(1,2)),list(width = '10px',targets=sort(unique(SP_reactive_input$event_file_filtered$`Week No`)) + 3)),
                                       fixedColumns = list(leftColumns = 4),
                                       paging = FALSE,
                                       searching = FALSE,
                                       dom = 'Bt',
                                       bSort=FALSE,
                                       scrollX = TRUE,
                                       scrollY = "400px"
                                     ),rownames = F) %>%
                             formatStyle(as.character(c(1:length(unique(SP_reactive_input$event_file_filtered$`Week No`)))),
                                         background = styleInterval(c(-99,SP_reactive_input$brks_ROI),c("white","red",clrs_ROI[-1]))) %>%
                             formatStyle(as.character(c(1:length(unique(SP_reactive_input$event_file_filtered$`Week No`)))),
                                         color = styleEqual(-100,c("white"))) %>%
                             formatStyle(0,target = 'row',lineHeight='70%') %>%
                             formatRound(as.character(c("RSP_latest",1:length(unique(SP_reactive_input$event_file_filtered$`Week No`)))), digits=2) %>%
                             formatStyle(as.character(c(1:length(unique(SP_reactive_input$event_file_filtered$`Week No`)))),cursor = 'pointer')
                         }
                       },server = FALSE)
                     })
                   })
    observeEvent(input$SP_promo_cal_format_selection,{
      toggle("SP_promo_cal_legend",condition = (input$SP_promo_cal_format_selection != "ROI Effectiveness"))
      # if(input$SP_promo_cal_format_selection == "ROI Effectiveness"){
      #   
      #   hide("SP_promo_cal_legend")
      # }else{
      #   
      #   show("SP_promo_cal_legend")
      # }
    })
    # output$event_popup <- renderUI({
    #   bsModal("cal_popup", "Event Information", "", size = "large",
    #           dataTableOutput("cal_popup_table"))
    # 
    # })
    
    ######Pop up Promo event information when clicked on a calendar event############
    observeEvent({
      input$SP_promo_cal_cell_clicked},{
        SP_reactive_input$SP_cal_cell <- input$SP_promo_cal_cell_clicked
        if (length(input$SP_promo_cal_cell_clicked) != 0){
          
          cal_event_selected_PPG <- SP_reactive_input$event_file_dcast[input$SP_promo_cal_cell_clicked$row,]$PPG
          SP_reactive_input$cal_event_selected <- SP_reactive_input$event_file_filtered_1[SP_reactive_input$event_file_filtered_1$PPG == cal_event_selected_PPG & SP_reactive_input$event_file_filtered_1$`Week No` == (input$SP_promo_cal_cell_clicked$col - 3),]$Event_Number
          
          SP_reactive_input$cal_event_selected_info <- SP_reactive_input$cal_sample[SP_reactive_input$cal_sample$PPG == cal_event_selected_PPG & SP_reactive_input$cal_sample$Event_Number == SP_reactive_input$cal_event_selected,c("Category","Brand","Format","PPG","Event","RSP","Promo Price","Discount","Inc_GM_Abs","Inc_units","R_ROI_GM","R_ROI_Rev","R_ROI_NIS")]
          
          SP_reactive_input$cal_event_selected_info$Discount <- SP_reactive_input$cal_event_selected_info$Discount * 100
          names(SP_reactive_input$cal_event_selected_info) <- c("Category","Brand","Format","PPG","Event","RSP","Promo Price","Discount %","Incremental Value Sales","Incremental Unit Sales","ROI_GM","ROI_Rev","ROI_NIS")
          
          showModal(
            modalDialog(
              title = "Event Information",
              HTML(paste0(
                "Category: ",SP_reactive_input$cal_event_selected_info$Category,"<br>",
                "Brand: ",SP_reactive_input$cal_event_selected_info$Brand,"<br>",
                "Format: ",SP_reactive_input$cal_event_selected_info$Format,"<br>",
                "PPG: ",SP_reactive_input$cal_event_selected_info$PPG,"<br>",
                "RSP: ",round(SP_reactive_input$cal_event_selected_info$RSP,2),"<br>",
                "Promo Price: ",round(SP_reactive_input$cal_event_selected_info$`Promo Price`,2),"<br>",
                "Incremental Sales: ",round(SP_reactive_input$cal_event_selected_info$`Incremental Value Sales`,2),"<br>",
                "Incremental GM ROI: ",round(SP_reactive_input$cal_event_selected_info$ROI_GM,2),"<br>",
                "Incremental NR ROI: ",round(SP_reactive_input$cal_event_selected_info$ROI_Rev,2),"<br>",
                "Incremental NIS ROI: ",round(SP_reactive_input$cal_event_selected_info$ROI_NIS,2))),
              size = "m",
              easyClose = TRUE
            )
          )
        }
      })
    ####Populating top dropdowns in Optimizer Input Tab##################
    observeEvent(c(input$SP_proceed_summ,
                   input$SP_cal_top_panel_right),{
                     updateSelectizeInput(session,"SP_opti_cat",selected = unique(SP_reactive_input$SP_opti_const$Category)[1],choices = unique(SP_reactive_input$SP_opti_const$Category))
                     
                     date_seq <- SP_input_tesco_slot()
                     # date_seq$`Start Date` <- ymd(date_seq$`Start Date`) + 6
                     date_seq$`Start Date`[1]="25/12/2025"
                     date_seq$`Start Date` <- dmy(date_seq$`Start Date`) 
                     date_seq$`End Date`<- dmy(date_seq$`End Date`)
                     #bilal debu code
                      
                     end_date_tmp <- ymd(date_seq[nrow(date_seq),]$`End Date`) - 77 
                     # end_date_tmp <- ymd(date_seq[nrow(date_seq),]$`End Date`)
                     #date_seq <- seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by = "week")
                     output$SP_opti_start_date_ui <- renderUI({
                       #SP_reactive_input$SP_date_vector <- unique(SP_reactive_input$SP_opti_const$Date)[order(unique(SP_reactive_input$SP_opti_const$Date))]
                       #selectizeInput("SP_opti_date_start","Start Date",choices = SP_reactive_input$SP_date_vector,selected = min(SP_reactive_input$SP_date_vector[year(SP_reactive_input$SP_date_vector) == year(max(SP_reactive_input$SP_date_vector))]))
                       #  selectizeInput("SP_opti_date_start","Start Date",choices = ymd(date_seq[date_seq$Tesco_Week_No != 0 & date_seq$`Start Date` <= end_date_tmp,]$`Start Date`),selected = ymd(date_seq[date_seq$Tesco_Week_No != 0 & date_seq$`Start Date` <= end_date_tmp,]$`Start Date`)[1])
                       selectizeInput("SP_opti_date_start","Start Date",choices = ymd(date_seq[date_seq$Slot != 0 & date_seq$`Start Date` <= end_date_tmp,]$`Start Date`),selected = ymd(date_seq[date_seq$Slot != 0 & date_seq$`Start Date` <= end_date_tmp,]$`Start Date`)[1])
                       
                     })
                     
                     
                     
                     output$SP_opti_end_date_ui <- renderUI({
                       #selectizeInput("SP_opti_date_end","End Date",choices = SP_reactive_input$SP_date_vector,selected = max(SP_reactive_input$SP_date_vector))
                       selectizeInput("SP_opti_date_end","End Date",choices = ymd(date_seq[date_seq$`End Date` >= (ymd(input$SP_opti_date_start) + 77),]$`End Date`),selected = ymd(date_seq[date_seq$`End Date` >= (ymd(input$SP_opti_date_start) + 77),]$`End Date`)[length(ymd(date_seq[date_seq$`End Date` >= (ymd(input$SP_opti_date_start) + 77),]$`End Date`))])
                     })
                     
                     observeEvent({
                       input$SP_opti_cat},{
                         ##Manufacturer filtering
                         updateSelectizeInput(session,"SP_opti_brand",choices =unique(SP_reactive_input$SP_opti_const[SP_reactive_input$SP_opti_const$Category == input$SP_opti_cat & SP_reactive_input$SP_opti_const$Manufacturer == SP_reactive_input$SP_manuf,]$Brand),selected = unique(SP_reactive_input$SP_opti_const[SP_reactive_input$SP_opti_const$Category == input$SP_opti_cat & SP_reactive_input$SP_opti_const$Manufacturer == SP_reactive_input$SP_manuf,]$Brand)[1])
                       }
                     )
                     
                     observeEvent({
                       input$SP_opti_cat
                       input$SP_opti_brand},{
                         
                         updateSelectizeInput(session,"SP_opti_format",choices = c("",unique(SP_reactive_input$SP_opti_const[SP_reactive_input$SP_opti_const$Category == input$SP_opti_cat & SP_reactive_input$SP_opti_const$Brand %in% input$SP_opti_brand,]$Format)),selected = NULL)
                       })
                     
                     observeEvent({
                       input$SP_opti_cat
                       input$SP_opti_brand
                       input$SP_opti_format},{
                         
                         #SP_reactive_input$SP_opti_const_ppg <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat & SP_reactive_input$exclude_ppg_data_prep$Brand %in% input$SP_opti_brand & SP_reactive_input$exclude_ppg_data_prep$Format %in% input$SP_opti_format,])
                         updateSelectizeInput(session,"SP_opti_ppg",choices = unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat & SP_reactive_input$exclude_ppg_data_prep$Brand %in% input$SP_opti_brand & SP_reactive_input$exclude_ppg_data_prep$Format %in% input$SP_opti_format,]$PPG))
                       })
                   })
    
    observeEvent(c(input$SP_opti_cat,
                   #input$SP_level,
                   input$SP_opti_brand,
                   input$SP_opti_format,
                   input$SP_opti_ppg,
                   input$SP_opti_goal,
                   input$SP_opti_date_start,
                   input$SP_opti_date_end,
                   input$SP_opti_reset,
                   input$SP_opti_ROI_selection,
                   input$SP_opti_restrictions$changes$changes[[1]]),{
                     
                     # ... existing code for restrictions table ...
                     
                     print(SP_reactive_input$SP_restrictions_selected)
                     
                     # DEBUG: Check if optimizer data preparation inputs are ready
                     cat("\n========== OPTIMIZER DATA PREPARATION CHECK ==========\n")
                     cat("1. shiny_opti_data_ip (base data):", 
                         ifelse(is.null(SP_reactive_input$shiny_opti_data_ip), "NULL", 
                                paste("rows =", nrow(SP_reactive_input$shiny_opti_data_ip))), "\n")
                     cat("2. shiny_ip_events_final (events):", 
                         ifelse(is.null(SP_reactive_input$shiny_ip_events_final), "NULL", 
                                paste("rows =", nrow(SP_reactive_input$shiny_ip_events_final))), "\n")
                     cat("3. SP_opti_prod_restrictions_default:", 
                         ifelse(is.null(SP_reactive_input$SP_opti_prod_restrictions_default), "NULL", 
                                paste("rows =", nrow(SP_reactive_input$SP_opti_prod_restrictions_default))), "\n")
                     cat("4. SP_opti_const_grouped:", 
                         ifelse(is.null(SP_reactive_input$SP_opti_const_grouped), "NULL", 
                                paste("rows =", nrow(SP_reactive_input$SP_opti_const_grouped))), "\n")
                     cat("5. SP_restrictions_selected:", 
                         ifelse(is.null(SP_reactive_input$SP_restrictions_selected), "NULL", 
                                paste("rows =", nrow(SP_reactive_input$SP_restrictions_selected))), "\n")
                     cat("6. exclude_ppg_data_prep:", 
                         ifelse(is.null(SP_reactive_input$exclude_ppg_data_prep), "NULL", 
                                paste("rows =", nrow(SP_reactive_input$exclude_ppg_data_prep))), "\n")
                     cat("7. Date inputs - Start:", input$SP_opti_date_start, "| End:", input$SP_opti_date_end, "\n")
                     cat("==================================================\n\n")
                     
                     print(c(input$SP_opti_cat,
                             input$SP_opti_brand,
                             input$SP_opti_format,
                             input$SP_opti_goal,
                             input$SP_opti_date_start,
                             input$SP_opti_date_end,
                             input$SP_opti_reset,
                             input$SP_opti_ROI_selection,
                             input$SP_opti_restrictions$changes$changes[[1]]))
                     #Making constraint change flag as 0 when new brand or format is selected
                     
                     #####Default selection of optmization sign based on goal
                     observeEvent(input$SP_opti_goal,{
                       if(grepl("Spend|spend|invest|Invest",input$SP_opti_goal,ignore.case = TRUE)){
                         updateSelectizeInput(session,"SP_opti_sign",choices = c("Max","Min"),selected = "Min")
                       }else{
                         updateSelectizeInput(session,"SP_opti_sign",choices = c("Max","Min"),selected = "Max")
                       }
                     })
                     
                     if(!(is.null(input$SP_opti_date_start)) & !(is.null(input$SP_opti_date_end))){
                       if(input$SP_opti_date_end != ""){
                         SP_reactive_input$SP_opti_const <- data.table(SP_reactive_input$SP_opti_const[!(is.na(SP_reactive_input$SP_opti_const$current_year_date)),])
                         SP_reactive_input$SP_opti_const_selected <- data.table(SP_reactive_input$SP_opti_const[SP_reactive_input$SP_opti_const$current_year_date >= as.Date(input$SP_opti_date_start) & SP_reactive_input$SP_opti_const$current_year_date <= as.Date(input$SP_opti_date_end),])
                       }
                     }
                     if(!(is.null(SP_reactive_input$SP_opti_const_selected))){
                       if((nrow(SP_reactive_input$SP_opti_const_selected) != 0)){
                         #
                         ######Calculating last year values(2018), Minimum(0.9) and maximum(1.1) values based on selected fields
                         #When Brand, Format and PPG are empty, populating the graphs
                         #ignore the category optimization
                         if(all(input$SP_opti_cat != "") & all(input$SP_opti_brand == "") & all(input$SP_opti_format == "") & all(input$SP_opti_ppg == "")){
                           SP_reactive_input$opti_ip_cat_sales <- data.frame("Units" = 1000000)
                           SP_reactive_input$opti_ip_cat_sales_value <- data.frame("Value" = 1000000)
                           SP_reactive_input$SP_opti_const_grouped <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat,.("CY_Volume" = sum(Units),"Inc_GM_Abs" = sum(Inc_GM_Abs), "Trade_Investment" = sum(Trade_Investment), "Gross_Sales" = sum(Gross_Sales),
                                                                                                                                                                                        "Net_Revenue" = sum(Net_Revenue), "GM_Abs" = sum(GM_Abs), "Inc_Revenue" = sum(Inc_Revenue),"Inc_NIS" = sum(Inc_NIS), "NIS" = sum(NIS),
                                                                                                                                                                                        "Retailer_Revenue" = sum(Retailer_Revenue), "Net_Cost_Total" = sum(Net_Cost_Total),"FM" = sum(FM_Total), "Retro_Funding" = sum(Retro_Fund_Total), "Display" = sum(Display_Cost),
                                                                                                                                                                                        "Inc_GM_Abs_New" = sum(R_GM_Inc), "Inc_Revenue_New" = sum(R_Net_Revenue_Inc), "Trade_Investment_New" = sum(R_Total_Trade_Inv_Inc ), "Inc_NIS_New" = sum(R_NIS_Inc)),by = .(Category)]
                           
                           SP_reactive_input$SP_RB_Financial <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$Flag_RB_Financial == 0,])
                           SP_reactive_input$SP_RB_Delist <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$DL == 1,])
                         }else if(all(input$SP_opti_brand != "") & all(input$SP_opti_format == "") & all(input$SP_opti_ppg == "")){
                           
                           SP_reactive_input$opti_ip_cat_sales <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & SP_reactive_input$SP_opti_const_selected$Brand != input$SP_opti_brand,.("Units" = sum(Units)),by = .(Category)]
                           SP_reactive_input$opti_ip_cat_sales_value <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & SP_reactive_input$SP_opti_const_selected$Brand != input$SP_opti_brand,.("Value" = sum(Retailer_Revenue)),by = .(Category)]
                           SP_reactive_input$SP_opti_const_grouped <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & SP_reactive_input$SP_opti_const_selected$Brand == input$SP_opti_brand,.("CY_Volume" = sum(Units),"Inc_GM_Abs" = sum(Inc_GM_Abs), "Trade_Investment" = sum(Trade_Investment), "Gross_Sales" = sum(Gross_Sales),
                                                                                                                                                                                                                                                                "Net_Revenue" = sum(Net_Revenue), "GM_Abs" = sum(GM_Abs), "Inc_Revenue" = sum(Inc_Revenue),"Inc_NIS" = sum(Inc_NIS), "NIS" = sum(NIS),
                                                                                                                                                                                                                                                                "Retailer_Revenue" = sum(Retailer_Revenue), "Net_Cost_Total" = sum(Net_Cost_Total),"FM" = sum(FM_Total), "Retro_Funding" = sum(Retro_Fund_Total), "Display" = sum(Display_Cost),
                                                                                                                                                                                                                                                                "Inc_GM_Abs_New" = sum(R_GM_Inc), "Inc_Revenue_New" = sum(R_Net_Revenue_Inc), "Trade_Investment_New" = sum(R_Total_Trade_Inv_Inc ), "Inc_NIS_New" = sum(R_NIS_Inc)),by = .(Category,Brand)]
                           SP_reactive_input$SP_RB_Financial <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand & SP_reactive_input$SP_nielsen$Flag_RB_Financial == 0,])
                           SP_reactive_input$SP_RB_Delist <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand & SP_reactive_input$SP_nielsen$DL == 1,])
                           
                         }else if(all(input$SP_opti_format != "") & all(input$SP_opti_ppg == "")){
                           
                           SP_reactive_input$opti_ip_cat_sales <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & !(SP_reactive_input$SP_opti_const_selected$Format %in% input$SP_opti_format),.("Units" = sum(Units)),by = .(Category)]
                           SP_reactive_input$opti_ip_cat_sales_value <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & !(SP_reactive_input$SP_opti_const_selected$Format %in% input$SP_opti_format),.("Value" = sum(Retailer_Revenue)),by = .(Category)]
                           
                           SP_reactive_input$SP_opti_const_grouped <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & SP_reactive_input$SP_opti_const_selected$Brand == input$SP_opti_brand & SP_reactive_input$SP_opti_const_selected$Format == input$SP_opti_format,.("CY_Volume" = sum(Units),"Inc_GM_Abs" = sum(Inc_GM_Abs), "Trade_Investment" = sum(Trade_Investment), "Gross_Sales" = sum(Gross_Sales),
                                                                                                                                                                                                                                                                                                                                          "Net_Revenue" = sum(Net_Revenue), "GM_Abs" = sum(GM_Abs), "Inc_Revenue" = sum(Inc_Revenue),"Inc_NIS" = sum(Inc_NIS), "NIS" = sum(NIS),
                                                                                                                                                                                                                                                                                                                                          "Retailer_Revenue" = sum(Retailer_Revenue), "Net_Cost_Total" = sum(Net_Cost_Total),"FM" = sum(FM_Total), "Retro_Funding" = sum(Retro_Fund_Total), "Display" = sum(Display_Cost),
                                                                                                                                                                                                                                                                                                                                          "Inc_GM_Abs_New" = sum(R_GM_Inc), "Inc_Revenue_New" = sum(R_Net_Revenue_Inc), "Trade_Investment_New" = sum(R_Total_Trade_Inv_Inc ), "Inc_NIS_New" = sum(R_NIS_Inc)),by = .(Category,Brand)]
                           SP_reactive_input$SP_RB_Financial <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_opti_format & SP_reactive_input$SP_nielsen$Flag_RB_Financial == 0,])
                           SP_reactive_input$SP_RB_Delist <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_opti_format & SP_reactive_input$SP_nielsen$DL == 1,])
                         }else if(all(input$SP_opti_ppg != "")){
                           
                           SP_reactive_input$opti_ip_cat_sales <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & !(SP_reactive_input$SP_opti_const_selected$PPG %in% input$SP_opti_ppg),.("Units" = sum(Units)),by = .(Category)]
                           SP_reactive_input$opti_ip_cat_sales_value <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & !(SP_reactive_input$SP_opti_const_selected$PPG %in% input$SP_opti_ppg),.("Value" = sum(Retailer_Revenue)),by = .(Category)]
                           
                           SP_reactive_input$SP_opti_const_grouped <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Category == input$SP_opti_cat & SP_reactive_input$SP_opti_const_selected$Brand == input$SP_opti_brand & SP_reactive_input$SP_opti_const_selected$Format == input$SP_opti_format & (SP_reactive_input$SP_opti_const_selected$PPG %in% input$SP_opti_ppg),.("CY_Volume" = sum(Units),"Inc_GM_Abs" = sum(Inc_GM_Abs), "Trade_Investment" = sum(Trade_Investment), "Gross_Sales" = sum(Gross_Sales),
                                                                                                                                                                                                                                                                                                                                                                                                                  "Net_Revenue" = sum(Net_Revenue), "GM_Abs" = sum(GM_Abs), "Inc_Revenue" = sum(Inc_Revenue),"Inc_NIS" = sum(Inc_NIS), "NIS" = sum(NIS),
                                                                                                                                                                                                                                                                                                                                                                                                                  "Retailer_Revenue" = sum(Retailer_Revenue), "Net_Cost_Total" = sum(Net_Cost_Total),"FM" = sum(FM_Total), "Retro_Funding" = sum(Retro_Fund_Total), "Display" = sum(Display_Cost),
                                                                                                                                                                                                                                                                                                                                                                                                                  "Inc_GM_Abs_New" = sum(R_GM_Inc), "Inc_Revenue_New" = sum(R_Net_Revenue_Inc), "Trade_Investment_New" = sum(R_Total_Trade_Inv_Inc ), "Inc_NIS_New" = sum(R_NIS_Inc)),by = .(Category,Brand)]
                           SP_reactive_input$SP_RB_Financial <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_opti_format & SP_reactive_input$SP_nielsen$PPG %in% input$SP_opti_ppg & SP_reactive_input$SP_nielsen$Flag_RB_Financial == 0,])
                           SP_reactive_input$SP_RB_Delist <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand & SP_reactive_input$SP_nielsen$Format %in% input$SP_opti_format & SP_reactive_input$SP_nielsen$PPG %in% input$SP_opti_ppg & SP_reactive_input$SP_nielsen$DL == 1,])
                         }
                         SP_reactive_input$SP_RB_Delist <- data.frame(SP_reactive_input$SP_RB_Delist)
                         SP_reactive_input$SP_RB_Delist <- left_join(SP_reactive_input$SP_RB_Delist, SP_reactive_input$date_mapping, by = c("Date" = "last_year_date"))
                         SP_reactive_input$SP_RB_Delist <- data.table(SP_reactive_input$SP_RB_Delist)
                         
                         if((is.null(input$SP_opti_restrictions$changes$changes[[1]])) & (SP_reactive_input$const_chg_flag != 1) & (!(is.null(SP_reactive_input$SP_opti_const_grouped)))){
                           #
                           SP_reactive_input$SP_restrictions <- data.frame("Include the Constraint"= rep(TRUE,9),"Constraint Order" = c(1:9),"KPI" = c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Scan Gross Sales","Gross Margin","Volume Sales","Trade ROI","Value Market Share"),"Last Year Value" = rep(0,9), "Minimum Value" = rep(0,9),"Maximum Value" = rep(0,9),check.names = FALSE)
                           SP_reactive_input$SP_restrictions$KPI <- as.character(SP_reactive_input$SP_restrictions$KPI)
                           SP_reactive_input$SP_restrictions$Scale <- "Absolute"
                           SP_reactive_input$SP_restrictions[grep("%|Share",SP_reactive_input$SP_restrictions$KPI),]$Scale <- "Percent"
                           SP_reactive_input$SP_restrictions$Scale <- as.factor(SP_reactive_input$SP_restrictions$Scale)
                           
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Volume Sales",]$`Last Year Value` <- round(SP_reactive_input$SP_opti_const_grouped$CY_Volume/10^6,2)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Volume Sales",]$`Minimum Value` <- round(SP_reactive_input$SP_opti_const_grouped$CY_Volume *0.9/10^6,2)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Volume Sales",]$`Maximum Value` <- round(SP_reactive_input$SP_opti_const_grouped$CY_Volume *1.1/10^6,2)
                           
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Scan Net Revenue",]$`Last Year Value` <- round(SP_reactive_input$SP_opti_const_grouped$Net_Revenue/10^6,2)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Scan Net Revenue",]$`Minimum Value` <- round(SP_reactive_input$SP_opti_const_grouped$Net_Revenue *0.9/10^6,2)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Scan Net Revenue",]$`Maximum Value` <- round(SP_reactive_input$SP_opti_const_grouped$Net_Revenue *1.1/10^6,2)
                           
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Gross Margin % of NR",]$`Last Year Value` <- (SP_reactive_input$SP_opti_const_grouped$GM_Abs*100/SP_reactive_input$SP_opti_const_grouped$Net_Revenue) 
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Gross Margin % of NR",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$GM_Abs*100/SP_reactive_input$SP_opti_const_grouped$Net_Revenue) * 0.9
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Gross Margin % of NR",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$GM_Abs*100/SP_reactive_input$SP_opti_const_grouped$Net_Revenue) * 1.1
                           
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Scan Gross Sales",]$`Last Year Value` <- round(SP_reactive_input$SP_opti_const_grouped$Gross_Sales/10^6,2)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Scan Gross Sales",]$`Minimum Value` <- round(SP_reactive_input$SP_opti_const_grouped$Gross_Sales *0.9/10^6,2)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Scan Gross Sales",]$`Maximum Value` <- round(SP_reactive_input$SP_opti_const_grouped$Gross_Sales *1.1/10^6,2)
                           
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Gross Margin",]$`Last Year Value` <- round(SP_reactive_input$SP_opti_const_grouped$GM_Abs/10^6,2)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Gross Margin",]$`Minimum Value` <- round(SP_reactive_input$SP_opti_const_grouped$GM_Abs *0.9/10^6,2)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Gross Margin",]$`Maximum Value` <- round(SP_reactive_input$SP_opti_const_grouped$GM_Abs *1.1/10^6,2)
                           if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
                             SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Last Year Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)
                             if(((SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New) < 0) | is.na(SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)){
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                             }else{
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                             }
                             
                             SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$KPI <- "Incremental GM ROI"
                             
                           }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
                             SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Last Year Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)
                             if((SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New) < 0){
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                             }else{
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                             }
                             
                             SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$KPI <- "Incremental NR ROI"
                             
                           }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
                             SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Last Year Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_NIS_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)
                             if((SP_reactive_input$SP_opti_const_grouped$Inc_NIS_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New) < 0){
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_NIS_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_NIS_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                             }else{
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_NIS_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                               SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_NIS_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                             }
                             
                             SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade ROI",]$KPI <- "Incremental NIS ROI"
                             
                           }
                           
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade Spend % of NR",]$`Last Year Value` <- (SP_reactive_input$SP_opti_const_grouped$Trade_Investment*100/SP_reactive_input$SP_opti_const_grouped$Net_Revenue)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade Spend % of NR",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Trade_Investment*100/SP_reactive_input$SP_opti_const_grouped$Net_Revenue)*0.9
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade Spend % of NR",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Trade_Investment*100/SP_reactive_input$SP_opti_const_grouped$Net_Revenue)*1.1
                           
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade Spend % of NIS",]$`Last Year Value` <- (SP_reactive_input$SP_opti_const_grouped$Trade_Investment*100/SP_reactive_input$SP_opti_const_grouped$NIS)
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade Spend % of NIS",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Trade_Investment*100/SP_reactive_input$SP_opti_const_grouped$NIS)*0.9
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Trade Spend % of NIS",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Trade_Investment*100/SP_reactive_input$SP_opti_const_grouped$NIS)*1.1
                           # 
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Value Market Share",]$`Last Year Value` <- (SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue*100/(SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue + SP_reactive_input$opti_ip_cat_sales_value$Value))
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Value Market Share",]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue*100/(SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue + SP_reactive_input$opti_ip_cat_sales_value$Value)) * 0.9
                           SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI == "Value Market Share",]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue*100/(SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue + SP_reactive_input$opti_ip_cat_sales_value$Value)) * 1.1
                           
                           SP_reactive_input$SP_restrictions[grep("Spend|spend|invest|Invest",SP_reactive_input$SP_restrictions$KPI,ignore.case = TRUE,invert = TRUE),]$`Maximum Value` <- NA
                           SP_reactive_input$SP_restrictions[grep("Spend|spend|invest|Invest",SP_reactive_input$SP_restrictions$KPI,ignore.case = TRUE),]$`Minimum Value` <- NA
                           
                           # SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI %in% c("Gross Margin % of NR","Scan Net Revenue","Trade Spend % of NR","Scan Gross Sales","Volume Sales",
                           #                                                                                                                              input$SP_opti_ROI_selection,"Value Market Share"),]
                           SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI != input$SP_opti_goal,c("Include the Constraint","Constraint Order","KPI","Scale","Last Year Value","Minimum Value","Maximum Value")]
                           SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions_selected[c(1:6),]
                           
                           SP_reactive_input$SP_restrictions_selected$`Constraint Order` <- as.factor(c(1:nrow(SP_reactive_input$SP_restrictions_selected)))
                           rownames(SP_reactive_input$SP_restrictions_selected) <- c(1:nrow(SP_reactive_input$SP_restrictions_selected))
                           
                         }else if(!(is.null(input$SP_opti_restrictions$changes$changes[[1]]))){
                           if((input$SP_opti_restrictions$changes$changes[[1]][[2]] == (which(colnames(hot_to_r(input$SP_opti_restrictions)) %in% c("KPI")) -1))){
                             
                             SP_reactive_input$SP_restrictions_1 <- hot_to_r(input$SP_opti_restrictions)
                             SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI != input$SP_opti_goal,c("Include the Constraint","Constraint Order","KPI","Scale","Last Year Value","Minimum Value","Maximum Value")]
                             SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions_selected[SP_reactive_input$SP_restrictions_selected$KPI %in% SP_reactive_input$SP_restrictions_1$KPI,]
                             
                             SP_reactive_input$SP_restrictions_selected$`Constraint Order` <- as.factor(c(1:nrow(SP_reactive_input$SP_restrictions_selected)))
                             rownames(SP_reactive_input$SP_restrictions_selected) <- c(1:nrow(SP_reactive_input$SP_restrictions_selected))
                             
                             SP_reactive_input$const_chg_flag <- 1
                             
                           }else if((input$SP_opti_restrictions$changes$changes[[1]][[2]] == (which(colnames(hot_to_r(input$SP_opti_restrictions)) %in% c("Scale")) -1))){
                             SP_reactive_input$SP_restrictions_1 <- hot_to_r(input$SP_opti_restrictions)
                             if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions$changes$changes[[1]][[1]]+1,"KPI"] == "Volume Sales"){
                               if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Absolute"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Volume Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- round(SP_reactive_input$SP_opti_const_grouped$CY_Volume *0.9/10^6,2)
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Volume Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- round(SP_reactive_input$SP_opti_const_grouped$CY_Volume *1.1/10^6,2)
                               }else if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Percent"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Volume Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Volume Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                               }
                             }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions$changes$changes[[1]][[1]]+1,"KPI"] == "Scan Gross Sales"){
                               if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Absolute"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Gross Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- round(SP_reactive_input$SP_opti_const_grouped$Gross_Sales *0.9/10^6,2)
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Gross Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- round(SP_reactive_input$SP_opti_const_grouped$Gross_Sales *1.1/10^6,2)
                               }else if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Percent"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Gross Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Gross Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                               }
                             }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions$changes$changes[[1]][[1]]+1,"KPI"] == "Gross Margin"){
                               if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Absolute"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Gross Margin" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- round(SP_reactive_input$SP_opti_const_grouped$GM_Abs *0.9/10^6,2)
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Gross Margin" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- round(SP_reactive_input$SP_opti_const_grouped$GM_Abs *1.1/10^6,2)
                               }else if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Percent"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Gross Margin" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Gross Margin" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                               }
                             }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions$changes$changes[[1]][[1]]+1,"KPI"] == input$SP_opti_ROI_selection){
                               if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Absolute"){
                                 if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
                                   if((SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New) < 0){
                                     SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                                     SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                                   }else{
                                     SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                                     SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_GM_Abs_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                                   }
                                 }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
                                   if((SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New) < 0){
                                     SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                                     SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                                   }else{
                                     SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*0.9
                                     SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Inc_Revenue_New/SP_reactive_input$SP_opti_const_grouped$Trade_Investment_New)*1.1
                                   }
                                 }
                               }else if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Percent"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                               }
                             }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions$changes$changes[[1]][[1]]+1,"KPI"] == "Value Market Share"){
                               if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Absolute"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Value Market Share" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- (SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue/(SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue + SP_reactive_input$opti_ip_cat_sales_value$Value)) * 0.9
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Value Market Share" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- (SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue/(SP_reactive_input$SP_opti_const_grouped$Retailer_Revenue + SP_reactive_input$opti_ip_cat_sales_value$Value)) * 1.1
                               }else if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Percent"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Value Market Share" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Value Market Share" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                               }
                             }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions$changes$changes[[1]][[1]]+1,"KPI"] == "Scan Net Revenue"){
                               if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Absolute"){
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Net Revenue" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- round(SP_reactive_input$SP_opti_const_grouped$Net_Revenue *0.9/10^6,2)
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Net Revenue" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- round(SP_reactive_input$SP_opti_const_grouped$Net_Revenue *1.1/10^6,2)
                               }else if(input$SP_opti_restrictions$changes$changes[[1]][[4]] == "Percent"){
                                 
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Net Revenue" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                                 SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Net Revenue" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                               }
                             }
                             SP_reactive_input$SP_restrictions_1[grep("Spend|spend|invest|Invest",SP_reactive_input$SP_restrictions_1$KPI,ignore.case = TRUE,invert = TRUE),]$`Maximum Value` <- NA
                             SP_reactive_input$SP_restrictions_1[grep("Spend|spend|invest|Invest",SP_reactive_input$SP_restrictions_1$KPI,ignore.case = TRUE),]$`Minimum Value` <- NA
                             
                             SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions_1
                             SP_reactive_input$const_chg_flag <- 1
                           }else{
                             SP_reactive_input$SP_restrictions_selected <- hot_to_r(input$SP_opti_restrictions)
                             SP_reactive_input$const_chg_flag <- 1
                           }
                           
                           # else{
                           #   observeEvent(input$SP_opti_goal,{
                           #     
                           #     SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions[SP_reactive_input$SP_restrictions$KPI %in% c("Gross Margin % of NR","Scan Net Revenue","Trade Spend % of NR","Scan Gross Sales","Volume Sales",
                           #                                                                                                                                  input$SP_opti_ROI_selection,"Value Market Share"),]
                           #     SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions_selected[SP_reactive_input$SP_restrictions_selected$KPI != input$SP_opti_goal,c("Include the Constraint","Constraint Order","KPI","Scale","Last Year Value","Minimum Value","Maximum Value")]
                           #     SP_reactive_input$SP_restrictions_selected$`Constraint Order` <- as.factor(c(1:nrow(SP_reactive_input$SP_restrictions_selected)))
                           #   })
                           # }
                         }
                         print(SP_reactive_input$SP_restrictions_selected)
                         
                       } 
                     }
                   })
    observeEvent(c(
      input$SP_opti_cat,
      #input$SP_level,
      input$SP_opti_brand,
      input$SP_opti_format,
      input$SP_opti_ppg,
      input$SP_opti_date_start,
      input$SP_opti_date_end,
      input$SP_opti_reset),{
        
        #######Populating exclude ppg table and prod restrictions table
        if(!(is.null(SP_reactive_input$exclude_ppg_data_prep))){
          if(all(input$SP_opti_brand == "") & all(input$SP_opti_format == "") & all(input$SP_opti_ppg == "")){
            SP_reactive_input$SP_opti_const_ppg <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat,])
            
          }else if(all(input$SP_opti_format == "") & all(input$SP_opti_ppg == "")){
            SP_reactive_input$SP_opti_const_ppg <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat & SP_reactive_input$exclude_ppg_data_prep$Brand %in% input$SP_opti_brand,])
          }else if(all(input$SP_opti_format != "") & all(input$SP_opti_ppg == "")){
            SP_reactive_input$SP_opti_const_ppg <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat & SP_reactive_input$exclude_ppg_data_prep$Brand %in% input$SP_opti_brand & SP_reactive_input$exclude_ppg_data_prep$Format %in% input$SP_opti_format,])
          }else if(all(input$SP_opti_ppg != "")){
            SP_reactive_input$SP_opti_const_ppg <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat & SP_reactive_input$exclude_ppg_data_prep$Brand %in% input$SP_opti_brand & SP_reactive_input$exclude_ppg_data_prep$Format %in% input$SP_opti_format & SP_reactive_input$exclude_ppg_data_prep$PPG %in% input$SP_opti_ppg,])
          }
          
          SP_reactive_input$SP_exclude_ppg <- cbind(data.frame("Exclude" = rep(FALSE,nrow(unique(SP_reactive_input$SP_opti_const_ppg[,c("PPG","PPG_Description","Category","Brand","Format")]))),check.names = FALSE),unique(SP_reactive_input$SP_opti_const_ppg[,c("PPG","PPG_Description","Category","Brand","Format")]))
          names(SP_reactive_input$SP_exclude_ppg) <- c("Exclude","PPG","PPG Description","Category","Brand","Format")
          output$SP_opti_exclude_ppg <- renderRHandsontable({
            rhandsontable(SP_reactive_input$SP_exclude_ppg,rowHeaders = NULL)
          })
          #
          #SP_reactive_input$SP_opti_prod_restrictions_default$`Display &/or Feature Event in Last Year` <- ifelse(SP_reactive_input$SP_opti_prod_restrictions_default$LY_Display_Weeks == 0,"No","Yes")
          
          if(!(is.null(input$SP_opti_date_start)) & !is.null(input$SP_opti_date_end)){
            if(input$SP_opti_date_end != ""){
              if(input$SP_opti_date_start == "2021-12-31" & input$SP_opti_date_start == "2021-12-31"){
                SP_reactive_input$SP_prod_restrict_time <- SP_reactive_input$SP_opti_prod_restrictions_default
              }else{
                SP_reactive_input$SP_prod_restrict_time <- time_based_prod_const(SP_reactive_input$SP_opti_prod_restrictions_default,SP_reactive_input$SP_opti_const,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),SP_input_tesco_slot())
              }
            }else{
              SP_reactive_input$SP_prod_restrict_time <- SP_reactive_input$SP_opti_prod_restrictions_default
            }
          }else{
            SP_reactive_input$SP_prod_restrict_time <- SP_reactive_input$SP_opti_prod_restrictions_default
          }
          SP_reactive_input$SP_prod_restrict <- unique(SP_reactive_input$SP_prod_restrict_time[SP_reactive_input$SP_prod_restrict_time$PPG %in% unique(SP_reactive_input$SP_opti_const_ppg$PPG),c("PPG","PPG_Description","Brand","Format","MRRP Max","MRRP Min","RSP_Unit","LY_Investment","Min_Investment","Max_Investment","LSM_Promo_Price_Min","LSM_Promo_Price_Max","Non_LSM_Min_Promo_Price","Non_LSM_Max_Promo_Price","Min_Total_Weeks","Max_Total_Weeks","Non_LSM_Min_Total_Weeks","Non_LSM_Max_Total_Weeks","Min_Display_Weeks","Max_Display_Weeks","Non_LSM_Min_Display_Weeks","Non_LSM_Max_Display_Weeks")])
          #removed
          #SP_reactive_input$SP_prod_restrict[,sapply(SP_reactive_input$SP_prod_restrict, is.numeric)] <- round(SP_reactive_input$SP_prod_restrict[,sapply(SP_reactive_input$SP_prod_restrict, is.numeric)],2)
          names(SP_reactive_input$SP_prod_restrict) <- c("PPG","PPG Description","Brand","Format","Max MRRP","Min MRRP","RSP","LY Investment","Min Investment","Max Investment","LSM Min Floor Price","LSM Max Floor Price","Unconstrained Min Promo Price","Unconstrained Max Promo Price","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","Unconstrained Min Total Promo Weeks","Unconstrained Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks","Unconstrained Min Display &/or Feature Weeks","Unconstrained Max Display &/or Feature Weeks")
          #SP_reactive_input$SP_prod_restrict <- data.frame("PPG" = unique(SP_reactive_input$SP_opti_const_ppg$PPG),"Min Investment" = rep(0,length(unique(SP_reactive_input$SP_opti_nielsen_ppg$PPG))), "Max Investment" = rep(0,length(unique(SP_reactive_input$SP_opti_nielsen_ppg$PPG))),"Min Discount %" = rep(0,length(unique(SP_reactive_input$SP_opti_nielsen_ppg$PPG))), "Max Discount %" = rep(0,length(unique(SP_reactive_input$SP_opti_nielsen_ppg$PPG))), "Min Promo Weeks" = rep(0,length(unique(SP_reactive_input$SP_opti_nielsen_ppg$PPG))), "Max Promo Weeks" = rep(0,length(unique(SP_reactive_input$SP_opti_nielsen_ppg$PPG))), "Min Display &/or Feature Weeks" = rep(0,length(unique(SP_reactive_input$SP_opti_nielsen_ppg$PPG))),"Max Display Weeks" = rep(0,length(unique(SP_reactive_input$SP_opti_nielsen_ppg$PPG))),check.names = FALSE)
          #
          output$SP_opti_prod_restrictions <- renderRHandsontable({
            color_renderer <- "
          function(instance, td) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.background = '#c2fafe';
          }
          "
            
            #td.style.format = '0,0.00';
            rhandsontable(SP_reactive_input$SP_prod_restrict,rowHeaders = NULL) %>%
              hot_cols(fixedColumnsLeft = 2) %>%
              hot_col(c("PPG","PPG Description","Brand","Format","Max MRRP","Min MRRP","RSP","LY Investment","LSM Min Floor Price","LSM Max Floor Price","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks"), readOnly = TRUE) %>%
              hot_col(c("PPG","PPG Description","Brand","Format","Max MRRP","Min MRRP","RSP","LY Investment","LSM Min Floor Price","LSM Max Floor Price","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks"), renderer = color_renderer) %>%
              hot_col(c("LY Investment","Min Investment","Max Investment"), format = "0,0.00")
          })
        }
      })
    observeEvent(c(input$SP_proceed_summ,
                   #input$SP_level,
                   input$SP_summ_top_panel_right,
                   input$SP_opti_op_top_panel_right,
                   input$SP_sim_top_panel_right),{
                     
                     SP_reactive_input$SP_cmp_scn_ip <- SP_cmp_scn_ip(SP_reactive_input$folder_path)
                     updateSelectizeInput(session,"SP_cmp_scn_cat",selected = unique(SP_reactive_input$SP_cmp_scn_ip$Category)[1],choices = unique(SP_reactive_input$SP_cmp_scn_ip$Category))
                     
                     observeEvent({
                       input$SP_cmp_scn_cat},{
                         updateSelectizeInput(session,"SP_cmp_scn_brand",choices = as.character(unique(SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Category == input$SP_cmp_scn_cat,]$Brand)),selected = as.character(unique(SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Category == input$SP_cmp_scn_cat,]$Brand))[1])
                       }
                     )
                     
                     observeEvent({
                       input$SP_cmp_scn_brand},{
                         #
                         updateSelectizeInput(session,"SP_cmp_scn_format",choices = c("",as.character(unique(SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Category == input$SP_cmp_scn_cat & SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand,]$Format))))
                       }
                     )
                     
                     observeEvent(c(
                       input$SP_cmp_scn_brand,
                       input$SP_cmp_scn_format,
                       input$SP_cmp_scn_ppg,
                       input$SP_cmp_scn_merge_confirm),{
                         if(input$SP_cmp_scn_format == "" | is.null(input$SP_cmp_scn_format)){
                           updateSelectizeInput(session,"SP_cmp_scn_tpo",choices = c("",as.character(unique(SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Category == input$SP_cmp_scn_cat & SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand,]$`TPO ID`))))
                           
                         }else if(input$SP_cmp_scn_ppg == "" | is.null(input$SP_cmp_scn_ppg)){
                           updateSelectizeInput(session,"SP_cmp_scn_tpo",choices = c("",as.character(unique(SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Category == input$SP_cmp_scn_cat & SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand & SP_reactive_input$SP_cmp_scn_ip$Format == input$SP_cmp_scn_format,]$`TPO ID`))))
                         }else{
                           updateSelectizeInput(session,"SP_cmp_scn_tpo",choices = c("",as.character(unique(SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Category == input$SP_cmp_scn_cat & SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand & SP_reactive_input$SP_cmp_scn_ip$Format == input$SP_cmp_scn_format & SP_reactive_input$SP_cmp_scn_ip$PPG == input$SP_cmp_scn_ppg,]$`TPO ID`))))
                         }
                       }
                     )
                     
                   })
    
    observeEvent({
      #input$SP_level
      input$SP_cmp_scn_brand
      input$SP_cmp_scn_format
      input$SP_cmp_scn_ppg
      input$SP_cmp_scn_tpo},{
        SP_reactive_input$SP_cmp_scn_ip <- SP_cmp_scn_ip(SP_reactive_input$folder_path)
        if((input$SP_cmp_scn_format == "" | is.null(input$SP_cmp_scn_format)) & (input$SP_cmp_scn_ppg == "" | is.null(input$SP_cmp_scn_ppg)) & (input$SP_cmp_scn_tpo == "" | is.null(input$SP_cmp_scn_tpo))){
          hide("SP_cmp_scn_op_top_panel_download")
          SP_reactive_input$SP_cmp_scn_ip <- SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand,]
        }else if((input$SP_cmp_scn_ppg == "" | is.null(input$SP_cmp_scn_ppg)) & (input$SP_cmp_scn_tpo == "" | is.null(input$SP_cmp_scn_tpo))){
          hide("SP_cmp_scn_op_top_panel_download")
          SP_reactive_input$SP_cmp_scn_ip <- SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand & SP_reactive_input$SP_cmp_scn_ip$Format == input$SP_cmp_scn_format,]
        }else if(input$SP_cmp_scn_tpo == "" | is.null(input$SP_cmp_scn_tpo)){
          hide("SP_cmp_scn_op_top_panel_download")
          SP_reactive_input$SP_cmp_scn_ip <- SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand & SP_reactive_input$SP_cmp_scn_ip$Format == input$SP_cmp_scn_format & SP_reactive_input$SP_cmp_scn_ip$PPG == input$SP_cmp_scn_ppg,]
        }else{
          shinyjs::show("SP_cmp_scn_op_top_panel_download")
          if(input$SP_cmp_scn_format == "" | is.null(input$SP_cmp_scn_format)){
            SP_reactive_input$SP_cmp_scn_ip <- SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand & SP_reactive_input$SP_cmp_scn_ip$`TPO ID` == input$SP_cmp_scn_tpo,]
            #
            load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",as.character(SP_reactive_input$SP_cmp_scn_ip$`TPO ID`),".RData"))
            SP_reactive_input$SP_cmp_scn_download <- tpo_list_temp$opti_output
          }else if(input$SP_cmp_scn_ppg == "" | is.null(input$SP_cmp_scn_ppg)){
            SP_reactive_input$SP_cmp_scn_ip <- SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand & SP_reactive_input$SP_cmp_scn_ip$Format == input$SP_cmp_scn_format & SP_reactive_input$SP_cmp_scn_ip$`TPO ID` == input$SP_cmp_scn_tpo,]
            load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",as.character(SP_reactive_input$SP_cmp_scn_ip$`TPO ID`),".RData"))
            SP_reactive_input$SP_cmp_scn_download <- tpo_list_temp$opti_output
          }else{
            SP_reactive_input$SP_cmp_scn_ip <- SP_reactive_input$SP_cmp_scn_ip[SP_reactive_input$SP_cmp_scn_ip$Brand == input$SP_cmp_scn_brand & SP_reactive_input$SP_cmp_scn_ip$Format == input$SP_cmp_scn_format & SP_reactive_input$SP_cmp_scn_ip$PPG == input$SP_cmp_scn_ppg & SP_reactive_input$SP_cmp_scn_ip$`TPO ID` == input$SP_cmp_scn_tpo,]
            load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",as.character(SP_reactive_input$SP_cmp_scn_ip$`TPO ID`),".RData"))
            SP_reactive_input$SP_cmp_scn_download <- tpo_list_temp$opti_output
          }
        }
      })
    observeEvent(c(input$SP_kam_top_panel_right,
                   input$SP_proceed_summ
                   #input$SP_level
    ),{
      
      if((file.exists(paste0(SP_reactive_input$folder_path,"/Final_Optimized_Plan.RData")))){
        load(paste0(SP_reactive_input$folder_path,"/Final_Optimized_Plan.RData"))
        SP_reactive_input$SP_final_tpo_plan <- tpo_list_temp
        SP_reactive_input$SP_date_vector_tbo <- unique(SP_reactive_input$SP_final_tpo_plan$opti_output$Week_Ending)[unique(SP_reactive_input$SP_final_tpo_plan$opti_output$Week_Ending) >= Sys.Date() + 42]
        
        updateSelectizeInput(session,"SP_opti_date_on_start",choices = SP_reactive_input$SP_date_vector_tbo,selected = min(SP_reactive_input$SP_date_vector_tbo))
        
        if(input$SP_opti_date_on_start != ""){
          SP_reactive_input$SP_date_vector_end_tbo <- SP_reactive_input$SP_date_vector_tbo[SP_reactive_input$SP_date_vector_tbo >= (as.Date(input$SP_opti_date_on_start) + 49)]
        }else{
          SP_reactive_input$SP_date_vector_end_tbo <- SP_reactive_input$SP_date_vector_tbo[SP_reactive_input$SP_date_vector_tbo >= (min(SP_reactive_input$SP_date_vector_tbo)+ 49)]
        }
        updateSelectizeInput(session,"SP_opti_date_on_end",choices = SP_reactive_input$SP_date_vector_end_tbo,selected = max(SP_reactive_input$SP_date_vector_end_tbo))
        updateSelectizeInput(session,"SP_opti_cat_on",selected = as.character(SP_reactive_input$SP_final_tpo_plan$cat),choices = as.character(SP_reactive_input$SP_final_tpo_plan$cat))
        updateSelectizeInput(session,"SP_opti_brand_on",choices = as.character(SP_reactive_input$SP_final_tpo_plan$brand),selected = as.character(SP_reactive_input$SP_final_tpo_plan$brand))      
        updateSelectizeInput(session,"SP_opti_format_on",choices = as.character(SP_reactive_input$SP_final_tpo_plan$format),selected = as.character(SP_reactive_input$SP_final_tpo_plan$format))
        updateSelectizeInput(session,"SP_opti_ppg_on",choices = as.character(SP_reactive_input$SP_final_tpo_plan$ppg),selected = as.character(SP_reactive_input$SP_final_tpo_plan$ppg))
        updateSelectizeInput(session,"SP_opti_tpo_on",choices = as.character(SP_reactive_input$SP_final_tpo_plan$tpo_id),selected = as.character(SP_reactive_input$SP_final_tpo_plan$tpo_id))
      }
      # else{
      #   showModal(
      #     modalDialog(
      #       title = "Final Optimized Plan is not available",
      #       "Finalize any existing plan in Compare Scenarios screen or run a new plan and save it",
      #       size = "l",
      #       footer = tagList(
      #         actionButton("SP_opti_on_cmp_scn","Go to Compare Scenarios",style = "color:#fff; background-color: #0099DC;border-color: #0099DC"),
      #         actionButton("SP_opti_on_opti_ann","Go to Annual Optimizer",style="color: #fff; background-color: #0099DC;border-color: #0099DC")
      #       )
      #     )
      #   )
      # }
    })
    
  })
  #Navigation buttons in Summary tab
  observeEvent(input$SP_summ_top_panel_left,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu1")
  })
  
  observeEvent(input$SP_summ_top_panel_right,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu3")
  })
  
  #Navigation buttons in Promotion Calendar tab
  observeEvent(input$SP_cal_top_panel_left,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu2")
  })
  
  observeEvent(input$SP_cal_top_panel_right,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu5")
  })
  
  
  
  observeEvent(c(input$SP_opti_brand,
                 input$SP_level,
                 input$SP_opti_format,
                 input$SP_opti_ppg,
                 input$SP_opti_date_start,
                 input$SP_opti_date_end,
                 input$SP_opti_reset),{
                   SP_reactive_input$const_chg_flag <- 0
                 })
  
  
  
  observeEvent(input$SP_opti_goal,{
    if(!(is.null(SP_reactive_input$SP_restrictions))){
      
      SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions_selected[SP_reactive_input$SP_restrictions_selected$KPI != input$SP_opti_goal,c("Include the Constraint","Constraint Order","KPI","Scale","Last Year Value","Minimum Value","Maximum Value")]
      SP_reactive_input$SP_restrictions_selected <- SP_reactive_input$SP_restrictions_selected[c(1:6),]
      SP_reactive_input$SP_restrictions_selected$`Constraint Order` <- as.factor(c(1:nrow(SP_reactive_input$SP_restrictions_selected)))
    }
  })
  
  observeEvent(input$SP_opti_ROI_selection,{
    updateSelectizeInput(session,"SP_opti_goal",choices = c("Scan Net Revenue","Gross Margin % of NR","Volume Sales","Scan Gross Sales",input$SP_opti_ROI_selection,"Trade Spend % of NR","Value Market Share","Trade Spend % of NIS","Gross Margin"),selected = "Scan Net Revenue")
  })
  
  output$SP_opti_restrictions <- renderRHandsontable({
    validate(
      need(SP_reactive_input$SP_restrictions_selected,"Select  Goal")
    )
    color_renderer <- "
    function(instance, td) {
    Handsontable.renderers.NumericRenderer.apply(this, arguments);
    td.style.background = '#c2fafe';
    }
    "
    
    #####Populating  constraint table
    rhandsontable(SP_reactive_input$SP_restrictions_selected[c(1:6),],rowHeaders = NULL) %>%
      hot_col("Last Year Value",readOnly = TRUE, renderer = color_renderer) %>%
      hot_col("KPI", type = "dropdown",source = c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Scan Gross Sales","Gross Margin","Volume Sales",input$SP_opti_ROI_selection,"Value Market Share")) %>%
      hot_col(c("Last Year Value","Minimum Value","Maximum Value"), format = "0,0.00") %>%
      #hot_col("Last Year Value", renderer = color_renderer, format = "0,0.00") %>%
      #hot_cell(as.numeric(row.names(SP_reactive_input$SP_restrictions_selected[SP_reactive_input$SP_restrictions_selected$KPI == "Trade Spend % of NR",])), "KPI",type = "dropdown", source = c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")) %>%
      hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_restrictions_selected$KPI)[1]), "Scale", readOnly = TRUE) %>%
      hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_restrictions_selected$KPI)[2]), "Scale", readOnly = TRUE) %>%
      hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_restrictions_selected$KPI)[3]), "Scale", readOnly = TRUE)
    #hot_col(c("Last Year Value","Minimum Value","Maximum Value"),"0,0.0000")
  })
  
  observeEvent(input$SP_opti_op_top_panel_left,{
    if(!is.null(input$SP_opti_prod_restrictions)){
      SP_reactive_input$SP_prod_restrict <- hot_to_r(input$SP_opti_prod_restrictions)
    }else{
      SP_reactive_input$SP_prod_restrict <- SP_reactive_input$SP_prod_restrict
    }
    
    if(any(names(SP_reactive_input$SP_prod_restrict) %in% c("LSM Min Floor Price","LSM Max Floor Price"))){
      setnames(SP_reactive_input$SP_prod_restrict,c("LSM Min Floor Price","LSM Max Floor Price"),c("LSM Min Promo Price","LSM Max Promo Price"))
    }
    output$SP_opti_prod_restrictions <- renderRHandsontable({
      color_renderer <- "
      function(instance, td) {
      Handsontable.renderers.NumericRenderer.apply(this, arguments);
      td.style.background = '#c2fafe';
      }
      "
      #
      rhandsontable(SP_reactive_input$SP_prod_restrict,rowHeaders = NULL) %>%
        hot_cols(fixedColumnsLeft = 2) %>%
        hot_col(c("PPG","PPG Description","Brand","Format","Max MRRP","Min MRRP","RSP","LY Investment","LSM Min Promo Price","LSM Max Promo Price","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks"), readOnly = TRUE) %>%
        hot_col(c("PPG","PPG Description","Brand","Format","Max MRRP","Min MRRP","RSP","LY Investment","LSM Min Promo Price","LSM Max Promo Price","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks"), renderer = color_renderer) %>%
        hot_col(c("LY Investment","Min Investment","Max Investment"), format = "0,0.00")
    })
  })
  
  observeEvent(input$SP_opti_disp_pp_const,{
    if(input$SP_opti_disp_pp_const == TRUE){
      shinyjs::show("SP_opti_max_display")
    }else{
      shinyjs::hide("SP_opti_max_display")
    }
  })
  
  ###########Running Annual Optimization#############
  observeEvent(input$SP_opti_run,{
    ###Initializing warning flag and empty TPO list for saving all the optimization variables
    warning_flag = 0
    SP_TPO_list <- list()
    
    SP_TPO_list$coun <- input$SP_opti_coun
    SP_TPO_list$cust <- input$SP_opti_cust
    SP_TPO_list$cat <- input$SP_opti_cat
    SP_TPO_list$brand <- input$SP_opti_brand
    ###Making Format as "ALL" for brand level optimization
    if(is.null(input$SP_opti_format) | all(input$SP_opti_format == "")){
      SP_reactive_input$opti_format_input <- "ALL"
    }else{
      SP_reactive_input$opti_format_input <- input$SP_opti_format
    }
    
    if(is.null(input$SP_opti_ppg)){
      SP_reactive_input$opti_ppg_input <- "ALL"
    }else if(input$SP_opti_ppg == ""){
      SP_reactive_input$opti_ppg_input <- "ALL"
    }else{
      SP_reactive_input$opti_ppg_input <- input$SP_opti_ppg
    }
    
    SP_TPO_list$format <- SP_reactive_input$opti_format_input
    SP_TPO_list$ppg <- SP_reactive_input$opti_ppg_input
    SP_TPO_list$goal_shiny <- input$SP_opti_goal
    SP_TPO_list$sign <- input$SP_opti_sign
    SP_TPO_list$roi <- input$SP_opti_ROI_selection
    SP_TPO_list$start_date <- input$SP_opti_start_date_ui
    SP_TPO_list$end_date <- input$SP_opti_end_date_ui
    SP_TPO_list$ppg_exclude_shiny <- hot_to_r(input$SP_opti_exclude_ppg)
    SP_TPO_list$ppg_exclude <- SP_TPO_list$ppg_exclude_shiny[SP_TPO_list$ppg_exclude_shiny$Exclude == TRUE,]
    
    ###Displaying a warning if all the PPGs are excluded in the optimization screen
    if(nrow(SP_TPO_list$ppg_exclude_shiny[SP_TPO_list$ppg_exclude_shiny$Exclude == FALSE,]) == 0){
      sendSweetAlert(session,"All the PPGs are exlcuded!!","No PPG available to run Optimization",type = "error")
      warning_flag = 1
    }
    SP_TPO_list$opti_const_shiny <- hot_to_r(input$SP_opti_restrictions)
    ###Replacing the NA rows with some constraint as 6 constraints are needed to run optimization
    
    SP_TPO_list$opti_const <- SP_TPO_list$opti_const_shiny
    levels(SP_TPO_list$opti_const$`Constraint Order`) <- c(1:6)
    
    if(any(is.na(SP_TPO_list$opti_const_shiny$KPI))){
      SP_TPO_list$opti_const[is.na(SP_TPO_list$opti_const$KPI),c("Include the Constraint")] <- FALSE
      SP_TPO_list$opti_const[is.na(SP_TPO_list$opti_const$KPI),c("Constraint Order")] <- 6
      SP_TPO_list$opti_const[is.na(SP_TPO_list$opti_const$KPI),c("Last Year Value","Minimum Value","Maximum Value")] <- NA
      SP_TPO_list$opti_const[is.na(SP_TPO_list$opti_const$KPI),c("Scale")] <- c("Absolute")
      SP_TPO_list$opti_const[is.na(SP_TPO_list$opti_const$KPI),c("KPI")] <- setdiff(c("Scan Net Revenue","Gross Margin % of NR","Volume Sales","Scan Gross Sales","Incremental GM ROI","Trade Spend % of NR","Value Market Share","Trade Spend % of NIS","Gross Margin"),c(input$SP_opti_goal,unique(SP_TPO_list$opti_const[!(is.na(SP_TPO_list$opti_const$KPI)),]$KPI)))[1]
    }
    SP_TPO_list$opti_const <- SP_TPO_list$opti_const[!(is.na(SP_TPO_list$opti_const$`Include the Constraint`)),]
    SP_TPO_list$opti_const <- SP_TPO_list$opti_const[order(-SP_TPO_list$opti_const$`Include the Constraint`,SP_TPO_list$opti_const$`Constraint Order`),]
    SP_TPO_list$opti_const$`Minimum Value` <- as.numeric(SP_TPO_list$opti_const$`Minimum Value`)
    SP_TPO_list$opti_const$`Maximum Value` <- as.numeric(SP_TPO_list$opti_const$`Maximum Value`)

    # Enforce Scale deterministically by KPI to avoid accidental mis-scaling
    # (e.g., Gross Margin should always be Absolute, not Percent).
    abs_kpis <- c("Scan Net Revenue", "Scan Gross Sales", "Gross Margin", "Volume Sales")
    pct_kpis <- c("Gross Margin % of NR", "Trade Spend % of NR", "Trade Spend % of NIS", "Value Market Share")
    SP_TPO_list$opti_const[SP_TPO_list$opti_const$KPI %in% abs_kpis, "Scale"] <- "Absolute"
    SP_TPO_list$opti_const[SP_TPO_list$opti_const$KPI %in% pct_kpis, "Scale"] <- "Percent"
    
    ###Converting percentage constraints to absolute values for facts which are not percentages and market share
    SP_TPO_list$opti_const[SP_TPO_list$opti_const$Scale == "Percent" & !(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Minimum Value` <- SP_TPO_list$opti_const[SP_TPO_list$opti_const$Scale == "Percent" & !(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Minimum Value` * SP_TPO_list$opti_const[SP_TPO_list$opti_const$Scale == "Percent" & !(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Last Year Value`/100
    SP_TPO_list$opti_const[SP_TPO_list$opti_const$Scale == "Percent" & !(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Maximum Value` <- SP_TPO_list$opti_const[SP_TPO_list$opti_const$Scale == "Percent" & !(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Maximum Value` * SP_TPO_list$opti_const[SP_TPO_list$opti_const$Scale == "Percent" & !(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Last Year Value`/100
    
    ###Converting all the facts to absoulte scale except ("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")
    SP_TPO_list$opti_const[!(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Minimum Value` <- SP_TPO_list$opti_const[!(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Minimum Value` * 10^6
    SP_TPO_list$opti_const[!(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Maximum Value` <- SP_TPO_list$opti_const[!(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Maximum Value` * 10^6
    SP_TPO_list$opti_const[!(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Last Year Value` <- SP_TPO_list$opti_const[!(SP_TPO_list$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Last Year Value` * 10^6
    
    ###Displaying a warning if sign is selected as "Maximum" for facts related to Investment
    if(grepl("Spend|spend|invest|Invest",input$SP_opti_goal,ignore.case = TRUE) & input$SP_opti_sign == "Max"){
      warning_flag = 1
      confirmSweetAlert(session,"opti_differ_sign",title = " Goal",text = "Goal changed from default value!!",type = "warning")
    }
    observeEvent(input$opti_differ_sign,{
      if(!(is.null(input$opti_differ_sign))){
        if(input$opti_differ_sign == FALSE){
          warning_flag = 1
        }else if(input$opti_differ_sign == TRUE){
          warning_flag = 0
        }
      }
    })
    
    ###Displaying a warning if sign is selected as "Minimum" for facts other than Investment
    if(!(grepl("Spend|spend|invest|Invest",input$SP_opti_goal,ignore.case = TRUE)) & input$SP_opti_sign == "Min"){
      confirmSweetAlert(session,"opti_differ_sign_1",title = " Goal",text = "Goal changed from default value!!",type = "warning")
      warning_flag = 1
      print(input$opti_differ_sign_1)
    }
    
    observeEvent(input$opti_differ_sign_1,{
      if(!(is.null(input$opti_differ_sign_1))){
        if(input$opti_differ_sign_1 == FALSE){
          print(input$opti_differ_sign_1)
          warning_flag = 1
        }
      }
    })
    
    ###Assigning very large negative value to empty cells in Minimum Constraint and very large value to empty cells in Maximum Constarint
    if(any(is.na(SP_TPO_list$opti_const$`Minimum Value`)) | any(SP_TPO_list$opti_const$`Include the Constraint` == FALSE)){
      SP_TPO_list$opti_const[is.na(SP_TPO_list$opti_const$`Minimum Value`) | (SP_TPO_list$opti_const$`Include the Constraint` == FALSE),]$`Minimum Value` <- -(10^20)
    }
    if(any(is.na(SP_TPO_list$opti_const$`Maximum Value`)) | any(SP_TPO_list$opti_const$`Include the Constraint` == FALSE)){
      SP_TPO_list$opti_const[is.na(SP_TPO_list$opti_const$`Maximum Value`) | (SP_TPO_list$opti_const$`Include the Constraint` == FALSE),]$`Maximum Value` <- (10^20)
    }
    
    ###Making all NA's as blank in "opti_const_shiny" to display them back when selected through saved plans
    if(any(is.na(SP_TPO_list$opti_const_shiny$`Minimum Value`)) | any(SP_TPO_list$opti_const_shiny$`Include the Constraint` == FALSE)){
      SP_TPO_list$opti_const_shiny[is.na(SP_TPO_list$opti_const_shiny$`Minimum Value`) | (SP_TPO_list$opti_const_shiny$`Include the Constraint` == FALSE),]$`Minimum Value` <- ""
    }
    if(any(is.na(SP_TPO_list$opti_const_shiny$`Maximum Value`)) | any(SP_TPO_list$opti_const_shiny$`Include the Constraint` == FALSE)){
      SP_TPO_list$opti_const_shiny[is.na(SP_TPO_list$opti_const_shiny$`Maximum Value`) | (SP_TPO_list$opti_const_shiny$`Include the Constraint` == FALSE),]$`Maximum Value` <- ""
    }
    
    ###Mapping for the constraint names as per the optimization code
    opti_const_mapping <- data.frame("KPI" = c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Scan Gross Sales","Gross Margin","Volume Sales",input$SP_opti_ROI_selection,"Value Market Share"),"KPI_Mapping" = c("Net_Sales_model","GM_percent_model","Trade_as_per_NR_model","Trade_as_per_NIS_model","Gross_sales_model","Gross_margin_model","Volume_sales_model","ROI_model","Market_Share_model"))
    SP_TPO_list$opti_const <- left_join(SP_TPO_list$opti_const,opti_const_mapping,by = "KPI")
    
    ###opti goal
    SP_TPO_list$goal <- left_join(data.frame("Goal" = SP_TPO_list$goal_shiny),opti_const_mapping,by = c("Goal" = "KPI"))
    
    ###Prod constraint
    if(!is.null(input$SP_opti_prod_restrictions)){
      SP_TPO_list$prod_const_shiny <- hot_to_r(input$SP_opti_prod_restrictions)
    }else{
      SP_TPO_list$prod_const_shiny <- SP_reactive_input$SP_prod_restrict
    }
    
    if(all(c("LSM Min Floor Price","LSM Max Floor Price") %in% names(SP_reactive_input$SP_prod_restrict))){
      setnames(SP_TPO_list$prod_const_shiny,c("LSM Min Floor Price","LSM Max Floor Price"),c("LSM Min Promo Price","LSM Max Promo Price"))
    }
    
    SP_TPO_list$prod_const_shiny <- data.table(left_join(SP_TPO_list$prod_const_shiny,SP_reactive_input$SP_opti_prod_restrictions_default[,!(names(SP_reactive_input$SP_opti_prod_restrictions_default) %in% c("Brand","Format","MRRP Max","MRRP Min","RSP_Unit","LY_Investment","Min_Investment","Max_Investment","LSM_Promo_Price_Min","LSM_Promo_Price_Max","Non_LSM_Min_Promo_Price","Non_LSM_Max_Promo_Price","Min_Total_Weeks","Max_Total_Weeks","Non_LSM_Min_Total_Weeks","Non_LSM_Max_Total_Weeks","Min_Display_Weeks","Max_Display_Weeks","Non_LSM_Min_Display_Weeks","Non_LSM_Max_Display_Weeks")),with = FALSE],by = "PPG"))
    
    ###Checking if any maxiumum value is less than minimum value
    if(any(SP_TPO_list$opti_const$`Maximum Value` < SP_TPO_list$opti_const$`Minimum Value`)){
      sendSweetAlert(session,"Error!!","Maximum Constraint values should be greater than or equal to Minimum Constraint values",type = "error")
      warning_flag = 1
    }
    ###Displaying an error if no product is available in selected format or brand
    if(nrow(SP_TPO_list$prod_const_shiny) == 0){
      sendSweetAlert(session,"Error!!","No Product available to run Optimization in the selected brand and format",type = "error")
      warning_flag = 1
    }
    ###Checking for any duplicate values in constraint order
    if(length(unique(SP_TPO_list$opti_const_shiny$`Constraint Order`)) < length(SP_TPO_list$opti_const_shiny$`Constraint Order`)){
      sendSweetAlert(session,"Error!!","Duplicate values in constraint order",type = "error")
      warning_flag = 1
    }
    
    # if(nrow(SP_TPO_list$opti_const_shiny[SP_TPO_list$opti_const_shiny$`Include the Constraint` == TRUE,]) > 6){
    #   sendSweetAlert(session,"Error!!","Optimizer cannot have more than 6 constraints",type = "error")
    #   warning_flag = 1
    # }
    
    ###Checking if any of the Unconstrained minimum total promo weeks is 0
    if(any(mod(SP_TPO_list$prod_const_shiny$`Unconstrained Min Total Promo Weeks`, 3) != 0)){
      SP_TPO_list$prod_const_shiny[mod(SP_TPO_list$prod_const_shiny$`Unconstrained Min Total Promo Weeks`, 3) != 0,]$`Unconstrained Min Total Promo Weeks` <- ceiling(SP_TPO_list$prod_const_shiny[mod(SP_TPO_list$prod_const_shiny$`Unconstrained Min Total Promo Weeks`, 3) != 0,]$`Unconstrained Min Total Promo Weeks`/3) * 3
      sendSweetAlert(session,"Min Total Promo Weeks","Min Total Promo Weeks rounded up to a multiple of 3",type = "warning")
    }
    
    ###Checking if any of the Unconstrained maximum total promo weeks is 0
    if(any(mod(SP_TPO_list$prod_const_shiny$`Unconstrained Max Total Promo Weeks`, 3) != 0)){
      SP_TPO_list$prod_const_shiny[mod(SP_TPO_list$prod_const_shiny$`Unconstrained Max Total Promo Weeks`, 3) != 0,]$`Unconstrained Max Total Promo Weeks` <- ceiling(SP_TPO_list$prod_const_shiny[mod(SP_TPO_list$prod_const_shiny$`Unconstrained Max Total Promo Weeks`, 3) != 0,]$`Unconstrained Max Total Promo Weeks`/3) * 3
      sendSweetAlert(session,"Max Total Promo Weeks","Max Total Promo Weeks rounded up to a multiple of 3",type = "warning")
    }
    
    ###Checking if any of the Unconstrained maximum display weeks is 0
    if(any(mod(SP_TPO_list$prod_const_shiny$`Unconstrained Max Display &/or Feature Weeks`, 3) != 0)){
      SP_TPO_list$prod_const_shiny[mod(SP_TPO_list$prod_const_shiny$`Unconstrained Max Display &/or Feature Weeks`, 3) != 0,]$`Unconstrained Max Display &/or Feature Weeks` <- ceiling(SP_TPO_list$prod_const_shiny[mod(SP_TPO_list$prod_const_shiny$`Unconstrained Max Display &/or Feature Weeks`, 3) != 0,]$`Unconstrained Max Display &/or Feature Weeks`/3) * 3
      sendSweetAlert(session,"Max Total Display Weeks","Max Total Display Weeks rounded up to a multiple of 3",type = "warning")
    }
    
    ###Checking if any of the Unconstrained minimum display weeks is 0
    if(any(mod(SP_TPO_list$prod_const_shiny$`Unconstrained Min Display &/or Feature Weeks`, 3) != 0)){
      SP_TPO_list$prod_const_shiny[mod(SP_TPO_list$prod_const_shiny$`Unconstrained Min Display &/or Feature Weeks`, 3) != 0,]$`Unconstrained Min Display &/or Feature Weeks` <- ceiling(SP_TPO_list$prod_const_shiny[mod(SP_TPO_list$prod_const_shiny$`Unconstrained Min Display &/or Feature Weeks`, 3) != 0,]$`Unconstrained Min Display &/or Feature Weeks`/3) * 3
      sendSweetAlert(session,"Min Total Display Weeks","Min Total Display Weeks rounded up to a multiple of 3",type = "warning")
    }
    
    ###Checking if any of the Unconstrained minimum total promo weeks is greater than 27
    if(any(SP_TPO_list$prod_const_shiny$`Unconstrained Min Total Promo Weeks` > 27)){
      sendSweetAlert(session,"Error!!","Min Total Promo Weeks cannot be > 27 for a PPG",type = "error")
      warning_flag = 1
    }
    
    ###Checking if any of the Unconstrained maximum total promo weeks is greater than 27
    if(any(SP_TPO_list$prod_const_shiny$`Unconstrained Max Total Promo Weeks` > 27)){
      sendSweetAlert(session,"Error!!","Max Total Promo Weeks cannot be > 27 for a PPG",type = "error")
      warning_flag = 1
    }
    
    ###Checking if any of the Unconstrained maximum display weeks is greater than 27
    if(any(SP_TPO_list$prod_const_shiny$`Unconstrained Max Display &/or Feature Weeks` > 27)){
      sendSweetAlert(session,"Error!!","Max Display Weeks cannot be > 27 for a PPG",type = "error")
      warning_flag = 1
    }
    
    ###Checking if any of the Unconstrained minimum display weeks is greater than 27
    if(any(SP_TPO_list$prod_const_shiny$`Unconstrained Min Display &/or Feature Weeks` > 27)){
      sendSweetAlert(session,"Error!!","Min Display Weeks cannot be > 27 for a PPG",type = "error")
      warning_flag = 1
    }
    
    ###Checking if any of the Unconstrained minimum promo weeks is greater than maximum promo weeks
    if(any(SP_TPO_list$prod_const_shiny$`Unconstrained Min Total Promo Weeks` > SP_TPO_list$prod_const_shiny$`Unconstrained Max Total Promo Weeks`)){
      sendSweetAlert(session,"Error!!","Min Total Promo Weeks cannot be > Max Total Promo Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    ###Checking if any of the Unconstrained minimum display weeks is greater than maximum display weeks
    if(any(SP_TPO_list$prod_const_shiny$`Unconstrained Min Display &/or Feature Weeks` > SP_TPO_list$prod_const_shiny$`Unconstrained Max Display &/or Feature Weeks`)){
      sendSweetAlert(session,"Error!!","Min Display Weeks cannot be > Max Display Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    ###Checking if any of the Unconstrained maximum display weeks is greater than maximum promo weeks
    if(any(SP_TPO_list$prod_const_shiny$`Unconstrained Max Display &/or Feature Weeks` > SP_TPO_list$prod_const_shiny$`Unconstrained Max Total Promo Weeks`)){
      sendSweetAlert(session,"Error!!","Max Display Weeks cannot be > Max Total Promo Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    ###Checking if any of the Unconstrained minimum display weeks is greater than minimum promo weeks
    if(any(SP_TPO_list$prod_const_shiny$`Unconstrained Min Display &/or Feature Weeks` > SP_TPO_list$prod_const_shiny$`Unconstrained Min Total Promo Weeks`)){
      sendSweetAlert(session,"Error!!","Min Display Weeks cannot be > Min Total Promo Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    ###Running the optimization only if warning flag is zero
    if(warning_flag == 0){
      ###Preparing the Product constraint files as required by the optimization code
      SP_TPO_list$prod_const <- as.data.frame(SP_TPO_list$prod_const_shiny[,c("PPG","Brand","Format","LSM Min Promo Price","LSM Max Promo Price","Unconstrained Min Promo Price","Unconstrained Max Promo Price","Unconstrained Min Total Promo Weeks","Unconstrained Max Total Promo Weeks","Unconstrained Min Display &/or Feature Weeks","Unconstrained Max Display &/or Feature Weeks","Global_Floor_Price")])
      names(SP_TPO_list$prod_const) <- c("PPG","BRAND","FORMAT","LSM_Promo_Price_Min", "LSM_Promo_Price_Max", "Non_LSM_Min_Promo_Price", "Non_LSM_Max_Promo_Price","Min_Disc_Slots","Max_Disc_Slots","Min_Display_Slots","Max_Display_Slots","Global_Floor_Price")
      SP_TPO_list$prod_const[,grep("Slots",names(SP_TPO_list$prod_const))] <- SP_TPO_list$prod_const[,grep("Slots",names(SP_TPO_list$prod_const))]/3 
      
      ###Tracking time log for running event list function
      strt_time <- Sys.time()
      
      event_list <- event_list(SP_reactive_input$shiny_ip_events_final,data.table(SP_TPO_list$prod_const))
      end_time <- Sys.time()
      opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
      log_tmp <- data.frame("Function" = "Event List", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time, check.names = FALSE)
      
      opti_log <- rbind(opti_log,log_tmp)
      write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
      
      SP_reactive_input$shiny_ip_events_lsm <- event_list[[1]]
      SP_reactive_input$shiny_ip_events_lsm$Promo_Price_Flag <- 1
      SP_reactive_input$shiny_ip_events <- event_list[[2]]
      SP_reactive_input$shiny_ip_events$Promo_Price_Flag <- 1
      
      ###Product restrictions for LSM used in Optimization Function
      SP_TPO_list$prod_const_lsm <- as.data.frame(SP_TPO_list$prod_const_shiny)
      SP_TPO_list$prod_const_lsm <- as.data.frame(SP_TPO_list$prod_const_shiny[,c("PPG","Brand","Format","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks")])
      names(SP_TPO_list$prod_const_lsm) <- c("PPG","BRAND","FORMAT","Min_Disc_Slots","Max_Disc_Slots","Min_Display_Slots","Max_Display_Slots")
      SP_TPO_list$prod_const_lsm[,grep("Slots",names(SP_TPO_list$prod_const_lsm))] <- SP_TPO_list$prod_const_lsm[,grep("Slots",names(SP_TPO_list$prod_const_lsm))]/3 
      
      ###Product restrictions Unconstrained for best Seq function
      SP_TPO_list$prod_const_promo_seq <- SP_TPO_list$prod_const_shiny[,c("PPG","Brand","Format","Unconstrained Min Total Promo Weeks","Unconstrained Max Total Promo Weeks","Unconstrained Min Display &/or Feature Weeks","Unconstrained Max Display &/or Feature Weeks")]
      names(SP_TPO_list$prod_const_promo_seq) <- c("PPG","BRAND","FORMAT","Min_Total_Weeks","Max_Total_Weeks","Min_Display_Weeks","Max_Display_Weeks")
      
      ###Product restrictions lsm for best Seq function
      SP_TPO_list$prod_const_promo_seq_lsm <- SP_TPO_list$prod_const_shiny[,c("PPG","Brand","Format","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks")]
      names(SP_TPO_list$prod_const_promo_seq_lsm) <- c("PPG","BRAND","FORMAT","Min_Total_Weeks","Max_Total_Weeks","Min_Display_Weeks","Max_Display_Weeks")
      
      ###Investment
      
      SP_TPO_list$prod_budget <- SP_TPO_list$prod_const_shiny[,c("PPG","PPG Description","Brand","Format","Min Investment","Max Investment")]
      names(SP_TPO_list$prod_budget) <- c("PPG","PPG_Description","PRODUCT RANGE","FORMAT","Min_Investment","Max_Investment")
      
      ###All other sales
      SP_TPO_list$other_sales <- SP_reactive_input$opti_ip_cat_sales[["Units"]]
      SP_TPO_list$other_sales_value <- SP_reactive_input$opti_ip_cat_sales_value[["Value"]]
      SP_TPO_list$simulated_flag <- 0
      SP_TPO_list$simulated_flag_lsm <- 0
      
      ###PPGs with no data related to finacials
      SP_reactive_input$SP_RB_Financial_PPG <- unique(SP_reactive_input$SP_RB_Financial[,c("Category","Brand","Format","PPG","PPG_Description")])
      names(SP_reactive_input$SP_RB_Financial_PPG) <- c("Category","Brand","Format","PPG","PPG Description")
      SP_reactive_input$SP_RB_Financial_PPG$Comment <- "RB Financials not available"
      
      SP_reactive_input$SP_RB_Financial_PPG_1 <- rbind(SP_reactive_input$SP_RB_Financial_PPG,cbind(SP_TPO_list$ppg_exclude[,c("Category","Brand","Format","PPG","PPG Description")],data.frame("Comment" = rep("Excluded in optimizer input screen",nrow(SP_TPO_list$ppg_exclude)))))
      
      ###Flag based on competition and cannibalization flags
      if(input$SP_opti_comp == "Competitor Promotion Timing"){
        SP_reactive_input$include_comp <- 1
        SP_reactive_input$include_cannib <- 0
      }else if(input$SP_opti_comp == "Cannibalization- Complementary Impact"){
        SP_reactive_input$include_comp <- 0
        SP_reactive_input$include_cannib <- 1
      }else{
        SP_reactive_input$include_comp <- 0
        SP_reactive_input$include_cannib <- 0
      }
      
      showModal(
        modalDialog(
          title = "Running Optimization",
          "Will take a few minutes based on the data size",
          size = "m",
          footer = "Please wait"
        )
      )
      
      ###Running best_seq_cannib or best_seq based on the input flag SP_reactive_input$include_cannib
      ###Tracking time log for running best seq(Unconstrained)
      #SP_reactive_input$comp_seq <- data.table(read.csv(paste0(SP_reactive_input$folder_path,"/Include_Competition_Promotion.csv"),check.names = FALSE))
      
      strt_time <- Sys.time()
      if(SP_reactive_input$include_cannib == 1){
        SP_reactive_input$SP_opti_best_seq <- data.table(best_seq_cannib(data.table(SP_reactive_input$shiny_opti_data_ip),SP_TPO_list$prod_const_promo_seq,data.table(SP_reactive_input$SP_cannibalization),SP_input_tesco_slot(),ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end)))
      }else if(SP_reactive_input$include_cannib == 0){
        
        SP_reactive_input$SP_opti_best_seq <- data.table(best_seq(SP_reactive_input$shiny_opti_data_ip,SP_TPO_list$prod_const_promo_seq,SP_reactive_input$comp_seq,SP_reactive_input$include_comp,SP_input_tesco_slot(),ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end)))
      }
      end_time <- Sys.time()
      opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
      log_tmp <- data.frame("Function" = "Best Sequence(Unconstrained)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time, check.names = FALSE)
      opti_log <- rbind(opti_log,log_tmp)
      
      
      ###Tracking time log for running best seq(lsm)
      strt_time <- Sys.time()
      if(SP_reactive_input$include_cannib == 1){
        SP_reactive_input$SP_opti_best_seq_lsm <- data.table(best_seq_cannib(data.table(SP_reactive_input$shiny_opti_data_ip),SP_TPO_list$prod_const_promo_seq_lsm,data.table(SP_reactive_input$SP_cannibalization),SP_input_tesco_slot(),ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end)))
      }else if(SP_reactive_input$include_cannib == 0){
        SP_reactive_input$SP_opti_best_seq_lsm <- data.table(best_seq(SP_reactive_input$shiny_opti_data_ip,SP_TPO_list$prod_const_promo_seq_lsm,SP_reactive_input$comp_seq,SP_reactive_input$include_comp,SP_input_tesco_slot(),ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end)))
      }
      end_time <- Sys.time()
      log_tmp <- data.frame("Function" = "Best Sequence(LSM)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time, check.names = FALSE)
      opti_log <- rbind(opti_log,log_tmp)
      write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
      
      SP_TPO_list$opti_const$KPI_Mapping <- as.character(SP_TPO_list$opti_const$KPI_Mapping)
      SP_TPO_list$goal$KPI_Mapping <- as.character(SP_TPO_list$goal$KPI_Mapping)
      
      ###EAN to PPG mapping
      
      if(SP_reactive_input$opti_format_input == "ALL"){
        SP_reactive_input$EAN_PPG_download <- unique(SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Manufacturer == SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected$Brand %in% input$SP_opti_brand,c("PPG","Category","Manufacturer","Brand","Format")])
      }else if(SP_reactive_input$opti_format_input != "ALL" & SP_reactive_input$opti_ppg_input == "ALL"){
        SP_reactive_input$EAN_PPG_download <- unique(SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Manufacturer == SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected$Brand %in% input$SP_opti_brand & SP_reactive_input$SP_opti_const_selected$Format %in% SP_reactive_input$opti_format_input,c("PPG","Category","Manufacturer","Brand","Format")])
      }else if(SP_reactive_input$opti_ppg_input != "ALL"){
        SP_reactive_input$EAN_PPG_download <- unique(SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Manufacturer == SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected$Brand %in% input$SP_opti_brand & SP_reactive_input$SP_opti_const_selected$Format %in% SP_reactive_input$opti_format_input & SP_reactive_input$SP_opti_const_selected$PPG %in% SP_reactive_input$opti_ppg_input,c("PPG","Category","Manufacturer","Brand","Format")])
      }
      
      SP_reactive_input$EAN_PPG_download <- left_join(SP_reactive_input$EAN_PPG_download,SP_reactive_input$EAN_PPG_mapping,by = "PPG")
      
      print(SP_TPO_list$goal)
      print(SP_TPO_list$opti_const)
      print(data.table(SP_TPO_list$prod_budget))
      print(data.table(SP_TPO_list$prod_const))
      print(data.table(SP_TPO_list$prod_const_lsm))
      ###Writing all the optimization inputs into an excel file
      list_of_datasets <- list("Best_Seq_NonLSM" = SP_reactive_input$SP_opti_best_seq,"Best_Seq_LSM" =  SP_reactive_input$SP_opti_best_seq_lsm,
                               "KPI_Constraints" = SP_TPO_list$opti_const, "Budget_Const" = SP_TPO_list$prod_budget, "All_Other_Sales" = SP_TPO_list$other_sales,"All_Other_Sales_Value" = SP_TPO_list$other_sales_value,
                               "Goal" = SP_TPO_list$goal, "Sign" = SP_TPO_list$sign,"Events_List_NonLSM" = SP_reactive_input$shiny_ip_events,"Events_List_LSM" = SP_reactive_input$shiny_ip_events_lsm,
                               "Product_Const_NonLSM" = SP_TPO_list$prod_const,"Product_Const_LSM" =  SP_TPO_list$prod_const_lsm, "Exclude_PPG_List" = SP_TPO_list$ppg_exclude$PPG, "Last_Year_KPI" = SP_reactive_input$SP_opti_const)
      write.xlsx(list_of_datasets, file = paste0(SP_reactive_input$folder_path,"/","Optimization_Inputs.xlsx"))
      
      ###ROI input from optimization input screen
      if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
        SP_reactive_input$opti_ROI_input <- "R_ROI_GM"
      }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
        SP_reactive_input$opti_ROI_input <- "R_ROI_Rev"
      }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
        SP_reactive_input$opti_ROI_input <- "R_ROI_NIS"
      }
      
      # best_seq <- SP_reactive_input$SP_opti_best_seq
      # opti_const <- SP_TPO_list$opti_const
      # prod_budget <- data.table(SP_TPO_list$prod_budget)
      # other_sales <- SP_TPO_list$other_sales
      # goal <- SP_TPO_list$goal
      # sign <- SP_TPO_list$sign
      # shiny_ip_events <- data.table(SP_reactive_input$shiny_ip_events)
      # prod_const <- data.table(SP_TPO_list$prod_const)
      # exclude_ppg <- SP_TPO_list$ppg_exclude$PPG
      # opti_ly_kpi <- SP_reactive_input$SP_opti_const
      # format <- SP_reactive_input$opti_format_input
      # roi <- SP_reactive_input$opti_ROI_input
      # best_seq_lsm <- SP_reactive_input$SP_opti_best_seq_lsm
      # prod_const_lsm <- data.table(SP_TPO_list$prod_const_lsm)
      # shiny_ip_events_lsm <- data.table(SP_reactive_input$shiny_ip_events_lsm)
      
      ###Optimization function - Running complete optimization
      if(input$SP_opti_run_choice == "Run Complete Optimization"){
        withProgress(message = 'Running LSM Optimization.. (this may take a while)', value = 0, {
          strt_time <- Sys.time()
          cat("\n=== Constraints passed to optimization (LSM) ===\n")
          print(SP_TPO_list$opti_const[, c("Include the Constraint","Constraint Order","KPI","KPI_Mapping","Scale","Last Year Value","Minimum Value","Maximum Value")])
          cat("=============================================\n\n")
          SP_reactive_input$SP_opti_output_lsm <- tryCatch({
            optimization(SP_reactive_input$SP_opti_best_seq_lsm,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events_lsm),data.table(SP_TPO_list$prod_const_lsm),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          }, warning = function(w) {
            showNotification(paste("Optimization completed with warnings:", w$message), type = "warning", duration = 10)
            # Return the result anyway (warnings don't stop execution)
            optimization(SP_reactive_input$SP_opti_best_seq_lsm,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events_lsm),data.table(SP_TPO_list$prod_const_lsm),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          }, error = function(e) {
            showNotification(paste("Optimization failed:", e$message), type = "error", duration = 15)
            stop(e)
          })
          end_time <- Sys.time()
          opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
          opti_log$Function <- as.character(opti_log$Function)
          
          #opti_log[opti_log$Function == "Optimization_Iteration",]$Function = "Optimization(LSM)"
          log_tmp <- data.frame("Function" = "Optimization(LSM)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time,check.names = FALSE)
          opti_log <- rbind(opti_log,log_tmp)
          write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
          
          # if(!file.exists(paste0(SP_reactive_input$folder_path,"/","lsm_op.RData"))){
          #   SP_reactive_input$SP_opti_output_lsm <- optimization(SP_reactive_input$SP_opti_best_seq_lsm,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events_lsm),data.table(SP_TPO_list$prod_const_lsm),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          #   SP_opti_output_lsm <- SP_reactive_input$SP_opti_output_lsm
          #   save(SP_opti_output_lsm,file = paste0(SP_reactive_input$folder_path,"/","lsm_op.RData"))
          # }else{
          #   load(paste0(SP_reactive_input$folder_path,"/","lsm_op.RData"))
          #   SP_reactive_input$SP_opti_output_lsm <- SP_opti_output_lsm
          # }
          
        })
        
        withProgress(message = 'Running Unconstrained Optimization.. (this may take a while)', value = 0, {
          
          ###Tracking time log for running Optimization(Unconstrained)
          
          strt_time <- Sys.time()
          cat("\n=== Constraints passed to optimization (Unconstrained) ===\n")
          print(SP_TPO_list$opti_const[, c("Include the Constraint","Constraint Order","KPI","KPI_Mapping","Scale","Last Year Value","Minimum Value","Maximum Value")])
          cat("========================================================\n\n")
          SP_reactive_input$SP_opti_output <- tryCatch({
            optimization(SP_reactive_input$SP_opti_best_seq,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events),data.table(SP_TPO_list$prod_const),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          }, warning = function(w) {
            showNotification(paste("Optimization completed with warnings:", w$message), type = "warning", duration = 10)
            suppressWarnings(optimization(SP_reactive_input$SP_opti_best_seq,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events),data.table(SP_TPO_list$prod_const),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE))
          }, error = function(e) {
            showNotification(paste("Optimization failed:", e$message), type = "error", duration = 15)
            stop(e)
          })
          end_time <- Sys.time()
          opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
          #
          #opti_log[opti_log$Function == "Optimization_Iteration",]$Function = "Optimization(Unconstrained)"
          log_tmp <- data.frame("Function" = "Optimization(Unconstrained)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time, check.names = FALSE)
          opti_log <- rbind(opti_log,log_tmp)
          write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
          
          # if(!file.exists(paste0(SP_reactive_input$folder_path,"/","nonlsm_op.RData"))){
          #   SP_reactive_input$SP_opti_output <- optimization(SP_reactive_input$SP_opti_best_seq,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events),data.table(SP_TPO_list$prod_const),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          #   SP_opti_output <- SP_reactive_input$SP_opti_output
          #   save(SP_opti_output,file = paste0(SP_reactive_input$folder_path,"/","nonlsm_op.RData"))
          # }else{
          #   load(paste0(SP_reactive_input$folder_path,"/","nonlsm_op.RData"))
          #   SP_reactive_input$SP_opti_output <- SP_opti_output
          # }
        })
        ###Optimization output - Runnning optimizer_op_prep to prepare the output
        strt_time <- Sys.time()
        SP_reactive_input$SP_opti_op_prepared <- optimizer_op_prep(data.table(SP_reactive_input$SP_opti_output[[1]]),data.table(SP_reactive_input$shiny_ip_tesco_cal),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end))
        SP_reactive_input$opti_op_ppg_budget_const <- SP_reactive_input$SP_opti_output[[4]]
        ###New Codes - Taking care of display slot constraints and promo price constraints
        if(input$SP_opti_disp_pp_const == TRUE){
          SP_reactive_input$SP_opti_op_prepared <- display_promo_const(SP_reactive_input$SP_opti_op_prepared,data.table(SP_reactive_input$shiny_ip_events),SP_reactive_input$opti_ROI_input)
          SP_reactive_input$SP_opti_op_prepared <- display_slot_const(SP_reactive_input$SP_opti_op_prepared,input$SP_opti_max_display,SP_reactive_input$opti_ROI_input)
        }
        
        end_time <- Sys.time()
        opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
        log_tmp <- data.frame("Function" = "Optimization Output Prep(Unconstrained)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time,check.names = FALSE)
        opti_log <- rbind(opti_log,log_tmp)
        #write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
        
        ###Optimization output - Runnning optimizer_op_prep to prepare the output
        strt_time <- Sys.time()
        SP_reactive_input$SP_opti_op_prepared_lsm <- optimizer_op_prep(data.table(SP_reactive_input$SP_opti_output_lsm[[1]]),data.table(SP_reactive_input$shiny_ip_tesco_cal),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end))
        SP_reactive_input$opti_op_ppg_budget_const_lsm <- SP_reactive_input$SP_opti_output_lsm[[4]]
        ###New Codes - Taking care of display slot constraints and promo price constraints
        if(input$SP_opti_disp_pp_const == TRUE){
          SP_reactive_input$SP_opti_op_prepared_lsm <- display_promo_const(SP_reactive_input$SP_opti_op_prepared_lsm,data.table(SP_reactive_input$shiny_ip_events_lsm),SP_reactive_input$opti_ROI_input)
          SP_reactive_input$SP_opti_op_prepared_lsm <- display_slot_const(SP_reactive_input$SP_opti_op_prepared_lsm,input$SP_opti_max_display,SP_reactive_input$opti_ROI_input)
        }
        end_time <- Sys.time()
        #opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
        log_tmp <- data.frame("Function" = "Optimization Output Prep(LSM)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time,check.names = FALSE)
        opti_log <- rbind(opti_log,log_tmp)
        write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
        
        
        setnames(SP_reactive_input$SP_opti_op_prepared,c("SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT"),c("Category","Manufacturer","Brand","Format"))
        SP_reactive_input$SP_opti_exc_brand <- SP_reactive_input$SP_opti_output[[2]]
        
        setnames(SP_reactive_input$SP_opti_op_prepared_lsm,c("SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT"),c("Category","Manufacturer","Brand","Format"))
        SP_reactive_input$SP_opti_exc_brand_lsm <- SP_reactive_input$SP_opti_output_lsm[[2]]
        SP_reactive_input$SP_TPO_list <- SP_TPO_list
        
        SP_reactive_input$optimizer_op_download <- list("LSM Output" = SP_reactive_input$SP_opti_op_prepared_lsm, "Unconstrained Output" = SP_reactive_input$SP_opti_op_prepared,
                                                        "Excluded PPG" = SP_reactive_input$SP_opti_exc_brand, "EAN PPG Mapping" = SP_reactive_input$EAN_PPG_download)
        write.xlsx(SP_reactive_input$optimizer_op_download, file = paste0(SP_reactive_input$folder_path,"/","Optimization Output.xlsx"))
      }
      ###Optimization function - Running only uncostrained optimization
      else if(input$SP_opti_run_choice == "Run Unconstrained Optimization"){
        withProgress(message = 'Running Unconstrained Optimization.. (this may take a while)', value = 0, {
          
          ###Tracking time log for running Optimization(Unconstrained)
          
          strt_time <- Sys.time()
          SP_reactive_input$SP_opti_output <- tryCatch({
            optimization(SP_reactive_input$SP_opti_best_seq,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events),data.table(SP_TPO_list$prod_const),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          }, warning = function(w) {
            showNotification(paste("Optimization completed with warnings:", w$message), type = "warning", duration = 10)
            # Suppress warnings and return result
            suppressWarnings(optimization(SP_reactive_input$SP_opti_best_seq,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events),data.table(SP_TPO_list$prod_const),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE))
          }, error = function(e) {
            showNotification(paste("Optimization failed:", e$message), type = "error", duration = 15)
            stop(e)
          })
          end_time <- Sys.time()
          opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
          #
          #opti_log[opti_log$Function == "Optimization_Iteration",]$Function = "Optimization(Unconstrained)"
          log_tmp <- data.frame("Function" = "Optimization(Unconstrained)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time, check.names = FALSE)
          opti_log <- rbind(opti_log,log_tmp)
          write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
          
          # if(!file.exists("nonlsm_op.RData")){
          #   SP_reactive_input$SP_opti_output <- optimization(SP_reactive_input$SP_opti_best_seq,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events),data.table(SP_TPO_list$prod_const),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          #   SP_opti_output <- SP_reactive_input$SP_opti_output
          #   save(SP_opti_output,file =  "nonlsm_op.RData")
          # }else{
          #   load("nonlsm_op.RData")
          #   SP_reactive_input$SP_opti_output <- SP_opti_output
          # }
          
        })
        ###Optimization output - Runnning optimizer_op_prep to prepare the output
        strt_time <- Sys.time()
        SP_reactive_input$SP_opti_op_prepared <- optimizer_op_prep(data.table(SP_reactive_input$SP_opti_output[[1]]),data.table(SP_reactive_input$shiny_ip_tesco_cal),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end))
        SP_reactive_input$opti_op_ppg_budget_const <- SP_reactive_input$SP_opti_output[[4]]
        ###New Codes - Taking care of display slot constraints and promo price constraints
        if(input$SP_opti_disp_pp_const == TRUE){
          SP_reactive_input$SP_opti_op_prepared <- display_promo_const(SP_reactive_input$SP_opti_op_prepared,data.table(SP_reactive_input$shiny_ip_events),SP_reactive_input$opti_ROI_input)
          SP_reactive_input$SP_opti_op_prepared <- display_slot_const(SP_reactive_input$SP_opti_op_prepared,input$SP_opti_max_display,SP_reactive_input$opti_ROI_input)
        }
        end_time <- Sys.time()
        opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
        log_tmp <- data.frame("Function" = "Optimization Output Prep(Unconstrained)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time,check.names = FALSE)
        opti_log <- rbind(opti_log,log_tmp)
        write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
        
        setnames(SP_reactive_input$SP_opti_op_prepared,c("SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT"),c("Category","Manufacturer","Brand","Format"))
        SP_reactive_input$SP_opti_exc_brand <- SP_reactive_input$SP_opti_output[[2]]
        
        SP_reactive_input$optimizer_op_download <- list("LSM Output" = "", "Unconstrained Output" = SP_reactive_input$SP_opti_op_prepared,
                                                        "Excluded PPG" = SP_reactive_input$SP_opti_exc_brand, "EAN PPG Mapping" = SP_reactive_input$EAN_PPG_download)
        write.xlsx(SP_reactive_input$optimizer_op_download, file = paste0(SP_reactive_input$folder_path,"/","Optimization Output.xlsx"))
        
        #Making LSM output as NULL
        SP_reactive_input$SP_opti_op_prepared_lsm <- NULL
      }
      ###Optimization function - Running only LSM optimization
      else if(input$SP_opti_run_choice == "Run LSM constrained Optimization"){
        withProgress(message = 'Running LSM constrained Optimization.. (this may take a while)', value = 0, {
          strt_time <- Sys.time()
          
          SP_reactive_input$SP_opti_output_lsm <- tryCatch({
            optimization(SP_reactive_input$SP_opti_best_seq_lsm,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events_lsm),data.table(SP_TPO_list$prod_const_lsm),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          }, warning = function(w) {
            showNotification(paste("Optimization completed with warnings:", w$message), type = "warning", duration = 10)
            suppressWarnings(optimization(SP_reactive_input$SP_opti_best_seq_lsm,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events_lsm),data.table(SP_TPO_list$prod_const_lsm),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE))
          }, error = function(e) {
            showNotification(paste("Optimization failed:", e$message), type = "error", duration = 15)
            stop(e)
          })
          end_time <- Sys.time()
          opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
          opti_log$Function <- as.character(opti_log$Function)
          
          #opti_log[opti_log$Function == "Optimization_Iteration",]$Function = "Optimization(LSM)"
          log_tmp <- data.frame("Function" = "Optimization(LSM)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time,check.names = FALSE)
          opti_log <- rbind(opti_log,log_tmp)
          write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
          
          # if(!file.exists("lsm_op.RData")){
          #   SP_reactive_input$SP_opti_output_lsm <- optimization(SP_reactive_input$SP_opti_best_seq_lsm,SP_TPO_list$opti_const,data.table(SP_TPO_list$prod_budget),SP_TPO_list$other_sales,SP_TPO_list$goal,SP_TPO_list$sign,data.table(SP_reactive_input$shiny_ip_events_lsm),data.table(SP_TPO_list$prod_const_lsm),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$SP_opti_const,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ROI_input,SP_TPO_list$other_sales_value,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end),progress = TRUE)
          #   SP_opti_output_lsm <- SP_reactive_input$SP_opti_output_lsm
          #   save(SP_opti_output_lsm,file = "lsm_op.RData")
          # }else{
          #   load("lsm_op.RData")
          #   SP_reactive_input$SP_opti_output_lsm <- SP_opti_output_lsm
          # }
          
        })
        ###Optimization output - Runnning optimizer_op_prep to prepare the output
        strt_time <- Sys.time()
        SP_reactive_input$SP_opti_op_prepared_lsm <- optimizer_op_prep(data.table(SP_reactive_input$SP_opti_output_lsm[[1]]),data.table(SP_reactive_input$shiny_ip_tesco_cal),SP_TPO_list$ppg_exclude$PPG,SP_reactive_input$opti_format_input,SP_reactive_input$opti_ppg_input,ymd(input$SP_opti_date_start) ,ymd(input$SP_opti_date_end))
        SP_reactive_input$opti_op_ppg_budget_const_lsm <- SP_reactive_input$SP_opti_output_lsm[[4]]
        ###New Codes - Taking care of display slot constraints and promo price constraints
        if(input$SP_opti_disp_pp_const == TRUE){
          SP_reactive_input$SP_opti_op_prepared_lsm <- display_promo_const(SP_reactive_input$SP_opti_op_prepared_lsm,data.table(SP_reactive_input$shiny_ip_events_lsm),SP_reactive_input$opti_ROI_input)
          SP_reactive_input$SP_opti_op_prepared_lsm <- display_slot_const(SP_reactive_input$SP_opti_op_prepared_lsm,input$SP_opti_max_display,SP_reactive_input$opti_ROI_input)
        }
        end_time <- Sys.time()
        opti_log <- read.csv(paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"),check.names = FALSE)
        log_tmp <- data.frame("Function" = "Optimization Output Prep(LSM)", "Iteration" = "Complete", "Start Time" = as.character(strt_time), "End Time" = as.character(end_time), "Time Taken" = end_time - strt_time,check.names = FALSE)
        opti_log <- rbind(opti_log,log_tmp)
        write.csv(opti_log, paste0(SP_reactive_input$folder_path,"/","optimization_log.csv"), row.names = FALSE)
        
        setnames(SP_reactive_input$SP_opti_op_prepared_lsm,c("SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT"),c("Category","Manufacturer","Brand","Format"))
        SP_reactive_input$SP_opti_exc_brand_lsm <- SP_reactive_input$SP_opti_output_lsm[[2]]
        
        SP_reactive_input$optimizer_op_download <- list("LSM Output" = SP_reactive_input$SP_opti_op_prepared_lsm, "Unconstrained Output" = "",
                                                        "Excluded PPG" = SP_reactive_input$SP_opti_exc_brand, "EAN PPG Mapping" = SP_reactive_input$EAN_PPG_download)
        write.xlsx(SP_reactive_input$optimizer_op_download, file = paste0(SP_reactive_input$folder_path,"/","Optimization Output.xlsx"))
        
        #Making Nonlsm output as NULL
        SP_reactive_input$SP_opti_op_prepared <- NULL
      }
      
      ###"Iterations" tab in Optimization output screen for Unconstrained optimization
      SP_reactive_input$kpi_iteration_nonlsm <- SP_reactive_input$SP_opti_output[[3]]
      
      SP_selected_const <- SP_TPO_list$opti_const[SP_TPO_list$opti_const$`Include the Constraint` == TRUE,]
      SP_selected_const$KPI <- as.character(SP_selected_const$KPI)
      SP_reactive_input$kpi_iteration_nonlsm_1 <- SP_reactive_input$kpi_iteration_nonlsm
      if(!(is.null(SP_reactive_input$kpi_iteration_nonlsm_1))){
        if(nrow(SP_reactive_input$kpi_iteration_nonlsm_1) > 0){
          names(SP_reactive_input$kpi_iteration_nonlsm_1) <- c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS",input$SP_opti_ROI_selection,"Scan Gross Sales","Gross Margin","Volume Sales","Value Market Share","Iteration")
          
          
          if(nrow(SP_selected_const) != 0){
            for(i in c(1:nrow(SP_selected_const))){
              SP_reactive_input$kpi_iteration_nonlsm_1 <- SP_reactive_input$kpi_iteration_nonlsm_1[SP_reactive_input$kpi_iteration_nonlsm_1[[SP_selected_const[,"KPI"][i]]] >= SP_selected_const[,"Minimum Value"][i] &
                                                                                                     SP_reactive_input$kpi_iteration_nonlsm_1[[SP_selected_const[,"KPI"][i]]] <= SP_selected_const[,"Maximum Value"][i],]
            }
          }
          
          SP_reactive_input$kpi_iteration_nonlsm_1 <- SP_reactive_input$kpi_iteration_nonlsm_1[order(SP_reactive_input$kpi_iteration_nonlsm_1$Iteration),]
          SP_reactive_input$opti_const_met_nonlsm <- SP_reactive_input$kpi_iteration_nonlsm_1[,"Iteration"][1]
          output$SP_KPI_Iteration_nonLSM <- renderPlotly({
            vline <- function(x = 0, color = "#0099DC") {
              list(
                type = "line",
                y0 = 0,
                y1 = 1,
                yref = "paper",
                x0 = x,
                x1 = x,
                line = list(color = color)
              )
            }
            #shapes = vline(SP_reactive_input$opti_const_met_nonlsm),
            plot_ly(SP_reactive_input$kpi_iteration_nonlsm, x = ~Iteration,y = ~`Scan Net Revenue`, name = 'Net Revenue',mode = 'lines',type = "scatter") %>%
              add_trace(y = ~`GM % NR`, name = 'GM % NR', mode = 'lines',yaxis = 'y2') %>%
              add_trace(y = ~`TI % NR`, name = 'TI % NR', mode = 'lines',yaxis = 'y2') %>%
              add_trace(y = ~`TI % NIS`, name = 'TI % NIS', mode = 'lines',yaxis = 'y2') %>%
              add_trace(y = ~`Scan Gross Sales`, name = 'Scan Gross Sales',mode = 'lines') %>%
              add_trace(y = ~`Gross Margin`, name = 'Gross Margin',mode = 'lines') %>%
              add_trace(y = ~input$SP_opti_ROI_selection, name = input$SP_opti_ROI_selection, mode = 'lines+markers',yaxis = 'y2') %>%
              add_trace(y = ~`Volume Sales`, name = 'Volume Sales', mode = 'lines') %>%
              add_trace(y = ~`Value Market Share`, name = 'Value Market Share', mode = 'lines',yaxis = 'y2') %>%
              layout(title = "Unconstrained Optimization",xaxis = list(title = 'Iteration'),yaxis = list(side = 'left',title = "Absolute"),yaxis2 = list(side = 'right', overlaying = "y",title = "Percentage"))
          })
        }
      }
      
      SP_reactive_input$SP_TPO_list <- SP_TPO_list
      SP_reactive_input$simulated_flag <- 0
      SP_reactive_input$simulated_flag_lsm <- 0
      
      ###"Iterations" tab in Optimization output screen for LSM optimization
      SP_reactive_input$kpi_iteration_lsm <- SP_reactive_input$SP_opti_output_lsm[[3]]
      SP_reactive_input$kpi_iteration_lsm_1 <- SP_reactive_input$kpi_iteration_lsm
      if(!(is.null(SP_reactive_input$kpi_iteration_lsm_1))){
        if(nrow(SP_reactive_input$kpi_iteration_lsm_1) > 0){
          names(SP_reactive_input$kpi_iteration_lsm_1) <- c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS",input$SP_opti_ROI_selection,"Scan Gross Sales","Gross Margin","Volume Sales","Value Market Share","Iteration")
          if(nrow(SP_selected_const) != 0){
            for(i in c(1:nrow(SP_selected_const))){
              SP_reactive_input$kpi_iteration_lsm_1 <- SP_reactive_input$kpi_iteration_lsm_1[SP_reactive_input$kpi_iteration_lsm_1[[SP_selected_const[,"KPI"][i]]] >= SP_selected_const[,"Minimum Value"][i] &
                                                                                               SP_reactive_input$kpi_iteration_lsm_1[[SP_selected_const[,"KPI"][i]]] <= SP_selected_const[,"Maximum Value"][i],]
            }
          }
          
          SP_reactive_input$kpi_iteration_lsm_1 <- SP_reactive_input$kpi_iteration_lsm_1[order(SP_reactive_input$kpi_iteration_lsm_1$Iteration),]
          SP_reactive_input$opti_const_met_lsm <- SP_reactive_input$kpi_iteration_lsm_1[,"Iteration"][1]
          
          output$SP_KPI_Iteration_LSM <- renderPlotly({
            vline <- function(x = 0, color = "#0099DC") {
              list(
                type = "line",
                y0 = 0,
                y1 = 1,
                yref = "paper",
                x0 = x,
                x1 = x,
                line = list(color = color)
              )
            }
            #shapes = vline(SP_reactive_input$opti_const_met_lsm),
            plot_ly(SP_reactive_input$kpi_iteration_lsm, x = ~Iteration,y = ~`Scan Net Revenue`, name = 'Net Revenue',mode = 'lines',type = "scatter") %>%
              add_trace(y = ~`GM % NR`, name = 'GM % NR', mode = 'lines',yaxis = 'y2') %>%
              add_trace(y = ~`TI % NR`, name = 'TI % NR', mode = 'lines',yaxis = 'y2') %>%
              add_trace(y = ~`TI % NIS`, name = 'TI % NIS', mode = 'lines',yaxis = 'y2') %>%
              add_trace(y = ~`Scan Gross Sales`, name = 'Scan Gross Sales',mode = 'lines') %>%
              add_trace(y = ~`Gross Margin`, name = 'Gross Margin',mode = 'lines') %>%
              add_trace(y = ~input$SP_opti_ROI_selection, name = input$SP_opti_ROI_selection, mode = 'lines+markers',yaxis = 'y2') %>%
              add_trace(y = ~`Volume Sales`, name = 'Volume Sales', mode = 'lines') %>%
              add_trace(y = ~`Value Market Share`, name = 'Value Market Share', mode = 'lines',yaxis = 'y2') %>%
              layout(title = "LSM Optimization",xaxis = list(title = 'Iteration'),yaxis = list(side = 'left',title = "Absolute"),yaxis2 = list(side = 'right', overlaying = "y",title = "Percentage"))
          })
        }
      }
      
      ###Mapping for KPI names for top tables in optimizer output screen
      SP_reactive_input$kpi_map_calc <- data.frame("KPI_opti" = c("Scan Net Revenue(MM GBP)","GM % NR(%)","TI % NR(%)","TI % NIS(%)","Scan Gross Sales(MM GBP)","Gross Margin(MM GBP)","Volume Sales(MM Units)",input$SP_opti_ROI_selection,"Value Market Share(%)"), "KPI_calc" = c("net_sales","gm_%","spend_%","spend_%NIS","gross_sales","gm","vol_sales","ROI","market_share"),"const_order" = c(3,5,6,7,1,4,2,8,9),
                                                   "KPI_model" = c("Net_Sales_model","GM_percent_model","Trade_as_per_NR_model","Trade_as_per_NIS_model","Gross_sales_model","Gross_margin_model","Volume_sales_model","ROI_model","Market_Share_model"),check.names = FALSE)
      SP_reactive_input$kpi_map_calc_ret <- data.frame("KPI_opti" = c("RSV(MM GBP)","COGS(MM GBP)","CPD(MM GBP)","Fixed(MM GBP)","FM(MM GBP)","BM(MM GBP)","FM %","BM %"), "KPI_calc" = c("RSV","COGS","CPD","fixed","FM","BM","FM%","BM%"),"const_order" = c(1:8),check.names = FALSE)
      
      ###Calculating RB KPI's in optimizer output screen
      SP_reactive_input$KPI_calculation <- KPI_calc(SP_reactive_input$SP_TPO_list,SP_reactive_input$SP_opti_op_prepared_lsm,SP_reactive_input$SP_opti_op_prepared,SP_reactive_input$SP_opti_exc_brand_lsm,SP_reactive_input$SP_opti_exc_brand,SP_reactive_input$SP_opti_const_grouped,SP_reactive_input$SP_RB_Delist,input$SP_opti_op_include_delist,input$SP_opti_op_include_exclude,input$SP_opti_ROI_selection)
      
      ###Calculating Retailer KPI's in optimizer output screen
      if(is.null(SP_reactive_input$SP_opti_op_prepared_lsm)){
        lsm_kpi_calc_input <- data.frame()
      }else{
        lsm_kpi_calc_input <- KPI_calc_input(SP_reactive_input$SP_opti_op_prepared_lsm,input$SP_opti_ROI_selection)
      }
      
      if(is.null(SP_reactive_input$SP_opti_op_prepared)){
        nonlsm_kpi_calc_input <- data.frame()
      }else{
        nonlsm_kpi_calc_input <- KPI_calc_input(SP_reactive_input$SP_opti_op_prepared,input$SP_opti_ROI_selection)
      }
      
      SP_reactive_input$KPI_calculation_ret <- KPI_calc_ret(SP_reactive_input$SP_TPO_list,lsm_kpi_calc_input,nonlsm_kpi_calc_input,SP_reactive_input$SP_opti_exc_brand_lsm,SP_reactive_input$SP_opti_exc_brand,SP_reactive_input$SP_opti_const_grouped,SP_reactive_input$SP_RB_Delist,input$SP_opti_op_include_delist,input$SP_opti_op_include_exclude,input$SP_opti_ROI_selection)
      
      ######Populating Unconstrained calendar on optimization output screen
      if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
        
        SP_reactive_input$opti_cal_filtered <- SP_reactive_input$SP_opti_op_prepared
        
        ###Calculating ROI based on roi selection on otimization input screen
        if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
          SP_reactive_input$opti_cal_filtered[,ROI := sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
        }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
          SP_reactive_input$opti_cal_filtered[,ROI := sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
        }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
          SP_reactive_input$opti_cal_filtered[,ROI := sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
        }
         
        SP_reactive_input$opti_cal_filtered$Date <- ymd(SP_reactive_input$opti_cal_filtered$Date)
        # Changing week end to Date and changing week no to slot no
        week_no <- data.frame("Date" = sort(unique(SP_reactive_input$opti_cal_filtered$Date)),"Week No" = c(1:length(sort(unique(SP_reactive_input$opti_cal_filtered$Date)))),check.names = FALSE)
        SP_reactive_input$opti_cal_filtered <- left_join(SP_reactive_input$opti_cal_filtered,week_no,by = "Date")
        
        if(!(is.null(SP_reactive_input$opti_cal_filtered))){
          SP_reactive_input$opti_cal_filtered$Tesco_Event <- ifelse(SP_reactive_input$opti_cal_filtered$Display_Flag == 1,"Display",ifelse(SP_reactive_input$opti_cal_filtered$TPR_Flag == 1,"TPR","No Promo"))
          #SP_reactive_input$opti_cal_filtered$Promo_Price <- round(SP_reactive_input$opti_cal_filtered$Promo_Price,2)
          SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event == "No Promo",]$Promo_Price <- NA_real_
          SP_reactive_input$opti_cal_filtered <- data.table(SP_reactive_input$opti_cal_filtered)
          
          ###Preparing data for ROI Effectiveness calendar
          SP_reactive_input$opti_cal_filtered$ROI_cal <- SP_reactive_input$opti_cal_filtered$ROI
          SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event == "No Promo",]$ROI_cal <- -100
          SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event == "TPR",]$Tesco_Event <- "Shelf Promotion"
          SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event == "Display",]$Tesco_Event <- "Display Feature Promotion"
          SP_reactive_input$brks_ROI_nonLSM <- quantile(SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event != "No Promo" & SP_reactive_input$opti_cal_filtered$ROI_cal >= 0,]$ROI_cal, seq(0, 1, .05), na.rm = TRUE)
          SP_reactive_input$opti_cal_filtered <- data.table(left_join(SP_reactive_input$opti_cal_filtered,SP_reactive_input$SP_TPO_list$prod_const_shiny[,c("PPG","Brand","Format","LSM Min Promo Price")],by = c("PPG","Brand","Format")))
          
          setnames(SP_reactive_input$opti_cal_filtered,"LSM Min Promo Price","LSM Promo Price")
          SP_reactive_input$opti_cal_dcast <- data.table::dcast(unique(SP_reactive_input$opti_cal_filtered[,c("Format","PPG","PPG_Description","Week No","Promo_Price","RSP_Unit","LSM Promo Price","Tesco_Event","ROI_cal")]),Format + PPG + PPG_Description + RSP_Unit + `LSM Promo Price`~`Week No`,value.var= c("Promo_Price","Tesco_Event","ROI_cal"))
          SP_reactive_input$opti_cal_dcast_ROI <- SP_reactive_input$opti_cal_dcast
          
          names(SP_reactive_input$opti_cal_dcast) <- gsub("Promo_Price_","",names(SP_reactive_input$opti_cal_dcast))
          names(SP_reactive_input$opti_cal_dcast) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast))
          
          names(SP_reactive_input$opti_cal_dcast_ROI) <- gsub("ROI_cal_","",names(SP_reactive_input$opti_cal_dcast_ROI))
          names(SP_reactive_input$opti_cal_dcast_ROI) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_ROI))
          
          setcolorder(SP_reactive_input$opti_cal_dcast,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`))),paste0("ROI_cal","_",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`)))))
          setcolorder(SP_reactive_input$opti_cal_dcast_ROI,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`))),paste0("Promo_Price","_",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`)))))
          
        }
        
        ###Showing all tables and charts related to Unconstrained optimization
        shinyjs::show("SP_opti_cal_legend_nonLSM")
        shinyjs::show("SP_opti_cal_nonLSM_format_selection")
        shinyjs::show("SP_opti_save_nonLSM")
        shinyjs::show("SP_opti_save_name_nonLSM")
        shinyjs::show("SP_opti_cal_nonLSM")
        shinyjs::show("SP_const1_KPI_nonlsm")
        shinyjs::show("SP_const1_KPI_ret_nonlsm")
        shinyjs::show("SP_const1_KPI_cat_ret_nonlsm")
        shinyjs::show("SP_graph_4_ly")
        shinyjs::show("SP_graph_2")
        shinyjs::show("SP_graph_1")
        shinyjs::show("SP_graph_roi_nonlsm_op")
        shinyjs::show("SP_graph_roi_nonlsm_ly")
        shinyjs::show("SP_KPI_Iteration_nonLSM")
        shinyjs::show("SP_graph_4_nonlsm")
        
        output$SP_opti_cal_legend_nonLSM <- renderDataTable({
          datatable(transpose(data.frame(c("Shelf Promotion","Display Feature Promotion"))),class='display',
                    rownames = NULL, colnames = c(rep("",2)), 
                    options = list(dom = 'b',ordering=FALSE)) %>%
            formatStyle(columns=c(0:2),
                        background = styleEqual(c("Shelf Promotion","Display Feature Promotion"), 
                                                c('#0099DC', '#E42E92')),
                        color = styleEqual(c("Shelf Promotion","Display Feature Promotion"), 
                                           c('white', 'white'))) %>%
            formatStyle(1,border = styleInterval(0, c('auto', '1px solid white'))) %>%
            formatStyle(0,target = 'row',lineHeight='70%')
        })
      }
      ###Hiding all tables and charts related to Unconstrained optimization if only LSM is run
      else{
        shinyjs::hide("SP_opti_cal_legend_nonLSM")
        shinyjs::hide("SP_opti_cal_nonLSM_format_selection")
        shinyjs::hide("SP_opti_save_nonLSM")
        shinyjs::hide("SP_opti_save_name_nonLSM")
        shinyjs::hide("SP_opti_cal_nonLSM")
        shinyjs::hide("SP_const1_KPI_nonlsm")
        shinyjs::hide("SP_const1_KPI_ret_nonlsm")
        shinyjs::hide("SP_const1_KPI_cat_ret_nonlsm")
        shinyjs::hide("SP_graph_4_ly")
        shinyjs::hide("SP_graph_4_nonlsm")
        shinyjs::hide("SP_graph_2")
        shinyjs::hide("SP_graph_1")
        shinyjs::hide("SP_graph_roi_nonlsm_op")
        shinyjs::hide("SP_graph_roi_nonlsm_ly")
        shinyjs::hide("SP_KPI_Iteration_nonLSM")
      }
      
      ##LSM Calendar on optimization output screen
      if(input$SP_opti_run_choice %in% c("Run LSM constrained Optimization","Run Complete Optimization")){
        SP_reactive_input$opti_cal_filtered_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm
        
        ###Calculating ROI based on roi selection on optimization input screen
        if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
          SP_reactive_input$opti_cal_filtered_lsm[,ROI := sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
        }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
          SP_reactive_input$opti_cal_filtered_lsm[,ROI := sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
        }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
          SP_reactive_input$opti_cal_filtered_lsm[,ROI := sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
        }
        browser()
        SP_reactive_input$opti_cal_filtered_lsm$Date <- ymd(SP_reactive_input$opti_cal_filtered_lsm$Week_Ending)
        week_no <- data.frame("Date" = sort(unique(SP_reactive_input$opti_cal_filtered_lsm$Date)),"Week No" = c(1:length(sort(unique(SP_reactive_input$opti_cal_filtered_lsm$Week_Ending)))),check.names = FALSE)
        SP_reactive_input$opti_cal_filtered_lsm <- left_join(SP_reactive_input$opti_cal_filtered_lsm,week_no,by = "Date")
        
        if(!(is.null(SP_reactive_input$opti_cal_filtered_lsm))){
          SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event <- ifelse(SP_reactive_input$opti_cal_filtered_lsm$Display_Flag == 1,"Display",ifelse(SP_reactive_input$opti_cal_filtered_lsm$TPR_Flag == 1,"TPR","No Promo"))
          SP_reactive_input$opti_cal_filtered_lsm$Promo_Price <- round(SP_reactive_input$opti_cal_filtered_lsm$Promo_Price,2)
          SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event == "No Promo",]$Promo_Price <- NA_real_
          SP_reactive_input$opti_cal_filtered_lsm <- data.table(SP_reactive_input$opti_cal_filtered_lsm)
          
          ###Preparing data for ROI Effectiveness calendar
          SP_reactive_input$opti_cal_filtered_lsm$ROI_cal <- SP_reactive_input$opti_cal_filtered_lsm$ROI
          SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event == "No Promo",]$ROI_cal <- -100
          SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event == "TPR",]$Tesco_Event <- "Shelf Promotion"
          SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event == "Display",]$Tesco_Event <- "Display Feature Promotion"
          
          SP_reactive_input$brks_ROI_LSM <- quantile(SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event != "No Promo" & SP_reactive_input$opti_cal_filtered_lsm$ROI_cal >= 0,]$ROI_cal, seq(0, 1, .05), na.rm = TRUE)
          SP_reactive_input$opti_cal_filtered_lsm <- data.table(left_join(SP_reactive_input$opti_cal_filtered_lsm,SP_reactive_input$SP_TPO_list$prod_const_shiny[,c("PPG","Brand","Format","LSM Min Promo Price")],by = c("PPG","Brand","Format")))
          
          setnames(SP_reactive_input$opti_cal_filtered_lsm,"LSM Min Promo Price","LSM Promo Price")
          SP_reactive_input$opti_cal_dcast_lsm <- data.table::dcast(unique(SP_reactive_input$opti_cal_filtered_lsm[,c("Format","PPG","PPG_Description","Week No","Promo_Price","RSP_Unit","LSM Promo Price","Tesco_Event","ROI_cal")]),Format + PPG + PPG_Description + RSP_Unit + `LSM Promo Price`~`Week No`,value.var= c("Promo_Price","Tesco_Event","ROI_cal"))
          SP_reactive_input$opti_cal_dcast_lsm_ROI <- SP_reactive_input$opti_cal_dcast_lsm
          names(SP_reactive_input$opti_cal_dcast_lsm) <- gsub("Promo_Price_","",names(SP_reactive_input$opti_cal_dcast_lsm))
          names(SP_reactive_input$opti_cal_dcast_lsm) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_lsm))
          
          names(SP_reactive_input$opti_cal_dcast_lsm_ROI) <- gsub("ROI_cal_","",names(SP_reactive_input$opti_cal_dcast_lsm_ROI))
          names(SP_reactive_input$opti_cal_dcast_lsm_ROI) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_lsm_ROI))
          
          setcolorder(SP_reactive_input$opti_cal_dcast_lsm,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`))),paste0("ROI_cal","_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))))
          setcolorder(SP_reactive_input$opti_cal_dcast_lsm_ROI,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`))),paste0("Promo_Price","_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`))))) 
          
        }
        ###showing all tables and charts related to LSM optimization
        shinyjs::show("SP_opti_cal_legend_LSM")
        shinyjs::show("SP_opti_cal_LSM_format_selection")
        shinyjs::show("SP_opti_save_LSM")
        shinyjs::show("SP_opti_save_name_LSM")
        shinyjs::show("SP_opti_cal_LSM")
        shinyjs::show("SP_const1_KPI_lsm")
        shinyjs::show("SP_const1_KPI_ret_lsm")
        shinyjs::show("SP_const1_KPI_cat_ret_lsm")
        shinyjs::show("SP_graph_3_ly")
        shinyjs::show("SP_graph_2_lsm")
        shinyjs::show("SP_graph_1_lsm")
        shinyjs::show("SP_graph_roi_lsm_op")
        shinyjs::show("SP_graph_roi_lsm_ly")
        shinyjs::show("SP_KPI_Iteration_LSM")
        shinyjs::show("SP_graph_4_lsm")
        
        output$SP_opti_cal_legend_LSM <- renderDataTable({
          datatable(transpose(data.frame(c("Shelf Promotion","Display Feature Promotion"))),class='display',
                    rownames = NULL, colnames = c(rep("",2)), 
                    options = list(dom = 'b',ordering=FALSE)) %>%
            formatStyle(columns=c(0:2),
                        background = styleEqual(c("Shelf Promotion","Display Feature Promotion"), 
                                                c('#0099DC', '#E42E92')),
                        color = styleEqual(c("Shelf Promotion","Display Feature Promotion"), 
                                           c('white', 'white'))) %>%
            formatStyle(1,border = styleInterval(0, c('auto', '1px solid white'))) %>%
            formatStyle(0,target = 'row',lineHeight='70%')
        })
      }
      ###Hiding all tables and charts related to LSM optimization if only Unconstrained is run
      else{
        shinyjs::hide("SP_opti_cal_legend_LSM")
        shinyjs::hide("SP_opti_cal_LSM_format_selection")
        shinyjs::hide("SP_opti_save_LSM")
        shinyjs::hide("SP_opti_save_name_LSM")
        shinyjs::hide("SP_opti_cal_LSM")
        shinyjs::hide("SP_const1_KPI_lsm")
        shinyjs::hide("SP_const1_KPI_ret_lsm")
        shinyjs::hide("SP_const1_KPI_cat_ret_lsm")
        shinyjs::hide("SP_graph_3_ly")
        shinyjs::hide("SP_graph_2_lsm")
        shinyjs::hide("SP_graph_1_lsm")
        shinyjs::hide("SP_graph_roi_lsm_op")
        shinyjs::hide("SP_graph_roi_lsm_ly")
        shinyjs::hide("SP_KPI_Iteration_LSM")
        shinyjs::hide("SP_graph_4_lsm")
      }
      
      ###Dataframe for Format level and PPG level output tabs in Optimizer output, taking lsm output when only LSM is run or else taking Unconstrained output
      if(is.null(SP_reactive_input$SP_opti_op_prepared)){
        SP_test1_ip <- SP_reactive_input$SP_opti_op_prepared_lsm
      }else{
        SP_test1_ip <- SP_reactive_input$SP_opti_op_prepared
      }
      # output$SP_opti_op_text <- renderText({
      #   paste0("Country:",SP_TPO_list$coun,", Customer:",SP_TPO_list$cust,", Category:",SP_TPO_list$cat,", Brand:",SP_TPO_list$brand)
      # })
      
      ###Updating top dropdown chocies in optimizer output screen
      updateSelectizeInput(session,"SP_opti_op_coun",choices = SP_TPO_list$coun,selected = SP_TPO_list$coun)
      updateSelectizeInput(session,"SP_opti_op_cust",choices = SP_TPO_list$cust,selected = SP_TPO_list$cust)
      updateSelectizeInput(session,"SP_opti_op_cat",choices = SP_TPO_list$cat,selected = SP_TPO_list$cat)
      updateSelectizeInput(session,"SP_opti_op_brand",choices = SP_TPO_list$brand,selected = SP_TPO_list$brand)
      
      ###Updating dropdown chocies on Product Level Deep Dive in optimizer output screen
      updateSelectizeInput(session,"SP_test1_format",choices = unique(SP_test1_ip$Format),selected = unique(SP_test1_ip$Format)[1])
      
      observeEvent({
        input$SP_test1_format},{
          updateSelectizeInput(session,"SP_test1_ppg",choices = unique(SP_test1_ip[SP_test1_ip$Format %in% input$SP_test1_format,]$PPG),selected = unique(SP_test1_ip[SP_test1_ip$Format == input$SP_test1_format,]$PPG)[1])
        }
      )
      
      ###Updating dropdown chocies on Format Level Deep Dive in optimizer output screen
      updateSelectizeInput(session,"SP_test2_brand",choices = unique(SP_test1_ip$Brand),selected = unique(SP_test1_ip$Brand)[1])
      observeEvent({
        input$SP_test2_brand},{
          updateSelectizeInput(session,"SP_test2_format",choices = c("",unique(SP_test1_ip[SP_test1_ip$Brand %in% input$SP_test2_brand,]$Format)),selected = "")
        }
      )
      
      observeEvent({
        input$SP_test2_format},{
          
          updateSelectizeInput(session,"SP_test2_ppg",choices = c("",unique(SP_test1_ip[SP_test1_ip$Format %in% input$SP_test2_format,]$PPG)),selected = "")
        }
      )
      
      ###Updating dropdown chocies on output ROI in optimizer output screen
      updateSelectizeInput(session,"SP_test3_cat",choices = unique(SP_test1_ip$Category),selected = unique(SP_test1_ip$Category)[1])
      observeEvent({
        input$SP_test3_cat},{
          updateSelectizeInput(session,"SP_test3_brand",choices = c("",unique(SP_test1_ip[SP_test1_ip$Category %in% input$SP_test3_cat,]$Brand)),selected = "")
        }
      )
      
      observeEvent({
        input$SP_test3_brand},{
          updateSelectizeInput(session,"SP_test3_format",choices = c("",unique(SP_test1_ip[SP_test1_ip$Brand %in% input$SP_test3_brand,]$Format)),selected = "")
        }
      )
      
      observeEvent({
        input$SP_test3_format},{
          
          updateSelectizeInput(session,"SP_test3_ppg",choices = c("",unique(SP_test1_ip[SP_test1_ip$Format %in% input$SP_test3_format,]$PPG)),selected = "")
        }
      )
      
      ###Updating dropdown chocies on Format Level Deep Dive in optimizer output screen
      updateSelectizeInput(session,"SP_test4_brand",choices = unique(SP_test1_ip$Brand),selected = unique(SP_test1_ip$Brand)[1])
      # Use current_year_date from optimizer constraint data instead of Week_Ending from optimizer output
      # This ensures the date filter matches the data being filtered (SP_opti_const_selected_1$current_year_date)
      if(!is.null(SP_reactive_input$SP_opti_const_selected) && nrow(SP_reactive_input$SP_opti_const_selected) > 0 && "current_year_date" %in% names(SP_reactive_input$SP_opti_const_selected)){
        date_choices <- unique(SP_reactive_input$SP_opti_const_selected$current_year_date)
        date_choices <- date_choices[!is.na(date_choices)]
        date_choices <- sort(date_choices)
        if(length(date_choices) > 0){
          updateSelectizeInput(session,"SP_test4_start_date",choices = date_choices,selected = date_choices[1])
        } else {
          # Fallback to optimizer output dates if constraint data dates are not available
          updateSelectizeInput(session,"SP_test4_start_date",choices = unique(SP_test1_ip$Week_Ending),selected = unique(SP_test1_ip$Week_Ending)[1])
        }
      } else {
        # Fallback to optimizer output dates if constraint data is not available
        updateSelectizeInput(session,"SP_test4_start_date",choices = unique(SP_test1_ip$Week_Ending),selected = unique(SP_test1_ip$Week_Ending)[1])
      }
      
      observeEvent(input$SP_test4_start_date,{
        
        if(input$SP_test4_start_date != ""){
          # Use current_year_date from optimizer constraint data if available
          if(!is.null(SP_reactive_input$SP_opti_const_selected) && nrow(SP_reactive_input$SP_opti_const_selected) > 0 && "current_year_date" %in% names(SP_reactive_input$SP_opti_const_selected)){
            date_choices <- unique(SP_reactive_input$SP_opti_const_selected$current_year_date)
            date_choices <- date_choices[!is.na(date_choices)]
            date_choices <- sort(date_choices)
            date_choices <- date_choices[date_choices >= as.Date(input$SP_test4_start_date)]
            if(length(date_choices) > 0){
              updateSelectizeInput(session,"SP_test4_end_date",choices = date_choices,selected = date_choices[length(date_choices)])
            } else {
              # Fallback to optimizer output dates
              updateSelectizeInput(session,"SP_test4_end_date",choices = unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date))],selected = unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date))][length(unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date))])])
            }
          } else {
            # Fallback to optimizer output dates
            updateSelectizeInput(session,"SP_test4_end_date",choices = unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date))],selected = unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date))][length(unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date))])])
          }
        }
      })
      
      observeEvent({
        input$SP_test4_brand},{
          updateSelectizeInput(session,"SP_test4_format",choices = c("",unique(SP_test1_ip[SP_test1_ip$Brand %in% input$SP_test4_brand,]$Format)),selected = "")
        }
      )
      
      observeEvent({
        input$SP_test4_format},{
          
          updateSelectizeInput(session,"SP_test4_ppg",choices = c("",unique(SP_test1_ip[SP_test1_ip$Format %in% input$SP_test4_format,]$PPG)),selected = "")
        }
      )
      
      ###Plots for Product Level Deep Dive
      observeEvent({
        input$SP_test1_format
        input$SP_test1_ppg},{
          if(input$SP_test1_ppg != ""){
            
            if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
              SP_reactive_input$SP_graph_1_ip <- SP_reactive_input$SP_opti_op_prepared[SP_reactive_input$SP_opti_op_prepared$Format %in% input$SP_test1_format & SP_reactive_input$SP_opti_op_prepared$PPG %in% input$SP_test1_ppg,c("Date","Promo_Price","Total_Sales","TPR_Flag","Display_Flag")]
              SP_reactive_input$SP_graph_1_ip$Event <- ifelse(SP_reactive_input$SP_graph_1_ip$Display_Flag == 1,"Display",ifelse(SP_reactive_input$SP_graph_1_ip$TPR_Flag == 1,"Shelf","No Promo"))
            }
            if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
              
              SP_reactive_input$SP_graph_1_lsm_ip <- SP_reactive_input$SP_opti_op_prepared_lsm[SP_reactive_input$SP_opti_op_prepared_lsm$Format %in% input$SP_test1_format & SP_reactive_input$SP_opti_op_prepared_lsm$PPG %in% input$SP_test1_ppg,c("Date","Promo_Price","Total_Sales","TPR_Flag","Display_Flag")]
              SP_reactive_input$SP_graph_1_lsm_ip$Event <- ifelse(SP_reactive_input$SP_graph_1_lsm_ip$Display_Flag == 1,"Display",ifelse(SP_reactive_input$SP_graph_1_lsm_ip$TPR_Flag == 1,"Shelf","No Promo"))
            }
          }
        })
      
      ###Plots for Output ROI tab
      observeEvent(c(
        input$SP_test3_cat,
        input$SP_test3_brand,
        input$SP_test3_format,
        input$SP_test3_ppg,
        input$SP_sim_replace_confirm),{
          if(!(is.null(SP_reactive_input$SP_opti_op_prepared_lsm)) | !(is.null(SP_reactive_input$SP_opti_op_prepared))){
            if(SP_reactive_input$opti_format_input != "ALL"){
              SP_reactive_input$SP_opti_const_selected_2 <- data.table(SP_reactive_input$cal_sample[SP_reactive_input$cal_sample$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$cal_sample$Brand %in% input$SP_brand & SP_reactive_input$cal_sample$Format %in% SP_reactive_input$opti_format_input,])
            }else{
              SP_reactive_input$SP_opti_const_selected_2 <- data.table(SP_reactive_input$cal_sample[SP_reactive_input$cal_sample$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$cal_sample$Brand %in% input$SP_brand,])
            }
            if(is.null(input$SP_test3_brand) & is.null(input$SP_test3_ppg) & is.null(input$SP_test3_format)){
              SP_reactive_input$SP_promo_eff_opti_ly <- SP_reactive_input$SP_opti_const_selected_2[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Total_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc),2)),by = .(Brand)]
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization")){
                SP_reactive_input$SP_promo_eff_opti_nonlsm <- SP_reactive_input$SP_opti_op_prepared[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand)]
                SP_reactive_input$SP_promo_eff_opti_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand)]
              }
              ###Unconstrained
              if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                SP_reactive_input$SP_promo_eff_opti_nonlsm <- SP_reactive_input$SP_opti_op_prepared[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand)]
                if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization")){
                  SP_reactive_input$SP_promo_eff_opti_lsm <- NULL
                }
                output$SP_graph_roi_nonlsm_op <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_nonlsm,"Brand",input$SP_promo_ROI_selection_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm,SP_reactive_input$SP_promo_eff_opti_ly)
                })
                
                output$SP_graph_roi_nonlsm_ly <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_ly,"Brand",input$SP_promo_ROI_selection_nonlsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm)
                })
              }
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
                if(input$SP_opti_run_choice %in% c("Run LSM constrained Optimization")){
                  SP_reactive_input$SP_promo_eff_opti_nonlsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand)]
                
                output$SP_graph_roi_lsm_op <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_lsm,"Brand",input$SP_promo_ROI_selection_lsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_ly)
                })
                
                output$SP_graph_roi_lsm_ly <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_ly,"Brand",input$SP_promo_ROI_selection_lsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm)
                })
              }
              
            }else if(is.null(input$SP_test3_ppg) & is.null(input$SP_test3_format)){
              SP_reactive_input$SP_promo_eff_opti_ly <- SP_reactive_input$SP_opti_const_selected_2[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Total_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc),2)),by = .(Brand,Format)]
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization")){
                SP_reactive_input$SP_promo_eff_opti_nonlsm <- SP_reactive_input$SP_opti_op_prepared[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format)]
                SP_reactive_input$SP_promo_eff_opti_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format)]
              }
              
              ###Unconstrained
              if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization")){
                  SP_reactive_input$SP_promo_eff_opti_lsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_nonlsm <- SP_reactive_input$SP_opti_op_prepared[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format)]
                output$SP_graph_roi_nonlsm_op <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_nonlsm,"Format",input$SP_promo_ROI_selection_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm,SP_reactive_input$SP_promo_eff_opti_ly)
                })
                
                output$SP_graph_roi_nonlsm_ly <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_ly,"Format",input$SP_promo_ROI_selection_nonlsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm)
                })
              }
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
                if(input$SP_opti_run_choice %in% c("Run LSM constrained Optimization")){
                  SP_reactive_input$SP_promo_eff_opti_nonlsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format)]
                output$SP_graph_roi_lsm_op <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_lsm,"Format",input$SP_promo_ROI_selection_lsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_ly)
                })
                
                output$SP_graph_roi_lsm_ly <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_ly,"Format",input$SP_promo_ROI_selection_lsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm)
                })
              }
              
            }else if(is.null(input$SP_test3_ppg) & !is.null(input$SP_test3_format)){
              SP_reactive_input$SP_promo_eff_opti_ly <- SP_reactive_input$SP_opti_const_selected_2[SP_reactive_input$SP_opti_const_selected_2$Format %in% input$SP_test3_format,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Total_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG)]
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization")){
                SP_reactive_input$SP_promo_eff_opti_nonlsm <- SP_reactive_input$SP_opti_op_prepared[SP_reactive_input$SP_opti_op_prepared$Format %in% input$SP_test3_format,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG)]
                SP_reactive_input$SP_promo_eff_opti_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm[SP_reactive_input$SP_opti_op_prepared_lsm$Format %in% input$SP_test3_format,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG)]
              }
              
              if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization")){
                  SP_reactive_input$SP_promo_eff_opti_lsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_nonlsm <- SP_reactive_input$SP_opti_op_prepared[SP_reactive_input$SP_opti_op_prepared$Format %in% input$SP_test3_format,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG)]
                output$SP_graph_roi_nonlsm_op <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_nonlsm,"PPG",input$SP_promo_ROI_selection_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm,SP_reactive_input$SP_promo_eff_opti_ly)
                })
                
                output$SP_graph_roi_nonlsm_ly <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_ly,"PPG",input$SP_promo_ROI_selection_nonlsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm)
                })
              }
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
                if(input$SP_opti_run_choice %in% c("Run LSM constrained Optimization")){
                  SP_reactive_input$SP_promo_eff_opti_nonlsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm[SP_reactive_input$SP_opti_op_prepared_lsm$Format %in% input$SP_test3_format,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG)]
                output$SP_graph_roi_lsm_op <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_lsm,"PPG",input$SP_promo_ROI_selection_lsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_ly)
                })
                
                output$SP_graph_roi_lsm_ly <- renderPlotly({
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_ly,"PPG",input$SP_promo_ROI_selection_lsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm)
                })
              }
              
            }else if(!is.null(input$SP_test3_ppg)){
              SP_reactive_input$SP_promo_eff_opti_ly <- SP_reactive_input$SP_opti_const_selected_2[SP_reactive_input$SP_opti_const_selected_2$Format %in% input$SP_test3_format & SP_reactive_input$SP_opti_const_selected_2$PPG %in% input$SP_test3_ppg,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Total_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG,Event_Number)]
              SP_reactive_input$SP_promo_eff_opti_ly$Event_Number <- paste0("Event-",SP_reactive_input$SP_promo_eff_opti_ly$Event_Number)
              setnames(SP_reactive_input$SP_promo_eff_opti_ly,"Event_Number","Event ID")
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization")){
                SP_reactive_input$SP_promo_eff_opti_nonlsm <- SP_reactive_input$SP_opti_op_prepared[SP_reactive_input$SP_opti_op_prepared$Format %in% input$SP_test3_format & SP_reactive_input$SP_opti_op_prepared$PPG %in% input$SP_test3_ppg,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG,Tesco_Week_No)]
                if(all(is.na(SP_reactive_input$SP_promo_eff_opti_nonlsm$ROI)) & all(is.na(SP_reactive_input$SP_promo_eff_opti_nonlsm$Investment))){
                  SP_reactive_input$SP_promo_eff_opti_nonlsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_nonlsm$Tesco_Week_No <- paste0("Event-",SP_reactive_input$SP_promo_eff_opti_nonlsm$Tesco_Week_No)
                setnames(SP_reactive_input$SP_promo_eff_opti_nonlsm,"Tesco_Week_No","Event ID")
                
                SP_reactive_input$SP_promo_eff_opti_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm[SP_reactive_input$SP_opti_op_prepared_lsm$Format %in% input$SP_test3_format & SP_reactive_input$SP_opti_op_prepared_lsm$PPG %in% input$SP_test3_ppg,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG,Tesco_Week_No)]
                if(all(is.na(SP_reactive_input$SP_promo_eff_opti_lsm$ROI)) & all(is.na(SP_reactive_input$SP_promo_eff_opti_lsm$Investment))){
                  SP_reactive_input$SP_promo_eff_opti_lsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_lsm$Tesco_Week_No <- paste0("Event-",SP_reactive_input$SP_promo_eff_opti_lsm$Tesco_Week_No)
                setnames(SP_reactive_input$SP_promo_eff_opti_lsm,"Tesco_Week_No","Event ID")
                
              }
              if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization")){
                  SP_reactive_input$SP_promo_eff_opti_lsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_nonlsm <- SP_reactive_input$SP_opti_op_prepared[SP_reactive_input$SP_opti_op_prepared$Format %in% input$SP_test3_format & SP_reactive_input$SP_opti_op_prepared$PPG %in% input$SP_test3_ppg,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG,Tesco_Week_No)]
                if(all(is.na(SP_reactive_input$SP_promo_eff_opti_nonlsm$ROI)) & all(is.na(SP_reactive_input$SP_promo_eff_opti_nonlsm$Investment))){
                  SP_reactive_input$SP_promo_eff_opti_nonlsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_nonlsm$Tesco_Week_No <- paste0("Event-",SP_reactive_input$SP_promo_eff_opti_nonlsm$Tesco_Week_No)
                setnames(SP_reactive_input$SP_promo_eff_opti_nonlsm,"Tesco_Week_No","Event ID")
                
                output$SP_graph_roi_nonlsm_op <- renderPlotly({
                  validate(
                    need(SP_reactive_input$SP_promo_eff_opti_nonlsm,"No Promotion happened for the selected PPG within selected time period")
                  )
                  
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_nonlsm,"Event ID",input$SP_promo_ROI_selection_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm,SP_reactive_input$SP_promo_eff_opti_ly)
                })
                
                output$SP_graph_roi_nonlsm_ly <- renderPlotly({
                  validate(
                    need(SP_reactive_input$SP_promo_eff_opti_ly,"No Promotion happened for the selected PPG within selected time period")
                  )
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_ly,"Event ID",input$SP_promo_ROI_selection_nonlsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm)
                })
              }
              
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
                if(input$SP_opti_run_choice %in% c("Run LSM constrained Optimization")){
                  SP_reactive_input$SP_promo_eff_opti_nonlsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm[SP_reactive_input$SP_opti_op_prepared_lsm$Format %in% input$SP_test3_format & SP_reactive_input$SP_opti_op_prepared_lsm$PPG %in% input$SP_test3_ppg,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG,Tesco_Week_No)]
                if(all(is.na(SP_reactive_input$SP_promo_eff_opti_lsm$ROI)) & all(is.na(SP_reactive_input$SP_promo_eff_opti_lsm$Investment))){
                  SP_reactive_input$SP_promo_eff_opti_lsm <- NULL
                }
                SP_reactive_input$SP_promo_eff_opti_lsm$Tesco_Week_No <- paste0("Event-",SP_reactive_input$SP_promo_eff_opti_lsm$Tesco_Week_No)
                setnames(SP_reactive_input$SP_promo_eff_opti_lsm,"Tesco_Week_No","Event ID")
                
                output$SP_graph_roi_lsm_op <- renderPlotly({
                  validate(
                    need(SP_reactive_input$SP_promo_eff_opti_lsm,"No Promotion happened for the selected PPG within selected time period")
                  )
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_lsm,"Event ID",input$SP_promo_ROI_selection_lsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_ly)
                })
                
                output$SP_graph_roi_lsm_ly <- renderPlotly({
                  validate(
                    need(SP_reactive_input$SP_promo_eff_opti_ly,"No Promotion happened for the selected PPG within selected time period")
                  )
                  promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_ly,"Event ID",input$SP_promo_ROI_selection_lsm,SP_reactive_input$SP_promo_eff_opti_nonlsm,SP_reactive_input$SP_promo_eff_opti_lsm)
                })
              }
              
            }
          }
        })
      
      ###Plots for Format Level Deep Dive tab
      observeEvent(c(
        input$SP_test2_brand,
        input$SP_test2_format,
        input$SP_test2_ppg,
        input$SP_test4_start_date,
        input$SP_test4_end_date,
        input$SP_opti_op_include_delist,
        input$SP_opti_op_include_exclude,
        input$SP_sim_replace_confirm),{
          if(!(is.null(SP_reactive_input$SP_opti_op_prepared_lsm)) | !(is.null(SP_reactive_input$SP_opti_op_prepared))){
            if(input$SP_test4_start_date != "" & input$SP_test4_end_date != ""){
              
              if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                SP_reactive_input$SP_opti_exc_brand <- data.table(SP_reactive_input$SP_opti_exc_brand)
              }else{
                SP_reactive_input$SP_opti_exc_brand <- data.table(SP_reactive_input$SP_opti_exc_brand_lsm)
              }
              
              SP_reactive_input$SP_opti_exc_brand_1 <- data.table(left_join(SP_reactive_input$SP_opti_exc_brand, SP_reactive_input$date_mapping, by = c("Date" = "last_year_date")))
              if(SP_reactive_input$opti_format_input != "ALL"){
                SP_reactive_input$SP_opti_const_selected_1 <- SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Format %in% SP_reactive_input$opti_format_input,]
              }else{
                SP_reactive_input$SP_opti_const_selected_1 <- SP_reactive_input$SP_opti_const_selected
              }
              #SP_reactive_input$SP_opti_const_selected_1 <- data.table(left_join(SP_reactive_input$SP_opti_const_selected_1, SP_reactive_input$date_mapping, by = c("Date" = "last_year_date")))
              
              
              if(is.null(input$SP_test2_ppg) & is.null(input$SP_test2_format)){
                # Debug: Check source data
                 
                cat("=== DEBUG: Checking source data ===\n")
                cat("SP_opti_const_selected_1 rows:", nrow(SP_reactive_input$SP_opti_const_selected_1), "\n")
                cat("Date filter - Start:", input$SP_test4_start_date, "End:", input$SP_test4_end_date, "\n")
                cat("Manufacturer filter:", paste(SP_reactive_input$SP_manuf, collapse = ", "), "\n")
                cat("Brand filter:", paste(input$SP_brand, collapse = ", "), "\n")
                str(SP_reactive_input$SP_opti_const_selected_1)
                
                SP_reactive_input$SP_opti_const_selected_1$Manufacturer="Manu01"
                SP_reactive_input$SP_opti_const_selected_1$Brand="Brand005"
                
                SP_reactive_input$SP_test2_ly <- SP_reactive_input$SP_opti_const_selected_1[SP_reactive_input$SP_opti_const_selected_1$current_year_date >= input$SP_test4_start_date & SP_reactive_input$SP_opti_const_selected_1$current_year_date <= input$SP_test4_end_date & SP_reactive_input$SP_opti_const_selected_1$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected_1$Brand %in% (input$SP_brand),.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                                                                                                                     "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "Format"]
                
                
                
                
                
                cat("SP_test2_ly rows after aggregation:", nrow(SP_reactive_input$SP_test2_ly), "\n")
                if(nrow(SP_reactive_input$SP_test2_ly) > 0){
                  cat("SP_test2_ly Format values:", paste(unique(SP_reactive_input$SP_test2_ly$Format), collapse = ", "), "\n")
                  cat("SP_test2_ly head:\n")
                  print(head(SP_reactive_input$SP_test2_ly))
                } else {
                  cat("WARNING: SP_test2_ly is EMPTY! Check filters.\n")
                }
                
                SP_reactive_input$SP_test2_exclude <- SP_reactive_input$SP_opti_exc_brand_1[SP_reactive_input$SP_opti_exc_brand_1$current_year_date >= input$SP_test4_start_date & SP_reactive_input$SP_opti_exc_brand_1$current_year_date <= input$SP_test4_end_date & SP_reactive_input$SP_opti_exc_brand_1$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_exc_brand_1$Brand %in% (input$SP_brand),.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                                 "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "Format"]
                
                SP_reactive_input$SP_test2_delist <- SP_reactive_input$SP_RB_Delist[SP_reactive_input$SP_RB_Delist$current_year_date >= input$SP_test4_start_date & SP_reactive_input$SP_RB_Delist$current_year_date <= input$SP_test4_end_date,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                                                                                                                  "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "Format"]
                
                if(input$SP_opti_op_include_delist == FALSE & input$SP_opti_op_include_exclude == TRUE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                     
                    
                    result_dt = data.table(x[1])
                    setnames(result_dt, c("Format","NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                    result_dt
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("Format" = SP_reactive_input$SP_test2_ly$Format),
                    rbindlist(result_list, fill = TRUE)
                  )
                }else if(input$SP_opti_op_include_delist == FALSE & input$SP_opti_op_include_exclude == FALSE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_delist$Format){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_delist[Format == x[1], c(2:length(SP_reactive_input$SP_test2_delist)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("Format" = SP_reactive_input$SP_test2_ly$Format),
                    rbindlist(result_list, fill = TRUE)
                  )
                  
                  result_list2 = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly_delist)), function(i){
                    x = SP_reactive_input$SP_test2_ly_delist[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_exclude$Format){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_exclude[Format == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("Format" = SP_reactive_input$SP_test2_ly_delist$Format),
                    rbindlist(result_list2, fill = TRUE)
                  )
                  
                }else if(input$SP_opti_op_include_delist == TRUE & input$SP_opti_op_include_exclude == TRUE){
                  SP_reactive_input$SP_test2_ly_delist <- data.frame(SP_reactive_input$SP_test2_ly)
                  
                }else if(input$SP_opti_op_include_delist == TRUE & input$SP_opti_op_include_exclude == FALSE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_exclude$Format){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_exclude[Format == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("Format" = SP_reactive_input$SP_test2_ly$Format),
                    rbindlist(result_list, fill = TRUE)
                  )
                }
                SP_reactive_input$SP_test2_ly_delist_exclude <- SP_reactive_input$SP_test2_ly_delist
                
                # Debug: Check SP_test2_ly_delist before creating exclude
                 
                cat("=== DEBUG: Checking data before join ===\n")
                cat("SP_test2_ly_delist rows:", nrow(SP_reactive_input$SP_test2_ly_delist), "\n")
                cat("SP_test2_ly_delist columns:", paste(names(SP_reactive_input$SP_test2_ly_delist), collapse = ", "), "\n")
                if(nrow(SP_reactive_input$SP_test2_ly_delist) > 0){
                  cat("SP_test2_ly_delist Format values:", paste(unique(SP_reactive_input$SP_test2_ly_delist$Format), collapse = ", "), "\n")
                  cat("SP_test2_ly_delist head:\n")
                  print(head(SP_reactive_input$SP_test2_ly_delist))
                }
                
                SP_reactive_input$SP_test2_ly_delist_exclude$GM_NR <- SP_reactive_input$SP_test2_ly_delist_exclude$Gross_Margin * 100/SP_reactive_input$SP_test2_ly_delist_exclude$NR
                SP_reactive_input$SP_test2_ly_delist_exclude$TI_NR <- SP_reactive_input$SP_test2_ly_delist_exclude$TI * 100/SP_reactive_input$SP_test2_ly_delist_exclude$NR
                
                ###Unconstrained
                if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                  cat("\n=== Creating SP_test2_nonlsm_op ===\n")
                  cat("SP_opti_op_prepared rows:", nrow(SP_reactive_input$SP_opti_op_prepared), "\n")
                  cat("Date filter - Start:", input$SP_test4_start_date, "End:", input$SP_test4_end_date, "\n")
                  
                  SP_reactive_input$SP_test2_nonlsm_op <- SP_reactive_input$SP_opti_op_prepared[SP_reactive_input$SP_opti_op_prepared$Week_Ending >= input$SP_test4_start_date & SP_reactive_input$SP_opti_op_prepared$Week_Ending <= input$SP_test4_end_date,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "Format"]
                  
                  cat("SP_test2_nonlsm_op after aggregation rows:", nrow(SP_reactive_input$SP_test2_nonlsm_op), "\n")
                  cat("SP_test2_nonlsm_op Format values:", paste(unique(SP_reactive_input$SP_test2_nonlsm_op$Format), collapse = ", "), "\n")
                  if(nrow(SP_reactive_input$SP_test2_nonlsm_op) > 0){
                    cat("SP_test2_nonlsm_op head:\n")
                    print(head(SP_reactive_input$SP_test2_nonlsm_op))
                  }
                  
                  cat("\nSP_test2_ly rows:", nrow(SP_reactive_input$SP_test2_ly), "\n")
                  cat("SP_test2_ly Format values:", paste(unique(SP_reactive_input$SP_test2_ly$Format), collapse = ", "), "\n")
                  
                  SP_reactive_input$SP_test2_nonlsm_op <- left_join(SP_reactive_input$SP_test2_ly[,c("Format")],SP_reactive_input$SP_test2_nonlsm_op,by = "Format")
                  
                  cat("SP_test2_nonlsm_op after left_join rows:", nrow(SP_reactive_input$SP_test2_nonlsm_op), "\n")
                  cat("SP_test2_nonlsm_op Format values after join:", paste(unique(SP_reactive_input$SP_test2_nonlsm_op$Format), collapse = ", "), "\n")
                  
                  SP_reactive_input$SP_test2_nonlsm_op[is.na(SP_reactive_input$SP_test2_nonlsm_op)] <- 0
                  if(input$SP_opti_op_include_exclude == TRUE){
                    result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_nonlsm_op)), function(i){
                      x = SP_reactive_input$SP_test2_nonlsm_op[i,]
                      
                      result_dt = data.table(x[1])
                      setnames(result_dt, c("Format","NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                      
                    })
                    SP_reactive_input$SP_test2_nonlsm_op <- cbind(
                      data.frame("Format" = SP_reactive_input$SP_test2_nonlsm_op$Format),
                      rbindlist(result_list, fill = TRUE)
                    )
                  }
                  SP_reactive_input$SP_test2_nonlsm_op$GM_NR <- SP_reactive_input$SP_test2_nonlsm_op$Gross_Margin * 100/SP_reactive_input$SP_test2_nonlsm_op$NR
                  SP_reactive_input$SP_test2_nonlsm_op$TI_NR <- SP_reactive_input$SP_test2_nonlsm_op$TI * 100/SP_reactive_input$SP_test2_nonlsm_op$NR
                  
                  # Debug: Check columns before join
                     # Check column names before join
                  cat("Columns in SP_test2_ly_delist_exclude:\n")
                  print(names(SP_reactive_input$SP_test2_ly_delist_exclude))
                  cat("\nColumns in SP_test2_nonlsm_op:\n")
                  print(names(SP_reactive_input$SP_test2_nonlsm_op))
                  cat("\nCommon columns (excluding Format):\n")
                  common_cols <- intersect(setdiff(names(SP_reactive_input$SP_test2_ly_delist_exclude), "Format"),
                                           setdiff(names(SP_reactive_input$SP_test2_nonlsm_op), "Format"))
                  print(common_cols)
                  
                   
                  SP_reactive_input$SP_test2_nonlsm_diff <- SP_reactive_input$SP_test2_nonlsm_op
                  
                  # Debug: Check columns after join
                  cat("\nColumns in SP_test2_nonlsm_diff after join:\n")
                  print(names(SP_reactive_input$SP_test2_nonlsm_diff))
                  
                  SP_reactive_input$SP_test2_nonlsm_diff[is.na(SP_reactive_input$SP_test2_nonlsm_diff)] <- 0
                  
                  # Check if data frame has rows before processing
                  if(nrow(SP_reactive_input$SP_test2_nonlsm_diff) > 0){
                    # Check if expected columns exist, if not create them with 0 values
                    req_cols = c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x",
                                 "NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y",
                                 "GM_NR.x","TI_NR.x","GM_NR.y","TI_NR.y")
                    missing_cols = setdiff(req_cols, names(SP_reactive_input$SP_test2_nonlsm_diff))
                    if(length(missing_cols) > 0){
                      for(col in missing_cols){
                        SP_reactive_input$SP_test2_nonlsm_diff[[col]] <- rep(0, nrow(SP_reactive_input$SP_test2_nonlsm_diff))
                      }
                    }
                    
                    SP_reactive_input$SP_test2_nonlsm_diff_calc <- cbind(data.frame("Format" = SP_reactive_input$SP_test2_nonlsm_diff$Format),(SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_nonlsm_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_nonlsm_diff[,c("GM_NR.x","TI_NR.x")])*100)
                    SP_reactive_input$SP_test2_nonlsm_diff_calc[is.na(SP_reactive_input$SP_test2_nonlsm_diff_calc)] <- 0
                  } else {
                    # If no rows, create empty data frame with expected structure
                    SP_reactive_input$SP_test2_nonlsm_diff_calc <- data.frame(
                      Format = character(0),
                      stringsAsFactors = FALSE
                    )
                  }
                }
                
                if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
                  SP_reactive_input$SP_test2_lsm_op <- SP_reactive_input$SP_opti_op_prepared_lsm[SP_reactive_input$SP_opti_op_prepared_lsm$Week_Ending >= input$SP_test4_start_date & SP_reactive_input$SP_opti_op_prepared_lsm$Week_Ending <= input$SP_test4_end_date,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                         "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "Format"]
                  
                  SP_reactive_input$SP_test2_lsm_op <- left_join(SP_reactive_input$SP_test2_ly[,c("Format")],SP_reactive_input$SP_test2_lsm_op,by = "Format")
                  SP_reactive_input$SP_test2_lsm_op[is.na(SP_reactive_input$SP_test2_lsm_op)] <- 0
                  if(input$SP_opti_op_include_exclude == TRUE){
                    result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_lsm_op)), function(i){
                      x = SP_reactive_input$SP_test2_lsm_op[i,]
                      if(x[1] %in% SP_reactive_input$SP_test2_exclude$Format){
                        sum_vec = as.numeric(x[2:length(x)]) + 
                          as.numeric(SP_reactive_input$SP_test2_exclude[Format == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                        result_dt = data.table(matrix(sum_vec, nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      } else {
                        result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      }
                    })
                    SP_reactive_input$SP_test2_lsm_op <- cbind(
                      data.frame("Format" = SP_reactive_input$SP_test2_lsm_op$Format),
                      rbindlist(result_list, fill = TRUE)
                    )
                  }
                  SP_reactive_input$SP_test2_lsm_op$GM_NR <- SP_reactive_input$SP_test2_lsm_op$Gross_Margin * 100/SP_reactive_input$SP_test2_lsm_op$NR
                  SP_reactive_input$SP_test2_lsm_op$TI_NR <- SP_reactive_input$SP_test2_lsm_op$TI * 100/SP_reactive_input$SP_test2_lsm_op$NR
                  
                  SP_reactive_input$SP_test2_lsm_diff <- left_join(SP_reactive_input$SP_test2_ly_delist_exclude,SP_reactive_input$SP_test2_lsm_op, by = "Format")
                  SP_reactive_input$SP_test2_lsm_diff[is.na(SP_reactive_input$SP_test2_lsm_diff)] <- 0
                  #rbindlist(sapply(sapply(c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"),function(x) {(SP_reactive_input$SP_test2_lsm_diff[paste0(x,".x")] - SP_reactive_input$SP_test2_lsm_diff[paste0(x,".y")])}),function(x) {setnames(data.frame(x),names(SP_reactive_input$SP_test2_lsm_diff))}))
                  SP_reactive_input$SP_test2_lsm_diff_calc <- cbind(data.frame("Format" = SP_reactive_input$SP_test2_lsm_diff$Format),(SP_reactive_input$SP_test2_lsm_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_lsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_lsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_lsm_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_lsm_diff[,c("GM_NR.x","TI_NR.x")])*100)
                  SP_reactive_input$SP_test2_lsm_diff_calc[is.na(SP_reactive_input$SP_test2_lsm_diff_calc)] <- 0
                }
                
              }else if(is.null(input$SP_test2_ppg) & !is.null(input$SP_test2_format)){
                SP_reactive_input$SP_test2_ly <- SP_reactive_input$SP_opti_const_selected_1[SP_reactive_input$SP_opti_const_selected_1$current_year_date >= input$SP_test4_start_date & SP_reactive_input$SP_opti_const_selected_1$current_year_date <= input$SP_test4_end_date & SP_reactive_input$SP_opti_const_selected_1$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected_1$Brand %in% (input$SP_brand) & SP_reactive_input$SP_opti_const_selected_1$Format %in% input$SP_test2_format,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                SP_reactive_input$SP_test2_exclude <- SP_reactive_input$SP_opti_exc_brand_1[SP_reactive_input$SP_opti_exc_brand_1$current_year_date >= input$SP_test4_start_date & SP_reactive_input$SP_opti_exc_brand_1$current_year_date <= input$SP_test4_end_date & SP_reactive_input$SP_opti_exc_brand_1$Format %in% input$SP_test2_format,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                  "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                SP_reactive_input$SP_test2_delist <- SP_reactive_input$SP_RB_Delist[SP_reactive_input$SP_RB_Delist$Format %in% input$SP_test2_format,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                       "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                if(input$SP_opti_op_include_delist == FALSE & input$SP_opti_op_include_exclude == TRUE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_delist$PPG){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_delist[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_delist)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("PPG" = SP_reactive_input$SP_test2_ly$PPG),
                    rbindlist(result_list, fill = TRUE)
                  )
                }else if(input$SP_opti_op_include_delist == FALSE & input$SP_opti_op_include_exclude == FALSE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_delist$PPG){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_delist[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_delist)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("PPG" = SP_reactive_input$SP_test2_ly$PPG),
                    rbindlist(result_list, fill = TRUE)
                  )
                  
                  result_list2 = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly_delist)), function(i){
                    x = SP_reactive_input$SP_test2_ly_delist[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_exclude[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("PPG" = SP_reactive_input$SP_test2_ly_delist$PPG),
                    rbindlist(result_list2, fill = TRUE)
                  )
                  
                }else if(input$SP_opti_op_include_delist == TRUE & input$SP_opti_op_include_exclude == TRUE){
                  SP_reactive_input$SP_test2_ly_delist <- data.frame(SP_reactive_input$SP_test2_ly)
                  
                }else if(input$SP_opti_op_include_delist == TRUE & input$SP_opti_op_include_exclude == FALSE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_exclude[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("PPG" = SP_reactive_input$SP_test2_ly$PPG),
                    rbindlist(result_list, fill = TRUE)
                  )
                }
                
                SP_reactive_input$SP_test2_ly_delist_exclude <- SP_reactive_input$SP_test2_ly_delist
                SP_reactive_input$SP_test2_ly_delist_exclude$GM_NR <- SP_reactive_input$SP_test2_ly_delist_exclude$Gross_Margin * 100/SP_reactive_input$SP_test2_ly_delist_exclude$NR
                SP_reactive_input$SP_test2_ly_delist_exclude$TI_NR <- SP_reactive_input$SP_test2_ly_delist_exclude$TI * 100/SP_reactive_input$SP_test2_ly_delist_exclude$NR
                
                if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                  SP_reactive_input$SP_test2_nonlsm_op <- SP_reactive_input$SP_opti_op_prepared[SP_reactive_input$SP_opti_op_prepared$Week_Ending >= input$SP_test4_start_date & SP_reactive_input$SP_opti_op_prepared$Week_Ending <= input$SP_test4_end_date & SP_reactive_input$SP_opti_op_prepared$Format %in% input$SP_test2_format,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                          "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                  
                  SP_reactive_input$SP_test2_nonlsm_op <- left_join(SP_reactive_input$SP_test2_ly[,c("PPG")],SP_reactive_input$SP_test2_nonlsm_op,by = "PPG")
                  SP_reactive_input$SP_test2_nonlsm_op[is.na(SP_reactive_input$SP_test2_nonlsm_op)] <- 0
                  if(input$SP_opti_op_include_exclude == TRUE){
                    result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_nonlsm_op)), function(i){
                      x = SP_reactive_input$SP_test2_nonlsm_op[i,]
                      if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){
                        sum_vec = as.numeric(x[2:length(x)]) + 
                          as.numeric(SP_reactive_input$SP_test2_exclude[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                        result_dt = data.table(matrix(sum_vec, nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      } else {
                        result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      }
                    })
                    SP_reactive_input$SP_test2_nonlsm_op <- cbind(
                      data.frame("PPG" = SP_reactive_input$SP_test2_nonlsm_op$PPG),
                      rbindlist(result_list, fill = TRUE)
                    )
                  }
                  
                  SP_reactive_input$SP_test2_nonlsm_op$GM_NR <- SP_reactive_input$SP_test2_nonlsm_op$Gross_Margin * 100/SP_reactive_input$SP_test2_nonlsm_op$NR
                  SP_reactive_input$SP_test2_nonlsm_op$TI_NR <- SP_reactive_input$SP_test2_nonlsm_op$TI * 100/SP_reactive_input$SP_test2_nonlsm_op$NR
                  
                  SP_reactive_input$SP_test2_nonlsm_diff <- left_join(SP_reactive_input$SP_test2_ly_delist_exclude,SP_reactive_input$SP_test2_nonlsm_op, by = "PPG")
                  SP_reactive_input$SP_test2_nonlsm_diff[is.na(SP_reactive_input$SP_test2_nonlsm_diff)] <- 0
                  
                  SP_reactive_input$SP_test2_nonlsm_diff_calc <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_nonlsm_diff$PPG),(SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_nonlsm_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_nonlsm_diff[,c("GM_NR.x","TI_NR.x")])*100)
                  SP_reactive_input$SP_test2_nonlsm_diff_calc[is.na(SP_reactive_input$SP_test2_nonlsm_diff_calc)] <- 0
                }
                if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
                  SP_reactive_input$SP_test2_lsm_op <- SP_reactive_input$SP_opti_op_prepared_lsm[SP_reactive_input$SP_opti_op_prepared_lsm$Week_Ending >= input$SP_test4_start_date & SP_reactive_input$SP_opti_op_prepared_lsm$Week_Ending <= input$SP_test4_end_date & SP_reactive_input$SP_opti_op_prepared_lsm$Format %in% input$SP_test2_format,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                       "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                  
                  SP_reactive_input$SP_test2_lsm_op <- left_join(SP_reactive_input$SP_test2_ly[,c("PPG")],SP_reactive_input$SP_test2_lsm_op,by = "PPG")
                  SP_reactive_input$SP_test2_lsm_op[is.na(SP_reactive_input$SP_test2_lsm_op)] <- 0
                  if(input$SP_opti_op_include_exclude == TRUE){
                    result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_lsm_op)), function(i){
                      x = SP_reactive_input$SP_test2_lsm_op[i,]
                      if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){
                        sum_vec = as.numeric(x[2:length(x)]) + 
                          as.numeric(SP_reactive_input$SP_test2_exclude[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                        result_dt = data.table(matrix(sum_vec, nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      } else {
                        result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      }
                    })
                    SP_reactive_input$SP_test2_lsm_op <- cbind(
                      data.frame("PPG" = SP_reactive_input$SP_test2_lsm_op$PPG),
                      rbindlist(result_list, fill = TRUE)
                    )
                  }
                  SP_reactive_input$SP_test2_lsm_op$GM_NR <- SP_reactive_input$SP_test2_lsm_op$Gross_Margin * 100/SP_reactive_input$SP_test2_lsm_op$NR
                  SP_reactive_input$SP_test2_lsm_op$TI_NR <- SP_reactive_input$SP_test2_lsm_op$TI * 100/SP_reactive_input$SP_test2_lsm_op$NR
                  
                  SP_reactive_input$SP_test2_lsm_diff <- left_join(SP_reactive_input$SP_test2_ly_delist_exclude,SP_reactive_input$SP_test2_lsm_op, by = "PPG")
                  SP_reactive_input$SP_test2_lsm_diff[is.na(SP_reactive_input$SP_test2_lsm_diff)] <- 0
                  SP_reactive_input$SP_test2_lsm_diff_calc <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_lsm_diff$PPG),(SP_reactive_input$SP_test2_lsm_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_lsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_lsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_lsm_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_lsm_diff[,c("GM_NR.x","TI_NR.x")])*100)
                  SP_reactive_input$SP_test2_lsm_diff_calc[is.na(SP_reactive_input$SP_test2_lsm_diff_calc)] <- 0
                }
                
              }else if(!is.null(input$SP_test2_ppg)){
                SP_reactive_input$SP_test2_ly <- SP_reactive_input$SP_opti_const_selected_1[SP_reactive_input$SP_opti_const_selected_1$current_year_date >= input$SP_test4_start_date & SP_reactive_input$SP_opti_const_selected_1$current_year_date <= input$SP_test4_end_date & SP_reactive_input$SP_opti_const_selected_1$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected_1$Brand %in% (input$SP_brand) & SP_reactive_input$SP_opti_const_selected_1$Format %in% input$SP_test2_format & SP_reactive_input$SP_opti_const_selected_1$PPG %in% input$SP_test2_ppg,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                SP_reactive_input$SP_test2_exclude <- SP_reactive_input$SP_opti_exc_brand_1[SP_reactive_input$SP_opti_exc_brand_1$current_year_date >= input$SP_test4_start_date & SP_reactive_input$SP_opti_exc_brand_1$current_year_date <= input$SP_test4_end_date & SP_reactive_input$SP_opti_exc_brand_1$Format %in% input$SP_test2_format & SP_reactive_input$SP_opti_exc_brand_1$PPG %in% input$SP_test2_ppg,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                      "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                SP_reactive_input$SP_test2_delist <- SP_reactive_input$SP_RB_Delist[SP_reactive_input$SP_RB_Delist$Format %in% input$SP_test2_format & SP_reactive_input$SP_RB_Delist$PPG %in% input$SP_test2_ppg,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Units), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Trade_Investment),
                                                                                                                                                                                                                    "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                if(input$SP_opti_op_include_delist == FALSE & input$SP_opti_op_include_exclude == TRUE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_delist$PPG){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_delist[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_delist)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("PPG" = SP_reactive_input$SP_test2_ly$PPG),
                    rbindlist(result_list, fill = TRUE)
                  )
                }else if(input$SP_opti_op_include_delist == FALSE & input$SP_opti_op_include_exclude == FALSE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_delist$PPG){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_delist[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_delist)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("PPG" = SP_reactive_input$SP_test2_ly$PPG),
                    rbindlist(result_list, fill = TRUE)
                  )
                  
                  result_list2 = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly_delist)), function(i){
                    x = SP_reactive_input$SP_test2_ly_delist[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_exclude[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("PPG" = SP_reactive_input$SP_test2_ly_delist$PPG),
                    rbindlist(result_list2, fill = TRUE)
                  )
                  
                }else if(input$SP_opti_op_include_delist == TRUE & input$SP_opti_op_include_exclude == TRUE){
                  SP_reactive_input$SP_test2_ly_delist <- data.frame(SP_reactive_input$SP_test2_ly)
                  
                }else if(input$SP_opti_op_include_delist == TRUE & input$SP_opti_op_include_exclude == FALSE){
                  result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_ly)), function(i){
                    x = SP_reactive_input$SP_test2_ly[i,]
                    if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){
                      diff_vec = as.numeric(x[2:length(x)]) - 
                        as.numeric(SP_reactive_input$SP_test2_exclude[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                      result_dt = data.table(matrix(diff_vec, nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    } else {
                      result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                      setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                      result_dt
                    }
                  })
                  SP_reactive_input$SP_test2_ly_delist <- cbind(
                    data.frame("PPG" = SP_reactive_input$SP_test2_ly$PPG),
                    rbindlist(result_list, fill = TRUE)
                  )
                }
                SP_reactive_input$SP_test2_ly_delist_exclude <- SP_reactive_input$SP_test2_ly_delist
                SP_reactive_input$SP_test2_ly_delist_exclude$GM_NR <- SP_reactive_input$SP_test2_ly_delist_exclude$Gross_Margin * 100/SP_reactive_input$SP_test2_ly_delist_exclude$NR
                SP_reactive_input$SP_test2_ly_delist_exclude$TI_NR <- SP_reactive_input$SP_test2_ly_delist_exclude$TI * 100/SP_reactive_input$SP_test2_ly_delist_exclude$NR
                
                if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                  SP_reactive_input$SP_test2_nonlsm_op <- SP_reactive_input$SP_opti_op_prepared[SP_reactive_input$SP_opti_op_prepared$Week_Ending >= input$SP_test4_start_date & SP_reactive_input$SP_opti_op_prepared$Week_Ending <= input$SP_test4_end_date & SP_reactive_input$SP_opti_op_prepared$Format %in% input$SP_test2_format & SP_reactive_input$SP_opti_op_prepared$PPG %in% input$SP_test2_ppg,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                              "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                  
                  SP_reactive_input$SP_test2_nonlsm_op <- left_join(SP_reactive_input$SP_test2_ly[,c("PPG")],SP_reactive_input$SP_test2_nonlsm_op,by = "PPG")
                  SP_reactive_input$SP_test2_nonlsm_op[is.na(SP_reactive_input$SP_test2_nonlsm_op)] <- 0
                  if(input$SP_opti_op_include_exclude == TRUE){
                    result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_nonlsm_op)), function(i){
                      x = SP_reactive_input$SP_test2_nonlsm_op[i,]
                      if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){
                        sum_vec = as.numeric(x[2:length(x)]) + 
                          as.numeric(SP_reactive_input$SP_test2_exclude[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                        result_dt = data.table(matrix(sum_vec, nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      } else {
                        result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      }
                    })
                    SP_reactive_input$SP_test2_nonlsm_op <- cbind(
                      data.frame("PPG" = SP_reactive_input$SP_test2_nonlsm_op$PPG),
                      rbindlist(result_list, fill = TRUE)
                    )
                  }
                  SP_reactive_input$SP_test2_nonlsm_op$GM_NR <- SP_reactive_input$SP_test2_nonlsm_op$Gross_Margin * 100/SP_reactive_input$SP_test2_nonlsm_op$NR
                  SP_reactive_input$SP_test2_nonlsm_op$TI_NR <- SP_reactive_input$SP_test2_nonlsm_op$TI * 100/SP_reactive_input$SP_test2_nonlsm_op$NR
                  
                  SP_reactive_input$SP_test2_nonlsm_diff <- left_join(SP_reactive_input$SP_test2_ly_delist_exclude,SP_reactive_input$SP_test2_nonlsm_op, by = "PPG")
                  SP_reactive_input$SP_test2_nonlsm_diff[is.na(SP_reactive_input$SP_test2_nonlsm_diff)] <- 0
                  
                  SP_reactive_input$SP_test2_nonlsm_diff_calc <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_nonlsm_diff$PPG),(SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_nonlsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_nonlsm_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_nonlsm_diff[,c("GM_NR.x","TI_NR.x")])*100)
                  SP_reactive_input$SP_test2_nonlsm_diff_calc[is.na(SP_reactive_input$SP_test2_nonlsm_diff_calc)] <- 0
                }
                
                if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
                  SP_reactive_input$SP_test2_lsm_op <- SP_reactive_input$SP_opti_op_prepared_lsm[SP_reactive_input$SP_opti_op_prepared_lsm$Week_Ending >= input$SP_test4_start_date & SP_reactive_input$SP_opti_op_prepared_lsm$Week_Ending <= input$SP_test4_end_date & SP_reactive_input$SP_opti_op_prepared_lsm$Format %in% input$SP_test2_format & SP_reactive_input$SP_opti_op_prepared_lsm$PPG %in% input$SP_test2_ppg,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                               "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                  
                  SP_reactive_input$SP_test2_lsm_op <- left_join(SP_reactive_input$SP_test2_ly[,c("PPG")],SP_reactive_input$SP_test2_lsm_op,by = "PPG")
                  SP_reactive_input$SP_test2_lsm_op[is.na(SP_reactive_input$SP_test2_lsm_op)] <- 0
                  if(input$SP_opti_op_include_exclude == TRUE){
                    result_list = lapply(seq_len(nrow(SP_reactive_input$SP_test2_lsm_op)), function(i){
                      x = SP_reactive_input$SP_test2_lsm_op[i,]
                      if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){
                        sum_vec = as.numeric(x[2:length(x)]) + 
                          as.numeric(SP_reactive_input$SP_test2_exclude[PPG == x[1], c(2:length(SP_reactive_input$SP_test2_exclude)), with = FALSE])
                        result_dt = data.table(matrix(sum_vec, nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      } else {
                        result_dt = data.table(matrix(as.numeric(x[2:length(x)]), nrow = 1, ncol = 7))
                        setnames(result_dt, c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))
                        result_dt
                      }
                    })
                    SP_reactive_input$SP_test2_lsm_op <- cbind(
                      data.frame("PPG" = SP_reactive_input$SP_test2_lsm_op$PPG),
                      rbindlist(result_list, fill = TRUE)
                    )
                  }
                  SP_reactive_input$SP_test2_lsm_op$GM_NR <- SP_reactive_input$SP_test2_lsm_op$Gross_Margin * 100/SP_reactive_input$SP_test2_lsm_op$NR
                  SP_reactive_input$SP_test2_lsm_op$TI_NR <- SP_reactive_input$SP_test2_lsm_op$TI * 100/SP_reactive_input$SP_test2_lsm_op$NR
                  
                  SP_reactive_input$SP_test2_lsm_diff <- left_join(SP_reactive_input$SP_test2_ly_delist_exclude,SP_reactive_input$SP_test2_lsm_op, by = "PPG")
                  SP_reactive_input$SP_test2_lsm_diff[is.na(SP_reactive_input$SP_test2_lsm_diff)] <- 0
                  SP_reactive_input$SP_test2_lsm_diff_calc <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_lsm_diff$PPG),(SP_reactive_input$SP_test2_lsm_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_lsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_lsm_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_lsm_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_lsm_diff[,c("GM_NR.x","TI_NR.x")])*100)
                  SP_reactive_input$SP_test2_lsm_diff_calc[is.na(SP_reactive_input$SP_test2_lsm_diff_calc)] <- 0
                }
                
                #, "UNCR" = sum(UNCR_Total), "OID" = sum(OID_Total), "Retro_Funding" = sum(Retro_Funding_Total), "Display_Cost" = sum(Display_Cost)
                
              }
              if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization","Run Complete Optimization")){
                SP_reactive_input$SP_test2_nonlsm_op[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")] <- SP_reactive_input$SP_test2_nonlsm_op[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")]/10^3
                
                
                setnames(SP_reactive_input$SP_test2_nonlsm_op,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"),c("Scan Net Revenue NonLSM('000 GBP)","Volume Sales NonLSM('000 Units)","Gross Sales NonLSM('000 GBP)","Gross Margin NonLSM('000 GBP)","Trade Investment NonLSM('000 GBP)","GM % NR NonLSM","TI % NR NonLSM"))
                setnames(SP_reactive_input$SP_test2_nonlsm_diff_calc,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y","GM_NR.y","TI_NR.y"),c("Scan Net Revenue(% Diff)","Volume Sales(% Diff)","Gross Sales % Diff","Gross Margin(% Diff)","Trade Investment(% Diff)","GM % NR(% Diff)","TI % NR(% Diff)"))
              }
              
              if(input$SP_opti_run_choice %in% c("Run Complete Optimization","Run LSM constrained Optimization")){
                SP_reactive_input$SP_test2_lsm_op[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")] <- SP_reactive_input$SP_test2_lsm_op[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")]/10^3
                
                setnames(SP_reactive_input$SP_test2_lsm_op,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"),c("Scan Net Revenue LSM('000 GBP)","Volume Sales LSM('000 Units)","Gross Sales LSM('000 GBP)","Gross Margin LSM('000 GBP)","Trade Investment LSM('000 GBP)","GM % NR LSM","TI % NR LSM"))
                setnames(SP_reactive_input$SP_test2_lsm_diff_calc,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y","GM_NR.y","TI_NR.y"),c("Scan Net Revenue(% Diff)","Volume Sales(% Diff)","Gross Sales % Diff","Gross Margin(% Diff)","Trade Investment(% Diff)","GM % NR(% Diff)","TI % NR(% Diff)"))
                
              }
              
              SP_reactive_input$SP_test2_ly_delist_exclude[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")] <- SP_reactive_input$SP_test2_ly_delist_exclude[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")]/10^3
              
              setnames(SP_reactive_input$SP_test2_ly_delist_exclude,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"),c("Scan Net Revenue('000 GBP)","Volume Sales('000 Units)","Gross Sales('000 GBP)","Gross Margin('000 GBP)","Trade Investment('000 GBP)","GM % NR","TI % NR"))
              
            }
          }
        })
      
      removeModal()
      updateTabItems(session, "sidebar_main", selected = "SP_subMenu6")
      if(input$SP_opti_run_choice == "Run Complete Optimization"){
        SP_reactive_input$ppg_budget_const_lsm <- SP_reactive_input$opti_op_ppg_budget_const_lsm[,c("PPG","Status")]
        
        SP_reactive_input$ppg_budget_const <- SP_reactive_input$opti_op_ppg_budget_const[,c("PPG","Status")]
        
        if(any(SP_reactive_input$ppg_budget_const_lsm$Status %in% c("Budget less than Minimum","Budget more than Maximum")) & any(SP_reactive_input$ppg_budget_const$Status %in% c("Budget less than Minimum","Budget more than Maximum"))){
          cat(paste0("Budget criteria not satisfied for both LSM and Unconstrained Optimization","Min Investment violated for PPG's: ", 
                     paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget less than Minimum",]$PPG,
                                  SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget less than Minimum",]$PPG), collapse = ","),"\n",
                     "   Max Investment violated for PPG's: ",
                     paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget more than Maximum",]$PPG,
                                  SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget more than Maximum",]$PPG), collapse = ",")))
          
          sendSweetAlert(session,"Budget criteria not satisfied for both LSM and Unconstrained Optimization",
                         HTML(paste0("Min Investment violated for PPG's: ", 
                                     paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget less than Minimum",]$PPG,
                                                  SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget less than Minimum",]$PPG), collapse = ","),"\n",
                                     "<br> Max Investment violated for PPG's: ",
                                     paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget more than Maximum",]$PPG,
                                                  SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget more than Maximum",]$PPG), collapse = ","))),
                         type = "warning")
        }else if(any(SP_reactive_input$ppg_budget_const_lsm$Status %in% c("Budget less than Minimum","Budget more than Maximum"))){
          cat(paste0("Budget criteria not satisfied for LSM Optimization","Min Investment violated for PPG's: ", 
                     paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                     "   Max Investment violated for PPG's: ",
                     paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget more than Maximum",]$PPG), collapse = ",")))
          
          sendSweetAlert(session,"Budget criteria not satisfied for LSM Optimization",
                         paste0("Min Investment violated for PPG's: ", 
                                paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                                "   Max Investment violated for PPG's: ",
                                paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget more than Maximum",]$PPG), collapse = ",")),
                         
                         
                         type = "warning")
        }else if(any(SP_reactive_input$ppg_budget_const$Status %in% c("Budget less than Minimum","Budget more than Maximum"))){
          cat(paste0("Budget criteria not satisfied for Unconstrained Optimization","Min Investment violated for PPG's: ", 
                     paste(unique(SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                     "   Max Investment violated for PPG's: ",
                     paste(unique(SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget more than Maximum",]$PPG), collapse = ",")))
          
          sendSweetAlert(session,"Budget criteria not satisfied for Unconstrained Optimization",
                         paste0("Min Investment violated for PPG's: ", 
                                paste(unique(SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                                "   Max Investment violated for PPG's: ",
                                paste(unique(SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget more than Maximum",]$PPG), collapse = ",")),
                         
                         
                         type = "warning")
        }
      }else if(input$SP_opti_run_choice == "Run LSM constrained Optimization"){
        SP_reactive_input$ppg_budget_const_lsm <- SP_reactive_input$opti_op_ppg_budget_const_lsm[,c("PPG","Status")]
        
        if(any(SP_reactive_input$ppg_budget_const_lsm$Status %in% c("Budget less than Minimum","Budget more than Maximum"))){
          cat(paste0("Budget criteria not satisfied for LSM Optimization","Min Investment violated for PPG's: ", 
                     paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                     "   Max Investment violated for PPG's: ",
                     paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget more than Maximum",]$PPG), collapse = ",")))
          
          sendSweetAlert(session,"Budget criteria not satisfied for LSM Optimization",
                         paste0("Min Investment violated for PPG's: ", 
                                paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                                "   Max Investment violated for PPG's: ",
                                paste(unique(SP_reactive_input$ppg_budget_const_lsm[SP_reactive_input$ppg_budget_const_lsm$Status == "Budget more than Maximum",]$PPG), collapse = ",")),
                         
                         type = "warning")
        }
      }else if(input$SP_opti_run_choice == "Run Unconstrained Optimization"){
        SP_reactive_input$ppg_budget_const <- SP_reactive_input$opti_op_ppg_budget_const[,c("PPG","Status")]
        
        if(any(SP_reactive_input$ppg_budget_const$Status %in% c("Budget less than Minimum","Budget more than Maximum"))){
          cat(paste0("Budget criteria not satisfied for Unconstrained Optimization", "Min Investment violated for PPG's: ", 
                     paste(unique(SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                     "   Max Investment violated for PPG's: ",
                     paste(unique(SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget more than Maximum",]$PPG), collapse = ",")))
          
          sendSweetAlert(session,"Budget criteria not satisfied for Unconstrained Optimization",
                         paste0("Min Investment violated for PPG's: ", 
                                paste(unique(SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                                "   Max Investment violated for PPG's: ",
                                paste(unique(SP_reactive_input$ppg_budget_const[SP_reactive_input$ppg_budget_const$Status == "Budget more than Maximum",]$PPG), collapse = ",")),
                         
                         
                         type = "warning")
        }
      }
    }
  })
  
  ###RB KPI's -- Last year, LSM, Unconstrained
  output$SP_const1_KPI <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation, "Run Optimization")
    )
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation, id = c("goal","sign"))
    goal_selected <- as.character(unique(SP_opti_op_KPI_data$goal))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc$KPI_opti <- as.character(SP_reactive_input$kpi_map_calc$KPI_opti)
    SP_reactive_input$kpi_map_calc$KPI_calc <- as.character(SP_reactive_input$kpi_map_calc$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$kpi_map_calc,by = c("variable" = "KPI_calc"))
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$SP_TPO_list$opti_const[,c("KPI_Mapping","Constraint Order")],by = c("KPI_model" = "KPI_Mapping"))
    
    #SP_reactive_input$SP_TPO_list$opti_const_shiny
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","min","max","const_order","variable", "Constraint Order")]
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint","const_order","variable", "Constraint Order")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Last Year Value`)
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Last Year Value`)
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$`LSM Change vs LY` > 0, "Negative", "Positive")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$non_lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$`Unconstrained Change vs LY` > 0, "Negative", "Positive")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$`LSM Change vs LY` > 0, "Negative", "Positive")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$non_lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$`Unconstrained Change vs LY` > 0, "Negative", "Positive")
    
    SP_opti_op_KPI_data[,c("LSM Change vs LY","Unconstrained Change vs LY","Min Constraint","Max Constraint")] <- lapply(SP_opti_op_KPI_data[,c("LSM Change vs LY","Unconstrained Change vs LY","Min Constraint","Max Constraint")], function(X) as.character(round(X,2)))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %like% goal_selected,]$`Min Constraint` <- "Goal Selected"
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %like% goal_selected,]$`Max Constraint` <- "Goal Selected"
    #SP_opti_op_KPI_data  <- SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$KPI %like% "ROI"),]
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(1,2,7,8,9,10)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_lsm <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation, "")
    )
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation, id = c("goal","sign"))
    goal_selected <- as.character(unique(SP_opti_op_KPI_data$goal))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc$KPI_opti <- as.character(SP_reactive_input$kpi_map_calc$KPI_opti)
    SP_reactive_input$kpi_map_calc$KPI_calc <- as.character(SP_reactive_input$kpi_map_calc$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$kpi_map_calc,by = c("variable" = "KPI_calc"))
    
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$SP_TPO_list$opti_const[,c("KPI_Mapping","Constraint Order")],by = c("KPI_model" = "KPI_Mapping"))
    
    #SP_reactive_input$SP_TPO_list$opti_const_shiny
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","min","max","const_order","variable", "Constraint Order")]
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint","const_order","variable", "Constraint Order")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Last Year Value`)
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Last Year Value`)
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$`LSM Change vs LY` >= 0, "Negative", "Positive")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$non_lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$`Unconstrained Change vs LY` >= 0, "Negative", "Positive")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$`LSM Change vs LY` >= 0, "Negative", "Positive")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$non_lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$`Unconstrained Change vs LY` >= 0, "Negative", "Positive")
    
    SP_opti_op_KPI_data[,c("LSM Change vs LY","Unconstrained Change vs LY","Min Constraint","Max Constraint")] <- lapply(SP_opti_op_KPI_data[,c("LSM Change vs LY","Unconstrained Change vs LY","Min Constraint","Max Constraint")], function(X) as.character(round(X,2)))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %like% goal_selected,]$`Min Constraint` <- "Goal Selected"
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %like% goal_selected,]$`Max Constraint` <- "Goal Selected"
    #SP_opti_op_KPI_data  <- SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$KPI %like% "ROI"),]
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(0,2,3,4,5,6,8,9,10)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_nonlsm <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation, "")
    )
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation, id = c("goal","sign"))
    goal_selected <- as.character(unique(SP_opti_op_KPI_data$goal))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc$KPI_opti <- as.character(SP_reactive_input$kpi_map_calc$KPI_opti)
    SP_reactive_input$kpi_map_calc$KPI_calc <- as.character(SP_reactive_input$kpi_map_calc$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$kpi_map_calc,by = c("variable" = "KPI_calc"))
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$SP_TPO_list$opti_const[,c("KPI_Mapping","Constraint Order")],by = c("KPI_model" = "KPI_Mapping"))
    
    #SP_reactive_input$SP_TPO_list$opti_const_shiny
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","min","max","const_order","variable", "Constraint Order")]
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint","const_order","variable", "Constraint Order")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value","Min Constraint","Max Constraint")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Last Year Value`)
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Last Year Value`)
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$`LSM Change vs LY` >= 0, "Negative", "Positive")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$non_lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%NIS"),]$`Unconstrained Change vs LY` >= 0, "Negative", "Positive")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$`LSM Change vs LY` >= 0, "Negative", "Positive")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$non_lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("spend_%"),]$`Unconstrained Change vs LY` >= 0, "Negative", "Positive")
    
    SP_opti_op_KPI_data[,c("LSM Change vs LY","Unconstrained Change vs LY","Min Constraint","Max Constraint")] <- lapply(SP_opti_op_KPI_data[,c("LSM Change vs LY","Unconstrained Change vs LY","Min Constraint","Max Constraint")], function(X) as.character(round(X,2)))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %like% goal_selected,]$`Min Constraint` <- "Goal Selected"
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %like% goal_selected,]$`Max Constraint` <- "Goal Selected"
    #SP_opti_op_KPI_data  <- SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$KPI %like% "ROI"),]
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(0,1,3,4,5,6,7,9,10)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  ###Retailer KPI's RB
  output$SP_const1_KPI_ret <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation_ret, "Run Optimization")
    )
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_ret, id = c("goal","sign"))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc_ret$KPI_calc <- as.character(SP_reactive_input$kpi_map_calc_ret$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$kpi_map_calc_ret,by = c("variable" = "KPI_calc"))
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","const_order","variable")]
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","const_order","variable")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`LSM Change vs LY`,2))
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`Unconstrained Change vs LY`,2))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(1,2,4,5,6,7)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_ret_lsm <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation_ret, "Run Optimization")
    )
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_ret, id = c("goal","sign"))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc_ret$KPI_calc <- as.character(SP_reactive_input$kpi_map_calc_ret$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$kpi_map_calc_ret,by = c("variable" = "KPI_calc"))
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","const_order","variable")]
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","const_order","variable")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`LSM Change vs LY`,2))
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`Unconstrained Change vs LY`,2))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(0,2,3,5,6,7)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_ret_nonlsm <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation_ret, "Run Optimization")
    )
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_ret, id = c("goal","sign"))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc_ret$KPI_calc <- as.character(SP_reactive_input$kpi_map_calc_ret$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$kpi_map_calc_ret,by = c("variable" = "KPI_calc"))
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","const_order","variable")]
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","const_order","variable")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`LSM Change vs LY`,2))
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`Unconstrained Change vs LY`,2))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(0,1,3,4,6,7)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_cat_ret <- renderDataTable({
    validate(
      need(SP_reactive_input$KPI_calculation_ret, "Run Optimization")
    )
    
    if(SP_reactive_input$opti_format_input == "ALL"){
      competition_display_calc <- sum(isolate(SP_reactive_input$competition_display)$Display_Cost_Comp)/10^6
      competition_KPI <- isolate(SP_reactive_input$competition_KPI)
    }else{
      competition_display_calc <- sum(isolate(SP_reactive_input$competition_display[SP_reactive_input$competition_display$FORMAT == SP_reactive_input$opti_format_input,])$Display_Cost_Comp)/10^6
      competition_KPI <- isolate(SP_reactive_input$competition_KPI[SP_reactive_input$competition_KPI$FORMAT == SP_reactive_input$opti_format_input,])
    }
    competition_KPI_calc <- data.frame("KPI" = c("RSV","COGS","CPD","fixed","FM","FM%","BM","BM%"), "comp_value" = c(sum(competition_KPI$Retailer_Revenue_Comp)/10^6, sum(competition_KPI$Net_Cost_Unit_Comp*competition_KPI$Units)/10^6,sum(competition_KPI$Retro_Fund_Total_Comp)/10^6,competition_display_calc/10^6,sum(competition_KPI$FM_Abs_Comp)/10^6,
                                                                                                                     sum(competition_KPI$FM_Abs_Comp)/sum(competition_KPI$Retailer_Revenue_Comp/(1+competition_KPI$VAT)),(sum(competition_KPI$FM_Abs_Comp) + competition_display_calc)/10^6,(sum(competition_KPI$FM_Abs_Comp) + competition_display_calc)/sum(competition_KPI$Retailer_Revenue_Comp/(1+competition_KPI$VAT))))
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_ret, id = c("goal","sign"))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc_ret$KPI_calc <- as.character(isolate(SP_reactive_input$kpi_map_calc_ret)$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,isolate(SP_reactive_input$kpi_map_calc_ret),by = c("variable" = "KPI_calc"))
    
    competition_KPI_calc$KPI <- as.character(competition_KPI_calc$KPI)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,competition_KPI_calc,by = c("variable" = "KPI"))
    SP_opti_op_KPI_data$lsm <- SP_opti_op_KPI_data$lsm + SP_opti_op_KPI_data$comp_value
    SP_opti_op_KPI_data$nonlsm <- SP_opti_op_KPI_data$nonlsm + SP_opti_op_KPI_data$comp_value
    SP_opti_op_KPI_data$ly <- SP_opti_op_KPI_data$ly + SP_opti_op_KPI_data$comp_value
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "BM%",c("lsm","ly","nonlsm")] <- SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "BM",c("lsm","ly","nonlsm")] * 100/(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "RSV",c("lsm","ly","nonlsm")]/1.2)
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "FM%",c("lsm","ly","nonlsm")] <- SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "FM",c("lsm","ly","nonlsm")] * 100/(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "RSV",c("lsm","ly","nonlsm")]/1.2)
    
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","const_order","variable")]
    
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","const_order","variable")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`LSM Change vs LY`,2))
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`Unconstrained Change vs LY`,2))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(1,2,4,5,6,7)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_cat_ret_lsm <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation_ret, "Run Optimization")
    )
    
    if(SP_reactive_input$opti_format_input == "ALL"){
      competition_display_calc <- sum(isolate(SP_reactive_input$competition_display)$Display_Cost_Comp)/10^6
      competition_KPI <- isolate(SP_reactive_input$competition_KPI)
    }else{
      competition_display_calc <- sum(isolate(SP_reactive_input$competition_display[SP_reactive_input$competition_display$FORMAT == SP_reactive_input$opti_format_input,])$Display_Cost_Comp)/10^6
      competition_KPI <- isolate(SP_reactive_input$competition_KPI[SP_reactive_input$competition_KPI$FORMAT == SP_reactive_input$opti_format_input,])
    }
    competition_KPI_calc <- data.frame("KPI" = c("RSV","COGS","CPD","fixed","FM","FM%","BM","BM%"), "comp_value" = c(sum(competition_KPI$Retailer_Revenue_Comp)/10^6, sum(competition_KPI$Net_Cost_Unit_Comp*competition_KPI$Units)/10^6,sum(competition_KPI$Retro_Fund_Total_Comp)/10^6,competition_display_calc/10^6,sum(competition_KPI$FM_Abs_Comp)/10^6,
                                                                                                                     sum(competition_KPI$FM_Abs_Comp)/sum(competition_KPI$Retailer_Revenue_Comp/(1+competition_KPI$VAT)),(sum(competition_KPI$FM_Abs_Comp) + competition_display_calc)/10^6,(sum(competition_KPI$FM_Abs_Comp) + competition_display_calc)/sum(competition_KPI$Retailer_Revenue_Comp/(1+competition_KPI$VAT))))
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_ret, id = c("goal","sign"))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc_ret$KPI_calc <- as.character(isolate(SP_reactive_input$kpi_map_calc_ret)$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,isolate(SP_reactive_input$kpi_map_calc_ret),by = c("variable" = "KPI_calc"))
    
    competition_KPI_calc$KPI <- as.character(competition_KPI_calc$KPI)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,competition_KPI_calc,by = c("variable" = "KPI"))
    SP_opti_op_KPI_data$lsm <- SP_opti_op_KPI_data$lsm + SP_opti_op_KPI_data$comp_value
    SP_opti_op_KPI_data$nonlsm <- SP_opti_op_KPI_data$nonlsm + SP_opti_op_KPI_data$comp_value
    SP_opti_op_KPI_data$ly <- SP_opti_op_KPI_data$ly + SP_opti_op_KPI_data$comp_value
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "BM%",c("lsm","ly","nonlsm")] <- SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "BM",c("lsm","ly","nonlsm")] * 100/(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "RSV",c("lsm","ly","nonlsm")]/1.2)
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "FM%",c("lsm","ly","nonlsm")] <- SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "FM",c("lsm","ly","nonlsm")] * 100/(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "RSV",c("lsm","ly","nonlsm")]/1.2)
    
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","const_order","variable")]
    
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","const_order","variable")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`LSM Change vs LY`,2))
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`Unconstrained Change vs LY`,2))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(0,2,3,5,6,7)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_cat_ret_nonlsm <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation_ret, "Run Optimization")
    )
    
    if(SP_reactive_input$opti_format_input == "ALL"){
      competition_display_calc <- sum(isolate(SP_reactive_input$competition_display)$Display_Cost_Comp)/10^6
      competition_KPI <- isolate(SP_reactive_input$competition_KPI)
    }else{
      competition_display_calc <- sum(isolate(SP_reactive_input$competition_display[SP_reactive_input$competition_display$FORMAT == SP_reactive_input$opti_format_input,])$Display_Cost_Comp)/10^6
      competition_KPI <- isolate(SP_reactive_input$competition_KPI[SP_reactive_input$competition_KPI$FORMAT == SP_reactive_input$opti_format_input,])
    }
    competition_KPI_calc <- data.frame("KPI" = c("RSV","COGS","CPD","fixed","FM","FM%","BM","BM%"), "comp_value" = c(sum(competition_KPI$Retailer_Revenue_Comp)/10^6, sum(competition_KPI$Net_Cost_Unit_Comp*competition_KPI$Units)/10^6,sum(competition_KPI$Retro_Fund_Total_Comp)/10^6,competition_display_calc/10^6,sum(competition_KPI$FM_Abs_Comp)/10^6,
                                                                                                                     sum(competition_KPI$FM_Abs_Comp)/sum(competition_KPI$Retailer_Revenue_Comp/(1+competition_KPI$VAT)),(sum(competition_KPI$FM_Abs_Comp) + competition_display_calc)/10^6,(sum(competition_KPI$FM_Abs_Comp) + competition_display_calc)/sum(competition_KPI$Retailer_Revenue_Comp/(1+competition_KPI$VAT))))
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_ret, id = c("goal","sign"))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(as.data.table(SP_opti_op_KPI_data),goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc_ret$KPI_calc <- as.character(isolate(SP_reactive_input$kpi_map_calc_ret)$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,isolate(SP_reactive_input$kpi_map_calc_ret),by = c("variable" = "KPI_calc"))
    
    competition_KPI_calc$KPI <- as.character(competition_KPI_calc$KPI)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,competition_KPI_calc,by = c("variable" = "KPI"))
    SP_opti_op_KPI_data$lsm <- SP_opti_op_KPI_data$lsm + SP_opti_op_KPI_data$comp_value
    SP_opti_op_KPI_data$nonlsm <- SP_opti_op_KPI_data$nonlsm + SP_opti_op_KPI_data$comp_value
    SP_opti_op_KPI_data$ly <- SP_opti_op_KPI_data$ly + SP_opti_op_KPI_data$comp_value
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "BM%",c("lsm","ly","nonlsm")] <- SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "BM",c("lsm","ly","nonlsm")] * 100/(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "RSV",c("lsm","ly","nonlsm")]/1.2)
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "FM%",c("lsm","ly","nonlsm")] <- SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "FM",c("lsm","ly","nonlsm")] * 100/(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "RSV",c("lsm","ly","nonlsm")]/1.2)
    
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","lsm","nonlsm","ly","const_order","variable")]
    
    names(SP_opti_op_KPI_data) <- c("KPI","LSM Planned","Unconstrained Planned","Last Year Value","const_order","variable")
    SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")] <- lapply(SP_opti_op_KPI_data[,c("LSM Planned","Unconstrained Planned","Last Year Value")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- (SP_opti_op_KPI_data$`LSM Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data$`Unconstrained Planned` - SP_opti_op_KPI_data$`Last Year Value`) * 100/SP_opti_op_KPI_data$`Last Year Value`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Planned` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Last Year Value`) * 100
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`lsm_sign` <- ifelse(SP_opti_op_KPI_data$`LSM Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    
    SP_opti_op_KPI_data$`LSM Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`LSM Change vs LY`,2))
    SP_opti_op_KPI_data$`Unconstrained Change vs LY` <- as.character(round(SP_opti_op_KPI_data$`Unconstrained Change vs LY`,2))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`LSM Change vs LY`,"%")
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Unconstrained Change vs LY`,"%")
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`LSM Change vs LY`,"bps")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Unconstrained Change vs LY`,"bps")
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(0,1,3,4,6,7)),list(className = 'dt-center', targets = 0:7)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("LSM Change vs LY","lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_graph_1 <- renderPlotly({
    validate(
      need(SP_reactive_input$SP_graph_1_ip,"Run Unconstrained Optimization")
    )
    plot_ly(SP_reactive_input$SP_graph_1_ip,x=~Week_Ending,y=~Promo_Price,name = "Promo Price",type = "scatter", mode = "lines",
            hovertext = round(SP_reactive_input$SP_graph_1_ip$Total_Sales,2),hoverinfo = 'text') %>%
      add_trace(y=~Total_Sales,name = "Total Sales",type = "scatter", mode = "lines",
                hovertext = round(SP_reactive_input$SP_graph_1_ip$Promo_Price,2),hoverinfo = 'text',yaxis = 'y2') %>%
      layout(margin=list(r=50),xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(side = 'left',title = "Promo Price", zeroline = FALSE,showgrid = FALSE),yaxis2 = list(side = 'right', overlaying = "y",title = "Volume Sales(Units)", zeroline = FALSE,showgrid = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
  })
  
  output$SP_graph_2 <- renderPlotly({
    validate(
      need(SP_reactive_input$SP_graph_1_ip,"Run Unconstrained Optimization")
    )
    plot_ly(SP_reactive_input$SP_graph_1_ip,x=~Week_Ending,y=~TPR_Flag,color=~Event,type='bar') %>%
      layout(xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(title = "", zeroline = FALSE,showgrid = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
  })
  
  output$SP_graph_1_lsm <- renderPlotly({
    
    validate(
      need(SP_reactive_input$SP_graph_1_lsm_ip,"Run LSM Constrained Optimization")
    )
    
    plot_ly(SP_reactive_input$SP_graph_1_lsm_ip,x=~Week_Ending,y=~Promo_Price,name = "Promo Price",type = "scatter", mode = "lines",
            hovertext = round(SP_reactive_input$SP_graph_1_lsm_ip$Total_Sales,2),hoverinfo = 'text') %>%
      add_trace(y=~Total_Sales,name = "Total Sales",type = "scatter", mode = "lines",
                hovertext = round(SP_reactive_input$SP_graph_1_lsm_ip$Promo_Price,2),hoverinfo = 'text',yaxis = 'y2') %>%
      layout(margin=list(r=50),xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(side = 'left',title = "Promo Price", zeroline = FALSE,showgrid = FALSE),yaxis2 = list(side = 'right', overlaying = "y",title = "Volume Sales(Units)", zeroline = FALSE,showgrid = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
  })
  
  output$SP_graph_2_lsm <- renderPlotly({
    validate(
      need(SP_reactive_input$SP_graph_1_lsm_ip,"Run LSM Constrained Optimization")
    )
    #
    #SP_reactive_input$SP_graph_1_lsm_ip[SP_reactive_input$SP_graph_1_lsm_ip$Event == "No Promo"]$Event <- ""
    plot_ly(SP_reactive_input$SP_graph_1_lsm_ip,x=~Week_Ending,y=~TPR_Flag,color=~Event,type='bar') %>%
      layout(xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(title = "", zeroline = FALSE,showgrid = FALSE),legend = list(orientation = 'h',x = 0, y = 50,trace = TRUE)) %>% config(displayModeBar = FALSE)
  })
  
  #intersect(SP_reactive_input$SP_test2_ly$Format,SP_reactive_input$SP_test2_lsm_op$Format)
  output$SP_graph_3_ly <- renderDataTable({
    validate(
      need(SP_reactive_input$SP_test2_ly_delist_exclude,"Run Optimization")
    )
    
    empty_df <- data.frame(matrix(rep("",nrow(SP_reactive_input$SP_test2_ly_delist_exclude)),ncol = 1))
    names(empty_df) <- ""
    
    SP_test2_ly_delist_exclude <- cbind(SP_reactive_input$SP_test2_ly_delist_exclude,empty_df,SP_reactive_input$SP_test2_lsm_op[,c(2:length(SP_reactive_input$SP_test2_lsm_op))],
                                        empty_df,SP_reactive_input$SP_test2_lsm_diff_calc[,c(2:length(SP_reactive_input$SP_test2_lsm_diff_calc))])
    
    sketch = htmltools::withTags(table(
      class = 'cell-border compact hover',
      thead(
        tr(
          th(colspan = 1, 'Product Level'),
          th(colspan = 8, 'Last Year'),
          th(colspan = 8, 'LSM Constrained'),
          th(colspan = 8, 'LSM Change vs LY')
        ),
        tr(
          lapply(
            c(names(SP_test2_ly_delist_exclude)[1],"Scan Net Revenue('000 GBP)","Volume Sales('000 Units)","Gross Sales('000 GBP)","Gross Margin('000 GBP)","Trade Investment('000 GBP)","GM % NR","TI % NR","Break",
              "Scan Net Revenue('000 GBP)","Volume Sales('000 Units)","Gross Sales('000 GBP)","Gross Margin('000 GBP)","Trade Investment('000 GBP)","GM % NR","TI % NR","Break",
              "Scan Net Revenue(%)","Volume Sales(%)","Gross Sales(%)","Gross Margin(%)","Trade Investment(%)","GM % NR(bps)","TI % NR(bps)"),
            th
          )
        )
      )
    ))
    
    datatable(SP_test2_ly_delist_exclude,container = sketch,class="cell-border compact hover",extensions = c('FixedColumns','Buttons'),
              options = list(
                buttons = list('copy', list(extend = 'csv',filename = 'Format Level LSM'),
                               list(extend = 'excel', filename = 'Format Level LSM')),
                columnDefs=list(list(className = 'dt-center', targets = 0:(length(SP_test2_ly_delist_exclude)-1)),list(width = '200px',targets=c(0))),
                paging = FALSE,
                searching = FALSE,
                dom = 'Bt',
                ordering = F,
                scrollX = TRUE,
                scrollY = "260px",
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      formatStyle(names(SP_test2_ly_delist_exclude),textAlign = 'center') %>%
      formatStyle(1, backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle(c("Scan Net Revenue(% Diff)","Volume Sales(% Diff)","Gross Sales % Diff","Gross Margin(% Diff)","GM % NR(% Diff)"),
                  background = styleInterval(c(0),c("#ff4343","#84c343"))) %>%
      formatStyle(c("Trade Investment(% Diff)","TI % NR(% Diff)"),
                  background = styleInterval(c(0),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_test2_ly_delist_exclude,is.numeric)),digits = 2)
  })
  
  output$SP_graph_4_ly <- renderDataTable({
    validate(
      need(SP_reactive_input$SP_test2_ly_delist_exclude,"Run Optimization")
    )
    empty_df <- data.frame(matrix(rep("",nrow(SP_reactive_input$SP_test2_ly_delist_exclude)),ncol = 1))
    names(empty_df) <- ""
    
    SP_test2_ly_delist_exclude <- cbind(SP_reactive_input$SP_test2_ly_delist_exclude,empty_df,SP_reactive_input$SP_test2_nonlsm_op[,c(2:length(SP_reactive_input$SP_test2_nonlsm_op))],
                                        empty_df,SP_reactive_input$SP_test2_nonlsm_diff_calc[,c(2:length(SP_reactive_input$SP_test2_nonlsm_diff_calc))])
    sketch = htmltools::withTags(table(
      class = 'cell-border compact hover',
      thead(
        tr(
          th(colspan = 1, 'Product Level'),
          th(colspan = 8, 'Last Year'),
          th(colspan = 8, 'Unconstrained'),
          th(colspan = 8, 'Unconstrained Change vs LY')
        ),
        tr(
          lapply(
            c(names(SP_test2_ly_delist_exclude)[1],"Scan Net Revenue('000 GBP)","Volume Sales('000 Units)","Gross Sales('000 GBP)","Gross Margin('000 GBP)","Trade Investment('000 GBP)","GM % NR","TI % NR","Break",
              "Scan Net Revenue('000 GBP)","Volume Sales('000 Units)","Gross Sales('000 GBP)","Gross Margin('000 GBP)","Trade Investment('000 GBP)","GM % NR","TI % NR","Break",
              "Scan Net Revenue(%)","Volume Sales(%)","Gross Sales(%)","Gross Margin(%)","Trade Investment(%)","GM % NR(bps)","TI % NR(bps)"),
            th
          )
        )
      )
    ))
    
    datatable(SP_test2_ly_delist_exclude,container = sketch,class="cell-border compact hover",extensions = c('Buttons'),
              options = list(
                buttons = list('copy', list(extend = 'csv',filename = 'Format Level Unconstrained'),
                               list(extend = 'excel', filename = 'Format Level Unconstrained')),
                #columnDefs=list(list(className = 'dt-center', targets = 0:(length(SP_test2_ly_delist_exclude)-1)),list(width = '200px',targets=c(0))),
                paging = FALSE,
                searching = FALSE,
                dom = 'Bt',
                ordering = F,
                scrollX = TRUE,
                scrollY = "260px",
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      formatStyle(names(SP_test2_ly_delist_exclude),textAlign = 'center') %>%
      formatStyle(1, backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle(c("Scan Net Revenue(% Diff)","Volume Sales(% Diff)","Gross Sales % Diff","Gross Margin(% Diff)","GM % NR(% Diff)"),
                  background = styleInterval(c(0),c("#ff4343","#84c343"))) %>%
      formatStyle(c("Trade Investment(% Diff)","TI % NR(% Diff)"),
                  background = styleInterval(c(0),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_test2_ly_delist_exclude,is.numeric)),digits = 2)
  })
  
  output$SP_opti_op_top_panel_download <- downloadHandler(
    filename = function() {
      paste("Optimization Output.xlsx")
    },
    content = function(file) {
      write.xlsx(SP_reactive_input$optimizer_op_download, file,row.names = FALSE)
    }
  )
  
  observeEvent({input$SP_opti_op_include_delist
    input$SP_opti_op_include_exclude},{
      validate(
        need(SP_reactive_input$SP_TPO_list,"Run Optimization")
        #need(SP_reactive_input$SP_opti_op_prepared_lsm, "Run Optimization"),
        #need(SP_reactive_input$SP_opti_op_prepared, "Run Optimization"),
        #need(SP_reactive_input$SP_RB_Delist, "No Delist SKUs and hence KPI's are same")
      )
      SP_reactive_input$KPI_calculation <- KPI_calc(SP_reactive_input$SP_TPO_list,SP_reactive_input$SP_opti_op_prepared_lsm,SP_reactive_input$SP_opti_op_prepared,SP_reactive_input$SP_opti_exc_brand_lsm,SP_reactive_input$SP_opti_exc_brand,SP_reactive_input$SP_opti_const_grouped,SP_reactive_input$SP_RB_Delist,input$SP_opti_op_include_delist,input$SP_opti_op_include_exclude,input$SP_opti_ROI_selection)
      if(is.null(SP_reactive_input$SP_opti_op_prepared_lsm)){
        lsm_kpi_calc_input <- data.frame()
      }else{
        lsm_kpi_calc_input <- KPI_calc_input(SP_reactive_input$SP_opti_op_prepared_lsm,input$SP_opti_ROI_selection)
      }
      
      if(is.null(SP_reactive_input$SP_opti_op_prepared)){
        nonlsm_kpi_calc_input <- data.frame()
      }else{
        nonlsm_kpi_calc_input <- KPI_calc_input(SP_reactive_input$SP_opti_op_prepared,input$SP_opti_ROI_selection)
      }
      SP_reactive_input$KPI_calculation_ret <- KPI_calc_ret(SP_reactive_input$SP_TPO_list,lsm_kpi_calc_input,nonlsm_kpi_calc_input,SP_reactive_input$SP_opti_exc_brand_lsm,SP_reactive_input$SP_opti_exc_brand,SP_reactive_input$SP_opti_const_grouped,SP_reactive_input$SP_RB_Delist,input$SP_opti_op_include_delist,input$SP_opti_op_include_exclude,input$SP_opti_ROI_selection)
    })
  
  ####Unconstrained Calendar
  
  observeEvent(input$SP_opti_cal_nonLSM_format_selection,{
    toggle("SP_opti_cal_legend_nonLSM",
           condition = (input$SP_opti_cal_nonLSM_format_selection != "ROI Effectiveness"))
    
    output$SP_opti_cal_nonLSM <- renderDataTable({
      validate(
        need(SP_reactive_input$opti_cal_dcast,"Run Unconstrained Optimization")
      )
      #  
      
      clrs_ROI <- c(round(seq(255,132,
                              length.out = (length(SP_reactive_input$brks_ROI_nonLSM) + 1)), 0) %>%
                      {paste0("rgb(", ., ",192,67)")})
      
      # use promo_cal_structure_output(data) that now returns slot header
      slot_ids <- sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`))   # <‑ NEW
      
      sketch = htmltools::withTags(table(
        class = 'cell-border compact hover',
        thead(
          tr(
            HTML(promo_cal_structure_output(SP_reactive_input$opti_cal_filtered)[[1]])
          ),
          tr(
            lapply(
              # second header row: left 5 id columns + one header per slot number
              c("Format","PPG","PPG Description","RSP","LSM Floor Price", slot_ids),  # <‑ CHANGED
              th
            )
          )
        )
      ))
      
      if(input$SP_opti_cal_nonLSM_format_selection == "Promotion Type"){
        datatable(
          SP_reactive_input$opti_cal_dcast,
          container = sketch,
          class = "cell-border compact hover",
          extensions = c("FixedColumns","Buttons"),
          selection = list(target="cell", mode="single"),
          options = list(
            order = list(0, "asc"),
            buttons = list(
              "copy",
              list(extend = "csv",   filename = "Optimized Unconstrained Calendar"),
              list(extend = "excel", filename = "Optimized Unconstrained Calendar")
            ),
            columnDefs = list(
              list(visible = FALSE,
                   targets = grep("Event|ROI", names(SP_reactive_input$opti_cal_dcast)) - 1),
              list(width = "70px", targets = c(0,3)),
              list(width = "30px", targets = c(1,2,4)),
              list(width = "10px", targets = slot_ids + 4)  # <‑ CHANGED: use Week No (slots)
            ),
            fixedColumns = list(leftColumns = 5),
            paging   = FALSE,
            searching = FALSE,
            dom      = "t",
            bSort    = FALSE,
            scrollX  = TRUE,
            scrollY  = "400px"
          ),
          rownames = FALSE
        ) %>%
          formatStyle(
            as.character(seq_along(slot_ids)),
            paste0("Event_", slot_ids),
            background = styleEqual(
              c("No Promo","Shelf Promotion","Display Feature Promotion","Feature","Display Feature"),
              c("white",  "#0099DC", "#E42E92","#90EE90","#FFDAB9")
            ),
            color = styleEqual(
              c("No Promo","Shelf Promotion","Display Feature Promotion","Feature","Display Feature"),
              c("white","white","white","white","white")
            )
          ) %>%
          formatStyle(0, target = "row", lineHeight = "85%") %>%
          formatRound(
            as.character(c("RSP_Unit", seq_along(slot_ids))),
            digits = 2
          ) %>%
          formatStyle(as.character(seq_along(slot_ids)), cursor = "pointer")
        
      } else if (input$SP_opti_cal_nonLSM_format_selection == "ROI Effectiveness") {
        
        datatable(
          SP_reactive_input$opti_cal_dcast_ROI,
          container = sketch,
          class = "cell-border compact hover",
          extensions = c("FixedColumns","Buttons"),
          selection = list(target="cell", mode="single"),
          options = list(
            order = list(0, "asc"),
            buttons = list(
              "copy",
              list(extend = "csv",   filename = "Optimized Unconstrained Calendar ROI"),
              list(extend = "excel", filename = "Optimized Unconstrained Calendar ROI")
            ),
            columnDefs = list(
              list(visible = FALSE,
                   targets = grep("Event_|Price_", names(SP_reactive_input$opti_cal_dcast_ROI)) - 1),
              list(width = "40px", targets = c(0,3)),
              list(width = "30px", targets = c(1,2,4)),
              list(width = "10px", targets = slot_ids + 4)   # <‑ CHANGED
            ),
            fixedColumns = list(leftColumns = 5),
            paging   = FALSE,
            searching = FALSE,
            dom      = "t",
            bSort    = FALSE,
            scrollX  = TRUE,
            scrollY  = "400px"
          ),
          rownames = FALSE
        ) %>%
          formatStyle(
            as.character(seq_along(slot_ids)),
            background = styleInterval(
              c(-99, SP_reactive_input$brks_ROI_nonLSM),
              c("white","red", clrs_ROI[-1])
            )
          ) %>%
          formatStyle(
            as.character(seq_along(slot_ids)),
            color = styleEqual(-100, c("white"))
          ) %>%
          formatStyle(0, target = "row", lineHeight = "85%") %>%
          formatRound(
            as.character(c("RSP_Unit", seq_along(slot_ids))),
            digits = 2
          ) %>%
          formatStyle(as.character(seq_along(slot_ids)), cursor = "pointer")
      }
    })
  })
  ####LSM Calendar
  observeEvent(input$SP_opti_cal_LSM_format_selection,{
    toggle("SP_opti_cal_legend_LSM",condition = (input$SP_opti_cal_LSM_format_selection != "ROI Effectiveness"))
    
    output$SP_opti_cal_LSM <- renderDataTable({
      validate(
        need(SP_reactive_input$opti_cal_dcast_lsm,"Run LSM Constrained Optimization")
      )
      
      #SP_reactive_input$cal_selection <- input$SP_opti_cal_LSM_selection
      clrs_ROI <- c(round(seq(255,132, length.out = (length(SP_reactive_input$brks_ROI_LSM) + 1)), 0) %>% {paste0("rgb(", ., ",192,67)")})
      sketch = htmltools::withTags(table(
        class = 'cell-border compact hover',
        thead(
          tr(
            # th(colspan = 5, 'Month'),
            # th(colspan = 5, 'January'),
            # th(colspan = 4, 'February'),
            # th(colspan = 5, 'March'),
            # th(colspan = 4, 'April'),
            # th(colspan = 4, 'May'),
            # th(colspan = 5, 'June'),
            # th(colspan = 4, 'July'),
            # th(colspan = 4, 'August'),
            # th(colspan = 5, 'September'),
            # th(colspan = 4, 'October'),
            # th(colspan = 4, 'November'),
            # th(colspan = 5, 'December')
            
            HTML(promo_cal_structure_output(SP_reactive_input$opti_cal_filtered_lsm)[[1]])
          ),
          tr(
            lapply(
              c("Format","PPG","PPG Description","RSP","LSM Floor Price",c(1:length(unique(SP_reactive_input$opti_cal_filtered_lsm$Week_Ending)))),
              th
            )
          )
        )
      ))
      if(input$SP_opti_cal_LSM_format_selection == "Promotion Type"){
        
        datatable(SP_reactive_input$opti_cal_dcast_lsm,container=sketch,class="cell-border compact hover",extensions = c('FixedColumns','Buttons'),selection =list(target='cell',mode="single"),
                  #selection=list(mode="single", target="cell"),
                  options = list(
                    order = list(0, 'asc'), 
                    buttons = list('copy', list(extend = 'csv',filename = 'Optimized LSM Calendar'),
                                   list(extend = 'excel', filename = 'Optimized LSM Calendar')),
                    # list(extend = 'pdf',
                    #      pageSize = 'A4',
                    #      orientation = 'landscape',
                    #      filename = 'Optimized LSM Calendar'), 'print'),
                    columnDefs=list(list(visible=FALSE, targets=grep("Event|ROI",names(SP_reactive_input$opti_cal_dcast_lsm))-1),list(width = '70px',targets=c(0,3)), list(width = '30px',targets=c(1,2,4)),list(width = '10px',targets=sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)) + 4)),
                    fixedColumns = list(leftColumns = 5),
                    paging = FALSE,
                    searching = FALSE,
                    dom = 't',
                    bSort=FALSE,
                    scrollX = TRUE,
                    scrollY = "400px"
                  ),rownames = F) %>% 
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))),paste0("Event_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`))),
                      background = styleEqual(c("No Promo","Shelf Promotion","Display Feature Promotion","Feature","Display Feature"), 
                                              c('white','#0099DC', '#E42E92','#90EE90','#FFDAB9')),
                      color = styleEqual(c("No Promo","Shelf Promotion","Display Feature Promotion","Feature","Display Feature"), 
                                         c('white','white', 'white','white','white'))) %>%
          formatStyle(0,target = 'row',lineHeight='85%') %>%
          formatRound(as.character(c("RSP_Unit",1:length(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))), digits=2) %>%
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))),cursor = 'pointer')
      }else if(input$SP_opti_cal_LSM_format_selection == "ROI Effectiveness"){
        
        datatable(SP_reactive_input$opti_cal_dcast_lsm_ROI,container=sketch,class="cell-border compact hover",extensions = c('FixedColumns','Buttons'),selection =list(target='cell',mode="single"),
                  #selection=list(mode="single", target="cell"),
                  options = list(
                    order = list(0, 'asc'), 
                    buttons = list('copy', list(extend = 'csv',filename = 'Optimized LSM Calendar ROI'),
                                   list(extend = 'excel', filename = 'Optimized LSM Calendar ROI')),
                    # list(extend = 'pdf',
                    #      pageSize = 'A4',
                    #      orientation = 'landscape',
                    #      filename = 'Optimized LSM Calendar ROI'), 'print'),
                    columnDefs=list(list(visible=FALSE, targets=grep("Event_|Price_",names(SP_reactive_input$opti_cal_dcast_lsm_ROI))-1),list(width = '70px',targets=c(0,3)), list(width = '30px',targets=c(1,2,4)),list(width = '10px',targets=sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)) + 4)),
                    fixedColumns = list(leftColumns = 5),
                    paging = FALSE,
                    searching = FALSE,
                    dom = 't',
                    bSort=FALSE,
                    scrollX = TRUE,
                    scrollY = "400px"
                  ),rownames = F) %>% 
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))),
                      background = styleInterval(c(-99,SP_reactive_input$brks_ROI_LSM),c("white","red",clrs_ROI[-1]))) %>%
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))),
                      color = styleEqual(-100,c("white"))) %>%
          formatStyle(0,target = 'row',lineHeight='85%') %>%
          formatRound(as.character(c("RSP_Unit",1:length(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))), digits=2) %>%
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))),cursor = 'pointer')
      }
    })
  })
  
  ###Excluded PPG list
  output$SP_opti_financial_table <- renderDataTable({
    datatable(SP_reactive_input$SP_RB_Financial_PPG_1,class="cell-border stripe",extensions = c('FixedColumns'),
              options = list(
                lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
                buttons = list('copy', list(extend = 'csv',filename = 'Out of Scope Products(Annual)'),
                               list(extend = 'excel', filename = 'Out of Scope Products(Annual)'),
                               list(extend = 'pdf',
                                    pageSize = 'A4',
                                    orientation = 'landscape',
                                    filename = 'Out of Scope Products(Annual)'), 'print'),
                pageLength = 10,
                dom = "Blftrip",
                scrollX = T,
                scrollY = "300px"
              ),rownames = F) %>%formatStyle(names(SP_reactive_input$SP_RB_Financial_PPG_1),textAlign = 'center')
  })
  
  #Saving LSM Output
  observeEvent(input$SP_opti_save_LSM,{
    ###Saving TPO ID table
    
    if(input$SP_opti_save_name_LSM != ""){
      if(file.exists(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",input$SP_opti_save_name_LSM,".RData"))){
        confirmSweetAlert(session,"confirm_save_lsm",title = paste0(input$SP_opti_save_name_LSM," already exists in the Saved Plans"),text = "Confirm replacing the saved plan!!",type = "warning")
      }else{
        confirmSweetAlert(session,"confirm_save_lsm",title = paste0("Saving LSM optimizer ",input$SP_opti_save_name_LSM),text = "Confirm Saving LSM optimizer plan!!",type = "success")
      }
    }else{
      sendSweetAlert(session,"Error!!","Optimizer name cannot be blank",type = "error")
    }
    print(input$confirm_save_lsm)
  })
  
  observeEvent(input$confirm_save_lsm,{
    if(input$confirm_save_lsm){
      if(SP_reactive_input$simulated_flag_lsm == 1){
        SP_reactive_input$save_counter_sim <- SP_reactive_input$save_counter_sim + 1
      }else if(SP_reactive_input$simulated_flag_lsm == 0){
        SP_reactive_input$save_counter <- SP_reactive_input$save_counter + 1
      }
      SP_reactive_input$SP_TPO_list$opti_output <- SP_reactive_input$SP_opti_op_prepared_lsm
      SP_reactive_input$SP_TPO_list$simulated_flag_lsm <- SP_reactive_input$simulated_flag_lsm
      SP_reactive_input$SP_TPO_list$excluded_brand <- SP_reactive_input$SP_opti_exc_brand_lsm
      
      # if(input$SP_opti_format != ""){
      #   SP_reactive_input$TPO_Name <- paste0(SP_reactive_input$SP_brand_short,"_",SP_reactive_input$format_mapping[SP_reactive_input$format_mapping$Format_Full == input$SP_opti_format,]$Format_Short,"_",year(input$SP_opti_date_start),"_",ifelse(SP_reactive_input$SP_TPO_list$simulated_flag_lsm == 1,paste0(SP_reactive_input$save_counter,"sim_",SP_reactive_input$save_counter_sim),paste0(SP_reactive_input$save_counter)))
      # }else{
      #   SP_reactive_input$TPO_Name <- paste0(SP_reactive_input$SP_brand_short,"_",year(input$SP_opti_date_start),"_",ifelse(SP_reactive_input$SP_TPO_list$simulated_flag_lsm == 1,paste0(SP_reactive_input$save_counter,"_sim_",SP_reactive_input$save_counter_sim),paste0(SP_reactive_input$save_counter)))
      # }
      SP_reactive_input$TPO_Name <- input$SP_opti_save_name_LSM
      SP_reactive_input[[SP_reactive_input$TPO_Name]] <- SP_reactive_input$SP_TPO_list
      
      SP_reactive_input$SP_saved_tpo_latest <- data.frame("TPO ID" = SP_reactive_input$TPO_Name,"Customer" = input$SP_opti_cust,"LSM Constrained/Unconstrained" = c("LSM"), "Optimization Objective" = paste0(input$SP_opti_goal,"(",input$SP_opti_sign,")"),check.names = FALSE)
      SP_reactive_input$SP_cmp_scn_ip_latest <- data.frame("TPO ID" = SP_reactive_input$TPO_Name,"Country" = SP_reactive_input$SP_TPO_list$coun,"Customer" = SP_reactive_input$SP_TPO_list$cust,"Category" = SP_reactive_input$SP_TPO_list$cat,
                                                           "Brand" = SP_reactive_input$SP_TPO_list$brand,"Format" = paste(unique(SP_reactive_input$SP_TPO_list$format),collapse = ","),"PPG" = paste(unique(SP_reactive_input$SP_TPO_list$ppg),collapse = ","),"Goal" = SP_reactive_input$SP_TPO_list$goal_shiny,
                                                           "Goal Objective" = SP_reactive_input$SP_TPO_list$sign, "LSM Constrained/Unconstrained" = "LSM",
                                                           "Scan Net Revenue" =  (sum(SP_reactive_input$SP_opti_op_prepared_lsm$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$Net_Revenue))/10^6,
                                                           "GM % NR" = (sum(SP_reactive_input$SP_opti_op_prepared_lsm$GM_Abs) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$GM_Abs))* 100/(sum(SP_reactive_input$SP_opti_op_prepared_lsm$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$Net_Revenue)),
                                                           "TI % NR" = (sum(SP_reactive_input$SP_opti_op_prepared_lsm$Total_Trade_Investment) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$Trade_Investment)) * 100/(sum(SP_reactive_input$SP_opti_op_prepared_lsm$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$Net_Revenue)),
                                                           "Gross Sales" = (sum(SP_reactive_input$SP_opti_op_prepared_lsm$Gross_Sales) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$Gross_Sales))/10^6,
                                                           #"Trade ROI" = (sum(SP_reactive_input$SP_opti_op_prepared_lsm$Inc_GM_Abs) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$Inc_GM_Abs))/(sum(SP_reactive_input$SP_opti_op_prepared_lsm$Total_Trade_Investment) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$Trade_Investment)),
                                                           "Volume Sales" = (sum(SP_reactive_input$SP_opti_op_prepared_lsm$Total_Sales) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$Units))/10^6,check.names = FALSE)
      
      if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared_lsm$R_GM_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$R_GM_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared_lsm$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$R_Trade_Inv_Inc))
      }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared_lsm$R_Net_Rev_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$R_Net_Rev_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared_lsm$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$R_Trade_Inv_Inc))
      }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared_lsm$R_NIS_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$R_NIS_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared_lsm$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_lsm$R_Trade_Inv_Inc))
      }
      if(nrow(unique(rbind(SP_reactive_input$SP_cmp_scn_ip,SP_reactive_input$SP_cmp_scn_ip_latest)[,!(names(SP_reactive_input$SP_cmp_scn_ip) %in% "TPO ID")])) > nrow(SP_reactive_input$SP_cmp_scn_ip)){
        SP_reactive_input$SP_saved_tpo <- rbind(SP_reactive_input$SP_saved_tpo,SP_reactive_input$SP_saved_tpo_latest)
        SP_reactive_input$SP_cmp_scn_ip <- rbind(SP_reactive_input$SP_cmp_scn_ip,SP_reactive_input$SP_cmp_scn_ip_latest)
        SP_reactive_input$SP_TPO_list$Compare_Scenario <- SP_reactive_input$SP_cmp_scn_ip_latest
        tpo_list_temp <- SP_reactive_input$SP_TPO_list
        save(tpo_list_temp,file = paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",SP_reactive_input$TPO_Name,".RData"))
      }else{
        sendSweetAlert(session,"Trying to save an existing plan!!","Optimizer Plan already exists in Saved plans, check Saved Optimizer Plans",type = "warning")
      }
      #SP_reactive_input$SP_TPO_list$opti_output <- SP_reactive_input$SP_opti_op_prepared_lsm
    }
  })
  
  observeEvent(input$SP_opti_save_nonLSM,{
    ###Saving TPO ID table
    if(input$SP_opti_save_name_nonLSM != ""){
      if(file.exists(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",input$SP_opti_save_name_nonLSM,".RData"))){
        confirmSweetAlert(session,"confirm_save_nonlsm",title = paste0(input$SP_opti_save_name_nonLSM," already exists in the Saved Plans"),text = "Confirm replacing the saved plan!!",type = "warning")
      }else{
        confirmSweetAlert(session,"confirm_save_nonlsm",title = paste0("Saving Unconstrained optimizer ",input$SP_opti_save_name_nonLSM),text = "Confirm Saving Unconstrained optimizer plan!!",type = "success")
      }
    }else{
      sendSweetAlert(session,"Error!!","Optimizer name cannot be blank",type = "error")
    }
  })
  
  observeEvent(input$confirm_save_nonlsm,{
    if(input$confirm_save_nonlsm){
      
      if(SP_reactive_input$simulated_flag == 1){
        SP_reactive_input$save_counter_sim <- SP_reactive_input$save_counter_sim + 1
      }else if(SP_reactive_input$simulated_flag == 0){
        SP_reactive_input$save_counter <- SP_reactive_input$save_counter + 1
      }
      
      SP_reactive_input$SP_TPO_list$opti_output <- SP_reactive_input$SP_opti_op_prepared
      SP_reactive_input$SP_TPO_list$simulated_flag <- SP_reactive_input$simulated_flag
      SP_reactive_input$SP_TPO_list$excluded_brand <- SP_reactive_input$SP_opti_exc_brand
      
      # if(input$SP_opti_format != ""){
      #   SP_reactive_input$TPO_Name <- paste0(SP_reactive_input$SP_brand_short,"_",SP_reactive_input$format_mapping[SP_reactive_input$format_mapping$Format_Full == input$SP_opti_format,]$Format_Short,"_",year(input$SP_opti_date_start),"_",ifelse(SP_reactive_input$SP_TPO_list$simulated_flag == 1,paste0(SP_reactive_input$save_counter,"sim_",SP_reactive_input$save_counter_sim),paste0(SP_reactive_input$save_counter)))
      # }else{
      #   SP_reactive_input$TPO_Name <- paste0(SP_reactive_input$SP_brand_short,"_",year(input$SP_opti_date_start),"_",ifelse(SP_reactive_input$SP_TPO_list$simulated_flag == 1,paste0(SP_reactive_input$save_counter,"_sim_",SP_reactive_input$save_counter_sim),paste0(SP_reactive_input$save_counter)))
      # }
      SP_reactive_input$TPO_Name <- input$SP_opti_save_name_nonLSM
      SP_reactive_input[[SP_reactive_input$TPO_Name]] <- SP_reactive_input$SP_TPO_list
      
      SP_reactive_input$SP_saved_tpo_latest <- data.frame("TPO ID" = SP_reactive_input$TPO_Name,"Customer" = input$SP_opti_cust,"LSM Constrained/Unconstrained" = c("Unconstrained"), "Optimization Objective" = paste0(input$SP_opti_goal,"(",input$SP_opti_sign,")"),check.names = FALSE)
      
      SP_reactive_input$SP_cmp_scn_ip_latest <- data.frame("TPO ID" = SP_reactive_input$TPO_Name,"Country" = SP_reactive_input$SP_TPO_list$coun,"Customer" = SP_reactive_input$SP_TPO_list$cust,"Category" = SP_reactive_input$SP_TPO_list$cat,
                                                           "Brand" = SP_reactive_input$SP_TPO_list$brand,"Format" = paste(unique(SP_reactive_input$SP_TPO_list$format),collapse = ","),"PPG" = paste(unique(SP_reactive_input$SP_TPO_list$ppg),collapse = ","),"Goal" = SP_reactive_input$SP_TPO_list$goal_shiny,
                                                           "Goal Objective" = SP_reactive_input$SP_TPO_list$sign, "LSM Constrained/Unconstrained" = "Unconstrained",
                                                           "Scan Net Revenue" =  (sum(SP_reactive_input$SP_opti_op_prepared$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand$Net_Revenue))/10^6,
                                                           "GM % NR" = (sum(SP_reactive_input$SP_opti_op_prepared$GM_Abs) + sum(SP_reactive_input$SP_opti_exc_brand$GM_Abs))* 100/(sum(SP_reactive_input$SP_opti_op_prepared$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand$Net_Revenue)),
                                                           "TI % NR" = (sum(SP_reactive_input$SP_opti_op_prepared$Total_Trade_Investment) + sum(SP_reactive_input$SP_opti_exc_brand$Trade_Investment)) * 100/(sum(SP_reactive_input$SP_opti_op_prepared$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand$Net_Revenue)),
                                                           "Gross Sales" = (sum(SP_reactive_input$SP_opti_op_prepared$Gross_Sales) + sum(SP_reactive_input$SP_opti_exc_brand$Gross_Sales))/10^6,
                                                           #"Trade ROI" = (sum(SP_reactive_input$SP_opti_op_prepared$Inc_GM_Abs) + sum(SP_reactive_input$SP_opti_exc_brand$Inc_GM_Abs))/(sum(SP_reactive_input$SP_opti_op_prepared$Total_Trade_Investment) + sum(SP_reactive_input$SP_opti_exc_brand$Trade_Investment)),
                                                           "Volume Sales" = (sum(SP_reactive_input$SP_opti_op_prepared$Total_Sales) + sum(SP_reactive_input$SP_opti_exc_brand$Units))/10^6,check.names = FALSE)
      
      if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared$R_GM_Inc) + sum(SP_reactive_input$SP_opti_exc_brand$R_GM_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand$R_Trade_Inv_Inc))
      }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared$R_Net_Rev_Inc) + sum(SP_reactive_input$SP_opti_exc_brand$R_Net_Rev_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand$R_Trade_Inv_Inc))
      }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared$R_NIS_Inc) + sum(SP_reactive_input$SP_opti_exc_brand$R_NIS_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand$R_Trade_Inv_Inc))
      }
      if(nrow(unique(rbind(SP_reactive_input$SP_cmp_scn_ip,SP_reactive_input$SP_cmp_scn_ip_latest)[,!(names(SP_reactive_input$SP_cmp_scn_ip) %in% "TPO ID")])) > nrow(SP_reactive_input$SP_cmp_scn_ip)){
        SP_reactive_input$SP_saved_tpo <- rbind(SP_reactive_input$SP_saved_tpo,SP_reactive_input$SP_saved_tpo_latest)
        SP_reactive_input$SP_cmp_scn_ip <- rbind(SP_reactive_input$SP_cmp_scn_ip,SP_reactive_input$SP_cmp_scn_ip_latest)
        SP_reactive_input$SP_TPO_list$Compare_Scenario <- SP_reactive_input$SP_cmp_scn_ip_latest
        tpo_list_temp <- SP_reactive_input$SP_TPO_list
        save(tpo_list_temp,file = paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",SP_reactive_input$TPO_Name,".RData"))
      }else{
        sendSweetAlert(session,"Trying to save an existing plan!!","Optimizer Plan already exists in Saved plans, check Saved Optimizer Plans",type = "warning")
      }
    }
  })
  
  output$SP_opti_saved_tpo <- renderDataTable({
    validate(
      need(SP_reactive_input$SP_saved_tpo,"Save an Optimization Plan")
    )
    #
    datatable(SP_reactive_input$SP_saved_tpo,class="cell-border stripe",extensions = c('FixedColumns'),selection =list(target='cell',mode="single"),
              options = list(
                lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
                pageLength = 10,
                dom = "Blftrip",
                # searching = FALSE,
                scrollX = T,
                scrollY = "150px"
              ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_saved_tpo),textAlign = 'center')%>%
      formatStyle(as.character("TPO ID"),cursor = 'pointer')
  })
  
  observeEvent(input$SP_opti_cal_LSM_cell_clicked,{
    
    if(length(input$SP_opti_cal_LSM_cell_clicked) != 0){
      
      updateSelectizeInput(session,"SP_sim_coun",choices = input$SP_opti_op_coun,selected = input$SP_opti_op_coun)
      updateSelectizeInput(session,"SP_sim_cust",choices = input$SP_opti_op_cust,selected = input$SP_opti_op_cust)
      updateSelectizeInput(session,"SP_sim_cat",choices = input$SP_opti_op_cat,selected = input$SP_opti_op_cat)
      updateSelectizeInput(session,"SP_sim_brand",choices = input$SP_opti_op_brand,selected = input$SP_opti_op_brand)
      
      SP_reactive_input$SP_opti_op_grouped_event <- SP_reactive_input$SP_opti_op_prepared_lsm[,.("TPR_Flag" = unique(TPR_Flag),"Display_Flag"=unique(Display_Flag),"Display"= unique(Display),"Promo Price"=mean(Promo_Price),"RSP" = mean(RSP_Unit),"Net Sales" = sum(Total_Sales),
                                                                                                 "ROI" = ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc)))),by = .(PPG,PPG_Description,Tesco_Week_No)]
      SP_reactive_input$SP_opti_op_grouped_event[SP_reactive_input$SP_opti_op_grouped_event$TPR_Flag != 1,]$ROI <- 0
      SP_reactive_input$SP_opti_op_grouped_event$`Promo Slots` <- paste0("Event-",SP_reactive_input$SP_opti_op_grouped_event$Tesco_Week_No)
      
      SP_reactive_input$SP_opti_op_lsm_clicked_ppg <- SP_reactive_input$opti_cal_dcast_lsm[input$SP_opti_cal_LSM_cell_clicked$row,]$PPG
      SP_reactive_input$SP_opti_op_lsm_clicked_event <- SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$PPG == SP_reactive_input$SP_opti_op_lsm_clicked_ppg & SP_reactive_input$opti_cal_filtered_lsm$`Week No` == (input$SP_opti_cal_LSM_cell_clicked$col - 4),]$Tesco_Week_No
      
      updateSelectizeInput(session,"SP_sim_format",choices = SP_reactive_input$opti_cal_dcast_lsm[input$SP_opti_cal_LSM_cell_clicked$row,]$Format,selected = SP_reactive_input$opti_cal_dcast_lsm[input$SP_opti_cal_LSM_cell_clicked$row,]$Format)
      updateSelectizeInput(session,"SP_sim_ppg",choices = SP_reactive_input$SP_opti_op_lsm_clicked_ppg,selected = SP_reactive_input$SP_opti_op_lsm_clicked_ppg)
      updateSelectizeInput(session,"SP_sim_event",choices = paste0("Event-",SP_reactive_input$SP_opti_op_lsm_clicked_event),selected = paste0("Event-",SP_reactive_input$SP_opti_op_lsm_clicked_event))
      
      SP_reactive_input$SP_opti_op_lsm_clicked_event_table <- SP_reactive_input$SP_opti_op_grouped_event[SP_reactive_input$SP_opti_op_grouped_event$PPG ==SP_reactive_input$SP_opti_op_lsm_clicked_ppg & SP_reactive_input$SP_opti_op_grouped_event$Tesco_Week_No ==SP_reactive_input$SP_opti_op_lsm_clicked_event,]
      SP_reactive_input$SP_opti_op_lsm_clicked_event_table$`lsm_nonlsm` <- "LSM"
      SP_reactive_input$SP_opti_op_lsm_clicked_event_table <- SP_reactive_input$SP_opti_op_lsm_clicked_event_table[,c("PPG","PPG_Description","RSP","lsm_nonlsm","Promo Slots","Display","Promo Price","ROI")]
      #
      names(SP_reactive_input$SP_opti_op_lsm_clicked_event_table) <- c("PPG","PPG Description","RSP","LSM Constrained/Unconstrained","Promo Slots","Display Type","Promoted Price",input$SP_opti_ROI_selection)
      
      SP_reactive_input$SP_opti_op_lsm_clicked_slot_display <- SP_reactive_input$SP_opti_op_grouped_event[SP_reactive_input$SP_opti_op_grouped_event$Display_Flag == 1 & SP_reactive_input$SP_opti_op_grouped_event$Tesco_Week_No ==SP_reactive_input$SP_opti_op_lsm_clicked_event,]
      SP_reactive_input$SP_opti_op_lsm_clicked_slot_display$`lsm_nonlsm` <- "LSM"
      SP_reactive_input$SP_opti_op_lsm_clicked_slot_display <- SP_reactive_input$SP_opti_op_lsm_clicked_slot_display[,c("PPG","PPG_Description","RSP","lsm_nonlsm","Promo Slots","Display","Promo Price","ROI")]
      names(SP_reactive_input$SP_opti_op_lsm_clicked_slot_display) <- c("PPG","PPG Description","RSP","LSM Constrained/Unconstrained","Promo Slots","Display Type","Promoted Price",input$SP_opti_ROI_selection)
      
      # SP_reactive_input$SP_opti_op_lsm_event_table <- SP_reactive_input$SP_opti_op_grouped_event[SP_reactive_input$SP_opti_op_grouped_event$PPG ==SP_reactive_input$SP_opti_op_lsm_clicked_ppg & SP_reactive_input$SP_opti_op_grouped_event$TPR_Flag == 1& SP_reactive_input$SP_opti_op_grouped_event$`Promo Slots` != paste0("Event-",SP_reactive_input$SP_opti_op_lsm_clicked_event),]
      # SP_reactive_input$SP_opti_op_lsm_event_table$`lsm_nonlsm` <- "LSM"
      # SP_reactive_input$SP_opti_op_lsm_event_table <- cbind(SP_reactive_input$SP_opti_op_lsm_event_table[,c("PPG","lsm_nonlsm","Promo Slots","Display","Promo Price","ROI")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_lsm_event_table))))
      # SP_reactive_input$SP_opti_op_lsm_event_table <- rbind(SP_reactive_input$SP_opti_op_lsm_event_table,data.frame("PPG" = unique(SP_reactive_input$SP_opti_op_lsm_event_table$PPG),"lsm_nonlsm" = "LSM","Promo Slots" = "No Event", "Display" = "", "Promo Price" = 0,"ROI" = 0,"Select" = FALSE,check.names = FALSE))
      observeEvent(c(input$SP_sim_reset),{
        SP_reactive_input$SP_opti_op_lsm_event_table <- SP_reactive_input$shiny_ip_events_final[SP_reactive_input$shiny_ip_events_final$PPG==SP_reactive_input$SP_opti_op_lsm_clicked_ppg,]
        SP_reactive_input$SP_opti_op_lsm_event_table$`lsm_nonlsm` <- "LSM"
        SP_reactive_input$SP_opti_op_lsm_event_table$`Event ID` <- paste0("Event Substitute-",c(1:nrow(SP_reactive_input$SP_opti_op_lsm_event_table)))
        
        if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
          SP_reactive_input$SP_opti_op_lsm_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_lsm_event_table[,c("PPG","PPG_Description","RSP_Unit","lsm_nonlsm","Event ID","Display","Promo_Price","ROI_GM")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_lsm_event_table))))
          names(SP_reactive_input$SP_opti_op_lsm_event_table_1) <- c("PPG","PPG_Description","RSP","lsm_nonlsm","Event ID","Display","Promo_Price","ROI","Select")
          
        }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
          
          SP_reactive_input$SP_opti_op_lsm_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_lsm_event_table[,c("PPG","PPG_Description","RSP_Unit","lsm_nonlsm","Event ID","Display","Promo_Price","ROI_Rev")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_lsm_event_table))))
          names(SP_reactive_input$SP_opti_op_lsm_event_table_1) <- c("PPG","PPG_Description","RSP","lsm_nonlsm","Event ID","Display","Promo_Price","ROI","Select")
          
        }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
          SP_reactive_input$SP_opti_op_lsm_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_lsm_event_table[,c("PPG","PPG_Description","RSP_Unit","lsm_nonlsm","Event ID","Display","Promo_Price","ROI_NIS")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_lsm_event_table))))
          names(SP_reactive_input$SP_opti_op_lsm_event_table_1) <- c("PPG","PPG_Description","RSP","lsm_nonlsm","Event ID","Display","Promo_Price","ROI","Select")
        }
        
        SP_reactive_input$SP_opti_op_lsm_event_table_1 <- rbind(SP_reactive_input$SP_opti_op_lsm_event_table_1,data.frame("PPG" = unique(SP_reactive_input$SP_opti_op_lsm_event_table_1$PPG),"PPG_Description" = unique(SP_reactive_input$SP_opti_op_lsm_event_table_1$PPG_Description),"RSP" = unique(SP_reactive_input$SP_opti_op_lsm_event_table_1$RSP),"lsm_nonlsm" = "LSM","Event ID" = "No Event", "Display" = "", "Promo_Price" = 0,"ROI" = 0,"Select" = FALSE,check.names = FALSE))
        #SP_reactive_input$SP_opti_op_lsm_event_table$`Promoted Price` <- as.numeric(SP_reactive_input$SP_opti_op_lsm_event_table$`Promoted Price`)
        SP_reactive_input$SP_opti_op_lsm_event_table_1 <- SP_reactive_input$SP_opti_op_lsm_event_table_1[order(-ROI),]
        names(SP_reactive_input$SP_opti_op_lsm_event_table_1) <- c("PPG","PPG Description","RSP","LSM Constrained/Unconstrained","Event ID","Display Type","Promoted Price",input$SP_opti_ROI_selection,"Select")
        #SP_reactive_input$SP_opti_op_lsm_event_table_1 <- SP_reactive_input$SP_opti_op_lsm_event_table_1[,!(names(SP_reactive_input$SP_opti_op_lsm_event_table_1) %in% "LSM Constrained/Unconstrained"),with = FALSE]
      })
      SP_reactive_input$simulator_op_summary_ppg <- NULL
      SP_reactive_input$simulator_op_summary_event_table <- NULL
      shinyjs::hide("scenario_analysis")
      
      updateTabItems(session, "sidebar_main", selected = "SP_subMenu7")
    }
    
    output$SP_sim_event_selected_table <- renderDataTable({
      datatable(SP_reactive_input$SP_opti_op_lsm_clicked_event_table,class="cell-border stripe",extensions = c('FixedColumns'),
                options = list(
                  # paging = FALSE,
                  dom = "t",
                  # searching = FALSE,
                  scrollX = F,
                  scrollY = F
                ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_opti_op_lsm_clicked_event_table),textAlign = 'center')%>%
        formatRound(c("Promoted Price","RSP",input$SP_opti_ROI_selection),digits = 2)
    })
    output$SP_sim_event_slot_display_table <- renderDataTable({
      datatable(SP_reactive_input$SP_opti_op_lsm_clicked_slot_display,class="cell-border stripe",extensions = c('FixedColumns'),
                options = list(
                  # paging = FALSE,
                  dom = "t",
                  # searching = FALSE,
                  scrollX = T,
                  scrollY = "150px"
                ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_opti_op_lsm_clicked_slot_display),textAlign = 'center')%>%
        formatRound(c("Promoted Price","RSP",input$SP_opti_ROI_selection),digits = 2)
    })
    #
    output$SP_sim_event_table <- renderRHandsontable({
      # Column order: PPG, PPG Description, RSP, LSM Constrained/Unconstrained, Event ID, Display Type, Promoted Price, ROI, Select
      rhandsontable(SP_reactive_input$SP_opti_op_lsm_event_table_1,rowHeaders = FALSE, height = 400) %>%
        hot_col(1, readOnly = TRUE, width = 80, colHeader = "PPG") %>%  # PPG
        hot_col(2, readOnly = TRUE, width = 200, colHeader = "PPG Description") %>%  # PPG Description
        hot_col(3, readOnly = TRUE, width = 80, colHeader = "RSP") %>%  # RSP
        hot_col(4, readOnly = TRUE, width = 120, colHeader = "LSM Constrained/Unconstrained") %>%  # LSM Constrained/Unconstrained
        hot_col(5, readOnly = TRUE, width = 150, colHeader = "Event ID") %>%  # Event ID
        hot_col(6, readOnly = TRUE, width = 100, colHeader = "Display Type") %>%  # Display Type
        hot_col(7, readOnly = TRUE, width = 120, colHeader = "Promoted Price") %>%  # Promoted Price
        hot_col(8, readOnly = TRUE, width = 120, colHeader = input$SP_opti_ROI_selection) %>%  # ROI column (variable name)
        hot_col(9, readOnly = FALSE, width = 80, type = "checkbox", colHeader = "Select") %>%  # Select checkbox column
        hot_cols(columnSorting = TRUE) %>%
        hot_table(stretchH = "none", contextMenu = TRUE)
    })
  })
  
  observeEvent(input$SP_opti_cal_nonLSM_cell_clicked,{
    
    if(length(input$SP_opti_cal_nonLSM_cell_clicked) != 0){
      updateSelectizeInput(session,"SP_sim_coun",choices = input$SP_opti_op_coun,selected = input$SP_opti_op_coun)
      updateSelectizeInput(session,"SP_sim_cust",choices = input$SP_opti_op_cust,selected = input$SP_opti_op_cust)
      updateSelectizeInput(session,"SP_sim_cat",choices = input$SP_opti_op_cat,selected = input$SP_opti_op_cat)
      updateSelectizeInput(session,"SP_sim_brand",choices = input$SP_opti_op_brand,selected = input$SP_opti_op_brand)
      
      SP_reactive_input$SP_opti_op_grouped_event <- SP_reactive_input$SP_opti_op_prepared[,.("TPR_Flag" = unique(TPR_Flag),"Display_Flag"=unique(Display_Flag),"Display"= unique(Display),"Promo Price"=mean(Promo_Price),"RSP" = mean(RSP_Unit),"Net Sales" = sum(Total_Sales),
                                                                                             "ROI" = ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc)))),by = .(PPG,PPG_Description,Tesco_Week_No)]
      SP_reactive_input$SP_opti_op_grouped_event[SP_reactive_input$SP_opti_op_grouped_event$TPR_Flag != 1,]$ROI <- 0
      SP_reactive_input$SP_opti_op_grouped_event$`Promo Slots` <- paste0("Event-",SP_reactive_input$SP_opti_op_grouped_event$Tesco_Week_No)
      
      SP_reactive_input$SP_opti_op_nonlsm_clicked_ppg <- SP_reactive_input$opti_cal_dcast[input$SP_opti_cal_nonLSM_cell_clicked$row,]$PPG
      SP_reactive_input$SP_opti_op_nonlsm_clicked_event <- SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$PPG == SP_reactive_input$SP_opti_op_nonlsm_clicked_ppg & SP_reactive_input$opti_cal_filtered$`Week No` == (input$SP_opti_cal_nonLSM_cell_clicked$col - 4),]$Tesco_Week_No
      
      updateSelectizeInput(session,"SP_sim_format",choices = SP_reactive_input$opti_cal_dcast[input$SP_opti_cal_nonLSM_cell_clicked$row,]$Format,selected = SP_reactive_input$opti_cal_dcast[input$SP_opti_cal_nonLSM_cell_clicked$row,]$Format)
      updateSelectizeInput(session,"SP_sim_ppg",choices = SP_reactive_input$SP_opti_op_nonlsm_clicked_ppg,selected = SP_reactive_input$SP_opti_op_nonlsm_clicked_ppg)
      updateSelectizeInput(session,"SP_sim_event",choices = paste0("Event-",SP_reactive_input$SP_opti_op_nonlsm_clicked_event),selected = paste0("Event-",SP_reactive_input$SP_opti_op_nonlsm_clicked_event))
      
      SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table <- SP_reactive_input$SP_opti_op_grouped_event[SP_reactive_input$SP_opti_op_grouped_event$PPG ==SP_reactive_input$SP_opti_op_nonlsm_clicked_ppg & SP_reactive_input$SP_opti_op_grouped_event$Tesco_Week_No ==SP_reactive_input$SP_opti_op_nonlsm_clicked_event,]
      SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table$`lsm_nonlsm` <- "Unconstrained"
      SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table <- SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table[,c("PPG","PPG_Description","RSP","lsm_nonlsm","Promo Slots","Display","Promo Price","ROI")]
      names(SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table) <- c("PPG","PPG Description","RSP","LSM Constrained/Unconstrained","Promo Slots","Display Type","Promoted Price",input$SP_opti_ROI_selection)
      #
      SP_reactive_input$SP_opti_op_nonlsm_clicked_slot_display <- SP_reactive_input$SP_opti_op_grouped_event[SP_reactive_input$SP_opti_op_grouped_event$Display_Flag == 1 & SP_reactive_input$SP_opti_op_grouped_event$Tesco_Week_No == SP_reactive_input$SP_opti_op_nonlsm_clicked_event,]
      SP_reactive_input$SP_opti_op_nonlsm_clicked_slot_display$`lsm_nonlsm` <- "Unconstrained"
      SP_reactive_input$SP_opti_op_nonlsm_clicked_slot_display <- SP_reactive_input$SP_opti_op_nonlsm_clicked_slot_display[,c("PPG","PPG_Description","RSP","lsm_nonlsm","Promo Slots","Display","Promo Price","ROI")]
      names(SP_reactive_input$SP_opti_op_nonlsm_clicked_slot_display) <- c("PPG","PPG Description","RSP","LSM Constrained/Unconstrained","Promo Slots","Display Type","Promoted Price",input$SP_opti_ROI_selection)
      
      observeEvent(c(input$SP_sim_reset),{
        SP_reactive_input$SP_opti_op_nonlsm_event_table <- SP_reactive_input$shiny_ip_events_final[SP_reactive_input$shiny_ip_events_final$PPG==SP_reactive_input$SP_opti_op_nonlsm_clicked_ppg,]
        SP_reactive_input$SP_opti_op_nonlsm_event_table$`lsm_nonlsm` <- "Unconstrained"
        SP_reactive_input$SP_opti_op_nonlsm_event_table$`Event ID` <- paste0("Event Substitute-",c(1:nrow(SP_reactive_input$SP_opti_op_nonlsm_event_table)))
        
        if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
          SP_reactive_input$SP_opti_op_nonlsm_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_nonlsm_event_table[,c("PPG","PPG_Description","RSP_Unit","lsm_nonlsm","Event ID","Display","Promo_Price","ROI_GM")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_nonlsm_event_table))))
          names(SP_reactive_input$SP_opti_op_nonlsm_event_table_1) <- c("PPG","PPG_Description","RSP","lsm_nonlsm","Event ID","Display","Promo_Price","ROI","Select")
          
        }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
          
          SP_reactive_input$SP_opti_op_nonlsm_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_nonlsm_event_table[,c("PPG","PPG_Description","RSP_Unit","lsm_nonlsm","Event ID","Display","Promo_Price","ROI_Rev")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_nonlsm_event_table))))
          names(SP_reactive_input$SP_opti_op_nonlsm_event_table_1) <- c("PPG","PPG_Description","RSP","lsm_nonlsm","Event ID","Display","Promo_Price","ROI","Select")
          
        }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
          SP_reactive_input$SP_opti_op_nonlsm_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_nonlsm_event_table[,c("PPG","PPG_Description","RSP_Unit","lsm_nonlsm","Event ID","Display","Promo_Price","ROI_NIS")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_nonlsm_event_table))))
          names(SP_reactive_input$SP_opti_op_nonlsm_event_table_1) <- c("PPG","PPG_Description","RSP","lsm_nonlsm","Event ID","Display","Promo_Price","ROI","Select")
        }
        
        #SP_reactive_input$SP_opti_op_nonlsm_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_nonlsm_event_table[,c("PPG","lsm_nonlsm","Event ID","Display","Promo_Price","ROI")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_nonlsm_event_table))))
        #SP_reactive_input$SP_opti_op_nonlsm_event_table$`Promoted Price` <- as.numeric(as.character(SP_reactive_input$SP_opti_op_nonlsm_event_table$`Promoted Price`))
        SP_reactive_input$SP_opti_op_nonlsm_event_table_1 <- rbind(SP_reactive_input$SP_opti_op_nonlsm_event_table_1,data.frame("PPG" = unique(SP_reactive_input$SP_opti_op_nonlsm_event_table_1$PPG),"PPG_Description" = unique(SP_reactive_input$SP_opti_op_nonlsm_event_table_1$PPG_Description),"RSP" = unique(SP_reactive_input$SP_opti_op_nonlsm_event_table_1$RSP),"lsm_nonlsm" = "Unconstrained","Event ID" = "No Event", "Display" = "", "Promo_Price" = 0,"ROI" = 0,"Select" = FALSE,check.names = FALSE))
        SP_reactive_input$SP_opti_op_nonlsm_event_table_1 <- SP_reactive_input$SP_opti_op_nonlsm_event_table_1[order(-ROI),]
        names(SP_reactive_input$SP_opti_op_nonlsm_event_table_1) <- c("PPG","PPG Description","RSP","LSM Constrained/Unconstrained","Event ID","Display Type","Promoted Price",input$SP_opti_ROI_selection,"Select")
        #SP_reactive_input$SP_opti_op_nonlsm_event_table_1 <- SP_reactive_input$SP_opti_op_nonlsm_event_table_1[,!(names(SP_reactive_input$SP_opti_op_nonlsm_event_table_1) %in% "LSM Constrained/Unconstrained"),with = FALSE]
      })
      SP_reactive_input$simulator_op_summary_ppg <- NULL
      SP_reactive_input$simulator_op_summary_event_table <- NULL
      shinyjs::hide("scenario_analysis")
      
      updateTabItems(session, "sidebar_main", selected = "SP_subMenu7")
    }
    output$SP_sim_event_selected_table <- renderDataTable({
      datatable(SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table,class="cell-border compact hover",extensions = c('FixedColumns'),
                options = list(
                  # paging = FALSE,
                  dom = "t",
                  # searching = FALSE,
                  scrollX = F,
                  scrollY = F
                ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table),textAlign = 'center')%>%
        formatRound(c("Promoted Price","RSP",input$SP_opti_ROI_selection),digits = 2)
    })
    
    output$SP_sim_event_slot_display_table <- renderDataTable({
      datatable(SP_reactive_input$SP_opti_op_nonlsm_clicked_slot_display,class="cell-border compact hover",extensions = c('FixedColumns'),
                options = list(
                  # paging = FALSE,
                  dom = "t",
                  # searching = FALSE,
                  scrollX = T,
                  scrollY = "150px"
                ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_opti_op_nonlsm_clicked_slot_display),textAlign = 'center')%>%
        formatRound(c("Promoted Price","RSP",input$SP_opti_ROI_selection),digits = 2)
    })
    
    output$SP_sim_event_table <- renderRHandsontable({
      # Column order: PPG, PPG Description, RSP, LSM Constrained/Unconstrained, Event ID, Display Type, Promoted Price, ROI, Select
      rhandsontable(SP_reactive_input$SP_opti_op_nonlsm_event_table_1,rowHeaders = FALSE, height = 400) %>%
        hot_col(1, readOnly = TRUE, width = 80, colHeader = "PPG") %>%  # PPG
        hot_col(2, readOnly = TRUE, width = 200, colHeader = "PPG Description") %>%  # PPG Description
        hot_col(3, readOnly = TRUE, width = 80, colHeader = "RSP") %>%  # RSP
        hot_col(4, readOnly = TRUE, width = 120, colHeader = "LSM Constrained/Unconstrained") %>%  # LSM Constrained/Unconstrained
        hot_col(5, readOnly = TRUE, width = 150, colHeader = "Event ID") %>%  # Event ID
        hot_col(6, readOnly = TRUE, width = 100, colHeader = "Display Type") %>%  # Display Type
        hot_col(7, readOnly = TRUE, width = 120, colHeader = "Promoted Price") %>%  # Promoted Price
        hot_col(8, readOnly = TRUE, width = 120, colHeader = input$SP_opti_ROI_selection) %>%  # ROI column (variable name)
        hot_col(9, readOnly = FALSE, width = 80, type = "checkbox", colHeader = "Select") %>%  # Select checkbox column
        hot_cols(columnSorting = TRUE) %>%
        hot_table(stretchH = "none", contextMenu = TRUE)
    })
  })
  
  observeEvent(input$SP_sim_reset,{
    SP_reactive_input$simulator_op_summary_ppg <- NULL
    SP_reactive_input$simulator_op_summary_event_table <- NULL
    shinyjs::hide("scenario_analysis")
  })
  
  observeEvent(input$SP_sim_run,{
    print("Unconstrained")
    SP_reactive_input$events_all_op <- hot_to_r(input$SP_sim_event_table)
    if(nrow(SP_reactive_input$events_all_op[SP_reactive_input$events_all_op$Select == TRUE,]) == 0){
      sendSweetAlert(session,"Error!!","Please select atleast one substitue event",type = "error")
    }else{
      if(unique(SP_reactive_input$events_all_op$`LSM Constrained/Unconstrained`) == "Unconstrained"){
        SP_reactive_input$events_all_op_selected <- SP_reactive_input$events_all_op[SP_reactive_input$events_all_op$Select == TRUE,]
        SP_reactive_input$simulator_op <- simulator(SP_reactive_input$SP_opti_op_prepared,SP_reactive_input$SP_opti_output[[2]],SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table,SP_reactive_input$events_all_op_selected,SP_reactive_input$SP_TPO_list$other_sales,SP_reactive_input$SP_opti_op_nonlsm_event_table,input$SP_opti_ROI_selection,SP_reactive_input$SP_TPO_list$other_sales_value)
        
        SP_reactive_input$simulator_op_summary_event <- SP_reactive_input$simulator_op[[4]]
        SP_reactive_input$simulator_op_summary_event$lsm_nonlsm <- "Unconstrained"
        
        SP_reactive_input$simulator_op_summary_event_table <- SP_reactive_input$simulator_op_summary_event[,c("Replaced.Event","RSP","DisplayType","PromotedPrice","lsm_nonlsm","GM_percent_model_event","Volume_sales_model_event","Gross_sales_model_event","ROI_model_event","Trade_as_per_NR_model_event","Net_Sales_model_event")]
        SP_reactive_input$simulator_op_summary_event_table[,c("Volume_sales_model_event","Gross_sales_model_event","Net_Sales_model_event")] <- SP_reactive_input$simulator_op_summary_event_table[,c("Volume_sales_model_event","Gross_sales_model_event","Net_Sales_model_event")]/10^3
        
        #names(SP_reactive_input$simulator_op_summary_event_table) <- c("Replaced Event","LSM Constrained/Unconstrained","Gross Margin % of NR","Volume Sales","Scan Gross Sales","Trade ROI","Trade Spend % of NR","Scan Net Revenue")
        
        names(SP_reactive_input$simulator_op_summary_event_table) <- c("Replaced Event","RSP","Display Type","Promoted Price","LSM Constrained/Unconstrained","Gross Margin % of NR","Volume Sales('000 Units)","Scan Gross Sales('000 GBP)",input$SP_opti_ROI_selection,"Trade Spend % of NR","Scan Net Revenue('000 GBP)")
        
        
        SP_reactive_input$simulator_op_summary_event_table <- cbind(data.frame("Select Event to be Replaced" = rep(FALSE,nrow(SP_reactive_input$simulator_op_summary_event_table)),check.names = FALSE),SP_reactive_input$simulator_op_summary_event_table)
        #SP_reactive_input$simulator_op_summary_event_table <- SP_reactive_input$simulator_op_summary_event_table[,!(names(SP_reactive_input$simulator_op_summary_event_table) %in% "LSM Constrained/Unconstrained"),with = FALSE]
        output$SP_sim_run_outcomes_table <- renderRHandsontable({
          validate(
            need(SP_reactive_input$simulator_op_summary_event_table,"Select an event and run simulation")
          )
          rhandsontable(SP_reactive_input$simulator_op_summary_event_table,rowHeaders = FALSE) %>%
            hot_cell(1, "Select Event to be Replaced", readOnly = TRUE)
        })
        ######Base sales and incremental sales at PPG Level
        SP_reactive_input$simulator_op_summary_ppg <- SP_reactive_input$simulator_op[[3]]
        SP_reactive_input$simulator_op_summary_ppg$lsm_nonlsm <- "Unconstrained"
        SP_reactive_input$simulator_op_summary_ppg$Replaced.Event <- as.character(SP_reactive_input$simulator_op_summary_ppg$Replaced.Event)
        output$SP_sim_run_outcomes_plot <- renderPlotly({
          validate(
            need(SP_reactive_input$simulator_op_summary_ppg,"Select an event and run simulation")
          )
          plot_ly(SP_reactive_input$simulator_op_summary_ppg,x= ~Replaced.Event, y= ~(base_sales_ppg),type='bar',name='Base Sales',marker = list(color='#7CC049'),
                  hovertext = paste0(round((SP_reactive_input$simulator_op_summary_ppg$base_sales_ppg/10^3),2),"('000 Units)"),hoverinfo = 'text') %>%
            add_trace(y= ~(inc_sales_ppg), name = 'Incremental Sales',
                      hovertext = paste0(round((SP_reactive_input$simulator_op_summary_ppg$inc_sales_ppg/10^3),2),"('000 Units)"),hoverinfo = 'text',marker = list(color='#CC66FF')) %>%
            layout(title = "Promotion Effectiveness",font = list(size =10),xaxis = list(title = "Event",showgrid = FALSE),yaxis = list(title = 'Volume Sales',showgrid = FALSE),barmode = 'stack') %>% config(displayModeBar = FALSE)
        })
        shinyjs::show("scenario_analysis")
      }else if(unique(SP_reactive_input$events_all_op$`LSM Constrained/Unconstrained`) == "LSM"){
        # SP_reactive_input$events_all_op_selected <- SP_reactive_input$events_all_op[SP_reactive_input$events_all_op$Select == TRUE,]
        # SP_reactive_input$simulator_op <- simulator(SP_reactive_input$SP_opti_op_prepared_lsm,SP_reactive_input$SP_opti_output_lsm[[2]],SP_reactive_input$SP_opti_op_lsm_clicked_event_table,SP_reactive_input$events_all_op_selected,SP_reactive_input$SP_TPO_list$other_sales,SP_reactive_input$SP_TPO_list$other_sales_value)
        # SP_reactive_input$simulator_op_summary <- SP_reactive_input$simulator_op[[2]]
        # SP_reactive_input$simulator_op_summary$lsm_nonlsm <- "LSM"
        # SP_reactive_input$simulator_op_summary_table <- SP_reactive_input$simulator_op_summary[,c("Replaced.Event","lsm_nonlsm","GM_percent_model","Volume_sales_model","Gross_sales_model","ROI_model","Trade_as_per_NR_model","Net_Sales_model")]
        SP_reactive_input$events_all_op_selected <- SP_reactive_input$events_all_op[SP_reactive_input$events_all_op$Select == TRUE,]
        SP_reactive_input$simulator_op <- simulator(SP_reactive_input$SP_opti_op_prepared_lsm,SP_reactive_input$SP_opti_output_lsm[[2]],SP_reactive_input$SP_opti_op_lsm_clicked_event_table,SP_reactive_input$events_all_op_selected,SP_reactive_input$SP_TPO_list$other_sales,SP_reactive_input$SP_opti_op_lsm_event_table,input$SP_opti_ROI_selection,SP_reactive_input$SP_TPO_list$other_sales_value)
        
        SP_reactive_input$simulator_op_summary_event <- SP_reactive_input$simulator_op[[4]]
        SP_reactive_input$simulator_op_summary_event$lsm_nonlsm <- "LSM"
        SP_reactive_input$simulator_op_summary_event_table <- SP_reactive_input$simulator_op_summary_event[,c("Replaced.Event","RSP","DisplayType","PromotedPrice","lsm_nonlsm","GM_percent_model_event","Volume_sales_model_event","Gross_sales_model_event","ROI_model_event","Trade_as_per_NR_model_event","Net_Sales_model_event")]
        SP_reactive_input$simulator_op_summary_event_table[,c("Volume_sales_model_event","Gross_sales_model_event","Net_Sales_model_event")] <- SP_reactive_input$simulator_op_summary_event_table[,c("Volume_sales_model_event","Gross_sales_model_event","Net_Sales_model_event")]/10^3
        
        #names(SP_reactive_input$simulator_op_summary_event_table) <- c("Replaced Event","LSM Constrained/Unconstrained","Gross Margin % of NR","Volume Sales","Scan Gross Sales","Trade ROI","Trade Spend % of NR","Scan Net Revenue")
        
        names(SP_reactive_input$simulator_op_summary_event_table) <- c("Replaced Event","RSP","Display Type","Promoted Price","LSM Constrained/Unconstrained","Gross Margin % of NR","Volume Sales('000 Units)","Scan Gross Sales('000 GBP)",input$SP_opti_ROI_selection,"Trade Spend % of NR","Scan Net Revenue('000 GBP)")
        
        SP_reactive_input$simulator_op_summary_event_table <- cbind(data.frame("Select Event to be Replaced" = rep(FALSE,nrow(SP_reactive_input$simulator_op_summary_event_table)),check.names = FALSE),SP_reactive_input$simulator_op_summary_event_table)
        #SP_reactive_input$simulator_op_summary_event_table <- SP_reactive_input$simulator_op_summary_event_table[,!(names(SP_reactive_input$simulator_op_summary_event_table) %in% "LSM Constrained/Unconstrained"),with = FALSE]
        ###keeping the selected event first
        #SP_reactive_input$simulator_op_summary_event_table <- SP_reactive_input$simulator_op_summary_event_table[order(-SP_reactive_input$simulator_op_summary_event_table$`Trade ROI`),]
        output$SP_sim_run_outcomes_table <- renderRHandsontable({
          validate(
            need(SP_reactive_input$simulator_op_summary_event_table,"Select an event and run simulation")
          )
          rhandsontable(SP_reactive_input$simulator_op_summary_event_table) %>%
            hot_cell(1, "Select Event to be Replaced", readOnly = TRUE)
          #hot_col(c("Volume Sales","Scan Gross Sales","Scan Net Revenue"),currency = "", interval = 3, mark = ",")
        })
        
        ######Base sales and incremental sales at PPG Level
        SP_reactive_input$simulator_op_summary_ppg <- SP_reactive_input$simulator_op[[3]]
        SP_reactive_input$simulator_op_summary_ppg$lsm_nonlsm <- "LSM"
        SP_reactive_input$simulator_op_summary_ppg$Replaced.Event <- as.character(SP_reactive_input$simulator_op_summary_ppg$Replaced.Event)
        
        output$SP_sim_run_outcomes_plot <- renderPlotly({
          validate(
            need(SP_reactive_input$simulator_op_summary_ppg,"Select an event and run simulation")
          )
          plot_ly(SP_reactive_input$simulator_op_summary_ppg,x= ~Replaced.Event, y= ~(base_sales_ppg),type='bar',name='Base Sales',marker = list(color='#7CC049'),
                  hovertext = paste0(round((SP_reactive_input$simulator_op_summary_ppg$base_sales_ppg/10^3),2),"('000 Units)"),hoverinfo = 'text') %>%
            add_trace(y= ~(inc_sales_ppg), name = 'Incremental Sales',
                      hovertext = paste0(round((SP_reactive_input$simulator_op_summary_ppg$inc_sales_ppg/10^3),2),"('000 Units)"),hoverinfo = 'text',marker = list(color='#CC66FF')) %>%
            layout(title = "Promotion Effectiveness",font = list(size =10),xaxis = list(title = SP_reactive_input$simulator_op_summary_ppg$Replaced.Event,showgrid = FALSE),yaxis = list(title = 'Volume Sales',showgrid = FALSE),barmode = 'stack') %>% config(displayModeBar = FALSE)
        })
        shinyjs::show("scenario_analysis")
      }
    }
  })
  ####Showing the metrics for Selected KPI and replaced KPI
  observeEvent(input$SP_sim_replace,{
    SP_reactive_input$SP_replace_event <- hot_to_r(input$SP_sim_run_outcomes_table)
    if(nrow(SP_reactive_input$SP_replace_event[SP_reactive_input$SP_replace_event$`Select Event to be Replaced` == TRUE,]) == 0){
      sendSweetAlert(session,"Error!!","Please select atleast one substitue event",type = "error")
      toggleModal(session, "SP_sim_replace_modal", toggle = "close")
    }else if(nrow(SP_reactive_input$SP_replace_event[SP_reactive_input$SP_replace_event$`Select Event to be Replaced` == TRUE,]) > 1){
      sendSweetAlert(session,"Error!!","Multiple events cannot be selected to replace, Please select single event",type = "error")
      toggleModal(session, "SP_sim_replace_modal", toggle = "close")
    }else if(nrow(SP_reactive_input$SP_replace_event[SP_reactive_input$SP_replace_event$`Select Event to be Replaced` == TRUE,]) == 1){
      if(unique(SP_reactive_input$SP_replace_event$`LSM Constrained/Unconstrained`) == "Unconstrained"){
        SP_reactive_input$SP_replace_event_selected <- SP_reactive_input$SP_replace_event[SP_reactive_input$SP_replace_event$`Select Event to be Replaced` == TRUE,]
        #SP_reactive_input$SP_replace_event_selected$`Replaced Event`
        SP_reactive_input$SP_sim_brand_results <- SP_reactive_input$simulator_op[[2]]
        SP_reactive_input$SP_sim_LSM_Violated <- ""
        SP_reactive_input$SP_replace_compare <- SP_reactive_input$SP_sim_brand_results[SP_reactive_input$SP_sim_brand_results$Replaced.Event %in% c(as.character(SP_reactive_input$SP_replace_event_selected$`Replaced Event`),SP_reactive_input$SP_opti_op_nonlsm_clicked_event_table$`Event ID`),]
        SP_reactive_input$SP_replace_compare <- t(SP_reactive_input$SP_replace_compare)
        colnames(SP_reactive_input$SP_replace_compare) <- SP_reactive_input$SP_replace_compare[1,]
        SP_reactive_input$SP_replace_compare <- as.data.frame(SP_reactive_input$SP_replace_compare)
        
        SP_reactive_input$SP_replace_compare$KPI <- c("KPI","Scan Gross Sales(MM GBP)","Volume Sales(MM Units)","Scan Net Revenue(MM GBP)","Gross Margin(MM GBP)","GM % NR(%)","TI % NR(%)","TI % NIS(%)","Trade ROI","Value Market Share(%)","Base Sales(MM Units)","Incremental Sales(MM Units)")
        SP_reactive_input$SP_replace_compare <- SP_reactive_input$SP_replace_compare[c(-1),]
        
        SP_reactive_input$SP_replace_compare[,c(1,2)] <- apply(SP_reactive_input$SP_replace_compare[,c(1,2)],2, as.numeric)
        SP_reactive_input$SP_replace_compare <- SP_reactive_input$SP_replace_compare[,c(3,1,2)]
        SP_reactive_input$SP_replace_compare$event_sign <- ifelse(SP_reactive_input$SP_replace_compare[,2] > SP_reactive_input$SP_replace_compare[,3], "Positive", ifelse(SP_reactive_input$SP_replace_compare[,2] < SP_reactive_input$SP_replace_compare[,3], "Negative","Neutral"))
        SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",]$event_sign <- ifelse(SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",2] < SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",3], "Positive", ifelse(SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",2] > SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",3], "Negative","Neutral"))
        
        output$SP_sim_replace_compare_table <- renderDataTable({
          datatable(SP_reactive_input$SP_replace_compare,class="cell-border compact hover",extensions = c('FixedColumns'),
                    options = list(
                      columnDefs=list(list(visible=FALSE, targets=c(length(SP_reactive_input$SP_replace_compare)-1))),
                      # paging = FALSE,
                      dom = "t",
                      # searching = FALSE,
                      scrollX = F,
                      scrollY = F
                    ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_replace_compare),textAlign = 'center')%>%
            formatStyle(c(2),"event_sign",
                        background = styleEqual(c("Positive","Negative","Neutral"),c("#84c343","#FF4343","#FFC000"))) %>%
            formatStyle(c(3),"event_sign",
                        background = styleEqual(c("Negative","Positive","Neutral"),c("#84c343","#FF4343","#FFC000"))) %>%
            formatRound(c(2,3),digits = 2)
        })
      }else if(unique(SP_reactive_input$SP_replace_event$`LSM Constrained/Unconstrained`) == "LSM"){
        SP_reactive_input$SP_replace_event_selected <- SP_reactive_input$SP_replace_event[SP_reactive_input$SP_replace_event$`Select Event to be Replaced` == TRUE,]
        SP_reactive_input$SP_sim_brand_results <- SP_reactive_input$simulator_op[[2]]
        
        SP_reactive_input$SP_sim_LSM_Violated <- SP_reactive_input$simulator_op[[5]][[as.character(SP_reactive_input$SP_replace_event_selected$`Replaced Event`)]]
        SP_reactive_input$SP_replace_compare <- SP_reactive_input$SP_sim_brand_results[SP_reactive_input$SP_sim_brand_results$Replaced.Event %in% c(as.character(SP_reactive_input$SP_replace_event_selected$`Replaced Event`),SP_reactive_input$SP_opti_op_lsm_clicked_event_table$`Event ID`),]
        SP_reactive_input$SP_replace_compare <- t(SP_reactive_input$SP_replace_compare)
        colnames(SP_reactive_input$SP_replace_compare) <- SP_reactive_input$SP_replace_compare[1,]
        
        SP_reactive_input$SP_replace_compare <- as.data.frame(SP_reactive_input$SP_replace_compare)
        SP_reactive_input$SP_replace_compare$KPI <- c("KPI","Scan Gross Sales(MM GBP)","Volume Sales(MM Units)","Scan Net Revenue(MM GBP)","Gross Margin(MM GBP)","GM % NR(%)","TI % NR(%)","TI % NIS(%)","Trade ROI","Value Market Share(%)","Base Sales(MM Units)","Incremental Sales(MM Units)")
        SP_reactive_input$SP_replace_compare <- SP_reactive_input$SP_replace_compare[c(-1),]
        SP_reactive_input$SP_replace_compare[,c(1,2)] <- apply(SP_reactive_input$SP_replace_compare[,c(1,2)],2, as.numeric)
        SP_reactive_input$SP_replace_compare <- SP_reactive_input$SP_replace_compare[,c(3,1,2)]
        SP_reactive_input$SP_replace_compare$event_sign <- ifelse(SP_reactive_input$SP_replace_compare[,2] > SP_reactive_input$SP_replace_compare[,3], "Positive", ifelse(SP_reactive_input$SP_replace_compare[,2] < SP_reactive_input$SP_replace_compare[,3], "Negative","Neutral"))
        SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",]$event_sign <- ifelse(SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",2] < SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",3], "Positive", ifelse(SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",2] > SP_reactive_input$SP_replace_compare[SP_reactive_input$SP_replace_compare$KPI == "TI % NR",3], "Negative","Neutral"))
        output$SP_sim_LSM_Violated <- renderText({
          if(SP_reactive_input$SP_sim_LSM_Violated == TRUE){
            return("LSM Constraint gets violated due to replacement of this event")
          }else{
            return("")
          }
        })
        output$SP_sim_replace_compare_table <- renderDataTable({
          datatable(SP_reactive_input$SP_replace_compare,class="cell-border compact hover",extensions = c('FixedColumns'),
                    options = list(
                      columnDefs=list(list(visible=FALSE, targets=c(length(SP_reactive_input$SP_replace_compare)-1))),
                      # paging = FALSE,
                      dom = "t",
                      # searching = FALSE,
                      scrollX = F,
                      scrollY = F
                    ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_replace_compare),textAlign = 'center')%>%
            formatStyle(c(2),"event_sign",
                        background = styleEqual(c("Positive","Negative","Neutral"),c("#84c343","#FF4343","#FFC000"))) %>%
            formatStyle(c(3),"event_sign",
                        background = styleEqual(c("Negative","Positive","Neutral"),c("#84c343","#FF4343","#FFC000"))) %>%
            formatRound(c(2,3),digits = 2)
        })
      }
    }
  })
  
  ###Replacing selected event in optimizer output tab#######Unconstrained #######
  observeEvent(input$SP_sim_replace_confirm,{
    SP_reactive_input$SP_replace_event <- hot_to_r(input$SP_sim_run_outcomes_table)
    if(unique(SP_reactive_input$SP_replace_event$`LSM Constrained/Unconstrained`) == "Unconstrained"){
      ####Updating KPI's
      SP_reactive_input$SP_replace_event_selected_details <- SP_reactive_input$simulator_op[[1]][[as.character(SP_reactive_input$SP_replace_event_selected$`Replaced Event`)]]
      SP_reactive_input$SP_opti_op_prepared <- SP_reactive_input$SP_replace_event_selected_details
      SP_reactive_input$simulated_flag <- 1
      SP_reactive_input$KPI_calculation <- KPI_calc(SP_reactive_input$SP_TPO_list,SP_reactive_input$SP_opti_op_prepared_lsm,SP_reactive_input$SP_opti_op_prepared,SP_reactive_input$SP_opti_exc_brand_lsm,SP_reactive_input$SP_opti_exc_brand,SP_reactive_input$SP_opti_const_grouped,SP_reactive_input$SP_RB_Delist,input$SP_opti_op_include_delist,input$SP_opti_op_include_exclude,input$SP_opti_ROI_selection)
      
      if(input$SP_opti_run_choice %in% c("Run Complete Optimization")){
        lsm_kpi_calc_input <- KPI_calc_input(data.table(SP_reactive_input$SP_opti_op_prepared_lsm),input$SP_opti_ROI_selection)
        nonlsm_kpi_calc_input <- KPI_calc_input(data.table(SP_reactive_input$SP_opti_op_prepared),input$SP_opti_ROI_selection)
      }else if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization")){
        nonlsm_kpi_calc_input <- KPI_calc_input(data.table(SP_reactive_input$SP_opti_op_prepared),input$SP_opti_ROI_selection)
        lsm_kpi_calc_input <- data.frame()
      }else if(input$SP_opti_run_choice %in% c("Run LSM constrained Optimization")){
        lsm_kpi_calc_input <- KPI_calc_input(data.table(SP_reactive_input$SP_opti_op_prepared_lsm),input$SP_opti_ROI_selection)
        nonlsm_kpi_calc_input <- data.frame()
      }
      
      SP_reactive_input$KPI_calculation_ret <- KPI_calc_ret(SP_reactive_input$SP_TPO_list,lsm_kpi_calc_input,nonlsm_kpi_calc_input,SP_reactive_input$SP_opti_exc_brand_lsm,SP_reactive_input$SP_opti_exc_brand,SP_reactive_input$SP_opti_const_grouped,SP_reactive_input$SP_RB_Delist,input$SP_opti_op_include_delist,input$SP_opti_op_include_exclude,input$SP_opti_ROI_selection)
      
      #####Updating Calendar
      SP_reactive_input$opti_cal_filtered <- SP_reactive_input$SP_opti_op_prepared
      if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
        SP_reactive_input$opti_cal_filtered[,ROI := sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
        SP_reactive_input$opti_cal_filtered[,ROI := sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
        SP_reactive_input$opti_cal_filtered[,ROI := sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }
       
      SP_reactive_input$opti_cal_filtered$Date <- ymd(SP_reactive_input$opti_cal_filtered$Date)
      week_no <- data.frame("Date" = sort(unique(SP_reactive_input$opti_cal_filtered$Date)),"Week No" = c(1:length(sort(unique(SP_reactive_input$opti_cal_filtered$Date)))),check.names = FALSE)
      SP_reactive_input$opti_cal_filtered <- left_join(SP_reactive_input$opti_cal_filtered, week_no, by = "Date")
      
      slot_date_col <- if ("Slot_Start_Date" %in% names(SP_reactive_input$opti_cal_filtered)) {
        "Slot_Start_Date"
      } else if ("Start Date" %in% names(SP_reactive_input$opti_cal_filtered)) {
        "Start Date"
      } else {
        "Date"
      }
      
      SP_reactive_input$opti_cal_filtered$Date <- ymd(SP_reactive_input$opti_cal_filtered[[slot_date_col]])
      
      week_no <- data.frame(
        "Date" = sort(unique(SP_reactive_input$opti_cal_filtered$Date)),
        "Week No" = seq_along(sort(unique(SP_reactive_input$opti_cal_filtered$Date))),
        check.names = FALSE
      )
      
      SP_reactive_input$opti_cal_filtered <- left_join(SP_reactive_input$opti_cal_filtered, week_no, by = "Date")
      
      SP_reactive_input$opti_cal_filtered <- left_join(SP_reactive_input$opti_cal_filtered,week_no,by = "Date")
      if(!(is.null(SP_reactive_input$opti_cal_filtered))){
        SP_reactive_input$opti_cal_filtered$Tesco_Event <- ifelse(SP_reactive_input$opti_cal_filtered$Display_Flag == 1,"Display",ifelse(SP_reactive_input$opti_cal_filtered$TPR_Flag == 1,"TPR","No Promo"))
        #SP_reactive_input$opti_cal_filtered$Promo_Price <- round(SP_reactive_input$opti_cal_filtered$Promo_Price,2)
        SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event == "No Promo",]$Promo_Price <- NA_real_
        SP_reactive_input$opti_cal_filtered <- data.table(SP_reactive_input$opti_cal_filtered)
        SP_reactive_input$opti_cal_filtered$ROI_cal <- SP_reactive_input$opti_cal_filtered$ROI
        SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event == "No Promo",]$ROI_cal <- -100
        SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event == "TPR",]$Tesco_Event <- "Shelf Promotion"
        SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event == "Display",]$Tesco_Event <- "Display Feature Promotion"
        SP_reactive_input$brks_ROI_nonLSM <- quantile(SP_reactive_input$opti_cal_filtered[SP_reactive_input$opti_cal_filtered$Tesco_Event != "No Promo" & SP_reactive_input$opti_cal_filtered$ROI_cal >= 0,]$ROI_cal, seq(0,1, .05), na.rm = TRUE)
        SP_reactive_input$opti_cal_filtered$`LSM Promo Price` <- ""
        SP_reactive_input$opti_cal_dcast <- data.table::dcast(unique(SP_reactive_input$opti_cal_filtered[,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price","Week No","Promo_Price","Tesco_Event","ROI_cal")]),Format + PPG + PPG_Description + RSP_Unit + `LSM Promo Price` ~`Week No`,value.var= c("Promo_Price","Tesco_Event","ROI_cal"))
        SP_reactive_input$opti_cal_dcast_ROI <- SP_reactive_input$opti_cal_dcast
        
        names(SP_reactive_input$opti_cal_dcast) <- gsub("Promo_Price_","",names(SP_reactive_input$opti_cal_dcast))
        names(SP_reactive_input$opti_cal_dcast) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast))
        
        names(SP_reactive_input$opti_cal_dcast_ROI) <- gsub("ROI_cal_","",names(SP_reactive_input$opti_cal_dcast_ROI))
        names(SP_reactive_input$opti_cal_dcast_ROI) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_ROI))
        
        setcolorder(SP_reactive_input$opti_cal_dcast,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`))),paste0("ROI_cal","_",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`)))))
        setcolorder(SP_reactive_input$opti_cal_dcast_ROI,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`))),paste0("Promo_Price","_",sort(unique(SP_reactive_input$opti_cal_filtered$`Week No`)))))
        SP_reactive_input$optimizer_op_download <- list("LSM Output" = SP_reactive_input$SP_opti_op_prepared_lsm, "Unconstrained Output" = SP_reactive_input$SP_opti_op_prepared,
                                                        "Excluded PPG" = SP_reactive_input$SP_opti_exc_brand, "EAN PPG Mapping" = SP_reactive_input$EAN_PPG_download)
      }
      
      toggleModal(session, "SP_sim_replace_modal", toggle = "close")
      updateTabItems(session, "sidebar_main", selected = "SP_subMenu6")
      
    }else if(unique(SP_reactive_input$SP_replace_event$`LSM Constrained/Unconstrained`) == "LSM"){
      ####Updating KPI's
      SP_reactive_input$SP_replace_event_selected_details <- SP_reactive_input$simulator_op[[1]][[as.character(SP_reactive_input$SP_replace_event_selected$`Replaced Event`)]]
      SP_reactive_input$SP_opti_op_prepared_lsm <- SP_reactive_input$SP_replace_event_selected_details
      SP_reactive_input$simulated_flag_lsm <- 1
      SP_reactive_input$KPI_calculation <- KPI_calc(SP_reactive_input$SP_TPO_list,SP_reactive_input$SP_opti_op_prepared_lsm,SP_reactive_input$SP_opti_op_prepared,SP_reactive_input$SP_opti_exc_brand_lsm,SP_reactive_input$SP_opti_exc_brand,SP_reactive_input$SP_opti_const_grouped,SP_reactive_input$SP_RB_Delist,input$SP_opti_op_include_delist,input$SP_opti_op_include_exclude,input$SP_opti_ROI_selection)
      
      if(input$SP_opti_run_choice %in% c("Run Complete Optimization")){
        lsm_kpi_calc_input <- KPI_calc_input(data.table(SP_reactive_input$SP_opti_op_prepared_lsm),input$SP_opti_ROI_selection)
        nonlsm_kpi_calc_input <- KPI_calc_input(data.table(SP_reactive_input$SP_opti_op_prepared),input$SP_opti_ROI_selection)
      }else if(input$SP_opti_run_choice %in% c("Run Unconstrained Optimization")){
        nonlsm_kpi_calc_input <- KPI_calc_input(data.table(SP_reactive_input$SP_opti_op_prepared),input$SP_opti_ROI_selection)
        lsm_kpi_calc_input <- data.frame()
      }else if(input$SP_opti_run_choice %in% c("Run LSM constrained Optimization")){
        lsm_kpi_calc_input <- KPI_calc_input(data.table(SP_reactive_input$SP_opti_op_prepared_lsm),input$SP_opti_ROI_selection)
        nonlsm_kpi_calc_input <- data.frame()
      }
      
      SP_reactive_input$KPI_calculation_ret <- KPI_calc_ret(SP_reactive_input$SP_TPO_list,lsm_kpi_calc_input,nonlsm_kpi_calc_input,SP_reactive_input$SP_opti_exc_brand_lsm,SP_reactive_input$SP_opti_exc_brand,SP_reactive_input$SP_opti_const_grouped,SP_reactive_input$SP_RB_Delist,input$SP_opti_op_include_delist,input$SP_opti_op_include_exclude,input$SP_opti_ROI_selection)
      
      
      #####Updating Calendar
      SP_reactive_input$opti_cal_filtered_lsm <- SP_reactive_input$SP_opti_op_prepared_lsm
      if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
        SP_reactive_input$opti_cal_filtered_lsm[,ROI := sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
        SP_reactive_input$opti_cal_filtered_lsm[,ROI := sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
        SP_reactive_input$opti_cal_filtered_lsm[,ROI := sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }
      
      SP_reactive_input$opti_cal_filtered_lsm$Date <- ymd(SP_reactive_input$opti_cal_filtered_lsm$Week_Ending)
      week_no <- data.frame("Date" = sort(unique(SP_reactive_input$opti_cal_filtered_lsm$Date)),"Week No" = c(1:length(sort(unique(SP_reactive_input$opti_cal_filtered_lsm$Week_Ending)))),check.names = FALSE)
      SP_reactive_input$opti_cal_filtered_lsm <- left_join(SP_reactive_input$opti_cal_filtered_lsm,week_no,by = "Date")
      if(!(is.null(SP_reactive_input$opti_cal_filtered_lsm))){
        SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event <- ifelse(SP_reactive_input$opti_cal_filtered_lsm$Display_Flag == 1,"Display",ifelse(SP_reactive_input$opti_cal_filtered_lsm$TPR_Flag == 1,"TPR","No Promo"))
        #SP_reactive_input$opti_cal_filtered_lsm$Promo_Price <- round(SP_reactive_input$opti_cal_filtered_lsm$Promo_Price,2)
        SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event == "No Promo",]$Promo_Price <- NA_real_
        SP_reactive_input$opti_cal_filtered_lsm <- data.table(SP_reactive_input$opti_cal_filtered_lsm)
        SP_reactive_input$opti_cal_filtered_lsm$ROI_cal <- SP_reactive_input$opti_cal_filtered_lsm$ROI
        SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event == "No Promo",]$ROI_cal <- -100
        SP_reactive_input$brks_ROI_LSM <- quantile(SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event != "No Promo" & SP_reactive_input$opti_cal_filtered_lsm$ROI_cal >= 0,]$ROI_cal, seq(0,1, .05), na.rm = TRUE)
        SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event == "TPR",]$Tesco_Event <- "Shelf Promotion"
        SP_reactive_input$opti_cal_filtered_lsm[SP_reactive_input$opti_cal_filtered_lsm$Tesco_Event == "Display",]$Tesco_Event <- "Display Feature Promotion"
        SP_reactive_input$opti_cal_filtered_lsm$`LSM Promo Price` <- ""
        SP_reactive_input$opti_cal_dcast_lsm <- data.table::dcast(unique(SP_reactive_input$opti_cal_filtered_lsm[,c("Format","PPG","PPG_Description","Week No","Promo_Price","RSP_Unit","LSM Promo Price","Tesco_Event","ROI_cal")]),Format + PPG + PPG_Description + RSP_Unit + `LSM Promo Price` ~`Week No`,value.var= c("Promo_Price","Tesco_Event","ROI_cal"))
        
        SP_reactive_input$opti_cal_dcast_lsm_ROI <- SP_reactive_input$opti_cal_dcast_lsm
        names(SP_reactive_input$opti_cal_dcast_lsm) <- gsub("Promo_Price_","",names(SP_reactive_input$opti_cal_dcast_lsm))
        names(SP_reactive_input$opti_cal_dcast_lsm) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_lsm))
        
        names(SP_reactive_input$opti_cal_dcast_lsm_ROI) <- gsub("ROI_cal_","",names(SP_reactive_input$opti_cal_dcast_lsm_ROI))
        names(SP_reactive_input$opti_cal_dcast_lsm_ROI) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_lsm_ROI))
        
        setcolorder(SP_reactive_input$opti_cal_dcast_lsm,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`))),paste0("ROI_cal","_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)))))
        setcolorder(SP_reactive_input$opti_cal_dcast_lsm_ROI,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`))),paste0("Promo_Price","_",sort(unique(SP_reactive_input$opti_cal_filtered_lsm$`Week No`))))) 
        SP_reactive_input$optimizer_op_download <- list("LSM Output" = SP_reactive_input$SP_opti_op_prepared_lsm, "Unconstrained Output" = SP_reactive_input$SP_opti_op_prepared,
                                                        "Excluded PPG" = SP_reactive_input$SP_opti_exc_brand, "EAN PPG Mapping" = SP_reactive_input$EAN_PPG_download)
      }
      toggleModal(session, "SP_sim_replace_modal", toggle = "close")
      updateTabItems(session, "sidebar_main", selected = "SP_subMenu6")
    }
  })
  
  ###TPO ID Selection
  observeEvent({
    input$SP_opti_saved_tpo_cell_clicked},{
      
      if(length(input$SP_opti_saved_tpo_cell_clicked) != 0){
        if(input$SP_opti_saved_tpo_cell_clicked$col == 0){
          #
          SP_reactive_input$SP_tpo_id_selected <- gsub("-","_",input$SP_opti_saved_tpo_cell_clicked$value)
          #SP_reactive_input$SP_tpo_id_data <- SP_reactive_input[[paste0("SP_",SP_reactive_input$SP_tpo_id_selected)]]
          load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",SP_reactive_input$SP_tpo_id_selected,".RData"))
          SP_reactive_input$SP_tpo_id_data <- tpo_list_temp
          updateSelectizeInput(session,"SP_opti_cat",selected = SP_reactive_input$SP_tpo_id_data$cat,choices = unique(SP_reactive_input$SP_nielsen$Category))
          
          # output$SP_opti_start_date_ui <- renderUI({
          #   SP_reactive_input$SP_date_vector <- unique(SP_reactive_input$SP_nielsen$Date)[order(unique(SP_reactive_input$SP_nielsen$Date))]
          #   selectizeInput("SP_opti_date_start","Start Date",choices = SP_reactive_input$SP_date_vector,selected = SP_reactive_input$SP_tpo_id_data$start_date)
          # })
          # 
          # output$SP_opti_end_date_ui <- renderUI({
          #   selectizeInput("SP_opti_date_end","End Date",choices = SP_reactive_input$SP_date_vector,selected = SP_reactive_input$SP_tpo_id_data$end_date)
          # })
          
          observeEvent({
            input$SP_opti_cat},{
              updateSelectizeInput(session,"SP_opti_brand",choices = unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat,]$Brand), selected = SP_reactive_input$SP_tpo_id_data$brand)
            }
          )
          
          observeEvent({
            input$SP_opti_cat
            input$SP_opti_brand},{
              updateSelectizeInput(session,"SP_opti_format",choices = unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand,]$Format), selected = ifelse(SP_reactive_input$SP_tpo_id_data$format == "ALL","",SP_reactive_input$SP_tpo_id_data$format))
            })
          
          updateSelectizeInput(session,"SP_opti_goal",choices = c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Scan Gross Sales","Volume Sales",input$SP_opti_ROI_selection,"Value Market Share"),selected = SP_reactive_input$SP_tpo_id_data$goal_shiny)
          updateSelectizeInput(session,"SP_opti_sign",choices = c("Max","Min"),selected = SP_reactive_input$SP_tpo_id_data$sign)
          #updateAwesomeRadio("SP_opti_constr_scale",choices = c("Absolute","Percentage"),selected = SP_reactive_input$SP_tpo_id_data$constr_scale)
          
          output$SP_opti_restrictions <- renderRHandsontable({
            color_renderer <- "
            function(instance, td) {
            Handsontable.renderers.NumericRenderer.apply(this, arguments);
            td.style.background = '#c2fafe';
            }
            "
            #####Populating  constraint table
            rhandsontable(SP_reactive_input$SP_tpo_id_data$opti_const_shiny,rowHeaders = NULL) %>%
              hot_col("Last Year Value",readOnly = TRUE, renderer = color_renderer) %>%
              hot_col("KPI", type = "dropdown",source = c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Scan Gross Sales","Gross Margin","Volume Sales",SP_reactive_input$SP_tpo_id_data$roi,"Value Market Share")) %>%
              hot_col(c("Last Year Value","Minimum Value","Maximum Value"), format = "0,0.00") %>%
              hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_tpo_id_data$opti_const_shiny)[1]), "Scale", readOnly = TRUE) %>%
              hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_tpo_id_data$opti_const_shiny)[2]), "Scale", readOnly = TRUE) %>%
              hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_tpo_id_data$opti_const_shiny)[3]), "Scale", readOnly = TRUE)
            
          })
          
          output$SP_opti_exclude_ppg <- renderRHandsontable({
            rhandsontable(SP_reactive_input$SP_tpo_id_data$ppg_exclude_shiny,rowHeaders = NULL)
          })
          
          output$SP_opti_prod_restrictions <- renderRHandsontable({
            color_renderer <- "
            function(instance, td) {
            Handsontable.renderers.NumericRenderer.apply(this, arguments);
            td.style.background = '#c2fafe';
            }
            "
            rhandsontable(SP_reactive_input$SP_tpo_id_data$prod_const_shiny,rowHeaders = NULL) %>%
              hot_cols(fixedColumnsLeft = 2) %>%
              hot_col(c("PPG","Brand","Format","Max MRRP","Min MRRP","RSP","LY Investment","LSM Min Floor Price","LSM Max Floor Price","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks"), readOnly = TRUE) %>%
              hot_col(c("PPG","Brand","Format","Max MRRP","Min MRRP","RSP","LY Investment","LSM Min Floor Price","LSM Max Floor Price","LSM Min Total Promo Weeks","LSM Max Total Promo Weeks","LSM Min Display &/or Feature Weeks","LSM Max Display &/or Feature Weeks"), renderer = color_renderer) %>%
              hot_col(c("LY Investment","Min Investment","Max Investment"), format = "0,0.00")
          })
        }
      }
    })
  
  # observeEvent(input$SP_opti_non_lsm,{
  #   if(input$SP_opti_non_lsm >= 1){
  #     show("SP_box_prod_restrictions")
  #   }
  # })
  
  #Navigation buttons in Optimizer tab
  observeEvent(input$SP_opti_top_panel_left,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu3")
  })
  
  observeEvent(input$SP_opti_top_panel_right,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu6")
  })
  
  #Navigation buttons in Optimizer Output tab
  observeEvent(input$SP_opti_op_top_panel_left,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu5")
  })
  
  observeEvent(input$SP_opti_op_top_panel_right,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu8")
  })
  
  #Navigation buttons in simulator tab
  observeEvent(input$SP_sim_top_panel_left,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu6")
  })
  
  observeEvent(input$SP_sim_top_panel_right,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu8")
  })
  
  #Navigation buttons in Compare Scenarios tab
  observeEvent(input$SP_cmp_scn_op_top_panel_left,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu6")
  })
  
  observeEvent(input$SP_cmp_scn_op_top_panel_right,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu9")
  })
  
  
  output$SP_cmp_scn_op_top_panel_download <- downloadHandler(
    # validate(
    #   ,
    #   need(SP_reactive_input$SP_cmp_scn_download,"Select a TPO ID")
    # ),
    filename = function() {
      paste("Optimization_Output_",as.character(SP_reactive_input$SP_cmp_scn_ip$`TPO ID`),".csv", sep="")
    },
    content = function(file) {
      write.csv(SP_reactive_input$SP_cmp_scn_download, file,row.names = FALSE)
    }
  )
  
  output$SP_cmp_scn_table <- renderDataTable({
    #
    validate(
      need(SP_reactive_input$SP_cmp_scn_ip,"Save Optimizers")
    )
    clrs_cmp <- c(round(seq(67, 192, length.out = 11), 0) %>% {paste0("rgb(255,", ., ",","67)")},
                  round(seq(255,132, length.out = 11), 0) %>% {paste0("rgb(", ., ",192,67)")})
    
    SP_cmp_scn_ip_1 <- SP_reactive_input$SP_cmp_scn_ip
    names(SP_cmp_scn_ip_1) <- c("TPO ID","Country","Customer","Category","Brand","Format","PPG","Goal","Goal Objective","LSM Constrained/Unconstrained","Scan Net Revenue(MM GBP)","GM % NR(%)", "TI % NR(%)","Scan Gross Sales(MM GBP)","Volume Sales(MM Units)","Trade ROI")
    #SP_cmp_scn_ip_1[,c("Scan Net Revenue(MM GBP)","GM % NR(%)", "TI % NR(%)","Scan Gross Sales(MM GBP)","Volume Sales(MM Units)","Trade ROI")] <- as.numeric(SP_cmp_scn_ip_1[,c("Scan Net Revenue(MM GBP)","GM % NR(%)", "TI % NR(%)","Scan Gross Sales(MM GBP)","Volume Sales(MM Units)","Trade ROI")])
    
    datatable(SP_cmp_scn_ip_1,class="cell-border compact hover",extensions = c('FixedColumns'),
              options = list(
                fixedColumns = list(leftColumns = 1),
                columnDefs=list(list(width = '150px',targets= 0)),
                pageLength = 20,
                lengthMenu = c(20, 50, 70, 100),
                dom = 'blftrip',
                scrollX = TRUE,
                scrollY = "400px"
              ),rownames = F) %>% 
      formatStyle("Scan Net Revenue(MM GBP)",
                  background = styleInterval(quantile(SP_cmp_scn_ip_1$`Scan Net Revenue(MM GBP)`, seq(0, 1, .05), na.rm = TRUE),clrs_cmp)) %>%
      formatStyle("GM % NR(%)",
                  background = styleInterval(quantile(SP_cmp_scn_ip_1$`GM % NR(%)`, seq(0, 1, .05), na.rm = TRUE),clrs_cmp)) %>%
      formatStyle("TI % NR(%)",
                  background = styleInterval(quantile(SP_cmp_scn_ip_1$`TI % NR(%)`, seq(0, 1, .05), na.rm = TRUE),clrs_cmp)) %>%
      formatStyle("Volume Sales(MM Units)",
                  background = styleInterval(quantile(SP_cmp_scn_ip_1$`Volume Sales(MM Units)`,seq(0, 1, .05), na.rm = TRUE),rev(clrs_cmp))) %>%
      formatStyle("Trade ROI",
                  background = styleInterval(quantile(SP_cmp_scn_ip_1$`Trade ROI`, seq(0, 1, .05), na.rm = TRUE),clrs_cmp)) %>%
      formatStyle("Scan Gross Sales(MM GBP)",
                  background = styleInterval(quantile(SP_cmp_scn_ip_1$`Scan Gross Sales(MM GBP)`, seq(0, 1, .05), na.rm = TRUE),clrs_cmp)) %>%
      formatStyle(names(SP_cmp_scn_ip_1),textAlign = 'center') %>%
      formatRound(names(select_if(SP_cmp_scn_ip_1,is.numeric)), digits = 2) %>%
      formatCurrency(names(select_if(SP_cmp_scn_ip_1,is.numeric)),currency = "", interval = 3, mark = ",")
    
  })
  
  output$SP_cmp_scn_merge_table <- renderRHandsontable({
    SP_cmp_scn_merge_ip <- SP_reactive_input$SP_cmp_scn_ip[,c("TPO ID","Country","Customer","Category","Brand","Format","PPG","Goal","Goal Objective","LSM Constrained/Unconstrained")]
    SP_cmp_scn_merge_ip <- cbind(data.frame("Select" = rep(FALSE,nrow(SP_cmp_scn_merge_ip))),SP_cmp_scn_merge_ip)
    SP_reactive_input$SP_cmp_scn_merge_ip <- SP_cmp_scn_merge_ip[SP_cmp_scn_merge_ip$Format != "ALL",]
    rhandsontable(SP_reactive_input$SP_cmp_scn_merge_ip,rowHeaders = NULL)
  })
  
  #Saving LSM Output
  observeEvent(input$SP_cmp_scn_merge_name,{
    ###Saving TPO ID table
    
    if(input$SP_opti_save_name_merge != ""){
      if(file.exists(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",input$SP_opti_save_name_merge,".RData"))){
        confirmSweetAlert(session,"SP_cmp_scn_merge_confirm",title = paste0(input$SP_opti_save_name_merge," already exists in the Saved Plans"),text = "Confirm replacing the saved plan!!",type = "warning")
      }else{
        confirmSweetAlert(session,"SP_cmp_scn_merge_confirm",title = paste0("Saving Merged optimizer plan",input$SP_opti_save_name_merge),text = "Confirm Saving Merged optimizer plan!!",type = "success")
      }
    }else{
      sendSweetAlert(session,"Error!!","Optimizer plan name cannot be blank",type = "error")
    }
    print(input$confirm_save_lsm)
  })
  
  observeEvent(input$SP_cmp_scn_merge_confirm,{
    #warning_flag_merge = 0
    
    SP_reactive_input$SP_cmp_scn_merge_op <- hot_to_r(input$SP_cmp_scn_merge_table)
    SP_reactive_input$SP_cmp_scn_merge_op <- SP_reactive_input$SP_cmp_scn_merge_op[SP_reactive_input$SP_cmp_scn_merge_op$Select == TRUE,]
    SP_reactive_input$SP_cmp_scn_merge_op$`TPO ID` <- as.character(SP_reactive_input$SP_cmp_scn_merge_op$`TPO ID`)
    if(nrow(SP_reactive_input$SP_cmp_scn_merge_op) == 0){
      sendSweetAlert(session,"Error!!","No plans are selected to merge",type = "error")
    }else if(length(unique(unlist(str_split(SP_reactive_input$SP_cmp_scn_merge_op$PPG,",")))) < length(unlist(str_split(SP_reactive_input$SP_cmp_scn_merge_op$PPG,",")))){
      sendSweetAlert(session,"Error!!","Multiple plans of same PPG are selected to merge",type = "error")
    }else if(any(SP_reactive_input$SP_cmp_scn_merge_op$PPG == "ALL") & (length(unique(unlist(str_split(SP_reactive_input$SP_cmp_scn_merge_op$Format,",")))) < length(unlist(str_split(SP_reactive_input$SP_cmp_scn_merge_op$Format,","))))){
      sendSweetAlert(session,"Error!!","Multiple plans of same format are selected to merge",type = "error")
    }else{
      merged_tpo_op_list <- list()
      merged_tpo_exclude_ppg_list <- list()
      merged_tpo_exclude_ppg_shiny_input <- list()
      merged_tpo_exclude_ppg_input <- list()
      merged_tpo_other_sales <- list()
      merged_tpo_other_sales_value <- list()
      
      for (i in SP_reactive_input$SP_cmp_scn_merge_op$`TPO ID`) {
        load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",i,".RData"))
        merged_tpo_op_list[[i]] <- tpo_list_temp$opti_output
        merged_tpo_exclude_ppg_list[[i]] <- tpo_list_temp$excluded_brand
        merged_tpo_exclude_ppg_shiny_input[[i]] <- tpo_list_temp$ppg_exclude_shiny
        merged_tpo_exclude_ppg_input[[i]] <- tpo_list_temp$ppg_exclude
        merged_tpo_other_sales[[i]] <- tpo_list_temp$other_sales
        merged_tpo_other_sales_value[[i]] <- tpo_list_temp$other_sales_value
      }
      merged_tpo_op <- do.call(rbind,merged_tpo_op_list)
      merged_tpo_exclude <- do.call(rbind,merged_tpo_exclude_ppg_list)
      merged_tpo_exclude_shiny_input <- do.call(rbind,merged_tpo_exclude_ppg_shiny_input)
      merged_tpo_exclude_input <- do.call(rbind,merged_tpo_exclude_ppg_input)
      merged_tpo_other_sales <- do.call(sum,merged_tpo_other_sales)
      merged_tpo_other_sales_value <- do.call(sum,merged_tpo_other_sales_value)
      
      SP_reactive_input$SP_cmp_scn_ip_latest <- data.frame("TPO ID" = input$SP_opti_save_name_merge,"Country" = unique(SP_reactive_input$SP_cmp_scn_merge_op$Country),"Customer" = unique(SP_reactive_input$SP_cmp_scn_merge_op$Customer),"Category" = unique(SP_reactive_input$SP_cmp_scn_merge_op$Category),
                                                           "Brand" = unique(SP_reactive_input$SP_cmp_scn_merge_op$Brand),"Format" = paste(unique(unlist(str_split(SP_reactive_input$SP_cmp_scn_merge_op$Format,","))),collapse = ","),"PPG" = paste(unique(unlist(str_split(SP_reactive_input$SP_cmp_scn_merge_op$PPG,","))),collapse = ","),"Goal" = "",
                                                           "Goal Objective" = "", "LSM Constrained/Unconstrained" = "",
                                                           "Scan Net Revenue" =  (sum(merged_tpo_op$Net_Revenue) + sum(merged_tpo_exclude$Net_Revenue))/10^6,
                                                           "GM % NR" = (sum(merged_tpo_op$GM_Abs) + sum(merged_tpo_exclude$GM_Abs))* 100/(sum(merged_tpo_op$Net_Revenue) + sum(merged_tpo_exclude$Net_Revenue)),
                                                           "TI % NR" = (sum(merged_tpo_op$Total_Trade_Investment) + sum(merged_tpo_exclude$Total_Trade_Investment)) * 100/(sum(merged_tpo_op$Net_Revenue) + sum(merged_tpo_exclude$Net_Revenue)),
                                                           "Gross Sales" = (sum(merged_tpo_op$Gross_Sales) + sum(merged_tpo_exclude$Gross_Sales))/10^6,
                                                           "Trade ROI" = (sum(merged_tpo_op$R_GM_Inc) + sum(merged_tpo_exclude$R_GM_Inc))/(sum(merged_tpo_op$R_Trade_Inv_Inc) + sum(merged_tpo_exclude$R_Trade_Inv_Inc)),
                                                           "Volume Sales" = (sum(merged_tpo_op$Total_Sales) + sum(merged_tpo_exclude$Total_Sales))/10^6,check.names = FALSE)
      #
      tpo_list_temp <- list("coun" = unique(SP_reactive_input$SP_cmp_scn_merge_op$Country), "cat" = unique(SP_reactive_input$SP_cmp_scn_merge_op$Category), "cust" = unique(SP_reactive_input$SP_cmp_scn_merge_op$Customer),
                            "brand" = unique(SP_reactive_input$SP_cmp_scn_merge_op$Brand), "format" = unique(unlist(str_split(SP_reactive_input$SP_cmp_scn_merge_op$Format,","))),"ppg" = unique(unlist(str_split(SP_reactive_input$SP_cmp_scn_merge_op$PPG,","))),
                            "opti_output" = data.table(merged_tpo_op), "excluded_brand" = data.table(merged_tpo_exclude),"ppg_exclude_shiny" = merged_tpo_exclude_shiny_input,"ppg_exclude"= merged_tpo_exclude_input, "Compare_Scenario" = SP_reactive_input$SP_cmp_scn_ip_latest, "other_sales" = merged_tpo_other_sales, "other_sales_value" = merged_tpo_other_sales_value)
      save(tpo_list_temp,file = paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",input$SP_opti_save_name_merge,".RData"))
      
      SP_reactive_input$SP_cmp_scn_ip <- rbind(SP_reactive_input$SP_cmp_scn_ip,SP_reactive_input$SP_cmp_scn_ip_latest)
      toggleModal(session, "SP_merge_popup", toggle = "close")
    }
  })
  
  #Navigation buttons in KAM Cockpit tab
  observeEvent(input$SP_kam_top_panel_left,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu8")
  })
  
  observeEvent(input$SP_kam_top_panel_right,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu10")
  })
  
  observeEvent(input$SP_cmp_scn_table_cell_clicked,{
    output$SP_kam_start_date_ui <- renderUI({
      #SP_reactive_input$SP_date_vector <- unique(SP_reactive_input$SP_nielsen$Date)[order(unique(SP_reactive_input$SP_nielsen$Date))]
      selectizeInput("SP_kam_date_start","Start Date",choices = "")
    })
    
    output$SP_kam_end_date_ui <- renderUI({
      selectizeInput("SP_kam_date_end","End Date",choices = "")
    })
    
    if(length(input$SP_cmp_scn_table_cell_clicked) != 0){
      if(input$SP_cmp_scn_table_cell_clicked$col == 0){
        
        showModal(
          modalDialog(
            title = "",
            HTML(paste0("Click on Proceed to KAM Cockpit to finalize the plan or Download the output of the Optimized plan","<br>","","<br>","")),
            size = "l",
            downloadButton("SP_cmp_scn_popup_download","Download the Optimized Output",icon = icon("download"),style = "color:#fff; background-color: #0099DC;border-color: #0099DC"),
            actionButton("SP_cmp_scn_popup_proceed_KAM","Finalize the selected plan & Proceed to KAM Cockpit",icon = icon("arrow-right"),style="color: #fff; background-color: #84C343"),
            easyClose = TRUE
            # footer = tagList(
            #   
            # )
          )
        )
        
      }
    }
  })
  
  output$SP_cmp_scn_popup_download <- downloadHandler(
    filename = function() {
      paste0("Optimization_Output_",input$SP_cmp_scn_table_cell_clicked$value,".csv")
    },
    content = function(file) {
      load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",gsub("-","_",input$SP_cmp_scn_table_cell_clicked$value),".RData"))
      SP_reactive_input$SP_TPO_selected <- tpo_list_temp
      removeModal()
      write.csv(SP_reactive_input$SP_TPO_selected$opti_output, file,row.names = FALSE)
    }
  )
  
  # observeEvent(input$SP_cmp_scn_popup_download,{
  #   
  #   write.csv(tpo_list_temp$opti_output,paste0("Optimization_Output_",input$SP_cmp_scn_table_cell_clicked$value,".csv"),row.names = FALSE)
  #   removeModal()
  # })
  
  observeEvent(input$SP_cmp_scn_popup_proceed_KAM,{
    
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu9")
    load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",gsub("-","_",input$SP_cmp_scn_table_cell_clicked$value),".RData"))
    tpo_list_temp$tpo_id <- gsub("-","_",input$SP_cmp_scn_table_cell_clicked$value)
    SP_reactive_input$SP_TPO_selected <- tpo_list_temp
    
    save(tpo_list_temp,file = paste0(SP_reactive_input$folder_path,"/Final_Optimized_Plan.RData"))
    updateSelectizeInput(session,"SP_kam_cat",selected = as.character(SP_reactive_input$SP_TPO_selected$cat),choices = as.character(SP_reactive_input$SP_TPO_selected$cat))
    updateSelectizeInput(session,"SP_kam_brand",choices = as.character(SP_reactive_input$SP_TPO_selected$brand),selected = as.character(SP_reactive_input$SP_TPO_selected$brand))      
    updateSelectizeInput(session,"SP_kam_format",choices = as.character(SP_reactive_input$SP_TPO_selected$format),selected = as.character(SP_reactive_input$SP_TPO_selected$format))
    updateSelectizeInput(session,"SP_kam_tpo",choices = as.character(SP_reactive_input$SP_TPO_selected$tpo_id),selected = as.character(SP_reactive_input$SP_TPO_selected$tpo_id))
    
    SP_reactive_input$SP_TPO_selected_weekly <- SP_reactive_input$SP_TPO_selected$opti_output[SP_reactive_input$SP_TPO_selected$opti_output$`Category` == SP_reactive_input$SP_TPO_selected$Compare_Scenario$Category & SP_reactive_input$SP_TPO_selected$opti_output$`Brand` == SP_reactive_input$SP_TPO_selected$Compare_Scenario$Brand,.("Net Invoice Sales" = sum(NIS),"CPD" = sum(Retro_Funding_Total), "TI" = sum(Total_Trade_Investment),"Scan Net Revenue" = sum(Net_Revenue),"Gross Margin" = sum(GM_Abs), "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                                                                                                                                                                                                                                                            "Trade ROI" = ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc))),
                                                                                                                                                                                                                                                                                                                                            "Inc_GM_Abs" = sum(Inc_GM_Abs),"R_GM_Inc" = sum(R_GM_Inc),"Inc_Revenue" = sum(Inc_Revenue), "R_Net_Rev_Inc" = sum(R_Net_Rev_Inc), "Inc_NIS" = sum(Inc_NIS), "R_NIS_Inc" = sum(R_NIS_Inc), "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue),"Retailer_Revenue" = sum(Retailer_Revenue),
                                                                                                                                                                                                                                                                                                                                            "CPD % NIS" = sum(Retro_Funding_Total) * 100/sum(NIS), "Total_Sales" = sum(Total_Sales),"R_Trade_Inv_Inc" = sum(R_Trade_Inv_Inc),
                                                                                                                                                                                                                                                                                                                                            "RSV" = sum(Retailer_Revenue),"COGS" = sum(Net_Cost_Unit * Total_Sales),"Front Margin" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales), "FM %" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales)* 100/sum(Retailer_Revenue/(1+VAT)), 
                                                                                                                                                                                                                                                                                                                                            "Fixed" = sum(Display_Cost), "Back Margin" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales) + sum(Display_Cost), "BM %" = (sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales) + sum(Display_Cost)) * 100/sum(Retailer_Revenue/(1+VAT))),by = .(`Category`,`Brand`,Week_Ending)]
    
    SP_reactive_input$SP_TPO_selected$excluded_brand <- data.table(SP_reactive_input$SP_TPO_selected$excluded_brand)
    
    SP_reactive_input$SP_TPO_selected_exclude_ppg <- SP_reactive_input$SP_TPO_selected$excluded_brand[SP_reactive_input$SP_TPO_selected$excluded_brand$`Category` == SP_reactive_input$SP_TPO_selected$Compare_Scenario$Category & SP_reactive_input$SP_TPO_selected$excluded_brand$`Brand` == SP_reactive_input$SP_TPO_selected$Compare_Scenario$Brand,.("Net Invoice Sales" = sum(NIS),"CPD" = sum(Retro_Fund_Total), "TI" = sum(Trade_Investment),"Scan Net Revenue" = sum(Net_Revenue),"Gross Margin" = sum(GM_Abs), "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                                                                                                                                                                                                                                                                          "Trade ROI" = ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",sum(R_GM_Inc)/sum(R_Total_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",sum(R_Net_Revenue_Inc)/sum(R_Total_Trade_Inv_Inc),sum(R_NIS_Inc)/sum(R_Total_Trade_Inv_Inc))),
                                                                                                                                                                                                                                                                                                                                                          "Inc_GM_Abs" = sum(Inc_GM_Abs),"R_GM_Inc" = sum(R_GM_Inc),"Inc_Revenue" = sum(Inc_Revenue), "R_Net_Rev_Inc" = sum(R_Net_Revenue_Inc), "Inc_NIS" = sum(Inc_NIS), "R_NIS_Inc" = sum(R_NIS_Inc), "TI % NR" = sum(Trade_Investment) * 100/sum(Net_Revenue),"Retailer_Revenue" = sum(Retailer_Revenue),
                                                                                                                                                                                                                                                                                                                                                          "CPD % NIS" = sum(Retro_Fund_Total) * 100/sum(NIS), "Total_Sales" = sum(Units),"R_Trade_Inv_Inc" = sum(R_Total_Trade_Inv_Inc),
                                                                                                                                                                                                                                                                                                                                                          "RSV" = sum(Retailer_Revenue),"COGS" = sum(Net_Cost_Total),"Front Margin" = sum(FM_Total), "FM %" = sum(FM_Total) * 100/sum(Retailer_Revenue/(1+VAT)), 
                                                                                                                                                                                                                                                                                                                                                          "Fixed" = sum(Display_Cost), "Back Margin" = sum(FM_Total) + sum(Display_Cost), "BM %" = (sum(FM_Total) + sum(Display_Cost)) * 100/sum(Retailer_Revenue/(1+VAT))),by = .(`Category`,`Brand`)]
    
    #SP_reactive_input$SP_cmp_scn_selected <- t(SP_reactive_input$SP_TPO_selected$Compare_Scenario[,c("Scan Net Revenue","GM % NR","TI % NR","Gross Sales","Trade ROI","Volume Sales")])
    
    SP_reactive_input$SP_cmp_scn_selected_epos <- data.frame("KPI Metrics" = c("Net Invoice Sales","CPD","TI","Scan Net Revenue","Gross Margin","GM % NR","Trade ROI","TI % NR","CPD % NIS","Value Market Share"),
                                                             "Predicted" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Net Invoice Sales`),
                                                                             sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD),
                                                                             sum(SP_reactive_input$SP_TPO_selected_weekly$TI) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$TI),
                                                                             sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`),
                                                                             sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Gross Margin`),
                                                                             (sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Gross Margin`)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`)),
                                                                             ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",(sum(SP_reactive_input$SP_TPO_selected_weekly$R_GM_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_GM_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Net_Rev_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Net_Rev_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc)+ sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)),(sum(SP_reactive_input$SP_TPO_selected_weekly$R_NIS_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_NIS_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)))),
                                                                             (sum(SP_reactive_input$SP_TPO_selected_weekly$TI) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$TI)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`)),
                                                                             (sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Net Invoice Sales`)),
                                                                             (sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Retailer_Revenue)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Retailer_Revenue) + SP_reactive_input$SP_TPO_selected$other_sales_value)),
                                                             "Actual" = rep("",10),
                                                             "Plan Projection" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Net Invoice Sales`),
                                                                                   sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD),
                                                                                   sum(SP_reactive_input$SP_TPO_selected_weekly$TI) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$TI),
                                                                                   sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`),
                                                                                   sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Gross Margin`),
                                                                                   (sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Gross Margin`)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`)),
                                                                                   ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",(sum(SP_reactive_input$SP_TPO_selected_weekly$R_GM_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_GM_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Net_Rev_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Net_Rev_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc)+ sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)),(sum(SP_reactive_input$SP_TPO_selected_weekly$R_NIS_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_NIS_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)))),
                                                                                   (sum(SP_reactive_input$SP_TPO_selected_weekly$TI) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$TI)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`)),
                                                                                   (sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Net Invoice Sales`)),
                                                                                   (sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Retailer_Revenue)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Retailer_Revenue) + SP_reactive_input$SP_TPO_selected$other_sales_value)),
                                                             
                                                             "Forecast Projection" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Net Invoice Sales`),
                                                                                       sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD),
                                                                                       sum(SP_reactive_input$SP_TPO_selected_weekly$TI) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$TI),
                                                                                       sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`),
                                                                                       sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Gross Margin`),
                                                                                       (sum(SP_reactive_input$SP_TPO_selected_weekly$`Gross Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Gross Margin`)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`)),
                                                                                       ifelse(input$SP_opti_ROI_selection == "Incremental GM ROI",(sum(SP_reactive_input$SP_TPO_selected_weekly$R_GM_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_GM_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)),ifelse(input$SP_opti_ROI_selection == "Incremental NR ROI",(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Net_Rev_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Net_Rev_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc)+ sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)),(sum(SP_reactive_input$SP_TPO_selected_weekly$R_NIS_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_NIS_Inc))/(sum(SP_reactive_input$SP_TPO_selected_weekly$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$R_Trade_Inv_Inc)))),
                                                                                       (sum(SP_reactive_input$SP_TPO_selected_weekly$TI) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$TI)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Scan Net Revenue`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Scan Net Revenue`)),
                                                                                       (sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$`Net Invoice Sales`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Net Invoice Sales`)),
                                                                                       (sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Retailer_Revenue)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$Retailer_Revenue) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Retailer_Revenue) + SP_reactive_input$SP_TPO_selected$other_sales_value)),check.names = FALSE)
    
    SP_reactive_input$SP_cmp_scn_selected_epos[SP_reactive_input$SP_cmp_scn_selected_epos$`KPI Metrics` %in% c("Net Invoice Sales","CPD","TI","Scan Net Revenue","Gross Margin"), c("Predicted", "Plan Projection","Forecast Projection")] <- SP_reactive_input$SP_cmp_scn_selected_epos[SP_reactive_input$SP_cmp_scn_selected_epos$`KPI Metrics` %in% c("Net Invoice Sales","CPD","TI","Scan Net Revenue","Gross Margin"), c("Predicted", "Plan Projection","Forecast Projection")]/10^6
    SP_reactive_input$SP_cmp_scn_selected_epos$`KPI Metrics` <- c("Net Invoice Sales(MM GBP)","CPD(MM GBP)","TI(MM GBP)","Scan Net Revenue(MM GBP)","Gross Margin(MM GBP)","GM % NR","Trade ROI","TI % NR","CPD % NIS","Value Market Share")
    
    # SP_reactive_input$SP_TPO_selected_epos_table <- rbind(data.frame("KPI Metrics" = rownames(SP_reactive_input$SP_cmp_scn_selected),"Predicted" = rep("",nrow(SP_reactive_input$SP_cmp_scn_selected)), "Actual" = rep("",nrow(SP_reactive_input$SP_cmp_scn_selected)),"Plan Projection" = SP_reactive_input$SP_cmp_scn_selected[,1], "Forecast Projection" = SP_reactive_input$SP_cmp_scn_selected[,1],check.names = FALSE),
    #                                                       SP_reactive_input$SP_cmp_scn_selected_1)
    
    SP_reactive_input$SP_cmp_scn_selected_cust <- data.frame("KPI Metrics" = c("RSV", "COGS","Front Margin","FM %","CPD","Fixed","Back Margin","BM %"),
                                                             "Predicted" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV),sum(SP_reactive_input$SP_TPO_selected_weekly$COGS) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$COGS),
                                                                             sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Front Margin`),(sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Front Margin`))* 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV/1.2)),
                                                                             sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD),
                                                                             sum(SP_reactive_input$SP_TPO_selected_weekly$Fixed) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Fixed),
                                                                             sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Back Margin`),
                                                                             (sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Back Margin`)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV/1.2))),
                                                             "Actual" = rep("",8),
                                                             "Plan Projection" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV),sum(SP_reactive_input$SP_TPO_selected_weekly$COGS) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$COGS),
                                                                                   sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Front Margin`),(sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Front Margin`))* 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV/1.2)),
                                                                                   sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD),
                                                                                   sum(SP_reactive_input$SP_TPO_selected_weekly$Fixed) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Fixed),
                                                                                   sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Back Margin`),
                                                                                   (sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Back Margin`)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV/1.2))),
                                                             "Forecast Projection" = c(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV),sum(SP_reactive_input$SP_TPO_selected_weekly$COGS) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$COGS),
                                                                                       sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Front Margin`),(sum(SP_reactive_input$SP_TPO_selected_weekly$`Front Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Front Margin`))* 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV/1.2)),
                                                                                       sum(SP_reactive_input$SP_TPO_selected_weekly$CPD) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$CPD),
                                                                                       sum(SP_reactive_input$SP_TPO_selected_weekly$Fixed) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$Fixed),
                                                                                       sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Back Margin`),
                                                                                       (sum(SP_reactive_input$SP_TPO_selected_weekly$`Back Margin`) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$`Back Margin`)) * 100/(sum(SP_reactive_input$SP_TPO_selected_weekly$RSV/1.2) + sum(SP_reactive_input$SP_TPO_selected_exclude_ppg$RSV/1.2))),check.names = FALSE)
    
    SP_reactive_input$SP_cmp_scn_selected_cust[SP_reactive_input$SP_cmp_scn_selected_cust$`KPI Metrics` %in% c("RSV","COGS","Front Margin","CPD","Fixed","Back Margin"),c("Predicted", "Plan Projection","Forecast Projection")] <- SP_reactive_input$SP_cmp_scn_selected_cust[SP_reactive_input$SP_cmp_scn_selected_cust$`KPI Metrics` %in% c("RSV","COGS","Front Margin","CPD","Fixed","Back Margin"),c("Predicted", "Plan Projection","Forecast Projection")]/10^6
    SP_reactive_input$SP_cmp_scn_selected_cust$`KPI Metrics` <- c("RSV(MM GBP)", "COGS(MM GBP)","FM(MM GBP)","FM %","CPD(MM GBP)","Fixed(MM GBP)","BM(MM GBP)","BM %")
    removeModal()
    output$SP_kam_RB_table <- renderDataTable({
      datatable(SP_reactive_input$SP_cmp_scn_selected_epos,class="cell-border stripe",extensions = c('FixedColumns'),
                options = list(
                  fixedColumns = list(leftColumns = 1),
                  # paging = FALSE,
                  dom = "t",
                  # searching = FALSE,
                  scrollX = TRUE,
                  scrollY = "250px"
                ),rownames = F) %>%
        formatStyle(names(SP_reactive_input$SP_cmp_scn_selected_epos),textAlign = 'center') %>%
        formatRound(names(select_if(SP_reactive_input$SP_cmp_scn_selected_epos,is.numeric)), digits = 2)
      
    })
    
    SP_reactive_input$SP_TPO_selected_epos_chart_RB <- SP_reactive_input$SP_TPO_selected_weekly[,c("Date","Net Invoice Sales"),with = FALSE]
    ###Actual vs predicted plot
    output$SP_kam_RB_chart <- renderPlotly({
      plot_ly(SP_reactive_input$SP_TPO_selected_epos_chart_RB) %>%
        add_trace(x = ~Week_Ending,y = ~`Net Invoice Sales`, type = 'scatter', mode = 'lines', line = list(width = 1,color = "#0099DC"),
                  hovertext = paste0(SP_reactive_input$SP_TPO_selected_epos_chart_RB$Week_Ending,", ",round(SP_reactive_input$SP_TPO_selected_epos_chart_RB[["Net Invoice Sales"]],2)),hoverinfo = 'text',
                  name = 'Predicted') %>%
        layout(showlegend = TRUE,margin = list(r= 60),xaxis = list(title = 'Time Period'),paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',yaxis = list(side = 'left',title = "Net Invoice Sales", zeroline = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
    })
    
    output$SP_kam_cust_table <- renderDataTable({
      datatable(SP_reactive_input$SP_cmp_scn_selected_cust,class="cell-border stripe",extensions = c('FixedColumns'),
                options = list(
                  fixedColumns = list(leftColumns = 1),
                  # paging = FALSE,
                  dom = "t",
                  # searching = FALSE,
                  scrollX = TRUE,
                  scrollY = "250px"
                ),rownames = F) %>%
        formatStyle(names(SP_reactive_input$SP_cmp_scn_selected_cust),textAlign = 'center') %>%
        formatRound(names(select_if(SP_reactive_input$SP_cmp_scn_selected_cust,is.numeric)), digits = 2)
    })
    
    SP_reactive_input$SP_TPO_selected_epos_chart_cust <- SP_reactive_input$SP_TPO_selected_weekly[,c("Date","RSV"),with = FALSE]
    ###Actual vs predicted plot
    output$SP_kam_cust_chart <- renderPlotly({
      plot_ly(SP_reactive_input$SP_TPO_selected_epos_chart_cust) %>%
        add_trace(x = ~Week_Ending,y = ~RSV, type = 'scatter', mode = 'lines', line = list(width = 1,color = "#0099DC"),
                  hovertext = paste0(SP_reactive_input$SP_TPO_selected_epos_chart_cust$Week_Ending,", ",round(SP_reactive_input$SP_TPO_selected_epos_chart_cust[["RSV"]],2)),hoverinfo = 'text',
                  name = 'Predicted') %>%
        layout(showlegend = TRUE,margin = list(r= 60),xaxis = list(title = 'Time Period'),paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',yaxis = list(side = 'left',title = "RSV", zeroline = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
    })
  })
  
  observeEvent(input$SP_kam_tpo,{
    
  })
  
  observeEvent(input$SP_kam_RB_table_cell_clicked,{
    
    if(length(input$SP_kam_RB_table_cell_clicked != 0)){
      if(input$SP_kam_RB_table_cell_clicked$col == 0){
        SP_reactive_input$RB_table_mapping <- data.frame("KPI_actual" = c("Net Invoice Sales","CPD","TI","Scan Net Revenue","Gross Margin","GM % NR","Trade ROI","TI % NR","CPD % NIS","Value Market Share"), "KPI_screen" = c("Net Invoice Sales(MM GBP)","CPD(MM GBP)","TI(MM GBP)","Scan Net Revenue(MM GBP)","Gross Margin(MM GBP)","GM % NR","Trade ROI","TI % NR","CPD % NIS","Value Market Share"))
        clicked_value <- as.character(SP_reactive_input$RB_table_mapping[SP_reactive_input$RB_table_mapping$KPI_screen == input$SP_kam_RB_table_cell_clicked$value,]$KPI_actual)
        SP_reactive_input$SP_TPO_selected_epos_chart_RB <- SP_reactive_input$SP_TPO_selected_weekly[,c("Date",clicked_value),with = FALSE]
        ###Actual vs predicted plot
        output$SP_kam_RB_chart <- renderPlotly({
          plot_ly(SP_reactive_input$SP_TPO_selected_epos_chart_RB) %>%
            add_trace(x = ~Week_Ending,y = ~(get(clicked_value)), type = 'scatter', mode = 'lines', line = list(width = 1,color = "#0099DC"),
                      hovertext = paste0(SP_reactive_input$SP_TPO_selected_epos_chart_RB$Week_Ending,", ",round(SP_reactive_input$SP_TPO_selected_epos_chart_RB[[clicked_value]],2)),hoverinfo = 'text',
                      name = 'Predicted') %>%
            layout(showlegend = TRUE,margin = list(r= 60),xaxis = list(title = 'Time Period'),paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',yaxis = list(side = 'left',title = clicked_value, zeroline = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
        })
      }
    }
  })
  
  observeEvent(input$SP_kam_cust_table_cell_clicked,{
    ###Actual vs predicted plot
    if(length(input$SP_kam_cust_table_cell_clicked != 0)){
      if(input$SP_kam_cust_table_cell_clicked$col == 0){
        SP_reactive_input$cust_table_mapping <- data.frame("KPI_actual" = c("RSV", "COGS","Front Margin","FM %","CPD","Fixed","Back Margin","BM %"), "KPI_screen" = c("RSV(MM GBP)", "COGS(MM GBP)","FM(MM GBP)","FM %","CPD(MM GBP)","Fixed(MM GBP)","BM(MM GBP)","BM %"))
        clicked_value <- as.character(SP_reactive_input$cust_table_mapping[SP_reactive_input$cust_table_mapping$KPI_screen == input$SP_kam_cust_table_cell_clicked$value,]$KPI_actual)
        SP_reactive_input$SP_TPO_selected_epos_chart_cust <- SP_reactive_input$SP_TPO_selected_weekly[,c("Date",clicked_value),with = FALSE]
        ###Actual vs predicted plot
        output$SP_kam_cust_chart <- renderPlotly({
          plot_ly(SP_reactive_input$SP_TPO_selected_epos_chart_cust) %>%
            add_trace(x = ~Week_Ending,y = ~(get(clicked_value)), type = 'scatter', mode = 'lines', line = list(width = 1,color = "#0099DC"),
                      hovertext = paste0(SP_reactive_input$SP_TPO_selected_epos_chart_cust$Week_Ending,", ",round(SP_reactive_input$SP_TPO_selected_epos_chart_cust[[clicked_value]],2)),hoverinfo = 'text',
                      name = 'Predicted') %>%
            layout(showlegend = TRUE,margin = list(r= 60),xaxis = list(title = 'Time Period'),paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',yaxis = list(side = 'left',title = clicked_value, zeroline = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
        })
      }
    }
  })
  
  
  #########################Ongoing Optimization###########################################
  
  #Navigation buttons in Ongoing Optimizer tab
  observeEvent(input$SP_opti_top_panel_left_on,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu9")
  })
  
  observeEvent(input$SP_opti_top_panel_right_on,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu11")
  })
  
  #Navigation buttons in KAM Cockpit tab
  observeEvent(input$SP_opti_op_top_panel_left_on,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu10")
  })
  
  observeEvent(input$SP_opti_op_top_panel_right_on,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu12")
  })
  
  
  observeEvent(input$SP_opti_on_cmp_scn,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu8")
  })
  
  observeEvent(input$SP_opti_on_opti_ann,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu5")
  })
  
  observeEvent(input$SP_opti_ROI_selection_on,{
    updateSelectizeInput(session,"SP_opti_goal_on",choices = c("Scan Net Revenue","Gross Margin % of NR","Volume Sales","Scan Gross Sales",input$SP_opti_ROI_selection_on,"Trade Spend % of NR","Value Market Share","Trade Spend % of NIS","Gross Margin"),selected = "Scan Net Revenue")
  })
  
  observeEvent(c(input$SP_opti_tpo_on,
                 input$SP_opti_reset_on,
                 input$SP_opti_date_on_start,
                 input$SP_opti_date_on_end),{
                   
                   SP_reactive_input$const_chg_flag_on <- 0
                 })
  observeEvent(c(input$SP_opti_tpo_on,
                 input$SP_opti_goal_on,
                 input$SP_opti_date_on_start,
                 input$SP_opti_date_on_end,
                 input$SP_opti_reset_on,
                 input$SP_opti_ROI_selection_on,
                 input$SP_opti_restrictions_on$changes$changes[[1]]),{
                   
                   #####Default selection of optmization sign based on goal
                   observeEvent(input$SP_opti_goal_on,{
                     if(grepl("Spend|spend|invest|Invest",input$SP_opti_goal_on,ignore.case = TRUE)){
                       updateSelectizeInput(session,"SP_opti_sign_on",choices = c("Max","Min"),selected = "Min")
                     }else{
                       updateSelectizeInput(session,"SP_opti_sign_on",choices = c("Max","Min"),selected = "Max")
                     }
                   })
                   SP_reactive_input$SP_opti_const_selected_on <- data.table(SP_reactive_input$SP_opti_const[SP_reactive_input$SP_opti_const$`Week End Date` >= SP_reactive_input$SP_date_vector_start,])
                   
                   if(input$SP_opti_tpo_on != "" & !(is.null(input$SP_opti_tpo_on))){
                     load(paste0(SP_reactive_input$folder_path,"/Final_Optimized_Plan.RData"))
                     SP_reactive_input$annual_optim_op <- tpo_list_temp
                     SP_reactive_input$ongoing_lsm_nonlsm <- as.character(SP_reactive_input$annual_optim_op$Compare_Scenario$`LSM Constrained/Unconstrained`)
                     
                     if(year(input$SP_opti_date_on_start) >= year(Sys.Date())){
                       SP_reactive_input$start_date_tbo <- as.Date(input$SP_opti_date_on_start)
                       SP_reactive_input$today_date_tbo <- as.Date(input$SP_opti_date_on_start)
                     }else{
                       SP_reactive_input$start_date_tbo <- as.Date(paste0(year(Sys.Date())+1,"-01-01"))
                       SP_reactive_input$today_date_tbo <- as.Date(paste0(year(Sys.Date())+1,"-01-01"))
                     }
                     
                     if(year(input$SP_opti_date_on_end) >= year(Sys.Date())){
                       SP_reactive_input$end_date_tbo <- as.Date(input$SP_opti_date_on_end)
                     }else{
                       SP_reactive_input$end_date_tbo <- as.Date(paste0(year(Sys.Date())+1,"-01-31"))
                     }
                     
                     # if(year(input$SP_opti_date_on_end) > year(Sys.Date())){
                     #   SP_reactive_input$today_date_tbo <- as.Date(Sys.Date())
                     # }else{
                     #   SP_reactive_input$today_date_tbo <- as.Date(paste0(year(Sys.Date())+1,"-01-01"))
                     # }
                     ######Calculating last year values(2018), Minimum(0.9) and maximum(1.1) values based on selected fields
                     #When Brand, Format and PPG are empty, populating the graphs
                     
                     if(all(input$SP_opti_brand_on == "") & all(input$SP_opti_format_on == "") & all(input$SP_opti_ppg_on == "")){
                       SP_reactive_input$opti_ip_cat_sales_on <- data.frame("Units" = 1000000)
                       SP_reactive_input$opti_ip_cat_sales_value_on <- data.frame("Value" = 1000000)
                       
                       #SP_reactive_input$SP_opti_const_grouped_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on,.("CY_Volume" = sum(Units),"Inc_GM_Abs" = sum(Inc_GM_Abs), "Trade_Investment" = sum(Trade_Investment), "Gross_Sales" = sum(Gross_Sales),
                       #                                                                                                                                                                "Net_Revenue" = sum(Net_Revenue), "GM_Abs" = sum(GM_Abs), "Inc_Revenue" = sum(Inc_Revenue),"Inc_NIS" = sum(Inc_NIS), "NIS" = sum(NIS)),by = .(Category)]
                       SP_reactive_input$SP_RB_Financial_on <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat_on & SP_reactive_input$SP_nielsen$Flag_RB_Financial == 0,])
                       SP_reactive_input$SP_RB_Delist_on <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat_on & SP_reactive_input$SP_nielsen$DL == 1,])
                     }else if(all(input$SP_opti_format_on == "") & all(input$SP_opti_ppg_on == "")){
                       #
                       SP_reactive_input$opti_ip_cat_sales_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & SP_reactive_input$SP_opti_const_selected_on$Brand != input$SP_opti_brand_on,.("Units" = sum(Units)),by = .(Category)]
                       SP_reactive_input$opti_ip_cat_sales_value_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & SP_reactive_input$SP_opti_const_selected_on$Brand != input$SP_opti_brand_on,.("Value" = sum(Retailer_Revenue)),by = .(Category)]
                       
                       SP_reactive_input$SP_opti_const_grouped_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & SP_reactive_input$SP_opti_const_selected_on$Brand == input$SP_opti_brand_on,.("CY_Volume" = sum(Units),"Inc_GM_Abs" = sum(Inc_GM_Abs), "Trade_Investment" = sum(Trade_Investment), "Gross_Sales" = sum(Gross_Sales),
                                                                                                                                                                                                                                                                              "Net_Revenue" = sum(Net_Revenue), "GM_Abs" = sum(GM_Abs), "Inc_Revenue" = sum(Inc_Revenue),"Inc_NIS" = sum(Inc_NIS), "NIS" = sum(NIS),
                                                                                                                                                                                                                                                                              "Retailer_Revenue" = sum(Retailer_Revenue), "Net_Cost_Total" = sum(Net_Cost_Total),"FM" = sum(FM_Total), "Retro_Funding" = sum(Retro_Fund_Total), "Display" = sum(Display_Cost),
                                                                                                                                                                                                                                                                              "Inc_GM_Abs_New" = sum(R_GM_Inc), "Inc_Revenue_New" = sum(R_Net_Revenue_Inc), "Trade_Investment_New" = sum(R_Total_Trade_Inv_Inc ), "Inc_NIS_New" = sum(R_NIS_Inc)),by = .(Category,Brand)]
                       SP_reactive_input$SP_RB_Financial_on <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat_on & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand_on & SP_reactive_input$SP_nielsen$Flag_RB_Financial == 0,])
                       SP_reactive_input$SP_RB_Delist_on <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat_on & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand_on & SP_reactive_input$SP_nielsen$DL == 1,])
                       
                     }else if(all(input$SP_opti_format_on != "") & all(input$SP_opti_ppg_on == "")){
                       SP_reactive_input$opti_ip_cat_sales_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & SP_reactive_input$SP_opti_const_selected_on$Format != input$SP_opti_format_on,.("Units" = sum(Units)),by = .(Category)]
                       SP_reactive_input$opti_ip_cat_sales_value_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & SP_reactive_input$SP_opti_const_selected_on$Format != input$SP_opti_format_on,.("Value" = sum(Retailer_Revenue)),by = .(Category)]
                       
                       SP_reactive_input$SP_opti_const_grouped_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & SP_reactive_input$SP_opti_const_selected_on$Brand == input$SP_opti_brand_on & SP_reactive_input$SP_opti_const_selected_on$Format == input$SP_opti_format_on,.("CY_Volume" = sum(Units),"Inc_GM_Abs" = sum(Inc_GM_Abs), "Trade_Investment" = sum(Trade_Investment), "Gross_Sales" = sum(Gross_Sales),
                                                                                                                                                                                                                                                                                                                                                              "Net_Revenue" = sum(Net_Revenue), "GM_Abs" = sum(GM_Abs), "Inc_Revenue" = sum(Inc_Revenue),"Inc_NIS" = sum(Inc_NIS), "NIS" = sum(NIS),
                                                                                                                                                                                                                                                                                                                                                              "Retailer_Revenue" = sum(Retailer_Revenue), "Net_Cost_Total" = sum(Net_Cost_Total),"FM" = sum(FM_Total), "Retro_Funding" = sum(Retro_Fund_Total), "Display" = sum(Display_Cost),
                                                                                                                                                                                                                                                                                                                                                              "Inc_GM_Abs_New" = sum(R_GM_Inc), "Inc_Revenue_New" = sum(R_Net_Revenue_Inc), "Trade_Investment_New" = sum(R_Total_Trade_Inv_Inc ), "Inc_NIS_New" = sum(R_NIS_Inc)),by = .(Category,Brand)]
                       SP_reactive_input$SP_RB_Financial_on <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat_on & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand_on & SP_reactive_input$SP_nielsen$Format %in% input$SP_opti_format_on & SP_reactive_input$SP_nielsen$Flag_RB_Financial == 0,])
                       SP_reactive_input$SP_RB_Delist_on <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat_on & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand_on & SP_reactive_input$SP_nielsen$Format %in% input$SP_opti_format_on & SP_reactive_input$SP_nielsen$DL == 1,])
                     }else{
                       SP_reactive_input$opti_ip_cat_sales_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & !(SP_reactive_input$SP_opti_const_selected_on$PPG %in% input$SP_opti_ppg_on),.("Units" = sum(Units)),by = .(Category)]
                       SP_reactive_input$opti_ip_cat_sales_value_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & !(SP_reactive_input$SP_opti_const_selected_on$PPG %in% input$SP_opti_ppg_on),.("Value" = sum(Retailer_Revenue)),by = .(Category)]
                       
                       SP_reactive_input$SP_opti_const_grouped_on <- SP_reactive_input$SP_opti_const_selected_on[SP_reactive_input$SP_opti_const_selected_on$Category == input$SP_opti_cat_on & SP_reactive_input$SP_opti_const_selected_on$Brand == input$SP_opti_brand_on & SP_reactive_input$SP_opti_const_selected_on$Format == input$SP_opti_format_on & SP_reactive_input$SP_opti_const_selected_on$PPG %in% input$SP_opti_ppg_on,.("CY_Volume" = sum(Units),"Inc_GM_Abs" = sum(Inc_GM_Abs), "Trade_Investment" = sum(Trade_Investment), "Gross_Sales" = sum(Gross_Sales),
                                                                                                                                                                                                                                                                                                                                                                                                                                          "Net_Revenue" = sum(Net_Revenue), "GM_Abs" = sum(GM_Abs), "Inc_Revenue" = sum(Inc_Revenue),"Inc_NIS" = sum(Inc_NIS), "NIS" = sum(NIS),
                                                                                                                                                                                                                                                                                                                                                                                                                                          "Retailer_Revenue" = sum(Retailer_Revenue), "Net_Cost_Total" = sum(Net_Cost_Total),"FM" = sum(FM_Total), "Retro_Funding" = sum(Retro_Fund_Total), "Display" = sum(Display_Cost),
                                                                                                                                                                                                                                                                                                                                                                                                                                          "Inc_GM_Abs_New" = sum(R_GM_Inc), "Inc_Revenue_New" = sum(R_Net_Revenue_Inc), "Trade_Investment_New" = sum(R_Total_Trade_Inv_Inc ), "Inc_NIS_New" = sum(R_NIS_Inc)),by = .(Category,Brand)]
                       SP_reactive_input$SP_RB_Financial_on <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat_on & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand_on & SP_reactive_input$SP_nielsen$Format %in% input$SP_opti_format_on & SP_reactive_input$SP_nielsen$PPG %in% input$SP_opti_ppg_on & SP_reactive_input$SP_nielsen$Flag_RB_Financial == 0,])
                       SP_reactive_input$SP_RB_Delist_on <- unique(SP_reactive_input$SP_nielsen[SP_reactive_input$SP_nielsen$Date >= SP_reactive_input$SP_date_vector_start & SP_reactive_input$SP_nielsen$Category == input$SP_opti_cat_on & SP_reactive_input$SP_nielsen$Brand %in% input$SP_opti_brand_on & SP_reactive_input$SP_nielsen$Format %in% input$SP_opti_format_on & SP_reactive_input$SP_nielsen$PPG %in% input$SP_opti_ppg_on & SP_reactive_input$SP_nielsen$DL == 1,])
                     }
                     if(input$SP_opti_ROI_selection_on == "Incremental GM ROI"){
                       SP_reactive_input$opti_ROI_input_tbo <- "R_ROI_GM"
                     }else if(input$SP_opti_ROI_selection_on == "Incremental NR ROI"){
                       SP_reactive_input$opti_ROI_input_tbo <- "R_ROI_Rev"
                     }else if(input$SP_opti_ROI_selection_on == "Incremental NIS ROI"){
                       SP_reactive_input$opti_ROI_input_tbo <- "R_ROI_NIS"
                     }
                     
                     if(is.null(input$SP_opti_restrictions_on$changes$changes[[1]]) & (SP_reactive_input$const_chg_flag_on != 1)){
                       
                       #SP_reactive_input$start_date_tbo <- ifelse(year(input$SP_opti_date_on_start) > year(Sys.Date()),as.Date(input$SP_opti_date_on_start),as.Date(paste0(year(Sys.Date())+1,"-01-01")))
                       #SP_reactive_input$end_date_tbo <- ifelse(year(input$SP_opti_date_on_end) > year(Sys.Date()),input$SP_opti_date_on_end,as.Date(paste0(year(Sys.Date())+1,"-01-31")))
                       #SP_reactive_input$today_date_tbo <- ifelse(year(input$SP_opti_date_on_start) > year(Sys.Date()),Sys.Date(),as.Date(paste0(year(Sys.Date())+1,"-01-01")))
                       SP_reactive_input$SP_restrictions_tbo <- ong_const_ip(data.table(SP_reactive_input$annual_optim_op$opti_output),SP_reactive_input$annual_optim_op$other_sales,SP_reactive_input$start_date_tbo,SP_reactive_input$end_date_tbo,SP_reactive_input$today_date_tbo,SP_reactive_input$opti_ROI_input_tbo,SP_reactive_input$retailer_week_end_day,SP_reactive_input$retailer_weekEndDay_no,SP_reactive_input$opti_ip_cat_sales_value_on[["Value"]])
                       
                       SP_reactive_input$SP_restrictions_tbo <- as.data.frame(SP_reactive_input$SP_restrictions_tbo)
                       
                       SP_reactive_input$SP_restrictions_tbo[!(grepl("Share|%|ROI",SP_reactive_input$SP_restrictions_tbo$KPI)),!(names(SP_reactive_input$SP_restrictions_tbo) %in% c("Constraint Order","KPI","KPI_Mapping"))] <- SP_reactive_input$SP_restrictions_tbo[!(grepl("Share|%|ROI",SP_reactive_input$SP_restrictions_tbo$KPI)),!(names(SP_reactive_input$SP_restrictions_tbo) %in% c("Constraint Order","KPI","KPI_Mapping"))]/10^6
                       SP_reactive_input$SP_restrictions_tbo[,!(names(SP_reactive_input$SP_restrictions_tbo) %in% c("Constraint Order","KPI","KPI_Mapping"))] <- round(SP_reactive_input$SP_restrictions_tbo[,!(names(SP_reactive_input$SP_restrictions_tbo) %in% c("Constraint Order","KPI","KPI_Mapping"))],2)
                       SP_reactive_input$SP_restrictions_tbo <- data.table(SP_reactive_input$SP_restrictions_tbo)
                       
                       SP_reactive_input$SP_restrictions_tbo <- cbind(data.frame("Include the Constraint"= c(rep(TRUE,9)),check.names = FALSE),SP_reactive_input$SP_restrictions_tbo)
                       SP_reactive_input$SP_restrictions_tbo$KPI <- as.character(SP_reactive_input$SP_restrictions_tbo$KPI)
                       SP_reactive_input$SP_restrictions_tbo$Scale <- "Absolute"
                       SP_reactive_input$SP_restrictions_tbo[grep("%|Share",SP_reactive_input$SP_restrictions_tbo$KPI),]$Scale <- "Percent"
                       SP_reactive_input$SP_restrictions_tbo$Scale <- as.factor(SP_reactive_input$SP_restrictions_tbo$Scale)
                       
                       SP_reactive_input$SP_restrictions_tbo[grep("Spend|spend|invest|Invest",SP_reactive_input$SP_restrictions_tbo$KPI,ignore.case = TRUE,invert = TRUE),]$`Maximum Value` <- NA
                       SP_reactive_input$SP_restrictions_tbo[grep("Spend|spend|invest|Invest",SP_reactive_input$SP_restrictions_tbo$KPI,ignore.case = TRUE),]$`Minimum Value` <- NA
                       #SP_reactive_input$SP_restrictions_selected_tbo <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI %in% c("Gross Margin % of NR","Scan Net Revenue","Trade Spend % of NR","Scan Gross Sales","Volume Sales",
                       #                                                                                                                                          "Incremental GM ROI","Incremental NR ROI","Value Market Share"),]
                       SP_reactive_input$SP_restrictions_selected_tbo <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI != input$SP_opti_goal_on,c("Include the Constraint","Constraint Order","KPI","Scale","Actuals till date","Forecast of the year","Planned for selected period","Minimum Value","Maximum Value")]
                       SP_reactive_input$SP_restrictions_selected_tbo <- SP_reactive_input$SP_restrictions_selected_tbo[c(1:6),]
                       SP_reactive_input$SP_restrictions_selected_tbo$`Constraint Order` <- as.factor(c(1:nrow(SP_reactive_input$SP_restrictions_selected_tbo)))
                       rownames(SP_reactive_input$SP_restrictions_selected_tbo) <- c(1:nrow(SP_reactive_input$SP_restrictions_selected_tbo))
                       SP_reactive_input$SP_restrictions_selected_tbo_bckp <- SP_reactive_input$SP_restrictions_selected_tbo
                     }else if(!(is.null(input$SP_opti_restrictions_on$changes$changes[[1]]))){
                       ####for any KPI choice changes
                       if((input$SP_opti_restrictions_on$changes$changes[[1]][[2]] == (which(colnames(hot_to_r(input$SP_opti_restrictions_on)) %in% c("KPI")) -1))){
                         
                         
                         SP_reactive_input$SP_restrictions_1 <- hot_to_r(input$SP_opti_restrictions_on)
                         
                         SP_reactive_input$SP_restrictions_selected_tbo <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI != input$SP_opti_goal_on,c("Include the Constraint","Constraint Order","KPI","Scale","Actuals till date","Forecast of the year","Planned for selected period","Minimum Value","Maximum Value")]
                         SP_reactive_input$SP_restrictions_selected_tbo <- SP_reactive_input$SP_restrictions_selected_tbo[SP_reactive_input$SP_restrictions_selected_tbo$KPI %in% SP_reactive_input$SP_restrictions_1$KPI,]
                         
                         SP_reactive_input$SP_restrictions_selected_tbo$`Constraint Order` <- as.factor(c(1:nrow(SP_reactive_input$SP_restrictions_selected_tbo)))
                         rownames(SP_reactive_input$SP_restrictions_selected_tbo) <- c(1:nrow(SP_reactive_input$SP_restrictions_selected_tbo))
                         
                         SP_reactive_input$const_chg_flag_on <- 1
                         
                       }else if((input$SP_opti_restrictions_on$changes$changes[[1]][[2]] == (which(colnames(hot_to_r(input$SP_opti_restrictions_on)) %in% c("Scale")) -1))){
                         SP_reactive_input$SP_restrictions_1 <- hot_to_r(input$SP_opti_restrictions_on)
                         
                         if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions_on$changes$changes[[1]][[1]]+1,"KPI"] == "Volume Sales"){
                           if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Absolute"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Volume Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Volume Sales",]$`Planned for selected period` *0.9
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Volume Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Volume Sales",]$`Planned for selected period` *1.1
                           }else if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Percent"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Volume Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Volume Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                           }
                         }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions_on$changes$changes[[1]][[1]]+1,"KPI"] == "Scan Gross Sales"){
                           if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Absolute"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Gross Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Scan Gross Sales",]$`Planned for selected period` *0.9
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Gross Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Scan Gross Sales",]$`Planned for selected period` *1.1
                           }else if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Percent"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Gross Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Gross Sales" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                           }
                         }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions_on$changes$changes[[1]][[1]]+1,"KPI"] == input$SP_opti_ROI_selection_on){
                           if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Absolute"){
                             if(input$SP_opti_ROI_selection_on == "Incremental GM ROI"){
                               SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection_on & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == input$SP_opti_ROI_selection_on,]$`Planned for selected period`*0.9
                               SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection_on & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == input$SP_opti_ROI_selection_on,]$`Planned for selected period`*1.1
                             }else if(input$SP_opti_ROI_selection_on == "Incremental NR ROI"){
                               SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection_on & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == input$SP_opti_ROI_selection_on,]$`Planned for selected period`*0.9
                               SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection_on & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == input$SP_opti_ROI_selection_on,]$`Planned for selected period`*1.1
                             }
                           }else if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Percent"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection_on & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == input$SP_opti_ROI_selection_on & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                           }
                         }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions_on$changes$changes[[1]][[1]]+1,"KPI"] == "Value Market Share"){
                           if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Absolute"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Value Market Share" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Value Market Share",]$`Planned for selected period` * 0.9
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Value Market Share" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Value Market Share",]$`Planned for selected period` * 1.1
                           }else if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Percent"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Value Market Share" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Value Market Share" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                           }
                         }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions_on$changes$changes[[1]][[1]]+1,"KPI"] == "Scan Net Revenue"){
                           if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Absolute"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Net Revenue" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Scan Net Revenue",]$`Planned for selected period` *0.9
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Net Revenue" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Scan Net Revenue",]$`Planned for selected period` *1.1
                           }else if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Percent"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Net Revenue" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Scan Net Revenue" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                           }
                         }else if(SP_reactive_input$SP_restrictions_1[input$SP_opti_restrictions_on$changes$changes[[1]][[1]]+1,"KPI"] == "Gross margin"){
                           if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Absolute"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Gross margin" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Gross margin",]$`Planned for selected period` *0.9
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Gross margin" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- SP_reactive_input$SP_restrictions_tbo[SP_reactive_input$SP_restrictions_tbo$KPI == "Gross margin",]$`Planned for selected period` *1.1
                           }else if(input$SP_opti_restrictions_on$changes$changes[[1]][[4]] == "Percent"){
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Gross margin" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Minimum Value` <- 90
                             SP_reactive_input$SP_restrictions_1[SP_reactive_input$SP_restrictions_1$KPI == "Gross margin" & !(is.na(SP_reactive_input$SP_restrictions_1$KPI)),]$`Maximum Value` <- 110
                           }
                         }
                         SP_reactive_input$SP_restrictions_1[grep("Spend|spend|invest|Invest",SP_reactive_input$SP_restrictions_1$KPI,ignore.case = TRUE,invert = TRUE),]$`Maximum Value` <- NA
                         SP_reactive_input$SP_restrictions_1[grep("Spend|spend|invest|Invest",SP_reactive_input$SP_restrictions_1$KPI,ignore.case = TRUE),]$`Minimum Value` <- NA
                         
                         SP_reactive_input$SP_restrictions_selected_tbo <- SP_reactive_input$SP_restrictions_1
                         SP_reactive_input$const_chg_flag_on <- 1
                       }
                     }
                     #print(SP_reactive_input$SP_restrictions_selected_tbo)
                   }
                 })
  
  observeEvent(input$SP_opti_goal_on,{
    if(!(is.null(SP_reactive_input$SP_restrictions_tbo))){
      SP_reactive_input$SP_restrictions_selected_tbo <- SP_reactive_input$SP_restrictions_selected_tbo[SP_reactive_input$SP_restrictions_selected_tbo$KPI != input$SP_opti_goal_on,c("Include the Constraint","Constraint Order","KPI","Scale","Actuals till date","Forecast of the year","Planned for selected period","Minimum Value","Maximum Value")]
      SP_reactive_input$SP_restrictions_selected_tbo <- SP_reactive_input$SP_restrictions_selected_tbo[c(1:6),]
      SP_reactive_input$SP_restrictions_selected_tbo$`Constraint Order` <- as.factor(c(1:nrow(SP_reactive_input$SP_restrictions_selected_tbo)))
    }
  })
  
  output$SP_opti_restrictions_on <- renderRHandsontable({
    validate(
      need(SP_reactive_input$SP_restrictions_selected_tbo,"Select Optimization Goal")
    )
    color_renderer <- "
    function(instance, td) {
    Handsontable.renderers.NumericRenderer.apply(this, arguments);
    td.style.background = '#c2fafe';
    }
    "
    
    #####Populating Optimization constraint table
    rhandsontable(SP_reactive_input$SP_restrictions_selected_tbo[c(1:6),],rowHeaders = NULL) %>%
      hot_col(c("Actuals till date","Forecast of the year","Planned for selected period"),readOnly = TRUE, renderer = color_renderer) %>%
      hot_col("KPI", type = "dropdown",source = c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Scan Gross Sales","Gross Margin","Volume Sales",input$SP_opti_ROI_selection_on,"Value Market Share")) %>%
      #hot_col("KPI", type = "dropdown",source = c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Scan Gross Sales","Volume Sales",input$SP_opti_ROI_selection_on,"Value Market Share")) %>%
      hot_col(c("Actuals till date","Forecast of the year","Planned for selected period","Minimum Value","Maximum Value"), format = "0,0.00") %>%
      hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_restrictions_selected_tbo$KPI)[1]), "Scale", readOnly = TRUE) %>%
      hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_restrictions_selected_tbo$KPI)[2]), "Scale", readOnly = TRUE) %>%
      hot_cell(as.numeric(grep("%|Share",SP_reactive_input$SP_restrictions_selected_tbo$KPI)[2]), "Scale", readOnly = TRUE)
  })
  
  observeEvent(c(
    #input$SP_level,
    input$SP_opti_cat_on,
    input$SP_opti_brand_on,
    input$SP_opti_format_on,
    input$SP_opti_ppg_on,
    input$SP_opti_tpo_on,
    input$SP_opti_date_on_start,
    input$SP_opti_date_on_end,
    input$SP_opti_reset),{
      
      #######Populating exclude ppg table and prod restrictions table
      if(!(is.null(SP_reactive_input$exclude_ppg_data_prep))){
        if(all(input$SP_opti_brand_on == "") & all(input$SP_opti_format_on == "") & all(input$SP_opti_ppg_on == "")){
          SP_reactive_input$SP_opti_const_ppg_tbo <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat_on,])
          
        }else if(all(input$SP_opti_format_on == "") & all(input$SP_opti_ppg_on == "")){
          SP_reactive_input$SP_opti_const_ppg_tbo <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat_on & SP_reactive_input$exclude_ppg_data_prep$Brand %in% input$SP_opti_brand_on,])
        }else if(all(input$SP_opti_format_on != "") & all(input$SP_opti_ppg_on == "")){
          SP_reactive_input$SP_opti_const_ppg_tbo <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat_on & SP_reactive_input$exclude_ppg_data_prep$Brand %in% input$SP_opti_brand_on & SP_reactive_input$exclude_ppg_data_prep$Format %in% input$SP_opti_format_on,])
        }else if(all(input$SP_opti_ppg_on != "")){
          SP_reactive_input$SP_opti_const_ppg_tbo <- unique(SP_reactive_input$exclude_ppg_data_prep[SP_reactive_input$exclude_ppg_data_prep$Category == input$SP_opti_cat_on & SP_reactive_input$exclude_ppg_data_prep$Brand %in% input$SP_opti_brand_on & SP_reactive_input$exclude_ppg_data_prep$Format %in% input$SP_opti_format_on & SP_reactive_input$exclude_ppg_data_prep$PPG %in% input$SP_opti_ppg_on,])
        }
        if(!(input$SP_opti_tpo_on == "")){
          load(paste0(SP_reactive_input$folder_path,"/Final_Optimized_Plan.RData"))
          SP_reactive_input$annual_optim_op <- tpo_list_temp
          
          SP_reactive_input$SP_opti_const_ppg_tbo <- SP_reactive_input$annual_optim_op$ppg_exclude_shiny[!(SP_reactive_input$annual_optim_op$ppg_exclude_shiny$PPG %in% SP_reactive_input$annual_optim_op$ppg_exclude$PPG),]
        }
        if("PPG_Description" %in% names(SP_reactive_input$SP_opti_const_ppg_tbo)){
          SP_reactive_input$SP_exclude_ppg_tbo <- cbind(data.frame("Exclude" = rep(FALSE,nrow(unique(SP_reactive_input$SP_opti_const_ppg_tbo[,c("PPG","PPG_Description","Category","Brand","Format")]))),check.names = FALSE),unique(SP_reactive_input$SP_opti_const_ppg_tbo[,c("PPG","PPG_Description","Category","Brand","Format")]))
        }else{
          SP_reactive_input$SP_exclude_ppg_tbo <- cbind(data.frame("Exclude" = rep(FALSE,nrow(unique(SP_reactive_input$SP_opti_const_ppg_tbo[,c("PPG","PPG Description","Category","Brand","Format")]))),check.names = FALSE),unique(SP_reactive_input$SP_opti_const_ppg_tbo[,c("PPG","PPG Description","Category","Brand","Format")]))
        }
        
        names(SP_reactive_input$SP_exclude_ppg_tbo) <- c("Exclude","PPG","PPG Description","Category","Brand","Format")
        
        output$SP_opti_exclude_ppg_on <- renderRHandsontable({
          rhandsontable(SP_reactive_input$SP_exclude_ppg_tbo,rowHeaders = NULL)
        })
      }
      
      if(input$SP_opti_tpo_on != "" & !(is.null(input$SP_opti_tpo_on))){
        load(paste0(SP_reactive_input$folder_path,"/Final_Optimized_Plan.RData"))
        SP_reactive_input$annual_optim_op <- tpo_list_temp
        if(year(input$SP_opti_date_on_start) >= year(Sys.Date())){
          SP_reactive_input$start_date_tbo <- as.Date(input$SP_opti_date_on_start)
          SP_reactive_input$today_date_tbo <- as.Date(input$SP_opti_date_on_start)
        }else{
          SP_reactive_input$start_date_tbo <- as.Date(paste0(year(Sys.Date())+1,"-01-01"))
          SP_reactive_input$today_date_tbo <- as.Date(paste0(year(Sys.Date())+1,"-01-01"))
        }
        
        if(year(input$SP_opti_date_on_end) >= year(Sys.Date())){
          SP_reactive_input$end_date_tbo <- as.Date(input$SP_opti_date_on_end)
        }else{
          SP_reactive_input$end_date_tbo <- as.Date(paste0(year(Sys.Date())+1,"-01-31"))
        }
        
        # if(year(input$SP_opti_date_on_end) > year(Sys.Date())){
        #   SP_reactive_input$today_date_tbo <- as.Date(Sys.Date())
        # }else{
        #   SP_reactive_input$today_date_tbo <- as.Date(paste0(year(Sys.Date())+1,"-01-01"))
        # }
        #
        #SP_reactive_input$SP_opti_prod_restrictions_default$`Display &/or Feature Event in Last Year` <- ifelse(SP_reactive_input$SP_opti_prod_restrictions_default$LY_Display_Weeks == 0,"No","Yes")
        
        SP_reactive_input$SP_prod_restrict_tbo_complete <- ong_prod_restrict(data.table(SP_reactive_input$annual_optim_op$opti_output),data.table(SP_input_LSM()),SP_reactive_input$start_date_tbo,SP_reactive_input$end_date_tbo,SP_reactive_input$retailer_week_end_day,SP_reactive_input$retailer_weekEndDay_no)
        
        SP_reactive_input$SP_prod_restrict_tbo <- unique(SP_reactive_input$SP_prod_restrict_tbo_complete[SP_reactive_input$SP_prod_restrict_tbo_complete$PPG %in% unique(SP_reactive_input$SP_opti_const_ppg_tbo$PPG),c("PPG","PPG_Description","Brand","Format","MRRP MAX","MRRP MIN","RSP","Planned Investment for total year","Actual Investment so far","Planned Investment in selected period","Investment to go",
                                                                                                                                                                                                                        "Minimum Investments","Maximim Investments","LSM Max Promo Price","Minimum Promo Price","Maximum Promo Price","Total available promotion weeks in selected period",
                                                                                                                                                                                                                        "Minimum Total Weeks on promotion","Maximum Total Weeks on promotion","Minimum Total Weeks on Display","Maximum Total Weeks on Display")])
        names(SP_reactive_input$SP_prod_restrict_tbo) <- c("PPG","PPG Description","Brand","Format","MRRP MAX","MRRP MIN","RSP","Planned Investment for total year","Actual Investment so far","Planned Investment in selected period","Investment to go",
                                                           "Minimum Investments","Maximim Investments","LSM Max Promo Price","Minimum Promo Price","Maximum Promo Price","Total available promotion weeks in selected period",
                                                           "Minimum Total Weeks on promotion","Maximum Total Weeks on promotion","Minimum Total Weeks on Display","Maximum Total Weeks on Display")
        output$SP_opti_prod_restrictions_on <- renderRHandsontable({
          color_renderer <- "
          function(instance, td) {
          Handsontable.renderers.NumericRenderer.apply(this, arguments);
          td.style.background = '#c2fafe';
          }
          "
          rhandsontable(SP_reactive_input$SP_prod_restrict_tbo,rowHeaders = NULL) %>%
            hot_cols(fixedColumnsLeft = 2) %>%
            #hot_table(c("Brand","Format"),overflow = "hidden",contextMenu = FALSE) %>%
            hot_col(c("PPG","PPG Description","Brand","Format","MRRP MAX","MRRP MIN","RSP","Planned Investment for total year","Actual Investment so far","Planned Investment in selected period","Investment to go","LSM Max Promo Price","Total available promotion weeks in selected period"), readOnly = TRUE) %>%
            hot_col(c("PPG","PPG Description","Brand","Format","MRRP MAX","MRRP MIN","RSP","Planned Investment for total year","Actual Investment so far","Planned Investment in selected period","Investment to go","LSM Max Promo Price","Total available promotion weeks in selected period"), renderer = color_renderer) %>%
            hot_col(c("Planned Investment for total year","Actual Investment so far","Planned Investment in selected period","Investment to go","Minimum Investments","Maximim Investments"), format = "0,0.00")
        })
      }
    })
  observeEvent(input$SP_opti_disp_pp_const_on,{
    if(input$SP_opti_disp_pp_const_on == TRUE){
      shinyjs::show("SP_opti_max_display_on")
    }else{
      shinyjs::hide("SP_opti_max_display_on")
    }
  })
  
  ###########Running optimization#############
  observeEvent(input$SP_opti_run_on,{
    ########Saving all the selected fields
    warning_flag = 0
    SP_TPO_list_tbo <- list()
    SP_TPO_list_tbo$coun <- input$SP_opti_coun_on
    SP_TPO_list_tbo$cust <- input$SP_opti_cust_on
    SP_TPO_list_tbo$cat <- input$SP_opti_cat_on
    SP_TPO_list_tbo$brand <- input$SP_opti_brand_on
    if(is.null(input$SP_opti_format_on) | (input$SP_opti_format_on == "")){
      SP_reactive_input$opti_format_input_tbo <- "ALL"
    }else{
      SP_reactive_input$opti_format_input_tbo <- input$SP_opti_format_on
    }
    
    if(is.null(input$SP_opti_ppg_on)){
      SP_reactive_input$opti_ppg_input_tbo <- "ALL"
    }else if(input$SP_opti_ppg_on == ""){
      SP_reactive_input$opti_ppg_input_tbo <- "ALL"
    }else{
      SP_reactive_input$opti_ppg_input_tbo <- input$SP_opti_ppg_on
    }
    
    SP_TPO_list_tbo$format <- SP_reactive_input$opti_format_input_tbo
    SP_TPO_list_tbo$ppg <- SP_reactive_input$opti_ppg_input_tbo
    SP_TPO_list_tbo$goal_shiny <- input$SP_opti_goal_on
    SP_TPO_list_tbo$sign <- input$SP_opti_sign_on
    SP_TPO_list_tbo$roi <- input$SP_opti_ROI_selection_on
    SP_TPO_list_tbo$start_date <- input$SP_opti_start_date_ui_on
    SP_TPO_list_tbo$end_date <- input$SP_opti_end_date_ui_on
    SP_TPO_list_tbo$ppg_exclude_shiny <- hot_to_r(input$SP_opti_exclude_ppg_on)
    SP_TPO_list_tbo$ppg_exclude <- SP_TPO_list_tbo$ppg_exclude_shiny[SP_TPO_list_tbo$ppg_exclude_shiny$Exclude == TRUE,]
    if(nrow(SP_TPO_list_tbo$ppg_exclude_shiny[SP_TPO_list_tbo$ppg_exclude_shiny$Exclude == FALSE,]) == 0){
      sendSweetAlert(session,"All the PPGs are exlcuded!!","No Product available to run Optimization",type = "error")
      warning_flag = 1
    }
    
    SP_TPO_list_tbo$opti_const_shiny <- hot_to_r(input$SP_opti_restrictions_on)
    
    SP_TPO_list_tbo$opti_const <- SP_TPO_list_tbo$opti_const_shiny
    levels(SP_TPO_list_tbo$opti_const$`Constraint Order`) <- c(1:6)
    if(any(is.na(SP_TPO_list_tbo$opti_const$KPI))){
      SP_TPO_list_tbo$opti_const[is.na(SP_TPO_list_tbo$opti_const$KPI),c("Include the Constraint")] <- FALSE
      SP_TPO_list_tbo$opti_const[is.na(SP_TPO_list_tbo$opti_const$KPI),c("Constraint Order")] <- 6
      SP_TPO_list_tbo$opti_const[is.na(SP_TPO_list_tbo$opti_const$KPI),c("Actuals till date","Forecast of the year","Planned for selected period","Minimum Value","Maximum Value")] <- NA
      SP_TPO_list_tbo$opti_const[is.na(SP_TPO_list_tbo$opti_const$KPI),c("Scale")] <- c("Absolute")
      SP_TPO_list_tbo$opti_const[is.na(SP_TPO_list_tbo$opti_const$KPI),c("KPI")] <- setdiff(c("Scan Net Revenue","Gross Margin % of NR","Volume Sales","Scan Gross Sales","Incremental GM ROI","Trade Spend % of NR","Value Market Share"),c(input$SP_opti_goal_on,unique(SP_TPO_list_tbo$opti_const[!(is.na(SP_TPO_list_tbo$opti_const$KPI)),]$KPI)))[1]
    }
    SP_TPO_list_tbo$opti_const <- SP_TPO_list_tbo$opti_const[!(is.na(SP_TPO_list_tbo$opti_const$KPI)),]
    SP_TPO_list_tbo$opti_const <- SP_TPO_list_tbo$opti_const[order(-SP_TPO_list_tbo$opti_const$`Include the Constraint`,SP_TPO_list_tbo$opti_const$`Constraint Order`),]
    
    ###Opti constraint
    SP_TPO_list_tbo$opti_const[SP_TPO_list_tbo$opti_const$Scale == "Percent" & !(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Minimum Value` <- SP_TPO_list_tbo$opti_const[SP_TPO_list_tbo$opti_const$Scale == "Percent" & !(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Minimum Value` * SP_TPO_list_tbo$opti_const[SP_TPO_list_tbo$opti_const$Scale == "Percent" & !(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Planned for selected period`/100
    SP_TPO_list_tbo$opti_const[SP_TPO_list_tbo$opti_const$Scale == "Percent" & !(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Maximum Value` <- SP_TPO_list_tbo$opti_const[SP_TPO_list_tbo$opti_const$Scale == "Percent" & !(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Maximum Value` * SP_TPO_list_tbo$opti_const[SP_TPO_list_tbo$opti_const$Scale == "Percent" & !(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Value Market Share")),]$`Planned for selected period`/100
    
    SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Minimum Value` <- SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Minimum Value` * 10^6
    SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Maximum Value` <- SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Maximum Value` * 10^6
    SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Planned for selected period` <- SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Planned for selected period` * 10^6
    SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Actuals till date` <- SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Actuals till date` * 10^6
    SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Forecast of the year` <- SP_TPO_list_tbo$opti_const[!(SP_TPO_list_tbo$opti_const$KPI %in% c("Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Incremental GM ROI","Incremental NR ROI","Value Market Share")),]$`Forecast of the year` * 10^6
    
    if(input$SP_opti_date_on_end < (as.Date(input$SP_opti_date_on_start) + 49)){
      sendSweetAlert(session,"Error!!","End date should be atleast 7 weeks ahead of Start date",type = "error")
      warning_flag = 1
    }
    
    if(grepl("Spend|spend|invest|Invest",input$SP_opti_goal_on,ignore.case = TRUE) & input$SP_opti_sign_on == "Max"){
      confirmSweetAlert(session,"opti_differ_sign_tbo",title = "Optimization Goal",text = "Goal changed from default value!!",type = "warning")
    }
    if(!(is.null(input$opti_differ_sign_tbo))){
      if(input$opti_differ_sign_tbo == FALSE){
        warning_flag = 1
      }
    }
    
    if(!(grepl("Spend|spend|invest|Invest",input$SP_opti_goal_on,ignore.case = TRUE)) & input$SP_opti_sign_on == "Min"){
      #warning_flag = 1
      confirmSweetAlert(session,"opti_differ_sign_1_tbo",title = "Optimization Goal",text = "Goal changed from default value!!",type = "warning")
    }
    
    
    observeEvent(input$opti_differ_sign_1_tbo,{
      if(!(is.null(input$opti_differ_sign_1_tbo))){
        if(input$opti_differ_sign_1_tbo == FALSE){
          warning_flag = 1
        }
      }
    })
    
    
    if(any(is.na(SP_TPO_list_tbo$opti_const$`Minimum Value`)) | any(SP_TPO_list_tbo$opti_const$`Include the Constraint` == FALSE)){
      SP_TPO_list_tbo$opti_const[is.na(SP_TPO_list_tbo$opti_const$`Minimum Value`) | (SP_TPO_list_tbo$opti_const$`Include the Constraint` == FALSE),]$`Minimum Value` <- -(10^20)
    }
    if(any(is.na(SP_TPO_list_tbo$opti_const$`Maximum Value`)) | any(SP_TPO_list_tbo$opti_const$`Include the Constraint` == FALSE)){
      SP_TPO_list_tbo$opti_const[is.na(SP_TPO_list_tbo$opti_const$`Maximum Value`) | (SP_TPO_list_tbo$opti_const$`Include the Constraint` == FALSE),]$`Maximum Value` <- (10^20)
    }
    
    if(any(is.na(SP_TPO_list_tbo$opti_const_shiny$`Minimum Value`)) | any(SP_TPO_list_tbo$opti_const_shiny$`Include the Constraint` == FALSE)){
      SP_TPO_list_tbo$opti_const_shiny[is.na(SP_TPO_list_tbo$opti_const_shiny$`Minimum Value`) | (SP_TPO_list_tbo$opti_const_shiny$`Include the Constraint` == FALSE),]$`Minimum Value` <- ""
    }
    if(any(is.na(SP_TPO_list_tbo$opti_const_shiny$`Maximum Value`)) | any(SP_TPO_list_tbo$opti_const_shiny$`Include the Constraint` == FALSE)){
      SP_TPO_list_tbo$opti_const_shiny[is.na(SP_TPO_list_tbo$opti_const_shiny$`Maximum Value`) | (SP_TPO_list_tbo$opti_const_shiny$`Include the Constraint` == FALSE),]$`Maximum Value` <- ""
    }
    
    opti_const_mapping <- data.frame("KPI" = c("Scan Net Revenue","Gross Margin % of NR","Trade Spend % of NR","Trade Spend % of NIS","Scan Gross Sales","Gross Margin","Volume Sales",input$SP_opti_ROI_selection_on,"Value Market Share"),"KPI_Mapping" = c("Net_Sales_model","GM_percent_model","Trade_as_per_NR_model","Trade_as_per_NIS_model","Gross_sales_model","Gross_margin_model","Volume_sales_model","ROI_model","Market_Share_model"))
    SP_TPO_list_tbo$opti_const <- left_join(SP_TPO_list_tbo$opti_const,opti_const_mapping,by = "KPI")
    
    if(any(SP_TPO_list_tbo$opti_const$`Maximum Value` < SP_TPO_list_tbo$opti_const$`Minimum Value`)){
      sendSweetAlert(session,"Error!!","Maximum Constraint values should be greater than or equal to Minimum Constraint values",type = "error")
      warning_flag = 1
    }
    
    ###opti goal
    SP_TPO_list_tbo$goal <- left_join(data.frame("Goal" = SP_TPO_list_tbo$goal_shiny),opti_const_mapping,by = c("Goal" = "KPI"))
    
    SP_TPO_list_tbo$opti_const$KPI_Mapping <- as.character(SP_TPO_list_tbo$opti_const$KPI_Mapping)
    
    
    ###opti goal
    SP_TPO_list_tbo$goal <- left_join(data.frame("Goal" = SP_TPO_list_tbo$goal_shiny),opti_const_mapping,by = c("Goal" = "KPI"))
    SP_TPO_list_tbo$goal$KPI_Mapping <- as.character(SP_TPO_list_tbo$goal$KPI_Mapping)
    ###Prod constraint
    if(!is.null(input$SP_opti_prod_restrictions_on)){
      SP_TPO_list_tbo$prod_const_shiny <- hot_to_r(input$SP_opti_prod_restrictions_on)
    }else{
      SP_TPO_list_tbo$prod_const_shiny <- SP_reactive_input$SP_prod_restrict_tbo
    }
    
    if(nrow(SP_TPO_list_tbo$prod_const_shiny) == 0){
      sendSweetAlert(session,"Error!!","No Product available to run Optimization in the selected brand and format",type = "error")
      warning_flag = 1
    }
    
    SP_TPO_list_tbo$prod_const_shiny <- left_join(SP_TPO_list_tbo$prod_const_shiny,SP_reactive_input$SP_prod_restrict_tbo_complete[,!(names(SP_reactive_input$SP_prod_restrict_tbo_complete) %in% c("Brand","Format","MRRP MAX","MRRP MIN","RSP","Planned Investment for total year","Actual Investment so far","Planned Investment in selected period","Investment to go",
                                                                                                                                                                                                    "Minimum Investments","Maximim Investments","LSM Max Promo Price","Minimum Promo Price","Maximum Promo Price","Total available promotion weeks in selected period",
                                                                                                                                                                                                    "Minimum Total Weeks on promotion","Maximum Total Weeks on promotion","Minimum Total Weeks on Display","Maximum Total Weeks on Display")),with = FALSE])
    
    if(length(unique(SP_TPO_list_tbo$opti_const_shiny$`Constraint Order`)) < length(SP_TPO_list_tbo$opti_const_shiny$`Constraint Order`)){
      sendSweetAlert(session,"Error!!","Duplicate values in constraint order",type = "error")
      warning_flag = 1
    }
    
    if(nrow(SP_TPO_list_tbo$opti_const_shiny[SP_TPO_list_tbo$opti_const_shiny$`Include the Constraint` == TRUE,]) > 6){
      sendSweetAlert(session,"Error!!","Optimizer cannot have more than 6 constraints",type = "error")
      warning_flag = 1
    }
    
    if(any(mod(SP_TPO_list_tbo$prod_const_shiny$`Minimum Total Weeks on promotion`, 3) != 0)){
      sendSweetAlert(session,"Error!!","Min Total Promo Weeks should be a multiple of 3 for each PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(mod(SP_TPO_list_tbo$prod_const_shiny$`Maximum Total Weeks on promotion`, 3) != 0)){
      sendSweetAlert(session,"Error!!","Max Total Promo Weeks should be a multiple of 3 for each PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(mod(SP_TPO_list_tbo$prod_const_shiny$`Minimum Total Weeks on Display`, 3) != 0)){
      sendSweetAlert(session,"Error!!","Max Display Weeks should be a multiple of 3 for each PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(mod(SP_TPO_list_tbo$prod_const_shiny$`Maximum Total Weeks on Display`, 3) != 0)){
      sendSweetAlert(session,"Error!!","Min Display Weeks should be a multiple of 3 for each PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(SP_TPO_list_tbo$prod_const_shiny$`Minimum Total Weeks on promotion` > SP_TPO_list_tbo$prod_const_shiny$`Total available promotion weeks in selected period`)){
      sendSweetAlert(session,"Error!!","Min Total Promo Weeks cannot be > Total available Promo Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(SP_TPO_list_tbo$prod_const_shiny$`Maximum Total Weeks on promotion` > SP_TPO_list_tbo$prod_const_shiny$`Total available promotion weeks in selected period`)){
      sendSweetAlert(session,"Error!!","Max Total Promo Weeks cannot be > Total available Promo Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(SP_TPO_list_tbo$prod_const_shiny$`Minimum Total Weeks on promotion` > SP_TPO_list_tbo$prod_const_shiny$`Maximum Total Weeks on promotion`)){
      sendSweetAlert(session,"Error!!","Min Total Promo Weeks cannot be > Max Total Promo Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(SP_TPO_list_tbo$prod_const_shiny$`Minimum Total Weeks on Display` > SP_TPO_list_tbo$prod_const_shiny$`Maximum Total Weeks on Display`)){
      sendSweetAlert(session,"Error!!","Min Display Weeks cannot be > Max Display Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(SP_TPO_list_tbo$prod_const_shiny$`Maximum Total Weeks on Display` > SP_TPO_list_tbo$prod_const_shiny$`Maximum Total Weeks on promotion`)){
      sendSweetAlert(session,"Error!!","Max Display Weeks cannot be > Max Total Promo Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    if(any(SP_TPO_list_tbo$prod_const_shiny$`Minimum Total Weeks on Display` > SP_TPO_list_tbo$prod_const_shiny$`Minimum Total Weeks on promotion`)){
      sendSweetAlert(session,"Error!!","Min Display Weeks cannot be > Min Total Promo Weeks for a PPG",type = "error")
      warning_flag = 1
    }
    
    if(warning_flag == 0){
      ###All other sales
      SP_TPO_list_tbo$other_sales <- SP_reactive_input$opti_ip_cat_sales_on[["Units"]]
      SP_TPO_list_tbo$other_sales_value <- SP_reactive_input$opti_ip_cat_sales_value_on[["Value"]]
      
      SP_reactive_input$simulated_flag_tbo <- 0
      
      SP_reactive_input$SP_RB_Financial_PPG_tbo <- unique(SP_reactive_input$SP_RB_Financial_on[,c("Category","Brand","Format","PPG","PPG_Description")])
      names(SP_reactive_input$SP_RB_Financial_PPG_tbo) <- c("Category","Brand","Format","PPG","PPG Description")
      SP_reactive_input$SP_RB_Financial_PPG_tbo$Comment <- "RB Financials not available"
      
      SP_reactive_input$SP_RB_Financial_PPG_1_tbo <- rbind(SP_reactive_input$SP_RB_Financial_PPG_tbo,cbind(SP_TPO_list_tbo$ppg_exclude[,c("Category","Brand","Format","PPG","PPG Description")],data.frame("Comment" = rep("Excluded in Optimization screen",nrow(SP_TPO_list_tbo$ppg_exclude)))))
      
      
      showModal(
        modalDialog(
          title = "Running Optimization",
          "Will take a few minutes based on the data size",
          size = "m",
          footer = "Please wait"
        )
      )
      
      if(input$SP_opti_comp_on == "Competitor Promotion Timing"){
        SP_reactive_input$include_comp_tbo <- 1
        SP_reactive_input$include_cannib_tbo <- 0
        SP_reactive_input$selected_plan_split <- split_to_modify_static_comp(data.table(SP_reactive_input$annual_optim_op$opti_output),data.table(SP_reactive_input$comp_seq),data.table(SP_TPO_list_tbo$prod_const_shiny),SP_reactive_input$start_date_tbo,SP_reactive_input$end_date_tbo,SP_reactive_input$retailer_week_end_day,SP_reactive_input$retailer_weekEndDay_no)
      }else if(input$SP_opti_comp_on == "Cannibalization- Complementary Impact"){
        SP_reactive_input$include_comp_tbo <- 0
        SP_reactive_input$include_cannib_tbo <- 1
        SP_reactive_input$selected_plan_split <- split_to_modify_static(data.table(SP_reactive_input$annual_optim_op$opti_output),data.table(SP_reactive_input$SP_cannibalization),data.table(SP_TPO_list_tbo$prod_const_shiny),SP_reactive_input$start_date_tbo,SP_reactive_input$end_date_tbo,SP_reactive_input$retailer_week_end_day,SP_reactive_input$retailer_weekEndDay_no,SP_reactive_input$include_cannib_tbo)
      }else{
        SP_reactive_input$include_comp_tbo <- 0
        SP_reactive_input$include_cannib_tbo <- 0
        SP_reactive_input$selected_plan_split <- split_to_modify_static(data.table(SP_reactive_input$annual_optim_op$opti_output),data.table(SP_reactive_input$SP_cannibalization),data.table(SP_TPO_list_tbo$prod_const_shiny),SP_reactive_input$start_date_tbo,SP_reactive_input$end_date_tbo,SP_reactive_input$retailer_week_end_day,SP_reactive_input$retailer_weekEndDay_no,SP_reactive_input$include_cannib_tbo)
      }
      if(SP_reactive_input$opti_format_input_tbo == "ALL" & SP_reactive_input$opti_ppg_input_tbo == "ALL"){
        SP_reactive_input$EAN_PPG_download_tbo <- unique(SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Manufacturer == SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected$Brand %in% input$SP_opti_brand_on,c("PPG","Category","Manufacturer","Brand","Format")])
      }else if(SP_reactive_input$opti_format_input_tbo != "ALL" & SP_reactive_input$opti_ppg_input_tbo == "ALL"){
        SP_reactive_input$EAN_PPG_download_tbo <- unique(SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Manufacturer == SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected$Brand %in% input$SP_opti_brand_on & SP_reactive_input$SP_opti_const_selected$Format %in% SP_reactive_input$opti_format_input_tbo,c("PPG","Category","Manufacturer","Brand","Format")])
      }else{
        SP_reactive_input$EAN_PPG_download_tbo <- unique(SP_reactive_input$SP_opti_const_selected[SP_reactive_input$SP_opti_const_selected$Manufacturer == SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected$Brand %in% input$SP_opti_brand_on & SP_reactive_input$SP_opti_const_selected$Format %in% SP_reactive_input$opti_format_input_tbo & SP_reactive_input$SP_opti_const_selected$PPG %in% SP_reactive_input$opti_ppg_input_tbo,c("PPG","Category","Manufacturer","Brand","Format")])
      }
      SP_reactive_input$EAN_PPG_download_tbo <- left_join(SP_reactive_input$EAN_PPG_download_tbo,SP_reactive_input$EAN_PPG_mapping,by = "PPG")
      
      SP_reactive_input$event_list_tbo <- event_list_tbo(data.table(SP_reactive_input$shiny_ip_events_final),data.table(SP_TPO_list_tbo$prod_const_shiny))
      SP_reactive_input$slot_invest_split <- ppg_slot_invest_tbo(data.table(SP_TPO_list_tbo$prod_const_shiny))
      
      list_of_datasets <- list("Static_Period" = SP_reactive_input$selected_plan_split[[2]],"Selected_Period" = SP_reactive_input$selected_plan_split[[1]], "Selected_Period_Weekly" = SP_reactive_input$selected_plan_split[[3]],"KPI_Constraints" = SP_TPO_list_tbo$opti_const, "Product_Const" = SP_TPO_list_tbo$prod_const_shiny, "All_Other_Sales" = SP_TPO_list_tbo$other_sales,
                               "Goal" = SP_TPO_list_tbo$goal, "Sign" = SP_TPO_list_tbo$sign,"Events_List" = SP_reactive_input$event_list_tbo,"Exclude_PPG_List" = SP_TPO_list_tbo$ppg_exclude$PPG, "Last_Year_KPI" = SP_reactive_input$SP_opti_const)
      write.xlsx(list_of_datasets, file = paste0(SP_reactive_input$folder_path,"/","Optimization_Inputs_TBO.xlsx"))
      
      
      
      if(input$SP_opti_ROI_selection_on == "Incremental GM ROI"){
        SP_reactive_input$opti_ROI_input_tbo <- "R_ROI_GM"
      }else if(input$SP_opti_ROI_selection_on == "Incremental NR ROI"){
        SP_reactive_input$opti_ROI_input_tbo <- "R_ROI_Rev"
      }else if(input$SP_opti_ROI_selection_on == "Incremental NIS ROI"){
        SP_reactive_input$opti_ROI_input_tbo <- "R_ROI_NIS"
      }
      
      ###Optimization function
      
      withProgress(message = 'Running Optimization.. (this may take a while)', value = 0, {
        
        SP_reactive_input$SP_opti_output_tbo <- optimization_tbo(data.table(SP_reactive_input$selected_plan_split[[1]]),data.table(SP_reactive_input$selected_plan_split[[2]]),data.table(SP_TPO_list_tbo$opti_const),data.table(SP_reactive_input$slot_invest_split[[1]]),data.table(SP_reactive_input$event_list_tbo),data.table(SP_reactive_input$slot_invest_split[[2]]),
                                                                 data.table(SP_reactive_input$SP_opti_const),SP_TPO_list_tbo$other_sales,SP_TPO_list_tbo$goal,SP_TPO_list_tbo$sign,SP_TPO_list_tbo$ppg_exclude$PPG,SP_reactive_input$opti_format_input_tbo,SP_reactive_input$opti_ROI_input_tbo,SP_TPO_list_tbo$other_sales_value,data.table(SP_reactive_input$selected_plan_split[[3]]),SP_reactive_input$opti_ppg_input_tbo,progress = TRUE)
        # if(!file.exists("op_tbo.RData")){
        #   SP_reactive_input$SP_opti_output_tbo <- optimization_tbo(data.table(SP_reactive_input$selected_plan_split[[1]]),data.table(SP_reactive_input$selected_plan_split[[2]]),data.table(SP_TPO_list_tbo$opti_const),data.table(SP_reactive_input$slot_invest_split[[1]]),data.table(SP_reactive_input$event_list_tbo),data.table(SP_reactive_input$slot_invest_split[[2]]),
        #                                                       data.table(SP_reactive_input$SP_opti_const),SP_TPO_list_tbo$other_sales,SP_TPO_list_tbo$goal,SP_TPO_list_tbo$sign,SP_TPO_list_tbo$ppg_exclude$PPG,SP_reactive_input$opti_format_input_tbo,SP_reactive_input$opti_ROI_input_tbo,SP_TPO_list_tbo$other_sales_value,data.table(SP_reactive_input$selected_plan_split[[3]]),SP_reactive_input$opti_ppg_input_tbo,progress = TRUE)
        #   SP_opti_output_tbo <- SP_reactive_input$SP_opti_output_tbo
        #   save(SP_opti_output_tbo,file = "op_tbo.RData")
        # }else{
        #   load("op_tbo.RData")
        #   SP_reactive_input$SP_opti_output_tbo <- SP_opti_output_tbo
        # }
        
      })
      ###Optimization output
      SP_reactive_input$opti_op_ppg_budget_const_tbo <- SP_reactive_input$SP_opti_output_tbo[[3]]
      SP_reactive_input$SP_opti_op_prepared_tbo_complete <- optimizer_op_prep_tbo(data.table(SP_reactive_input$SP_opti_output_tbo[[1]]),data.table(SP_reactive_input$selected_plan_split[[3]]),data.table(SP_reactive_input$selected_plan_split[[2]]),SP_TPO_list_tbo$ppg_exclude$PPG,SP_reactive_input$opti_format_input_tbo,data.table(SP_reactive_input$SP_opti_output_tbo[[2]]),SP_reactive_input$opti_ppg_input_tbo)
      SP_reactive_input$SP_opti_op_prepared_tbo <- data.table(SP_reactive_input$SP_opti_op_prepared_tbo_complete[[1]])
      
      if(input$SP_opti_disp_pp_const_on == TRUE){
        SP_reactive_input$SP_opti_op_prepared_tbo <- display_promo_const_tbo(SP_reactive_input$SP_opti_op_prepared_tbo,SP_reactive_input$event_list_tbo,SP_reactive_input$opti_ROI_input_tbo,SP_reactive_input$selected_plan_split[[4]],SP_reactive_input$selected_plan_split[[5]])
        SP_reactive_input$SP_opti_op_prepared_tbo <- display_slot_const_tbo(data.table(SP_reactive_input$SP_opti_op_prepared_tbo),input$SP_opti_max_display_on,SP_reactive_input$opti_ROI_input_tbo,SP_reactive_input$selected_plan_split[[4]],SP_reactive_input$selected_plan_split[[5]])
      }
      
      setnames(SP_reactive_input$SP_opti_op_prepared_tbo,c("SECTOR 2","TRADING COMPANY","PRODUCT RANGE","FORMAT"),c("Category","Manufacturer","Brand","Format"))
      SP_reactive_input$SP_opti_exc_brand_tbo <- data.table(SP_reactive_input$SP_opti_output_tbo[[2]])
      
      SP_reactive_input$SP_TPO_list_tbo <- SP_TPO_list_tbo
      
      SP_reactive_input$optimizer_op_download_tbo <- list("Optimizer Output" = SP_reactive_input$SP_opti_op_prepared_tbo,"Excluded PPG" = SP_reactive_input$SP_opti_exc_brand_tbo, "EAN PPG Mapping" = SP_reactive_input$EAN_PPG_download_tbo)
      write.xlsx(SP_reactive_input$optimizer_op_download_tbo, file = paste0(SP_reactive_input$folder_path,"/","Optimization Output TBO.xlsx"))
      
      ###Populating Simulator KPI's
      SP_reactive_input$kpi_map_calc <- data.frame("KPI_opti" = c("Scan Net Revenue(MM GBP)","GM % NR(%)","TI % NR(%)","TI % NIS(%)","Scan Gross Sales(MM GBP)","Gross Margin(MM GBP)","Volume Sales(MM Units)",input$SP_opti_ROI_selection,"Value Market Share(%)"), "KPI_calc" = c("net_sales","gm_%","spend_%","spend_%NIS","gross_sales","gm","vol_sales","ROI","market_share"),"const_order" = c(3,5,6,7,1,4,2,8,9),
                                                   "KPI_model" = c("Net_Sales_model","GM_percent_model","Trade_as_per_NR_model","Trade_as_per_NIS_model","Gross_sales_model","Gross_margin_model","Volume_sales_model","ROI_model","Market_Share_model"),check.names = FALSE)
      SP_reactive_input$kpi_map_calc_ret <- data.frame("KPI_opti" = c("RSV(MM GBP)","COGS(MM GBP)","CPD(MM GBP)","Fixed(MM GBP)","FM(MM GBP)","BM(MM GBP)","FM %","BM %"), "KPI_calc" = c("RSV","COGS","CPD","fixed","FM","BM","FM%","BM%"),"const_order" = c(1:8),check.names = FALSE)
      
      SP_reactive_input$KPI_calculation_tbo <- KPI_calc_tbo(SP_reactive_input$SP_TPO_list_tbo,SP_reactive_input$annual_optim_op,SP_reactive_input$SP_opti_op_prepared_tbo,SP_reactive_input$SP_opti_exc_brand_tbo,SP_reactive_input$SP_opti_const_grouped_on,SP_reactive_input$SP_RB_Delist_on,input$SP_opti_ROI_selection_on,SP_reactive_input$selected_plan_split[[3]],SP_reactive_input$SP_opti_op_prepared_tbo_complete[[2]],SP_reactive_input$SP_restrictions_tbo,input$SP_opti_op_include_exclude_on)
      SP_reactive_input$KPI_calculation_ret_tbo <- KPI_calc_ret_tbo(SP_reactive_input$SP_TPO_list_tbo,SP_reactive_input$annual_optim_op,SP_reactive_input$SP_opti_op_prepared_tbo,SP_reactive_input$SP_opti_exc_brand_tbo,SP_reactive_input$SP_opti_const_grouped_on,SP_reactive_input$SP_RB_Delist_on,input$SP_opti_ROI_selection_on,SP_reactive_input$selected_plan_split[[3]],SP_reactive_input$SP_opti_op_prepared_tbo_complete[[2]],SP_reactive_input$SP_restrictions_tbo,input$SP_opti_op_include_exclude_on)
      SP_reactive_input$SP_opti_op_prepared_tbo <- data.table(SP_reactive_input$SP_opti_op_prepared_tbo)
      ######Populating Ongoing Optimized calendar on optimization output screen
      SP_reactive_input$opti_cal_filtered_tbo <- data.table(SP_reactive_input$SP_opti_op_prepared_tbo)
      if(input$SP_opti_ROI_selection_on == "Incremental GM ROI"){
        SP_reactive_input$opti_cal_filtered_tbo[,ROI := sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }else if(input$SP_opti_ROI_selection_on == "Incremental NR ROI"){
        SP_reactive_input$opti_cal_filtered_tbo[,ROI := sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }else if(input$SP_opti_ROI_selection_on == "Incremental NIS ROI"){
        SP_reactive_input$opti_cal_filtered_tbo[,ROI := sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
      }
      
      SP_reactive_input$opti_cal_filtered_tbo$Date <- ymd(SP_reactive_input$opti_cal_filtered_tbo$Week_Ending)
      week_no <- data.frame("Date" = sort(unique(SP_reactive_input$opti_cal_filtered_tbo$Date)),"Week No" = c(1:length(sort(unique(SP_reactive_input$opti_cal_filtered_tbo$Week_Ending)))),check.names = FALSE)
      SP_reactive_input$opti_cal_filtered_tbo <- left_join(SP_reactive_input$opti_cal_filtered_tbo,week_no,by = "Date")
      
      if(!(is.null(SP_reactive_input$opti_cal_filtered_tbo))){
        SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event <- ifelse(SP_reactive_input$opti_cal_filtered_tbo$Display_Flag == 1,"Display",ifelse(SP_reactive_input$opti_cal_filtered_tbo$TPR_Flag == 1,"TPR","No Promo"))
        #SP_reactive_input$opti_cal_filtered_tbo$Promo_Price <- round(SP_reactive_input$opti_cal_filtered_tbo$Promo_Price,2)
        SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event == "No Promo",]$Promo_Price <- NA_real_
        SP_reactive_input$opti_cal_filtered_tbo <- data.table(SP_reactive_input$opti_cal_filtered_tbo)
        SP_reactive_input$opti_cal_filtered_tbo$ROI_cal <- SP_reactive_input$opti_cal_filtered_tbo$ROI
        SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event == "No Promo",]$ROI_cal <- -100
        SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event == "TPR",]$Tesco_Event <- "Shelf Promotion"
        SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event == "Display",]$Tesco_Event <- "Display Feature Promotion"
        SP_reactive_input$brks_ROI_tbo <- quantile(SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event != "No Promo" & SP_reactive_input$opti_cal_filtered_tbo$ROI_cal >= 0,]$ROI_cal, seq(0, 1, .05), na.rm = TRUE)
        SP_reactive_input$opti_cal_filtered_tbo$`LSM Promo Price` <- ""
        
        #SP_reactive_input$opti_cal_filtered_tbo$RSP_Unit <- ""
        SP_reactive_input$opti_cal_dcast_tbo <- data.table::dcast(unique(SP_reactive_input$opti_cal_filtered_tbo[,c("Format","PPG","PPG_Description","Week No","Promo_Price","RSP_Unit","LSM Promo Price","Tesco_Event","ROI_cal")]),Format + PPG + PPG_Description + RSP_Unit + `LSM Promo Price`~`Week No`,value.var= c("Promo_Price","Tesco_Event","ROI_cal"))
        SP_reactive_input$opti_cal_dcast_ROI_tbo <- SP_reactive_input$opti_cal_dcast_tbo
        
        names(SP_reactive_input$opti_cal_dcast_tbo) <- gsub("Promo_Price_","",names(SP_reactive_input$opti_cal_dcast_tbo))
        names(SP_reactive_input$opti_cal_dcast_tbo) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_tbo))
        
        names(SP_reactive_input$opti_cal_dcast_ROI_tbo) <- gsub("ROI_cal_","",names(SP_reactive_input$opti_cal_dcast_ROI_tbo))
        names(SP_reactive_input$opti_cal_dcast_ROI_tbo) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_ROI_tbo))
        
        setcolorder(SP_reactive_input$opti_cal_dcast_tbo,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`))),paste0("ROI_cal","_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))))
        setcolorder(SP_reactive_input$opti_cal_dcast_ROI_tbo,c("Format","PPG","PPG_Description","RSP_Unit","LSM Promo Price",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`))),paste0("Promo_Price","_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))))
        
      }
      
      output$SP_opti_cal_legend_tbo <- renderDataTable({
        datatable(transpose(data.frame(c("Shelf Promotion","Display Feature Promotion"))),class='display',
                  rownames = NULL, colnames = c(rep("",2)), 
                  options = list(dom = 'b',ordering=FALSE)) %>%
          formatStyle(columns=c(0:2),
                      background = styleEqual(c("Shelf Promotion","Display Feature Promotion"), 
                                              c('#0099DC', '#E42E92')),
                      color = styleEqual(c("Shelf Promotion","Display Feature Promotion"), 
                                         c('white', 'white'))) %>%
          formatStyle(1,border = styleInterval(0, c('auto', '1px solid white'))) %>%
          formatStyle(0,target = 'row',lineHeight='70%')
      })
      #toggleModal(session, "SP_opti_financial_on", toggle = "close")
      updateSelectizeInput(session,"SP_opti_op_coun_on",choices = SP_TPO_list_tbo$coun,selected = SP_TPO_list_tbo$coun)
      updateSelectizeInput(session,"SP_opti_op_cust_on",choices = SP_TPO_list_tbo$cust,selected = SP_TPO_list_tbo$cust)
      updateSelectizeInput(session,"SP_opti_op_cat_on",choices = SP_TPO_list_tbo$cat,selected = SP_TPO_list_tbo$cat)
      updateSelectizeInput(session,"SP_opti_op_brand_on",choices = SP_TPO_list_tbo$brand,selected = SP_TPO_list_tbo$brand)
      
      
      ##Additional tabs
      SP_test1_ip <- SP_reactive_input$SP_opti_op_prepared_tbo
      updateSelectizeInput(session,"SP_test1_format_on",choices = unique(SP_test1_ip$Format),selected = unique(SP_test1_ip$Format)[1])
      
      observeEvent({
        input$SP_test1_format_on},{
          updateSelectizeInput(session,"SP_test1_ppg_on",choices = unique(SP_test1_ip[SP_test1_ip$Format %in% input$SP_test1_format_on,]$PPG),selected = unique(SP_test1_ip[SP_test1_ip$Format == input$SP_test1_format_on,]$PPG)[1])
        }
      )
      
      updateSelectizeInput(session,"SP_test2_brand_on",choices = unique(SP_test1_ip$Brand),selected = unique(SP_test1_ip$Brand)[1])
      observeEvent({
        input$SP_test2_brand_on},{
          updateSelectizeInput(session,"SP_test2_format_on",choices = c("",unique(SP_test1_ip[SP_test1_ip$Brand %in% input$SP_test2_brand_on,]$Format)),selected = "")
        }
      )
      
      observeEvent({
        input$SP_test2_format_on},{
          
          updateSelectizeInput(session,"SP_test2_ppg_on",choices = c("",unique(SP_test1_ip[SP_test1_ip$Format %in% input$SP_test2_format_on,]$PPG)),selected = "")
        }
      )
      updateSelectizeInput(session,"SP_test3_cat_on",choices = unique(SP_test1_ip$Category),selected = unique(SP_test1_ip$Category)[1])
      observeEvent({
        input$SP_test3_cat_on},{
          updateSelectizeInput(session,"SP_test3_brand_on",choices = c("",unique(SP_test1_ip[SP_test1_ip$Category %in% input$SP_test3_cat_on,]$Brand)),selected = "")
        }
      )
      
      observeEvent({
        input$SP_test3_brand_on},{
          updateSelectizeInput(session,"SP_test3_format_on",choices = c("",unique(SP_test1_ip[SP_test1_ip$Brand %in% input$SP_test3_brand_on,]$Format)),selected = "")
        }
      )
      
      observeEvent({
        input$SP_test3_format_on},{
          
          updateSelectizeInput(session,"SP_test3_ppg_on",choices = c("",unique(SP_test1_ip[SP_test1_ip$Format %in% input$SP_test3_format_on,]$PPG)),selected = "")
        }
      )
      
      updateSelectizeInput(session,"SP_test4_brand_on",choices = unique(SP_test1_ip$Brand),selected = unique(SP_test1_ip$Brand)[1])
      updateSelectizeInput(session,"SP_test4_start_date_on",choices = unique(SP_test1_ip$Week_Ending),selected = unique(SP_test1_ip$Week_Ending)[1])
      
      observeEvent(input$SP_test4_start_date_on,{
        
        if(input$SP_test4_start_date_on != ""){
          
          updateSelectizeInput(session,"SP_test4_end_date_on",choices = unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date_on))],selected = unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date_on))][length(unique(SP_test1_ip$Week_Ending)[(unique(SP_test1_ip$Week_Ending) >= as.Date(input$SP_test4_start_date_on))])])
        }
      })
      
      observeEvent({
        input$SP_test4_brand_on},{
          updateSelectizeInput(session,"SP_test4_format_on",choices = c("",unique(SP_test1_ip[SP_test1_ip$Brand %in% input$SP_test4_brand_on,]$Format)),selected = "")
        }
      )
      
      observeEvent({
        input$SP_test4_format_on},{
          
          updateSelectizeInput(session,"SP_test4_ppg_on",choices = c("",unique(SP_test1_ip[SP_test1_ip$Format %in% input$SP_test4_format_on,]$PPG)),selected = "")
        }
      )
      #SP_reactive_input$annual_optim_op$opti_output
      observeEvent({
        input$SP_test1_format_on
        input$SP_test1_ppg_on},{
          if(input$SP_test1_ppg_on != ""){
            SP_reactive_input$SP_graph_1_ip_final <- SP_reactive_input$annual_optim_op$opti_output[SP_reactive_input$annual_optim_op$opti_output$Format %in% input$SP_test1_format_on & SP_reactive_input$annual_optim_op$opti_output$PPG %in% input$SP_test1_ppg_on,c("Date","Promo_Price","Total_Sales","TPR_Flag","Display_Flag")]
            SP_reactive_input$SP_graph_1_ip_final$Event <- ifelse(SP_reactive_input$SP_graph_1_ip_final$Display_Flag == 1,"Display",ifelse(SP_reactive_input$SP_graph_1_ip_final$TPR_Flag == 1,"Shelf","No Promo"))
            
            SP_reactive_input$SP_graph_1_ip_tbo <- SP_reactive_input$SP_opti_op_prepared_tbo[SP_reactive_input$SP_opti_op_prepared_tbo$Format %in% input$SP_test1_format_on & SP_reactive_input$SP_opti_op_prepared_tbo$PPG %in% input$SP_test1_ppg_on,c("Date","Promo_Price","Total_Sales","TPR_Flag","Display_Flag")]
            SP_reactive_input$SP_graph_1_ip_tbo$Event <- ifelse(SP_reactive_input$SP_graph_1_ip_tbo$Display_Flag == 1,"Display",ifelse(SP_reactive_input$SP_graph_1_ip_tbo$TPR_Flag == 1,"Shelf","No Promo"))
          }
        })
      
      observeEvent(c(
        input$SP_test2_brand_on,
        input$SP_test2_format_on,
        input$SP_test2_ppg_on,
        input$SP_test4_start_date_on,
        input$SP_test4_end_date_on,
        input$SP_opti_op_include_exclude_on,
        input$SP_sim_replace_confirm_on),{
          if(!(is.null(SP_reactive_input$SP_opti_op_prepared_tbo))){
            if(input$SP_test4_start_date_on != "" & input$SP_test4_end_date_on != ""){
              
              SP_reactive_input$SP_opti_exc_brand_1 <- data.table(SP_reactive_input$SP_opti_exc_brand_tbo)
              
              SP_reactive_input$SP_opti_const_selected_1 <- SP_reactive_input$annual_optim_op$opti_output
              
              if(is.null(input$SP_test2_ppg_on) & is.null(input$SP_test2_format_on)){
                SP_reactive_input$SP_test2_ly <- SP_reactive_input$SP_opti_const_selected_1[SP_reactive_input$SP_opti_const_selected_1$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_const_selected_1$Week_Ending <= input$SP_test4_end_date_on & SP_reactive_input$SP_opti_const_selected_1$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected_1$Brand %in% (input$SP_brand),.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                                               "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "Format"]
                
                SP_reactive_input$SP_test2_exclude <- SP_reactive_input$SP_opti_exc_brand_1[SP_reactive_input$SP_opti_exc_brand_1$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_exc_brand_1$Week_Ending <= input$SP_test4_end_date_on & SP_reactive_input$SP_opti_exc_brand_1$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_exc_brand_1$Brand %in% (input$SP_brand),.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                           "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "Format"]
                
                if(input$SP_opti_op_include_exclude_on == TRUE){
                  SP_reactive_input$SP_test2_ly_delist <- data.frame(SP_reactive_input$SP_test2_ly)
                  
                }else if(input$SP_opti_op_include_exclude_on == FALSE){
                  SP_reactive_input$SP_test2_ly_delist <- cbind(data.frame("Format" = SP_reactive_input$SP_test2_ly$Format),rbindlist(apply(SP_reactive_input$SP_test2_ly,1,function(x) {if(x[1] %in% SP_reactive_input$SP_test2_exclude$Format){as.numeric(x[2:length(x)]) - SP_reactive_input$SP_test2_exclude[Format == x[1],c(2:length(SP_reactive_input$SP_test2_exclude)),with = FALSE]}
                    else{setnames(data.frame(matrix(as.numeric(x[2:length(x)]),ncol = 7)),c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))}})))
                }
                SP_reactive_input$SP_test2_final_delist_exclude <- SP_reactive_input$SP_test2_ly_delist
                SP_reactive_input$SP_test2_final_delist_exclude$GM_NR <- SP_reactive_input$SP_test2_final_delist_exclude$Gross_Margin * 100/SP_reactive_input$SP_test2_final_delist_exclude$NR
                SP_reactive_input$SP_test2_final_delist_exclude$TI_NR <- SP_reactive_input$SP_test2_final_delist_exclude$TI * 100/SP_reactive_input$SP_test2_final_delist_exclude$NR
                
                SP_reactive_input$SP_test2_tbo_op <- SP_reactive_input$SP_opti_op_prepared_tbo[SP_reactive_input$SP_opti_op_prepared_tbo$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_op_prepared_tbo$Week_Ending <= input$SP_test4_end_date_on,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                             "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "Format"]
                
                SP_reactive_input$SP_test2_tbo_op <- left_join(SP_reactive_input$SP_test2_ly[,c("Format")],SP_reactive_input$SP_test2_tbo_op,by = "Format")
                SP_reactive_input$SP_test2_tbo_op[is.na(SP_reactive_input$SP_test2_tbo_op)] <- 0
                if(input$SP_opti_op_include_exclude_on == TRUE){
                  SP_reactive_input$SP_test2_tbo_op <- cbind(data.frame("Format" = SP_reactive_input$SP_test2_tbo_op$Format),rbindlist(apply(SP_reactive_input$SP_test2_tbo_op,1,function(x) {if(x[1] %in% SP_reactive_input$SP_test2_exclude$Format){as.numeric(x[2:length(x)]) + SP_reactive_input$SP_test2_exclude[Format == x[1],c(2:length(SP_reactive_input$SP_test2_exclude)),with = FALSE]}
                    else{setnames(data.frame(matrix(as.numeric(x[2:length(x)]),ncol = 7)),c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))}})))
                }
                SP_reactive_input$SP_test2_tbo_op$GM_NR <- SP_reactive_input$SP_test2_tbo_op$Gross_Margin * 100/SP_reactive_input$SP_test2_tbo_op$NR
                SP_reactive_input$SP_test2_tbo_op$TI_NR <- SP_reactive_input$SP_test2_tbo_op$TI * 100/SP_reactive_input$SP_test2_tbo_op$NR
                
                SP_reactive_input$SP_test2_tbo_diff <- left_join(SP_reactive_input$SP_test2_final_delist_exclude,SP_reactive_input$SP_test2_tbo_op, by = "Format")
                SP_reactive_input$SP_test2_tbo_diff[is.na(SP_reactive_input$SP_test2_tbo_diff)] <- 0
                
                SP_reactive_input$SP_test2_tbo_diff_calc <- cbind(data.frame("Format" = SP_reactive_input$SP_test2_tbo_diff$Format),(SP_reactive_input$SP_test2_tbo_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_tbo_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_tbo_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_tbo_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_tbo_diff[,c("GM_NR.x","TI_NR.x")])*100)
                SP_reactive_input$SP_test2_tbo_diff_calc[is.na(SP_reactive_input$SP_test2_tbo_diff_calc)] <- 0
                
              }else if(is.null(input$SP_test2_ppg_on) & !is.null(input$SP_test2_format_on)){
                SP_reactive_input$SP_test2_ly <- SP_reactive_input$SP_opti_const_selected_1[SP_reactive_input$SP_opti_const_selected_1$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_const_selected_1$Week_Ending <= input$SP_test4_end_date_on & SP_reactive_input$SP_opti_const_selected_1$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected_1$Brand %in% (input$SP_brand) & SP_reactive_input$SP_opti_const_selected_1$Format %in% input$SP_test2_format_on,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                SP_reactive_input$SP_test2_exclude <- SP_reactive_input$SP_opti_exc_brand_1[SP_reactive_input$SP_opti_exc_brand_1$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_exc_brand_1$Week_Ending <= input$SP_test4_end_date_on & SP_reactive_input$SP_opti_exc_brand_1$Format %in% input$SP_test2_format_on,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                               "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                if(input$SP_opti_op_include_exclude_on == TRUE){
                  SP_reactive_input$SP_test2_ly_delist <- data.frame(SP_reactive_input$SP_test2_ly)
                  
                }else if(input$SP_opti_op_include_exclude_on == FALSE){
                  SP_reactive_input$SP_test2_ly_delist <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_ly$PPG),rbindlist(apply(SP_reactive_input$SP_test2_ly,1,function(x) {if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){as.numeric(x[2:length(x)]) - SP_reactive_input$SP_test2_exclude[PPG == x[1],c(2:length(SP_reactive_input$SP_test2_exclude)),with = FALSE]}
                    else{setnames(data.frame(matrix(as.numeric(x[2:length(x)]),ncol = 7)),c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))}})))
                }
                
                SP_reactive_input$SP_test2_final_delist_exclude <- SP_reactive_input$SP_test2_ly_delist
                SP_reactive_input$SP_test2_final_delist_exclude$GM_NR <- SP_reactive_input$SP_test2_final_delist_exclude$Gross_Margin * 100/SP_reactive_input$SP_test2_final_delist_exclude$NR
                SP_reactive_input$SP_test2_final_delist_exclude$TI_NR <- SP_reactive_input$SP_test2_final_delist_exclude$TI * 100/SP_reactive_input$SP_test2_final_delist_exclude$NR
                
                SP_reactive_input$SP_test2_tbo_op <- SP_reactive_input$SP_opti_op_prepared_tbo[SP_reactive_input$SP_opti_op_prepared_tbo$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_op_prepared_tbo$Week_Ending <= input$SP_test4_end_date_on & SP_reactive_input$SP_opti_op_prepared_tbo$Format %in% input$SP_test2_format_on,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                              "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                SP_reactive_input$SP_test2_tbo_op <- left_join(SP_reactive_input$SP_test2_ly[,c("PPG")],SP_reactive_input$SP_test2_tbo_op,by = "PPG")
                SP_reactive_input$SP_test2_tbo_op[is.na(SP_reactive_input$SP_test2_tbo_op)] <- 0
                if(input$SP_opti_op_include_exclude_on == TRUE){
                  SP_reactive_input$SP_test2_tbo_op <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_tbo_op$PPG),rbindlist(apply(SP_reactive_input$SP_test2_tbo_op,1,function(x) {if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){as.numeric(x[2:length(x)]) + SP_reactive_input$SP_test2_exclude[PPG == x[1],c(2:length(SP_reactive_input$SP_test2_exclude)),with = FALSE]}
                    else{setnames(data.frame(matrix(as.numeric(x[2:length(x)]),ncol = 7)),c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))}})))
                }
                
                SP_reactive_input$SP_test2_tbo_op$GM_NR <- SP_reactive_input$SP_test2_tbo_op$Gross_Margin * 100/SP_reactive_input$SP_test2_tbo_op$NR
                SP_reactive_input$SP_test2_tbo_op$TI_NR <- SP_reactive_input$SP_test2_tbo_op$TI * 100/SP_reactive_input$SP_test2_tbo_op$NR
                
                SP_reactive_input$SP_test2_tbo_diff <- left_join(SP_reactive_input$SP_test2_final_delist_exclude,SP_reactive_input$SP_test2_tbo_op, by = "PPG")
                SP_reactive_input$SP_test2_tbo_diff[is.na(SP_reactive_input$SP_test2_tbo_diff)] <- 0
                
                SP_reactive_input$SP_test2_tbo_diff_calc <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_tbo_diff$PPG),(SP_reactive_input$SP_test2_tbo_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_tbo_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_tbo_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_tbo_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_tbo_diff[,c("GM_NR.x","TI_NR.x")])*100)
                SP_reactive_input$SP_test2_tbo_diff_calc[is.na(SP_reactive_input$SP_test2_tbo_diff_calc)] <- 0
                
              }else if(!is.null(input$SP_test2_ppg_on)){
                SP_reactive_input$SP_test2_ly <- SP_reactive_input$SP_opti_const_selected_1[SP_reactive_input$SP_opti_const_selected_1$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_const_selected_1$Week_Ending <= input$SP_test4_end_date_on & SP_reactive_input$SP_opti_const_selected_1$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$SP_opti_const_selected_1$Brand %in% (input$SP_brand) & SP_reactive_input$SP_opti_const_selected_1$Format %in% input$SP_test2_format_on & SP_reactive_input$SP_opti_const_selected_1$PPG %in% input$SP_test2_ppg_on,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                SP_reactive_input$SP_test2_exclude <- SP_reactive_input$SP_opti_exc_brand_1[SP_reactive_input$SP_opti_exc_brand_1$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_exc_brand_1$Week_Ending <= input$SP_test4_end_date_on & SP_reactive_input$SP_opti_exc_brand_1$Format %in% input$SP_test2_format_on & SP_reactive_input$SP_opti_exc_brand_1$PPG %in% input$SP_test2_ppg_on,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                      "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                if(input$SP_opti_op_include_exclude_on == TRUE){
                  SP_reactive_input$SP_test2_ly_delist <- data.frame(SP_reactive_input$SP_test2_ly)
                  
                }else if(input$SP_opti_op_include_exclude_on == FALSE){
                  SP_reactive_input$SP_test2_ly_delist <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_ly$PPG),rbindlist(apply(SP_reactive_input$SP_test2_ly,1,function(x) {if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){as.numeric(x[2:length(x)]) - SP_reactive_input$SP_test2_exclude[PPG == x[1],c(2:length(SP_reactive_input$SP_test2_exclude)),with = FALSE]}
                    else{setnames(data.frame(matrix(as.numeric(x[2:length(x)]),ncol = 7)),c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))}})))
                }
                SP_reactive_input$SP_test2_final_delist_exclude <- SP_reactive_input$SP_test2_ly_delist
                SP_reactive_input$SP_test2_final_delist_exclude$GM_NR <- SP_reactive_input$SP_test2_final_delist_exclude$Gross_Margin * 100/SP_reactive_input$SP_test2_final_delist_exclude$NR
                SP_reactive_input$SP_test2_final_delist_exclude$TI_NR <- SP_reactive_input$SP_test2_final_delist_exclude$TI * 100/SP_reactive_input$SP_test2_final_delist_exclude$NR
                
                SP_reactive_input$SP_test2_tbo_op <- SP_reactive_input$SP_opti_op_prepared_tbo[SP_reactive_input$SP_opti_op_prepared_tbo$Week_Ending >= input$SP_test4_start_date_on & SP_reactive_input$SP_opti_op_prepared_tbo$Week_Ending <= input$SP_test4_end_date_on & SP_reactive_input$SP_opti_op_prepared_tbo$Format %in% input$SP_test2_format_on & SP_reactive_input$SP_opti_op_prepared_tbo$PPG %in% input$SP_test2_ppg_on,.("NR" = sum(Net_Revenue), "Total_Sales" = sum(Total_Sales), "Gross_Sales" = sum(Gross_Sales), "Gross_Margin" = sum(GM_Abs),"TI" = sum(Total_Trade_Investment),
                                                                                                                                                                                                                                                                                                                                                                                                                                         "GM_NR" = sum(GM_Abs) * 100/sum(Net_Revenue), "TI_NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue)),by = "PPG"]
                
                SP_reactive_input$SP_test2_tbo_op <- left_join(SP_reactive_input$SP_test2_ly[,c("PPG")],SP_reactive_input$SP_test2_tbo_op,by = "PPG")
                SP_reactive_input$SP_test2_tbo_op[is.na(SP_reactive_input$SP_test2_tbo_op)] <- 0
                if(input$SP_opti_op_include_exclude_on == TRUE){
                  SP_reactive_input$SP_test2_tbo_op <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_tbo_op$PPG),rbindlist(apply(SP_reactive_input$SP_test2_tbo_op,1,function(x) {if(x[1] %in% SP_reactive_input$SP_test2_exclude$PPG){as.numeric(x[2:length(x)]) + SP_reactive_input$SP_test2_exclude[PPG == x[1],c(2:length(SP_reactive_input$SP_test2_exclude)),with = FALSE]}
                    else{setnames(data.frame(matrix(as.numeric(x[2:length(x)]),ncol = 7)),c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"))}})))
                }
                SP_reactive_input$SP_test2_tbo_op$GM_NR <- SP_reactive_input$SP_test2_tbo_op$Gross_Margin * 100/SP_reactive_input$SP_test2_tbo_op$NR
                SP_reactive_input$SP_test2_tbo_op$TI_NR <- SP_reactive_input$SP_test2_tbo_op$TI * 100/SP_reactive_input$SP_test2_tbo_op$NR
                
                SP_reactive_input$SP_test2_tbo_diff <- left_join(SP_reactive_input$SP_test2_final_delist_exclude,SP_reactive_input$SP_test2_tbo_op, by = "PPG")
                SP_reactive_input$SP_test2_tbo_diff[is.na(SP_reactive_input$SP_test2_tbo_diff)] <- 0
                
                SP_reactive_input$SP_test2_tbo_diff_calc <- cbind(data.frame("PPG" = SP_reactive_input$SP_test2_tbo_diff$PPG),(SP_reactive_input$SP_test2_tbo_diff[,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y")] - SP_reactive_input$SP_test2_tbo_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")]) * 100/SP_reactive_input$SP_test2_tbo_diff[,c("NR.x","Total_Sales.x","Gross_Sales.x","Gross_Margin.x","TI.x")],(SP_reactive_input$SP_test2_tbo_diff[,c("GM_NR.y","TI_NR.y")] - SP_reactive_input$SP_test2_tbo_diff[,c("GM_NR.x","TI_NR.x")])*100)
                SP_reactive_input$SP_test2_tbo_diff_calc[is.na(SP_reactive_input$SP_test2_tbo_diff_calc)] <- 0
                
              }
              SP_reactive_input$SP_test2_tbo_op[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")] <- SP_reactive_input$SP_test2_tbo_op[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")]/10^3
              
              
              setnames(SP_reactive_input$SP_test2_tbo_op,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"),c("Scan Net Revenue NonLSM('000 GBP)","Volume Sales NonLSM('000 Units)","Gross Sales NonLSM('000 GBP)","Gross Margin NonLSM('000 GBP)","Trade Investment NonLSM('000 GBP)","GM % NR NonLSM","TI % NR NonLSM"))
              setnames(SP_reactive_input$SP_test2_tbo_diff_calc,c("NR.y","Total_Sales.y","Gross_Sales.y","Gross_Margin.y","TI.y","GM_NR.y","TI_NR.y"),c("Scan Net Revenue(% Diff)","Volume Sales(% Diff)","Gross Sales % Diff","Gross Margin(% Diff)","Trade Investment(% Diff)","GM % NR(% Diff)","TI % NR(% Diff)"))
              
              SP_reactive_input$SP_test2_final_delist_exclude[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")] <- SP_reactive_input$SP_test2_final_delist_exclude[,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI")]/10^3
              
              setnames(SP_reactive_input$SP_test2_final_delist_exclude,c("NR","Total_Sales","Gross_Sales","Gross_Margin","TI","GM_NR","TI_NR"),c("Scan Net Revenue('000 GBP)","Volume Sales('000 Units)","Gross Sales('000 GBP)","Gross Margin('000 GBP)","Trade Investment('000 GBP)","GM % NR","TI % NR"))
              
            }
          }
        })
      
      observeEvent(c(
        input$SP_test3_cat_on,
        input$SP_test3_brand_on,
        input$SP_test3_format_on,
        input$SP_test3_ppg_on,
        input$SP_sim_replace_confirm_on),{
          if(!(is.null(SP_reactive_input$SP_opti_op_prepared_tbo))){
            
            SP_reactive_input$SP_opti_const_selected_2 <- data.table(SP_reactive_input$annual_optim_op$opti_output[SP_reactive_input$annual_optim_op$opti_output$Manufacturer %in% SP_reactive_input$SP_manuf & SP_reactive_input$annual_optim_op$opti_output$Brand %in% input$SP_brand,])
            #
            if(is.null(input$SP_test3_brand_on) & is.null(input$SP_test3_ppg_on) & is.null(input$SP_test3_format_on)){
              SP_reactive_input$SP_promo_eff_opti_final <- SP_reactive_input$SP_opti_const_selected_2[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand)]
              
              SP_reactive_input$SP_promo_eff_opti_tbo <- SP_reactive_input$SP_opti_op_prepared_tbo[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand)]
              
              output$SP_graph_roi_tbo_op <- renderPlotly({
                promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_tbo,"Brand",input$SP_promo_ROI_selection_tbo,SP_reactive_input$SP_promo_eff_opti_tbo,SP_reactive_input$SP_promo_eff_opti_final)
              })
              
              output$SP_graph_roi_tbo_ly <- renderPlotly({
                promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_final,"Brand",input$SP_promo_ROI_selection_tbo,SP_reactive_input$SP_promo_eff_opti_tbo,SP_reactive_input$SP_promo_eff_opti_tbo)
              })
              
            }else if(is.null(input$SP_test3_ppg_on) & is.null(input$SP_test3_format_on)){
              SP_reactive_input$SP_promo_eff_opti_final <- SP_reactive_input$SP_opti_const_selected_2[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format)]
              SP_reactive_input$SP_promo_eff_opti_tbo <- SP_reactive_input$SP_opti_op_prepared_tbo[,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format)]
              
              output$SP_graph_roi_tbo_op <- renderPlotly({
                promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_tbo,"Format",input$SP_promo_ROI_selection_tbo,SP_reactive_input$SP_promo_eff_opti_tbo,SP_reactive_input$SP_promo_eff_opti_final)
              })
              
              output$SP_graph_roi_tbo_ly <- renderPlotly({
                promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_final,"Format",input$SP_promo_ROI_selection_tbo,SP_reactive_input$SP_promo_eff_opti_tbo,SP_reactive_input$SP_promo_eff_opti_tbo)
              })
              
            }else if(is.null(input$SP_test3_ppg_on) & !is.null(input$SP_test3_format_on)){
              SP_reactive_input$SP_promo_eff_opti_final <- SP_reactive_input$SP_opti_const_selected_2[SP_reactive_input$SP_opti_const_selected_2$Format %in% input$SP_test3_format_on,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG)]
              SP_reactive_input$SP_promo_eff_opti_tbo <- SP_reactive_input$SP_opti_op_prepared_tbo[SP_reactive_input$SP_opti_op_prepared_tbo$Format %in% input$SP_test3_format_on,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG)]
              output$SP_graph_roi_tbo_op <- renderPlotly({
                promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_tbo,"PPG",input$SP_promo_ROI_selection_tbo,SP_reactive_input$SP_promo_eff_opti_tbo,SP_reactive_input$SP_promo_eff_opti_final)
              })
              
              output$SP_graph_roi_tbo_ly <- renderPlotly({
                promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_final,"PPG",input$SP_promo_ROI_selection_tbo,SP_reactive_input$SP_promo_eff_opti_tbo,SP_reactive_input$SP_promo_eff_opti_tbo)
              })
              
            }else if(!is.null(input$SP_test3_ppg_on)){
              SP_reactive_input$SP_promo_eff_opti_final <- SP_reactive_input$SP_opti_const_selected_2[SP_reactive_input$SP_opti_const_selected_2$Format %in% input$SP_test3_format_on & SP_reactive_input$SP_opti_const_selected_2$PPG %in% input$SP_test3_ppg_on,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG,Event_Number)]
              SP_reactive_input$SP_promo_eff_opti_final$Tesco_Week_No <- paste0("Event-",SP_reactive_input$SP_promo_eff_opti_final$Tesco_Week_No)
              setnames(SP_reactive_input$SP_promo_eff_opti_final,"Tesco_Week_No","Event ID")
              
              SP_reactive_input$SP_promo_eff_opti_tbo <- SP_reactive_input$SP_opti_op_prepared_tbo[SP_reactive_input$SP_opti_op_prepared_tbo$Format %in% input$SP_test3_format_on & SP_reactive_input$SP_opti_op_prepared_tbo$PPG %in% input$SP_test3_ppg_on,.("Incremental Sales" = sum(R_GM_Inc),"Investment" = sum(R_Trade_Inv_Inc),"Incremental GM ROI" = round(sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),2),"Incremental NR ROI" = round(sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),2), "Incremental NIS ROI" = round(sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc),2)),by = .(Brand,Format,PPG,Tesco_Week_No)]
              # if(all(is.na(SP_reactive_input$SP_promo_eff_opti_tbo$ROI)) & all(is.na(SP_reactive_input$SP_promo_eff_opti_tbo$Investment))){
              #   SP_reactive_input$SP_promo_eff_opti_tbo <- NULL
              # }
              SP_reactive_input$SP_promo_eff_opti_tbo$Tesco_Week_No <- paste0("Event-",SP_reactive_input$SP_promo_eff_opti_tbo$Tesco_Week_No)
              setnames(SP_reactive_input$SP_promo_eff_opti_tbo,"Tesco_Week_No","Event ID")
              
              output$SP_graph_roi_tbo_op <- renderPlotly({
                validate(
                  need(SP_reactive_input$SP_promo_eff_opti_tbo,"No Promotion happened for the selected PPG within selected time period")
                )
                
                promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_tbo,"Event ID",input$SP_promo_ROI_selection_tbo,SP_reactive_input$SP_promo_eff_opti_tbo,SP_reactive_input$SP_promo_eff_opti_final)
              })
              
              output$SP_graph_roi_tbo_ly <- renderPlotly({
                validate(
                  need(SP_reactive_input$SP_promo_eff_opti_final,"No Promotion happened for the selected PPG within selected time period")
                )
                promo_ROI_chart_op(SP_reactive_input$SP_promo_eff_opti_final,"Event ID",input$SP_promo_ROI_selection_tbo,SP_reactive_input$SP_promo_eff_opti_tbo,SP_reactive_input$SP_promo_eff_opti_tbo)
              })
              
            }
          }
        })
      
      removeModal()
      updateTabItems(session, "sidebar_main", selected = "SP_subMenu11")
      SP_reactive_input$ppg_budget_const_tbo <- SP_reactive_input$opti_op_ppg_budget_const_tbo[,c("PPG","Status")]
      
      if(any(SP_reactive_input$ppg_budget_const_tbo$Status %in% c("Budget less than Minimum","Budget more than Maximum"))){
        sendSweetAlert(session,"Budget criteria not satisfied for Ongoing Optimization",
                       paste0("Min Investment violated for PPG's: ", 
                              paste(unique(SP_reactive_input$ppg_budget_const_tbo[SP_reactive_input$ppg_budget_const$Status == "Budget less than Minimum",]$PPG), collapse = ","),
                              "   Max Investment violated for PPG's: ",
                              paste(unique(SP_reactive_input$ppg_budget_const_tbo[SP_reactive_input$ppg_budget_const$Status == "Budget more than Maximum",]$PPG), collapse = ",")),
                       
                       
                       type = "warning")
      }
      
    }
  })
  
  output$SP_opti_op_top_panel_download_on <- downloadHandler(
    # validate(
    #   ,
    #   need(SP_reactive_input$SP_cmp_scn_download,"Select a TPO ID")
    # ),
    filename = function() {
      paste("Optimization Output(Ongoing).xlsx")
    },
    content = function(file) {
      write.xlsx(SP_reactive_input$optimizer_op_download_tbo, file,row.names = FALSE)
    }
  )
  
  output$SP_const1_KPI_tbo <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation_tbo, "Run Optimization")
    )
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_tbo, id = c("goal","sign"))
    goal_selected <- as.character(unique(SP_opti_op_KPI_data$goal))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(SP_opti_op_KPI_data,goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_opti_op_KPI_data <- data.table(left_join(SP_opti_op_KPI_data,SP_reactive_input$kpi_map_calc,by = c("variable" = "KPI_calc")))
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,SP_reactive_input$SP_TPO_list_tbo$opti_const[,c("KPI_Mapping","Constraint Order")],by = c("KPI_model" = "KPI_Mapping"))
    
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","actual","planned","optimized","min","max","old","new","const_order","Constraint Order")]
    names(SP_opti_op_KPI_data) <- c("KPI","Actuals till date","Planned for selected period","Optimized for selected period","Min Constraint","Max Constraint","Old Forecast","New Forecast","const_order","Constraint Order")
    SP_opti_op_KPI_data[,c("Actuals till date","Planned for selected period","Optimized for selected period","Min Constraint","Max Constraint","Old Forecast","New Forecast")] <- lapply(SP_opti_op_KPI_data[,c("Actuals till date","Planned for selected period","Optimized for selected period","Min Constraint","Max Constraint","Old Forecast","New Forecast")],function(X) as.numeric(X))
    #
    SP_opti_op_KPI_data$`Ongoing vs Finalized Plan` <- (SP_opti_op_KPI_data$`New Forecast` - SP_opti_op_KPI_data$`Old Forecast`) * 100/SP_opti_op_KPI_data$`Old Forecast`
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`Ongoing vs Finalized Plan` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`New Forecast` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("gm_%","spend_%","market_share","spend_%NIS"),]$`old Forecast`) * 100
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Ongoing vs Finalized Plan` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`New Forecast` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("ROI"),]$`Old Forecast`)
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`sign` <- ifelse(SP_opti_op_KPI_data$`Ongoing vs Finalized Plan` >= 0, "Positive", "Negative")
    #SP_opti_op_KPI_data$`non_lsm_sign` <- ifelse(SP_opti_op_KPI_data$`Unconstrained Change vs LY` >= 0, "Positive", "Negative")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %in% c("Trade Spend % of NIS","Trade Spend % of NR"),]$sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %in% c("Trade Spend % of NIS","Trade Spend % of NR"),]$`Ongoing vs Finalized Plan` >= 0, "Negative", "Positive")
    #SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %in% c("Trade Spend % of NIS","Trade Spend % of NR"),]$non_lsm_sign <- ifelse(SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %in% c("Trade Spend % of NIS","Trade Spend % of NR"),]$`Unconstrained Change vs LY` >= 0, "Negative", "Positive")
    
    SP_opti_op_KPI_data$`Min Constraint` <- as.character(SP_opti_op_KPI_data$`Min Constraint`)
    SP_opti_op_KPI_data$`Max Constraint` <- as.character(SP_opti_op_KPI_data$`Max Constraint`)
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %like% goal_selected,]$`Min Constraint` <- "Goal Selected"
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$KPI %like% goal_selected,]$`Max Constraint` <- "Goal Selected"
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% "const_order"),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(10)),list(className = 'dt-center', targets = 0:9)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      formatStyle("KPI", backgroundColor = "#0099DC", color = "#FFFFFF") %>%
      formatStyle("Ongoing vs Finalized Plan","sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      #formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_ret_tbo <- renderDataTable({
    
    validate(
      need(SP_reactive_input$KPI_calculation_ret_tbo, "Run Optimization")
    )
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_ret_tbo, id = c("goal","sign"))
    goal_selected <- as.character(unique(SP_opti_op_KPI_data$goal))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(SP_opti_op_KPI_data,goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc_ret$KPI_calc <- as.character(SP_reactive_input$kpi_map_calc_ret$KPI_calc)
    SP_opti_op_KPI_data <- data.table(left_join(SP_opti_op_KPI_data,SP_reactive_input$kpi_map_calc_ret,by = c("variable" = "KPI_calc")))
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","actual","planned","optimized","old","new","const_order","variable")]
    names(SP_opti_op_KPI_data) <- c("KPI","Actuals till date","Planned for selected period","Optimized for selected period","Old Forecast","New Forecast","const_order","variable")
    SP_opti_op_KPI_data[,c("Actuals till date","Planned for selected period","Optimized for selected period","Old Forecast","New Forecast")] <- lapply(SP_opti_op_KPI_data[,c("Actuals till date","Planned for selected period","Optimized for selected period","Old Forecast","New Forecast")],function(X) as.numeric(X))
    #
    SP_opti_op_KPI_data$`Ongoing vs Finalized Plan` <- (SP_opti_op_KPI_data$`New Forecast` - SP_opti_op_KPI_data$`Old Forecast`) * 100/SP_opti_op_KPI_data$`Old Forecast`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Ongoing vs Finalized Plan` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`New Forecast` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Old Forecast`) * 100
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`sign` <- ifelse(SP_opti_op_KPI_data$`Ongoing vs Finalized Plan` >= 0, "Positive", "Negative")
    
    SP_opti_op_KPI_data$`Ongoing vs Finalized Plan` <- as.character(round(SP_opti_op_KPI_data$`Ongoing vs Finalized Plan`,2))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Ongoing vs Finalized Plan` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Ongoing vs Finalized Plan`,"%")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Ongoing vs Finalized Plan` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Ongoing vs Finalized Plan`,"bps")
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(7)),list(className = 'dt-center', targets = 0:6)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      formatStyle("KPI", backgroundColor = "#0099DC", color = "#FFFFFF") %>%
      formatStyle("Ongoing vs Finalized Plan","sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      #formatStyle("Unconstrained Change vs LY","non_lsm_sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_const1_KPI_cat_ret_tbo <- renderDataTable({
    validate(
      need(SP_reactive_input$KPI_calculation_ret_tbo, "Run Optimization")
    )
    
    if(SP_reactive_input$opti_format_input_tbo == "ALL"){
      competition_display_calc <- sum(isolate(SP_reactive_input$competition_display)$Display_Cost_Comp)/10^6
      competition_KPI <- isolate(SP_reactive_input$competition_KPI)
    }else{
      competition_display_calc <- sum(isolate(SP_reactive_input$competition_display[SP_reactive_input$competition_display$FORMAT == SP_reactive_input$opti_format_input_tbo,])$Display_Cost_Comp)/10^6
      competition_KPI <- isolate(SP_reactive_input$competition_KPI[SP_reactive_input$competition_KPI$FORMAT == SP_reactive_input$opti_format_input_tbo,])
    }
    competition_KPI_calc <- data.frame("KPI" = c("RSV","COGS","CPD","fixed","FM","FM%","BM","BM%"), "comp_value" = c(sum(competition_KPI$Retailer_Revenue_Comp)/10^6, sum(competition_KPI$Net_Cost_Unit_Comp*competition_KPI$Units)/10^6,sum(competition_KPI$Retro_Fund_Total_Comp)/10^6,competition_display_calc/10^6,sum(competition_KPI$FM_Abs_Comp)/10^6,
                                                                                                                     sum(competition_KPI$FM_Abs_Comp)/sum(competition_KPI$Retailer_Revenue_Comp/(1+competition_KPI$VAT)),(sum(competition_KPI$FM_Abs_Comp) + competition_display_calc)/10^6,(sum(competition_KPI$FM_Abs_Comp) + competition_display_calc)/sum(competition_KPI$Retailer_Revenue_Comp/(1+competition_KPI$VAT))))
    
    SP_opti_op_KPI_data <- melt(SP_reactive_input$KPI_calculation_ret_tbo, id = c("goal","sign"))
    SP_opti_op_KPI_data$KPI_Value <- sapply(SP_opti_op_KPI_data$variable,function(X) str_split(X,"_")[[1]][[length(str_split(X,"_")[[1]])]])
    KPI_replace <- paste(unlist(paste0("_",SP_opti_op_KPI_data$KPI_Value)), collapse = "|")
    SP_opti_op_KPI_data$variable <- sapply(SP_opti_op_KPI_data$variable, function(X) gsub(KPI_replace,"",X))
    SP_opti_op_KPI_data <- data.table::dcast(SP_opti_op_KPI_data,goal + sign + variable ~ KPI_Value, value.var= c("value"))
    
    SP_reactive_input$kpi_map_calc_ret$KPI_calc <- as.character(isolate(SP_reactive_input$kpi_map_calc_ret)$KPI_calc)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,isolate(SP_reactive_input$kpi_map_calc_ret),by = c("variable" = "KPI_calc"))
    
    competition_KPI_calc$KPI <- as.character(competition_KPI_calc$KPI)
    SP_opti_op_KPI_data <- left_join(SP_opti_op_KPI_data,competition_KPI_calc,by = c("variable" = "KPI"))
    
    SP_opti_op_KPI_data$old <- as.numeric(SP_opti_op_KPI_data$old)
    SP_opti_op_KPI_data$new <- as.numeric(SP_opti_op_KPI_data$new)
    
    SP_opti_op_KPI_data$old <- SP_opti_op_KPI_data$old + SP_opti_op_KPI_data$comp_value
    SP_opti_op_KPI_data$new <- SP_opti_op_KPI_data$new + SP_opti_op_KPI_data$comp_value
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "BM%",c("old","new")] <- SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "BM",c("old","new")] * 100/(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "RSV",c("old","new")]/1.2)
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "FM%",c("old","new")] <- SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "FM",c("old","new")] * 100/(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable == "RSV",c("old","new")]/1.2)
    
    SP_opti_op_KPI_data <- SP_opti_op_KPI_data[,c("KPI_opti","old","new","const_order","variable")]
    
    names(SP_opti_op_KPI_data) <- c("KPI","Old Forecast","New Forecast","const_order","variable")
    SP_opti_op_KPI_data[,c("Old Forecast","New Forecast")] <- lapply(SP_opti_op_KPI_data[,c("Old Forecast","New Forecast")],function(X) as.numeric(X))
    
    SP_opti_op_KPI_data$`Ongoing vs Finalized Plan` <- (SP_opti_op_KPI_data$`New Forecast` - SP_opti_op_KPI_data$`Old Forecast`) * 100/SP_opti_op_KPI_data$`Old Forecast`
    
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Ongoing vs Finalized Plan` <- (SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`New Forecast` - SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Old Forecast`) * 100
    
    SP_opti_op_KPI_data <- setDT(SP_opti_op_KPI_data)[order(const_order),]
    SP_opti_op_KPI_data$`sign` <- ifelse(SP_opti_op_KPI_data$`Ongoing vs Finalized Plan` >= 0, "Positive", "Negative")
    
    SP_opti_op_KPI_data$`Ongoing vs Finalized Plan` <- as.character(round(SP_opti_op_KPI_data$`Ongoing vs Finalized Plan`,2))
    
    SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Ongoing vs Finalized Plan` <- paste0(SP_opti_op_KPI_data[!(SP_opti_op_KPI_data$variable %in% c("BM%","FM%")),]$`Ongoing vs Finalized Plan`,"%")
    SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Ongoing vs Finalized Plan` <- paste0(SP_opti_op_KPI_data[SP_opti_op_KPI_data$variable %in% c("BM%","FM%"),]$`Ongoing vs Finalized Plan`,"bps")
    
    SP_opti_op_KPI <- SP_opti_op_KPI_data[,!(names(SP_opti_op_KPI_data) %in% c("const_order","variable")),with = FALSE]
    
    datatable(SP_opti_op_KPI,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(visible=FALSE, targets=c(4)),list(className = 'dt-center', targets = 0:3)),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = FALSE,
                scrollY = FALSE,
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      #"#FFD966", "#A9D08E", "#F4B084", "#8EA9DB", "#D2B4DE", "#BFBFBF"
      formatStyle(names(SP_opti_op_KPI),textAlign = 'center') %>%
      # formatStyle("LSM Planned", backgroundColor = "#BED661") %>%
      # formatStyle("Unconstrained Planned", backgroundColor = "#89E894") %>%
      # formatStyle("Last Year Value", backgroundColor = "#BFBFBF") %>%
      # formatStyle("Min Constraint", backgroundColor = "#34dddd") %>%
      # formatStyle("Max Constraint", backgroundColor = "#93e2d5") %>%
      #formatStyle("LSM Change vs LY", backgroundColor = "#34dddd") %>%
      #formatStyle("Unconstrained Change vs LY", backgroundColor = "#93e2d5") %>%
      formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle("Ongoing vs Finalized Plan","sign",backgroundColor = styleEqual(c("Positive","Negative"),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_opti_op_KPI,is.numeric)),digits = 2)
  })
  
  output$SP_graph_1_final <- renderPlotly({
    
    validate(
      need(SP_reactive_input$SP_graph_1_ip_final,"Run Ongoing Optimization")
    )
    
    plot_ly(SP_reactive_input$SP_graph_1_ip_final,x=~Week_Ending,y=~Promo_Price,name = "Promo Price",type = "scatter", mode = "lines",
            hovertext = round(SP_reactive_input$SP_graph_1_ip_final$Total_Sales,2),hoverinfo = 'text') %>%
      add_trace(y=~Total_Sales,name = "Total Sales",type = "scatter", mode = "lines",
                hovertext = round(SP_reactive_input$SP_graph_1_ip_final$Promo_Price,2),hoverinfo = 'text',yaxis = 'y2') %>%
      layout(margin=list(r=50),xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(side = 'left',title = "Promo Price", zeroline = FALSE,showgrid = FALSE),yaxis2 = list(side = 'right', overlaying = "y",title = "Volume Sales(Units)", zeroline = FALSE,showgrid = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
  })
  
  output$SP_graph_2_final <- renderPlotly({
    validate(
      need(SP_reactive_input$SP_graph_1_ip_final,"Run Ongoing Optimization")
    )
    #
    #SP_reactive_input$SP_graph_1_lsm_ip[SP_reactive_input$SP_graph_1_lsm_ip$Event == "No Promo"]$Event <- ""
    plot_ly(SP_reactive_input$SP_graph_1_ip_final,x=~Week_Ending,y=~TPR_Flag,color=~Event,type='bar') %>%
      layout(xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(title = "", zeroline = FALSE,showgrid = FALSE),legend = list(orientation = 'h',x = 0, y = 50,trace = TRUE)) %>% config(displayModeBar = FALSE)
  })
  
  output$SP_graph_1_tbo <- renderPlotly({
    
    validate(
      need(SP_reactive_input$SP_graph_1_ip_tbo,"Run Ongoing Optimization")
    )
    
    plot_ly(SP_reactive_input$SP_graph_1_ip_tbo,x=~Week_Ending,y=~Promo_Price,name = "Promo Price",type = "scatter", mode = "lines",
            hovertext = round(SP_reactive_input$SP_graph_1_ip_tbo$Total_Sales,2),hoverinfo = 'text') %>%
      add_trace(y=~Total_Sales,name = "Total Sales",type = "scatter", mode = "lines",
                hovertext = round(SP_reactive_input$SP_graph_1_ip_tbo$Promo_Price,2),hoverinfo = 'text',yaxis = 'y2') %>%
      layout(margin=list(r=50),xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(side = 'left',title = "Promo Price", zeroline = FALSE,showgrid = FALSE),yaxis2 = list(side = 'right', overlaying = "y",title = "Volume Sales(Units)", zeroline = FALSE,showgrid = FALSE),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
  })
  
  output$SP_graph_2_tbo <- renderPlotly({
    validate(
      need(SP_reactive_input$SP_graph_1_ip_tbo,"Run Ongoing Optimization")
    )
    #
    #SP_reactive_input$SP_graph_1_lsm_ip[SP_reactive_input$SP_graph_1_lsm_ip$Event == "No Promo"]$Event <- ""
    plot_ly(SP_reactive_input$SP_graph_1_ip_tbo,x=~Week_Ending,y=~TPR_Flag,color=~Event,type='bar') %>%
      layout(xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(title = "", zeroline = FALSE,showgrid = FALSE),legend = list(orientation = 'h',x = 0, y = 50,trace = TRUE)) %>% config(displayModeBar = FALSE)
  })
  
  output$SP_graph_3_tbo <- renderDataTable({
    validate(
      need(SP_reactive_input$SP_test2_final_delist_exclude,"Run Optimization")
    )
    
    empty_df <- data.frame(matrix(rep("",nrow(SP_reactive_input$SP_test2_final_delist_exclude)),ncol = 1))
    names(empty_df) <- ""
    
    SP_test2_final_delist_exclude <- cbind(SP_reactive_input$SP_test2_final_delist_exclude,empty_df,SP_reactive_input$SP_test2_tbo_op[,c(2:length(SP_reactive_input$SP_test2_tbo_op))],
                                           empty_df,SP_reactive_input$SP_test2_tbo_diff_calc[,c(2:length(SP_reactive_input$SP_test2_tbo_diff_calc))])
    
    sketch = htmltools::withTags(table(
      class = 'cell-border compact hover',
      thead(
        tr(
          th(colspan = 1, 'Product Level'),
          th(colspan = 8, 'Finalized Plan'),
          th(colspan = 8, 'Optimized(Ongoing)'),
          th(colspan = 8, 'Optimized(Ongoing) vs Finalized Plan')
        ),
        tr(
          lapply(
            c(names(SP_test2_final_delist_exclude)[1],"Scan Net Revenue('000 GBP)","Volume Sales('000 Units)","Gross Sales('000 GBP)","Gross Margin('000 GBP)","Trade Investment('000 GBP)","GM % NR","TI % NR","Break",
              "Scan Net Revenue('000 GBP)","Volume Sales('000 Units)","Gross Sales('000 GBP)","Gross Margin('000 GBP)","Trade Investment('000 GBP)","GM % NR","TI % NR","Break",
              "Scan Net Revenue(%)","Volume Sales(%)","Gross Sales(%)","Gross Margin(%)","Trade Investment(%)","GM % NR(bps)","TI % NR(bps)"),
            th
          )
        )
      )
    ))
    
    datatable(SP_test2_final_delist_exclude,container = sketch,class="cell-border compact hover",
              options = list(
                columnDefs=list(list(className = 'dt-center', targets = 0:(length(SP_test2_final_delist_exclude)-1)),list(width = '200px',targets=c(0))),
                paging = FALSE,
                searching = FALSE,
                dom = 't',
                ordering = F,
                scrollX = TRUE,
                scrollY = "260px",
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                  "}")
              ),rownames = F) %>%
      formatStyle(names(SP_test2_final_delist_exclude),textAlign = 'center') %>%
      formatStyle(1, backgroundColor = "#0099cc", color = "#FFFFFF") %>%
      formatStyle(c("Scan Net Revenue(% Diff)","Volume Sales(% Diff)","Gross Sales % Diff","Gross Margin(% Diff)","GM % NR(% Diff)"),
                  background = styleInterval(c(0),c("#ff4343","#84c343"))) %>%
      formatStyle(c("Trade Investment(% Diff)","TI % NR(% Diff)"),
                  background = styleInterval(c(0),c("#84c343","#ff4343"))) %>%
      formatRound(names(select_if(SP_test2_final_delist_exclude,is.numeric)),digits = 2)
  })
  
  ####Ongoing Optimized Calendar
  observeEvent(input$SP_opti_cal_format_selection_tbo,{
    toggle("SP_opti_cal_legend_tbo",condition = (input$SP_opti_cal_format_selection_tbo != "ROI Effectiveness"))
    
    output$SP_opti_cal_tbo <- renderDataTable({
      validate(
        need(SP_reactive_input$opti_cal_dcast_tbo,"")
      )
      #SP_reactive_input$cal_selection <- input$SP_opti_cal_LSM_selection
      clrs_ROI <- c(round(seq(255,132, length.out = (length(SP_reactive_input$brks_ROI_tbo) + 1)), 0) %>% {paste0("rgb(", ., ",192,67)")})
      sketch = htmltools::withTags(table(
        class = 'cell-border compact hover',
        thead(
          tr(
            # th(colspan = 5, 'Month'),
            # th(colspan = 5, 'January'),
            # th(colspan = 4, 'February'),
            # th(colspan = 5, 'March'),
            # th(colspan = 4, 'April'),
            # th(colspan = 4, 'May'),
            # th(colspan = 5, 'June'),
            # th(colspan = 4, 'July'),
            # th(colspan = 4, 'August'),
            # th(colspan = 5, 'September'),
            # th(colspan = 4, 'October'),
            # th(colspan = 4, 'November'),
            # th(colspan = 5, 'December')
            
            HTML(promo_cal_structure_output(SP_reactive_input$opti_cal_filtered_tbo)[[1]])
          ),
          tr(
            lapply(
              c("Format","PPG","PPG Description","RSP","LSM Floor Price",c(1:length(unique(SP_reactive_input$opti_cal_filtered_tbo$Week_Ending)))),
              th
            )
          )
        )
      ))
      
      if(input$SP_opti_cal_format_selection_tbo == "Promotion Type"){
        datatable(SP_reactive_input$opti_cal_dcast_tbo,container=sketch,class="cell-border compact hover",extensions = c('FixedColumns','Buttons'),selection =list(target='cell',mode="single"),
                  #selection=list(mode="single", target="cell"),
                  options = list(
                    order = list(0, 'asc'),
                    buttons = list('copy', list(extend = 'csv',filename = 'Optimized Calendar (Ongoing)'),
                                   list(extend = 'excel', filename = 'Optimized Calendar (Ongoing)')),
                    # list(extend = 'pdf',
                    #      pageSize = 'A4',
                    #      orientation = 'landscape',
                    #      filename = 'Optimized Calendar (Ongoing)'), 'print'),
                    columnDefs=list(list(visible=FALSE, targets=grep("Event|ROI",names(SP_reactive_input$opti_cal_dcast_tbo))-1),list(width = '70px',targets=c(0,3)), list(width = '30px',targets=c(1,2,4)),list(width = '10px',targets=sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)) + 4)),
                    fixedColumns = list(leftColumns = 5),
                    paging = FALSE,
                    searching = FALSE,
                    dom = 't',
                    bSort=FALSE,
                    scrollX = TRUE,
                    scrollY = "400px"
                  ),rownames = F) %>% 
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))),paste0("Event_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`))),
                      background = styleEqual(c("No Promo","Shelf Promotion","Display Feature Promotion","Feature","Display Feature"), 
                                              c('white','#0099DC', '#E42E92','#90EE90','#FFDAB9')),
                      color = styleEqual(c("No Promo","Shelf Promotion","Display Feature Promotion","Feature","Display Feature"), 
                                         c('white','white', 'white','white','white'))) %>%
          formatStyle(0,target = 'row',lineHeight='85%') %>%
          formatRound(as.character(c("RSP_Unit",1:length(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))), digits=2) %>%
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))),cursor = 'pointer')
      }else if(input$SP_opti_cal_format_selection_tbo == "ROI Effectiveness"){
        datatable(SP_reactive_input$opti_cal_dcast_ROI_tbo,container=sketch,class="cell-border compact hover",extensions = c('FixedColumns','Buttons'),selection =list(target='cell',mode="single"),
                  #selection=list(mode="single", target="cell"),
                  options = list(
                    order = list(0, 'asc'), 
                    buttons = list('copy', list(extend = 'csv',filename = 'Optimized Unconstrained Calendar ROI'),
                                   list(extend = 'excel', filename = 'Optimized Unconstrained Calendar ROI')),
                    # list(extend = 'pdf',
                    #      pageSize = 'A4',
                    #      orientation = 'landscape',
                    #      filename = 'Optimized Unconstrained Calendar ROI'), 'print'),
                    columnDefs=list(list(visible=FALSE, targets=grep("Event_|Price_",names(SP_reactive_input$opti_cal_dcast_ROI_tbo))-1),list(width = '70px',targets=c(0,3)), list(width = '30px',targets=c(1,2,4)),list(width = '10px',targets=sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`))+4)),
                    fixedColumns = list(leftColumns = 5),
                    paging = FALSE,
                    searching = FALSE,
                    dom = 't',
                    bSort=FALSE,
                    scrollX = TRUE,
                    scrollY = "400px"
                  ),rownames = F) %>% 
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))),
                      background = styleInterval(c(-99,SP_reactive_input$brks_ROI_tbo),c("white","red",clrs_ROI[-1]))) %>%
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))),
                      color = styleEqual(-100,c("white"))) %>%
          formatStyle(0,target = 'row',lineHeight='85%') %>%
          formatRound(as.character(c("RSP_Unit",1:length(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))), digits=2) %>%
          formatStyle(as.character(c(1:length(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))),cursor = 'pointer')
        
      }
    })
  })
  
  ###Excluded PPG list
  output$SP_opti_financial_table_on <- renderDataTable({
    
    datatable(SP_reactive_input$SP_RB_Financial_PPG_1_tbo,class="cell-border stripe",extensions = c('FixedColumns'),
              options = list(
                lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
                buttons = list('copy', list(extend = 'csv',filename = 'Out of Scope Products(Ongoing)'),
                               list(extend = 'excel', filename = 'Out of Scope Products(Ongoing)'),
                               list(extend = 'pdf',
                                    pageSize = 'A4',
                                    orientation = 'landscape',
                                    filename = 'Out of Scope Products(Ongoing)'), 'print'),
                pageLength = 10,
                dom = "Blftrip",
                scrollX = T,
                scrollY = "300px"
              ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_RB_Financial_PPG_1_tbo),textAlign = 'center')
  })
  
  output$SP_opti_financial_download_on <- downloadHandler(
    filename = function() {
      paste("Excluded_PPG_TBO.csv")
    },
    content = function(file) {
      write.csv(SP_reactive_input$SP_RB_Financial_PPG_1_tbo, file,row.names = FALSE)
    }
  )
  
  ##Saving ongoing optimizer
  observeEvent(input$SP_opti_save_tbo,{
    ###Saving TPO ID table
    if(input$SP_opti_save_name_tbo != ""){
      if(file.exists(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Ongoing/",input$SP_opti_save_name_tbo,".RData"))){
        confirmSweetAlert(session,"confirm_save_tbo",title = paste0(input$SP_opti_save_name_tbo," already exists in the Saved Plans"),text = "Confirm replacing the saved plan!!",type = "warning")
      }else{
        confirmSweetAlert(session,"confirm_save_tbo",title = paste0("Saving optimizer ",input$SP_opti_save_name_tbo),text = "Confirm Saving optimizer plan!!",type = "success")
      }
    }else{
      sendSweetAlert(session,"Error!!","Optimizer name cannot be blank",type = "error")
    }
  })
  
  observeEvent(input$confirm_save_tbo,{
    if(input$confirm_save_tbo){
      
      if(SP_reactive_input$simulated_flag_tbo == 1){
        SP_reactive_input$save_counter_sim_tbo <- SP_reactive_input$save_counter_sim_tbo + 1
      }else if(SP_reactive_input$simulated_flag_tbo == 0){
        SP_reactive_input$save_counter_tbo <- SP_reactive_input$save_counter_tbo + 1
      }
      
      SP_reactive_input$SP_TPO_list_tbo$opti_output <- SP_reactive_input$SP_opti_op_prepared_tbo
      SP_reactive_input$SP_TPO_list_tbo$simulated_flag_tbo <- SP_reactive_input$simulated_flag_tbo
      SP_reactive_input$SP_TPO_list_tbo$excluded_brand <- SP_reactive_input$SP_opti_exc_brand_tbo
      
      # if(input$SP_opti_format_on != ""){
      #   SP_reactive_input$TPO_Name <- paste0(SP_reactive_input$SP_brand_short,"_",SP_reactive_input$format_mapping[SP_reactive_input$format_mapping$Format_Full == input$SP_opti_format_on,]$Format_Short,"_",year(input$SP_opti_date_start),"_",ifelse(SP_reactive_input$SP_TPO_list_tbo$simulated_flag_tbo == 1,paste0("tbo_",SP_reactive_input$save_counter_tbo,"sim_",SP_reactive_input$save_counter_sim_tbo),paste0("tbo_",SP_reactive_input$save_counter_tbo)))
      # }else{
      #   SP_reactive_input$TPO_Name <- paste0(SP_reactive_input$SP_brand_short,"_",year(input$SP_opti_date_start),"_",ifelse(SP_reactive_input$SP_TPO_list_tbo$simulated_flag_tbo == 1,paste0("tbo_",SP_reactive_input$save_counter_tbo,"_sim_",SP_reactive_input$save_counter_sim_tbo),paste0("tbo_",SP_reactive_input$save_counter_tbo)))
      # }
      SP_reactive_input$TPO_Name <- input$SP_opti_save_name_tbo
      SP_reactive_input[[SP_reactive_input$TPO_Name]] <- SP_reactive_input$SP_TPO_list_tbo
      
      SP_reactive_input$SP_saved_tpo_latest <- data.frame("TPO ID" = SP_reactive_input$TPO_Name,"Customer" = input$SP_opti_cust,"LSM Constrained/Unconstrained" = SP_reactive_input$ongoing_lsm_nonlsm, "Optimization Objective" = paste0(input$SP_opti_goal,"(",input$SP_opti_sign,")"),check.names = FALSE)
      
      SP_reactive_input$SP_cmp_scn_ip_latest_tbo <- data.frame("TPO ID" = SP_reactive_input$TPO_Name,"Country" = SP_reactive_input$SP_TPO_list_tbo$coun,"Customer" = SP_reactive_input$SP_TPO_list_tbo$cust,"Category" = SP_reactive_input$SP_TPO_list_tbo$cat,
                                                               "Brand" = SP_reactive_input$SP_TPO_list_tbo$brand,"Format" = SP_reactive_input$SP_TPO_list_tbo$format,"PPG" = SP_reactive_input$SP_TPO_list_tbo$ppg,"Goal" = SP_reactive_input$SP_TPO_list_tbo$goal_shiny,
                                                               "Goal Objective" = SP_reactive_input$SP_TPO_list_tbo$sign, "LSM Constrained/Unconstrained" = SP_reactive_input$ongoing_lsm_nonlsm,
                                                               "Scan Net Revenue" =  sum(SP_reactive_input$SP_opti_op_prepared_tbo$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$Net_Revenue),
                                                               "GM % NR" = (sum(SP_reactive_input$SP_opti_op_prepared_tbo$GM_Abs) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$GM_Abs))* 100/(sum(SP_reactive_input$SP_opti_op_prepared_tbo$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$Net_Revenue)),
                                                               "TI % NR" = (sum(SP_reactive_input$SP_opti_op_prepared_tbo$Total_Trade_Investment) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$Trade_Investment)) * 100/(sum(SP_reactive_input$SP_opti_op_prepared_tbo$Net_Revenue) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$Net_Revenue)),
                                                               "Gross Sales" = (sum(SP_reactive_input$SP_opti_op_prepared_tbo$Gross_Sales) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$Gross_Sales)),
                                                               #"Trade ROI" = (sum(SP_reactive_input$SP_opti_op_prepared_tbo$Inc_GM_Abs) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$Inc_GM_Abs))/(sum(SP_reactive_input$SP_opti_op_prepared_tbo$Total_Trade_Investment) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$Trade_Investment)),
                                                               "Volume Sales" = (sum(SP_reactive_input$SP_opti_op_prepared_tbo$Total_Sales) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$Units)),check.names = FALSE)
      
      if(input$SP_opti_ROI_selection == "Incremental GM ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest_tbo$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared_tbo$R_GM_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$R_GM_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared_tbo$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$R_Trade_Inv_Inc))
      }else if(input$SP_opti_ROI_selection == "Incremental NR ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest_tbo$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared_tbo$R_Net_Rev_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$R_Net_Rev_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared_tbo$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$R_Trade_Inv_Inc))
      }else if(input$SP_opti_ROI_selection == "Incremental NIS ROI"){
        SP_reactive_input$SP_cmp_scn_ip_latest_tbo$`Trade ROI` <- (sum(SP_reactive_input$SP_opti_op_prepared_tbo$R_NIS_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$R_NIS_Inc))/(sum(SP_reactive_input$SP_opti_op_prepared_tbo$R_Trade_Inv_Inc) + sum(SP_reactive_input$SP_opti_exc_brand_tbo$R_Trade_Inv_Inc))
      }
      
      if(nrow(unique(rbind(SP_reactive_input$SP_cmp_scn_ip,SP_reactive_input$SP_cmp_scn_ip_latest_tbo)[,!(names(SP_reactive_input$SP_cmp_scn_ip) %in% "TPO ID")])) > nrow(SP_reactive_input$SP_cmp_scn_ip)){
        SP_reactive_input$SP_saved_tpo <- rbind(SP_reactive_input$SP_saved_tpo,SP_reactive_input$SP_saved_tpo_latest)
        SP_reactive_input$SP_cmp_scn_ip <- rbind(SP_reactive_input$SP_cmp_scn_ip,SP_reactive_input$SP_cmp_scn_ip_latest_tbo)
        SP_reactive_input$SP_TPO_list_tbo$Compare_Scenario <- SP_reactive_input$SP_cmp_scn_ip_latest_tbo
        tpo_list_temp <- SP_reactive_input$SP_TPO_list_tbo
        save(tpo_list_temp,file = paste0(SP_reactive_input$folder_path,"/Saved Optimizers Ongoing/",SP_reactive_input$TPO_Name,".RData"))
      }else{
        sendSweetAlert(session,"Trying to save an existing plan!!","Optimizer Plan already exists in Saved plans, check Saved Optimizer Plans",type = "warning")
      }
      
      output$SP_opti_saved_tpo <- renderDataTable({
        validate(
          need(SP_reactive_input$SP_saved_tpo,"Save an Optimization Plan")
        )
        #
        datatable(SP_reactive_input$SP_saved_tpo,class="cell-border stripe",extensions = c('FixedColumns'),selection =list(target='cell',mode="single"),
                  options = list(
                    lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
                    pageLength = 10,
                    dom = "Blftrip",
                    # searching = FALSE,
                    scrollX = T,
                    scrollY = "150px"
                  ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_saved_tpo),textAlign = 'center')%>%
          formatStyle(as.character("TPO ID"),cursor = 'pointer')
      })
    }
  })
  
  observeEvent(input$SP_opti_cal_tbo_cell_clicked,{
    
    if(length(input$SP_opti_cal_tbo_cell_clicked) != 0){
      
      updateSelectizeInput(session,"SP_sim_coun_on",choices = input$SP_opti_op_coun_on,selected = input$SP_opti_op_coun_on)
      updateSelectizeInput(session,"SP_sim_cust_on",choices = input$SP_opti_op_cust_on,selected = input$SP_opti_op_cust_on)
      updateSelectizeInput(session,"SP_sim_cat_on",choices = input$SP_opti_op_cat_on,selected = input$SP_opti_op_cat_on)
      updateSelectizeInput(session,"SP_sim_brand_on",choices = input$SP_opti_op_brand_on,selected = input$SP_opti_op_brand_on)
      
      SP_reactive_input$SP_opti_op_grouped_event_tbo <- SP_reactive_input$SP_opti_op_prepared_tbo[,.("TPR_Flag" = unique(TPR_Flag),"Display_Flag"=unique(Display_Flag),"Display"= unique(Display),"Promo Price"=mean(Promo_Price),"RSP" = mean(RSP_Unit),"Net Sales" = sum(Total_Sales),
                                                                                                     "ROI" = ifelse(input$SP_opti_ROI_selection_on == "Incremental GM ROI",sum(R_GM_Inc)/sum(R_Trade_Inv_Inc),ifelse(input$SP_opti_ROI_selection_on == "Incremental NR ROI",sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc),sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc)))),by = .(PPG,PPG_Description,Tesco_Week_No)]
      SP_reactive_input$SP_opti_op_grouped_event_tbo[SP_reactive_input$SP_opti_op_grouped_event_tbo$TPR_Flag != 1,]$ROI <- 0
      SP_reactive_input$SP_opti_op_grouped_event_tbo$`Promo Slots` <- paste0("Event-",SP_reactive_input$SP_opti_op_grouped_event_tbo$Tesco_Week_No)
      
      SP_reactive_input$SP_opti_op_tbo_clicked_ppg <- SP_reactive_input$opti_cal_dcast_tbo[input$SP_opti_cal_tbo_cell_clicked$row,]$PPG
      SP_reactive_input$SP_opti_op_tbo_clicked_event <- SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$PPG == SP_reactive_input$SP_opti_op_tbo_clicked_ppg & SP_reactive_input$opti_cal_filtered_tbo$`Week No` == (input$SP_opti_cal_tbo_cell_clicked$col - 3),]$Tesco_Week_No
      
      updateSelectizeInput(session,"SP_sim_format_on",choices = SP_reactive_input$opti_cal_dcast_tbo[input$SP_opti_cal_tbo_cell_clicked$row,]$Format,selected = SP_reactive_input$opti_cal_dcast_tbo[input$SP_opti_cal_tbo_cell_clicked$row,]$Format)
      updateSelectizeInput(session,"SP_sim_ppg_on",choices = SP_reactive_input$SP_opti_op_tbo_clicked_ppg,selected = SP_reactive_input$SP_opti_op_tbo_clicked_ppg)
      updateSelectizeInput(session,"SP_sim_event_on",choices = paste0("Event-",SP_reactive_input$SP_opti_op_tbo_clicked_event),selected = paste0("Event-",SP_reactive_input$SP_opti_op_tbo_clicked_event))
      
      SP_reactive_input$SP_opti_op_tbo_clicked_event_table <- SP_reactive_input$SP_opti_op_grouped_event_tbo[SP_reactive_input$SP_opti_op_grouped_event_tbo$PPG ==SP_reactive_input$SP_opti_op_tbo_clicked_ppg & SP_reactive_input$SP_opti_op_grouped_event_tbo$Tesco_Week_No ==SP_reactive_input$SP_opti_op_tbo_clicked_event,]
      SP_reactive_input$SP_opti_op_tbo_clicked_event_table$`lsm_nonlsm` <- SP_reactive_input$ongoing_lsm_nonlsm
      SP_reactive_input$SP_opti_op_tbo_clicked_event_table <- SP_reactive_input$SP_opti_op_tbo_clicked_event_table[,c("PPG","PPG_Description","RSP","Promo Slots","Display","Promo Price","ROI")]
      names(SP_reactive_input$SP_opti_op_tbo_clicked_event_table) <- c("PPG","PPG_Description","RSP","Promo Slots","Display Type","Promoted Price",input$SP_opti_ROI_selection_on)
      
      SP_reactive_input$SP_opti_op_tbo_clicked_slot_display <- SP_reactive_input$SP_opti_op_grouped_event_tbo[SP_reactive_input$SP_opti_op_grouped_event_tbo$Display_Flag == 1 & SP_reactive_input$SP_opti_op_grouped_event_tbo$Tesco_Week_No ==SP_reactive_input$SP_opti_op_tbo_clicked_event,]
      SP_reactive_input$SP_opti_op_tbo_clicked_slot_display$`lsm_nonlsm` <- SP_reactive_input$ongoing_lsm_nonlsm
      SP_reactive_input$SP_opti_op_tbo_clicked_slot_display <- SP_reactive_input$SP_opti_op_tbo_clicked_slot_display[,c("PPG","PPG_Description","RSP","Promo Slots","Display","Promo Price","ROI")]
      names(SP_reactive_input$SP_opti_op_tbo_clicked_slot_display) <- c("PPG","PPG_Description","RSP","Promo Slots","Display Type","Promoted Price",input$SP_opti_ROI_selection_on)
      
      observeEvent(c(input$SP_sim_reset_on),{
        # SP_reactive_input$SP_opti_op_tbo_event_table <- SP_reactive_input$SP_opti_op_grouped_event_tbo[SP_reactive_input$SP_opti_op_grouped_event_tbo$PPG ==SP_reactive_input$SP_opti_op_tbo_clicked_ppg & SP_reactive_input$SP_opti_op_grouped_event_tbo$TPR_Flag == 1& SP_reactive_input$SP_opti_op_grouped_event_tbo$`Event ID` != paste0("Event-",SP_reactive_input$SP_opti_op_tbo_clicked_event),]
        # SP_reactive_input$SP_opti_op_tbo_event_table$`lsm_nonlsm` <- "LSM"
        # SP_reactive_input$SP_opti_op_tbo_event_table <- cbind(SP_reactive_input$SP_opti_op_tbo_event_table[,c("PPG","lsm_nonlsm","Event ID","Display","Promo Price","ROI")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_tbo_event_table))))
        # SP_reactive_input$SP_opti_op_tbo_event_table <- rbind(SP_reactive_input$SP_opti_op_tbo_event_table,data.frame("PPG" = unique(SP_reactive_input$SP_opti_op_tbo_event_table$PPG),"lsm_nonlsm" = "LSM","Event ID" = "No Event", "Display" = "", "Promo Price" = 0,"ROI" = 0,"Select" = FALSE,check.names = FALSE))
        SP_reactive_input$SP_opti_op_tbo_event_table <- SP_reactive_input$shiny_ip_events_final[SP_reactive_input$shiny_ip_events_final$PPG==SP_reactive_input$SP_opti_op_tbo_clicked_ppg,]
        SP_reactive_input$SP_opti_op_tbo_event_table$`lsm_nonlsm` <- SP_reactive_input$ongoing_lsm_nonlsm
        
        SP_reactive_input$SP_opti_op_tbo_event_table$`Event ID` <- paste0("Event Substitute-",c(1:nrow(SP_reactive_input$SP_opti_op_tbo_event_table)))
        
        if(input$SP_opti_ROI_selection_on == "Incremental GM ROI"){
          SP_reactive_input$SP_opti_op_tbo_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_tbo_event_table[,c("PPG","PPG_Description","RSP_Unit","Event ID","Display","Promo_Price","ROI_GM")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_tbo_event_table))))
          names(SP_reactive_input$SP_opti_op_tbo_event_table_1) <- c("PPG","PPG_Description","RSP","Event ID","Display","Promo_Price","ROI","Select")
          
        }else if(input$SP_opti_ROI_selection_on == "Incremental NR ROI"){
          
          SP_reactive_input$SP_opti_op_tbo_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_tbo_event_table[,c("PPG","PPG_Description","RSP_Unit","Event ID","Display","Promo_Price","ROI_Rev")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_tbo_event_table))))
          names(SP_reactive_input$SP_opti_op_tbo_event_table_1) <- c("PPG","PPG_Description","RSP","Event ID","Display","Promo_Price","ROI","Select")
          
        }else if(input$SP_opti_ROI_selection_on == "Incremental NIS ROI"){
          SP_reactive_input$SP_opti_op_tbo_event_table_1 <- cbind(SP_reactive_input$SP_opti_op_tbo_event_table[,c("PPG","PPG_Description","RSP_Unit","Event ID","Display","Promo_Price","ROI_NIS")],data.frame("Select" = rep(FALSE,nrow(SP_reactive_input$SP_opti_op_tbo_event_table))))
          names(SP_reactive_input$SP_opti_op_tbo_event_table_1) <- c("PPG","PPG_Description","RSP","Event ID","Display","Promo_Price","ROI","Select")
        }
        
        
        SP_reactive_input$SP_opti_op_tbo_event_table_1 <- rbind(SP_reactive_input$SP_opti_op_tbo_event_table_1,data.frame("PPG" = unique(SP_reactive_input$SP_opti_op_tbo_event_table_1$PPG),"PPG_Description" = unique(SP_reactive_input$SP_opti_op_tbo_event_table_1$PPG_Description),"RSP" = unique(SP_reactive_input$SP_opti_op_tbo_event_table_1$RSP),"Event ID" = "No Event", "Display" = "", "Promo_Price" = 0,"ROI" = 0,"Select" = FALSE,check.names = FALSE))
        #SP_reactive_input$SP_opti_op_tbo_event_table$`Promoted Price` <- as.numeric(SP_reactive_input$SP_opti_op_tbo_event_table$`Promoted Price`)
        SP_reactive_input$SP_opti_op_tbo_event_table_1 <- SP_reactive_input$SP_opti_op_tbo_event_table_1[order(-ROI),]
        names(SP_reactive_input$SP_opti_op_tbo_event_table_1) <- c("PPG","PPG Description","RSP","Event ID","Display Type","Promoted Price",input$SP_opti_ROI_selection_on,"Select")
        
        #SP_reactive_input$SP_opti_op_tbo_event_table_1 <- SP_reactive_input$SP_opti_op_tbo_event_table_1[,!(names(SP_reactive_input$SP_opti_op_tbo_event_table_1) %in% "LSM/Non LSM"),with = FALSE]
        
      })
      SP_reactive_input$simulator_op_tbo_summary_event_table <- NULL
      SP_reactive_input$simulator_op_tbo_summary_ppg <- NULL
      shinyjs::hide("scenario_analysis_on")
      updateTabItems(session, "sidebar_main", selected = "SP_subMenu12")
    }
    
    output$SP_sim_event_selected_table_on <- renderDataTable({
      datatable(SP_reactive_input$SP_opti_op_tbo_clicked_event_table,class="cell-border stripe",extensions = c('FixedColumns'),
                options = list(
                  # paging = FALSE,
                  dom = "t",
                  # searching = FALSE,
                  scrollX = F,
                  scrollY = F
                ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_opti_op_tbo_clicked_event_table),textAlign = 'center')%>%
        formatRound(c("Promoted Price","RSP",input$SP_opti_ROI_selection_on),digits = 2)
    })
    output$SP_sim_event_slot_display_table_on <- renderDataTable({
      datatable(SP_reactive_input$SP_opti_op_tbo_clicked_slot_display,class="cell-border stripe",extensions = c('FixedColumns'),
                options = list(
                  # paging = FALSE,
                  dom = "t",
                  # searching = FALSE,
                  scrollX = T,
                  scrollY = "150px"
                ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_opti_op_tbo_clicked_slot_display),textAlign = 'center')%>%
        formatRound(c("Promoted Price","RSP",input$SP_opti_ROI_selection_on),digits = 2)
    })
    #
    output$SP_sim_event_table_on <- renderRHandsontable({
      rhandsontable(SP_reactive_input$SP_opti_op_tbo_event_table_1,rowHeaders = FALSE,width = 1500, height = 400) %>%
        hot_col(c(2,4,5,6,7),readOnly = TRUE, width = 200) %>%
        hot_col(c(1,3),readOnly = TRUE, width = 200) %>%
        hot_cols(columnSorting = TRUE) %>%
        hot_table(stretchH = "all")
    })
  })
  observeEvent(input$SP_sim_reset_on,{
    SP_reactive_input$simulator_op_tbo_summary_event_table <- NULL
    SP_reactive_input$simulator_op_tbo_summary_ppg <- NULL
    shinyjs::hide("scenario_analysis_on")
  })
  
  observeEvent(input$SP_sim_run_on,{
    SP_reactive_input$events_all_op <- hot_to_r(input$SP_sim_event_table_on)
    if(nrow(SP_reactive_input$events_all_op[SP_reactive_input$events_all_op$Select == TRUE,]) == 0){
      sendSweetAlert(session,"Error!!","Please select atleast one substitue event",type = "error")
    }else{
      SP_reactive_input$events_all_op_selected <- SP_reactive_input$events_all_op[SP_reactive_input$events_all_op$Select == TRUE,]
      
      SP_reactive_input$simulator_op_tbo <- simulator(SP_reactive_input$SP_opti_op_prepared_tbo,SP_reactive_input$SP_opti_output_tbo[[2]],SP_reactive_input$SP_opti_op_tbo_clicked_event_table,SP_reactive_input$events_all_op_selected,SP_reactive_input$SP_TPO_list_tbo$other_sales,SP_reactive_input$SP_opti_op_tbo_event_table,input$SP_opti_ROI_selection_on,SP_reactive_input$SP_TPO_list_tbo$other_sales_value)
      
      SP_reactive_input$simulator_op_tbo_summary_event <- SP_reactive_input$simulator_op_tbo[[4]]
      SP_reactive_input$simulator_op_tbo_summary_event$lsm_nonlsm <- SP_reactive_input$ongoing_lsm_nonlsm
      SP_reactive_input$simulator_op_tbo_summary_event_table <- SP_reactive_input$simulator_op_tbo_summary_event[,c("Replaced.Event","RSP","DisplayType","PromotedPrice","lsm_nonlsm","GM_percent_model_event","Volume_sales_model_event","Gross_sales_model_event","ROI_model_event","Trade_as_per_NR_model_event","Net_Sales_model_event")]
      SP_reactive_input$simulator_op_tbo_summary_event_table[,c("Volume_sales_model_event","Gross_sales_model_event","Net_Sales_model_event")] <- SP_reactive_input$simulator_op_summary_event_table[,c("Volume_sales_model_event","Gross_sales_model_event","Net_Sales_model_event")]/10^3
      
      names(SP_reactive_input$simulator_op_summary_event_table) <- c("Replaced Event","RSP","Display Type","Promoted Price","LSM Constrained/Unconstrained","Gross Margin % of NR","Volume Sales('000 Units)","Scan Gross Sales('000 GBP)",input$SP_opti_ROI_selection_on,"Trade Spend % of NR","Scan Net Revenue('000 GBP)")
      
      SP_reactive_input$simulator_op_tbo_summary_event_table <- cbind(data.frame("Select Event to be Replaced" = rep(FALSE,nrow(SP_reactive_input$simulator_op_tbo_summary_event_table)),check.names = FALSE),SP_reactive_input$simulator_op_tbo_summary_event_table)
      #SP_reactive_input$simulator_op_tbo_summary_event_table <- SP_reactive_input$simulator_op_tbo_summary_event_table[,!(names(SP_reactive_input$simulator_op_tbo_summary_event_table) %in% "LSM/Non LSM"),with = FALSE]
      ###keeping the selected event first
      #SP_reactive_input$simulator_op_tbo_summary_event_table <- SP_reactive_input$simulator_op_tbo_summary_event_table[order(-SP_reactive_input$simulator_op_tbo_summary_event_table$`Trade ROI`),]
      output$SP_sim_run_outcomes_table_on <- renderRHandsontable({
        validate(
          need(SP_reactive_input$simulator_op_tbo_summary_event_table,"Select an event and run simulation")
        )
        rhandsontable(SP_reactive_input$simulator_op_tbo_summary_event_table) %>%
          hot_cell(1, "Select Event to be Replaced", readOnly = TRUE)
      })
      
      ######Base sales and incremental sales at PPG Level
      SP_reactive_input$simulator_op_tbo_summary_ppg <- SP_reactive_input$simulator_op_tbo[[3]]
      SP_reactive_input$simulator_op_tbo_summary_ppg$lsm_nonlsm <- SP_reactive_input$ongoing_lsm_nonlsm
      SP_reactive_input$simulator_op_tbo_summary_ppg$Replaced.Event <- as.character(SP_reactive_input$simulator_op_tbo_summary_ppg$Replaced.Event)
      
      output$SP_sim_run_outcomes_plot_on <- renderPlotly({
        validate(
          need(SP_reactive_input$simulator_op_tbo_summary_ppg,"Select an event and run simulation")
        )
        plot_ly(SP_reactive_input$simulator_op_tbo_summary_ppg,x= ~Replaced.Event, y= ~(base_sales_ppg),type='bar',name='Base Sales',marker = list(color='#7CC049'),
                hovertext = paste0(round((SP_reactive_input$simulator_op_tbo_summary_ppg$base_sales_ppg/10^3),2),"('000 Units)"),hoverinfo = 'text') %>%
          add_trace(y= ~(inc_sales_ppg), name = 'Incremental Sales',
                    hovertext = paste0(round((SP_reactive_input$simulator_op_tbo_summary_ppg$inc_sales_ppg/10^3),2),"('000 Units)"),hoverinfo = 'text',marker = list(color='#CC66FF')) %>%
          layout(title = "Promotion Effectiveness",font = list(size =10),xaxis = list(title = SP_reactive_input$simulator_op_tbo_summary_ppg$Replaced.Event,showgrid = FALSE),yaxis = list(title = 'Volume Sales',showgrid = FALSE),barmode = 'stack') %>% config(displayModeBar = FALSE)
      })
      shinyjs::show("scenario_analysis_on")
    }
  })
  ####Showing the metrics for Selected KPI and replaced KPI
  observeEvent(input$SP_sim_replace_on,{
    SP_reactive_input$SP_replace_event_tbo <- hot_to_r(input$SP_sim_run_outcomes_table_on)
    if(nrow(SP_reactive_input$SP_replace_event_tbo[SP_reactive_input$SP_replace_event_tbo$`Select Event to be Replaced` == TRUE,]) == 0){
      sendSweetAlert(session,"Error!!","Please select atleast one substitue event",type = "error")
      toggleModal(session, "SP_sim_replace_modal_on", toggle = "close")
    }else if(nrow(SP_reactive_input$SP_replace_event_tbo[SP_reactive_input$SP_replace_event_tbo$`Select Event to be Replaced` == TRUE,]) > 1){
      sendSweetAlert(session,"Error!!","Multiple events cannot be selected to replace, Please select single event",type = "error")
      toggleModal(session, "SP_sim_replace_modal_on", toggle = "close")
    }else if(nrow(SP_reactive_input$SP_replace_event_tbo[SP_reactive_input$SP_replace_event_tbo$`Select Event to be Replaced` == TRUE,]) == 1){
      SP_reactive_input$SP_replace_event_tbo_selected <- SP_reactive_input$SP_replace_event_tbo[SP_reactive_input$SP_replace_event_tbo$`Select Event to be Replaced` == TRUE,]
      SP_reactive_input$SP_sim_brand_results <- SP_reactive_input$simulator_op_tbo[[2]]
      if(SP_reactive_input$ongoing_lsm_nonlsm == "LSM"){
        SP_reactive_input$SP_sim_LSM_Violated_on <- SP_reactive_input$simulator_op_tbo[[5]][[as.character(SP_reactive_input$SP_replace_event_tbo_selected$`Replaced Event`)]]
      }else{
        SP_reactive_input$SP_sim_LSM_Violated_on <- ""
      }
      
      SP_reactive_input$SP_replace_compare_tbo <- SP_reactive_input$SP_sim_brand_results[SP_reactive_input$SP_sim_brand_results$Replaced.Event %in% c(as.character(SP_reactive_input$SP_replace_event_tbo_selected$`Replaced Event`),SP_reactive_input$SP_opti_op_tbo_clicked_event_table$`Event ID`),]
      SP_reactive_input$SP_replace_compare_tbo <- t(SP_reactive_input$SP_replace_compare_tbo)
      colnames(SP_reactive_input$SP_replace_compare_tbo) <- SP_reactive_input$SP_replace_compare_tbo[1,]
      
      SP_reactive_input$SP_replace_compare_tbo <- as.data.frame(SP_reactive_input$SP_replace_compare_tbo)
      
      SP_reactive_input$SP_replace_compare_tbo$KPI <- c("KPI","Gross Sales","Volume Sales","Net Revenue","Gross Margin","GM % NR","TI % NR","TI % NIS","Trade ROI","Value Market Share","Base Sales","Incremental Sales")
      SP_reactive_input$SP_replace_compare_tbo <- SP_reactive_input$SP_replace_compare_tbo[c(-1),]
      SP_reactive_input$SP_replace_compare_tbo[,c(1,2)] <- apply(SP_reactive_input$SP_replace_compare_tbo[,c(1,2)],2, as.numeric)
      SP_reactive_input$SP_replace_compare_tbo <- SP_reactive_input$SP_replace_compare_tbo[,c(3,1,2)]
      SP_reactive_input$SP_replace_compare_tbo$event_sign <- ifelse(SP_reactive_input$SP_replace_compare_tbo[,2] > SP_reactive_input$SP_replace_compare_tbo[,3], "Positive", ifelse(SP_reactive_input$SP_replace_compare_tbo[,2] < SP_reactive_input$SP_replace_compare_tbo[,3], "Negative","Neutral"))
      SP_reactive_input$SP_replace_compare_tbo[SP_reactive_input$SP_replace_compare_tbo$KPI == "TI % NR",]$event_sign <- ifelse(SP_reactive_input$SP_replace_compare_tbo[SP_reactive_input$SP_replace_compare_tbo$KPI == "TI % NR",2] < SP_reactive_input$SP_replace_compare_tbo[SP_reactive_input$SP_replace_compare_tbo$KPI == "TI % NR",3], "Positive", ifelse(SP_reactive_input$SP_replace_compare_tbo[SP_reactive_input$SP_replace_compare_tbo$KPI == "TI % NR",2] > SP_reactive_input$SP_replace_compare_tbo[SP_reactive_input$SP_replace_compare_tbo$KPI == "TI % NR",3], "Negative","Neutral"))
      
      output$SP_sim_LSM_Violated_on <- renderText({
        if(SP_reactive_input$SP_sim_LSM_Violated_on == TRUE){
          return("LSM Constraint gets violated due to replacement of this event")
        }else{
          return("")
        }
      })
      output$SP_sim_replace_compare_table_on <- renderDataTable({
        datatable(SP_reactive_input$SP_replace_compare_tbo,class="cell-border compact hover",extensions = c('FixedColumns'),
                  options = list(
                    columnDefs=list(list(visible=FALSE, targets=c(length(SP_reactive_input$SP_replace_compare_tbo)-1))),
                    # paging = FALSE,
                    dom = "t",
                    # searching = FALSE,
                    scrollX = F,
                    scrollY = F
                  ),rownames = F)%>%formatStyle(names(SP_reactive_input$SP_replace_compare_tbo),textAlign = 'center')%>%
          formatStyle(c(2),"event_sign",
                      background = styleEqual(c("Positive","Negative","Neutral"),c("#84c343","#FF4343","#FFC000"))) %>%
          formatStyle(c(3),"event_sign",
                      background = styleEqual(c("Negative","Positive","Neutral"),c("#84c343","#FF4343","#FFC000"))) %>%
          formatRound(c(2,3),digits = 2)
      })
    }
  })
  
  ###Replacing selected event in optimizer output tab#######Non LSM #######
  observeEvent(input$SP_sim_replace_confirm_on,{
    SP_reactive_input$SP_replace_event_tbo <- hot_to_r(input$SP_sim_run_outcomes_table_on)
    ####Updating KPI's
    SP_reactive_input$SP_replace_event_tbo_selected_details <- SP_reactive_input$simulator_op_tbo[[1]][[as.character(SP_reactive_input$SP_replace_event_tbo_selected$`Replaced Event`)]]
    SP_reactive_input$SP_opti_op_prepared_tbo <- SP_reactive_input$SP_replace_event_tbo_selected_details
    SP_reactive_input$simulated_flag_tbo <- 1
    
    SP_reactive_input$KPI_calculation_tbo <- KPI_calc_tbo(SP_reactive_input$SP_TPO_list_tbo,SP_reactive_input$annual_optim_op,SP_reactive_input$SP_opti_op_prepared_tbo,SP_reactive_input$SP_opti_exc_brand_tbo,SP_reactive_input$SP_opti_const_grouped_on,SP_reactive_input$SP_RB_Delist_on,input$SP_opti_ROI_selection_on,SP_reactive_input$selected_plan_split[[3]],SP_reactive_input$SP_opti_op_prepared_tbo_complete[[2]],SP_reactive_input$SP_restrictions_tbo,input$SP_opti_op_include_exclude_on)
    SP_reactive_input$KPI_calculation_ret_tbo <- KPI_calc_ret_tbo(SP_reactive_input$SP_TPO_list_tbo,SP_reactive_input$annual_optim_op,SP_reactive_input$SP_opti_op_prepared_tbo,SP_reactive_input$SP_opti_exc_brand_tbo,SP_reactive_input$SP_opti_const_grouped_on,SP_reactive_input$SP_RB_Delist_on,input$SP_opti_ROI_selection_on,SP_reactive_input$selected_plan_split[[3]],SP_reactive_input$SP_opti_op_prepared_tbo_complete[[2]],SP_reactive_input$SP_restrictions_tbo,input$SP_opti_op_include_exclude_on)
    
    
    #####Updating Calendar
    SP_reactive_input$opti_cal_filtered_tbo <- SP_reactive_input$SP_opti_op_prepared_tbo
    if(input$SP_opti_ROI_selection_on == "Incremental GM ROI"){
      SP_reactive_input$opti_cal_filtered_tbo[,ROI := sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
    }else if(input$SP_opti_ROI_selection_on == "Incremental NR ROI"){
      SP_reactive_input$opti_cal_filtered_tbo[,ROI := sum(R_Net_Rev_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
    }else if(input$SP_opti_ROI_selection_on == "Incremental NIS ROI"){
      SP_reactive_input$opti_cal_filtered_tbo[,ROI := sum(R_NIS_Inc)/sum(R_Trade_Inv_Inc), by = c("PPG","Category","Brand","Format","Tesco_Week_No")]
    }
    
    SP_reactive_input$opti_cal_filtered_tbo$Date <- ymd(SP_reactive_input$opti_cal_filtered_tbo$Week_Ending)
    week_no <- data.frame("Date" = sort(unique(SP_reactive_input$opti_cal_filtered_tbo$Date)),"Week No" = c(1:length(sort(unique(SP_reactive_input$opti_cal_filtered_tbo$Week_Ending)))),check.names = FALSE)
    SP_reactive_input$opti_cal_filtered_tbo <- left_join(SP_reactive_input$opti_cal_filtered_tbo,week_no,by = "Date")
    if(!(is.null(SP_reactive_input$opti_cal_filtered_tbo))){
      SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event <- ifelse(SP_reactive_input$opti_cal_filtered_tbo$Display_Flag == 1,"Display",ifelse(SP_reactive_input$opti_cal_filtered_tbo$TPR_Flag == 1,"TPR","No Promo"))
      #SP_reactive_input$opti_cal_filtered_tbo$Promo_Price <- round(SP_reactive_input$opti_cal_filtered_tbo$Promo_Price,2)
      SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event == "No Promo",]$Promo_Price <- NA_real_
      SP_reactive_input$opti_cal_filtered_tbo <- data.table(SP_reactive_input$opti_cal_filtered_tbo)
      SP_reactive_input$opti_cal_filtered_tbo$ROI_cal <- SP_reactive_input$opti_cal_filtered_tbo$ROI
      SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event == "No Promo",]$ROI_cal <- -100
      SP_reactive_input$brks_ROI_tbo <- quantile(SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event != "No Promo" & SP_reactive_input$opti_cal_filtered_tbo$ROI_cal >= 0,]$ROI_cal, seq(0,1, .05), na.rm = TRUE)
      SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event == "TPR",]$Tesco_Event <- "Shelf Promotion"
      SP_reactive_input$opti_cal_filtered_tbo[SP_reactive_input$opti_cal_filtered_tbo$Tesco_Event == "Display",]$Tesco_Event <- "Display Feature Promotion"
      SP_reactive_input$opti_cal_filtered_tbo$`LSM Promo Price` <- ""
      SP_reactive_input$opti_cal_dcast_tbo <- data.table::dcast(unique(SP_reactive_input$opti_cal_filtered_tbo[,c("Format","PPG","PPG_Description","Week No","Promo_Price","RSP_Unit","LSM Promo Price","Tesco_Event","ROI_cal")]),Format + PPG + PPG_Description + RSP_Unit + `LSM Promo Price` ~`Week No`,value.var= c("Promo_Price","Tesco_Event","ROI_cal"))
      
      SP_reactive_input$opti_cal_dcast_tbo_ROI <- SP_reactive_input$opti_cal_dcast_tbo
      names(SP_reactive_input$opti_cal_dcast_tbo) <- gsub("Promo_Price_","",names(SP_reactive_input$opti_cal_dcast_tbo))
      names(SP_reactive_input$opti_cal_dcast_tbo) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_tbo))
      
      names(SP_reactive_input$opti_cal_dcast_tbo_ROI) <- gsub("ROI_cal_","",names(SP_reactive_input$opti_cal_dcast_tbo_ROI))
      names(SP_reactive_input$opti_cal_dcast_tbo_ROI) <- gsub("Tesco_","",names(SP_reactive_input$opti_cal_dcast_tbo_ROI))
      
      setcolorder(SP_reactive_input$opti_cal_dcast_tbo,c("Format","PPG","PPG_Description","RSP_Unit",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`))),paste0("ROI_cal","_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)))))
      setcolorder(SP_reactive_input$opti_cal_dcast_tbo_ROI,c("Format","PPG","PPG_Description","RSP_Unit",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`)),paste0("Event","_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`))),paste0("Promo_Price","_",sort(unique(SP_reactive_input$opti_cal_filtered_tbo$`Week No`))))) 
      
    }
    SP_reactive_input$optimizer_op_download_tbo <- list("Optimizer Output" = SP_reactive_input$SP_opti_op_prepared_tbo,"Excluded PPG" = SP_reactive_input$SP_opti_exc_brand_tbo, "EAN PPG Mapping" = SP_reactive_input$EAN_PPG_download_tbo)
    write.xlsx(SP_reactive_input$optimizer_op_download_tbo, file = paste0(SP_reactive_input$folder_path,"/","Optimization Output TBO.xlsx"))
    toggleModal(session, "SP_sim_replace_modal_on", toggle = "close")
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu11")
  })
  
  #Navigation buttons in simulator (ongoing) tab
  observeEvent(input$SP_sim_top_panel_left_on,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu11")
  })
  
  observeEvent(input$SP_sim_top_panel_right_on,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu13")
    ong_opti_plans <- gsub(".RData","",list.files(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Ongoing/")))
    ann_opti_plans <- gsub(".RData","",list.files(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/")))
    updateSelectizeInput(session,"SP_cmp_scn_base_tpo",choices = ann_opti_plans, selected = input$SP_opti_tpo_on)
    updateSelectizeInput(session,"SP_cmp_scn_tpo_on_1",choices = ong_opti_plans, selected = ong_opti_plans[1])
    updateSelectizeInput(session,"SP_cmp_scn_tpo_on_2",choices = ong_opti_plans, selected = ong_opti_plans[2])
  })
  
  observeEvent({input$SP_cmp_scn_base_tpo
    input$SP_cmp_scn_tpo_on_1
    input$SP_cmp_scn_tpo_on_2},{
      if(input$SP_opti_tpo_on!= ""){
        load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Annual/",input$SP_cmp_scn_base_tpo,".RData"))
        SP_reactive_input$cmp_scn_base_plan <- tpo_list_temp
      }
      if(input$SP_cmp_scn_tpo_on_1 != "" & input$SP_cmp_scn_tpo_on_2 != ""){
        load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Ongoing/",input$SP_cmp_scn_tpo_on_1,".RData"))
        SP_reactive_input$cmp_scn_tpo_1 <- tpo_list_temp
        
        load(paste0(SP_reactive_input$folder_path,"/Saved Optimizers Ongoing/",input$SP_cmp_scn_tpo_on_2,".RData"))
        SP_reactive_input$cmp_scn_tpo_2 <- tpo_list_temp
        SP_reactive_input$cmp_scn_tpo_1$opti_output$Week_Ending <- as.Date(SP_reactive_input$cmp_scn_tpo_1$opti_output$Week_Ending)
        SP_reactive_input$cmp_scn_tpo_2$opti_output$Week_Ending <- as.Date(SP_reactive_input$cmp_scn_tpo_2$opti_output$Week_Ending)
        
        SP_reactive_input$cmp_scn_base_plan_opti_op_total <- SP_reactive_input$cmp_scn_base_plan$opti_output[,.("Scan Net Revenue(MM GBP)" = sum(Net_Revenue)/10^6,"Gross Margin(MM GBP)" = sum(GM_Abs)/10^6,"Scan Gross Sales(MM GBP)" = sum(Gross_Sales)/10^6, "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                                "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue), "TI % NIS" = sum(Total_Trade_Investment) * 100/sum(NIS), "Incremental ROI" = sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), "Volume Sales(MM Units)" = sum(Total_Sales)/10^6),by = .(Category,Brand)]
        
        SP_reactive_input$cmp_scn_tpo_1_opti_op_total <- SP_reactive_input$cmp_scn_tpo_1$opti_output[,.("Scan Net Revenue(MM GBP)" = sum(Net_Revenue)/10^6,"Gross Margin(MM GBP)" = sum(GM_Abs)/10^6,"Scan Gross Sales(MM GBP)" = sum(Gross_Sales)/10^6, "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                        "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue), "TI % NIS" = sum(Total_Trade_Investment) * 100/sum(NIS), "Incremental ROI" = sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), "Volume Sales(MM Units)" = sum(Total_Sales)/10^6),by = .(Category,Brand)]
        SP_reactive_input$cmp_scn_tpo_2_opti_op_total <- SP_reactive_input$cmp_scn_tpo_2$opti_output[,.("Scan Net Revenue(MM GBP)" = sum(Net_Revenue)/10^6,"Gross Margin(MM GBP)" = sum(GM_Abs)/10^6,"Scan Gross Sales(MM GBP)" = sum(Gross_Sales)/10^6, "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                        "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue), "TI % NIS" = sum(Total_Trade_Investment) * 100/sum(NIS), "Incremental ROI" = sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), "Volume Sales(MM Units)" = sum(Total_Sales)/10^6),by = .(Category,Brand)]
        SP_reactive_input$base_sim_merged_total <- data.frame(cbind(setdiff(names(SP_reactive_input$cmp_scn_base_plan_opti_op_total),c("Category","Brand")),as.numeric(t(SP_reactive_input$cmp_scn_base_plan_opti_op_total[,-c("Category","Brand")])),as.numeric(t(SP_reactive_input$cmp_scn_tpo_1_opti_op_total[,-c("Category","Brand")])),as.numeric(t(SP_reactive_input$cmp_scn_tpo_2_opti_op_total[,-c("Category","Brand")]))),stringsAsFactors = FALSE)
        
        names(SP_reactive_input$base_sim_merged_total) <- c("KPI","Base Plan","TPO 1","TPO 2")
        SP_reactive_input$base_sim_merged_total[,c("Base Plan","TPO 1","TPO 2")] <- sapply(SP_reactive_input$base_sim_merged_total[,c("Base Plan","TPO 1","TPO 2")],as.numeric)
        
        output$SP_cmp_scn_table_kpi <- renderDataTable({
          datatable(SP_reactive_input$base_sim_merged_total,class="cell-border compact hover",selection =list(target='cell',mode="single"),
                    options = list(
                      columnDefs=list(list(className = 'dt-center', targets = 0:(length(SP_reactive_input$base_sim_merged_total)-1)),list(width = '200px',targets=c(0))),
                      paging = FALSE,
                      searching = FALSE,
                      dom = 't',
                      ordering = F,
                      scrollX = TRUE,
                      scrollY = "260px",
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#EA3592', 'color': '#fff'});",
                        "}")
                    ),rownames = F) %>%
            formatStyle(names(SP_reactive_input$base_sim_merged_total),textAlign = 'center') %>%
            formatStyle("KPI", backgroundColor = "#0099cc", color = "#FFFFFF") %>%
            formatRound(names(select_if(SP_reactive_input$base_sim_merged_total,is.numeric)),digits = 2) %>%
            formatStyle("KPI",cursor = "pointer")
        })
        SP_reactive_input$cmp_scn_base_plan_opti_op <- SP_reactive_input$cmp_scn_base_plan$opti_output[,.("Scan Net Revenue(MM GBP)" = sum(Net_Revenue)/10^6,"Gross Margin(MM GBP)" = sum(GM_Abs)/10^6,"Scan Gross Sales(MM GBP)" = sum(Gross_Sales)/10^6, "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                          "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue), "TI % NIS" = sum(Total_Trade_Investment) * 100/sum(NIS), "Incremental ROI" = sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), "Volume Sales(MM Units)" = sum(Total_Sales)/10^6),by = .(Category,Brand,Week_Ending)]
        setnames(SP_reactive_input$cmp_scn_base_plan_opti_op,names(select_if(SP_reactive_input$cmp_scn_base_plan_opti_op,is.numeric)),paste0(names(select_if(SP_reactive_input$cmp_scn_base_plan_opti_op,is.numeric)),"_base"))
        SP_reactive_input$cmp_scn_tpo_1_opti_op <- SP_reactive_input$cmp_scn_tpo_1$opti_output[,.("Scan Net Revenue(MM GBP)" = sum(Net_Revenue)/10^6,"Gross Margin(MM GBP)" = sum(GM_Abs)/10^6,"Scan Gross Sales(MM GBP)" = sum(Gross_Sales)/10^6, "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                  "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue), "TI % NIS" = sum(Total_Trade_Investment) * 100/sum(NIS), "Incremental ROI" = sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), "Volume Sales(MM Units)" = sum(Total_Sales)/10^6),by = .(Category,Brand,Week_Ending)]
        setnames(SP_reactive_input$cmp_scn_tpo_1_opti_op,names(select_if(SP_reactive_input$cmp_scn_tpo_1_opti_op,is.numeric)),paste0(names(select_if(SP_reactive_input$cmp_scn_tpo_1_opti_op,is.numeric)),"_tpo1"))
        SP_reactive_input$cmp_scn_tpo_2_opti_op <- SP_reactive_input$cmp_scn_tpo_2$opti_output[,.("Scan Net Revenue(MM GBP)" = sum(Net_Revenue)/10^6,"Gross Margin(MM GBP)" = sum(GM_Abs)/10^6,"Scan Gross Sales(MM GBP)" = sum(Gross_Sales)/10^6, "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                                                                                                  "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue), "TI % NIS" = sum(Total_Trade_Investment) * 100/sum(NIS), "Incremental ROI" = sum(R_GM_Inc)/sum(R_Trade_Inv_Inc), "Volume Sales(MM Units)" = sum(Total_Sales)/10^6),by = .(Category,Brand,Week_Ending)]
        setnames(SP_reactive_input$cmp_scn_tpo_2_opti_op,names(select_if(SP_reactive_input$cmp_scn_tpo_2_opti_op,is.numeric)),paste0(names(select_if(SP_reactive_input$cmp_scn_tpo_2_opti_op,is.numeric)),"_tpo2"))
        SP_reactive_input$base_sim_merged <- left_join(left_join(SP_reactive_input$cmp_scn_base_plan_opti_op,SP_reactive_input$cmp_scn_tpo_1_opti_op,by = c("Category","Brand","Date")),SP_reactive_input$cmp_scn_tpo_2_opti_op,by = c("Category","Brand","Date"))
        
        output$SP_cmp_scn_chart_on <- renderPlotly({
          plot_ly(SP_reactive_input$base_sim_merged,x = ~Week_Ending,y = ~`Scan Net Revenue(MM GBP)_base`, name = "Forecast Base Plan",type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~`Scan Net Revenue(MM GBP)_tpo1`, name = "Forecast TPO 1",type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~`Scan Net Revenue(MM GBP)_tpo2`, name = "Forecast TPO 2",type = 'scatter', mode = 'lines') %>%
            layout(title = "",xaxis = list(title = "Time Period"),yaxis = list(title = "Scan Net Revenue(MM GBP)"),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
        })
      }
    })
  
  observeEvent(input$SP_cmp_scn_table_kpi_cell_clicked,{
    if(length(input$SP_cmp_scn_table_kpi_cell_clicked) != 0){
      SP_reactive_input$base_sim_merged <- data.table(SP_reactive_input$base_sim_merged)
      kpi_selected <- input$SP_cmp_scn_table_kpi_cell_clicked$value
      output$SP_cmp_scn_chart_on <- renderPlotly({
        plot_ly(SP_reactive_input$base_sim_merged,x = ~Week_Ending,y = SP_reactive_input$base_sim_merged[[(paste0(kpi_selected,"_base"))]], name = "Forecast Base Plan",type = 'scatter', mode = 'lines') %>%
          add_trace(y = SP_reactive_input$base_sim_merged[[(paste0(kpi_selected,"_tpo1"))]], name = "Forecast TPO 1",type = 'scatter', mode = 'lines') %>%
          add_trace(y = SP_reactive_input$base_sim_merged[[(paste0(kpi_selected,"_tpo2"))]], name = "Forecast TPO 2",type = 'scatter', mode = 'lines') %>%
          layout(title = "",xaxis = list(title = "Time Period"),yaxis = list(title = kpi_selected),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
      })
    }
  })
  
  # observeEvent(input$SP_cmp_scn_op_top_panel_right_on,{
  #   SP_reactive_input$SP_act_req_data <- fread("AR_Test.csv")
  #   updateSelectizeInput(session,"SP_act_req_cat",selected = unique(SP_reactive_input$SP_act_req_data$Category)[1],choices = unique(SP_reactive_input$SP_act_req_data$Category))
  #   
  #   # output$SP_act_req_start_date_ui <- renderUI({
  #   #   SP_reactive_input$SP_date_vector <- unique(SP_reactive_input$SP_act_req_data$Date)[order(unique(SP_reactive_input$SP_act_req_data$Date))]
  #   #   selectizeInput("SP_act_req_date_start","Start Date",choices = SP_reactive_input$SP_date_vector,selected = min(SP_reactive_input$SP_date_vector[year(SP_reactive_input$SP_date_vector) == year(max(SP_reactive_input$SP_date_vector))]))
  #   # })
  #   # 
  #   # output$SP_act_req_end_date_ui <- renderUI({
  #   #   selectizeInput("SP_act_req_date_end","End Date",choices = SP_reactive_input$SP_date_vector,selected = max(SP_reactive_input$SP_date_vector))
  #   # })
  #   
  #   observeEvent({
  #     input$SP_act_req_cat},{
  #       updateSelectizeInput(session,"SP_act_req_brand",choices = c("",unique(SP_reactive_input$SP_act_req_data[SP_reactive_input$SP_act_req_data$Category == input$SP_act_req_cat,]$Brand)),selected = unique(SP_reactive_input$SP_act_req_data[SP_reactive_input$SP_act_req_data$Category == input$SP_act_req_cat,]$Brand)[1])
  #     }
  #   )
  #   
  #   observeEvent({
  #     input$SP_act_req_cat
  #     input$SP_act_req_brand},{
  #       updateSelectizeInput(session,"SP_act_req_format",choices = c("",unique(SP_reactive_input$SP_act_req_data[SP_reactive_input$SP_act_req_data$Category == input$SP_act_req_cat & SP_reactive_input$SP_act_req_data$Brand %in% input$SP_act_req_brand,]$Format)),selected = unique(SP_reactive_input$SP_act_req_data[SP_reactive_input$SP_act_req_data$Category == input$SP_act_req_cat & SP_reactive_input$SP_act_req_data$Brand %in% input$SP_act_req_brand,]$Format)[1])
  #     })
  #   
  #   observeEvent({
  #     input$SP_act_req_cat
  #     input$SP_act_req_format
  #     input$SP_act_req_brand},{
  #       updateSelectizeInput(session,"SP_act_req_ppg",choices = c("",unique(SP_reactive_input$SP_act_req_data[SP_reactive_input$SP_act_req_data$Category == input$SP_act_req_cat & SP_reactive_input$SP_act_req_data$Brand %in% input$SP_act_req_brand & SP_reactive_input$SP_act_req_data$Format %in% input$SP_act_req_format,]$`PPG`)),selected = unique(SP_reactive_input$SP_act_req_data[SP_reactive_input$SP_act_req_data$Category == input$SP_act_req_cat & SP_reactive_input$SP_act_req_data$Brand %in% input$SP_act_req_brand & SP_reactive_input$SP_act_req_data$Format %in% input$SP_act_req_format,]$`PPG`)[1])
  #     })
  #   
  #   updateTabItems(session, "sidebar_main", selected = "SP_subMenu14")
  # })
  # 
  # observeEvent({
  #   input$SP_act_req_cat
  #   input$SP_act_req_brand
  #   input$SP_act_req_format
  #   input$SP_act_req_ppg
  #   input$SP_act_req_date[1]
  #   input$SP_act_req_date[2]},{
  #     if(!(is.null(SP_reactive_input$SP_act_req_data))){
  #       
  #       SP_reactive_input$SP_act_req_data_selected <- data.table(SP_reactive_input$SP_act_req_data[SP_reactive_input$SP_act_req_data$Category == input$SP_act_req_cat & SP_reactive_input$SP_act_req_data$Brand == input$SP_act_req_brand & SP_reactive_input$SP_act_req_data$Format == input$SP_act_req_format & SP_reactive_input$SP_act_req_data$PPG == input$SP_act_req_ppg &
  #                                                                                                    SP_reactive_input$SP_act_req_data$Week_Ending >= input$SP_act_req_date[1] & SP_reactive_input$SP_act_req_data$Week_Ending <= input$SP_act_req_date[2],])
  #       SP_reactive_input$SP_act_req_data_grouped <- SP_reactive_input$SP_act_req_data_selected[,.("NR" = sum(Net_Revenue), "NR_Act" = sum(Net_Revenue_Actual), "GM" = sum(GM_Abs), "GM_Act" = sum(GM_Abs_Actual), "Gross_Sales" = sum(Gross_Sales), "Gross_Sales_Act" = sum(Gross_Sales_Actual),
  #                                                                                                  "TI" = sum(Total_Trade_Investment), "TI_Act" = sum(Total_Trade_Investment_Actual), "Inc_GM_Abs" = sum(Inc_GM_Abs), "Inc_GM_Abs_Act" = sum(Inc_GM_Abs_Actual), "Vol_Sales" = sum(Total_Sales), "Vol_Sales_Act" = sum(Total_Sales_Actual),
  #                                                                                                  "GM % NR" = sum(GM_Abs)* 100/sum(Net_Revenue), "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue), "ROI" = sum(Inc_GM_Abs)/sum(Total_Trade_Investment), "GM % NR_Act" = sum(GM_Abs_Actual) * 100/sum(Net_Revenue_Actual), "TI % NR_Act" = sum(Total_Trade_Investment_Actual) * 100/sum(Net_Revenue_Actual), "ROI_Act" = sum(Inc_GM_Abs_Actual)/sum(Total_Trade_Investment_Actual)),by = .(PPG,Tesco_Week_No)]
  #       SP_reactive_input$SP_act_req_data_grouped$`NR_%` <- (SP_reactive_input$SP_act_req_data_grouped$NR_Act - SP_reactive_input$SP_act_req_data_grouped$NR)/SP_reactive_input$SP_act_req_data_grouped$NR_Act
  #       SP_reactive_input$SP_act_req_data_grouped$`GM_NR_%` <- (SP_reactive_input$SP_act_req_data_grouped$`GM % NR_Act` - SP_reactive_input$SP_act_req_data_grouped$`GM % NR`)/SP_reactive_input$SP_act_req_data_grouped$`GM % NR_Act`
  #       SP_reactive_input$SP_act_req_data_grouped$`TI_NR_%` <- (SP_reactive_input$SP_act_req_data_grouped$`TI % NR_Act` - SP_reactive_input$SP_act_req_data_grouped$`TI % NR`)/SP_reactive_input$SP_act_req_data_grouped$`TI % NR_Act`
  #       SP_reactive_input$SP_act_req_data_grouped$`Gross_Sales_%` <- (SP_reactive_input$SP_act_req_data_grouped$Gross_Sales_Act - SP_reactive_input$SP_act_req_data_grouped$Gross_Sales)/SP_reactive_input$SP_act_req_data_grouped$Gross_Sales_Act
  #       SP_reactive_input$SP_act_req_data_grouped$`Vol_Sales_%` <- (SP_reactive_input$SP_act_req_data_grouped$Vol_Sales_Act - SP_reactive_input$SP_act_req_data_grouped$Vol_Sales)/SP_reactive_input$SP_act_req_data_grouped$Vol_Sales_Act
  #       SP_reactive_input$SP_act_req_data_grouped$`ROI_%` <- (SP_reactive_input$SP_act_req_data_grouped$ROI_Act - SP_reactive_input$SP_act_req_data_grouped$ROI)/SP_reactive_input$SP_act_req_data_grouped$ROI_Act
  #       
  #       SP_reactive_input$SP_act_req_data_grouped_req <- SP_reactive_input$SP_act_req_data_grouped[,c("NR","NR_Act","NR_%","GM % NR","GM % NR_Act","GM_NR_%","TI % NR","TI % NR_Act","TI_NR_%","Gross_Sales","Gross_Sales_Act","Gross_Sales_%","Vol_Sales","Vol_Sales_Act","Vol_Sales_%","ROI","ROI_Act","ROI_%")]
  #       #names(SP_reactive_input$SP_act_req_data_grouped_req) <- c("Net Revenue Projected","Net Revenue ")
  #     }
  #     
  #     
  #   })
  # 
  # output$SP_act_req_table <- renderDataTable({
  #   validate(
  #     need(SP_reactive_input$SP_act_req_data_grouped_req,"")
  #   )
  #   sketch = htmltools::withTags(table(
  #     class = 'cell-border compact hover',
  #     thead(
  #       tr(
  #         th(colspan = 1, 'Month'),
  #         th(colspan = 3, 'Net Revenue'),
  #         th(colspan = 3, 'GM % NR'),
  #         th(colspan = 3, 'TI % NR'),
  #         th(colspan = 3, 'Gross Sales'),
  #         th(colspan = 3, 'Volume Sales'),
  #         th(colspan = 3, 'Trade ROI')
  #       ),
  #       tr(
  #         lapply(
  #           c("Event ID",rep(c("Actual","Projected","% Diff"),6)),
  #           th
  #         )
  #       )
  #     )
  #   ))
  #   
  #   datatable(SP_reactive_input$SP_act_req_data_grouped_req,container=sketch,class="cell-border compact hover",extensions = c('FixedColumns','Buttons'),
  #             #selection=list(mode="single", target="cell"),
  #             options = list(
  #               fixedColumns = list(leftColumns = 1),
  #               paging = FALSE,
  #               searching = FALSE,
  #               dom = 'Bt',
  #               bSort=FALSE,
  #               scrollX = TRUE,
  #               scrollY = "200px"
  #             ),rownames = F)
  # })
  
  #Navigation buttons in Compare Scenarios Ongoing
  observeEvent(input$SP_cmp_scn_op_top_panel_left_on,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu12")
  })
  
  observeEvent(input$SP_cmp_scn_op_top_panel_right_on,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu14")
  })
  
  #Navigation buttons in Actions Required tab
  observeEvent(input$SP_act_req_op_top_panel_left,{
    updateTabItems(session, "sidebar_main", selected = "SP_subMenu13")
  })
})