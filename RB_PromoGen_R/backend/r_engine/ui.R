library(shiny)
library(shinydashboard)
library(shinyjs)
ui <- shinydashboard::dashboardPage(

  skin = "black",
  title = "PromoGen",

  #---------------- HEADER ----------------#
  dashboardHeader(
    title = HTML(paste0(
      '<span class="logo-lg"><a href="#"><img src="reckitt_logo_MASTER_RGB.png" height="35" width="170"></a></span>',
      '<span class="logo-mini"><a href="#"><img src="reckitt_logo_MASTER_RGB.png" height="40" width="50"></a></span>'
    )),
    titleWidth = 300,
    user = userOutput("user")
  ),
  ),
  
  #- sidebar
  dashboardSidebar(
    
    width = 300,
    
    sidebarUserPanelOutput(outputId = "dynamicSidebarUserPanel"),
    
    sidebarMenu(
      
      id = "sidebar_main",
      
      menuItem("PromoGen", tabName = "SP", icon = icon("line-chart"),
               menuSubItem("Data Input", tabName = "SP_subMenu1"),
               menuSubItem("Executive Summary", tabName = "SP_subMenu2"),
               menuSubItem("Promotion Effectiveness", tabName = "SP_subMenu3"),
               #menuSubItem("Promotion Example",icon = icon("calculator"), tabName = "SP_subMenu4"),
               menuSubItem("Optimizer", tabName = "SP_subMenu5"),
               menuSubItem("Optimization Outputs", tabName = "SP_subMenu6"),
               menuSubItem("Simulator", tabName = "SP_subMenu7")
               )
    )
    
  ),
  
  #- body
  dashboardBody(
    
    # add top title for the toolkit
    tags$head(tags$style(HTML(
      '.myClass { 
      line-height: 50px;
      text-align: center;
      font-family: "Arial";
      padding: 0 15px;
      color: #E42E92;
      font-size: x-large;
      font-weight: 900;
      }
      @media (min-width: 1200px) {
      .myClass {
      line-height: 50px;
      text-align: center;
      font-family: "Arial";
      padding: 0 15px;
      color: #E42E92;
      font-size: x-large;
      font-weight: 900
      }
      }
      '))),
    tags$script(HTML('
                     $(document).ready(function() {
                     $("header").find("nav").append(\'<div class="myClass"> PromoGen</div>\');
                     })
                     ')),
    
    # add custom css and js
    
    # enter buttons to mimic click
    tags$head(tags$script(src="js/enter_as_click.js")),
    
    # ajax data tables error 
    tags$head(tags$script(src="js/ajax_error.js")),
    
    HTML('<script type="text/javascript" src="https://c64.assets-yammer.com/assets/platform_embed.js"></script>'),
    
    # CSS to control body color
    tags$head(
      tags$style(
        HTML("
             
             /* User body */
             .skin-black .content-wrapper{
             background: linear-gradient(white, #edeaea);
             }
             
             /* Box header */
             .box.box-solid.box-primary>.box-header {
             background: #e42e92;
             }
             
             /* Box border colors */
             .box.box-solid.box-primary{
             border-bottom-color:#e42e92;
             border-left-color:#e42e92;
             border-right-color:#e42e92;
             border-top-color:#e42e92;
             }
             .navbar {
             min-height: 20px;
             height: 20px;
             }
             
             .box.box-primary {
             border-top-color: #e42e92;
             }
             
             ")
             )
             ),
    
    # CSS to control fonts styles of body 
    tags$head(
      tags$style(
        HTML("
             
             h4.intro {
             font-family: 'Helvetica','Arial', sans-serif;
             color: #717d87;
             }
             
             ")
        )
        ),
    
    
    
    
    # add tab w.r.t menu
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "SP_subMenu1",
        fluidRow(
          # box(
          #   status = "primary", solidHeader = TRUE,width = 6,title = 'Data Upload',
          #   div(fileInput("SP_inputpath_nielsen_rms", label = h5(tags$b("Choose Nielsen RMS data"),
          #                                                    bsButton("tip_1","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 # accept = c(
          #                 #   ".xlsx",
          #                 #   "text/csv",
          #                 #   "text/comma-separated-values,text/plain",
          #                 #   ".csv")
          #                 accept = c(".Zip")
          #   )),
          #   bsTooltip("tip_1","Weekly sales for entire Tesco retailer at SKU level(Nielsen RMS data)",placement = "right",trigger = "hover",options = list(container = "body")),
          #   
          #   div(fileInput("SP_inputpath_nielsen_model_results", label = h5(tags$b("Choose Nielsen Model Results data"),
          #                                                        bsButton("tip_2","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 # accept = c(
          #                 #   ".xlsx",
          #                 #   "text/csv",
          #                 #   "text/comma-separated-values,text/plain",
          #                 #   ".csv")
          #                 accept = c(".Zip")
          #   )),
          #   bsTooltip("tip_2","Nielsen Model Results",placement = "right",trigger = "hover",options = list(container = "body")),
          #   
          #   div(fileInput("SP_inputpath_nielsen_event", label = h5(tags$b("Choose Nielsen Event list"),
          #                                                        bsButton("tip_3","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 # accept = c(
          #                 #   ".xlsx",
          #                 #   "text/csv",
          #                 #   "text/comma-separated-values,text/plain",
          #                 #   ".csv")
          #                 accept = c(".Zip")
          #   )),
          #   bsTooltip("tip_3","Nielsen Promotion Event List",placement = "right",trigger = "hover",options = list(container = "body")),
          #   
          #   div(fileInput("SP_inputpath_EAN_PPG", label = h5(tags$b("Choose EAN to PPG Mapping"),
          #                                                        bsButton("tip_4","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 # accept = c(
          #                 #   ".xlsx",
          #                 #   "text/csv",
          #                 #   "text/comma-separated-values,text/plain",
          #                 #   ".csv")
          #                 accept = c(".Zip")
          #   )),
          #   bsTooltip("tip_4","EAN to PPG mapping of RB SKUs",placement = "right",trigger = "hover",options = list(container = "body")),
          #   
          #   div(fileInput("SP_inputpath_cost_bible", label = h5(tags$b("Choose Cost Bible data"),
          #                                                        bsButton("tip_5","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 # accept = c(
          #                 #   ".xlsx",
          #                 #   "text/csv",
          #                 #   "text/comma-separated-values,text/plain",
          #                 #   ".csv")
          #                 accept = c(".Zip")
          #   )),
          #   bsTooltip("tip_5","Cost Bible data for all RB SKUs",placement = "right",trigger = "hover",options = list(container = "body")),
          #   
          #   div(fileInput("SP_inputpath_tesco_slot", label = h5(tags$b("Choose Tesco Slots data"),
          #                                                       bsButton("tip_6","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 accept = c(
          #                   ".xlsx",
          #                   "text/csv",
          #                   "text/comma-separated-values,text/plain",
          #                   ".csv")
          #   )),
          #   bsTooltip("tip_6","Promotion Slots of retailer for optimization period at weekly Granularity",placement = "right",trigger = "hover",options = list(container = "body")),
          #   
          #   div(fileInput("SP_inputpath_ean_ldesc_map", label = h5(tags$b("Choose EAN to SKU Desc Mapping"),
          #                                                       bsButton("tip_7","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 accept = c(
          #                   ".xlsx",
          #                   "text/csv",
          #                   "text/comma-separated-values,text/plain",
          #                   ".csv")
          #   )),
          #   bsTooltip("tip_7","EAN to SKU Description mapping",placement = "right",trigger = "hover",options = list(container = "body")),
          #   
          #   div(fileInput("SP_inputpath_LSM", label = h5(tags$b("Choose LSM output data"),
          #                                                          bsButton("tip_8","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 accept = c(
          #                   ".xlsx",
          #                   "text/csv",
          #                   "text/comma-separated-values,text/plain",
          #                   ".csv")
          #   )),
          #   bsTooltip("tip_8","LSM Output",placement = "right",trigger = "hover",options = list(container = "body")),
          #   
          #   div(fileInput("SP_inputpath_delist", label = h5(tags$b("Choose delisted SKUs"),
          #                                                          bsButton("tip_9","",icon = icon("question"),style = "info",size = "extra-small")),
          #                 accept = c(
          #                   ".xlsx",
          #                   "text/csv",
          #                   "text/comma-separated-values,text/plain",
          #                   ".csv")
          #   )),
          #   bsTooltip("tip_9","Delisted SKUs data",placement = "right",trigger = "hover",options = list(container = "body")),
          #   actionButton("SP_proceed_summ","Proceed",icon=icon("arrow-right"),
          #                style = "color:#fff; background-color: #0099DC;border-color: #0099DC")
          # )
          column(12, align="center",
                 div(style="display: inline-block;",img(src="landing_page_3.jpg", height=650, width=1300)))
        ),br(),
        fluidRow(
          column(2,selectizeInput("SP_cust","Customer",choices="Carrefour",multiple = F)),
          #
          column(2,selectizeInput("SP_manuf","Manufacturer",choices=c("Manu01"),multiple = F)),
          column(2,selectizeInput("SP_brand","Brand",choices = c("Brand005"),multiple = F)),
          column(2,selectizeInput("SP_level","PPG/Sub PPG",choices = c("PPG","Sub PPG"),multiple = F)),
          #(list.files(paste0(str_sub(getwd(),1,gregexpr("/",getwd())[[1]][length(gregexpr("/",getwd())[[1]])]-1)))[!(list.files(paste0(str_sub(getwd(),1,gregexpr("/",getwd())[[1]][length(gregexpr("/",getwd())[[1]])]-1))) %in% "Codes")])
          
          column(2,br(),actionButton("SP_proceed_summ","Proceed",icon=icon("arrow-right"),
                                     style = "color:#fff; background-color: #0099DC;border-color: #0099DC"))
        )
        ,br(),br(),br(),br()
        #,br(),br(),br(),br()
      ),
      tabItem(
        tabName = "SP_subMenu2",
        fluidRow(column(12,
                        column(4,h4("Executive Summary",skin = "#E42E92")),
                        #column(1,offset = 5,downloadButton("SP_summ_top_panel_download","",icon = icon("download"))),
                        column(1,offset = 6,actionButton("SP_summ_top_panel_left","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_summ_top_panel_right","",icon = icon("arrow-right"))))),
        #tags$hr(style="broder-color:purple;"),
        # absolutePanel(id = "top_panel",top = "10%",left = 0,right = 0,bottom = "80%", width = "100%", height = "10%", fixed = TRUE,
        #               column(12,
        #                      column(4,h2("Absolute panel")),
        #                      column(4,h2("Absolute panel")),
        #                      column(4,h2("Absolute panel")),
        #                      column(4,h2("Absolute panel"))
        #               )
        #),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(1,
                   selectizeInput("SP_summ_coun","Country",choices="UAE")),
            column(1,
                   selectizeInput("SP_summ_cust","Customer",choices="Carrefour")),
            column(1,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_summ_cat","Category",choices=NULL)),
            # column(3,
            #        selectizeInput("SP_summ_seg","Segment",choices=NULL)),
            column(1,
                   selectizeInput("SP_summ_manuf","Manufacturer",choices=NULL,multiple = T)),
            column(1,
                   selectizeInput("SP_summ_brand","Brand",choices=NULL,multiple = T)),
            column(2,
                   selectizeInput("SP_summ_format","Format",choices=NULL,multiple = T)),
            column(1,
                   selectizeInput("SP_summ_ppg","PPG",choices=NULL,multiple = T)),
            # column(2,
            #        #tags$style(type='text/css',".selectize-input {font-size:32px;line-height:32px;} .selectize-dropdown {font-size:28px;line-height:28px;}"),
            #        selectizeInput("SP_summ_sb","Sub Brand",choices=NULL,multiple = T,size = "75%")),
            column(2,
                   uiOutput("SP_summ_start_date_ui")),
            column(2,
                   uiOutput("SP_summ_end_date_ui")),
            # column(12,
            #        column(4,
            #               selectizeInput("SP_summ_sb","Sub Brand",choices=NULL,multiple = T)),
            #        column(3,
            #               htmlOutput("SP_summ_date_selector"))),
            column(12,
                   column(1,
                          radioButtons(
                            "SP_summ_top_selection",
                            label="KPI",
                            choices = c(
                              "Value","Volume","Price"
                            ),selected = "Value",inline = F
                          )),
                   conditionalPanel(
                     condition = "input.SP_summ_top_selection=='Value'",
                     column(6,plotlyOutput("SP_summ_value_share_plot",height = "150px")),
                     column(5,plotlyOutput("SP_summ_value_yoy_chg",height = "150px"))
                   ),
                   conditionalPanel(
                     condition = "input.SP_summ_top_selection=='Volume'",
                     column(6,plotlyOutput("SP_summ_volume_share_plot",height = "150px")),
                     column(5,plotlyOutput("SP_summ_volume_yoy_chg",height = "150px"))
                   ),
                   conditionalPanel(
                     condition = "input.SP_summ_top_selection=='Price'",
                     column(6,plotlyOutput("SP_summ_price_abs_plot",height = "200px")),
                     column(5,plotlyOutput("SP_summ_price_yoy_chg",height = "200px"))
                   )
            )
          )),
        wellPanel(  
          tabsetPanel(type="tabs",
                      tabPanel("Volume vs Price",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(6,plotlyOutput("SP_summ_bubble_chart",height = "390px"),
                                               tags$em(h6("Bubble size is based on Current year Volume Sales"))),
                                        column(6,
                                               #plotlyOutput("SP_summ_val_share_contri",height = "150px"),
                                               dataTableOutput("SP_summ_cy_ya_table",height = "400px"),style="font-size:90%;")
                                 )
                               ))),
                      tabPanel("Volume Decomposition",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(5,(dataTableOutput("SP_summ_fact_table",height = "400px")),style="font-size:95%;"),
                                        column(7,(plotlyOutput("SP_summ_volume_price_chart",height = "400px")),style="font-size:90%;")
                                        #column(4,(plotlyOutput("SP_summ_disc_dist_chart")),style="font-size:85%;")
                                 )
                               ))),
                      tabPanel("Promotion Analysis",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(4,plotlyOutput("SP_promo_split"),style="font-size:75%;"),
                                        column(8,plotlyOutput("SP_promo_WoW_split"),style="font-size:75%;"))
                                 
                               ))),
                      tabPanel("Data Table",
                               wellPanel(fluidRow(
                                 column(12,dataTableOutput("SP_summ_SKU_table"))
                                 
                               )))
                      #)
                      #)
                      
                      # column(12,
                      #        ,
                      #        column(3,actionButton("SP_generate_summ","Generate Summary"))
                      # 
                      # )
          )
        )
      ),
      tabItem(
        tabName = "SP_subMenu3",
        fluidRow(column(12,
                        column(4,h4("Promotion Effectiveness RB Brands")),
                        column(1,offset = 5,downloadButton("SP_cal_top_panel_download","",icon = icon("download"))),
                        column(1,actionButton("SP_cal_top_panel_left","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_cal_top_panel_right","",icon = icon("arrow-right"))))),
        
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(2,
                   selectizeInput("SP_cal_coun","Country",choices="UAE")),
            column(2,
                   selectizeInput("SP_cal_cust","Customer",choices="Carrefour")),
            column(2,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_cal_cat","Category",choices=NULL)),
            column(2,
                   selectizeInput("SP_cal_brand","Brand",choices=NULL,multiple = T)),
            column(2,
                   selectizeInput("SP_cal_format","Format",choices=NULL,multiple = T)),
            column(2,
                   selectizeInput("SP_cal_ppg","PPG",choices=NULL,multiple = T))
            # column(2,
            #        uiOutput("SP_cal_start_date_ui")),
            # column(2,
            #        uiOutput("SP_cal_end_date_ui"))
          )),
        wellPanel(  
          tabsetPanel(type="tabs",
                      tabPanel("ROI Analysis",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(6,(dataTableOutput("SP_promo_fact_table",height = "380px")),style="font-size:80%;",
                                               tags$em(h6("Off Invoice/Unit is considered to be same for Shipment and Scan to calculate Investment"))),
                                        column(6,
                                               radioButtons(
                                                 "SP_promo_ROI_selection",
                                                 label="",
                                                 choices = c(
                                                   "Incremental GM ROI","Incremental NR ROI","Incremental NIS ROI"
                                                 ),selected = "Incremental GM ROI",inline = T
                                               ),
                                               (plotlyOutput("SP_promo_ROI",height = "320px")),style="font-size:100%;",
                                               tags$em(h6("Bubble size is based on Incremental Sales")))
                                 )
                               ))),
                      tabPanel("Promotion Calendar",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(1,div()),
                                        column(4,(dataTableOutput("SP_promo_cal_legend")),style="font-size:75%;"),
                                        column(4,
                                               radioButtons(
                                                 "SP_promo_cal_format_selection",
                                                 label="",
                                                 choices = c(
                                                   "Promotion Type","ROI Effectiveness"
                                                 ),selected = "Promotion Type",inline = T
                                               ))
                                 ),
                                 # column(3,
                                 #        selectizeInput("SP_promo_cal_selection","Select Calendar",choices=c("RB Financial Year","Carrefour Financial Year")))),
                                 column(12,dataTableOutput("SP_promo_cal"),style="font-size:75%;",
                                        tags$em(h6("RSP corresponds to the latest week's price")))
                                 
                                 #uiOutput("event_popup"),
                                 
                               ))
                      )
                      # tabPanel("Promotion Calendar Simulation",
                      #          wellPanel(fluidRow(
                      #            column(12,
                      #                   column(4,plotlyOutput("SP_promo_analysis")),br(),br(),
                      #                   column(4,actionButton("SP_promo_price_chg","Change Promo Price"))
                      #                   #column(4,textOutput("SP_promo_price"))
                      #            )
                      #          )
                      #          )
                      # )
          )
        ),br(),br(),br(),br()),
      tabItem(
        tabName = "SP_subMenu5",
        fluidRow(column(12,
                        column(4,h4("Promotion Optimizer")),
                        #column(1,offset = 5,downloadButton("SP_opti_top_panel_download","",icon = icon("download"))),
                        column(1,offset = 6,actionButton("SP_opti_top_panel_left","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_opti_top_panel_right","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(1,
                   selectizeInput("SP_opti_coun","Country",choices="UAE")),
            column(1,
                   selectizeInput("SP_opti_cust","Customer",choices="Carrefour")),
            column(2,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_opti_cat","Category",choices=NULL)),
            column(1,
                   selectizeInput("SP_opti_brand","Brand",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_opti_format","Format",choices=NULL,multiple = T)),
            column(1,
                   selectizeInput("SP_opti_ppg","PPG",choices=NULL,multiple = T)),
            column(2,
                   selectizeInput("SP_opti_comp","Slot Selection Criterion",choices=c("Seasonality Trend","Competitor Promotion Timing", "Cannibalization- Complementary Impact"), selected = "Seasonality Trend",multiple = FALSE)),
            column(2,br(),
                   checkboxInput("SP_opti_disp_pp_const","Include Display Slots and Promo Price Constraints",FALSE))
            # column(2,br(),
            #        dropdownButton(
            #          awesomeRadio(
            #            inputId = "SP_opti_comp",
            #            label = "", 
            #            choices = c("Seasonality Trend","Competitor Promotion Timing", "Cannibalization- Complementary Impact"),
            #            selected = "Seasonality Trend",
            #            inline = FALSE
            #          ),status = "success" ,icon = icon("gear"),label = "Slot Selection Criterion",tooltip = TRUE, width = "300px"))
            
          )
        ),
        # fluidRow(
        #   column(2,div()),
        #   column(2,h4("Basic Selections")),
        #   column(4,div()),
        #   column(2,h4("What If Parameters")),
        #   column(2,div())
        # ),
        fluidRow(
          box(
            collapsible = FALSE,status = "primary", width = 6,title = "Scenario Information",solidHeader = TRUE,
            fluidRow(
              column(3,
                     selectizeInput("SP_opti_goal","Optimization Goal",choices = c("Scan Net Revenue","Gross Margin % of NR","Volume Sales","Scan Gross Sales","Incremental GM ROI","Trade Spend % of NR","Value Market Share","Trade Spend % of NIS","Gross Margin"),selected = 1)),
              column(2,
                     selectizeInput("SP_opti_sign","",choices = c("Max","Min"),selected = 1)),
              ###New Codes
              column(3,
                     numericInput("SP_opti_max_display","Max Display Slots",value = 6,min = 0)),
              column(2,
                     uiOutput("SP_opti_start_date_ui")),
              column(2,
                     uiOutput("SP_opti_end_date_ui"))
            ),
            fluidRow(
              column(5,br(),
                     helpText("Choose ROI format for Optimization")),
              column(7,
                     awesomeRadio("SP_opti_ROI_selection","",choices = c("Incremental GM ROI","Incremental NR ROI"),selected = "Incremental GM ROI",inline = TRUE))
            ),
            # fluidRow(
            #   column(12,
            #   helpText("Gross Margin % of NR and Trade Spend % of NR will always be taken in terms of Percentage scale")
            #   )
            # ),
            fluidRow(
              column(12,rHandsontableOutput("SP_opti_restrictions"),style="font-size:85%;")
            )
          ),
          box(
            collapsible = FALSE,status = "primary", width = 6,title = "Products to be excluded from Optimization",solidHeader = TRUE,
            column(12,rHandsontableOutput("SP_opti_exclude_ppg",height = "330px"),style="font-size:85%;")
          )
        ),
        # fluidRow(
        #   column(2,h4("Products Exclude")),
        #   column(4,div()),
        #   column(2,h4("Product Restrictions")),
        #   column(4,div())
        # ),
        fluidRow(
          box(
            id = "SP_box_prod_restrictions",width = 12,collapsible = TRUE,status = "primary",title = "Product Restrictions",solidHeader = TRUE,collapsed = TRUE,
            rHandsontableOutput("SP_opti_prod_restrictions",height = "200px")
          )
        ),
        
        fluidRow(
          box(
            collapsible = TRUE,status = "primary",width = 12,title = "Saved Plans",solidHeader = TRUE,collapsed = TRUE,
            dataTableOutput("SP_opti_saved_tpo",height = "200px")
          )
        ),
        fluidRow(
          column(12,
                 column(2,dropdownButton(selectizeInput("SP_opti_run_choice","Select Optimization to run",choices = c("Run LSM constrained Optimization","Run Unconstrained Optimization","Run Complete Optimization"),selected = "Run Complete Optimization"),
                                         actionButton("SP_opti_run","Confirm",style="color: #fff; background-color: #84C343"),
                                         circle = FALSE, status = "success", icon = NULL,label = "Run Optimization", tooltip = FALSE, width = 4)),
                 column(9,div()),
                 column(1,actionButton("SP_opti_reset","Reset Selections",style="color: #fff; background-color: #FF4343"))
          )
        ),br(),br(),br(),br(),br(),br(),br(),br(),br()
      ),
      tabItem(
        tabName = "SP_subMenu6",
        fluidRow(column(12,
                        column(3,h4("Optimized Promotion Calendar")),
                        column(2,
                               checkboxInput("SP_opti_op_include_exclude","Include Excluded Products",TRUE)),
                        column(2,
                               checkboxInput("SP_opti_op_include_delist","Include Delisted Products in LY",FALSE)),
                        column(2,
                               actionButton("SP_exclude_ppg_popup","Out of Scope Products"),
                               bsModal("SP_opti_financial", "Out of Scope Products", "SP_exclude_ppg_popup", size = "large",
                                       dataTableOutput("SP_opti_financial_table"))),
                        #downloadButton("SP_opti_financial_download","Download"))),
                        column(1,downloadButton("SP_opti_op_top_panel_download","",icon = icon("download"))),
                        column(1,actionButton("SP_opti_op_top_panel_left","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_opti_op_top_panel_right","",icon = icon("arrow-right"))))),
        wellPanel(  
          # fluidRow(
          #   column(12,verbatimTextOutput("SP_opti_op_text")),
          #   tags$head(tags$style("#SP_opti_op_text{color: blue;
          #                        font-size: 20px;
          #                        font-style: italic;
          #                        }"
          #                )
          #   )
          # ),
          fluidRow(
            box(
              collapsible = TRUE,status = "primary", width = 12,
              column(3,
                     selectizeInput("SP_opti_op_coun","Country",choices="UAE")),
              column(3,
                     selectizeInput("SP_opti_op_cust","Customer",choices="Carrefour")),
              column(3,
                     tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                     selectizeInput("SP_opti_op_cat","Category",choices=NULL)),
              column(3,
                     selectizeInput("SP_opti_op_brand","Brand",choices=NULL,multiple = FALSE))
            )),
          tabsetPanel(type="tabs",
                      tabPanel("RB KPI's",
                               wellPanel(fluidRow(
                                 column(12,
                                        #conditionalPanel(
                                        condition = "input.SP_opti_run_choice == 'Run Complete Optimization'",
                                        column(6,dataTableOutput("SP_const1_KPI")),
                                        column(3,dataTableOutput("SP_const1_KPI_lsm")),
                                        column(3,dataTableOutput("SP_const1_KPI_nonlsm"))
                                        #)
                                        # ,
                                        # conditionalPanel(
                                        #   condition = "input.SP_opti_run_choice == 'Run LSM constrained Optimization'",
                                        #   column(6,dataTableOutput("SP_const1_KPI")),
                                        #   column(3,dataTableOutput("SP_const1_KPI_lsm"))
                                        #   #column(3,dataTableOutput("SP_const1_KPI_nonlsm"))
                                        # ),
                                        # conditionalPanel(
                                        #   condition = "input.SP_opti_run_choice == 'Run Unconstrained Optimization'",
                                        #   column(6,dataTableOutput("SP_const1_KPI")),
                                        #   #column(3,dataTableOutput("SP_const1_KPI_lsm")),
                                        #   column(3,dataTableOutput("SP_const1_KPI_nonlsm"))
                                        # )
                                 )
                               ))
                      ),
                      tabPanel("Retailer KPI's(RB)",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(5,dataTableOutput("SP_const1_KPI_ret")),
                                        column(3,dataTableOutput("SP_const1_KPI_ret_lsm")),
                                        column(4,dataTableOutput("SP_const1_KPI_ret_nonlsm"))
                                 )
                               ))
                      ),
                      tabPanel("Category Retailer KPI's",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(5,dataTableOutput("SP_const1_KPI_cat_ret")),
                                        column(3,dataTableOutput("SP_const1_KPI_cat_ret_lsm")),
                                        column(4,dataTableOutput("SP_const1_KPI_cat_ret_nonlsm"))
                                 )
                               ))
                      )
          )
        ),
        
        # fluidRow(
        #   box(
        #     collapsible = TRUE,status = "primary", width = 12,
        #     column(12,dataTableOutput("SP_const1_KPI")
        #            #column(2,dataTableOutput("SP_opti_goal_KPI")),                   
        #            #column(12,dataTableOutput("SP_const1_KPI"))
        #            # column(2,dataTableOutput("SP_const5_KPI")),
        #            # column(2,dataTableOutput("SP_const3_KPI")),
        #            # column(2,dataTableOutput("SP_const4_KPI")),
        #            # column(2,dataTableOutput("SP_const2_KPI"))
        #     )
        #   )
        # ),
        wellPanel(  
          tabsetPanel(type="tabs",
                      tabPanel("LSM Constrained",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(1,div()),
                                        column(4,(dataTableOutput("SP_opti_cal_legend_LSM")),style="font-size:75%;"),
                                        column(2,div()),
                                        column(3,
                                               radioButtons(
                                                 "SP_opti_cal_LSM_format_selection",
                                                 label="",
                                                 choices = c(
                                                   "Promotion Type","ROI Effectiveness"
                                                 ),selected = "Promotion Type",inline = T
                                               )),
                                        # column(2,
                                        #        selectizeInput("SP_opti_cal_LSM_selection","Select Calendar",choices=c("RB","Tesco"))),
                                        
                                        #column(2,br(),actionButton("SP_opti_save_LSM","Save Optimizer Plan",style = "color:#fff; background-color: #0099DC;border-color: #0099DC"))),
                                        column(2,br(),dropdown(textInput("SP_opti_save_name_LSM","Optimizer Name"),actionButton("SP_opti_save_LSM","Confirm",style="color:#fff; background-color: #0099DC;border-color: #0099DC"),
                                                               style = "jelly",circle = FALSE, icon = NULL,label = "Save Optimizer Plan", tooltip = FALSE, width = "300px",status = "success",
                                                               animate = animateOptions(
                                                                 enter = animations$specials$rollIn,
                                                                 exit = animations$specials$rollOut
                                                               )))),
                                 column(12,dataTableOutput("SP_opti_cal_LSM"),style="font-size:75%;")
                                 
                               ))
                      ),
                      tabPanel("Unconstrained",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(1,div()),
                                        column(4,(dataTableOutput("SP_opti_cal_legend_nonLSM")),style="font-size:75%;"),
                                        column(2,div()),
                                        column(3,
                                               radioButtons(
                                                 "SP_opti_cal_nonLSM_format_selection",
                                                 label="",
                                                 choices = c(
                                                   "Promotion Type","ROI Effectiveness"
                                                 ),selected = "Promotion Type",inline = T
                                               )),
                                        # column(2,
                                        #        selectizeInput("SP_opti_cal_nonLSM_selection","Select Calendar",choices=c("RB","Tesco"))),
                                        #column(2,br(),actionButton("SP_opti_save_nonLSM","Save Optimizer Plan",style = "color:#fff; background-color: #0099DC;border-color: #0099DC"))),
                                        column(2,br(),dropdown(textInput("SP_opti_save_name_nonLSM","Optimizer Name"),actionButton("SP_opti_save_nonLSM","Confirm",style="color:#fff; background-color: #0099DC;border-color: #0099DC"),
                                                               style = "jelly",circle = FALSE, icon = NULL,label = "Save Optimizer Plan", tooltip = FALSE, width = "300px",status = "success",
                                                               animate = animateOptions(
                                                                 enter = animations$specials$rollIn,
                                                                 exit = animations$specials$rollOut
                                                               )))),
                                 column(12,dataTableOutput("SP_opti_cal_nonLSM"),style="font-size:75%;"))
                               )
                      ),
                      tabPanel("Iterations",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(6,plotlyOutput("SP_KPI_Iteration_LSM",height = "600px")),
                                        column(6,plotlyOutput("SP_KPI_Iteration_nonLSM",height = "600px"))
                                 ))
                               )
                      ),
                      tabPanel("Format Level Deep Dive",
                               wellPanel(
                                 fluidRow(
                                   column(12,
                                          column(2,
                                                 selectizeInput("SP_test2_brand","Brand",choices=NULL,multiple = FALSE)),
                                          column(2,
                                                 selectizeInput("SP_test2_format","Format",choices=NULL,multiple = T)),
                                          column(2,
                                                 selectizeInput("SP_test2_ppg","PPG",choices=NULL,multiple = T)),
                                          column(2,
                                                 selectizeInput("SP_test4_start_date","Start Date",choices=NULL,multiple = FALSE)),
                                          column(2,
                                                 selectizeInput("SP_test4_end_date","End Date",choices=NULL,multiple = FALSE))
                                   )
                                 ),
                                 fluidRow(column(12,
                                                 column(2,div()),
                                                 column(2,h4("Last Year")),
                                                 column(2,div()),
                                                 column(2,h4("LSM Constrained")),
                                                 column(2,div()),
                                                 column(2,h4("LSM Change vs LY")))),
                                 fluidRow(
                                   column(12,
                                          column(12,dataTableOutput("SP_graph_3_ly",height = "300px"),style="font-size:70%;")
                                          
                                   )),br(),
                                 fluidRow(br(),column(12,
                                                      column(2,div()),
                                                      column(2,h4("Last Year")),
                                                      column(2,div()),
                                                      column(2,h4("Unconstrained")),
                                                      column(2,div()),
                                                      column(2,h4("Unconstrained Change vs LY")))),
                                 fluidRow(
                                   column(12,
                                          column(12,dataTableOutput("SP_graph_4_ly",height = "300px"),style="font-size:70%;")
                                          
                                   ))
                               )
                      ),
                      
                      tabPanel("Product Level Deep Dive",
                               wellPanel(
                                 fluidRow(
                                   column(12,
                                          column(2,
                                                 selectizeInput("SP_test1_format","Format",choices=NULL,multiple = FALSE)),
                                          column(2,
                                                 selectizeInput("SP_test1_ppg","PPG",choices=NULL,multiple = FALSE))
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          column(6,h4("LSM Constrained"),plotlyOutput("SP_graph_1_lsm",height = "300px")),
                                          column(6,h4("Unconstrained"),plotlyOutput("SP_graph_1",height = "300px"))
                                   )),
                                 fluidRow(
                                   column(12,
                                          column(6,plotlyOutput("SP_graph_2_lsm",height = "300px")),
                                          column(6,plotlyOutput("SP_graph_2",height = "300px"))
                                   ))
                               )
                      ),
                      tabPanel("Output ROI",
                               wellPanel(
                                 fluidRow(
                                   column(12,
                                          column(3,
                                                 selectizeInput("SP_test3_cat","Category",choices=NULL)),
                                          column(2,
                                                 selectizeInput("SP_test3_brand","Brand",choices=NULL,multiple = T)),
                                          column(2,
                                                 selectizeInput("SP_test3_format","Format",choices=NULL,multiple = T)),
                                          column(2,
                                                 selectizeInput("SP_test3_ppg","PPG",choices=NULL,multiple = T))
                                   )
                                 ),
                                 fluidRow(column(12,
                                                 column(6,h4("Last Year")),
                                                 column(6,h4("LSM Constrained"))
                                 )),
                                 fluidRow(column(12,
                                                 column(4,div()),
                                                 column(5,
                                                        radioButtons(
                                                          "SP_promo_ROI_selection_lsm",
                                                          label="",
                                                          choices = c(
                                                            "Incremental GM ROI","Incremental NR ROI","Incremental NIS ROI"
                                                          ),selected = "Incremental GM ROI",inline = T
                                                        )),
                                                 column(3,div())
                                 )),
                                 fluidRow(
                                   column(12,
                                          column(6,
                                                 (plotlyOutput("SP_graph_roi_lsm_ly",height = "320px")),style="font-size:100%;",
                                                 tags$em(h6("Bubble size is based on Incremental Sales"))),
                                          column(6,
                                                 (plotlyOutput("SP_graph_roi_lsm_op",height = "320px")),style="font-size:100%;",
                                                 tags$em(h6("Bubble size is based on Incremental Sales")))
                                   )),
                                 fluidRow(column(12,
                                                 column(6,h4("Last Year")),
                                                 column(6,h4("Unconstrained"))
                                 )),
                                 fluidRow(column(12,
                                                 column(4,div()),
                                                 column(5,
                                                        radioButtons(
                                                          "SP_promo_ROI_selection_nonlsm",
                                                          label="",
                                                          choices = c(
                                                            "Incremental GM ROI","Incremental NR ROI","Incremental NIS ROI"
                                                          ),selected = "Incremental GM ROI",inline = T
                                                        )),
                                                 column(3,div())
                                 )),
                                 fluidRow(
                                   column(12,
                                          column(6,
                                                 (plotlyOutput("SP_graph_roi_nonlsm_ly",height = "320px")),style="font-size:100%;",
                                                 tags$em(h6("Bubble size is based on Incremental Sales"))),
                                          column(6,
                                                 (plotlyOutput("SP_graph_roi_nonlsm_op",height = "320px")),style="font-size:100%;",
                                                 tags$em(h6("Bubble size is based on Incremental Sales")))
                                   ))
                               )
                      )       
          )),br(),br()
      ),
      tabItem(
        tabName = "SP_subMenu7",
        fluidRow(column(12,
                        column(4,h4("Event Simulator")),
                        #column(1,offset =5,downloadButton("SP_sim_top_panel_download","",icon = icon("download"))),
                        column(1,offset =6,actionButton("SP_sim_top_panel_left","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_sim_top_panel_right","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(1,
                   selectizeInput("SP_sim_coun","Country",choices="UAE")),
            column(1,
                   selectizeInput("SP_sim_cust","Customer",choices="Carrefour")),
            column(2,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_sim_cat","Category",choices=NULL)),
            column(2,
                   selectizeInput("SP_sim_brand","Brand",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_sim_format","Format",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_sim_ppg","PPG",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_sim_event","Promo Slots",choices=NULL,multiple = FALSE))
          )
        ),
        fluidRow(
          box(
            collapsible = FALSE,status = "primary", width = 12,title = "Event to be replaced",solidHeader = TRUE,
            column(12,dataTableOutput("SP_sim_event_selected_table",height = "100px"))
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,title = "Display Events Planned",solidHeader = TRUE,collapsed = TRUE,id = "slot_display_events",
            column(12,dataTableOutput("SP_sim_event_slot_display_table",height = "200px"))
          )
        ),
        fluidRow(
          box(
            collapsible = FALSE,status = "primary", width = 12,title = "Alternative Events",solidHeader = TRUE,
            fluidRow(
              column(12,
                div(style = "overflow-x: auto; overflow-y: visible; width: 100%;",
                  rHandsontableOutput("SP_sim_event_table",height = "400px")
                )
              )
            ),br(),
            fluidRow(column(12,column(2,actionButton("SP_sim_run","Run Simulation",style="color: #fff; background-color: #84C343")),
                            column(8,div()),
                            column(2,actionButton("SP_sim_reset","Reset",style="color: #fff; background-color: #FF4343"))))
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,title = "Simulator Outcomes",solidHeader = TRUE,collapsed = TRUE,id = "scenario_analysis",
            fluidRow(
              column(12,h4("Promo Effectivenss (Event)"),
                     rHandsontableOutput("SP_sim_run_outcomes_table",height = "200px"),style="font-size:90%;")
            ),
            fluidRow(
              column(12,h4("Promo Effectivenss (PPG)"),
                     plotlyOutput("SP_sim_run_outcomes_plot",height = "200px"))
            )
          )
        ),
        
        fluidRow(
          column(9,div()),
          #column(1,actionButton("SP_sim_reset","Reset",style="color: #fff; background-color: #FF4343")),
          #column(2,actionButton("SP_sim_run","Run Simulation")),
          column(3,actionButton("SP_sim_replace","Replace Event",style = "color:#fff; background-color: #0099DC;border-color: #0099DC"),
                 bsModal("SP_sim_replace_modal", "Comparison between selected event and replaced event(Brand Level Metrics)", "SP_sim_replace", size = "large",
                         verbatimTextOutput("SP_sim_LSM_Violated"),
                         tags$head(tags$style("#SP_sim_LSM_Violated{color: red;
                                              font-size: 14px;
                                              font-style: italic;
                                              }"
                         )
                         ),
                         dataTableOutput("SP_sim_replace_compare_table"),br(),actionButton("SP_sim_replace_confirm","Confirm"))
                         )
                         )
                 ),
      tabItem(
        tabName = "SP_subMenu8",
        fluidRow(column(12,
                        column(4,h4("Compare Scenarios")),
                        column(1,offset =5,downloadButton("SP_cmp_scn_op_top_panel_download","",icon = icon("download"))),
                        column(1,actionButton("SP_cmp_scn_op_top_panel_left","",icon = icon("arrow-left"))),
                        column(1,div()))),
        #column(1,actionButton("SP_cmp_scn_op_top_panel_right","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(2,
                   selectizeInput("SP_cmp_scn_coun","Country",choices="UAE")),
            column(2,
                   selectizeInput("SP_cmp_scn_cust","Customer",choices="Carrefour")),
            column(2,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_cmp_scn_cat","Category",choices=NULL)),
            column(1,
                   selectizeInput("SP_cmp_scn_brand","Brand",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_cmp_scn_format","Format",choices=NULL,multiple = FALSE)),
            column(1,
                   selectizeInput("SP_cmp_scn_ppg","PPG",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_cmp_scn_tpo","TPO ID",choices=NULL,multiple = FALSE))
          )
        ),
        fluidRow(column(12,
                        dataTableOutput("SP_cmp_scn_table"),style="font-size:100%;")),
        fluidRow(column(12,br(),
                        column(2,actionButton("SP_cmp_scn_merge","Merge Plans"),
                               bsModal("SP_merge_popup", "Select plans to be merged", "SP_cmp_scn_merge", size = "large",
                                       rHandsontableOutput("SP_cmp_scn_merge_table"),br(),
                                       dropdown(textInput("SP_opti_save_name_merge","Optimizer Name"),actionButton("SP_cmp_scn_merge_name","Confirm",style="color:#fff; background-color: #0099DC;border-color: #0099DC"),
                                                style = "jelly",circle = FALSE, icon = NULL,label = "Save Merged Plan", tooltip = FALSE, width = "300px",status = "success",
                                                animate = animateOptions(
                                                  enter = animations$specials$rollIn,
                                                  exit = animations$specials$rollOut
                                                )))
                        ))),
        br(),br(),br()
      ),
      tabItem(
        tabName = "SP_subMenu9",
        fluidRow(column(12,
                        column(4,h4("KAM Cockpit")),
                        #column(1,offset = 5,downloadButton("SP_kam_top_panel_download","",icon = icon("download"))),
                        column(1,offset = 6,actionButton("SP_kam_top_panel_left","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_kam_top_panel_right","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(2,
                   selectizeInput("SP_kam_coun","Country",choices="UAE")),
            column(2,
                   selectizeInput("SP_kam_cust","Customer",choices="Carrefour")),
            column(2,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_kam_cat","Category",choices=NULL)),
            column(2,
                   selectizeInput("SP_kam_brand","Brand",choices=NULL,multiple = F)),
            column(2,
                   selectizeInput("SP_kam_format","Format",choices=NULL,multiple = F)),
            column(2,
                   selectizeInput("SP_kam_tpo","TPO ID",choices=NULL,multiple = F))
          )),
        fluidRow(
          box(
            collapsible = FALSE,status = "primary", width = 6,title = "ePOS based RB P&L",solidHeader = TRUE,
            fluidRow(
              column(12,dataTableOutput("SP_kam_RB_table",height = "300px"),style="font-size:90%;")
            ),br(),
            fluidRow(
              column(12,plotlyOutput("SP_kam_RB_chart",height = "300px"))
            )
          ),
          box(
            collapsible = FALSE,status = "primary", width = 6,title = "Customer P&L",solidHeader = TRUE,
            fluidRow(
              column(12,dataTableOutput("SP_kam_cust_table",height = "300px"),style="font-size:90%;")
            ),br(),
            fluidRow(
              column(12,plotlyOutput("SP_kam_cust_chart",height = "300px"))
            )
          )
        ),
        
        br(),br(),br(),br(),br(),br(),br(),br(),br()
      ),
      tabItem(
        tabName = "SP_subMenu10",
        fluidRow(column(12,
                        column(4,h4("Promotion Optimizer (Ongoing)")),
                        #column(1,offset = 5,downloadButton("SP_opti_top_panel_download_on","",icon = icon("download"))),
                        column(1,offset = 6,actionButton("SP_opti_top_panel_left_on","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_opti_top_panel_right_on","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(1,
                   selectizeInput("SP_opti_coun_on","Country",choices="UAE")),
            column(1,
                   selectizeInput("SP_opti_cust_on","Customer",choices="Carrefour")),
            column(1,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_opti_cat_on","Category",choices=NULL)),
            column(1,
                   selectizeInput("SP_opti_brand_on","Brand",choices=NULL,multiple = FALSE)),
            column(1,
                   selectizeInput("SP_opti_format_on","Format",choices=NULL,multiple = TRUE)),
            column(1,
                   selectizeInput("SP_opti_ppg_on","PPG",choices=NULL,multiple = TRUE)),
            column(2,
                   selectizeInput("SP_opti_tpo_on","TPO ID",choices=NULL)),
            column(2,
                   selectizeInput("SP_opti_comp_on","Slot Selection Criterion",choices=c("Seasonality Trend","Competitor Promotion Timing", "Cannibalization- Complementary Impact"), selected = "Seasonality Trend",multiple = FALSE)),
            column(2,br(),
                   checkboxInput("SP_opti_disp_pp_const_on","Include Display Slots and Promo Price Constraints",FALSE))
            
          )
        ),
        fluidRow(
          box(
            collapsible = FALSE,status = "primary", width = 6,title = "Scenario Information",solidHeader = TRUE,
            fluidRow(
              column(3,
                     selectizeInput("SP_opti_goal_on","Optimization Goal",choices = c("Scan Net Revenue","Gross Margin % of NR","Volume Sales","Scan Gross Sales","Incremental GM ROI","Incremental NR ROI","Gross Margin","Trade Spend % of NIS","Trade Spend % of NR","Value Market Share"),selected = 1)),
              column(2,
                     selectizeInput("SP_opti_sign_on","",choices = c("Max","Min"),selected = 1)),
              ###New Codes
              column(3,
                     numericInput("SP_opti_max_display_on","Max Display Slots",value = 6,min = 0)),
              # column(4,
              #        dateRangeInput("SP_opti_date_on", "Optimization Date Range",start = Sys.Date() + 42, end = "2019-12-31",min = Sys.Date() + 42, max = "2019-12-31", format = "yyyy-dd-mm")
              # )
              column(2,
                     selectizeInput("SP_opti_date_on_start","Start Date",choices = "",selected = "")),
              column(2,
                     selectizeInput("SP_opti_date_on_end","End Date",choices = "",selected = ""))
            ),
            fluidRow(
              column(4,br(),
                     helpText("Choose ROI for Optimization")),
              column(8,
                     awesomeRadio("SP_opti_ROI_selection_on","",choices = c("Incremental GM ROI","Incremental NR ROI"),selected = "Incremental GM ROI",inline = TRUE))
            ),
            # fluidRow(
            #   column(12,
            #   helpText("Gross Margin % of NR and Trade Spend % of NR will always be taken in terms of Percentage scale")
            #   )
            # ),
            fluidRow(
              column(12,rHandsontableOutput("SP_opti_restrictions_on"),style="font-size:85%;")
            )
          ),
          box(
            collapsible = FALSE,status = "primary", width = 6,title = "Products to be excluded from Optimization",solidHeader = TRUE,
            column(12,rHandsontableOutput("SP_opti_exclude_ppg_on",height = "330px"),style="font-size:85%;")
          )
        ),
        fluidRow(
          box(
            id = "SP_box_prod_restrictions_on",width = 12,collapsible = TRUE,status = "primary",title = "Product Restrictions",solidHeader = TRUE,collapsed = TRUE,
            rHandsontableOutput("SP_opti_prod_restrictions_on",height = "200px")
          )
        ),
        
        fluidRow(
          box(
            collapsible = TRUE,status = "primary",width = 12,title = "Saved Plans",solidHeader = TRUE,collapsed = TRUE,
            dataTableOutput("SP_opti_saved_tpo_on",height = "200px")
          )
        ),
        fluidRow(
          column(2,actionButton("SP_opti_run_on","Run Optimization",style="color: #fff; background-color: #84C343")),
          column(9,div()),
          column(1,actionButton("SP_opti_reset_on","Reset Selections",style="color: #fff; background-color: #FF4343"))
        ),br(),br(),br(),br(),br(),br()
      ),
      tabItem(
        tabName = "SP_subMenu11",
        fluidRow(column(12,
                        column(4,h4("Optimized Promotion Calendar (Ongoing)")),
                        column(2,offset = 1,
                               checkboxInput("SP_opti_op_include_exclude_on","Include Excluded Products",TRUE)),
                        column(2,
                               actionButton("SP_exclude_ppg_popup_on","Out of Scope Products"),
                               bsModal("SP_opti_financial_on", "Out of Scope Products", "SP_exclude_ppg_popup_on", size = "large",
                                       dataTableOutput("SP_opti_financial_table_on"))),
                        #downloadButton("SP_opti_financial_download_on","Download"))),
                        column(1,downloadButton("SP_opti_op_top_panel_download_on","",icon = icon("download"))),
                        column(1,actionButton("SP_opti_op_top_panel_left_on","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_opti_op_top_panel_right_on","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(3,
                   selectizeInput("SP_opti_op_coun_on","Country",choices="UAE")),
            column(3,
                   selectizeInput("SP_opti_op_cust_on","Customer",choices="Carrefour")),
            column(3,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_opti_op_cat_on","Category",choices=NULL)),
            column(3,
                   selectizeInput("SP_opti_op_brand_on","Brand",choices=NULL,multiple = FALSE))
          )),
        tabsetPanel(type="tabs",
                    tabPanel("RB KPI's",
                             wellPanel(fluidRow(
                               column(12,
                                      column(12,dataTableOutput("SP_const1_KPI_tbo"))
                                      #column(3,dataTableOutput("SP_const1_KPI_lsm_tbo")),
                                      #column(4,dataTableOutput("SP_const1_KPI_nonlsm_tbo"))
                               )
                             ))
                    ),
                    tabPanel("Retailer KPI's(RB)",
                             wellPanel(fluidRow(
                               column(12,
                                      column(12,dataTableOutput("SP_const1_KPI_ret_tbo"))
                                      #column(3,dataTableOutput("SP_const1_KPI_ret_lsm_tbo")),
                                      #column(4,dataTableOutput("SP_const1_KPI_ret_nonlsm_tbo"))
                               )
                             ))
                    ),
                    tabPanel("Category Retailer KPI's",
                             wellPanel(fluidRow(
                               column(12,
                                      column(12,dataTableOutput("SP_const1_KPI_cat_ret_tbo"))
                                      #column(3,dataTableOutput("SP_const1_KPI_cat_ret_lsm_tbo")),
                                      #column(4,dataTableOutput("SP_const1_KPI_cat_ret_nonlsm_tbo"))
                               )
                             ))
                    )
        ),
        wellPanel(  
          tabsetPanel(type="tabs",
                      tabPanel("Optimized Plan",
                               wellPanel(fluidRow(
                                 column(12,
                                        column(1,div()),
                                        column(4,(dataTableOutput("SP_opti_cal_legend_tbo")),style="font-size:75%;"),
                                        column(2,div()),
                                        column(3,
                                               radioButtons(
                                                 "SP_opti_cal_format_selection_tbo",
                                                 label="",
                                                 choices = c(
                                                   "Promotion Type","ROI Effectiveness"
                                                 ),selected = "Promotion Type",inline = T
                                               )),
                                        #column(2,br(),actionButton("SP_opti_save_tbo","Save Optimizer Plan",style = "color:#fff; background-color: #0099DC;border-color: #0099DC"))),
                                        column(2,br(),dropdown(textInput("SP_opti_save_name_tbo","Optimizer Name"),actionButton("SP_opti_save_tbo","Confirm",style="color:#fff; background-color: #0099DC;border-color: #0099DC"),
                                                               style = "jelly",circle = FALSE, icon = NULL,label = "Save Optimizer Plan", tooltip = FALSE, width = "300px",status = "success",
                                                               animate = animateOptions(
                                                                 enter = animations$specials$rollIn,
                                                                 exit = animations$specials$rollOut
                                                               )))),
                                 column(12,dataTableOutput("SP_opti_cal_tbo"),style="font-size:75%;")
                                 
                               ))
                      ),
                      # tabPanel("Iterations",
                      #          wellPanel(fluidRow(
                      #            column(12,
                      #                   plotlyOutput("SP_KPI_Iteration_tbo",height = "600px")
                      #            ))
                      #          )
                      # ),
                      tabPanel("Format Level Deep Dive",
                               wellPanel(
                                 fluidRow(
                                   column(12,
                                          column(2,
                                                 selectizeInput("SP_test2_brand_on","Brand",choices=NULL,multiple = FALSE)),
                                          column(2,
                                                 selectizeInput("SP_test2_format_on","Format",choices=NULL,multiple = T)),
                                          column(2,
                                                 selectizeInput("SP_test2_ppg_on","PPG",choices=NULL,multiple = T)),
                                          column(2,
                                                 selectizeInput("SP_test4_start_date_on","Start Date",choices=NULL,multiple = FALSE)),
                                          column(2,
                                                 selectizeInput("SP_test4_end_date_on","End Date",choices=NULL,multiple = FALSE))
                                   )
                                 ),
                                 fluidRow(column(12,
                                                 column(2,div()),
                                                 column(2,h4("Finalized Plan")),
                                                 column(2,div()),
                                                 column(2,h4("Optimized(Ongoing)")),
                                                 column(2,div()),
                                                 column(2,h4("Optimized(Ongoing) vs Finalized Plan")))),
                                 fluidRow(
                                   column(12,
                                          column(12,dataTableOutput("SP_graph_3_tbo",height = "300px"),style="font-size:70%;")
                                          
                                   ))
                               )
                      ),
                      tabPanel("Product Level Deep Dive",
                               wellPanel(
                                 fluidRow(
                                   column(12,
                                          column(2,
                                                 selectizeInput("SP_test1_format_on","Format",choices=NULL,multiple = FALSE)),
                                          column(2,
                                                 selectizeInput("SP_test1_ppg_on","PPG",choices=NULL,multiple = FALSE))
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          column(6,h4("Finalized Plan"),plotlyOutput("SP_graph_1_final",height = "300px")),
                                          column(6,h4("Optimized(Ongoing)"),plotlyOutput("SP_graph_1_tbo",height = "300px"))
                                          
                                   )),
                                 fluidRow(
                                   column(12,
                                          column(6,plotlyOutput("SP_graph_2_final",height = "300px")),
                                          column(6,plotlyOutput("SP_graph_2_tbo",height = "300px"))
                                   ))
                               )
                      ),
                      tabPanel("Output ROI",
                               wellPanel(
                                 fluidRow(
                                   column(12,
                                          column(3,
                                                 selectizeInput("SP_test3_cat_on","Category",choices=NULL)),
                                          column(2,
                                                 selectizeInput("SP_test3_brand_on","Brand",choices=NULL,multiple = T)),
                                          column(2,
                                                 selectizeInput("SP_test3_format_on","Format",choices=NULL,multiple = T)),
                                          column(2,
                                                 selectizeInput("SP_test3_ppg_on","PPG",choices=NULL,multiple = T))
                                   )
                                 ),
                                 fluidRow(column(12,
                                                 column(2,div()),
                                                 column(3,h4("Finalized Plan")),
                                                 column(3,div()),
                                                 column(3,h4("Optimized(Ongoing)")),
                                                 column(1,div())
                                 )),
                                 fluidRow(column(12,
                                                 column(4,div()),
                                                 column(5,
                                                        radioButtons(
                                                          "SP_promo_ROI_selection_tbo",
                                                          label="",
                                                          choices = c(
                                                            "Incremental GM ROI","Incremental NR ROI","Incremental NIS ROI"
                                                          ),selected = "Incremental GM ROI",inline = T
                                                        )),
                                                 column(3,div())
                                 )),
                                 fluidRow(
                                   column(12,
                                          column(6,
                                                 (plotlyOutput("SP_graph_roi_tbo_ly",height = "320px")),style="font-size:100%;",
                                                 tags$em(h6("Bubble size is based on Incremental Sales"))),
                                          column(6,
                                                 (plotlyOutput("SP_graph_roi_tbo_op",height = "320px")),style="font-size:100%;",
                                                 tags$em(h6("Bubble size is based on Incremental Sales")))
                                   ))
                               )
                      )
                      
          )),br(),br()
      ),
      tabItem(
        tabName = "SP_subMenu12",
        fluidRow(column(12,
                        column(4,h4("Event Simulator(Ongoing)")),
                        column(1,offset =5,downloadButton("SP_sim_top_panel_download_on","",icon = icon("download"))),
                        column(1,actionButton("SP_sim_top_panel_left_on","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_sim_top_panel_right_on","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(1,
                   selectizeInput("SP_sim_coun_on","Country",choices="UAE")),
            column(1,
                   selectizeInput("SP_sim_cust_on","Customer",choices="Carrefour")),
            column(2,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_sim_cat_on","Category",choices=NULL)),
            column(2,
                   selectizeInput("SP_sim_brand_on","Brand",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_sim_format_on","Format",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_sim_ppg_on","PPG",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_sim_event_on","Promo Slots",choices=NULL,multiple = FALSE))
          )
        ),
        fluidRow(
          box(
            collapsible = FALSE,status = "primary", width = 12,title = "Event to be replaced",solidHeader = TRUE,
            column(12,dataTableOutput("SP_sim_event_selected_table_on",height = "100px"))
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,title = "Display Events Planned",solidHeader = TRUE,collapsed = TRUE,id = "slot_display_events_on",
            column(12,dataTableOutput("SP_sim_event_slot_display_table_on",height = "200px"))
          )
        ),
        fluidRow(
          box(
            collapsible = FALSE,status = "primary", width = 12,title = "Alternative Events",solidHeader = TRUE,
            fluidRow(
              column(12,column(12,rHandsontableOutput("SP_sim_event_table_on",height = "400px",width = "2000px")))),br(),
            fluidRow(column(12,column(2,actionButton("SP_sim_run_on","Run Simulation",style="color: #fff; background-color: #84C343")),
                            column(8,div()),
                            column(2,actionButton("SP_sim_reset_on","Reset",style="color: #fff; background-color: #FF4343"))))
          )
        ),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,title = "Simulator Outcomes",solidHeader = TRUE,collapsed = TRUE,id = "scenario_analysis_on",
            fluidRow(
              column(12,h4("Promo Effectivenss (Event)"),
                     rHandsontableOutput("SP_sim_run_outcomes_table_on",height = "200px"))
            ),
            fluidRow(
              column(12,h4("Promo Effectivenss (PPG)"),
                     plotlyOutput("SP_sim_run_outcomes_plot_on",height = "200px"))
              
            )
          )
        ),
        
        fluidRow(
          column(9,div()),
          #column(1,actionButton("SP_sim_reset","Reset",style="color: #fff; background-color: #FF4343")),
          #column(2,actionButton("SP_sim_run","Run Simulation")),
          column(3,actionButton("SP_sim_replace_on","Replace Event",style = "color:#fff; background-color: #0099DC;border-color: #0099DC"),
                 bsModal("SP_sim_replace_modal_on", "Comparison between selected event and replaced event(Brand Level Metrics)", "SP_sim_replace_on", size = "large",
                         verbatimTextOutput("SP_sim_LSM_Violated_on"),
                         tags$head(tags$style("#SP_sim_LSM_Violated_on{color: red;
                                              font-size: 14px;
                                              font-style: italic;
                                              }"
                         )
                         ),
                         dataTableOutput("SP_sim_replace_compare_table_on"),br(),actionButton("SP_sim_replace_confirm_on","Confirm"))
                         )
                         )
                 ),
      tabItem(
        tabName = "SP_subMenu13",
        fluidRow(column(12,
                        column(4,h4("Compare Scenarios(Ongoing)")),
                        column(1,offset =5,downloadButton("SP_cmp_scn_op_top_panel_download_on","",icon = icon("download"))),
                        column(1,actionButton("SP_cmp_scn_op_top_panel_left_on","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_cmp_scn_op_top_panel_right_on","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(2,
                   selectizeInput("SP_cmp_scn_coun_on","Country",choices="UAE")),
            column(2,
                   selectizeInput("SP_cmp_scn_cust_on","Customer",choices="Carrefour")),
            # column(2,
            #        tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
            #        selectizeInput("SP_cmp_scn_cat_on","Category",choices=NULL)),
            column(2,
                   selectizeInput("SP_cmp_scn_brand_on","Brand",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_cmp_scn_base_tpo","Base TPO",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_cmp_scn_tpo_on_1","TPO ID 1",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_cmp_scn_tpo_on_2","TPO ID 2",choices=NULL,multiple = FALSE))
          )
        ),
        fluidRow(column(12,br(),
                        dataTableOutput("SP_cmp_scn_table_kpi"),style="font-size:100%;")),
        
        fluidRow(column(12,
                        plotlyOutput("SP_cmp_scn_chart_on"),style="font-size:90%;")),
        
        br(),br(),br()
      ),
      tabItem(
        tabName = "SP_subMenu14",
        fluidRow(column(12,
                        column(4,h4("Actions Required")),
                        column(1,offset =5,downloadButton("SP_act_req_op_top_panel_download","",icon = icon("download"))),
                        column(1,actionButton("SP_act_req_op_top_panel_left","",icon = icon("arrow-left"))),
                        column(1,actionButton("SP_act_req_op_top_panel_right","",icon = icon("arrow-right"))))),
        fluidRow(
          box(
            collapsible = TRUE,status = "primary", width = 12,
            column(2,
                   tags$style(type='text/css',".selectize-input {font-size:12px;line-height:10px;} .selectize-dropdown {font-size:10px;line-height:10px;}"),
                   selectizeInput("SP_act_req_cat","Category",choices=NULL)),
            column(2,
                   selectizeInput("SP_act_req_brand","Brand",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_act_req_format","Format",choices=NULL,multiple = FALSE)),
            column(2,
                   selectizeInput("SP_act_req_ppg","PPG",choices=NULL,multiple = FALSE)),
            column(4,
                   dateRangeInput("SP_act_req_date","Optimization Date Range",start = "2019-12-01", end = "2019-12-31",min = "2019-12-01", max = "2019-12-31", format = "yyyy-dd-mm")
            )
          )
        ),
        fluidRow(column(12,
                        dataTableOutput("SP_act_req_table"),style="font-size:100%;")),
        fluidRow(column(12,
                        dataTableOutput("SP_act_req_cal"),style="font-size:100%;")),
        br(),br(),br()
      )
          )
        ),
  #- footer
     #---------------- FOOTER ----------------#
    tags$footer(
      style = "
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: #222d32;
        color: white;
        text-align: center;
        padding: 6px;
        z-index: 1000;
      ",
      uiOutput("dynamicFooter")
    )
  )
)
,
  
  #- controlbar
  controlbar = NULL
        )

