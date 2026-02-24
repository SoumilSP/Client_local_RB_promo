################ Clear The Environment ##############
rm(list=ls())
#--------------------------install required packages---------------------------#
# install.packages("devtools")
# library(devtools)
# install_github("gyang274/shinydashboard")


#------------------------------------------------------------------------------#
#------------------------------------ init ------------------------------------#
#------------------------------------------------------------------------------#
#update.packages(ask = FALSE)
if(!require('shiny'))install.packages('shiny')
if(!require('shinydashboard'))install.packages('shinydashboard')
if(!require('shinythemes'))install.packages('shinythemes')
if(!require('readxl'))install.packages('readxl')
if(!require('shinyFiles'))install.packages('shinyFiles')
if(!require('shinyBS'))install.packages('shinyBS')
if(!require('caret'))install.packages('caret')
if(!require('stringr'))install.packages('stringr')
if(!require('plotly'))install.packages('plotly')
if(!require('shinyjs'))install.packages('shinyjs')
if(!require('magrittr'))install.packages('magrittr')
if(!require('data.table'))install.packages('data.table')
if(!require('DT'))install.packages('DT')
if(!require('dplyr'))install.packages('dplyr')
if(!require('scales'))install.packages('scales')
if(!require('lubridate'))install.packages('lubridate')
if(!require('xlsx'))install.packages('xlsx')
if(!require('shinyWidgets'))install.packages('shinyWidgets')
if(!require('grid'))install.packages('grid')
if(!require('gtools'))install.packages('gtools')
if(!require('rhandsontable'))install.packages('rhandsontable')
if(!require('bit64'))install.packages('bit64')
if(!require('tidyr'))install.packages('tidyr')
if(!require('shinyBS'))install.packages('shinyBS')
if(!require('openxlsx'))install.packages('openxlsx')
if(!require('snow'))install.packages('snow')
if(!require('doParallel'))install.packages('doParallel')
if(!require('foreach'))install.packages('foreach')
if(!require('miscTools'))install.packages('miscTools')
if(!require('parallel'))install.packages('parallel')
if(!require('zoo'))install.packages('zoo')
if(!require('reshape2'))install.packages('reshape2')

#####source all the functions
source("data_prep_event_list.R")
source("Competitor_seq.R")
source("annual_optimization.R")
source("ongoing_optimization.R")

column_header_mapping <- function(data,sheet){
  input_col_mapping <- data.table(read_excel(paste(getwd(),"/Column_Mapping.xlsx",sep = ""),sheet = sheet))
  input_data_header <- data.frame("Input Data Names" = names(data),check.names = FALSE)
  input_data_header <- left_join(input_data_header,input_col_mapping,by = "Input Data Names")
  names(data) <- input_data_header$`Code Names`
  return(data)
}

###Summary tab plots
value_share_plot <- function(data,prod_level,brand){
  data <- data[order(data$CY_Volume),]
  data[[prod_level]] <- factor(data[[prod_level]], levels = unique(data[[prod_level]]))
  plot_ly(data, x = ~`val_share`, y = brand,color= ~get(prod_level),type = "bar",
          hoverinfo = 'text',hovertext = paste0(data[[prod_level]],": ",round(data$CY_Value/10^6,2),"Mn GBP; ",round(data$val_share,2),"%"),
          text = paste0(round(data$val_share,2),"%"),textposition = 'auto',textfont = list(color = '#000000')
  )%>%
    layout(title = paste0(prod_level," Value Share Contribution"),showlegend = FALSE,paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',font = list(size =10),xaxis = list(title = "% Value Share",showgrid = FALSE),yaxis = list(
      title = prod_level,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    ), barmode = "stack") %>% config(displayModeBar = FALSE)
}

value_yoy_chg_plot <- function(data,data_agg,prod_level,prod_level_agg){
  #data$color <- data[[prod_level]]
  data <- data[order(-data$CY_Volume),]
  data[[prod_level]] <- factor(data[[prod_level]], levels = unique(data[[prod_level]]))
  data$total_val_chg_contri_flag <- ifelse(data$total_val_chg_contri >= 0 , "rgb(132,195,67)","rgb(255,67,67)")
  data_agg[[prod_level_agg]] <- factor(data_agg[[prod_level_agg]], levels = unique(data_agg[[prod_level_agg]]))
  data_agg$val_chg_flag <- ifelse(data_agg$val_chg >= 0,"rgb(132,195,67)","rgb(255,67,67)")
  plot_ly(data) %>%
    add_trace(x=data_agg[[prod_level_agg]],y=data_agg$`val_chg`, type = 'bar',
              hoverinfo = 'text',
              marker = list(color =~data_agg$val_chg_flag),
              #color =data_agg$val_chg_flag,
              text = paste0(round(data_agg$val_chg,2),"%"),textposition = "auto",textfont = list(color = '#000000')) %>%
    add_trace(x=~get(prod_level),y=~`total_val_chg_contri`, type = 'bar',
              hoverinfo = 'text',
              marker = list(color =~data$total_val_chg_contri_flag),
              #color =data$total_val_chg_contri_flag,
              text = paste0(round(data$`total_val_chg_contri`,2),"%"),textposition = "auto",textfont = list(color = '#000000')) %>%
    layout(showlegend = FALSE,title = "YoY % Change",paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',font = list(size =10),xaxis = list(title = c(prod_level_agg,prod_level),tickfont = list(size =8)), yaxis = list(
      title = "",
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )) %>% config(displayModeBar = FALSE)
}

volume_share_plot <- function(data,prod_level){
  data <- data[order(data$CY_Volume),]
  data[[prod_level]] <- factor(data[[prod_level]], levels = unique(data[[prod_level]]))
  plot_ly(data, x = ~`vol_share`, color= ~get(prod_level),type = "bar",
          hoverinfo = 'text',hovertext = paste0(data[[prod_level]],": ",round(data$CY_Volume/10^3,2),"('000 Units); ",round(data$vol_share,2),"%"),
          text = paste0(round(data$vol_share,2),"%"),textposition = 'auto',textfont = list(color = '#000000')
  )%>%
    layout(title = paste0(prod_level," Volume Share Contribution"),showlegend = FALSE,paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',font = list(size =10),xaxis = list(title = "% Volume Share",showgrid = FALSE),yaxis = list(
      title = prod_level,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    ), barmode = "stack") %>% config(displayModeBar = FALSE)
}

volume_yoy_chg_plot <- function(data,data_agg,prod_level,prod_level_agg){
  data <- data[order(-data$CY_Volume),]
  data[[prod_level]] <- factor(data[[prod_level]], levels = unique(data[[prod_level]]))
  data$total_vol_chg_contri_flag <- ifelse(data$total_vol_chg_contri >= 0 , "rgb(132,195,67)","rgb(255,67,67)")
  data_agg[[prod_level_agg]] <- factor(data_agg[[prod_level_agg]], levels = unique(data_agg[[prod_level_agg]]))
  data_agg$vol_chg_flag <- ifelse(data_agg$vol_chg >= 0,"rgb(132,195,67)","rgb(255,67,67)")
  plot_ly(data) %>%
    add_trace(x=data_agg[[prod_level_agg]],y=data_agg$`vol_chg`, type = 'bar',
              hoverinfo = 'text',
              #color =data_agg[[prod_level_agg]],
              marker = list(color =~data_agg$vol_chg_flag),
              text = paste0(round(data_agg$vol_chg,2),"%"),textposition = "auto",textfont = list(color = '#000000')) %>%
    add_trace(x=~get(prod_level),y=~`total_vol_chg_contri`, type = 'bar',
              hoverinfo = 'text',
              #color =~get(prod_level),
              marker = list(color =~data$total_vol_chg_contri_flag),
              text = paste0(round(data$`total_vol_chg_contri`,2),"%"),textposition = "auto",textfont = list(color = '#000000')) %>%
    layout(showlegend = FALSE,title = "YoY % Change",paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',font = list(size =10),xaxis = list(title = c(prod_level_agg,prod_level),tickfont = list(size =8)), yaxis = list(
      title = "",
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )) %>% config(displayModeBar = FALSE)
}

price_abs_plot <- function(data,data_agg,prod_level,prod_level_agg){
  
  data <- data[order(data$CY_Volume),]
  data[[prod_level]] <- factor(data[[prod_level]], levels = unique(data[[prod_level]]))
  data_agg[[prod_level_agg]] <- factor(data_agg[[prod_level_agg]], levels = unique(data_agg[[prod_level_agg]]))
  plot_ly(data) %>%
    add_trace(x=~CY_Price,y=~get(prod_level),orientation = 'h',
              hoverinfo = 'text',
              color = ~get(prod_level), type = 'bar',text = round(data$CY_Price,2),textposition = "auto",textfont = list(color = '#000000')) %>%
    add_trace(x=data_agg$CY_Price,y=data_agg[[prod_level_agg]],orientation = 'h',
              hoverinfo = 'text',
              color = data_agg[[prod_level_agg]], type = 'bar',text = round(data_agg$CY_Price,2),textposition = "auto",textfont = list(color = '#000000')) %>%
    layout(title = "Average Price plot",margin= list(l = 100),paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',font = list(size =10),showlegend = FALSE,xaxis = list(title = "Price",showgrid = FALSE), yaxis = list(
      title = c(prod_level_agg,prod_level),
      showgrid = FALSE,
      tickfont = list(size =8)
    )) %>% config(displayModeBar = FALSE)
}

price_yoy_chg_plot <- function(data,data_agg,prod_level,prod_level_agg){
  data <- data[order(-data$CY_Volume),]
  data[[prod_level]] <- factor(data[[prod_level]], levels = unique(data[[prod_level]]))
  data$total_prc_chg_contri_flag <- ifelse(data$total_prc_chg_contri >= 0 , "rgb(132,195,67)","rgb(255,67,67)")
  data_agg[[prod_level_agg]] <- factor(data_agg[[prod_level_agg]], levels = unique(data_agg[[prod_level_agg]]))
  data_agg$prc_chg_flag <- ifelse(data_agg$prc_chg >= 0,"rgb(132,195,67)","rgb(255,67,67)")
  plot_ly(data) %>%
    add_trace(x=data_agg[[prod_level_agg]],y=data_agg$`prc_chg`, type = 'bar',
              hoverinfo = 'text',
              marker = list(color =~data_agg$prc_chg_flag),
              #color =data_agg[[prod_level_agg]],
              text = paste0(round(data_agg$prc_chg,2),"%"),textposition = "auto",textfont = list(color = '#000000')) %>%
    add_trace(x=~get(prod_level),y=~`total_prc_chg_contri`, type = 'bar',
              hoverinfo = 'text',
              #color =~get(prod_level),
              marker = list(color =~data$total_prc_chg_contri_flag),
              text = paste0(round(data$`total_prc_chg_contri`,2),"%"),textposition = "auto",textfont = list(color = '#000000')) %>%
    layout(showlegend = FALSE,title = "YoY % Change",paper_bgcolor = '#F5F5F5',plot_bgcolor = '#F5F5F5',font = list(size =10),xaxis = list(title = c(prod_level_agg,prod_level),tickfont = list(size =8)), yaxis = list(
      title = "",
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )) %>% config(displayModeBar = FALSE)
}

vol_price_bubble_chart <- function(data,prod_level){
  if(any(data$vol_chg > 100)){
    data[data$vol_chg > 100,]$vol_chg <- 100
  }
  if(any(data$vol_chg < -100)){
    data[data$vol_chg < -100,]$vol_chg <- -100
  }
  plot_ly(data,x= ~price_chg, y = ~vol_chg, 
          hoverinfo = 'text',
          hovertext = paste0(round(data$price_chg,2),",",round(data$vol_chg,2)),
          text = data[[prod_level]],textposition = "top center",textfont = list(color = '#000000',size = 8),type = 'scatter',mode = 'markers+text',color = ~get(prod_level),size=~(CY_Volume),sizes = c(10,2000)) %>% 
    layout(showlegend = FALSE,xaxis = list(title = "% Price Change"),yaxis = list(title = "% Volume Change")) %>% config(displayModeBar = FALSE)
}

cy_ya_table <- function(data,prod_level){
  
  data <- data[order(-data$`CY Volume('000 Units)`),]
  if(any(data$`% Volume Chg` > 100)){
    data[data$`% Volume Chg` > 100,]$`% Volume Chg` <- 100
  }
  if(any(data$`% Volume Chg` < -100)){
    data[data$`% Volume Chg` < -100,]$`% Volume Chg` <- -100
  }
  data$vol_chg_color <- ifelse(data$`% Volume Chg` > 0, "Positive",ifelse(data$`% Volume Chg` < 0, "Negative","Neutral"))
  data$prc_chg_color <- ifelse(data$`% Price Chg` > 0, "Positive",ifelse(data$`% Price Chg` < 0, "Negative","Neutral"))
  datatable(data,class="cell-border stripe",extensions = c('FixedColumns'),
            options = list(
              columnDefs=list(list(visible=FALSE, targets=c(length(data)-1,length(data)-2))),
              # paging = FALSE,
              dom = "t",
              # searching = FALSE,
              scrollX = T,
              scrollY = "350px"
            ),rownames = F)%>%formatStyle(names(data),textAlign = 'center')%>%formatRound(c(1:ncol(data)),2) %>%
    formatStyle(c("% Volume Chg","% Price Chg"),c("vol_chg_color","prc_chg_color"),backgroundColor = styleEqual(c("Positive","Negative","Neutral"),c("#84c343","#FF4343","#ffc000")))
}

val_share_contri_chart <- function(data,data_agg,prod_level,prod_level_agg){
  data <- data[order(data$CY_Volume),]
  data[[prod_level]] <- factor(data[[prod_level]], levels = unique(data[[prod_level]]))
  plot_ly(data) %>%
    add_trace(x = ~`total_val_share_contri`, color= ~get(prod_level),type = "bar",
              hoverinfo = 'text',hovertext = paste0(data[[prod_level]],": ",round(data$total_val_share_contri,2),"%"),
              text = paste0(round(data$total_val_share_contri,2),"%"),textposition = 'auto',textfont = list(color = '#000000'))%>%
    layout(showlegend = FALSE,font = list(size =10),title = paste0("Value Share Contribution to % CHG in ",prod_level_agg), xaxis = list(title = "% Change(Value Share)",showgrid = FALSE),yaxis = list(
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    ), barmode = "stack") %>% add_trace(x=data_agg$`val_chg`,y=data_agg[[prod_level_agg]],type="bar",
                                        hoverinfo = 'text',hovertext = paste0(data_agg[[prod_level_agg]],": ",round(data_agg$val_chg,2),"%"),
                                        text = paste0(round(data_agg$`val_chg`,2),"%"),textposition = 'auto',textfont = list(color = '#000000')) %>% config(displayModeBar = FALSE)
}

agg_fact_table <- function(data){
  
  data$Metrics <- as.character(data$Metrics)
  data[data$Metrics == "Value Sales Promo(Million GBP)",]$Metrics <- "Value Sales Incremental(Million GBP)"
  data[data$Metrics == "Volume Sales Promo('000 Units)",]$Metrics <- "Volume Sales Incremental('000 Units)"
  data[data$Metrics == "Base Price",]$Metrics <- "Regular Price(RSP)"
  data$abs_chg_color <- ifelse(data$`Absolute Change` > 0, "Positive",ifelse(data$`Absolute Change` < 0, "Negative","Neutral"))
  data$`%_chg_color` <- ifelse(data$`% Change` > 0, "Positive",ifelse(data$`% Change` < 0, "Negative","Neutral"))
  datatable(data,class="cell-border stripe",extensions = c('FixedColumns'),
            options = list(
              columnDefs=list(list(visible=FALSE, targets=c(length(data)-1,length(data)-2))),
              # paging = FALSE,
              dom = "t",
              # searching = FALSE,
              scrollX = T,
              scrollY = T
            ),rownames = F)%>%formatStyle(names(data),textAlign = 'center')%>%formatRound(c(2:ncol(data)),2) %>%
    formatStyle(c("Absolute Change","% Change"),c("abs_chg_color","%_chg_color"),backgroundColor = styleEqual(c("Positive","Negative","Neutral"),c("#84c343","#FF4343","#ffc000")))
}

vol_price_area_chart <- function(data){
  
  data <- data[order(data$Date),]
  #print(str(data))
  #print(head(data))
  plot_ly(data) %>% 
    add_trace(x = ~Date, y = ~((CY_Volume)/10^3), type = 'scatter', mode = 'lines', fill = 'tozeroy',line = list(width = 0.5,color = '#84c343'),
              hovertext = paste0(data$Date,", ",round((data$CY_Volume - data$base_volume)/10^3,2)),hoverinfo = 'text',
              name = 'Incremental Volume',fillcolor = '#7cc049') %>%
    add_trace(x = ~Date, y = ~((base_volume)/10^3), type = 'scatter', mode = 'lines',fill = 'tozeroy', line = list(width = 0.5,color='#F5FF8D'),
              hovertext = paste0(data$Date,", ",round((data$base_volume)/10^3,2)),hoverinfo = 'text',
              name = 'Base Volume',fillcolor = '#FFCC07') %>%
    add_trace(x = ~Date, y = ~discount, type = 'bar',name = 'Discount',yaxis = 'y2',
              hovertext = paste0(data$Date,", ",round(data$discount,2)),hoverinfo = 'text',
              marker = list(color = '#0099DC')) %>%
    #add_trace(x = ~Date, y = ~CY_Price, type = 'scatter', mode = 'lines',yaxis = 'y2',line = list(color = '#45171D'),name = 'Avg Price') %>%
    #add_trace(x = ~Date, y = ~base_price_calc, type = 'scatter', mode = 'lines',yaxis = 'y2',name = 'Base Price') %>%
    layout(margin = list(r= 50),xaxis = list(title = 'Time Period'),yaxis = list(side = 'left',title = "Volume Sales('000 Units)", zeroline = FALSE,range=c(0,max(data$CY_Volume)/10^3)),yaxis2 = list(side = 'right', overlaying = "y",title = "Discount %", zeroline = FALSE,range=c(0,100)),legend = list(orientation = 'h',x = 0, y = 50)) %>% config(displayModeBar = FALSE)
}

sku_table <- function(data){
  data <- data[order(-data$`Volume Sales`),]
  datatable(data,class="cell-border stripe",extensions = c('FixedColumns','Buttons'),
            options = list(autoWidth = TRUE,
                           buttons = list('copy', list(extend = 'csv',filename = 'SKU Data Table'),
                                          list(extend = 'excel', filename = 'SKU Data Table')),
                                          # list(extend = 'pdf',
                                          #      pageSize = 'A4',
                                          #      orientation = 'landscape',
                                          #      filename = 'SKU Data Table'), 'print'),
                           # paging = FALSE,
                           lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
                           pageLength = 10,
                           dom = "Blftrip",
                           #searching = FALSE,
                           scrollX = T,
                           scrollY = "250px"
            ),rownames = F)%>%formatStyle(names(data),textAlign = 'center')%>%
    formatCurrency(names(select_if(data,is.numeric)),currency = "", interval = 3, mark = ",")
}

promo_split_chart <- function(data,prod_level){
  plot_ly(data,x = ~(`CY_Value_Base`)/10^6,y=~get(prod_level),type = "bar",orientation='h',name="Base Sales",
          hovertext = paste0(data[[prod_level]],": ",round((data$`CY_Value_Base`)/10^6,2)),hoverinfo = 'text',
          marker = list(color='#7CC049'),text = round((data$`CY_Value_Base`)/10^6,2),textposition = 'auto',textfont = list(color = '#FFFFFF')) %>%
    add_trace(x = ~(`CY_Value`-`CY_Value_Base`)/10^6,name="Incremental Sales",marker = list(color='#CC66FF'),
              hovertext = paste0(data[[prod_level]],": ",round((data$`CY_Value`-data$`CY_Value_Base`)/10^6,2)),hoverinfo = 'text',
              text = round((data$`CY_Value`-data$`CY_Value_Base`)/10^6,2),textposition = 'auto',textfont = list(color = '#FFFFFF'))%>%
    layout(margin=list(t=50),font = list(size =10),legend= list(x=0,y=1.2,orientation = 'h'),xaxis = list(title = "Value Sales"),yaxis = list(
      title=prod_level,
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    ), barmode = "stack") %>% config(displayModeBar = FALSE)
}

promo_split_chart_CY_YA <- function(data,prod_level){
  
  # Validate input data
  if(is.null(data) || nrow(data) == 0){
    return(plot_ly() %>% 
           layout(title = "Promotion Volume Sales (CY & YA)",
                  xaxis = list(title = prod_level, showgrid = FALSE),
                  yaxis = list(title = "Volume Sales('000 Units)", showgrid = FALSE),
                  annotations = list(text = "No data available", showarrow = FALSE)))
  }
  
  # Check if required columns exist
  if(!all(c(prod_level, "CY_Volume", "CY_base_volume") %in% names(data))){
    return(plot_ly() %>% 
           layout(title = "Promotion Volume Sales (CY & YA)",
                  xaxis = list(title = prod_level, showgrid = FALSE),
                  yaxis = list(title = "Volume Sales('000 Units)", showgrid = FALSE),
                  annotations = list(text = "Current year data not available", showarrow = FALSE)))
  }
  
  # Extract CY data - ensure we have valid rows
  data_cy <- data[,c(prod_level,"CY_Volume","CY_base_volume"), with = FALSE]
  # Remove rows where CY data is NA or invalid
  data_cy <- data_cy[!is.na(CY_Volume) & !is.na(CY_base_volume),]
  if(nrow(data_cy) == 0){
    return(plot_ly() %>% 
           layout(title = "Promotion Volume Sales (CY & YA)",
                  xaxis = list(title = prod_level, showgrid = FALSE),
                  yaxis = list(title = "Volume Sales('000 Units)", showgrid = FALSE),
                  annotations = list(text = "No valid current year data", showarrow = FALSE)))
  }
  
  names(data_cy) <- c(prod_level,"Volume","Volume_Base")
  data_cy$Period <- "CY"
  
  # Extract YA data - check if columns exist and have valid data
  has_ya_data <- FALSE
  if(all(c(prod_level, "YA_Volume", "YA_base_volume") %in% names(data))){
    data_ya <- data[,c(prod_level,"YA_Volume","YA_base_volume"), with = FALSE]
    # Filter out rows where YA data is all NA
    data_ya <- data_ya[!is.na(YA_Volume) & !is.na(YA_base_volume),]
    if(nrow(data_ya) > 0){
      names(data_ya) <- c(prod_level,"Volume","Volume_Base")
      data_ya$Period <- "YA"
      has_ya_data <- TRUE
    }
  }
  
  # Combine data based on what's available
  if(has_ya_data && nrow(data_ya) > 0){
    # Both CY and YA data available - combine them
    # Ensure both have same columns in same order before rbinding
    common_cols <- intersect(names(data_ya), names(data_cy))
    if(length(common_cols) == length(names(data_cy)) && length(common_cols) == length(names(data_ya))){
      # Columns match, safe to rbind
      data_total <- rbind(data_ya[, common_cols, with = FALSE], 
                         data_cy[, common_cols, with = FALSE])
    } else {
      # Columns don't match perfectly, use only CY data
      data_total <- data_cy
    }
  } else {
    # Only CY data available
    data_total <- data_cy
  }
  
  # Always set factor levels to include both, even if only one exists
  # This ensures consistent structure for plotly
  data_total$Period <- factor(data_total$Period, levels = c("YA","CY"))
  
  # Convert to data.frame for plotly - this ensures consistent structure
  data_total_df <- as.data.frame(data_total)
  
  # Ensure we have at least one row
  if(nrow(data_total_df) == 0){
    return(plot_ly() %>% 
           layout(title = "Promotion Volume Sales (CY & YA)",
                  xaxis = list(title = prod_level, showgrid = FALSE),
                  yaxis = list(title = "Volume Sales('000 Units)", showgrid = FALSE),
                  annotations = list(text = "No data available", showarrow = FALSE)))
  }
  
  # Ensure numeric columns and handle any remaining NA
  data_total_df$Volume <- as.numeric(as.character(data_total_df$Volume))
  data_total_df$Volume_Base <- as.numeric(as.character(data_total_df$Volume_Base))
  data_total_df$Volume[is.na(data_total_df$Volume)] <- 0
  data_total_df$Volume_Base[is.na(data_total_df$Volume_Base)] <- 0
  
  # Calculate incremental volume
  data_total_df$Incremental_Volume <- data_total_df$Volume - data_total_df$Volume_Base
  
  # Verify all columns have the same length
  n_rows <- nrow(data_total_df)
  if(length(data_total_df$Period) != n_rows || 
     length(data_total_df$Volume_Base) != n_rows ||
     length(data_total_df$Incremental_Volume) != n_rows){
    return(plot_ly() %>% 
           layout(title = "Promotion Volume Sales (CY & YA)",
                  xaxis = list(title = prod_level, showgrid = FALSE),
                  yaxis = list(title = "Volume Sales('000 Units)", showgrid = FALSE),
                  annotations = list(text = "Data structure error", showarrow = FALSE)))
  }
  
  # Create hovertext vectors - ensure they match data length exactly
  hovertext_base <- character(n_rows)
  hovertext_inc <- character(n_rows)
  for(i in 1:n_rows){
    hovertext_base[i] <- paste(round(data_total_df$Volume_Base[i]/10^3, 2), "('000 Units)")
    hovertext_inc[i] <- paste(round(data_total_df$Incremental_Volume[i]/10^3, 2), "('000 Units)")
  }
  
  # Create the plot using formula notation with data frame
  # This is the safest way to ensure plotly handles the data correctly
  p <- plot_ly(data_total_df, 
               x = ~Period, 
               y = ~(Volume_Base/10^3), 
               type = 'bar', 
               name = 'Base Volume', 
               marker = list(color='#7CC049'),
               hovertext = hovertext_base, 
               hoverinfo = 'text')
  
  p <- p %>% add_trace(data = data_total_df,
                       x = ~Period,
                       y = ~(Incremental_Volume/10^3), 
                       name = 'Incremental Volume',
                       hovertext = hovertext_inc, 
                       hoverinfo = 'text', 
                       marker = list(color='#CC66FF'),
                       type = 'bar')
  
  p <- p %>% layout(title = "Promotion Volume Sales (CY & YA)", 
                    font = list(size = 10),
                    xaxis = list(title = prod_level, showgrid = FALSE),
                    yaxis = list(title = "Volume Sales('000 Units)", showgrid = FALSE),
                    barmode = 'stack')
  
  p <- p %>% config(displayModeBar = FALSE)
  
  return(p)
}

promo_split_chart_weekly <- function(data){
  plot_ly(data,x=~Date, y= ~(CY_Volume_Base)/10^3,type='bar',name='Base',
          hovertext = round((data$CY_Volume_Base)/10^3,2),hoverinfo = 'text',marker = list(color='#7CC049')) %>%
    add_trace(y= ~(CY_Volume_Unsupp*(CY_Volume - CY_Volume_Base)/(CY_Volume_Disp + CY_Volume_FeatDisp + CY_Volume_Feat + CY_Volume_Multi + CY_Volume_Unsupp))/10^3,name = 'Shelf Promotion',
              hovertext = round((data$CY_Volume_Unsupp*(data$CY_Volume - data$CY_Volume_Base)/(data$CY_Volume_Disp + data$CY_Volume_FeatDisp + data$CY_Volume_Feat + data$CY_Volume_Multi + data$CY_Volume_Unsupp))/10^3,2),
              hoverinfo = 'text',marker = list(color='#FFCC07')) %>%
    add_trace(y= ~((CY_Volume_Disp + CY_Volume_FeatDisp + CY_Volume_Feat + CY_Volume_Multi)*(CY_Volume - CY_Volume_Base)/(CY_Volume_Disp + CY_Volume_FeatDisp + CY_Volume_Feat + CY_Volume_Multi + CY_Volume_Unsupp))/10^3,name = 'Display &/or Feature Promotion',
              hovertext = round((data$CY_Volume_Disp + data$CY_Volume_FeatDisp + data$CY_Volume_Feat + data$CY_Volume_Multi)*(data$CY_Volume - data$CY_Volume_Base)/(data$CY_Volume_Disp + data$CY_Volume_FeatDisp + data$CY_Volume_Feat + data$CY_Volume_Multi + data$CY_Volume_Unsupp)/10^3,2),
              hoverinfo = 'text',marker = list(color ='#CC66FF')) %>%
    layout(title = "Promotion Volume Sales (WoW Analysis)",font = list(size =10),legend= list(x=0,y=1,orientation = 'h'),xaxis = list(title = 'Time Period',showgrid = FALSE),yaxis = list(title = "Volume Sales('000 Units)",showgrid = FALSE),barmode = 'stack') %>% config(displayModeBar = FALSE)
}

# promo_split_fact_table <- function(data,prod_level){
#   data <- data[,c(prod_level,"CY_Value_Promo","CY_Value_Disp","CY_Value_FeatDisp","CY_Value_Feat","CY_Value_Unsupp","CY_Value_Multi","CY_Value_Promo","CY_Investment")]
#   names(data) <- c(prod_level,"Total Promo","Display","Display & Feature","Feature","Unsupported","Multi Buy","Return","Investment")
#   datatable(data,class="cell-border stripe",extensions = c('FixedColumns'),
#             options = list(
#               # paging = FALSE,
#               dom = "t",
#               # searching = FALSE,
#               scrollX = T,
#               scrollY = "350px"
#             ),rownames = F)%>%formatStyle(names(data),textAlign = 'center')%>%formatRound(c(2:ncol(data)),2)
# }

promo_split_fact_table <- function(data,prod_level){
  data <- data[order(-data$`Incremental Sales`),]
  if(prod_level == "Event"){
    data <- data[data[[prod_level]] != "No Promo",] 
  }
  data[,c("Incremental Sales","Investment")] <- data[,c("Incremental Sales","Investment")]/10^6
  data <- data[,c(prod_level,"Incremental Sales","Investment","Incremental GM ROI","Incremental NR ROI","Incremental NIS ROI","Uplift"),with = FALSE]
  names(data) <- c(prod_level,"Incremental GM Sales (MM GBP)","Incremental Investment (MM GBP)","Incremental GM ROI","Incremental NR ROI","Incremental NIS ROI","Uplift")
  datatable(data,class="cell-border stripe",extensions = c('FixedColumns'),
            options = list(
              # paging = FALSE,
              dom = "t",
              # searching = FALSE,
              scrollX = T,
              scrollY = "330px"
            ),rownames = F)%>%formatStyle(names(data),textAlign = 'center')%>%formatRound(c(2:ncol(data)),2)
}

promo_ROI_chart <- function(data,prod_level,roi_selected){
  plot_ly(data,x= ~get(roi_selected), y = ~round(Investment/10^6,2), 
          hoverinfo = 'text',
          hovertext = round(data[[roi_selected]],2),
          text = data[[prod_level]],textposition = "top center",textfont = list(color = '#000000',size = 8),type = 'scatter',mode = 'markers+text',color = ~get(prod_level),size=~(`Incremental Sales`),sizes = c(10,2000)) %>% 
    layout(title = "ROI Chart",showlegend = FALSE,font = list(size =10),xaxis = list(title = "ROI"),yaxis = list(title = "Investment(Million GBP)")) %>% config(displayModeBar = FALSE)
}

promo_ROI_chart_op <- function(data,prod_level,roi_selected,data_1,data_2){
  data[is.na(data)] <- 0
  if(!(is.null(data_1))){
    data_1[is.na(data_1)] <- 0
  }
  if(!(is.null(data_2))){
    data_2[is.na(data_2)] <- 0
  }
  
  min_roi <- min(data[[roi_selected]],data_1[[roi_selected]],data_2[[roi_selected]])
  max_roi <- max(data[[roi_selected]],data_1[[roi_selected]],data_2[[roi_selected]])
  
  min_inv <- min(data$Investment,data_1$Investment,data_2$Investment)
  max_inv <- max(data$Investment,data_1$Investment,data_2$Investment)
  
  plot_ly(data,x= ~get(roi_selected), y = ~round(Investment/10^6,2), 
          hoverinfo = 'text',
          hovertext = round(data[[roi_selected]],2),
          text = data[[prod_level]],textposition = "top center",textfont = list(color = '#000000',size = 8),type = 'scatter',mode = 'markers+text',color = ~get(prod_level),size=~(`Incremental Sales`),sizes = c(10,2000)) %>% 
    layout(title = "ROI Chart",showlegend = FALSE,font = list(size =10),xaxis = list(title = "ROI",range = c(ifelse(min_roi < 0,min_roi * 1.3,min_roi * 0.7),max_roi * 1.3)),yaxis = list(title = "Investment(Million GBP)",range = c(min_inv * 0.7/10^6,max_inv * 1.3/10^6))) %>% config(displayModeBar = FALSE)
}

promo_cal_structure <- function(data){
  start_date <- ymd(min(data$Tesco_Date))
  end_date <- ymd(max(data$Tesco_Date))
  date_selected <- data.frame("Date" = seq(start_date,end_date,by ='week'))
  date_selected$`Month Name` <- month.name[month(date_selected$Date)]
  date_selected$`Year Name` <- year(date_selected$Date)
  #month_no_mapping <- data.frame("Month"=month.name,"Weeks" = rep(c(4,4,5),4))
  #date_selected <- left_join(date_selected,month_no_mapping,by = c("Month Name" = "Month"))
  month_selected <- data.table(date_selected)[,list("Weeks"= .N),by = list(`Month Name`,`Year Name`)]
  cont <- "<th colspan = '4'>Month</th>"
  for(i in c(1:nrow(month_selected))){
    cont <- paste0(cont,"<th colspan = '",month_selected[i,"Weeks"],"'>",month_selected[i,"Month Name"],"</th>")
  }
  return(list(cont,month_selected$Weeks))
}

#promo_cal_structure_output <- function(data){
 # start_date <- ymd(min(data$Date))
  #end_date <- ymd(max(data$Date))
  #date_selected <- data.frame("Date" = seq(start_date,end_date,by ='week'))
  #date_selected$`Month Name` <- month.name[month(date_selected$Date)]
  #date_selected$`Year Name` <- year(date_selected$Date)
  #month_no_mapping <- data.frame("Month"=month.name,"Weeks" = rep(c(4,4,5),4))
  #date_selected <- left_join(date_selected,month_no_mapping,by = c("Month Name" = "Month"))
  #month_selected <- data.table(date_selected)[,list("Weeks"= .N),by = list(`Month Name`,`Year Name`)]
  #cont <- "<th colspan = '5'>Month</th>"
  #for(i in c(1:nrow(month_selected))){
   # cont <- paste0(cont,"<th colspan = '",month_selected[i,"Weeks"],"'>",month_selected[i,"Month Name"],"</th>")
  #}
  #return(list(cont,month_selected$Weeks))
#}

promo_cal_structure_output <- function(data) {
  library(data.table)
  library(lubridate)
  
  dt <- as.data.table(data)
  
  # make sure slot dates are Date class
  dt[, `Start Date` := as.Date(`Start Date`)]
  dt[, `End Date`   := as.Date(`End Date`)]
  
  # one row per slot (Tesco_Week_No is your slot id)
  slots <- unique(dt[,list(
    Slot_Number = Tesco_Week_No,
    Slot_Start  = `Start Date`,
    Slot_End    = `End Date`
  )])
  
  data.table::setorder(slots, Slot_Number)
  
  # Top header row: first 5 cols are labels, then one <th> per slot
  cont <- "<th colspan='5'>Slot</th>"
  for (i in seq_len(nrow(slots))) {
    lab <- paste0(
      "S", slots$Slot_Number[i], "<br>",
      format(slots$Slot_Start[i], "%d-%b"), " – ",
      format(slots$Slot_End[i],   "%d-%b")
    )
    cont <- paste0(cont, "<th>", lab, "</th>")
  }
  
  # Second element: "Weeks" vector used for colspans; each slot has width 1
  return(list(cont, rep(1, nrow(slots))))
}
KPI_calc <- function(tpo_list,opti_op_lsm,opti_op_nonlsm,exc_brand_lsm,exc_brand_nonlsm,opti_const_grouped,delist_sku,delist_selection,exclude_selection,ROI_selected){
  if(ROI_selected == "Incremental NR ROI"){
    opti_op_lsm$Inc_GM_Abs <- opti_op_lsm$R_Net_Rev_Inc
    opti_op_nonlsm$Inc_GM_Abs <- opti_op_nonlsm$R_Net_Rev_Inc
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_Revenue_New
    exc_brand_lsm$Inc_GM_Abs <- exc_brand_lsm$R_Net_Revenue_Inc
    exc_brand_nonlsm$Inc_GM_Abs <- exc_brand_nonlsm$R_Net_Revenue_Inc
  }else if(ROI_selected == "Incremental NIS ROI"){
    opti_op_lsm$Inc_GM_Abs <- opti_op_lsm$R_NIS_Inc
    opti_op_nonlsm$Inc_GM_Abs <- opti_op_nonlsm$R_NIS_Inc
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_NIS_New
    exc_brand_lsm$Inc_GM_Abs <- exc_brand_lsm$R_NIS_Inc
    exc_brand_nonlsm$Inc_GM_Abs <- exc_brand_nonlsm$R_NIS_Inc
  }else if(ROI_selected == "Incremental GM ROI"){
    opti_op_lsm$Inc_GM_Abs <- opti_op_lsm$R_GM_Inc
    opti_op_nonlsm$Inc_GM_Abs <- opti_op_nonlsm$R_GM_Inc
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_GM_Abs_New
    exc_brand_lsm$Inc_GM_Abs <- exc_brand_lsm$R_GM_Inc
    exc_brand_nonlsm$Inc_GM_Abs <- exc_brand_nonlsm$R_GM_Inc
  }
  
  if(is.null(exc_brand_nonlsm)){
    exc_brand_nonlsm <- exc_brand_lsm
  }
  if(delist_selection == FALSE & exclude_selection == TRUE){
    
    delist_NR <- sum(delist_sku$Net_Revenue)
    delist_GM_Abs <- sum(delist_sku$GM_Abs)
    delist_Gross_Sales <- sum(delist_sku$Gross_Sales)
    delist_TI <- sum(delist_sku$Trade_Investment)
    delist_NIS <- sum(delist_sku$NIS)
    #delist_Inc_GM_Abs <- sum(delist_sku$Inc_GM_Abs)
    
    delist_units <- sum(delist_sku$Units)
    delist_RSV <- sum(delist_sku$Retailer_Revenue)
    if(ROI_selected == "Incremental NR ROI"){
      delist_Inc <- sum(delist_sku$R_Net_Revenue_Inc)
    }else if(ROI_selected == "Incremental NIS ROI"){
      delist_Inc <- sum(delist_sku$R_NIS_Inc)
    }else if(ROI_selected == "Incremental GM ROI"){
      delist_Inc <- sum(delist_sku$R_GM_Inc)
      #delist_Inc <- 0
    }
    
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  "net_sales_lsm" = (sum(opti_op_lsm$Net_Revenue) + sum(exc_brand_lsm$Net_Revenue))/10^6,
                                  "net_sales_nonlsm" = (sum(opti_op_nonlsm$Net_Revenue) + sum(exc_brand_nonlsm$Net_Revenue))/10^6,
                                  "net_sales_ly" = (opti_const_grouped$Net_Revenue - delist_NR)/10^6,
                                  "net_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Minimum Value`,""),
                                  "net_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Maximum Value`,""),
                                  
                                  "gm_%_lsm" = (sum(opti_op_lsm$GM_Abs) + sum(exc_brand_lsm$GM_Abs))* 100/(sum(opti_op_lsm$Net_Revenue) + sum(exc_brand_lsm$Net_Revenue)),
                                  "gm_%_nonlsm" = (sum(opti_op_nonlsm$GM_Abs) + sum(exc_brand_nonlsm$GM_Abs)) * 100/(sum(opti_op_nonlsm$Net_Revenue) + sum(exc_brand_nonlsm$Net_Revenue)),
                                  "gm_%_ly" = ((opti_const_grouped$GM_Abs - delist_GM_Abs)*100/(opti_const_grouped$Net_Revenue - delist_NR)),
                                  "gm_%_min" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Minimum Value`,""),
                                  "gm_%_max" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Maximum Value`,""),
                                  
                                  "gross_sales_lsm" = (sum(opti_op_lsm$Gross_Sales) + sum(exc_brand_lsm$Gross_Sales))/10^6,
                                  "gross_sales_nonlsm" = (sum(opti_op_nonlsm$Gross_Sales) + sum(exc_brand_nonlsm$Gross_Sales))/10^6,
                                  "gross_sales_ly" = (opti_const_grouped$Gross_Sales - delist_Gross_Sales)/10^6,
                                  "gross_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Minimum Value`,""),
                                  "gross_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Maximum Value`,""),
                                  
                                  "gm_lsm" = (sum(opti_op_lsm$GM_Abs) + sum(exc_brand_lsm$GM_Abs))/10^6,
                                  "gm_nonlsm" = (sum(opti_op_nonlsm$GM_Abs) + sum(exc_brand_nonlsm$GM_Abs))/10^6,
                                  "gm_ly" = ((opti_const_grouped$GM_Abs - delist_GM_Abs))/10^6,
                                  "gm_min" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Minimum Value`,""),
                                  "gm_max" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Maximum Value`,""),
                                  
                                  "spend_%_lsm" = (sum(opti_op_lsm$Total_Trade_Investment) + sum(exc_brand_lsm$Trade_Investment))* 100/(sum(opti_op_lsm$Net_Revenue) + sum(exc_brand_lsm$Net_Revenue)),
                                  "spend_%_nonlsm" = (sum(opti_op_nonlsm$Total_Trade_Investment) + sum(exc_brand_nonlsm$Trade_Investment))* 100/(sum(opti_op_nonlsm$Net_Revenue) + sum(exc_brand_nonlsm$Net_Revenue)),
                                  "spend_%_ly" = ((opti_const_grouped$Trade_Investment - delist_TI)*100/(opti_const_grouped$Net_Revenue - delist_NR)),
                                  "spend_%_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Minimum Value`,""),
                                  "spend_%_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Maximum Value`,""),
                                  
                                  "spend_%NIS_lsm" = (sum(opti_op_lsm$Total_Trade_Investment) + sum(exc_brand_lsm$Trade_Investment))* 100/(sum(opti_op_lsm$NIS) + sum(exc_brand_lsm$NIS)),
                                  "spend_%NIS_nonlsm" = (sum(opti_op_nonlsm$Total_Trade_Investment) + sum(exc_brand_nonlsm$Trade_Investment))* 100/(sum(opti_op_nonlsm$NIS) + sum(exc_brand_nonlsm$NIS)),
                                  "spend_%NIS_ly" = ((opti_const_grouped$Trade_Investment - delist_TI)*100/(opti_const_grouped$NIS - delist_NIS)),
                                  "spend_%NIS_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Minimum Value`,""),
                                  "spend_%NIS_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Maximum Value`,""),
                                  
                                  "ROI_lsm" = (sum(opti_op_lsm$Inc_GM_Abs) + sum(exc_brand_lsm$Inc_GM_Abs))/(sum(opti_op_lsm$R_Trade_Inv_Inc) + sum(exc_brand_lsm$R_Total_Trade_Inv_Inc)),
                                  "ROI_nonlsm" = (sum(opti_op_nonlsm$Inc_GM_Abs) + sum(exc_brand_nonlsm$Inc_GM_Abs))/(sum(opti_op_nonlsm$R_Trade_Inv_Inc) + sum(exc_brand_nonlsm$R_Total_Trade_Inv_Inc)),
                                  "ROI_ly" = ((opti_const_grouped$Inc_GM_Abs - delist_Inc)/(opti_const_grouped$Trade_Investment_New - sum(delist_sku$R_Total_Trade_Inv_Inc))),
                                  "ROI_min" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Minimum Value`,""),
                                  "ROI_max" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Maximum Value`,""),
                                  
                                  "vol_sales_lsm" = (sum(opti_op_lsm$Total_Sales) + sum(exc_brand_lsm$Units))/10^6,
                                  "vol_sales_nonlsm" = (sum(opti_op_nonlsm$Total_Sales) + sum(exc_brand_nonlsm$Units))/10^6,
                                  "vol_sales_ly" = (opti_const_grouped$CY_Volume - delist_units)/10^6,
                                  "vol_sales_min" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Minimum Value`,""),
                                  "vol_sales_max" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Maximum Value`,""),
                                  
                                  "market_share_lsm" = (sum(opti_op_lsm$Retailer_Revenue) + sum(exc_brand_lsm$Retailer_Revenue))*100/(sum(opti_op_lsm$Retailer_Revenue) + sum(exc_brand_lsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_nonlsm" = (sum(opti_op_nonlsm$Retailer_Revenue) + sum(exc_brand_nonlsm$Retailer_Revenue))*100/(sum(opti_op_nonlsm$Retailer_Revenue) + sum(exc_brand_nonlsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_ly" = (opti_const_grouped$Retailer_Revenue - delist_RSV)* 100/(opti_const_grouped$Retailer_Revenue - delist_RSV + (tpo_list$other_sales_value)),
                                  "market_share_min" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Minimum Value`,""),
                                  "market_share_max" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Maximum Value`,""),check.names = FALSE)
  }else if(delist_selection == TRUE & exclude_selection == TRUE){
    
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  "net_sales_lsm" = (sum(opti_op_lsm$Net_Revenue) + sum(exc_brand_lsm$Net_Revenue))/10^6,
                                  "net_sales_nonlsm" = (sum(opti_op_nonlsm$Net_Revenue) + sum(exc_brand_nonlsm$Net_Revenue))/10^6,
                                  "net_sales_ly" = (opti_const_grouped$Net_Revenue)/10^6,
                                  "net_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Minimum Value`,""),
                                  "net_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Maximum Value`,""),
                                  
                                  "gm_%_lsm" = (sum(opti_op_lsm$GM_Abs) + sum(exc_brand_lsm$GM_Abs))* 100/(sum(opti_op_lsm$Net_Revenue) + sum(exc_brand_lsm$Net_Revenue)),
                                  "gm_%_nonlsm" = (sum(opti_op_nonlsm$GM_Abs) + sum(exc_brand_nonlsm$GM_Abs)) * 100/(sum(opti_op_nonlsm$Net_Revenue) + sum(exc_brand_nonlsm$Net_Revenue)),
                                  "gm_%_ly" = (opti_const_grouped$GM_Abs*100/opti_const_grouped$Net_Revenue),
                                  "gm_%_min" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Minimum Value`,""),
                                  "gm_%_max" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Maximum Value`,""),
                                  
                                  "gross_sales_lsm" = (sum(opti_op_lsm$Gross_Sales) + sum(exc_brand_lsm$Gross_Sales))/10^6,
                                  "gross_sales_nonlsm" = (sum(opti_op_nonlsm$Gross_Sales) + sum(exc_brand_nonlsm$Gross_Sales))/10^6,
                                  "gross_sales_ly" = (opti_const_grouped$Gross_Sales)/10^6,
                                  "gross_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Minimum Value`,""),
                                  "gross_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Maximum Value`,""),
                                  
                                  "gm_lsm" = (sum(opti_op_lsm$GM_Abs) + sum(exc_brand_lsm$GM_Abs))/10^6,
                                  "gm_nonlsm" = (sum(opti_op_nonlsm$GM_Abs) + sum(exc_brand_nonlsm$GM_Abs))/10^6,
                                  "gm_ly" = ((opti_const_grouped$GM_Abs))/10^6,
                                  "gm_min" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Minimum Value`,""),
                                  "gm_max" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Maximum Value`,""),
                                  
                                  "spend_%_lsm" = (sum(opti_op_lsm$Total_Trade_Investment) + sum(exc_brand_lsm$Trade_Investment))* 100/(sum(opti_op_lsm$Net_Revenue) + sum(exc_brand_lsm$Net_Revenue)),
                                  "spend_%_nonlsm" = (sum(opti_op_nonlsm$Total_Trade_Investment) + sum(exc_brand_nonlsm$Trade_Investment))* 100/(sum(opti_op_nonlsm$Net_Revenue) + sum(exc_brand_nonlsm$Net_Revenue)),
                                  "spend_%_ly" = (opti_const_grouped$Trade_Investment*100/opti_const_grouped$Net_Revenue),
                                  "spend_%_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Minimum Value`,""),
                                  "spend_%_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Maximum Value`,""),
                                  
                                  "spend_%NIS_lsm" = (sum(opti_op_lsm$Total_Trade_Investment) + sum(exc_brand_lsm$Trade_Investment))* 100/(sum(opti_op_lsm$NIS) + sum(exc_brand_lsm$NIS)),
                                  "spend_%NIS_nonlsm" = (sum(opti_op_nonlsm$Total_Trade_Investment) + sum(exc_brand_nonlsm$Trade_Investment))* 100/(sum(opti_op_nonlsm$NIS) + sum(exc_brand_nonlsm$NIS)),
                                  "spend_%NIS_ly" = (opti_const_grouped$Trade_Investment*100/opti_const_grouped$NIS),
                                  "spend_%NIS_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Minimum Value`,""),
                                  "spend_%NIS_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Maximum Value`,""),
                                  
                                  "ROI_lsm" = (sum(opti_op_lsm$Inc_GM_Abs) + sum(exc_brand_lsm$Inc_GM_Abs))/(sum(opti_op_lsm$R_Trade_Inv_Inc) + sum(exc_brand_lsm$R_Total_Trade_Inv_Inc)),
                                  "ROI_nonlsm" = (sum(opti_op_nonlsm$Inc_GM_Abs) + sum(exc_brand_nonlsm$Inc_GM_Abs))/(sum(opti_op_nonlsm$R_Trade_Inv_Inc) + sum(exc_brand_nonlsm$R_Total_Trade_Inv_Inc)),
                                  "ROI_ly" = (opti_const_grouped$Inc_GM_Abs/opti_const_grouped$Trade_Investment_New),
                                  "ROI_min" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Minimum Value`,""),
                                  "ROI_max" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Maximum Value`,""),
                                  
                                  "vol_sales_lsm" = (sum(opti_op_lsm$Total_Sales) + sum(exc_brand_lsm$Units))/10^6,
                                  "vol_sales_nonlsm" = (sum(opti_op_nonlsm$Total_Sales) + sum(exc_brand_nonlsm$Units))/10^6,
                                  "vol_sales_ly" = (opti_const_grouped$CY_Volume)/10^6,
                                  "vol_sales_min" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Minimum Value`,""),
                                  "vol_sales_max" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Maximum Value`,""),
                                  
                                  "market_share_lsm" = (sum(opti_op_lsm$Retailer_Revenue) + sum(exc_brand_lsm$Retailer_Revenue))*100/(sum(opti_op_lsm$Retailer_Revenue) + sum(exc_brand_lsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_nonlsm" = (sum(opti_op_nonlsm$Retailer_Revenue) + sum(exc_brand_nonlsm$Retailer_Revenue))*100/(sum(opti_op_nonlsm$Retailer_Revenue) + sum(exc_brand_nonlsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_ly" = (opti_const_grouped$Retailer_Revenue) * 100/(opti_const_grouped$Retailer_Revenue + (tpo_list$other_sales_value)),
                                  "market_share_min" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Minimum Value`,""),
                                  "market_share_max" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Maximum Value`,""),check.names = FALSE)
  }else if(delist_selection == FALSE & exclude_selection == FALSE){
    
    delist_NR <- sum(delist_sku$Net_Revenue)
    delist_GM_Abs <- sum(delist_sku$GM_Abs)
    delist_Gross_Sales <- sum(delist_sku$Gross_Sales)
    delist_TI <- sum(delist_sku$Trade_Investment)
    delist_NIS <- sum(delist_sku$NIS)
    #delist_Inc_GM_Abs <- sum(delist_sku$Inc_GM_Abs)
    
    delist_units <- sum(delist_sku$Units)
    delist_RSV <- sum(delist_sku$Retailer_Revenue)
    if(ROI_selected == "Incremental NR ROI"){
      delist_Inc <- sum(delist_sku$R_Net_Revenue_Inc)
    }else if(ROI_selected == "Incremental NIS ROI"){
      delist_Inc <- sum(delist_sku$R_NIS_Inc)
    }else if(ROI_selected == "Incremental GM ROI"){
      delist_Inc <- sum(delist_sku$R_GM_Inc)
      #delist_Inc <- 0
    }
    
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  "net_sales_lsm" = (sum(opti_op_lsm$Net_Revenue))/10^6,
                                  "net_sales_nonlsm" = (sum(opti_op_nonlsm$Net_Revenue))/10^6,
                                  "net_sales_ly" = (opti_const_grouped$Net_Revenue - delist_NR - sum(exc_brand_nonlsm$Net_Revenue))/10^6,
                                  "net_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Minimum Value`,""),
                                  "net_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Maximum Value`,""),
                                  
                                  "gm_%_lsm" = (sum(opti_op_lsm$GM_Abs))* 100/(sum(opti_op_lsm$Net_Revenue)),
                                  "gm_%_nonlsm" = (sum(opti_op_nonlsm$GM_Abs)) * 100/(sum(opti_op_nonlsm$Net_Revenue)),
                                  "gm_%_ly" = ((opti_const_grouped$GM_Abs - delist_GM_Abs - sum(exc_brand_nonlsm$GM_Abs))*100/(opti_const_grouped$Net_Revenue - delist_NR - sum(exc_brand_nonlsm$Net_Revenue))),
                                  "gm_%_min" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Minimum Value`,""),
                                  "gm_%_max" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Maximum Value`,""),
                                  
                                  "gross_sales_lsm" = (sum(opti_op_lsm$Gross_Sales))/10^6,
                                  "gross_sales_nonlsm" = (sum(opti_op_nonlsm$Gross_Sales))/10^6,
                                  "gross_sales_ly" = (opti_const_grouped$Gross_Sales - delist_Gross_Sales - sum(exc_brand_nonlsm$Gross_Sales))/10^6,
                                  "gross_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Minimum Value`,""),
                                  "gross_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Maximum Value`,""),
                                  
                                  "gm_lsm" = (sum(opti_op_lsm$GM_Abs))/10^6,
                                  "gm_nonlsm" = (sum(opti_op_nonlsm$GM_Abs))/10^6,
                                  "gm_ly" = ((opti_const_grouped$GM_Abs - delist_GM_Abs - sum(exc_brand_nonlsm$GM_Abs)))/10^6,
                                  "gm_min" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Minimum Value`,""),
                                  "gm_max" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Maximum Value`,""),
                                  
                                  "spend_%_lsm" = (sum(opti_op_lsm$Total_Trade_Investment))* 100/(sum(opti_op_lsm$Net_Revenue)),
                                  "spend_%_nonlsm" = (sum(opti_op_nonlsm$Total_Trade_Investment))* 100/(sum(opti_op_nonlsm$Net_Revenue)),
                                  "spend_%_ly" = ((opti_const_grouped$Trade_Investment - delist_TI - sum(exc_brand_nonlsm$Trade_Investment))*100/(opti_const_grouped$Net_Revenue - delist_NR - sum(exc_brand_nonlsm$Net_Revenue))),
                                  "spend_%_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Minimum Value`,""),
                                  "spend_%_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Maximum Value`,""),
                                  
                                  "spend_%NIS_lsm" = (sum(opti_op_lsm$Total_Trade_Investment))* 100/(sum(opti_op_lsm$NIS)),
                                  "spend_%NIS_nonlsm" = (sum(opti_op_nonlsm$Total_Trade_Investment))* 100/(sum(opti_op_nonlsm$NIS)),
                                  "spend_%NIS_ly" = ((opti_const_grouped$Trade_Investment - delist_TI - sum(exc_brand_nonlsm$Trade_Investment))*100/(opti_const_grouped$NIS - delist_NIS - sum(exc_brand_nonlsm$NIS))),
                                  "spend_%NIS_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Minimum Value`,""),
                                  "spend_%NIS_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Maximum Value`,""),
                                  
                                  "ROI_lsm" = (sum(opti_op_lsm$Inc_GM_Abs))/(sum(opti_op_lsm$R_Trade_Inv_Inc)),
                                  "ROI_nonlsm" = (sum(opti_op_nonlsm$Inc_GM_Abs))/(sum(opti_op_nonlsm$R_Trade_Inv_Inc)),
                                  "ROI_ly" = ((opti_const_grouped$Inc_GM_Abs - delist_Inc - sum(exc_brand_nonlsm$Inc_GM_Abs))/(opti_const_grouped$Trade_Investment_New - sum(delist_sku$R_Total_Trade_Inv_Inc) - sum(exc_brand_nonlsm$R_Total_Trade_Inv_Inc))),
                                  "ROI_min" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Minimum Value`,""),
                                  "ROI_max" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Maximum Value`,""),
                                  
                                  "vol_sales_lsm" = (sum(opti_op_lsm$Total_Sales))/10^6,
                                  "vol_sales_nonlsm" = (sum(opti_op_nonlsm$Total_Sales))/10^6,
                                  "vol_sales_ly" = (opti_const_grouped$CY_Volume - delist_units - sum(exc_brand_nonlsm$Units))/10^6,
                                  "vol_sales_min" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Minimum Value`,""),
                                  "vol_sales_max" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Maximum Value`,""),
                                  
                                  "market_share_lsm" = (sum(opti_op_lsm$Retailer_Revenue))*100/(sum(opti_op_lsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_nonlsm" = (sum(opti_op_nonlsm$Retailer_Revenue))*100/(sum(opti_op_nonlsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_ly" = (opti_const_grouped$Retailer_Revenue - delist_RSV - sum(exc_brand_nonlsm$Retailer_Revenue))* 100/(opti_const_grouped$Retailer_Revenue - delist_RSV  - sum(exc_brand_nonlsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_min" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Minimum Value`,""),
                                  "market_share_max" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Maximum Value`,""),check.names = FALSE)
  }else if(delist_selection == TRUE & exclude_selection == FALSE){
    
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  "net_sales_lsm" = (sum(opti_op_lsm$Net_Revenue))/10^6,
                                  "net_sales_nonlsm" = (sum(opti_op_nonlsm$Net_Revenue))/10^6,
                                  "net_sales_ly" = (opti_const_grouped$Net_Revenue - sum(exc_brand_nonlsm$Net_Revenue))/10^6,
                                  "net_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Minimum Value`,""),
                                  "net_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Maximum Value`,""),
                                  
                                  "gm_%_lsm" = (sum(opti_op_lsm$GM_Abs))* 100/(sum(opti_op_lsm$Net_Revenue)),
                                  "gm_%_nonlsm" = (sum(opti_op_nonlsm$GM_Abs)) * 100/(sum(opti_op_nonlsm$Net_Revenue)),
                                  "gm_%_ly" = ((opti_const_grouped$GM_Abs - sum(exc_brand_nonlsm$GM_Abs))*100/(opti_const_grouped$Net_Revenue - sum(exc_brand_nonlsm$Net_Revenue))),
                                  "gm_%_min" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Minimum Value`,""),
                                  "gm_%_max" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Maximum Value`,""),
                                  
                                  "gross_sales_lsm" = (sum(opti_op_lsm$Gross_Sales))/10^6,
                                  "gross_sales_nonlsm" = (sum(opti_op_nonlsm$Gross_Sales))/10^6,
                                  "gross_sales_ly" = (opti_const_grouped$Gross_Sales - sum(exc_brand_nonlsm$Gross_Sales))/10^6,
                                  "gross_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Minimum Value`,""),
                                  "gross_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Maximum Value`,""),
                                  
                                  "gm_lsm" = (sum(opti_op_lsm$GM_Abs))/10^6,
                                  "gm_nonlsm" = (sum(opti_op_nonlsm$GM_Abs))/10^6,
                                  "gm_ly" = ((opti_const_grouped$GM_Abs - sum(exc_brand_nonlsm$GM_Abs)))/10^6,
                                  "gm_min" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Minimum Value`,""),
                                  "gm_max" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Maximum Value`,""),
                                  
                                  "spend_%_lsm" = (sum(opti_op_lsm$Total_Trade_Investment))* 100/(sum(opti_op_lsm$Net_Revenue)),
                                  "spend_%_nonlsm" = (sum(opti_op_nonlsm$Total_Trade_Investment))* 100/(sum(opti_op_nonlsm$Net_Revenue)),
                                  "spend_%_ly" = ((opti_const_grouped$Trade_Investment - sum(exc_brand_nonlsm$Trade_Investment))*100/(opti_const_grouped$Net_Revenue - sum(exc_brand_nonlsm$Net_Revenue))),
                                  "spend_%_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Minimum Value`,""),
                                  "spend_%_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Maximum Value`,""),
                                  
                                  "spend_%NIS_lsm" = (sum(opti_op_lsm$Total_Trade_Investment))* 100/(sum(opti_op_lsm$NIS)),
                                  "spend_%NIS_nonlsm" = (sum(opti_op_nonlsm$Total_Trade_Investment))* 100/(sum(opti_op_nonlsm$NIS)),
                                  "spend_%NIS_ly" = ((opti_const_grouped$Trade_Investment - sum(exc_brand_nonlsm$Trade_Investment))*100/(opti_const_grouped$NIS - sum(exc_brand_nonlsm$NIS))),
                                  "spend_%NIS_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Minimum Value`,""),
                                  "spend_%NIS_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Maximum Value`,""),
                                  
                                  "ROI_lsm" = (sum(opti_op_lsm$Inc_GM_Abs))/(sum(opti_op_lsm$R_Trade_Inv_Inc)),
                                  "ROI_nonlsm" = (sum(opti_op_nonlsm$Inc_GM_Abs))/(sum(opti_op_nonlsm$R_Trade_Inv_Inc)),
                                  "ROI_ly" = ((opti_const_grouped$Inc_GM_Abs - sum(exc_brand_nonlsm$Inc_GM_Abs))/(opti_const_grouped$Trade_Investment_New - sum(exc_brand_nonlsm$R_Total_Trade_Inv_Inc))),
                                  "ROI_min" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Minimum Value`,""),
                                  "ROI_max" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Maximum Value`,""),
                                  
                                  "vol_sales_lsm" = (sum(opti_op_lsm$Total_Sales))/10^6,
                                  "vol_sales_nonlsm" = (sum(opti_op_nonlsm$Total_Sales))/10^6,
                                  "vol_sales_ly" = (opti_const_grouped$CY_Volume - sum(exc_brand_nonlsm$Units))/10^6,
                                  "vol_sales_min" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Minimum Value`,""),
                                  "vol_sales_max" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Maximum Value`,""),
                                  
                                  "market_share_lsm" = (sum(opti_op_lsm$Retailer_Revenue))*100/(sum(opti_op_lsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_nonlsm" = (sum(opti_op_nonlsm$Retailer_Revenue))*100/(sum(opti_op_nonlsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_ly" = (opti_const_grouped$Retailer_Revenue - sum(exc_brand_nonlsm$Retailer_Revenue))* 100/(opti_const_grouped$Retailer_Revenue - sum(exc_brand_nonlsm$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_min" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Minimum Value`,""),
                                  "market_share_max" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Maximum Value`,""),check.names = FALSE)
  }
  
  return(KPI_calculation)
}

KPI_calc_input <- function(opti_op,roi_selected){
  
  output <- opti_op[,list(
    "End Date"=`End Date`,
    "Duration"= Duration,
    "Net Invoice Sales" = sum(NIS),"CPD" = sum(Retro_Funding_Total), "TI" = sum(Total_Trade_Investment),"Scan Net Revenue" = sum(Net_Revenue),"Gross Margin" = sum(GM_Abs), "GM % NR" = sum(GM_Abs) * 100/sum(Net_Revenue),
                       "Trade ROI" = ifelse(roi_selected == "Incremental GM ROI",sum(Inc_GM_Abs)/sum(Total_Trade_Investment),ifelse(roi_selected == "Incremental NR ROI",sum(Inc_Revenue)/sum(Total_Trade_Investment),sum(Inc_NIS)/sum(Total_Trade_Investment))),
                       "Inc_GM_Abs" = sum(Inc_GM_Abs), "TI % NR" = sum(Total_Trade_Investment) * 100/sum(Net_Revenue), "CPD % NIS" = sum(Retro_Funding_Total) * 100/sum(NIS), "Total_Sales" = sum(Total_Sales),
                       "RSV" = sum(Retailer_Revenue),"COGS" = sum(Net_Cost_Unit * Total_Sales),"Front Margin" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales), "FM %" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales)/sum(Retailer_Revenue/1.2), "Fixed" = sum(Display_Cost), "Back Margin" = sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales) + sum(Display_Cost), "BM %" = (sum(((Promo_Price/1.2) - Net_Cost_Unit + Retro_Funding_Unit) * Total_Sales) + sum(Display_Cost))/sum(Retailer_Revenue/1.2)),by = list(`Start Date`)]
  
  
  return(output)
}

KPI_calc_ret <- function(tpo_list,opti_op_lsm,opti_op_nonlsm,exc_brand_lsm,exc_brand_nonlsm,opti_const_grouped,delist_sku,delist_selection,exclude_selection,ROI_selected){
  if(ROI_selected == "Incremental NR ROI"){
    opti_op_lsm$Inc_GM_Abs <- opti_op_lsm$R_Net_Rev_Inc
    opti_op_nonlsm$Inc_GM_Abs <- opti_op_nonlsm$R_Net_Rev_Inc
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_Revenue
  }else if(ROI_selected == "Incremental NIS ROI"){
    opti_op_lsm$Inc_GM_Abs <- opti_op_lsm$R_NIS_Inc
    opti_op_nonlsm$Inc_GM_Abs <- opti_op_nonlsm$R_NIS_Inc
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_NIS
  }else if(ROI_selected == "Incremental GM ROI"){
    opti_op_lsm$Inc_GM_Abs <- opti_op_lsm$R_GM_Inc
    opti_op_nonlsm$Inc_GM_Abs <- opti_op_nonlsm$R_GM_Inc
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_GM_Abs
  }
  
  if(is.null(exc_brand_nonlsm)){
    exc_brand_nonlsm <- exc_brand_lsm
  }
  
  if(delist_selection == FALSE & exclude_selection == TRUE){
    
    delist_RSV <- sum(delist_sku$Retailer_Revenue)
    delist_Net_Cost <- sum(delist_sku$Net_Cost_Total)
    delist_FM <- sum(delist_sku$FM_Total)
    delist_CPD <- sum(delist_sku$Retro_Fund_Unit * delist_sku$Units)
    delist_fixed <- sum(delist_sku$R_Display_Fee_Total)
    
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  
                                  "RSV_lsm" = (sum(opti_op_lsm$RSV) + sum(exc_brand_lsm$Retailer_Revenue))/10^6,
                                  "RSV_nonlsm" = (sum(opti_op_nonlsm$RSV) + sum(exc_brand_nonlsm$Retailer_Revenue))/10^6,
                                  "RSV_ly" = (sum(opti_const_grouped$Retailer_Revenue) - delist_RSV)/10^6,
                                  
                                  "COGS_lsm" = (sum(opti_op_lsm$COGS) + sum(exc_brand_lsm$Net_Cost_Total))/10^6,
                                  "COGS_nonlsm" = (sum(opti_op_nonlsm$COGS) + sum(exc_brand_nonlsm$Net_Cost_Total))/10^6,
                                  "COGS_ly" = (sum(opti_const_grouped$Net_Cost_Total) - delist_Net_Cost)/10^6,
                                  
                                  "FM_lsm" = (sum(opti_op_lsm$`Front Margin`) + sum(exc_brand_lsm$FM_Total))/10^6,
                                  "FM_nonlsm" = (sum(opti_op_nonlsm$`Front Margin`) + sum(exc_brand_nonlsm$FM_Total))/10^6,
                                  "FM_ly" = (sum(opti_const_grouped$FM) - delist_FM)/10^6,
                                  
                                  "CPD_lsm" = (sum(opti_op_lsm$CPD) + sum(exc_brand_lsm$Retro_Fund_Total))/10^6,
                                  "CPD_nonlsm" = (sum(opti_op_nonlsm$CPD) + sum(exc_brand_nonlsm$Retro_Fund_Total))/10^6,
                                  "CPD_ly" = (sum(opti_const_grouped$Retro_Funding) - delist_CPD)/10^6,
                                  
                                  "fixed_lsm" = (sum(opti_op_lsm$Fixed) + sum(exc_brand_lsm$Display_Cost))/10^6,
                                  "fixed_nonlsm" = (sum(opti_op_nonlsm$Fixed) + sum(exc_brand_nonlsm$Display_Cost))/10^6,
                                  "fixed_ly" = (sum(opti_const_grouped$Display) - delist_fixed)/10^6,
                                  
                                  "BM_lsm" = (sum(opti_op_lsm$`Back Margin`) + sum(exc_brand_lsm$FM_Total) + sum(exc_brand_lsm$Display_Cost))/10^6,
                                  "BM_nonlsm" = (sum(opti_op_nonlsm$`Back Margin`) + sum(exc_brand_nonlsm$FM_Total) + sum(exc_brand_nonlsm$Display_Cost))/10^6,
                                  "BM_ly" = (sum(opti_const_grouped$FM) - delist_FM + sum(opti_const_grouped$Display) - delist_fixed)/10^6,
                                  
                                  "BM%_lsm" = (sum(opti_op_lsm$`Back Margin`) + sum(exc_brand_lsm$FM_Total) + sum(exc_brand_lsm$Display_Cost))*100/(sum(opti_op_lsm$RSV/1.2) + sum(exc_brand_lsm$Retailer_Revenue/1.2)),
                                  "BM%_nonlsm" = (sum(opti_op_nonlsm$`Back Margin`) + sum(exc_brand_nonlsm$FM_Total) + sum(exc_brand_nonlsm$Display_Cost))*100/(sum(opti_op_nonlsm$RSV/1.2) + sum(exc_brand_nonlsm$Retailer_Revenue/1.2)),
                                  "BM%_ly" = (sum(opti_const_grouped$FM) + sum(opti_const_grouped$Display) - delist_FM - delist_fixed)*100/(sum(opti_const_grouped$Retailer_Revenue/1.2) - (delist_RSV/1.2)),
                                  
                                  "FM%_lsm" = (sum(opti_op_lsm$`Front Margin`) + sum(exc_brand_lsm$FM_Total))*100/(sum(opti_op_lsm$RSV/1.2) + sum(exc_brand_lsm$Retailer_Revenue/1.2)),
                                  "FM%_nonlsm" = (sum(opti_op_nonlsm$`Front Margin`) + sum(exc_brand_nonlsm$FM_Total))*100/(sum(opti_op_nonlsm$RSV/1.2) + sum(exc_brand_nonlsm$Retailer_Revenue/1.2)),
                                  "FM%_ly" = (sum(opti_const_grouped$FM) - delist_FM)*100/(sum(opti_const_grouped$Retailer_Revenue/1.2) - (delist_RSV/1.2)),check.names = FALSE)
    
  }else if(delist_selection == TRUE & exclude_selection == TRUE){
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  
                                  "RSV_lsm" = (sum(opti_op_lsm$RSV) + sum(exc_brand_lsm$Retailer_Revenue))/10^6,
                                  "RSV_nonlsm" = (sum(opti_op_nonlsm$RSV) + sum(exc_brand_nonlsm$Retailer_Revenue))/10^6,
                                  "RSV_ly" = (sum(opti_const_grouped$Retailer_Revenue))/10^6,
                                  
                                  "COGS_lsm" = (sum(opti_op_lsm$COGS) + sum(exc_brand_lsm$Net_Cost_Total))/10^6,
                                  "COGS_nonlsm" = (sum(opti_op_nonlsm$COGS) + sum(exc_brand_nonlsm$Net_Cost_Total))/10^6,
                                  "COGS_ly" = sum(opti_const_grouped$Net_Cost_Total)/10^6,
                                  
                                  "FM_lsm" = (sum(opti_op_lsm$`Front Margin`) + sum(exc_brand_lsm$FM_Total))/10^6,
                                  "FM_nonlsm" = (sum(opti_op_nonlsm$`Front Margin`) + sum(exc_brand_nonlsm$FM_Total))/10^6,
                                  "FM_ly" = (sum(opti_const_grouped$FM))/10^6,
                                  
                                  "CPD_lsm" = (sum(opti_op_lsm$CPD) + sum(exc_brand_lsm$Retro_Fund_Total))/10^6,
                                  "CPD_nonlsm" = (sum(opti_op_nonlsm$CPD) + sum(exc_brand_nonlsm$Retro_Fund_Total))/10^6,
                                  "CPD_ly" = (sum(opti_const_grouped$Retro_Funding))/10^6,
                                  
                                  "fixed_lsm" = (sum(opti_op_lsm$Fixed) + sum(exc_brand_lsm$Display_Cost))/10^6,
                                  "fixed_nonlsm" = (sum(opti_op_nonlsm$Fixed) + sum(exc_brand_nonlsm$Display_Cost))/10^6,
                                  "fixed_ly" = (sum(opti_const_grouped$Display))/10^6,
                                  
                                  "BM_lsm" = (sum(opti_op_lsm$`Back Margin`) + sum(exc_brand_lsm$FM_Total) + sum(exc_brand_lsm$Display_Cost))/10^6,
                                  "BM_nonlsm" = (sum(opti_op_nonlsm$`Back Margin`) + sum(exc_brand_nonlsm$FM_Total) + sum(exc_brand_nonlsm$Display_Cost))/10^6,
                                  "BM_ly" = (sum(opti_const_grouped$FM) + sum(opti_const_grouped$Display))/10^6,
                                  
                                  "BM%_lsm" = (sum(opti_op_lsm$`Back Margin`) + sum(exc_brand_lsm$FM_Total) + sum(exc_brand_lsm$Display_Cost))*100/(sum(opti_op_lsm$RSV/1.2) + sum(exc_brand_lsm$Retailer_Revenue/1.2)),
                                  "BM%_nonlsm" = (sum(opti_op_nonlsm$`Back Margin`) + sum(exc_brand_nonlsm$FM_Total) + sum(exc_brand_nonlsm$Display_Cost))*100/(sum(opti_op_nonlsm$RSV/1.2) + sum(exc_brand_nonlsm$Retailer_Revenue/1.2)),
                                  "BM%_ly" = (sum(opti_const_grouped$FM) + sum(opti_const_grouped$Display))*100/(sum(opti_const_grouped$Retailer_Revenue/1.2)),
                                  
                                  "FM%_lsm" = (sum(opti_op_lsm$`Front Margin`) + sum(exc_brand_lsm$FM_Total))*100/(sum(opti_op_lsm$RSV/1.2) + sum(exc_brand_lsm$Retailer_Revenue/1.2)),
                                  "FM%_nonlsm" = (sum(opti_op_nonlsm$`Front Margin`) + sum(exc_brand_nonlsm$FM_Total))*100/(sum(opti_op_nonlsm$RSV/1.2) + sum(exc_brand_nonlsm$Retailer_Revenue/1.2)),
                                  "FM%_ly" = (sum(opti_const_grouped$FM))*100/(sum(opti_const_grouped$Retailer_Revenue/1.2)),check.names = FALSE)
    
  }else if(delist_selection == FALSE & exclude_selection == FALSE){
    
    delist_RSV <- sum(delist_sku$Retailer_Revenue)
    delist_Net_Cost <- sum(delist_sku$Net_Cost_Total)
    delist_FM <- sum(delist_sku$FM_Total)
    delist_CPD <- sum(delist_sku$Retro_Fund_Unit * delist_sku$Units)
    delist_fixed <- sum(delist_sku$R_Display_Fee_Total)
    
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  
                                  "RSV_lsm" = (sum(opti_op_lsm$RSV))/10^6,
                                  "RSV_nonlsm" = (sum(opti_op_nonlsm$RSV))/10^6,
                                  "RSV_ly" = (sum(opti_const_grouped$Retailer_Revenue) - delist_RSV - sum(exc_brand_nonlsm$Retailer_Revenue))/10^6,
                                  
                                  "COGS_lsm" = (sum(opti_op_lsm$COGS))/10^6,
                                  "COGS_nonlsm" = (sum(opti_op_nonlsm$COGS))/10^6,
                                  "COGS_ly" = (sum(opti_const_grouped$Net_Cost_Total) - delist_Net_Cost - sum(exc_brand_nonlsm$Net_Cost_Total))/10^6,
                                  
                                  "FM_lsm" = (sum(opti_op_lsm$`Front Margin`))/10^6,
                                  "FM_nonlsm" = (sum(opti_op_nonlsm$`Front Margin`))/10^6,
                                  "FM_ly" = (sum(opti_const_grouped$FM) - delist_FM - sum(exc_brand_nonlsm$FM_Total))/10^6,
                                  
                                  "CPD_lsm" = sum(opti_op_lsm$CPD)/10^6,
                                  "CPD_nonlsm" = sum(opti_op_nonlsm$CPD)/10^6,
                                  "CPD_ly" = (sum(opti_const_grouped$Retro_Funding) - delist_CPD - sum(exc_brand_nonlsm$Retro_Fund_Total))/10^6,
                                  
                                  "fixed_lsm" = (sum(opti_op_lsm$Fixed))/10^6,
                                  "fixed_nonlsm" = (sum(opti_op_nonlsm$Fixed))/10^6,
                                  "fixed_ly" = (sum(opti_const_grouped$Display) - delist_fixed - sum(exc_brand_nonlsm$Display_Cost))/10^6,
                                  
                                  "BM_lsm" = (sum(opti_op_lsm$`Back Margin`))/10^6,
                                  "BM_nonlsm" = (sum(opti_op_nonlsm$`Back Margin`))/10^6,
                                  "BM_ly" = (sum(opti_const_grouped$FM) - delist_FM + sum(opti_const_grouped$Display) - delist_fixed - sum(exc_brand_nonlsm$FM_Total) - sum(exc_brand_nonlsm$Display_Cost))/10^6,
                                  
                                  "BM%_lsm" = (sum(opti_op_lsm$`Back Margin`))*100/(sum(opti_op_lsm$RSV/1.2)),
                                  "BM%_nonlsm" = (sum(opti_op_nonlsm$`Back Margin`))*100/(sum(opti_op_nonlsm$RSV/1.2)),
                                  "BM%_ly" = (sum(opti_const_grouped$FM) + sum(opti_const_grouped$Display) - delist_FM - delist_fixed - sum(exc_brand_nonlsm$FM_Total) - sum(exc_brand_nonlsm$Display_Cost))*100/(sum(opti_const_grouped$Retailer_Revenue/1.2) - (delist_RSV/1.2) - sum(exc_brand_nonlsm$Retailer_Revenue/1.2)),
                                  
                                  "FM%_lsm" = (sum(opti_op_lsm$`Front Margin`))*100/(sum(opti_op_lsm$RSV/1.2)),
                                  "FM%_nonlsm" = (sum(opti_op_nonlsm$`Front Margin`))*100/(sum(opti_op_nonlsm$RSV/1.2)),
                                  "FM%_ly" = (sum(opti_const_grouped$FM) - delist_FM - sum(exc_brand_nonlsm$FM_Total))*100/(sum(opti_const_grouped$Retailer_Revenue/1.2) - (delist_RSV/1.2) - sum(exc_brand_nonlsm$Retailer_Revenue/1.2)),check.names = FALSE)
    
  }else if(delist_selection == TRUE & exclude_selection == FALSE){
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  
                                  "RSV_lsm" = (sum(opti_op_lsm$RSV))/10^6,
                                  "RSV_nonlsm" = (sum(opti_op_nonlsm$RSV))/10^6,
                                  "RSV_ly" = (sum(opti_const_grouped$Retailer_Revenue) - sum(exc_brand_nonlsm$Retailer_Revenue))/10^6,
                                  
                                  "COGS_lsm" = (sum(opti_op_lsm$COGS))/10^6,
                                  "COGS_nonlsm" = (sum(opti_op_nonlsm$COGS))/10^6,
                                  "COGS_ly" = (sum(opti_const_grouped$Net_Cost_Total) - sum(exc_brand_nonlsm$Net_Cost_Total))/10^6,
                                  
                                  "FM_lsm" = (sum(opti_op_lsm$`Front Margin`))/10^6,
                                  "FM_nonlsm" = (sum(opti_op_nonlsm$`Front Margin`))/10^6,
                                  "FM_ly" = (sum(opti_const_grouped$FM) - sum(exc_brand_nonlsm$FM_Total))/10^6,
                                  
                                  "CPD_lsm" = sum(opti_op_lsm$CPD)/10^6,
                                  "CPD_nonlsm" = sum(opti_op_nonlsm$CPD)/10^6,
                                  "CPD_ly" = (sum(opti_const_grouped$Retro_Funding) - sum(exc_brand_nonlsm$Retro_Fund_Total))/10^6,
                                  
                                  "fixed_lsm" = (sum(opti_op_lsm$Fixed))/10^6,
                                  "fixed_nonlsm" = (sum(opti_op_nonlsm$Fixed))/10^6,
                                  "fixed_ly" = (sum(opti_const_grouped$Display) - sum(exc_brand_nonlsm$Display_Cost))/10^6,
                                  
                                  "BM_lsm" = (sum(opti_op_lsm$`Back Margin`))/10^6,
                                  "BM_nonlsm" = (sum(opti_op_nonlsm$`Back Margin`))/10^6,
                                  "BM_ly" = (sum(opti_const_grouped$FM) + sum(opti_const_grouped$Display) - sum(exc_brand_nonlsm$FM_Total) - sum(exc_brand_nonlsm$Display_Cost))/10^6,
                                  
                                  "BM%_lsm" = (sum(opti_op_lsm$`Back Margin`))*100/(sum(opti_op_lsm$RSV/1.2)),
                                  "BM%_nonlsm" = (sum(opti_op_nonlsm$`Back Margin`))*100/(sum(opti_op_nonlsm$RSV/1.2)),
                                  "BM%_ly" = (sum(opti_const_grouped$FM) + sum(opti_const_grouped$Display) - sum(exc_brand_nonlsm$FM_Total) - sum(exc_brand_nonlsm$Display_Cost))*100/(sum(opti_const_grouped$Retailer_Revenue/1.2) - sum(exc_brand_nonlsm$Retailer_Revenue/1.2)),
                                  
                                  "FM%_lsm" = (sum(opti_op_lsm$`Front Margin`))*100/(sum(opti_op_lsm$RSV/1.2)),
                                  "FM%_nonlsm" = (sum(opti_op_nonlsm$`Front Margin`))*100/(sum(opti_op_nonlsm$RSV/1.2)),
                                  "FM%_ly" = (sum(opti_const_grouped$FM) - sum(exc_brand_nonlsm$FM_Total))*100/(sum(opti_const_grouped$Retailer_Revenue/1.2) - sum(exc_brand_nonlsm$Retailer_Revenue/1.2)),check.names = FALSE)
    
  }
  
  return(KPI_calculation)
}

KPI_calc_tbo <- function(tpo_list,tpo_list_old,opti_op_tbo,exc_brand_tbo,opti_const_grouped,delist_sku,ROI_selected,planned_select,optimized_select,opti_const_tbo,exclude_selection){
  if(ROI_selected == "Incremental NR ROI"){
    opti_op_tbo$Inc_GM_Abs <- opti_op_tbo$Inc_Revenue
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_Revenue
  }else if(ROI_selected == "Incremental NIS ROI"){
    opti_op_tbo$Inc_GM_Abs <- opti_op_tbo$Inc_NIS
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_NIS
  }
  
  
  if(exclude_selection == TRUE){
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  
                                  "net_sales_actual" = opti_const_tbo[opti_const_tbo$KPI == "Scan Net Revenue",]$`Actuals till date`/10^6,
                                  "net_sales_planned" = sum(planned_select$Net_Revenue)/10^6,
                                  "net_sales_optimized" = sum(optimized_select$Net_Revenue)/10^6,
                                  "net_sales_old" = (sum(tpo_list_old$opti_output$Net_Revenue) + sum(tpo_list_old$excluded_brand$Net_Revenue))/10^6,
                                  "net_sales_new" = (sum(opti_op_tbo$Net_Revenue) + sum(exc_brand_tbo$Net_Revenue))/10^6,
                                  #"net_sales_old" = ifelse(tpo_list_old$goal_shiny != "Scan Net Revenue",(tpo_list_old$opti_const_shiny[tpo_list_old$opti_const_shiny$KPI == "Scan Net Revenue",]$`Last Year Value`- delist_NR),opti_const_grouped$Net_Revenue - delist_NR),
                                  "net_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Minimum Value`,""),
                                  "net_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Maximum Value`,""),
                                  
                                  "gm_%_actual" = opti_const_tbo[opti_const_tbo$KPI == "Gross Margin % of NR",]$`Actuals till date`,
                                  "gm_%_planned" = sum(planned_select$GM_Abs) * 100/sum(planned_select$Net_Revenue),
                                  "gm_%_optimized" = sum(optimized_select$GM_Abs) * 100/sum(optimized_select$Net_Revenue),
                                  "gm_%_old" = (sum(tpo_list_old$opti_output$GM_Abs) + sum(tpo_list_old$excluded_brand$GM_Abs)) * 100/(sum(tpo_list_old$opti_output$Net_Revenue) + sum(tpo_list_old$excluded_brand$Net_Revenue)),
                                  "gm_%_new" = (sum(opti_op_tbo$GM_Abs) + sum(exc_brand_tbo$GM_Abs)) * 100/(sum(opti_op_tbo$Net_Revenue) + sum(exc_brand_tbo$Net_Revenue)),
                                  #"gm_%_ly" = ((opti_const_grouped$GM_Abs - delist_GM_Abs)*100/(opti_const_grouped$Net_Revenue - delist_NR)),
                                  "gm_%_min" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Minimum Value`,""),
                                  "gm_%_max" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Maximum Value`,""),
                                  
                                  "gross_sales_actual" = opti_const_tbo[opti_const_tbo$KPI == "Scan Gross Sales",]$`Actuals till date`/10^6,
                                  "gross_sales_planned" = sum(planned_select$Gross_Sales)/10^6,
                                  "gross_sales_optimized" = sum(optimized_select$Gross_Sales)/10^6,
                                  "gross_sales_old" = (sum(tpo_list_old$opti_output$Gross_Sales) + sum(tpo_list_old$excluded_brand$Gross_Sales))/10^6,
                                  "gross_sales_new" = (sum(opti_op_tbo$Gross_Sales) + sum(exc_brand_tbo$Gross_Sales))/10^6,
                                  #"gross_sales_ly" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Last Year Value` - delist_Gross_Sales,opti_const_grouped$Gross_Sales - delist_Gross_Sales),
                                  "gross_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Minimum Value`,""),
                                  "gross_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Maximum Value`,""),
                                  
                                  "gm_actual" = opti_const_tbo[opti_const_tbo$KPI == "Gross Margin",]$`Actuals till date`/10^6,
                                  "gm_planned" = (sum(planned_select$GM_Abs))/10^6,
                                  "gm_optimized" = (sum(optimized_select$GM_Abs))/10^6,
                                  "gm_old" = (sum(tpo_list_old$opti_output$GM_Abs) + sum(tpo_list_old$excluded_brand$GM_Abs))/10^6,
                                  "gm_new" = (sum(opti_op_tbo$GM_Abs) + sum(exc_brand_tbo$GM_Abs))/10^6,
                                  #"gm_ly" = ((opti_const_grouped$GM_Abs - delist_GM_Abs)),
                                  "gm_min" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Minimum Value`,""),
                                  "gm_max" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Maximum Value`,""),
                                  
                                  "spend_%_actual" = opti_const_tbo[opti_const_tbo$KPI == "Trade Spend % of NR",]$`Actuals till date`,
                                  "spend_%_planned" = (sum(planned_select$Total_Trade_Investment))* 100/(sum(planned_select$Net_Revenue)),
                                  "spend_%_optimized" = (sum(optimized_select$Total_Trade_Investment))* 100/(sum(optimized_select$Net_Revenue)),
                                  "spend_%_old" = (sum(tpo_list_old$opti_output$Total_Trade_Investment) + sum(tpo_list_old$excluded_brand$Trade_Investment))* 100/(sum(tpo_list_old$opti_output$Net_Revenue) + sum(tpo_list_old$excluded_brand$Net_Revenue)),
                                  "spend_%_new" = (sum(opti_op_tbo$Total_Trade_Investment) + sum(exc_brand_tbo$Trade_Investment))* 100/(sum(opti_op_tbo$Net_Revenue) + sum(exc_brand_tbo$Net_Revenue)),
                                  #"spend_%_ly" = ((opti_const_grouped$Trade_Investment - delist_TI)*100/(opti_const_grouped$Net_Revenue - delist_NR)),
                                  "spend_%_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Minimum Value`,""),
                                  "spend_%_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Maximum Value`,""),
                                  
                                  "spend_%NIS_actual" = opti_const_tbo[opti_const_tbo$KPI == "Trade Spend % of NIS",]$`Actuals till date`,
                                  "spend_%NIS_planned" = (sum(planned_select$Total_Trade_Investment))* 100/(sum(planned_select$NIS)),
                                  "spend_%NIS_optimized" = (sum(optimized_select$Total_Trade_Investment))* 100/(sum(optimized_select$NIS)),
                                  "spend_%NIS_old" = (sum(tpo_list_old$opti_output$Total_Trade_Investment) + sum(tpo_list_old$excluded_brand$Trade_Investment))* 100/(sum(tpo_list_old$opti_output$NIS) + sum(tpo_list_old$excluded_brand$NIS)),
                                  "spend_%NIS_new" = (sum(opti_op_tbo$Total_Trade_Investment) + sum(exc_brand_tbo$Trade_Investment))* 100/(sum(opti_op_tbo$NIS) + sum(exc_brand_tbo$NIS)),
                                  #"spend_%NIS_ly" = ((opti_const_grouped$Trade_Investment - delist_TI)*100/(opti_const_grouped$NIS - delist_NIS)),
                                  "spend_%NIS_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Minimum Value`,""),
                                  "spend_%NIS_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Maximum Value`,""),
                                  
                                  "ROI_actual" = opti_const_tbo[opti_const_tbo$KPI == ROI_selected,]$`Actuals till date`,
                                  "ROI_planned" = (sum(planned_select$Inc_GM_Abs))/(sum(planned_select$Total_Trade_Investment)),
                                  "ROI_optimized" = (sum(optimized_select$Inc_GM_Abs))/(sum(optimized_select$Total_Trade_Investment)),
                                  "ROI_old" = (sum(tpo_list_old$opti_output$Inc_GM_Abs) + sum(tpo_list_old$excluded_brand$Inc_GM_Abs))/(sum(tpo_list_old$opti_output$Total_Trade_Investment) + sum(tpo_list_old$excluded_brand$Trade_Investment)),
                                  "ROI_new" = (sum(opti_op_tbo$Inc_GM_Abs) + sum(exc_brand_tbo$Inc_GM_Abs))/(sum(opti_op_tbo$Total_Trade_Investment) + sum(exc_brand_tbo$Trade_Investment)),
                                  #"ROI_ly" = ((opti_const_grouped$Inc_GM_Abs - delist_Inc)/(opti_const_grouped$Trade_Investment - delist_TI)),
                                  "ROI_min" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Minimum Value`,""),
                                  "ROI_max" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Maximum Value`,""),
                                  
                                  "vol_sales_actual" = opti_const_tbo[opti_const_tbo$KPI == "Volume Sales",]$`Actuals till date`/10^6,
                                  "vol_sales_planned" = (sum(planned_select$Total_Sales))/10^6,
                                  "vol_sales_optimized" = (sum(optimized_select$Total_Sales))/10^6,
                                  "vol_sales_old" = (sum(tpo_list_old$opti_output$Total_Sales) + sum(tpo_list_old$excluded_brand$Units))/10^6,
                                  "vol_sales_new" = (sum(opti_op_tbo$Total_Sales) + sum(exc_brand_tbo$Units))/10^6,
                                  #"vol_sales_ly" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Last Year Value` - delist_units,opti_const_grouped$CY_Volume - delist_units),
                                  "vol_sales_min" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Minimum Value`,""),
                                  "vol_sales_max" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Maximum Value`,""),
                                  
                                  "market_share_actual" = opti_const_tbo[opti_const_tbo$KPI == "Value Market Share",]$`Actuals till date`,
                                  "market_share_planned" = (sum(planned_select$Retailer_Revenue))*100/(sum(planned_select$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_optimized" = (sum(optimized_select$Retailer_Revenue))*100/(sum(optimized_select$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_old" = (sum(tpo_list_old$opti_output$Retailer_Revenue) + sum(tpo_list_old$excluded_brand$Retailer_Revenue))*100/(sum(tpo_list_old$opti_output$Retailer_Revenue) + sum(tpo_list_old$excluded_brand$Retailer_Revenue) + (tpo_list_old$other_sales_value)),
                                  "market_share_new" = (sum(opti_op_tbo$Retailer_Revenue) + sum(exc_brand_tbo$Retailer_Revenue))*100/(sum(opti_op_tbo$Retailer_Revenue) + sum(exc_brand_tbo$Retailer_Revenue) + (tpo_list_old$other_sales_value)),
                                  #"market_share_ly" = (opti_const_grouped$CY_Volume - delist_units)* 100/(opti_const_grouped$CY_Volume - delist_units + (tpo_list$other_sales)),
                                  "market_share_min" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Minimum Value`,""),
                                  "market_share_max" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Maximum Value`,""),check.names = FALSE)
  }else{
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  
                                  "net_sales_actual" = opti_const_tbo[opti_const_tbo$KPI == "Scan Net Revenue",]$`Actuals till date`/10^6,
                                  "net_sales_planned" = sum(planned_select$Net_Revenue)/10^6,
                                  "net_sales_optimized" = sum(optimized_select$Net_Revenue)/10^6,
                                  "net_sales_old" = sum(tpo_list_old$opti_output$Net_Revenue)/10^6,
                                  "net_sales_new" = sum(opti_op_tbo$Net_Revenue)/10^6,
                                  #"net_sales_old" = ifelse(tpo_list_old$goal_shiny != "Scan Net Revenue",(tpo_list_old$opti_const_shiny[tpo_list_old$opti_const_shiny$KPI == "Scan Net Revenue",]$`Last Year Value`- delist_NR),opti_const_grouped$Net_Revenue - delist_NR),
                                  "net_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Minimum Value`,""),
                                  "net_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Net Revenue",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Net Revenue",]$`Maximum Value`,""),
                                  
                                  "gm_%_actual" = opti_const_tbo[opti_const_tbo$KPI == "Gross Margin % of NR",]$`Actuals till date`,
                                  "gm_%_planned" = sum(planned_select$GM_Abs) * 100/sum(planned_select$Net_Revenue),
                                  "gm_%_optimized" = sum(optimized_select$GM_Abs) * 100/sum(optimized_select$Net_Revenue),
                                  "gm_%_old" = (sum(tpo_list_old$opti_output$GM_Abs)) * 100/(sum(tpo_list_old$opti_output$Net_Revenue)),
                                  "gm_%_new" = (sum(opti_op_tbo$GM_Abs)) * 100/(sum(opti_op_tbo$Net_Revenue)),
                                  #"gm_%_ly" = ((opti_const_grouped$GM_Abs - delist_GM_Abs)*100/(opti_const_grouped$Net_Revenue - delist_NR)),
                                  "gm_%_min" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Minimum Value`,""),
                                  "gm_%_max" = ifelse(tpo_list$goal_shiny != "Gross Margin % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin % of NR",]$`Maximum Value`,""),
                                  
                                  "gross_sales_actual" = opti_const_tbo[opti_const_tbo$KPI == "Scan Gross Sales",]$`Actuals till date`/10^6,
                                  "gross_sales_planned" = sum(planned_select$Gross_Sales)/10^6,
                                  "gross_sales_optimized" = sum(optimized_select$Gross_Sales)/10^6,
                                  "gross_sales_old" = sum(tpo_list_old$opti_output$Gross_Sales)/10^6,
                                  "gross_sales_new" = sum(opti_op_tbo$Gross_Sales)/10^6,
                                  #"gross_sales_ly" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Last Year Value` - delist_Gross_Sales,opti_const_grouped$Gross_Sales - delist_Gross_Sales),
                                  "gross_sales_min" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Minimum Value`,""),
                                  "gross_sales_max" = ifelse(tpo_list$goal_shiny != "Scan Gross Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Scan Gross Sales",]$`Maximum Value`,""),
                                  
                                  "gm_actual" = opti_const_tbo[opti_const_tbo$KPI == "Gross Margin",]$`Actuals till date`/10^6,
                                  "gm_planned" = (sum(planned_select$GM_Abs))/10^6,
                                  "gm_optimized" = (sum(optimized_select$GM_Abs))/10^6,
                                  "gm_old" = (sum(tpo_list_old$opti_output$GM_Abs))/10^6,
                                  "gm_new" = (sum(opti_op_tbo$GM_Abs))/10^6,
                                  #"gm_ly" = ((opti_const_grouped$GM_Abs - delist_GM_Abs)),
                                  "gm_min" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Minimum Value`,""),
                                  "gm_max" = ifelse(tpo_list$goal_shiny != "Gross Margin",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Gross Margin",]$`Maximum Value`,""),
                                  
                                  "spend_%_actual" = opti_const_tbo[opti_const_tbo$KPI == "Trade Spend % of NR",]$`Actuals till date`,
                                  "spend_%_planned" = (sum(planned_select$Total_Trade_Investment))* 100/(sum(planned_select$Net_Revenue)),
                                  "spend_%_optimized" = (sum(optimized_select$Total_Trade_Investment))* 100/(sum(optimized_select$Net_Revenue)),
                                  "spend_%_old" = (sum(tpo_list_old$opti_output$Total_Trade_Investment))* 100/(sum(tpo_list_old$opti_output$Net_Revenue)),
                                  "spend_%_new" = (sum(opti_op_tbo$Total_Trade_Investment))* 100/(sum(opti_op_tbo$Net_Revenue)),
                                  #"spend_%_ly" = ((opti_const_grouped$Trade_Investment - delist_TI)*100/(opti_const_grouped$Net_Revenue - delist_NR)),
                                  "spend_%_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Minimum Value`,""),
                                  "spend_%_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NR",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NR",]$`Maximum Value`,""),
                                  
                                  "spend_%NIS_actual" = opti_const_tbo[opti_const_tbo$KPI == "Trade Spend % of NIS",]$`Actuals till date`,
                                  "spend_%NIS_planned" = (sum(planned_select$Total_Trade_Investment))* 100/(sum(planned_select$NIS)),
                                  "spend_%NIS_optimized" = (sum(optimized_select$Total_Trade_Investment))* 100/(sum(optimized_select$NIS)),
                                  "spend_%NIS_old" = (sum(tpo_list_old$opti_output$Total_Trade_Investment))* 100/(sum(tpo_list_old$opti_output$NIS)),
                                  "spend_%NIS_new" = (sum(opti_op_tbo$Total_Trade_Investment))* 100/(sum(opti_op_tbo$NIS)),
                                  #"spend_%NIS_ly" = ((opti_const_grouped$Trade_Investment - delist_TI)*100/(opti_const_grouped$NIS - delist_NIS)),
                                  "spend_%NIS_min" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Minimum Value`,""),
                                  "spend_%NIS_max" = ifelse(tpo_list$goal_shiny != "Trade Spend % of NIS",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Trade Spend % of NIS",]$`Maximum Value`,""),
                                  
                                  "ROI_actual" = opti_const_tbo[opti_const_tbo$KPI == ROI_selected,]$`Actuals till date`,
                                  "ROI_planned" = (sum(planned_select$Inc_GM_Abs))/(sum(planned_select$Total_Trade_Investment)),
                                  "ROI_optimized" = (sum(optimized_select$Inc_GM_Abs))/(sum(optimized_select$Total_Trade_Investment)),
                                  "ROI_old" = (sum(tpo_list_old$opti_output$Inc_GM_Abs))/(sum(tpo_list_old$opti_output$Total_Trade_Investment)),
                                  "ROI_new" = (sum(opti_op_tbo$Inc_GM_Abs))/(sum(opti_op_tbo$Total_Trade_Investment)),
                                  #"ROI_ly" = ((opti_const_grouped$Inc_GM_Abs - delist_Inc)/(opti_const_grouped$Trade_Investment - delist_TI)),
                                  "ROI_min" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Minimum Value`,""),
                                  "ROI_max" = ifelse(tpo_list$goal_shiny != ROI_selected,tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == ROI_selected,]$`Maximum Value`,""),
                                  
                                  "vol_sales_actual" = opti_const_tbo[opti_const_tbo$KPI == "Volume Sales",]$`Actuals till date`/10^6,
                                  "vol_sales_planned" = (sum(planned_select$Total_Sales))/10^6,
                                  "vol_sales_optimized" = (sum(optimized_select$Total_Sales))/10^6,
                                  "vol_sales_old" = (sum(tpo_list_old$opti_output$Total_Sales))/10^6,
                                  "vol_sales_new" = (sum(opti_op_tbo$Total_Sales))/10^6,
                                  #"vol_sales_ly" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Last Year Value` - delist_units,opti_const_grouped$CY_Volume - delist_units),
                                  "vol_sales_min" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Minimum Value`,""),
                                  "vol_sales_max" = ifelse(tpo_list$goal_shiny != "Volume Sales",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Volume Sales",]$`Maximum Value`,""),
                                  
                                  "market_share_actual" = opti_const_tbo[opti_const_tbo$KPI == "Value Market Share",]$`Actuals till date`,
                                  "market_share_planned" = (sum(planned_select$Retailer_Revenue))*100/(sum(planned_select$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_optimized" = (sum(optimized_select$Retailer_Revenue))*100/(sum(optimized_select$Retailer_Revenue) + (tpo_list$other_sales_value)),
                                  "market_share_old" = (sum(tpo_list_old$opti_output$Retailer_Revenue))*100/(sum(tpo_list_old$opti_output$Retailer_Revenue) + (tpo_list_old$other_sales_value)),
                                  "market_share_new" = (sum(opti_op_tbo$Retailer_Revenue))*100/(sum(opti_op_tbo$Retailer_Revenue) + (tpo_list_old$other_sales_value)),
                                  #"market_share_ly" = (opti_const_grouped$CY_Volume - delist_units)* 100/(opti_const_grouped$CY_Volume - delist_units + (tpo_list$other_sales)),
                                  "market_share_min" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Minimum Value`,""),
                                  "market_share_max" = ifelse(tpo_list$goal_shiny != "Value Market Share",tpo_list$opti_const_shiny[tpo_list$opti_const_shiny$KPI == "Value Market Share",]$`Maximum Value`,""),check.names = FALSE)
    
  }
  
  return(KPI_calculation)
}

KPI_calc_ret_tbo <- function(tpo_list,tpo_list_old,opti_op_tbo,exc_brand_tbo,opti_const_grouped,delist_sku,ROI_selected,planned_select,optimized_select,opti_const_tbo,exclude_selection){
  
  if(ROI_selected == "Incremental NR ROI"){
    opti_op_tbo$Inc_GM_Abs <- opti_op_tbo$R_Net_Rev_Inc
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_Revenue
  }else if(ROI_selected == "Incremental NIS ROI"){
    opti_op_tbo$Inc_GM_Abs <- opti_op_tbo$R_NIS_Inc
    opti_const_grouped$Inc_GM_Abs <- opti_const_grouped$Inc_NIS
  }
  
  if(exclude_selection == TRUE){
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  
                                  "RSV_actual" = "",
                                  "RSV_planned" = sum(planned_select$Retailer_Revenue)/10^6,
                                  "RSV_optimized" = sum(optimized_select$Retailer_Revenue)/10^6,
                                  "RSV_old" = (sum(tpo_list_old$opti_output$Retailer_Revenue) + sum(tpo_list_old$excluded_brand$Retailer_Revenue))/10^6,
                                  "RSV_new" = (sum(opti_op_tbo$Retailer_Revenue) + sum(exc_brand_tbo$Retailer_Revenue))/10^6,
                                  
                                  "COGS_actual" = "",
                                  "COGS_planned" = sum(planned_select$Net_Cost_Unit * planned_select$Total_Sales)/10^6,
                                  "COGS_optimized" = sum(optimized_select$Net_Cost_Unit * optimized_select$Total_Sales)/10^6,
                                  "COGS_old" = (sum(tpo_list_old$opti_output$Net_Cost_Unit * tpo_list_old$opti_output$Total_Sales) + sum(tpo_list_old$excluded_brand$Net_Cost_Total))/10^6,
                                  "COGS_new" = (sum(opti_op_tbo$Net_Cost_Unit * opti_op_tbo$Total_Sales) + sum(exc_brand_tbo$Net_Cost_Unit * exc_brand_tbo$Total_Sales))/10^6,
                                  
                                  "FM_actual" = "",
                                  "FM_planned" = sum(planned_select$FM_Total)/10^6,
                                  "FM_optimized" = sum(optimized_select$FM_Total)/10^6,
                                  "FM_old" = (sum(tpo_list_old$opti_output$FM_Total) + sum(tpo_list_old$excluded_brand$FM_Total))/10^6,
                                  "FM_new" = (sum(opti_op_tbo$FM_Total) + sum(exc_brand_tbo$FM_Total))/10^6,
                                  
                                  "CPD_actual" = "",
                                  "CPD_planned" = sum(planned_select$Retro_Funding_Total)/10^6,
                                  "CPD_optimized" = sum(optimized_select$Retro_Funding_Total)/10^6,
                                  "CPD_old" = (sum(tpo_list_old$opti_output$Retro_Funding_Total) + sum(tpo_list_old$excluded_brand$Retro_Fund_Total))/10^6,
                                  "CPD_new" = (sum(opti_op_tbo$Retro_Funding_Total) + sum(exc_brand_tbo$Retro_Funding_Total))/10^6,
                                  
                                  "fixed_actual" = "",
                                  "fixed_planned" = sum(planned_select$Display_Cost)/10^6,
                                  "fixed_optimized" = sum(optimized_select$Display_Cost)/10^6,
                                  "fixed_old" = (sum(tpo_list_old$opti_output$Display_Cost) + sum(tpo_list_old$excluded_brand$Display_Cost))/10^6,
                                  "fixed_new" = (sum(opti_op_tbo$Display_Cost) + sum(exc_brand_tbo$Display_Cost))/10^6,
                                  
                                  "BM_actual" = "",
                                  "BM_planned" = (sum(planned_select$Display_Cost) + sum(planned_select$FM_Total))/10^6,
                                  "BM_optimized" = (sum(optimized_select$Display_Cost) + sum(optimized_select$FM_Total))/10^6,
                                  "BM_old" = (sum(tpo_list_old$opti_output$Display_Cost) + sum(tpo_list_old$opti_output$FM_Total) + sum(tpo_list_old$excluded_brand$Display_Cost) + sum(tpo_list_old$excluded_brand$FM_Total))/10^6,
                                  "BM_new" = (sum(opti_op_tbo$Display_Cost) + sum(opti_op_tbo$FM_Total) + sum(exc_brand_tbo$Display_Cost) + sum(exc_brand_tbo$FM_Total))/10^6,
                                  
                                  
                                  "BM%_actual" = "",
                                  "BM%_planned" = (sum(planned_select$FM_Total) + sum(planned_select$Display_Cost))/sum(planned_select$Retailer_Revenue/1.2),
                                  "BM%_optimized" = (sum(optimized_select$FM_Total) + sum(optimized_select$Display_Cost))/sum(optimized_select$Retailer_Revenue/1.2),
                                  "BM%_old" = (sum(tpo_list_old$opti_output$FM_Total) + sum(tpo_list_old$excluded_brand$FM_Total) + sum(tpo_list_old$opti_output$Display_Cost) + sum(tpo_list_old$excluded_brand$Display_Cost))/(sum(tpo_list_old$opti_output$Retailer_Revenue/1.2) + sum(tpo_list_old$excluded_brand$Retailer_Revenue/1.2)),
                                  "BM%_new" = (sum(opti_op_tbo$FM_Total) + sum(exc_brand_tbo$FM_Total) + sum(opti_op_tbo$Display_Cost) + sum(exc_brand_tbo$Display_Cost))/(sum(opti_op_tbo$Retailer_Revenue/1.2) + sum(exc_brand_tbo$Retailer_Revenue/1.2)),
                                  
                                  "FM%_actual" = "",
                                  "FM%_planned" = sum(planned_select$FM_Total)/sum(planned_select$Retailer_Revenue/1.2),
                                  "FM%_optimized" = sum(optimized_select$FM_Total)/sum(optimized_select$Retailer_Revenue/1.2),
                                  "FM%_old" = (sum(tpo_list_old$opti_output$FM_Total) + sum(tpo_list_old$excluded_brand$FM_Total))/(sum(tpo_list_old$opti_output$Retailer_Revenue/1.2) + sum(tpo_list_old$excluded_brand$Retailer_Revenue/1.2)),
                                  "FM%_new" = (sum(opti_op_tbo$FM_Total) + sum(exc_brand_tbo$FM_Total))/(sum(opti_op_tbo$Retailer_Revenue/1.2) + sum(exc_brand_tbo$Retailer_Revenue/1.2)),check.names = FALSE)
    
  }else{
    KPI_calculation <- data.frame("goal" = tpo_list$goal_shiny,
                                  "sign" = tpo_list$sign,
                                  
                                  "RSV_actual" = "",
                                  "RSV_planned" = sum(planned_select$Retailer_Revenue)/10^6,
                                  "RSV_optimized" = sum(optimized_select$Retailer_Revenue)/10^6,
                                  "RSV_old" = sum(tpo_list_old$opti_output$Retailer_Revenue)/10^6,
                                  "RSV_new" = sum(opti_op_tbo$Retailer_Revenue)/10^6,
                                  
                                  "COGS_actual" = "",
                                  "COGS_planned" = sum(planned_select$Net_Cost_Unit * planned_select$Total_Sales)/10^6,
                                  "COGS_optimized" = sum(optimized_select$Net_Cost_Unit * optimized_select$Total_Sales)/10^6,
                                  "COGS_old" = sum(tpo_list_old$opti_output$Net_Cost_Unit * tpo_list_old$opti_output$Total_Sales)/10^6,
                                  "COGS_new" = sum(opti_op_tbo$Net_Cost_Unit * opti_op_tbo$Total_Sales)/10^6,
                                  
                                  "FM_actual" = "",
                                  "FM_planned" = sum(planned_select$FM_Total)/10^6,
                                  "FM_optimized" = sum(optimized_select$FM_Total)/10^6,
                                  "FM_old" = sum(tpo_list_old$opti_output$FM_Total)/10^6,
                                  "FM_new" = sum(opti_op_tbo$FM_Total)/10^6,
                                  
                                  "CPD_actual" = "",
                                  "CPD_planned" = sum(planned_select$Retro_Funding_Total)/10^6,
                                  "CPD_optimized" = sum(optimized_select$Retro_Funding_Total)/10^6,
                                  "CPD_old" = sum(tpo_list_old$opti_output$Retro_Funding_Total)/10^6,
                                  "CPD_new" = sum(opti_op_tbo$Retro_Funding_Total)/10^6,
                                  
                                  "fixed_actual" = "",
                                  "fixed_planned" = sum(planned_select$Display_Cost)/10^6,
                                  "fixed_optimized" = sum(optimized_select$Display_Cost)/10^6,
                                  "fixed_old" = sum(tpo_list_old$opti_output$Display_Cost)/10^6,
                                  "fixed_new" = sum(opti_op_tbo$Display_Cost)/10^6,
                                  
                                  "BM_actual" = "",
                                  "BM_planned" = (sum(planned_select$Display_Cost) + sum(planned_select$FM_Total))/10^6,
                                  "BM_optimized" = (sum(optimized_select$Display_Cost) + sum(optimized_select$FM_Total))/10^6,
                                  "BM_old" = (sum(tpo_list_old$opti_output$Display_Cost) + sum(tpo_list_old$opti_output$FM_Total))/10^6,
                                  "BM_new" = (sum(opti_op_tbo$Display_Cost) + sum(opti_op_tbo$FM_Total))/10^6,
                                  
                                  
                                  "BM%_actual" = "",
                                  "BM%_planned" = (sum(planned_select$FM_Total) + sum(planned_select$Display_Cost))/sum(planned_select$Retailer_Revenue/1.2),
                                  "BM%_optimized" = (sum(optimized_select$FM_Total) + sum(optimized_select$Display_Cost))/sum(optimized_select$Retailer_Revenue/1.2),
                                  "BM%_old" = (sum(tpo_list_old$opti_output$FM_Total) + sum(tpo_list_old$opti_output$Display_Cost) )/(sum(tpo_list_old$opti_output$Retailer_Revenue/1.2)),
                                  "BM%_new" = (sum(opti_op_tbo$FM_Total) + sum(opti_op_tbo$Display_Cost))/(sum(opti_op_tbo$Retailer_Revenue/1.2)),
                                  
                                  "FM%_actual" = "",
                                  "FM%_planned" = sum(planned_select$FM_Total)/sum(planned_select$Retailer_Revenue/1.2),
                                  "FM%_optimized" = sum(optimized_select$FM_Total)/sum(optimized_select$Retailer_Revenue/1.2),
                                  "FM%_old" = (sum(tpo_list_old$opti_output$FM_Total))/(sum(tpo_list_old$opti_output$Retailer_Revenue/1.2)),
                                  "FM%_new" = (sum(opti_op_tbo$FM_Total))/(sum(opti_op_tbo$Retailer_Revenue/1.2)),check.names = FALSE)
    
  }
  
  return(KPI_calculation)
}