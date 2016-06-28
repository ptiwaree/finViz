#library(datasets)

options(scipen = 999)
##############################################Functions########################################
# - Filter functions

categorical_filter = function(data, indicator, user_input) {
  if (length(user_input) > 0) {
    return(subset(data,indicator %in% user_input))
  } else {
    return(data)
  }
}

set_min_filter = function(data, indicator, user_input) {
  if (length(user_input) > 0) {
    return(data[indicator >= user_input,])
  } else {
    return(data)
  }
}

set_max_filter = function(data, indicator, user_input) {
  if (length(user_input) > 0) {
    return(data[indicator <= user_input,])
  } else {
    return(data)
  }
}

# Median Absolute Deviation
med_abs_dev = function(data, no_of_mads=3) {
  return(abs(data - median(data,na.rm=TRUE)) < no_of_mads*mad(data,na.rm=TRUE))
}

# rounds numerical columns in a dataframe
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  return(df)
}

# Round the data to 3 decimal places
DATA = round_df(DATA,3)

# Shiny Server Function
#=================================================================
shinyServer(function(input, output) { 

##########################REACTIVE UI WIDGETS##########################
  
  # - Index Universe Selector UI
    output$portfolio_nameSelector <- renderUI({  
      dropdown = unique(DATA$portfolio_name[!is.na(DATA$portfolio_name)])
      selectInput("portfolio_name",label="INDEX",
              choices=dropdown, selected=dropdown[1],multiple=TRUE)
  })

  # Categorical/Indicator Variables Selector UI exclude INDEX since we create one already
  lapply(IndicatorDropDown[-3], function(x){
    #Example x = "BLOOMBERG TICKER"
    #Use headers to translate to original headers
    y = headers$V1[[which(headers$V2 == x)]] #Example y = "BT"
    #Can also do select(DATA,which(headers$V2 == x)) for below
    #z = select(DATA, one_of(c(y))) #new dataframe from DATA containing only column y
    #z = z[1:nrow(z),] #get all the values, now its in the same for as DATA$BT
    z = DATA[[y]]
    
    output[[paste0(y, "Selector")]] <- renderUI({
      #Below gives a dropdown list for z that filters on selected index
      dropdown = unique(categorical_filter(z, DATA$portfolio_name, input$portfolio_name))
      #dropdown = unique(z)
      selectInput(paste0(y,""),label=paste0(x,""), choices=dropdown, multiple=TRUE)
    })
  })

################Plot Widgets##############################
  output$hist_vars_selector <- renderUI({  
    selectizeInput("HistVars",label="Variables",choices=NumericalDropDown, selected=NumericalDropDown[1], multiple=TRUE, options = list(placeholder = 'Choose variables to plot'))
  })

  output$histograms <- renderUI({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making histogram", value = 0)
    
    histPlots()
    hist_output_list <- lapply(1:length(input$HistVars), function(x) {
      plotname <- paste("hist", x, sep="")
      #plotOutput(plotname, height = 280, width = 1000)
      plotlyOutput(plotname)
    })
    do.call(tagList, hist_output_list)
  })
  
  histPlots <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making histogram", value = 0)
    
    lapply(1:length(input$HistVars), function(i) {
      progress$inc(i/length(input$HistVars))
      plotname <- paste("hist", i, sep="")
      output[[plotname]] <- renderPlotly({
        x = input$HistVars[i] #x = "P/E (FY0)"
        y = headers$V1[[which(headers$V2 == x)]] #Example y = "pe_fy0"
        color_index = ifelse(i%%length(colors)==0,length(colors),i%%length(colors))
        # only plot data that is within 3 median absolute deviation
        no_outlier_data = subset(outputdata(), med_abs_dev(outputdata()[[y]], input$MAD))
        data_to_plot = no_outlier_data[[y]]
        #fill=sample(colors,1), 
        ggplotly(ggplot(no_outlier_data, aes(data_to_plot)) + geom_histogram(fill=colors[color_index],color="white") + xlab(x) + coord_cartesian(xlim = c(get_min(data_to_plot),get_max(data_to_plot))) + theme(panel.background = element_rect(fill = 'white')))
        })
    }) #end of lapply
  }) #end of histPlots reactive
  
  output$boxes <- renderUI({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making box plots", value = 0)
    
    boxPlots()
    plot_output_list <- lapply(1:length(input$HistVars), function(x) {
      plotname <- paste("box", x, sep="")
      #plotOutput(plotname, height = 280, width = 1000)
      plotlyOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
  
  output$densities <- renderUI({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making density plots", value = 0)
    
    densityPlots()
    plot_output_list <- lapply(1:length(input$HistVars), function(x) {
      plotname <- paste("density", x, sep="")
      #plotOutput(plotname, height = 280, width = 1000)
      plotlyOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })

  densityPlots <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making density plots", value = 0)
    
    lapply(1:length(input$HistVars), function(i) {
      progress$inc(i/length(input$HistVars))
      plotname <- paste("density", i, sep="")
      output[[plotname]] <- renderPlotly({
        x = input$HistVars[i] #x = "P/E (FY0)"
        y = headers$V1[[which(headers$V2 == x)]] #Example y = "pe_fy0"
        # only plot data that is within 3 median absolute deviation
        no_outlier_data = subset(outputdata(), med_abs_dev(outputdata()[[y]],input$MAD))
        original_data = subset(DATA, med_abs_dev(DATA[[y]],input$MAD))
        no_outlier_data$type = "Screened"
        original_data$type = "Universe"
        new_data = rbind(no_outlier_data, original_data)
        data_to_plot = new_data[[y]]
        Type = new_data$type
        ggplotly(ggplot(new_data, aes(data_to_plot, color = Type)) + geom_density(alpha = 0.2) + xlab(x) + coord_cartesian(xlim = c(get_min(no_outlier_data[[y]]),get_max(no_outlier_data[[y]]))) + theme(panel.background = element_rect(fill = 'white')))
      })
    }) #end of lapply
  }) #end of densityPlots reactive

  boxPlots <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making box plots", value = 0)
    
    lapply(1:length(input$HistVars), function(i) {
      progress$inc(i/length(input$HistVars))
      plotname <- paste("box", i, sep="")
      output[[plotname]] <- renderPlotly({
        x = input$HistVars[i] #x = "P/E (FY0)"
        y = headers$V1[[which(headers$V2 == x)]] #Example y = "pe_fy0"
        color_index = ifelse(i%%length(colors)==0,length(colors),i%%length(colors))
        # only plot data that is within 3 median absolute deviation
        no_outlier_data = subset(outputdata(), med_abs_dev(outputdata()[[y]], input$MAD))
        data_to_plot = no_outlier_data[[y]]
        #sector = no_outlier_data[["sector"]]
        ggplotly(ggplot(data=no_outlier_data, aes(1,data_to_plot)) + geom_boxplot(colour=colors[color_index]) + theme(panel.background = element_rect(fill = 'white')) + ylab(x) + coord_cartesian(ylim = c(get_min(data_to_plot),get_max(data_to_plot))))
      })
    }) #end of lapply
  }) #end of boxPlots reactive
  
  output$bar_vars_selector <- renderUI({  
    selectizeInput("BarVars",label="Variables", choices=c("MARKET","CURRENCY","COUNTRY","SECTOR","SUBSECTOR","INDUSTRY","SUBINDUSTRY"), selected="SECTOR", multiple=TRUE, options = list(placeholder = 'Choose variables to plot'))
  })
  
  output$bars <- renderUI({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making Bar Chart", value = 0)
    
    barPlots()
    plot_output_list <- lapply(1:length(input$BarVars), function(x) {
      plotname <- paste("bar", x, sep="")
      #plotOutput(plotname, height = 280, width = 1000)
      highchartOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
  
  barPlots <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Making Bar Chart", value = 0)
    
    lapply(1:length(input$BarVars), function(i) {
      progress$inc(i/length(input$BarVars))
      plotname <- paste("bar", i, sep="")
      output[[plotname]] <- renderHighchart({
        x = input$BarVars[i] #x = "P/E (FY0)"
        y = headers$V1[[which(headers$V2 == x)]] #Example y = "pe_fy0"
        color_index = ifelse(i%%length(colors)==0,length(colors),i%%length(colors))
        data_to_plot = outputdata()[[y]]
        #fill=sample(colors,1), 
        hchart(data_to_plot, colorByPoint = TRUE, name = paste0("Bar Chart: ", x))
      })
    }) #end of lapply
  }) #end of histPlots reactive

################################OUTPUT DATA###################
  # - Output data
  outputdata <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating output table", value = 0)
    
    filtered_data = DATA
    # - Apply categorical filter
    for (x in input$Indicator) {
      #Example x = "BLOOMBERG TICKER"
      #Use headers to translate to original headers
      y = headers$V1[[which(headers$V2 == x)]] #Example y = "BT"
      #indicator = select(filtered_data, one_of(c(y))) #new dataframe from filtered_data containing only column y
      #indicator = indicator[1:nrow(indicator),] #get all the values, now its in the same for as DATA$BT
      indicator = filtered_data[[y]]
      user_input = input[[paste0(y, "")]] 
      filtered_data = categorical_filter(filtered_data, indicator, user_input)
    }
    
    # - Apply Minimum filter
    for (x in input$Numerical) {
      #Example x = "P/E (FY0)"
      #Use headers to translate to original headers
      y = headers$V1[[which(headers$V2 == x)]] #Example y = "pe_fy0"
      #indicator = select(filtered_data, one_of(c(y))) #new dataframe from filtered_data containing only column y
      #indicator = indicator[1:nrow(indicator),] #get all the values, now its in the same for as DATA$BT
      indicator = filtered_data[[y]]
      user_input = input[[paste0(y, "_min")]]
      filtered_data = set_min_filter(filtered_data, indicator, user_input)
    }
    
    # - Apply Maximum filter
    for (x in input$Numerical) {
      #Example x = "P/E (FY0)"
      #Use headers to translate to original headers
      y = headers$V1[[which(headers$V2 == x)]] #Example y = "pe_fy0"
      #indicator = select(filtered_data, one_of(c(y))) #new dataframe from filtered_data containing only column y
      #indicator = indicator[1:nrow(indicator),] #get all the values, now its in the same for as DATA$BT
      indicator = filtered_data[[y]]
      user_input = input[[paste0(y, "_max")]]
      filtered_data = set_max_filter(filtered_data, indicator, user_input)
    }
    
    return(filtered_data)
  })
  
  outputTableData <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating output table", value = 0)
    
    temp = format(outputdata(),big.mark=",", trim=TRUE)
    #translate user chosen output vars into original headers to show in table
    output_table_vars = filter(headers,headers$V2 %in% input$OutputTableVars)[[1]]
    temp = format(subset(temp,select=c(output_table_vars)),scientific=F)
    # output table should have friendly headers so switch to those
    names(temp) = lapply(names(temp), function(x)(return(headers2[[x]])))
    temp = round_df(temp,3)
    return(unique(temp))
  })

  scatterPlot <- reactive({
    x1 = input$ScatterVarsX
    x2 = input$ScatterVarsY
    #Example x1 = "P/E (FY0)"
    y1 = headers$V1[[which(headers$V2 == x1)]] #Example y1 = "pe_fy0"
    y2 = headers$V1[[which(headers$V2 == x2)]] 
    no_outlier_data = subset(outputdata(), med_abs_dev(outputdata()[[y1]],input$MAD))
    no_outlier_data = subset(no_outlier_data, med_abs_dev(no_outlier_data[[y2]],input$MAD))
    x = no_outlier_data[[y1]]
    y = no_outlier_data[[y2]]
    p1 <- ggplot(no_outlier_data, aes(x,y)) + geom_point(colour = colors[1], size = 3) + xlab(x1) + ylab(x2) + theme(panel.background = element_rect(fill = 'white')) +
      coord_cartesian(xlim = c(get_min(x),get_max(x)), ylim=c(get_min(y),get_max(y))) + geom_smooth(method = "lm", se = FALSE, colour="blue")
    return(ggplotly(p1))
  })

  summaryStats <-  reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Creating output table", value = 0)
    
    summaryStat = stat.desc(outputdata())
    numerical_vars_headers = filter(headers,headers$V2 %in% intersect(input$OutputTableVars, NumericalDropDown))[[1]]
    summaryStat = subset(summaryStat ,select=c(numerical_vars_headers))
    # output table should have friendly headers so switch to those
    names(summaryStat) = lapply(names(summaryStat), function(x)(return(headers2[[x]])))
    summaryStat = format(summaryStat,big.mark=",", trim=TRUE, scientific=FALSE)
    summaryStat = round_df(summaryStat,3)
    return(summaryStat)
  })
  
  # - Peer Analyzer - Make a time series data
  peer_analyzer_output_data = reactive({
    temp = time_series_data %>% filter(BT %in% input$pa_ticker & quote_type == input$pa_quote_type)
    temp = temp %>% select(new_date, BT, quote_value)
    temp <- spread(temp,BT,quote_value)
    tempxts = as.xts(x = temp[,-1], order.by = temp[,1]) #converts to time series object
    return(tempxts)
  })
  

  # - Output data to table ,class = 'cell-border stripe'
  output$table <- DT::renderDataTable(round_df(outputTableData(),3),  class = 'cell-border stripe')
  output$summary <- DT::renderDataTable(round_df(summaryStats(),3),  class = 'cell-border stripe')
  output$scatter <- renderPlotly(scatterPlot())
  
  output$pa_dygraph <- renderDygraph({
    dygraph(peer_analyzer_output_data(), main = paste0("Time Series: ",input$pa_quote_type)) %>%
      dyOptions(drawGrid = TRUE, stackedGraph = TRUE) %>%
      dyRangeSelector(height = 20)
  })

}) #End of Shiny Server Function