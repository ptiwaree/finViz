library(gridExtra)
library(plotly)
library(d3heatmap)
library(pastecs) #used for summary statistics

options(scipen = 999)

# Shiny Server Function
shinyServer(function(input, output) { 
##########################################UI WIDGETS########################################
  
  # - Index Universe Selector UI
    output$portfolio_nameSelector <- renderUI({  
      dropdown = unique(DATA$portfolio_name)
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
  

################################OUTPUT DATA#####################################################
  # - Output data
  outputdata <- reactive({
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
    temp = format(outputdata(),big.mark=",", trim=TRUE)
    #translate user chosen output vars into original headers to show in table
    output_table_vars = filter(headers,headers$V2 %in% input$OutputTableVars)[[1]]
    temp = format(subset(temp,select=c(output_table_vars)),scientific=F)
    # output table should have friendly headers so switch to those
    names(temp) = lapply(names(temp), function(x)(return(headers2[[x]])))
    return(unique(temp))
  })

  scatterPlot <- reactive({
    x1 = input$ScatterVarsX
    x2 = input$ScatterVarsY
    #Example x1 = "P/E (FY0)"
    y1 = headers$V1[[which(headers$V2 == x1)]] #Example y1 = "pe_fy0"
    y2 = headers$V1[[which(headers$V2 == x2)]]
    x = outputdata()[[y1]]
    y = outputdata()[[y2]]
    p1 <- ggplot(outputdata(), aes(x,y)) + geom_point(colour = "red", size = 3) + xlab(x1) + ylab(x2) + theme(panel.background = element_rect(fill = 'white')) +
      coord_cartesian(xlim = c(get_min(x),get_max(x)), ylim=c(get_min(y),get_max(y))) + geom_smooth(method = "lm", se = FALSE, colour="blue")
    return(p1)
  })
  
  densityPlot <- reactive({
    if (length(input$DensityVars) > 0) {
      plots = list()
      for (i in 1:length(input$DensityVars)) {
        x = input$DensityVars[i]
        #Example x = "P/E (FY0)"
        y = headers$V1[[which(headers$V2 == x)]] #Example y = "pe_fy0"
        #data_to_plot = select(outputdata(), one_of(c(y))) #can just do outputdata()[[y]]
        #data_to_plot = data_to_plot[1:nrow(data_to_plot),]
        data_to_plot = outputdata()[[y]]
        sector = outputdata()[["sector"]]
        #binW= abs(range(data_to_plot))/100
        plots[[i]] <- ggplot(data=outputdata(), aes(data_to_plot, fill=sector)) + geom_histogram() + xlab(x) + coord_cartesian(xlim = c(get_min(data_to_plot),get_max(data_to_plot))) + theme(panel.background = element_rect(fill = 'white'))
      }
      #n <- length(plots)
      #nCol <- floor(sqrt(n))
      return(do.call("grid.arrange", c(plots, ncol=1)))
    }
  })

  boxPlot <- reactive({
    if (length(input$BoxVars) > 0) {
      plots = list()
      for (i in 1:length(input$BoxVars)) {
        x = input$BoxVars[i]
        #Example x = "P/E (FY0)"
        y = headers$V1[[which(headers$V2 == x)]] #Example y = "pe_fy0"
        #data_to_plot = select(outputdata(), one_of(c(y))) #can just do outputdata()[[y]]
        #data_to_plot = data_to_plot[1:nrow(data_to_plot),]
        data_to_plot = outputdata()[[y]]
        plots[[i]] <- ggplot(data=outputdata(), aes(1,data_to_plot)) + geom_boxplot() + theme(panel.background = element_rect(fill = 'white')) + ylab(x) + coord_cartesian(ylim = c(get_min(data_to_plot),get_max(data_to_plot)))
        #plots[[i]] <- qplot(y=data_to_plot, x= 1, geom = "boxplot")
      }
      #n <- length(plots)
      #nCol <- floor(sqrt(n))
      return(do.call("grid.arrange", c(plots, ncol=2)))
    }
  })

  summaryStats <-  reactive({
    summaryStat = stat.desc(outputdata())
    summaryStat$STATISTICS = rownames(summaryStat)
    numerical_vars_headers = filter(headers,headers$V2 %in% input$OutputStatsVars)[[1]]
    summaryStat = subset(summaryStat ,select=c("STATISTICS",numerical_vars_headers))
    # output table should have friendly headers so switch to those
    headers2$STATISTICS = "STATISTICS"
    names(summaryStat) = lapply(names(summaryStat), function(x)(return(headers2[[x]])))
    summaryStat = format(summaryStat,big.mark=",", trim=TRUE, scientific=FALSE)
    return(summaryStat)
  })

  # - Output data to table ,class = 'cell-border stripe'
  output$table <- renderDataTable(outputTableData())
  output$summary <- renderDataTable(summaryStats())
  output$density <- renderPlot(densityPlot())
  output$scatter <- renderPlot(scatterPlot())
  output$box <- renderPlot(boxPlot())
##############################################FILTERS########################################
  # - Set Filter Data

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
})