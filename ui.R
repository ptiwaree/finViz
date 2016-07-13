# This is a UI script for the Equity Portal App. In this script, we build widgets used in the
# User Interface of the App

library(shiny)
library(plotly)
library(dygraphs)


shinyUI(navbarPage("Applications:", id="eqapps", 
  tabPanel("Equity Screener", #Equity Screener App
  sidebarLayout(
    # Sidebar Panel
    sidebarPanel(
      h3("Screening Criteria"),
      selectInput("Indicator",label="Categorical Variables",
                  choices=IndicatorDropDown, selected="INDEX", multiple=TRUE,selectize=FALSE),
      selectInput("Numerical",label="Numerical Variables",
                  choices=NumericalDropDown, selected=NumericalDropDown[1],multiple=TRUE, selectize=FALSE),
      
      # - Widget for Choosing Index 
      conditionalPanel(
        condition = 'input.Indicator.indexOf("INDEX") !== -1', #Javascript expression
        uiOutput("portfolio_nameSelector")
      ),
    
      # - Categorical Variables for setting Criterias
      lapply(IndicatorDropDown[-3], function(x){
        #Example of x is "BLOOMBERG TICKER". 
        #Use headers to translate to original headers
        y = headers$V1[[which(headers$V2 == x)]] #Example value: "BT"
        #Can also do select(DATA,which(headers$V2 == x)) for below 
        condition = paste0('input.Indicator.indexOf("',x)
        condition = paste0(condition,'") !== -1')
        conditionalPanel(
          #condition = 'input.Indicator.indexOf(x) !== -1', #Javascript expression
          condition = condition,
          uiOutput(paste0(y, "Selector"))
        )
      }),
      
      # - Numerical Variables for setting Criterias
      lapply(NumericalDropDown, function(x){
        #Example of x is "P/E (FY0)". 
        #Use headers to translate to original headers
        y = headers$V1[[which(headers$V2 == x)]] #Example value: "pe_fy0"
        #Can also do select(DATA,which(headers$V2 == x)) for below
        z = select(DATA, one_of(c(y))) #Example is: DATA$pe_fy0 
        condition = paste0('input.Numerical.indexOf("',x)
        condition = paste0(condition,'") !== -1')
        conditionalPanel(
          #condition = 'input.Numerical.indexOf(x) !== -1', #Javascript expression
          condition = condition,
          fluidRow(
            column(6,numericInput(paste0(y,"_min"), paste0(x," (min)"), value = round(get_min(z),4))
            ),
            column(6,numericInput(paste0(y,"_max"), paste0(x," (max)"), value = round(get_max(z),4))
            )
          ))
      })
      ), #end of Sidebar Panel
    # Main Panel
    mainPanel(
      tabsetPanel(
          tabPanel("Tables", #Table Tab
                   fluidRow(
                     column(12, radioButtons("tables", "", choices=c("Raw Data"), selected = "Raw Data", inline = TRUE, width = '100%')),
                     conditionalPanel(
                       condition = 'input.tables.indexOf("Raw Data") !== -1 || input.tables.indexOf("Summary") !== -1',
                       column(12, # - Table Variables To Output
                              selectizeInput("OutputTableVars",label="Add Columns",
                                             choices=c(IndicatorDropDown,NumericalDropDown), multiple=TRUE, options = list(placeholder = 'Choose variables to output in the table'), selected=c("BLOOMBERG TICKER","SEDOL","SECURITY NAME", "CURRENCY", "P/E (FY0)"))),
                              selectizeInput("OutputTableVarsSort",label="Sort by",
                                            choices=c(IndicatorDropDown,NumericalDropDown), multiple=FALSE, selected=c("P/E (FY0)"))
                       ),
                     conditionalPanel(
                       condition = 'input.tables.indexOf("Raw Data") !== -1',
                        column(12, DT::dataTableOutput('table'))
                     )
                     )),
          tabPanel("Charts", 
                   fluidRow(
                      column(12, radioButtons("plots", "", choices=c("Histogram", "Bar", "Density", "Scatter", "Box"), selected = "Histogram", inline = TRUE, width = '100%')),
                      conditionalPanel(
                        condition = 'input.plots.indexOf("Histogram") !== -1 || input.plots.indexOf("Density") !== -1 || input.plots.indexOf("Box") !== -1',
                        column(12, uiOutput("hist_vars_selector"))
                      ),
                      conditionalPanel(
                        condition = 'input.plots !== "Bar"',
                        column(6, numericInput("MAD", "Median Abs. Deviation", 3, min = 0, max = 20, step = 1, width = '25%'))
                      ),
                      conditionalPanel(
                        condition = 'input.plots.indexOf("Histogram") !== -1',
                        column(12, uiOutput("histograms"))
                      ),
                      conditionalPanel(
                        condition = 'input.plots.indexOf("Density") !== -1',
                        column(12, uiOutput("densities"))
                      ),
                      conditionalPanel(
                        condition = 'input.plots.indexOf("Box") !== -1',
                        column(12, uiOutput("boxes"))
                      ),
                      conditionalPanel(
                        condition = 'input.plots.indexOf("Bar") !== -1',
                        column(12, uiOutput("bar_vars_selector")),
                        column(12, uiOutput("bars"))
                      ),
                      conditionalPanel(
                        condition = 'input.plots.indexOf("Scatter") !== -1',
                          column(6, selectizeInput("ScatterVarsX",label="X",
                                                   choices=NumericalDropDown, selected=NumericalDropDown[1], multiple=FALSE, options = list(placeholder = 'Choose X variable'))),
                          column(6, selectizeInput("ScatterVarsY",label="Y",
                                                   choices=NumericalDropDown, selected=NumericalDropDown[2], multiple=FALSE, options = list(placeholder = 'Choose Y variable'))),
                          column(12, plotlyOutput('scatter'))
                      )
                    ))
        )) #end of main panel and tab panel
  ) #end of sidebarLayout
), #end of Equity Screener tabPanel
tabPanel("Peer Analyzer", #Peer Analyzer App
     sidebarLayout(
       sidebarPanel(
        selectizeInput("pa_ticker",label="TICKERS",
                      choices=unique(time_series_data$BT),multiple=TRUE,  selected="BLK US", options = list(maxItems = 5, placeholder = 'Enter Upto 5 Tickers')),
        selectizeInput("pa_quote_type",label="QUOTE TYPE",
                      choices= unique(time_series_data$quote_type[!is.na(time_series_data$quote_type)]),multiple=FALSE, width = '25%')
       ),
       mainPanel(
         dygraphOutput("pa_dygraph")
       )
     ) #end of sidebarLayout
   ) #end of tabPanel
)) #end of Shiny UI