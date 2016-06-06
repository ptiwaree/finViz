library(shiny)

shinyUI(navbarPage("Applications:", id="eqapps", 
  tabPanel("Equity Screener",
  sidebarLayout(
    sidebarPanel(
      #h3("Screening Option"),
      selectInput("Indicator",label="Categorical Variables",
                  choices=IndicatorDropDown, selected="INDEX", multiple=TRUE,selectize=FALSE),
      selectInput("Numerical",label="Numerical Variables",
                  choices=NumericalDropDown, selected=NumericalDropDown[1],multiple=TRUE, selectize=FALSE),
      
      # Index Options 
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
      ),
    
    mainPanel(
      tabsetPanel(
          tabPanel("Table", 
                   fluidRow(
                     column(12, # - Table Variables To Output
                            selectizeInput("OutputTableVars",label="",
                                        choices=c(IndicatorDropDown,NumericalDropDown), multiple=TRUE, options = list(placeholder = 'Choose variables to output in the table'), selected=c("BLOOMBERG TICKER","SEDOL","SECURITY NAME"))),
                     column(12, dataTableOutput('table'))
                     )),
          tabPanel("Summary",
                   fluidRow(
                     column(12, # - Table Variables To Output
                            selectizeInput("OutputStatsVars",label="",
                                           choices=c(NumericalDropDown), multiple=TRUE, options = list(placeholder = 'Choose variables to output in the table'), selected=c("STATISTICS","P/E (FY0)"))),
                     column(12, dataTableOutput('summary'))
                     )),
          tabPanel("Histograms", 
                   fluidRow(
                      column(12, selectizeInput("DensityVars",label="",
                                            choices=NumericalDropDown, selected=NumericalDropDown[1],multiple=TRUE, options = list(placeholder = 'Choose variables to plot density'))),
                      column(12,plotOutput('density'))
                    )),
          tabPanel("Scatter Plot", 
                   fluidRow(
                     column(6, selectizeInput("ScatterVarsX",label="",
                                              choices=NumericalDropDown, selected=NumericalDropDown[1], multiple=FALSE, options = list(placeholder = 'Choose X variable'))),
                     column(6, selectizeInput("ScatterVarsY",label="",
                                              choices=NumericalDropDown, selected=NumericalDropDown[2], multiple=FALSE, options = list(placeholder = 'Choose Y variable'))),
                     column(12, plotOutput('scatter'))
                   )),#end of scatter tabPanel
        tabPanel("Box Plots", 
                 fluidRow(
                   column(12, selectizeInput("BoxVars",label="",
                                             choices=NumericalDropDown, selected=NumericalDropDown[1],multiple=TRUE, options = list(placeholder = 'Choose variables for box plots'))),
                   column(12,plotOutput('box'))
                 )) #end of box tabpanel
        )) #end of main panel and tab panel
  ) #end of sidebarLayout
), #end of Equity Screener tabPanel
tabPanel("Company Analyzer",
   sidebarLayout(
     sidebarPanel(
       #options = list(maxItems = 5)
       selectizeInput("Ticker",label="TICKERS",
                   choices=unique(DATA$BT),multiple=TRUE,  options = list(maxItems = 5, placeholder = 'Enter Upto 5 Tickers'))       
       ),
     mainPanel()
   )
   )
))