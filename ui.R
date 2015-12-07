library(shiny)
# Shiny App 
# Version - Beta 2.0
# Creator : hb2483,bjt2133,hy2456
# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Technology Stocks-Statistics",
             
tabPanel("EDA of Dataset",
sidebarLayout(
  sidebarPanel(
    helpText("Statistics on Log Returns"),
      selectInput("stock", 
                label = "Choose a Stock",
                choices = c("ADS", "AAPL","FB", "GME","GOOG",
                            "LNVGY","NKE","SSNLF","TSLA","UA"),
                selected = "ADS"),
    
    numericInput("bins",
                 label = h3("Number of Bins for Dataset"),
                 value = 20),
    
    numericInput("cf", 
                 label = h3("Confidence Interval"), 
                 value = 95)
  ),
  
  mainPanel(plotOutput("distPlot"),textOutput("text1"),textOutput("text2"))
  
  )
),


tabPanel("Regression Output",
  sidebarLayout(
    sidebarPanel(
      helpText("Regression Output"),
            selectInput("ind", 
            label = "Choose a Stock/Independent Variable",
            choices = c("ADS", "AAPL","FB", "GME","GOOG","LNVGY","NKE","SSNLF","TSLA","UA"),selected = "ADS"),
                          
            selectInput("dep", 
                        label = "Choose a Stock/Dependent Variable",
                        choices = c("ADS", "AAPL","FB", "GME","GOOG","LNVGY","NKE","SSNLF","TSLA","UA"),selected = "GOOG")
                        
                 ),
   mainPanel(plotOutput("scatrPlot"),tableOutput("regStats"))
                )
         ),
navbarMenu("More",
           tabPanel("Residuals",
                    plotOutput("resStats")),
           tabPanel("Summary",
                    textOutput("summaryStats"))
     )
  )
)