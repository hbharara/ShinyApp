library(shiny)
# Shiny App 
# Version - Beta 2.3
# Last Update : 12/12/2015
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
  
  mainPanel(plotOutput("distPlot"),textOutput("text1"),textOutput("text2"),textOutput("text3"),textOutput("text4"))
  
  )
),


navbarMenu("Two Stocks",
           tabPanel("Regression",
   sidebarLayout(
     sidebarPanel(
       helpText("Regression Output"),
            selectInput("ind", 
            label = "Choose a Stock/Independent Variable",
            choices = c("ADS", "AAPL","FB", "GME","GOOG","LNVGY","NKE","SSNLF","TSLA","UA"),selected = "ADS"),
                          
            selectInput("dep", 
                        label = "Choose a Stock/Dependent Variable",
                        choices = c("ADS", "AAPL","FB", "GME","GOOG","LNVGY","NKE","SSNLF","TSLA","UA"),selected = "GOOG"),
                        
      numericInput("popMean", 
                   label = h3("Confidence Interval for Estimating Equality of Population Mean"), 
                   value = 95)
      
                   ),
    
    
   mainPanel(plotOutput("scatrPlot"),tableOutput("regStats"),textOutput("meanDifference"))
                )),
   
   tabPanel("Residual",
            plotOutput("resStats"))
   
   ),

navbarMenu("Log Returns V/s Time",
         tabPanel("Regression",  
         sidebarLayout(
           sidebarPanel(
             helpText("Regression Output-Log Returns V/s Time"),
             selectInput("LogReturns", 
                         label = "Choose a Stock/Independent Variable",
                         choices = c("ADS", "AAPL","FB", "GME","GOOG","LNVGY","NKE","SSNLF","TSLA","UA"),selected = "ADS")
                        ),
           mainPanel(plotOutput("LogPlot"),tableOutput("LogStats"))
         )),
         tabPanel("Log Returns-Residuals",
                  plotOutput("logresStats"))
         )
    )

)

