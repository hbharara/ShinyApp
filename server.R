library(shiny)

## Creates an Empty Data Frame
directory <- './Datasets/'
files_list <- list.files(directory, full.names = TRUE) ## Creates a list of data frames

adidas <- data.frame() 					               
adidas <- read.csv(files_list[1],header = TRUE) ## Opening the File in the Working Directory
transform(adidas , cl_log = as.numeric(cl_log))

apple <- data.frame() 					               
apple <- read.csv(files_list[2],header = TRUE) ## Opening the File in the Working Directory
transform(apple , cl_log = as.numeric(cl_log))

facebook <- data.frame() 					               
facebook <- read.csv(files_list[3],header = TRUE) ## Opening the File in the Working Directory
transform(facebook , cl_log = as.numeric(cl_log))

gamestop <- data.frame() 					               
gamestop <- read.csv(files_list[4],header = TRUE) ## Opening the File in the Working Directory
transform(gamestop , cl_log = as.numeric(cl_log))

google <- data.frame() 					               
google <- read.csv(files_list[5],header = TRUE) ## Opening the File in the Working Directory
transform(google , cl_log = as.numeric(cl_log))

lenovo <- data.frame() 					               
lenovo <- read.csv(files_list[6],header = TRUE) ## Opening the File in the Working Directory
transform(lenovo , cl_log = as.numeric(cl_log))

nike <- data.frame() 					               
nike <- read.csv(files_list[7],header = TRUE) ## Opening the File in the Working Directory
transform(nike , cl_log = as.numeric(cl_log))

samsung <- data.frame() 					               
samsung <- read.csv(files_list[8],header = TRUE) ## Opening the File in the Working Directory
transform(samsung , cl_log = as.numeric(cl_log))

tesla <- data.frame() 					               
tesla <- read.csv(files_list[9],header = TRUE) ## Opening the File in the Working Directory
transform(tesla , cl_log = as.numeric(cl_log))

underarmour <- data.frame() 					               
underarmour <- read.csv(files_list[10],header = TRUE) ## Opening the File in the Working Directory
transform(underarmour , cl_log = as.numeric(cl_log))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  
  
  output$distPlot <- renderPlot({
    # Code to Switch Between Elements
    
    data <- switch(input$stock, 
                   "ADS" = adidas$cl_log,
                   "AAPL" = apple$cl_log,
                   "FB" = facebook$cl_log,
                   "GME" = gamestop$cl_log,
                   "GOOG" = google$cl_log,
                   "LNVGY" = lenovo$cl_log,
                   "NKE" = nike$cl_log,
                   "SSNLF" = samsung$cl_log,
                   "TSLA" = tesla$cl_log,
                   "UA" = underarmour$cl_log)
    
    # Code to Calculate and print the Upper and Lower Limit of the Confidence Interval
    m   <- mean(data)
    std <- sqrt(var(data))
    cnt <- sum(data)/mean(data)
    se <- std/sqrt(cnt)
    
    cf <- 1-((100-input$cf)/200)
 
    cv<-qt(cf,df=cnt-1)     
  
      
    # draw the histogram with the specified number of bins
    # bins <- seq(min(data), max(data), length.out = input$range + 1)
  
    output$text1 <- renderText({ 
      print(paste("The Upper Limit Confidence Interval: ",m+cv*se))
    })
    output$text2 <- renderText({ 
      print(paste("The Lower Limit Confidence Interval: ",m-cv*se))  
    })
     hist(data,nclass = input$bins,prob = T,xlab = "Log Returns Of Closing Price")
     curve(dnorm(x,mean = m , sd = std ), col = "darkblue" , lwd = 2 , add = TRUE)
      })
  
  output$scatrPlot <- renderPlot({
    # Code to Switch Between Elements
    
    ind_var <- switch(input$ind, 
                        "ADS" = adidas$cl_log,
                        "AAPL" = apple$cl_log,
                        "FB" = facebook$cl_log,
                        "GME" = gamestop$cl_log,
                        "GOOG" = google$cl_log,
                        "LNVGY" = lenovo$cl_log,
                        "NKE" = nike$cl_log,
                        "SSNLF" = samsung$cl_log,
                        "TSLA" = tesla$cl_log,
                        "UA" = underarmour$cl_log)
    
    dep_var <- switch(input$dep, 
                  "ADS" = adidas$cl_log,
                  "AAPL" = apple$cl_log,
                  "FB" = facebook$cl_log,
                  "GME" = gamestop$cl_log,
                  "GOOG" = google$cl_log,
                  "LNVGY" = lenovo$cl_log,
                  "NKE" = nike$cl_log,
                  "SSNLF" = samsung$cl_log,
                  "TSLA" = tesla$cl_log,
                  "UA" = underarmour$cl_log)
    # Code to Calculate and print the Upper and Lower Limit of the Confidence Interval
    plot(ind_var, dep_var, main=paste("Scatterplot of Log Returns"), 
         xlab=input$ind, ylab=input$dep, pch=19)
    abline(lm(dep_var~ind_var), col="red") # regression line (y~x) 
    
    
    output$regStats <- renderTable({ 
      lreg <- data.frame()
      lreg<-lm(dep_var~ind_var)
      print(lreg)
      })
    output$resStats <- renderPlot({ 
      lreg <- data.frame()
      lreg<-lm(dep_var~ind_var)
      res <- resid(lreg)
      plot(ind_var, res, main=paste("Scatterplot of Residuals"), 
           xlab=input$ind, ylab="Residuals", pch=19)
      abline(0, 0,col="blue")
      })
    
    output$summaryStats <- renderText({
      lreg <- data.frame()
      lreg<-lm(dep_var~ind_var)
      el<-summary.lm(lreg)
      el[[3]]
      })
      })
    
})
