#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(grid)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {print(plots[[1]])} 
  else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


filenames <- list.files("Output Files", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)

Indices <- read.csv('indices.csv', stringsAsFactors = FALSE)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
   # Application title
   titlePanel("Stock Trade Simulator Incorporating News and Trend Data with ARIMA-GARCH Time-Series Model"),
   
   # Sidebar with a slider input for number of bins 
   titlePanel(""),
   sidebarLayout(
     sidebarPanel(
       selectInput("stockInput", p("Choose company"),
                   choices = as.character(Indices$Company)),
       br(),
       checkboxInput("short", "Include short positions", FALSE),
       br(),
       h5("Stock Index"),
       verbatimTextOutput("oid1"),
       br(),
       sliderInput("decimal", "Risk:",
                   min = 0, max = 1, value = 0.5, step= 0.01),
       "0 = Conservative, 1 = Risky",
       br(),br(),
       h5("Average Return from google trend + ARIMA-GARCH model (%)"),
       verbatimTextOutput("oid2"),
       h5("Average Return from 'buy & hold' strategy (%)"),
       verbatimTextOutput("oid3"),
       dateRangeInput('dateRange',
                      label = p('Date range input:'),
                      start = as.Date("2014-08-13", format = '%Y-%m-%d'), 
                      end = as.Date("2016-07-22", format = '%Y-%m-%d')
       )
       ),
     mainPanel(
       fluidRow(
         splitLayout(cellWidths = c("30%", "30%", "30%"), 
                     plotOutput("gtrend"), plotOutput("gnews"),plotOutput("stock"))
       ),
       plotOutput("profits")
       
     )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  # Reactive functions 
  stockIndex2 <- reactive({
   as.character(dplyr::filter(Indices, Company==input$stockInput)['index.1'])})
  tol2 <- reactive({(1 - input$decimal) * 0.0015})
  stock_data2 <- reactive({
    tol <- tol2()
    stockIndex <- stockIndex2()
    stock_data <- as.data.frame(ldf[grep(paste0(stockIndex,'_output_gtrend_gnews.csv'), filenames)][[1]])
    stock_data$position <- ifelse(stock_data$ind > tol, 1, 
                                  ifelse(stock_data$ind < (-tol), -1, 0))
    stock_data <- dplyr::filter(stock_data, as.Date(date) > input$dateRange[1] & 
                                  as.Date(date)< input$dateRange[2])
    stock_data
    })
  news_data2 <- reactive({    
    stockIndex<- stockIndex2()
    news_data <- as.data.frame(ldf[grep(paste0(stockIndex,'_news.csv'), filenames)][[1]])
    news_data
  })
  adstock2 <- reactive({    
    stock_data <- stock_data2()
    stock_data$Adjusted.Stock
  })
  portfolio.value2 <- reactive({
    stock_data <- stock_data2()
    adstock <- adstock2()
    i <- 1
    portfolio.value <- data.frame(matrix(ncol = 2, nrow = nrow(stock_data)))
    pos1 <- 1 # look for 
    profit <- 0
    if(!input$short){
    while (i<nrow(stock_data)){
      newi = suppressWarnings(min(which(stock_data$position[i:nrow(stock_data)] == pos1)) + i-1)
      if(is.infinite(newi)){newi <- nrow(stock_data)}
      portfolio.value[i:newi,1] <- -0.5*pos1 + 0.5
      portfolio.value[i:newi,2] <- ifelse(pos1 == 1, profit, 
                                          profit + as.data.frame(adstock[i:newi]-
                                                                   as.numeric(adstock[i])))
      if(pos1 == -1){
        profit <- profit + as.numeric(adstock[newi]) - 
          as.numeric(adstock[i])
      }
      i <- newi
      pos1 <- -1*pos1
    }
    portfolio.value$date <- stock_data$date
    colnames(portfolio.value) <- c('Position', 'Profit', 'date')
    portfolio.value
    }
    else{
      
      while (i<nrow(stock_data)){
        newi = suppressWarnings(min(which(stock_data$position[i:nrow(stock_data)] == pos1)) + i-1)
        
        if(is.infinite(newi)){newi <- nrow(stock_data)}
        portfolio.value[i:newi,1] <- -pos1
        portfolio.value[i:newi,2] <- ifelse(pos1 == 1, profit+ as.data.frame(adstock[i]-
                                                                               as.numeric(adstock[i:newi])), 
                                            profit + as.data.frame(adstock[i:newi]-
                                                                     as.numeric(adstock[i])))
        if(pos1 == -1){
          profit <- profit + as.numeric(adstock[newi]) - 
            as.numeric(adstock[i])
        }
        else{
          profit <- profit + as.numeric(adstock[i]) - 
            as.numeric(adstock[newi])
        }
        i <- newi
        pos1 <- -pos1
      }
      portfolio.value$date <- stock_data$date
      colnames(portfolio.value) <- c('Position', 'Profit', 'date')
      portfolio.value
    }
  })
  
  # Outputs
  output$oid1<- renderPrint({
    stockIndex2()
  })
  
   output$gtrend <- renderPlot({
     stockIndex <- stockIndex2()
     stock_gtrend <- as.data.frame(ldf[grep(paste0(stockIndex,'_gtrend.csv'), filenames)][[1]])
     stock_data <- stock_data2()
     
     stock_gtrend <- dplyr::filter(stock_gtrend, !is.na(index) & 
                                     as.Date(week) > min(as.Date(stock_data$date)))
     stock_gtrend <- dplyr::filter(stock_gtrend, as.Date(week) > input$dateRange[1] & 
                                     as.Date(week)< input$dateRange[2])
     ggplot() + geom_line(data = stock_gtrend, aes(x = as.Date(week), y = index),
                          colour = '#2C3E50') + 
       ggtitle('Google Trend Index') + xlab('Date') + ylab('Index')
     
   })
   output$gnews <- renderPlot({
     stockIndex <- stockIndex2()
     news_data <- news_data2()
     stock_data <- stock_data2()
     stock_gnews <- dplyr::filter(news_data, !is.na(score) & 
                                     as.Date(date) > min(as.Date(stock_data$date)))
     stock_gnews <- dplyr::filter(stock_gnews, as.Date(date) > input$dateRange[1] & 
                                     as.Date(date)< input$dateRange[2])
     ggplot() + geom_line(data = stock_gnews, aes(x = as.Date(date), y = score),
                          colour = '#2C3E50') + 
       ggtitle('Google News Index') + xlab('Date') + ylab('Score')
     
   })
   output$stock<- renderPlot({
     stockIndex <- stockIndex2() 
     stock_data <- stock_data2()
     ggplot() + geom_line(data = stock_data, aes(x = as.Date(date), y = Adjusted.Stock),
                          colour = '#2C3E50') + 
       ggtitle('Stock Price') + xlab('Date') + ylab('Stock Price ($)')
     
   })
   output$profits<- renderPlot({
     stockIndex <- stockIndex2()
     stock_data <- stock_data2()
     portfolio.value <- portfolio.value2()
     
     p1 <- ggplot() + geom_line(data = portfolio.value, aes(x = as.Date(date), y = Position),
                                colour = '#2C3E50') + 
       ggtitle('Position') + xlab('Date') 
     p2 <- ggplot() + geom_line(data = portfolio.value, aes(x = as.Date(date), y = Profit, color = 'Google Data\n + ARIMA-GARCH')) +
       geom_line(data = stock_data, aes(x = as.Date(date), y = Adjusted.Stock-Adjusted.Stock[1], color = 'Buy and Hold')) +
       ggtitle('Profit') + xlab('Date') + ylab('Total Profit ($)') +
       scale_colour_manual(name="Strategy: ", values=c(`Google Data\n + ARIMA-GARCH`="#18BC9C",
                                                     `Buy and Hold`="#2C3E50")) + 
       theme(legend.position="top")
     multiplot(p1, p2, cols = 2)
    
   })
   output$oid2<- renderPrint({
     portfolio.value <- portfolio.value2()
     adstock <- adstock2()
     tail(portfolio.value$Profit, n=1)/adstock[1]*100
   })
   output$oid3<- renderPrint({
     adstock <- adstock2()
     (tail(adstock, n=1)-adstock[1])/adstock[1]*100
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

