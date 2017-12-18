library(shiny)
library("knitr")
library("ggplot2")
library("gplots")
library("scales")
library("ggalt")
library("tidyquant")
library("reshape2")

#Create the full set etf data frame which will be trimmed by the multi-select
etfs <- data.frame(ETF = c("DBA", "DBC", "IFGL", "IYR", "TIP",
                           "AGG", "IEMG", "IEFA", "IJR", "ITOT"),
                   Type = c("Agricultural", "Commodities", "Intl Real Estate",
                            "US Real Estate", "TIPs Bonds", "US Aggregated Bonds",
                            "Emerging Markets", "Foreign Total", "S&P Small Cap",
                            "S&P Total"),
                   "Commission Free" = c("No", "No", "Yes", "No", "Yes",
                                         "Yes", "Yes", "Yes", "Yes", "Yes"),
                   check.names = FALSE)
options <- etfs$ETF
names(options) <- paste(etfs$ETF, " -- ", etfs$Type)

ui <- fluidPage(
  #Remove search from data tables
  tags$head(tags$style(
    type="text/css", "tfoot {display:none;}"
  )),
  dateRangeInput(
    inputId = "dateRange", 
    label="Date Range",
    start="2004-01-01",
    end=Sys.Date()
  ),
  selectInput(
    inputId = "assetSelect",
    choices = setNames(as.character(etfs$ETF), paste(etfs$ETF, " -- ", etfs$Type)),
    selected = etfs$ETF,
    label="ETF Selection",
    multiple = TRUE,
    selectize = TRUE
  ),
  dataTableOutput("etfs"),
  actionButton(inputId = "submit", label ="Fetch ETF Data"),
  plotOutput("assetPriceHistory")
)

server <- function(input, output) {

  
  #Show a list of all currently selected ETFs
  output$etfs <- renderDataTable(
    {etfs <- etfs[etfs$ETF %in% input$assetSelect, ]},
    options=list(
      paging=0, # no pagination
      searching=0, # global search box off
      info=0 # information off (how many records filtered, etc)
    )
  )
  
  #Add an observer to the fetch button that populates a data frame of ETF data.
  observeEvent(input$submit, {
    # Loop through the collection of ETFs, fetch their data for the specified date range. This
    # seems like a very non-R way to do this, but it doesn't look like the tq_get function can take
    # multiple stock tickers. I also don't know how the most efficient way to iteratively add big
    # chunks to a Data Frame
    etfs <- etfs[etfs$ETF %in% input$assetSelect, ]
    assetPriceHistory <- NULL
    withProgress(message = 'Making plot', value = 0, {
      for (i in 1:length(etfs$ETF)) {
        incProgress(1/length(etfs$ETF), detail = paste("Fetching stock", i))
        print(etfs$ETF[i])
        curEtf <- tq_get(toString(etfs$ETF[i]),
          get = "stock.prices",
          from = input$dateRange[1],
          to = input$dateRange[2]
        )
        curEtf$etf <- etfs$ETF[i]
        curEtf$type <- etfs$Type[i]
        #Add an column to the ETF data frame that is a 200 day average
        curEtf <- tq_mutate(curEtf, select = adjusted,
                                  mutate_fun = rollapply, FUN = mean,
                                  width = 200, align = "right",
                                  col_rename = "avg200")
        assetPriceHistory <- rbind(assetPriceHistory, curEtf)
      }
      
      assetPriceHistory$position <- "invested"
      assetPriceHistory[which(assetPriceHistory$adjusted <
                                assetPriceHistory$avg200),]$position <- "cash"
  
      print(assetPriceHistory)
      output$assetPriceHistory <- renderPlot({
        assetPriceHistory %>%
          ggplot(aes(x = date, y = adjusted, color = etf)) +
          geom_line() +
          facet_wrap(~ type, scales = "free_y") +
          ggtitle("Asset Clases Line Charts") +
          ylab("Adjusted Closing Price") +
          xlab("") +
          theme_tq() +
          theme(strip.text.x = element_text(size = 7))
      }) #Render
    })# Progress Bar
  })
}

shinyApp(ui = ui, server = server)