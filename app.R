library(shiny)
library("knitr")
library("ggplot2")
library("gplots")
library("scales")
library("ggalt")
library("tidyquant")
library("reshape2")

ui <- fluidPage(
  dateRangeInput(
    inputId = "dateRange", 
    label="Date Range",
    start="2004-01-01",
    end=Sys.Date()
  ),
  dataTableOutput("etfs"),
  actionButton(inputId = "submit", label ="Fetch ETF Data"),
  plotOutput("assetPriceHistory")
)

server <- function(input, output) {
  
  #Set the detault set of ETFs
  etfs <- data.frame(ETF = c("DBA", "DBC", "IFGL", "IYR", "TIP",
                             "AGG", "IEMG", "IEFA", "IJR", "ITOT"),
                     Type = c("Agricultural", "Commodities", "Intl Real Estate",
                              "US Real Estate", "TIPs Bonds", "US Aggregated Bonds",
                              "Emerging Markets", "Foreign Total", "S&P Small Cap",
                              "S&P Total"),
                     "Commission Free" = c("No", "No", "Yes", "No", "Yes",
                                           "Yes", "Yes", "Yes", "Yes", "Yes"),
                     check.names = FALSE)
  
  # Test data set
  # etfs <- data.frame(ETF = c("DBA", "DBC"),
  #                    Type = c("Agricultural", "Commodities"),
  #                    "Commission Free" = c("No", "No"),
  #                    check.names = FALSE)
  
  #Show the user the ETFs we are using
  output$etfs <- renderDataTable({
    title <- "ETF Information"
    etfs
  })
  
  #Add an observer to the fetch button that populates a data frame of ETF data.
  observeEvent(input$submit, {
    
    # Loop through the collection of ETFs, fetch their data for the specified date range. This
    # seems like a very non-R way to do this, but it doesn't look like the tq_get function can take
    # multiple stock tickers. I also don't know how the most efficient way to iteratively add big
    # chunks to a Data Frame
    assetPriceHistory <- NULL
    for (i in 1:length(etfs$ETF)) {
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
    
    ######### TEST DATA
    # ETF Data
    # today <- Sys.Date()
    # agricultural <- tq_get("DBA", get = "stock.prices",
    #                        from = "2004-01-01", to = today)
    # agricultural$etf <- "DBA"
    # agricultural$type <- etfs[etfs$ETF == "DBA",]$Type
    # agricultural <- tq_mutate(agricultural, select = adjusted,
    #                           mutate_fun = rollapply, FUN = mean,
    #                           width = 200, align = "right",
    #                           col_rename = "avg200")
    # 
    # commodities <- tq_get("DBC", get = "stock.prices",
    #                       from = "2004-01-01", to = today)
    # commodities$etf <- "DBC"
    # commodities$type <- etfs[etfs$ETF == "DBC",]$Type
    # commodities <- tq_mutate(commodities, select = adjusted,
    #                          mutate_fun = rollapply, FUN = mean,
    #                          width = 200, align = "right",
    #                          col_rename = "avg200")
    # assetPriceHistory <- rbind(agricultural, commodities)
    ######### TEST DATA
    
    
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
    })
  })
}

shinyApp(ui = ui, server = server)