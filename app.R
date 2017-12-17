library(shiny)
library("knitr")
library("ggplot2")
library("gplots")
library("scales")
library("ggalt")
library("tidyquant")
library("reshape2")

today <- Sys.Date()

etfs <- data.frame(ETF = c("DBA", "DBC", "IFGL", "IYR", "TIP",
                           "AGG", "IEMG", "IEFA", "IJR", "ITOT"),
                   Type = c("Agricultural", "Commodities", "Intl Real Estate",
                            "US Real Estate", "TIPs Bonds", "US Aggregated Bonds",
                            "Emerging Markets", "Foreign Total", "S&P Small Cap",
                            "S&P Total"),
                   "Commission Free" = c("No", "No", "Yes", "No", "Yes",
                                         "Yes", "Yes", "Yes", "Yes", "Yes"),
                   check.names = FALSE)

ui <- fluidPage(
  dateInput(
    inputId = "startDate", 
    label="Starting Date",
    value=Sys.Date()
  ),
  dateInput(
    inputId = "endDate", 
    label="Ending Range",
    value=Sys.Date()
  ),
  dataTableOutput("etfs")
)

server <- function(input, output) {
  output$etfs <- renderDataTable({
    title <- "ETF Information"
    etfs
  })
}

shinyApp(ui = ui, server = server)