#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Rjsr)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(htmlwidgets)
library(cowplot)

report.choices <- c(
    "2024 Spring Table1",
    "2024 Spring Table2",
    "2024 Spring Table5",
    "2023 Spring Table2",
    "2022 Spring Table2",
    "2023 Fall Table3",
    "2022 Fall Table5",
    "2022 Fall Table6"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("View Joint Staff Report Tables"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width=2,
            pickerInput(inputId = "report", label = "Report", choices = report.choices, options = list(`actions-box` = TRUE),multiple=F)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("reportTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$reportTable <- DT::renderDataTable(server = FALSE,{

        parsed.input <- strsplit(input$report,split=" ")[[1]]
        yr <- parsed.input[1]
        rpt <- parsed.input[2]
        tbl <- parsed.input[3] %>% substring(6) %>% as.numeric()
        print(parsed.input)
        print(paste(rpt,yr,tbl))
        
        
        table <- getTable(report=rpt,year=yr,tablenum=tbl)
        
        table$body
    },
    extensions = 'Buttons',
    options = list(scrollX = TRUE,
                   searching = FALSE,
                   dom = 'Bfrtip',
                   pageLength = 30,
                   buttons = c('colvis', 'csv')),
    selection = "single",
    escape = FALSE,
    rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
