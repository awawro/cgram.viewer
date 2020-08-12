#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(parse.masshunter)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chromatogram Viewer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("datafile", "csv datafile", accept = ".csv"),
            uiOutput("select_file"),
            uiOutput("select_precursor"),
            uiOutput("select_product"),
            uiOutput("slider_time_range")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("cgram_plot")),
                tabPanel("Table", tableOutput("cgram_table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    parsed_datafile <- reactive({
        parse_masshunter_csv(input$datafile$datapath)
    })

    cgram_files_all <- reactive(unique(parsed_datafile()$file))
    cgram_precursor_all <- reactive(unique(parsed_datafile()$precursor.ion))
    cgram_product_all <- reactive(unique(parsed_datafile()$product.ion))
    
    output$select_file <- renderUI({
        req(input$datafile)
        selectInput("cgram_files_selected", "select cgram files", choices = cgram_files_all(), multiple = TRUE)
    })
    
    output$select_precursor <- renderUI({
        req(input$datafile)
        selectInput("cgram_precursor_selected", "select precursor ion", choices = cgram_precursor_all(), multiple = TRUE)
    })
    
    cgram_product_available <- reactive({
         MRM_all <- unique(parsed_datafile()[c("precursor.ion", "product.ion")])
         product_available <- filter(MRM_all, precursor.ion %in% input$cgram_precursor_selected)$product.ion
         return(product_available)
    })
    
    output$select_product <- renderUI({
        req(input$datafile)
        selectInput("cgram_product_selected", "select product ion", choices = cgram_product_available(), multiple = TRUE)
    })

    filtered_parsed_datafile <- reactive({
        filter(parsed_datafile(),
            file %in% input$cgram_files_selected &
            precursor.ion %in% input$cgram_precursor_selected &
            product.ion %in% input$cgram_product_selected)
    })
    
    filtered_time_range <- reactive({
        req(input$cgram_product_selected)
        round(c(min(filtered_parsed_datafile()$time), max(filtered_parsed_datafile()$time)), digits = 1)
    })
    
    output$slider_time_range <- renderUI({
        req(input$datafile)
        sliderInput("time_range", "select time range", min = filtered_time_range()[1], max = filtered_time_range()[2], value = c(filtered_time_range()[1], filtered_time_range()[2]), step = 0.05)
    })
    
    output$cgram_plot <- renderPlot({
        req(input$datafile, input$cgram_product_selected)
        ggplot(data = filtered_parsed_datafile(), aes(x = time, y = intensity, color = file)) +
            geom_line() +
            xlim(input$time_range[1], input$time_range[2]) +
            theme_bw() +
            facet_grid(precursor.ion ~ product.ion)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
