#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
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
            prettySwitch("display_options", strong("Show Display Options"), status = "success", fill = TRUE), 
            prettySwitch("export_options", strong("Show Export Options"), status = "success", fill = TRUE), 
            helpText("Upload a .csv file containing MRM chromatogram data"),
            fileInput("datafile", "csv datafile", accept = ".csv"),
            uiOutput("select_file"),
            uiOutput("select_precursor"),
            uiOutput("select_product"),
            uiOutput("slider_time_range"),
            conditionalPanel(condition = "input.display_options == true",
                h4(strong("Display Options")),
                prettySwitch("fix_y_axis", "Lock y-axis", status = "success", fill = TRUE),
                fluidRow(
                    column(6, sliderInput("legend_cols", "Legend columns", min = 1, max = 5, value = 2, step = 1),),
                    column(6, sliderInput("line_size", "Line (pt)", min = 0.2, max = 2,value = 1, step = 0.2))
                ),
                fluidRow(
                    column(6, sliderInput("plot_width", "Width (px)", min = 200, max = 1200, value = 400, step = 10)),
                    column(6, sliderInput("plot_height", "Height (px)", min = 200, max = 1200, value = 400, step = 10))
                )
            ),
            conditionalPanel(condition = "input.export_options == true",
                h4(strong("Export Options")),
                fluidRow(
                    column(6, numericInput("out_width", "Width (in)", 5)),
                    column(6, numericInput("out_height", "Height (in)", 5))
                ),
                fluidRow(
                    column(4, downloadButton("downloadPdfPlot", label = "PDF")),
                    column(4, downloadButton("downloadPngPlot", label = "PNG")),
                    column(4, downloadButton("downloadTifPlot", label = "TIFF"))
                )
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", 
                         plotOutput("cgram_plot")),
                tabPanel("Table", tableOutput("cgram_table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    vals <- reactiveValues()
    
    parsed_datafile <- reactive({
        parse_masshunter_csv(input$datafile$datapath)
    })

    cgram_files_all <- reactive(unique(parsed_datafile()$file))
    cgram_precursor_all <- reactive(unique(parsed_datafile()$precursor.ion))
    cgram_product_all <- reactive(unique(parsed_datafile()$product.ion))
    
    output$select_file <- renderUI({
        req(input$datafile)
        pickerInput("cgram_files_selected", "select cgram files", choices = cgram_files_all(), multiple = TRUE)
    })
    
    output$select_precursor <- renderUI({
        req(input$datafile)
        pickerInput("cgram_precursor_selected", "select precursor ion", choices = cgram_precursor_all(), multiple = TRUE)
    })
    
    cgram_product_available <- reactive({
         MRM_all <- unique(parsed_datafile()[c("precursor.ion", "product.ion")])
         product_available <- filter(MRM_all, precursor.ion %in% input$cgram_precursor_selected)$product.ion
         return(product_available)
    })
    
    output$select_product <- renderUI({
        req(input$datafile)
        pickerInput("cgram_product_selected", "select product ion", choices = cgram_product_available(), multiple = TRUE)
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
        
        scales_y <- if_else(input$fix_y_axis, "fixed", "free_y")
        
        gg <- ggplot(data = filtered_parsed_datafile(), aes(x = time, y = intensity, color = file)) +
            geom_line(size = input$line_size) +
            xlim(input$time_range[1], input$time_range[2]) +
            theme_bw() +
            theme(legend.position = "bottom") +
            guides(color = guide_legend(ncol = input$legend_cols)) +
            facet_wrap(.~interaction(precursor.ion,product.ion, sep = " -> "), scales = scales_y)
        
        vals$gg <- gg
        
        print(gg)
    }, width = function() input$plot_width, height = function() input$plot_height)
    
    output$downloadPdfPlot <- downloadHandler(
        filename = "plot.pdf",
        content = function(file) {
            ggsave(file, vals$gg, width = input$out_width, height = input$out_height, device = "pdf")
        }
    )
    
    output$downloadPngPlot <- downloadHandler(
        filename = "plot.png",
        content = function(file) {
            ggsave(file, vals$gg, width = input$out_width, height = input$out_height, device = "png")
        }
    )
    
    output$downloadTifPlot <- downloadHandler(
        filename = "plot.tiff",
        content = function(file) {
            ggsave(file, vals$gg, width = input$out_width, height = input$out_height, device = "tiff")
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
