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
library(DT)
library(parse.masshunter)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chromatogram Viewer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("datafile", "Load a .csv export file", accept = ".csv"),
            radioGroupButtons("side_panel", label = NULL, choices = c("Data", "Display", "Export"), justified = TRUE, status = "primary"),
            conditionalPanel(condition = "input.side_panel == 'Data'",
                uiOutput("download_all"),
                div(style="margin-bottom:15px"),
                uiOutput("select_file"),
                uiOutput("select_mrm"),
                uiOutput("slider_time_range"),
                uiOutput("download_final")
            ),
            conditionalPanel(condition = "input.side_panel == 'Display'",
                h4(strong("Display Options")),
                prettySwitch("fix_y_axis", "Lock y-axis", status = "success", fill = TRUE),
                fluidRow(
                    column(6, sliderInput("legend_cols", "Legend cols", min = 1, max = 5, value = 2, step = 1),),
                    column(6, sliderInput("line_size", "Line (pt)", min = 0.2, max = 2,value = 0.8, step = 0.1))
                ),
                fluidRow(
                    column(6, sliderInput("plot_width", "Width (px)", min = 200, max = 1200, value = 400, step = 10)),
                    column(6, sliderInput("plot_height", "Height (px)", min = 200, max = 1200, value = 400, step = 10))
                )
            ),
            conditionalPanel(condition = "input.side_panel == 'Export'",
                h4(strong("Export Options")),
                fluidRow(
                    column(6, numericInput("out_width", "Width (in)", 5)),
                    column(6, numericInput("out_height", "Height (in)", 5))
                ),
                fluidRow(
                    column(3),
                    column(6, numericInput("dpi", "DPI", 300)),
                    column(3)
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
                tabPanel("Table", DTOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    vals <- reactiveValues()
    
    parsed_datafile <- reactive({
        parse_masshunter_csv(input$datafile$datapath) %>%
            mutate(MRM = paste(precursor.ion, product.ion, sep = " -> "))
    })
    
    output$download_all <- renderUI({
        req(input$datafile)
        downloadButton("download_full", label = "Download parsed dataset", style = "width:100%;")
    })
    
    output$download_full <- downloadHandler(
        filename = "parsed.csv",
        content = function(file) {
            write.csv(select(parsed_datafile(), -"MRM"), file, row.names = FALSE)
        }
    )
    
    cgram_files_all <- reactive(unique(parsed_datafile()$file))

    output$select_file <- renderUI({
        req(input$datafile)
        pickerInput("cgram_files_selected", "select cgram files", choices = cgram_files_all(), multiple = TRUE)
    })
    
    cgram_mrms <- reactive({
        unique(parsed_datafile()$MRM)
    })
    
    output$select_mrm <- renderUI({
        req(input$datafile)
        pickerInput("cgram_mrm_selected", "select MRMs", choices = cgram_mrms(), multiple = TRUE)
    })

    filtered_parsed_datafile <- reactive({
        filter(parsed_datafile(), file %in% input$cgram_files_selected & MRM %in% input$cgram_mrm_selected)
    })
    
    output$download_final <- renderUI({
        req(input$cgram_mrm_selected)
        downloadButton("download_filtered", label = "Download filtered dataset", style = "width:100%;")
    })
    
    output$download_filtered <- downloadHandler(
        filename = "filtered.csv",
        content = function(file) {
            write.csv(select(filtered_parsed_datafile(), -"MRM"), file, row.names = FALSE)
        }
    )
    
    output$table <- renderDT({
        req(input$datafile)
        filtered_parsed_datafile() %>%
            select(file:product.ion) %>%
            unique()
    })
    
    filtered_time_range <- reactive({
        req(input$cgram_mrm_selected)
        round(c(min(filtered_parsed_datafile()$time), max(filtered_parsed_datafile()$time)), digits = 1)
    })
    
    output$slider_time_range <- renderUI({
        req(input$datafile)
        sliderInput("time_range", "select time range", min = filtered_time_range()[1], max = filtered_time_range()[2], value = c(filtered_time_range()[1], filtered_time_range()[2]), step = 0.05)
    })
    
    output$cgram_plot <- renderPlot({
        req(input$datafile, input$cgram_mrm_selected)
        
        scales_y <- if_else(input$fix_y_axis, "fixed", "free_y")
        
        gg <- ggplot(data = filtered_parsed_datafile(), aes(x = time, y = intensity, color = file)) +
            geom_line(size = input$line_size) +
            xlim(input$time_range[1], input$time_range[2]) +
            theme_bw() +
            labs(x = "time / min", y = "intensity", color = "sample filename") +
            theme(legend.position = "bottom") +
            guides(color = guide_legend(ncol = input$legend_cols)) +
            facet_wrap(.~MRM, scales = scales_y)
        
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
            ggsave(file, vals$gg, width = input$out_width, height = input$out_height, device = "png", dpi = input$dpi)
        }
    )
    
    output$downloadTifPlot <- downloadHandler(
        filename = "plot.tiff",
        content = function(file) {
            ggsave(file, vals$gg, width = input$out_width, height = input$out_height, device = "tiff", dpi = input$dpi)
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
