library(shiny)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(ggplot2)
library(DT)
library(parse.masshunter)

options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(

    titlePanel("Chromatogram Viewer"),

    sidebarLayout(
        sidebarPanel(
            fileInput("datafile", "Load a .csv export file", accept = ".csv"),
            radioGroupButtons("side_panel", label = NULL, choices = c("Data", "Display", "Export"), justified = TRUE, status = "primary"),
            # Data panel
            conditionalPanel(condition = "input.side_panel == 'Data'",
                div(style="margin-bottom:15px"),
                uiOutput("select_data"),
                uiOutput("select_file"),
                conditionalPanel(condition = "input.data_type == 'MRM'",
                                 uiOutput("select_mrm")
                ),
                conditionalPanel(condition = "input.data_type == 'SIM'",
                                 uiOutput("select_sim")
                ),
                conditionalPanel(condition = "input.data_type == 'EIC'",
                                 uiOutput("select_eic")
                ),
                uiOutput("slider_time_range")
            ),
            # Display panel
            conditionalPanel(condition = "input.side_panel == 'Display'",
                h4(strong("Display Options")),
                prettySwitch("fix_y_axis", "Lock y-axis", status = "success", fill = TRUE),
                selectInput("facet_by", "Facet by:", choices = c("ion", "file")),
                fluidRow(
                    column(6, sliderInput("legend_cols", "Legend cols", min = 1, max = 5, value = 2, step = 1),),
                    column(6, sliderInput("line_size", "Line (pt)", min = 0.25, max = 1.5 ,value = 0.5, step = 0.25))
                ),
                fluidRow(
                    column(6, sliderInput("plot_width", "Width (in)", min = 4, max = 12, value = 6, step = 0.25)),
                    column(6, sliderInput("plot_height", "Height (in)", min = 4, max = 12, value = 6, step = 0.25))
                )
            ),
            # Export panel
            conditionalPanel(condition = "input.side_panel == 'Export'",
                h4(strong("Export Options")),
                uiOutput("download_all"),
                div(style="margin-bottom:10px"),
                uiOutput("download_final"),
                div(style="margin-bottom:10px"),
                uiOutput("download_pdf")
            )
            
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Plot", 
                         plotOutput("cgram_plot")),
                tabPanel("Parsed",
                         DTOutput("parsed_table")),
                tabPanel("Filtered",
                         DTOutput("filtered_table"))
            )
        )
    )
)

server <- function(input, output, session) {
    
    # download handlers and outputs
    source("downloadData.R", local = TRUE)

    # initialize reactive values for ggplot storage
    vals <- reactiveValues()
    
    # use parse.masshunter
    parsed_datafile <- reactive({
        parse_masshunter_csv(input$datafile$datapath) %>%
            {if("MRM" %in% .$type) . <- mutate(., MRM = paste(precursor.ion, product.ion, sep = " -> ")) else .}
    })
    
    output$select_data <- renderUI({
        req(input$datafile)
        selectInput("data_type", "Select data type", choices = unique(parsed_datafile()$type))
    })
    
    files_available <- reactive({
        parsed_datafile() %>%
        filter(type == input$data_type) %>%
        pull(file) %>%
        unique()
    })
    
    output$select_file <- renderUI({
        req(input$datafile)
        pickerInput("cgram_files_selected", "select cgram files", choices = files_available(), multiple = TRUE)
    })
    
    output$select_mrm <- renderUI({
        req(input$datafile, input$data_type == "MRM")
        MRMs_available <- parsed_datafile() %>%
            filter(file %in% input$cgram_files_selected) %>%
            pull(MRM) %>%
            unique()
        pickerInput("cgram_mrm_selected", "select MRMs", choices = MRMs_available, multiple = TRUE)
    })
    
    output$select_sim <- renderUI({
        req(input$datafile, input$data_type == "SIM")
        SIMs_available <- parsed_datafile() %>%
            filter(file %in% input$cgram_files_selected) %>%
            pull(SIM) %>%
            unique()
        pickerInput("cgram_sim_selected", "select SIMs", choices = SIMs_available, multiple = TRUE)
    })
    
    output$select_eic <- renderUI({
        req(input$datafile, input$data_type == "EIC")
        EICs_available <- parsed_datafile() %>%
            filter(file %in% input$cgram_files_selected) %>%
            pull(EIC) %>%
            unique()
        pickerInput("cgram_eic_selected", "select EICs", choices = EICs_available, multiple = TRUE)
    })
    
    ions_selected <- reactive({
        case_when(
            input$data_type == "MRM" ~ list(input$cgram_mrm_selected),
            input$data_type == "SIM" ~ list(input$cgram_sim_selected),
            input$data_type == "EIC" ~ list(input$cgram_eic_selected)) %>%
            unlist()
    })
    
    filtered_parsed_datafile <- reactive({
        filter(parsed_datafile(),
                file %in% input$cgram_files_selected &
                !!sym(input$data_type) %in% ions_selected()
                )
    })

    output$slider_time_range <- renderUI({
        req(input$datafile, (!is.null(input$cgram_mrm_selected) | !is.null(input$cgram_sim_selected) | !is.null(input$cgram_eic_selected)))
        filtered_time_range <- round(c(min(filtered_parsed_datafile()$time), max(filtered_parsed_datafile()$time)), digits = 1)
        sliderInput("time_range", "select time range", min = filtered_time_range[1], max = filtered_time_range[2], value = c(filtered_time_range[1], filtered_time_range[2]), step = 0.05)
    })
    
    # render plot
    output$cgram_plot <- renderPlot({
        req(input$datafile, (!is.null(input$cgram_mrm_selected) | !is.null(input$cgram_sim_selected) | !is.null(input$cgram_eic_selected)))
        
        scales_y <- if_else(input$fix_y_axis, "fixed", "free_y")
        fac_col <- if_else(rep(input$facet_by == "ion", 2), c(input$data_type, "file"), c("file", input$data_type))
        
        gg <- ggplot(data = filtered_parsed_datafile(), aes(x = time, y = intensity, color = !!sym(fac_col[2]))) +
            geom_line(size = input$line_size) +
            xlim(input$time_range[1], input$time_range[2]) +
            theme_bw() +
            labs(x = "time / min", y = "intensity", color = fac_col[2]) +
            theme(legend.position = "bottom") +
            guides(color = guide_legend(ncol = input$legend_cols)) +
            facet_wrap(vars(!!sym(fac_col[1])), scales = scales_y)
        
        vals$gg <- gg
        
        print(gg)
    }, res = 72, width = function() {input$plot_width * 72}, height = function() {input$plot_height * 72})

    output$parsed_table <- renderDT({
        parsed_datafile()
    })
    
    output$filtered_table <- renderDT({
        filtered_parsed_datafile()
    })
    
}

shinyApp(ui = ui, server = server)
