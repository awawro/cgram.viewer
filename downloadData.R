# Download full parsed dataset
output$download_all <- renderUI({
  req(input$datafile)
  downloadButton("download_full", label = "Download parsed dataset", style = "width:100%;")
})

output$download_full <- downloadHandler(
  filename = "parsed.csv",
  content = function(file) {
    write.csv(parsed_datafile(), file, row.names = FALSE)
  }
)

# Download filtered parsed dataset
output$download_final <- renderUI({
  req(input$datafile, (!is.null(input$cgram_mrm_selected) | !is.null(input$cgram_sim_selected) | !is.null(input$cgram_eic_selected)))
  downloadButton("download_filtered", label = "Download filtered dataset", style = "width:100%;")
})

output$download_filtered <- downloadHandler(
  filename = "filtered.csv",
  content = function(file) {
    write.csv(filtered_parsed_datafile(), file, row.names = FALSE)
  }
)

# Download PDF handler
output$downloadPdfPlot <- downloadHandler(
  filename = "plot.pdf",
  content = function(file) {
    ggsave(file, vals$gg, width = input$plot_width, height = input$plot_height, device = "pdf")
  }
)

output$download_pdf <- renderUI({
  req(input$datafile, (!is.null(input$cgram_mrm_selected) | !is.null(input$cgram_sim_selected) | !is.null(input$cgram_eic_selected)))
  downloadButton("downloadPdfPlot", label = "Download plot as PDF", style = "width:100%;")
})

