# Download full parsed dataset
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

# Download filtered parsed dataset
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

# Download PDF handler
output$downloadPdfPlot <- downloadHandler(
  filename = "plot.pdf",
  content = function(file) {
    ggsave(file, vals$gg, width = input$plot_width, height = input$plot_height, device = "pdf")
  }
)