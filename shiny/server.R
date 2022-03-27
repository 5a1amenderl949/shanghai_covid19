
function(input, output) {
    # Fill in the spot we created for a plot
    output$phonePlot <- renderPlot({
        # Render a plot
        if (input$地区=="上海")
          d.info <- info$无症状信息
        else
          d.info <- info$无症状信息[info$无症状信息$地区 %in% input$地区, ]
        plot.district(
            d.info = d.info,
            title = paste0(input$地区, "每日新增人数")
        )
    })
}
