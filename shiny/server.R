
# 总体数据一览
out_cols <- c(
    "date",
    "新增本土新冠肺炎确诊病例", "新增本土无症状感染者", "新增境外输入性新冠肺炎确诊病例",
    "新增境外输入性无症状感染者", "治愈出院", "解除医学观察无症状感染者",
    "新增本土确诊（含无症状）", "无症状闭环隔离管控人数", "无症状风险人群筛查人数",
    "非管控区域病例比例"
)
function(input, output) {
    # Fill in the spot we created for a plot
    output$new_txt <- renderText({
        paste0(paste(input$地区, collapse = "，"), "每日新增确诊人数")
    })
    output$disctrict_plot <- renderPlot({
        # 绘制每日新增人数
        # browser()
        d.info <- info$无症状信息[info$无症状信息$地区 %in% input$地区 &
            info$无症状信息$日期 >= input$开始时间, ]
        plot.district(
            d.info = d.info,
            title = ""
        )
    })

    output$cumsum_txt <- renderText({
        paste0(paste(input$地区, collapse = "，"), "累计新增确诊人数")
    })
    output$disctrict_plot_cumulative <- renderPlot({
        # 绘制累计新增人数
        d.info <- info$无症状信息[info$无症状信息$地区 %in% input$地区 &
            info$无症状信息$日期 >= input$开始时间, ]
        plot.district(
            d.info = d.info,
            title = "",
            is.cumsum = TRUE
        )
    })

    # 输出数据一览
    output$rawtable_all <- renderDataTable({
        # 总体数据一览
        if (input$data_type == "全体数据一览") {
            head(d.basic[, out_cols], input$maxrows_all)
        } else if (input$data_type == "确诊信息") {
            # 确诊信息
            head(info$确诊信息, input$maxrows_all)
        } else {
            # 无症状信息
            head(info$无症状信息, input$maxrows_all)
        }
    })
}