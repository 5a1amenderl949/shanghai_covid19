if (!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if (!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if (!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if (!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if (!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")

message("0. 导入画图函数")
source("plot.r")
message("1. 爬取上海卫健委网站")
source("web_crawler.r")

message("2. 提取确诊和无症状感染者信息")
source("extract_info.r")

# 总体数据一览
out_cols <- c(
    "date",
    "新增本土新冠肺炎确诊病例", "新增本土无症状感染者", "新增境外输入性新冠肺炎确诊病例",
    "新增境外输入性无症状感染者", "治愈出院", "解除医学观察无症状感染者",
    "新增本土确诊含无症状", "无症状闭环隔离管控人数", "无症状风险人群筛查人数",
    "非管控区域病例比例"
)

# Use a fluid Bootstrap layout
message("3. 网站启动")
ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(
        theme = shinytheme("flatly"), collapsible = TRUE,
        HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">上海疫情趋势分析</a>'), id = "nav",
        windowTitle = "上海疫情趋势分析",
        tabPanel(
            "地区病例趋势",
            sidebarLayout(
                sidebarPanel(
                    span(tags$i(
                        h6("数据来源于："),
                        tags$a(
                            href = "http://wsjkw.sh.gov.cn/yqtb/",
                            h6("上海卫健委官方网站")
                        )
                    ), style = "color:#045a8d"),
                    selectInput(
                        "plot_type",
                        "确诊类型：",
                        choices = c("确诊信息", "无症状信息"),
                        selected = "无症状信息"
                    ),
                    pickerInput("地区", "地区：",
                        choices = disctrict,
                        options = list(`actions-box` = TRUE, `none-selected-text` = "请选择地区！"),
                        selected = disctrict[1],
                        multiple = TRUE
                    ),
                    sliderInput("开始时间",
                        "开始时间：",
                        min = MIN.DATE,
                        max = Sys.Date(),
                        value = MIN.DATE,
                        timeFormat = "%m月%d日"
                    ),
                    selectInput(
                        "is_log",
                        "对数轴：",
                        choices = c("是", "否"),
                        selected = "否"
                    ),
                    "请选择地区和开始时间查看疫情趋势变化。"
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("今日新增", htmlOutput("new_txt"), plotOutput("disctrict_plot")),
                        tabPanel("累计新增", htmlOutput("cumsum_txt"), plotOutput("disctrict_plot_cumulative"))
                    )
                )
            )
        ),
        tabPanel(
            "非管控区域病例比例", plotOutput("ratio_plot")
        ),
        tabPanel(
            "详细数据",
            selectInput(
                "data_type",
                "数据类型：",
                choices = c("全体数据一览", "确诊信息", "无症状信息"),
                selected = "全体数据一览"
            ),
            numericInput("maxrows_all", "显示最大行数", 5),
            dataTableOutput("rawtable_all"),
            downloadButton("downloadCsv", "下载csv文件"), shiny::tags$br(), shiny::tags$br(),
            "数据来源于：", tags$a(
                href = "http://wsjkw.sh.gov.cn/yqtb/",
                "上海卫健委官方网站"
            )
        ),
        tabPanel(
            "关于",
            tags$div(
                tags$h4("上海疫情趋势分析"),
                "关于大家关注我的",
                tags$a(href = "https://github.com/kekincai", "Github"),
                shiny::tags$br(), shiny::tags$br(),
                "参考项目",
                tags$a(href = "https://shiny.rstudio.com/gallery/covid19-tracker.html", "covid19-tracker")
            )
        )
    )
)

server <- function(input, output) {

    # Fill in the spot we created for a plot
    output$new_txt <- renderUI({
        HTML(paste0("<h4>", paste(input$地区, collapse = "，"), "每日新增人数", "</h4>"))
    })
    output$disctrict_plot <- renderPlot({
        # 绘制每日新增人数
        # browser()
        is_log <- ifelse(input$is_log == "是", TRUE, FALSE)
        d.info <- info[[input$plot_type]]
        d.info <- d.info[d.info$地区 %in% input$地区 &
            d.info$日期 >= input$开始时间, ]
        plot.district(
            d.info = d.info,
            title = "",
            is_log = is_log
        )
    })

    output$cumsum_txt <- renderUI({
        HTML("<h4>", paste0(paste(input$地区, collapse = "，"), "累计新增人数", "</h4>"))
    })
    output$disctrict_plot_cumulative <- renderPlot({
        # 绘制累计新增人数
        is_log <- ifelse(input$is_log == "是", TRUE, FALSE)
        d.info <- info[[input$plot_type]]
        d.info <- d.info[d.info$地区 %in% input$地区 &
            d.info$日期 >= input$开始时间, ]
        plot.district(
            d.info = d.info,
            title = "",
            is.cumsum = TRUE,
            is_log = is_log
        )
    })
    # 绘制非管控区域病例比例
    output$ratio_plot <- renderPlot({
        plot.ratio()
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
    # 下载csv文件
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste0(input$data_type, ".csv")
        },
        content = function(file) {
            # 总体数据一览
            if (input$data_type == "全体数据一览") {
                d_csv <- d.basic[, out_cols]
            } else if (input$data_type == "确诊信息") {
                # 确诊信息
                d_csv <- info$确诊信息
            } else {
                # 无症状信息
                d_csv <- info$无症状信息
            }
            write.csv(d_csv, file)
        }
    )
}

shinyApp(ui, server)