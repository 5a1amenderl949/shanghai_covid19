library(ggplot2)
library(reshape2)
library(rvest)
library(dplyr)
library(stringr)
library(data.table)
library(shinythemes)
library(shinyWidgets)
# Use a fluid Bootstrap layout
# 提取地区并加上上海全体
disctrict <- unique(c(info$无症状信息$地区, info$确诊信息$地区))
disctrict <- disctrict[!is.na(disctrict)]

bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(
        theme = shinytheme("flatly"), collapsible = TRUE,
        HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">上海疫情趋势分析</a>'), id = "nav",
        windowTitle = "上海疫情趋势分析",
        tabPanel(
            "地区病例趋势",
            sidebarLayout(
                sidebarPanel(
                    span(tags$i(h6("数据来源于上海卫健委官方网站。")), style = "color:#045a8d"),
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
                    "请选择地区和开始时间查看疫情趋势变化。"
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("今日新增", textOutput("new_txt"), plotOutput("disctrict_plot")),
                        tabPanel("累计新增", textOutput("cumsum_txt"), plotOutput("disctrict_plot_cumulative"))
                    )
                )
            )
        ),
        tabPanel(
            "详细数据",
            tabsetPanel(
                tabPanel(
                    "总体数据一览", numericInput("maxrows_all", "显示最大行数", 10),
                    dataTableOutput("rawtable_all"),
                    downloadButton("downloadCsv", "下载csv文件"), shiny::tags$br(), shiny::tags$br(),
                    "数据来源于：", tags$a(
                        href = "http://wsjkw.sh.gov.cn/yqtb/",
                        "上海卫健委官方网站"
                    )
                )
                # ),
                # tabPanel(
                #     "确诊者信息", numericInput("maxrows_diag", "显示最大行数", 25),
                #     verbatimTextOutput("rawtable"),
                #     downloadButton("downloadCsv", "下载csv文件"), shiny::tags$br(), shiny::tags$br(),
                #     "数据来源于：", tags$a(
                #         href = "http://wsjkw.sh.gov.cn/yqtb/",
                #         "上海卫健委官方网站"
                #     )
                # ),
                # tabPanel(
                #     "无症状感染者信息", numericInput("maxrows_asym", "显示最大行数", 25),
                #     verbatimTextOutput("rawtable"),
                #     downloadButton("downloadCsv", "下载csv文件"), shiny::tags$br(), shiny::tags$br(),
                #     "数据来源于：", tags$a(
                #         href = "http://wsjkw.sh.gov.cn/yqtb/",
                #         "上海卫健委官方网站"
                #     )
                # )
            )
        ),
        tabPanel(
            "关于",
            tags$div(
                tags$h4("上海疫情趋势分析"),
                "关于大家关注我的github", shiny::tags$br(),
                tags$a(href = "https://github.com/kekincai", "kekincai")
            )
        )
    )
)