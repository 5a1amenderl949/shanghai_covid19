# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)

# Use a fluid Bootstrap layout
# 提取地区并加上上海全体
disctrict <- unique(info$无症状信息$地区, info$确诊信息$地区)
disctrict <- disctrict[!is.na(disctrict)]
disctrict <- c("上海", disctrict)

fluidPage(

    # Give the page a title
    titlePanel("上海疫情趋势分析"),

    # Generate a row with a sidebar
    sidebarLayout(
        # Define the sidebar with one input
        sidebarPanel(
            selectInput("地区", "地区:",
                choices = disctrict
            ),
            hr(),
            helpText("数据来源于上海卫健委官方网站。")
        ),

        # Create a spot for the barplot
        mainPanel(
            plotOutput("phonePlot")
        )
    )
)
