library(stringr)
library(data.table)
extract.district <- function(urls = d[, c("href", "date")]) {
    d.ls <- lapply(seq_len(nrow(urls)), function(i) {
        url <- urls$href[i]
        index.page <- read_html(url)
        article <- index.page %>%
            html_elements("#ivs_content") %>%
            html_text()
        ## 字符串匹配
        # 确诊模式
        # 该病例，男，18岁，居住于闵行区东川路800号
        pat.diag <- "病例\\d*[，：][男女]，\\d+岁，[(居住于)(居住地为)][\u4e00-\u9fa5]+区"
        # 无症状模式
        # 该无症状感染者，男，23岁，为入境人员集中隔离点工作人员
        pat.asym <- "无症状感染者\\d*[，：][男女]，\\d+岁(，[(居住于)(居住地为)][\u4e00-\u9fa5]+区)?"
        diagnosed <- unlist(str_extract_all(article, pat.diag))
        asymptomatic <- unlist(str_extract_all(article, pat.asym))
        if (length(diagnosed) == 0) {
              diagnosed <- NULL
          }
        if (length(asymptomatic) == 0) {
              browser()
          }
        list(日期 = urls$date[i], 确诊信息 = diagnosed, 无症状信息 = asymptomatic)
    })
    # 确诊信息数据作成
    d.diag <- lapply(d.ls, function(dd) {
        if (is.null(dd$确诊信息)) {
              return(NULL)
          }
        diagnosed <- dd$确诊信息 %>%
            stringr::str_split_fixed("[，：]", 4) %>%
            as.data.frame()
        names(diagnosed) <- c("病例", "性别", "年龄", "地区")
        diagnosed$日期 <- dd$日期
        is.baby <- grepl("月龄", diagnosed$年龄)
        diagnosed$年龄[is.baby] <- as.integer(gsub("月龄", "", diagnosed$年龄[is.baby])) / 12
        diagnosed$年龄[!is.baby] <- as.integer(gsub("岁", "", diagnosed$年龄[!is.baby]))
        # tmp <- "居住于闵行区龙吴路永德小区"
        # diagnosed$地区 <- gsub("(居住于)|(居住地为)(.+区).*", "\\3", diagnosed$地区)
        diagnosed$地区 <- str_extract(diagnosed$地区, "(?<=于|为).{2,3}?区")
        diagnosed
    })
    d.diag <- rbindlist(d.diag)
    setDF(d.diag)

    # 无症状信息数据作成
    d.asym <- lapply(d.ls, function(dd) {
        if (is.null(dd$无症状信息)) {
              return(NULL)
          }
        asymptomatic <- dd$无症状信息 %>%
            stringr::str_split_fixed("[，：]", 4) %>%
            as.data.frame()
        names(asymptomatic) <- c("病例", "性别", "年龄", "地区")
        asymptomatic$日期 <- dd$日期
        is.baby <- grepl("月龄", asymptomatic$年龄)
        asymptomatic$年龄[is.baby] <- as.integer(gsub("月龄", "", asymptomatic$年龄[is.baby])) / 12
        asymptomatic$年龄[!is.baby] <- as.integer(gsub("岁", "", asymptomatic$年龄[!is.baby]))
        asymptomatic$地区 <- str_extract(asymptomatic$地区, "(?<=于|为).{2,3}?(?<!校)区")
        asymptomatic
    })
    d.asym <- rbindlist(d.asym)
    setDF(d.asym)
    # HACK
    d.diag$年龄 <- as.integer(d.diag$年龄)
    d.asym$年龄 <- as.integer(d.asym$年龄)
    list(
        确诊信息 = d.diag,
        无症状信息 = d.asym
    )
}

if (FALSE) {
    source("extract.district.r")
    info <- extract.district()
}