library(stringr)
library(data.table)
extract_info <- function(urls = d.basic[, c("href", "date")]) {
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
        diagnosed <- NULL
        if (grepl(pat.diag, article)) {
              diagnosed <- unlist(str_extract_all(article, pat.diag))
          } else {
            # 病例1，居住于徐汇区
            pat.diag <- "病例\\d*，居住于[\u4e00-\u9fa5]+区"
            if (grepl(pat.diag, article)) {
                  diagnosed <- c(diagnosed, unlist(str_extract_all(article, pat.diag)))
              }
            # 病例20—病例27，居住于嘉定区
            pat.diag <- "病例\\d*(—病例\\d*)，居住于[\u4e00-\u9fa5]+区"
            if (grepl(pat.diag, article)) {
                diags <- unlist(str_extract_all(article, pat.diag))
                diags <- unlist(lapply(diags, function(x) {
                    i.s <- as.integer(str_extract(x, "(?<=病例)\\d+(?!，)"))
                    i.e <- as.integer(str_extract(x, "(?<=病例)\\d+(?=，)"))
                    district <- str_extract(x, "(?<=居住于)[\u4e00-\u9fa5]+区")
                    i <- seq(i.s, i.e)
                    paste0("病例", i, "，居住于", district)
                }))
                diagnosed <- c(diagnosed, diags)
            }
        }
        # 无症状模式
        # 该无症状感染者，男，23岁，为入境人员集中隔离点工作人员
        pat.asym <- "无症状感染者\\d*[，：][男女]，\\d+岁(，[(居住于)(居住地为)][\u4e00-\u9fa5]+区)?"
        asymptomatic <- NULL
        if (grepl(pat.asym, article)) {
              asymptomatic <- unlist(str_extract_all(article, pat.asym))
          } else {
            # 无症状感染者2631，居住于崇明区
            pat.asym <- "无症状感染者\\d*，居住于[\u4e00-\u9fa5]+区"
            if (grepl(pat.asym, article)) {
                  asyms <- c(asymptomatic, unlist(str_extract_all(article, pat.asym)))
              }
            # 无症状感染者1937—无症状感染者2162，居住于嘉定区
            pat.asym <- "无症状感染者\\d*(—无症状感染者\\d*)，居住于[\u4e00-\u9fa5]+区"
            if (grepl(pat.asym, article)) {
                asyms <- unlist(str_extract_all(article, pat.asym))
                asyms <- unlist(lapply(asyms, function(x) {
                    # browser()
                    i.s <- as.integer(str_extract(x, "(?<=无症状感染者)\\d+(?!，)"))
                    i.e <- as.integer(str_extract(x, "(?<=无症状感染者)\\d+(?=，)"))
                    district <- str_extract(x, "(?<=居住于)[\u4e00-\u9fa5]+区")
                    i <- seq(i.s, i.e)
                    paste0("无症状感染者", i, "，居住于", district)
                }))
                asymptomatic <- c(asymptomatic, asyms)
            }
        }

        list(日期 = urls$date[i], 确诊信息 = diagnosed, 无症状信息 = asymptomatic)
    })
    # browser()
    # 确诊信息数据作成
    d.diag <- lapply(d.ls, function(dd) {
        if (is.null(dd$确诊信息)) {
            return(NULL)
        }
        diagnosed <- dd$确诊信息 %>%
            stringr::str_split_fixed("[，：]", 4) %>%
            as.data.frame()
        if (all(diagnosed$V3 == "" & diagnosed$V4 == "")) {
              names(diagnosed) <- c("病例", "地区", "年龄", "性别")
          } else {
              names(diagnosed) <- c("病例", "性别", "年龄", "地区")
          }
        diagnosed <- diagnosed[, c("病例", "性别", "年龄", "地区")]
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
        if (all(asymptomatic$V3 == "" & asymptomatic$V4 == "")) {
              names(asymptomatic) <- c("病例", "地区", "年龄", "性别")
          } else {
              names(asymptomatic) <- c("病例", "性别", "年龄", "地区")
          }
        asymptomatic <- asymptomatic[, c("病例", "性别", "年龄", "地区")]
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
    source("extract_info.r")
    info <- extract_info()
    write.csv(d.basic, "../output/上海疫情感染信息一览.csv", row.names = FALSE)
    write.csv(d.basic, "../output/上海疫情感染信息一览.csv", row.names = FALSE)
}