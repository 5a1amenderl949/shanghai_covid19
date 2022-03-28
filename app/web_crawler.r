library(rvest)
library(dplyr)
MIN.DATE <- as.Date("2022-03-01")
gsub2 <- function(pattern, replacement, x, def = 0, ...) {
    ifelse(grepl(pattern, x),
        gsub(pattern, replacement, x, ...),
        def
    )
}
web_crawler <- function(url = "http://wsjkw.sh.gov.cn/yqtb/",
                        page.len = 3,
                        d.old = d.basic,
                        add = TRUE) {

    # 数据追加的情况
    if (add) {
        if (!exists("d.old")) {
            stop("原有数据不存在")
        } else {
            latest.date <- max(d.old$date)
        }
    }

    # 遍历页面数
    page.seq <- 2:page.len
    # 遍历网页url作成
    urls <- paste0(url, "index_", page.seq, ".html")
    urls <- c(url, urls)
    href <- NULL
    ttl <- NULL
    update.time <- NULL
    for (i in seq_along(urls)) {
        index.page <- read_html(urls[i])
        nodes <- index.page %>% html_elements(".uli16 a")
        update.time <- c(
            update.time,
            index.page %>% html_elements(".time") %>% html_text()
        )
        href <- c(href, unlist(nodes %>% html_attr("href")))
        ttl <- c(ttl, unlist(nodes %>% html_text()))
    }

    # 把每天发布的确诊信息网页url提取出来
    d <- data.frame(
        href = href,
        update.time = update.time
    )
    # 只要确诊人员详细分布的网页
    pat <- paste0(
        "(", "上海[[:digit:]]{4}年[[:digit:]]月[[:digit:]]+日.*新增本土新冠肺炎确诊病例.*",
        ")|", "(",
        "上海新增(.+本土确诊病例)?.+本土无症状感染者",
        ")"
    )
    use <- grepl(pat, ttl)
    ttl <- ttl[use]
    d <- d[use, ]
    d$date <- as.Date(gsub2(
        ".+([[:digit:]]{4}年[[:digit:]]月[[:digit:]]+日).+",
        "\\1",
        ttl
    ),
    format = "%Y年%m月%d日"
    )
    idx.latest <- which(d$date > latest.date)
    ttl <- ttl[idx.latest]
    d <- d[idx.latest, ]
    if (nrow(d) == 0) {
          return(d.old)
      }

    d$href <- paste0("http://wsjkw.sh.gov.cn", d$href)

    d$新增本土新冠肺炎确诊病例 <- as.integer(gsub2(
        "上海.+新增本土新冠肺炎确诊病例([[:digit:]]+)例.+",
        "\\1",
        ttl
    ))
    d$新增本土无症状感染者 <- as.integer(gsub2(
        "上海.+新增本土无症状感染者([[:digit:]]+)例.+",
        "\\1",
        ttl
    ))
    d$新增境外输入性新冠肺炎确诊病例 <- as.integer(gsub2(
        "上海.+新增境外输入性新冠肺炎确诊病例([[:digit:]]+)例.+",
        "\\1",
        ttl
    ))
    d$新增境外输入性无症状感染者 <- as.integer(gsub2(
        "上海.+新增境外输入性无症状感染者([[:digit:]]+)例.+",
        "\\1",
        ttl
    ))
    d$治愈出院 <- as.integer(gsub2(
        "上海.+治愈出院([[:digit:]]+)例.+",
        "\\1",
        ttl
    ))
    d$解除医学观察无症状感染者 <- as.integer(gsub2(
        "上海.+解除医学观察无症状感染者([[:digit:]]+)例$",
        "\\1",
        ttl
    ))
    # 3月13日数据修正
    need.fix <- is.na(d$date)
    d$date[need.fix] <- d$update.time[need.fix]
    d$新增本土新冠肺炎确诊病例[need.fix] <- as.integer(gsub2(
        "上海新增([[:digit:]]+)例本土确诊病例.+",
        "\\1",
        ttl[need.fix]
    ))
    d$新增本土无症状感染者[need.fix] <- as.integer(gsub2(
        "上海.+和([[:digit:]]+)例本土无症状感染者.+",
        "\\1",
        ttl[need.fix]
    ))
    d$`新增本土确诊含无症状` <- d$新增本土新冠肺炎确诊病例 + d$新增本土无症状感染者

    # 无症状感染者1455，男，21岁，居住于长宁区，均为本市闭环隔离管控人员，其间新冠病毒核酸检测结果异常，经市疾控中心复核结果为阳性。
    # 无症状感染者1580，男，27岁，居住于杨浦区，在风险人群筛查中发现新冠病毒核酸检测结果异常，即被隔离管控。
    num.closed <- sapply(seq_len(nrow(d)), function(i) {
        url <- d$href[i]
        index.page <- read_html(url)
        article <- index.page %>%
            html_elements("#ivs_content") %>%
            html_text()
        ## 字符串匹配
        # 闭环隔离管控人数调查模式
        # 无症状感染者2338—无症状感染者2363，居住于崇明区，均为本市闭环隔离管控人员
        pat.closed <- ".+无症状感染者(\\d+)，.+闭环隔离管控人员.+"
        as.integer(gsub2(pat.closed, "\\1", article))
    })
    d$无症状闭环隔离管控人数 <- num.closed

    d$无症状风险人群筛查人数 <- d$新增本土无症状感染者 - d$无症状闭环隔离管控人数
    d$非管控区域病例比例 <- d$无症状风险人群筛查人数 / d$新增本土无症状感染者 * 100
    d[d$date >= MIN.DATE, ]
    if (add) {
        rbind(d, d.old)
    } else {
        d
    }
}

if (TRUE) {
    fn <- "上海疫情感染信息一览.csv"
    message("读取：", fn)
    d.basic <- read.csv(fn)
    d.basic <- web_crawler()
    # write.csv(d.basic, "../output/上海疫情感染信息一览.csv", row.names = FALSE)
}