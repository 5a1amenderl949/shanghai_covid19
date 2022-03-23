library(rvest)
library(dplyr)
gsub2 <- function(pattern, replacement, x, def = 0, ...) {
    ifelse(grepl(pattern, x),
        gsub(pattern, replacement, x, ...),
        def
    )
}
download.file <- function(urls = c(
                              "http://wsjkw.sh.gov.cn/yqtb/index.html",
                              "https://wsjkw.sh.gov.cn/yqtb/index_2.html"
                          )) { # nolint
    # browser()
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
    d
}

if (FALSE) {
    source("download.file.r")
    d <- download.file()
}