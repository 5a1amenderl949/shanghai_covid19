library(rvest)
download.file <- function(url = "http://wsjkw.sh.gov.cn/yqtb/index.html") { # nolint
    browser()
    index.page <- read_html(url)
}

if (FALSE) {
    source("download.file.r")
    download.file()
}