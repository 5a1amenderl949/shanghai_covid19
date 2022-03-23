library(xts)
predict.cases <- function(x=d$`新增本土确诊（含无症状）`, date=d$date) {
    x <- xts(x, date)
}