plot.trend <- function(x = d$date, y = d[, c(
                           "新增本土新冠肺炎确诊病例",
                           "新增本土无症状感染者",
                           "新增境外输入性新冠肺炎确诊病例",
                           "新增境外输入性无症状感染者",
                           "治愈出院",
                           "解除医学观察无症状感染者"
                       )],
                       lty = 1:5,
                       lwd = 2,
                       col = 1:6) {
    par(family = "PingFangSC-Regular")
    matplot(
        x = x, y = y, type = "l",
        xaxt = "n",
        lty = lty,
        lwd = lwd,
        col = col,
        xlab = "时间",
        ylab = "人数"
    )
    axis.Date(1, x, x, format = "%m月%d日")
    legend("topleft",
        legend = names(y),
        lty = lty,
        col = col,
        lwd = lwd
    )
}

if (FALSE) {
    source("plot.trend.r")
    plot.trend()
}

save.trend <- function(x = d$date, y = d[, c(
                           "新增本土新冠肺炎确诊病例",
                           "新增本土无症状感染者",
                           "新增境外输入性新冠肺炎确诊病例",
                           "新增境外输入性无症状感染者",
                           "治愈出院",
                           "解除医学观察无症状感染者"
                       )],
                       fn = "../out/上海疫情趋势图.png",
                       w = 16,
                       h = 8,
                       pointsize = 12,
                       res = 600,
                       ...) {
    png(fn, width = w * res, height = h * res, pointsize = pointsize, res = res)
    message("saving: ", fn)
    plot.trend(x, y, ...)
    dev.off()
}

if (FALSE) {
    source("plot.trend.r")
    save.trend()
}