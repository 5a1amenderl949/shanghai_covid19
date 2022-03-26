library(ggplot2)
library(reshape2)
# 每日新增人数图
plot.new <- function(d.new=d.new,
                     fn="../out/上海疫情趋势图.png") {
    ggplot(d.new) + geom_line(aes(x=date, y=value, colour=variable)) +
        xlab("日期") +
        ylab("病例数") +
        theme(text = element_text(family='Kai'),
              legend.title = element_blank(),
              legend.position = c("top"))
    message("saving: ", fn)
    ggsave(fn, width = 8, height = 6, dpi = 600)
        
}

if (FALSE) {
    d.new <- reshape2::melt(d, id="date", measure=c("新增本土新冠肺炎确诊病例",
                                               "新增本土无症状感染者",
                                               "新增境外输入性新冠肺炎确诊病例",
                                               "新增境外输入性无症状感染者",
                                               "治愈出院",
                                               "解除医学观察无症状感染者"))
    source("plot.r")
    plot.new(d.new=d.new)
}

# 非管控区域病例比例
plot.ratio <- function(d.ratio=d[d$非管控区域病例比例<100, c("date", "非管控区域病例比例")],
                     fn="../out/非管控区域病例比例.png") {

    ggplot(d.ratio) + geom_line(aes(x=date, y=非管控区域病例比例), color = "red") +
        labs(title="非管控区域病例比例",
             x ="日期", y = "%") +
        theme(text = element_text(family='Kai'))
    message("saving: ", fn)
    ggsave(fn, width = 8, height = 6, dpi = 600)
    
}

if (FALSE) {
    source("plot.r")
    plot.ratio()
}

# 确诊和无症状信息
plot.info <- function(d.info = info$确诊信息,
                       fn="../out/确诊信息.png") {
    
    ggplot(d.info) + geom_point(aes(x=日期, y=年龄, color = 地区, shape=性别)) +
        labs(title="确诊者年龄地区性别分布",
             x ="日期", y = "年龄") +
        theme(text = element_text(family='Kai'))
    message("saving: ", fn)
    ggsave(fn, width = 16, height = 8, dpi = 600)
    
}

if (FALSE) {
    source("plot.r")
    plot.info(d.info = info$确诊信息,
                   fn="../out/确诊信息.png")
    plot.info(d.info = info$无症状信息,
                   fn="../out/无症状信息.png")
}
