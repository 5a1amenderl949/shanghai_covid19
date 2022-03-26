library(ggplot2)
library(reshape2)
# 每日新增人数图
plot.new <- function(d.new = d.new,
                     fn = "../out/上海疫情趋势图.png") {
    ggplot(d.new) +
        geom_line(aes(x = date, y = value, colour = variable)) +
        xlab("日期") +
        ylab("病例数") +
        theme(
            text = element_text(family = "Kai"),
            legend.title = element_blank(),
            legend.position = c("top")
        )
    message("saving: ", fn)
    ggsave(fn, width = 8, height = 6, dpi = 600)
}

if (FALSE) {
    d.new <- reshape2::melt(d, id = "date", measure = c(
        "新增本土新冠肺炎确诊病例",
        "新增本土无症状感染者",
        "新增境外输入性新冠肺炎确诊病例",
        "新增境外输入性无症状感染者",
        "治愈出院",
        "解除医学观察无症状感染者"
    ))
    source("plot.r")
    plot.new(d.new = d.new)
}

# 非管控区域病例比例
plot.ratio <- function(d.ratio = d[d$非管控区域病例比例 < 100, c("date", "非管控区域病例比例")],
                       fn = "../out/非管控区域病例比例.png") {
    ggplot(d.ratio) +
        geom_line(aes(x = date, y = 非管控区域病例比例), color = "red") +
        labs(
            title = "非管控区域病例比例",
            x = "日期", y = "%"
        ) +
        theme(text = element_text(family = "Kai"))
    message("saving: ", fn)
    ggsave(fn, width = 8, height = 6, dpi = 600)
}

if (FALSE) {
    source("plot.r")
    plot.ratio()
}

# 确诊和无症状信息
plot.info <- function(d.info = info$确诊信息,
                      fn = "../out/确诊信息.png") {
    ggplot(d.info) +
        geom_point(aes(x = 日期, y = 年龄, color = 地区, shape = 性别)) +
        labs(
            title = "确诊者年龄地区性别分布",
            x = "日期", y = "年龄"
        ) +
        theme(text = element_text(family = "Kai"))
    message("saving: ", fn)
    ggsave(fn, width = 16, height = 8, dpi = 600)
}

if (FALSE) {
    source("plot.r")
    plot.info(
        d.info = info$确诊信息,
        fn = "../out/确诊信息.png"
    )
    plot.info(
        d.info = info$无症状信息,
        fn = "../out/无症状信息.png"
    )
}

# 按区域画每日新增人数
plot.district <- function(d.info = info$确诊信息,
                          title = "按区域分每日新增人数",
                          fn = "../out/确诊信息.png") {
    dd <- aggregate(
        d.info$病例,
        list(日期 = d.info$日期, 地区 = d.info$地区),
        length
    )
    ggplot(dd) +
        geom_line(aes(x = 日期, y = x, color = 地区)) +
        labs(
            title = title,
            x = "日期", y = "病例数"
        ) +
        theme(text = element_text(family = "Kai"))
    message("saving: ", fn)
    ggsave(fn, width = 16, height = 8, dpi = 600)
}

if (FALSE) {
    source("plot.r")
    plot.district(
        d.info = info$确诊信息,
        title = "按区域分每日新增确诊人数",
        fn = "../out/按区域分每日新增确诊人数.png"
    )
    plot.district(
        d.info = info$无症状信息,
        title = "按区域分每日新增无症状人数",
        fn = "../out/按区域分每日新增无症状人数.png"
    )
}

# 按区域/时间画每日年龄
plot.by.group <- function(d.info = info$确诊信息,
                          title = "按区域分年龄分布",
                          group = "日期",
                          fn = "../out/确诊人数按区域分年龄分布.png") {
    d.info$日期 <- as.character(d.info$日期)
    ggplot(d.info, aes(get(group), 年龄)) +
        geom_boxplot() +
        labs(
            title = title,
            x = group, y = "年龄"
        ) +
        theme(text = element_text(family = "Kai"))
    message("saving: ", fn)
    ggsave(fn, width = 16, height = 8, dpi = 600)
}

plot.by.group.all <- function(info = info,
                              groups = c("日期", "地区", "性别"),
                              dir = "../out/") {
    for (i in seq_along(groups)) {
        group <- groups[i]
        ttl <- paste0("确诊患者按", group, "分", "年龄分布")
        fn <- paste0(dir, ttl, ".png")
        plot.by.group(
            d.info = info$确诊信息,
            title = ttl,
            group = group,
            fn = fn
        )
        ttl <- paste0("无症状患者按", group, "分", "年龄分布")
        fn <- paste0(dir, ttl, ".png")
        plot.by.group(
            d.info = info$无症状信息,
            title = ttl,
            group = group,
            fn = fn
        )
    }
}

if (FALSE) {
    source("plot.r")
    plot.by.group.all(info = info)
}