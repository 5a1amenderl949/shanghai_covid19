library(ggplot2)
library(reshape2)

# 保存图片
save.plot <- function(p,
                      fn = "../out/确诊信息.png") {
    message("saving: ", fn)
    ggsave(fn, p, width = 16, height = 8, dpi = 600)
}

# 每日新增人数图
plot.new <- function(d.new = d.new) {
    p <- ggplot(d.new) +
        geom_line(aes(x = date, y = value, colour = variable)) +
        xlab("日期") +
        ylab("病例数") +
        theme(
            text = element_text(family = "Kai"),
            legend.title = element_blank(),
            legend.position = c("top")
        )
    p
}

if (FALSE) {
    d.new <- reshape2::melt(d.basic, id = "date", measure = c(
        "新增本土新冠肺炎确诊病例",
        "新增本土无症状感染者",
        "新增境外输入性新冠肺炎确诊病例",
        "新增境外输入性无症状感染者",
        "治愈出院",
        "解除医学观察无症状感染者"
    ))
    source("plot.r")
    p <- plot.new(d.new = d.new)
    save.plot(p, "../out/上海疫情趋势图.png")
}

# 非管控区域病例比例
plot.ratio <- function(d.ratio = d.basic[d.basic$非管控区域病例比例 < 100, c("date", "非管控区域病例比例")]) {
    p <- ggplot(d.ratio) +
        geom_line(aes(x = date, y = 非管控区域病例比例), color = "red") +
        labs(
            title = "非管控区域病例比例",
            x = "日期", y = "%"
        ) +
        theme(text = element_text(family = "Kai"))
    p
}

if (FALSE) {
    source("plot.r")
    p <- plot.ratio()
    save.plot(p, "../out/非管控区域病例比例.png")
}

# 按区域画每日新增人数
plot.district <- function(d.info = info$确诊信息,
                          title = "按区域分每日新增人数",
                          is.cumsum = FALSE) {
    dd <- aggregate(
        d.info$病例,
        list(日期 = d.info$日期, 地区 = d.info$地区),
        length
    )
    if (is.cumsum) {
        dd <- dd[order(dd$地区, dd$日期), ]
        dd <- dd %>% group_by(地区) %>% arrange(地区) %>% mutate(x = cumsum(x))
    }
    p <- ggplot(dd) +
        geom_line(aes(x = 日期, y = x, color = 地区)) +
        labs(
            title = title,
            x = "日期", y = "病例数"
        ) +
        theme(text = element_text(family = "Kai"))
    p
}

if (FALSE) {
    source("plot.r")
    p <- plot.district()
    save.plot(
        p,
        fn = "../out/按区域分每日新增确诊人数.png"
    )
    p <- plot.district(
        d.info = info$无症状信息,
        title = "按区域分每日新增无症状人数"
    )
    save.plot(p, "../out/按区域分每日新增无症状人数.png")
}

# 按区域/时间画每日年龄
plot.by.group <- function(d.info = info$确诊信息,
                          title = "按区域分年龄分布",
                          group = "日期") {
    d.info$日期 <- as.character(d.info$日期)
    p <- ggplot(d.info, aes(get(group), 年龄)) +
        geom_boxplot() +
        labs(
            title = title,
            x = group, y = "年龄"
        ) +
        theme(text = element_text(family = "Kai"))
    p
}

plot.by.group.all <- function(info = info,
                              groups = c("日期", "地区", "性别"),
                              dir = "../out/") {
    for (i in seq_along(groups)) {
        group <- groups[i]
        ttl <- paste0("确诊患者按", group, "分", "年龄分布")
        fn <- paste0(dir, ttl, ".png")
        p <- plot.by.group(
            d.info = info$确诊信息,
            title = ttl,
            group = group
        )
        save.plot(p, fn)
        ttl <- paste0("无症状患者按", group, "分", "年龄分布")
        fn <- paste0(dir, ttl, ".png")
        p <- plot.by.group(
            d.info = info$无症状信息,
            title = ttl,
            group = group
        )
        save.plot(p, fn)
    }
}

if (FALSE) {
    source("plot.r")
    plot.by.group.all(info = info)
}